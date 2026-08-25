;;; my-airbaltic.el --- Add airBaltic bookings to Google Calendar -*- lexical-binding: t; -*-

;;; Commentary:

;; Validate airBaltic booking-confirmation flight data against the airline's
;; timetable, resolve airport time zones, and add the flights to Google Calendar.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'json)
(require 'seq)
(require 'subr-x)

(defconst dotfiles--airbaltic-airports-url
  "https://www.airbaltic.com/api/orig-dest/lt")

(defconst dotfiles--airbaltic-timetable-url
  "https://api.airbaltic.com/schedule/timetable/simple")

(defconst dotfiles--airbaltic-open-meteo-url
  "https://api.open-meteo.com/v1/forecast")

;; Generated response accessors can only be file-checked by `check-declare'.
(declare-function org-gcal--get-access-token "ext:org-gcal" (calendar-id))
(declare-function request "request" (url &rest settings))
(declare-function request-response-data "request" (response) t)
(declare-function request-response-status-code "request" (response) t)

(cl-defstruct (dotfiles--airbaltic-leg
               (:constructor dotfiles--airbaltic-leg-create))
  date
  departure-clock
  departure-city
  arrival-clock
  arrival-city
  flight-number
  origin
  destination
  departure-time
  arrival-time
  next-day
  departure-zone
  arrival-zone)

(defun dotfiles--airbaltic--leaf-cell-texts (html)
  "Return normalized text from leaf table cells in HTML."
  (declare (ftype (function (string) list))
           (important-return-value t)
           (side-effect-free t))
  (let ((dom (with-temp-buffer
               (insert html)
               (libxml-parse-html-region))))
    (mapcar
     (lambda (cell)
       (string-trim
        (replace-regexp-in-string "[[:space:]]+" " " (dom-texts cell " "))))
     (seq-filter
      (lambda (cell) (= (length (dom-by-tag cell 'td)) 1))
      (dom-by-tag dom 'td)))))

(defun dotfiles--airbaltic--date-in-cell (cell)
  "Return the ISO date embedded in CELL, or nil."
  (declare (ftype (function (string) (or string null)))
           (important-return-value t)
           (side-effect-free t))
  (when (string-match
         "\\([0-9]\\{2\\}\\)\\.\\([0-9]\\{2\\}\\)\\.\\([0-9]\\{4\\}\\)"
         cell)
    (format "%s-%s-%s" (match-string 3 cell) (match-string 2 cell)
            (match-string 1 cell))))

(defun dotfiles--airbaltic--time-and-city (cell)
  "Return the clock and city from an itinerary CELL, or nil."
  (declare (ftype (function (string) (or cons null)))
           (important-return-value t)
           (side-effect-free t))
  (when (string-match
         "^\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)[[:space:]]+\\(.+\\)$" cell)
    (let ((clock (match-string 1 cell))
          (city (match-string 2 cell)))
      (cons clock
            (string-trim
             (replace-regexp-in-string
              ",[[:space:]]*Terminalas.*$" "" city))))))

(defun dotfiles--airbaltic--departure-label-p (cell)
  "Return non-nil when CELL is an itinerary departure label."
  (declare (ftype (function (string) boolean))
           (important-return-value t)
           (side-effect-free t))
  (and (string-suffix-p "vykimas" cell)
       (not (string-suffix-p "Atvykimas" cell))))

(defun dotfiles--airbaltic-parse-itinerary (html)
  "Parse airBaltic booking itinerary from HTML.
Return a list of `dotfiles--airbaltic-leg' values."
  (declare (ftype (function (string) list))
           (important-return-value t)
           (side-effect-free t))
  (let (date state departure arrival legs)
    (dolist (cell (dotfiles--airbaltic--leaf-cell-texts html))
      (let ((cell-date (dotfiles--airbaltic--date-in-cell cell))
            (time-and-city (dotfiles--airbaltic--time-and-city cell)))
        (cond
         (cell-date
          ;; The outbound section heading uses the same text as a departure
          ;; row and immediately precedes its date.
          (unless (memq state '(nil departure))
            (user-error "Incomplete itinerary before %s" cell-date))
          (setq date cell-date
                state nil
                departure nil
                arrival nil))
         ((dotfiles--airbaltic--departure-label-p cell)
          (when state
            (user-error "Incomplete itinerary before another departure"))
          (setq state 'departure))
         ((string= cell "Atvykimas:")
          (unless (eq state 'arrival-label)
            (user-error "Unexpected arrival label in itinerary"))
          (setq state 'arrival))
         ((string= cell "Skrydis:")
          (unless (eq state 'flight-label)
            (user-error "Unexpected flight label in itinerary"))
          (setq state 'flight))
         ((and time-and-city (eq state 'departure))
          (unless date
            (user-error "Departure has no itinerary date"))
          (setq departure time-and-city
                state 'arrival-label))
         ((and time-and-city (eq state 'arrival))
          (setq arrival time-and-city
                state 'flight-label))
         ((and (eq state 'flight)
               (string-match "^\\(BT[0-9]+\\)[[:space:]]*/" cell))
          (push
           (dotfiles--airbaltic-leg-create
            :date date
            :departure-clock (car departure)
            :departure-city (cdr departure)
            :arrival-clock (car arrival)
            :arrival-city (cdr arrival)
            :flight-number (match-string 1 cell))
           legs)
          (setq state nil
                departure nil
                arrival nil))
         ((string-empty-p cell))
         (state
          (user-error "Unexpected itinerary data: %s" cell)))))
    (when state
      (user-error "Incomplete itinerary at end of message"))
    (unless legs
      (user-error "No flights found in the airBaltic booking"))
    (nreverse legs)))

(defun dotfiles--airbaltic--offset-seconds (offset)
  "Return OFFSET as seconds east of UTC."
  (declare (ftype (function (string) integer))
           (important-return-value t)
           (side-effect-free t))
  (unless (string-match
           "^\\([+-]\\)\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)$" offset)
    (error "Invalid UTC offset: %s" offset))
  (let ((seconds (+ (* 3600 (string-to-number (match-string 2 offset)))
                    (* 60 (string-to-number (match-string 3 offset))))))
    (if (string= (match-string 1 offset) "-") (- seconds) seconds)))

(defun dotfiles--airbaltic-timezone-matches-p (date clock offset zone)
  "Return non-nil when ZONE has OFFSET at DATE and CLOCK."
  (declare (ftype (function (string string string string) boolean))
           (important-return-value t)
           (side-effect-free t))
  (unless (and (string-match
                "^\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)$"
                date)
               (string-match
                "^\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)$" clock))
    (error "Invalid local date or time: %s %s" date clock))
  (let* ((year (string-to-number (substring date 0 4)))
         (month (string-to-number (substring date 5 7)))
         (day (string-to-number (substring date 8 10)))
         (hour (string-to-number (substring clock 0 2)))
         (minute (string-to-number (substring clock 3 5)))
         (time
          (encode-time
           (list 0 minute hour day month year nil -1
                 (dotfiles--airbaltic--offset-seconds offset))))
         (numeric-offset (format-time-string "%z" time zone))
         (colon-offset (concat (substring numeric-offset 0 3) ":"
                               (substring numeric-offset 3))))
    (and (string= (format-time-string "%F" time zone) date)
         (string= (format-time-string "%R" time zone) clock)
         (string= colon-offset offset))))

(defun dotfiles--airbaltic--next-date (date)
  "Return the ISO date after DATE."
  (declare (ftype (function (string) string))
           (important-return-value t)
           (side-effect-free t))
  (let ((year (string-to-number (substring date 0 4)))
        (month (string-to-number (substring date 5 7)))
        (day (string-to-number (substring date 8 10))))
    (format-time-string
     "%F" (time-add (encode-time (list 0 0 0 day month year nil -1 t))
                    (days-to-time 1))
     t)))

(defun dotfiles--airbaltic--rfc3339 (date local-time)
  "Combine DATE and offset-bearing LOCAL-TIME as RFC 3339."
  (declare (ftype (function (string string) string))
           (important-return-value t)
           (side-effect-free t))
  (concat date "T" (substring local-time 0 5) ":00"
          (substring local-time 5)))

(defun dotfiles--airbaltic--parse-json-response ()
  "Parse the current HTTP response buffer as JSON."
  (declare (ftype (function () t))
           (important-return-value t))
  (json-parse-buffer :object-type 'hash-table :array-type 'list
                     :null-object nil :false-object nil))

(defun dotfiles--airbaltic--request-json
    (url &optional method data headers)
  "Request URL and return its HTTP status paired with parsed JSON.
METHOD defaults to GET.  Send DATA and HEADERS when provided."
  (declare (ftype (function (string &optional string string list) cons))
           (important-return-value t))
  (require 'request)
  (let* ((response
          (request url :type (or method "GET") :data data :headers headers
                   :parser #'dotfiles--airbaltic--parse-json-response
                   :sync t :timeout 20))
         (status (request-response-status-code response)))
    (unless status
      (user-error "No HTTP response from the remote service"))
    (cons status (request-response-data response))))

(defun dotfiles--airbaltic--successful-response-data (response service)
  "Return RESPONSE data when SERVICE returned a successful status."
  (declare (ftype (function (cons string) t))
           (important-return-value t))
  (unless (<= 200 (car response) 299)
    (user-error "%s returned HTTP %d" service (car response)))
  (cdr response))

(defun dotfiles--airbaltic--airport-from-json (airport)
  "Return the relevant fields from an AIRPORT JSON object."
  (declare (ftype (function (hash-table) list))
           (important-return-value t)
           (side-effect-free t))
  (list :code (gethash "code" airport)
        :city (gethash "city" airport)
        :apt (gethash "apt" airport)
        :latitude (gethash "latitude" airport)
        :longitude (gethash "longitude" airport)))

(defun dotfiles--airbaltic--airport-group-values (group)
  "Return airport plists from JSON object GROUP."
  (declare (ftype (function (hash-table) list))
           (important-return-value t)
           (side-effect-free t))
  (let (airports)
    (maphash
     (lambda (_key airport)
       (when (string= (gethash "type" airport) "A")
         (push (dotfiles--airbaltic--airport-from-json airport) airports)))
     group)
    airports))

(defun dotfiles--airbaltic--fetch-airports ()
  "Fetch airBaltic's Lithuanian airport catalog."
  (declare (ftype (function () list))
           (important-return-value t))
  (let* ((response
          (dotfiles--airbaltic--request-json dotfiles--airbaltic-airports-url))
         (data
          (dotfiles--airbaltic--successful-response-data response "airBaltic"))
         (origin-data (gethash "origData" data))
         (airports
          (append
           (dotfiles--airbaltic--airport-group-values
            (gethash "btOrigins" origin-data))
           (dotfiles--airbaltic--airport-group-values
            (gethash "nonBtOrigins" origin-data)))))
    (cl-delete-duplicates airports :test #'string=
                          :key (lambda (airport) (plist-get airport :code)))))

(defun dotfiles--airbaltic--location-key (name)
  "Return a normalized comparison key for airport NAME."
  (declare (ftype (function (string) string))
           (important-return-value t)
           (side-effect-free t))
  (downcase (string-trim (replace-regexp-in-string "[[:space:]]+" " " name))))

(defun dotfiles--airbaltic--matching-airports (city airports)
  "Return AIRPORTS whose city or airport name exactly matches CITY."
  (declare (ftype (function (string list) list))
           (important-return-value t)
           (side-effect-free t))
  (let ((key (dotfiles--airbaltic--location-key city)))
    (cl-remove-if-not
     (lambda (airport)
       (or (string= key (dotfiles--airbaltic--location-key
                         (plist-get airport :city)))
           (string= key (dotfiles--airbaltic--location-key
                         (plist-get airport :apt)))))
     airports)))

(defun dotfiles--airbaltic--fetch-timetable (origin destination date)
  "Fetch the airBaltic timetable from ORIGIN to DESTINATION on DATE."
  (declare (ftype (function (string string string) list))
           (important-return-value t))
  (let* ((body
          (json-encode `(("orig" . ,origin)
                         ("dest" . ,destination)
                         ("periodStart" . ,date)
                         ("periodEnd" . ,date))))
         (response
          (dotfiles--airbaltic--request-json
           dotfiles--airbaltic-timetable-url "POST" body
           '(("Content-Type" . "application/json")))))
    (dotfiles--airbaltic--successful-response-data response "airBaltic")))

(defun dotfiles--airbaltic--timetable-entry-matches-p
    (entry leg origin destination)
  "Return non-nil when ENTRY validates LEG from ORIGIN to DESTINATION."
  (declare (ftype (function (hash-table dotfiles--airbaltic-leg list list)
                            boolean))
           (important-return-value t)
           (side-effect-free t))
  (and (not (gethash "connFlightNumber" entry))
       (string= (gethash "flightNumber" entry)
                (dotfiles--airbaltic-leg-flight-number leg))
       (string= (gethash "orig" entry) (plist-get origin :code))
       (string= (gethash "dest" entry) (plist-get destination :code))
       (member (dotfiles--airbaltic-leg-date leg) (gethash "dates" entry))
       (string-prefix-p (dotfiles--airbaltic-leg-departure-clock leg)
                        (gethash "depTime" entry))
       (string-prefix-p (dotfiles--airbaltic-leg-arrival-clock leg)
                        (gethash "arrTime" entry))))

(defun dotfiles--airbaltic--leg-from-timetable
    (leg origin destination entry)
  "Return LEG augmented with ORIGIN, DESTINATION, and timetable ENTRY."
  (declare (ftype (function (dotfiles--airbaltic-leg list list hash-table)
                            dotfiles--airbaltic-leg))
           (important-return-value t)
           (side-effect-free t))
  (let ((resolved (copy-dotfiles--airbaltic-leg leg)))
    (setf (dotfiles--airbaltic-leg-origin resolved) origin
          (dotfiles--airbaltic-leg-destination resolved) destination
          (dotfiles--airbaltic-leg-departure-time resolved)
          (gethash "depTime" entry)
          (dotfiles--airbaltic-leg-arrival-time resolved)
          (gethash "arrTime" entry)
          (dotfiles--airbaltic-leg-next-day resolved)
          (gethash "nextDay" entry))
    resolved))

(defun dotfiles--airbaltic--resolve-leg (leg airports)
  "Validate and resolve LEG against AIRPORTS and the dated timetable."
  (declare (ftype (function (dotfiles--airbaltic-leg list)
                            dotfiles--airbaltic-leg))
           (important-return-value t))
  (let ((origins
         (dotfiles--airbaltic--matching-airports
          (dotfiles--airbaltic-leg-departure-city leg) airports))
        (destinations
         (dotfiles--airbaltic--matching-airports
          (dotfiles--airbaltic-leg-arrival-city leg) airports))
        matches)
    (unless origins
      (user-error "No airport found for %s"
                  (dotfiles--airbaltic-leg-departure-city leg)))
    (unless destinations
      (user-error "No airport found for %s"
                  (dotfiles--airbaltic-leg-arrival-city leg)))
    (dolist (origin origins)
      (dolist (destination destinations)
        (dolist
            (entry
             (dotfiles--airbaltic--fetch-timetable
              (plist-get origin :code) (plist-get destination :code)
              (dotfiles--airbaltic-leg-date leg)))
          (when (dotfiles--airbaltic--timetable-entry-matches-p
                 entry leg origin destination)
            (push (dotfiles--airbaltic--leg-from-timetable
                   leg origin destination entry)
                  matches)))))
    (unless (= (length matches) 1)
      (user-error "Expected one dated timetable match for %s; found %d"
                  (dotfiles--airbaltic-leg-flight-number leg)
                  (length matches)))
    (car matches)))

(defun dotfiles--airbaltic--airport-code-equal-p (left right)
  "Return non-nil when airport plists LEFT and RIGHT have the same code."
  (declare (ftype (function (list list) boolean))
           (important-return-value t)
           (side-effect-free t))
  (string= (plist-get left :code) (plist-get right :code)))

(defun dotfiles--airbaltic--itinerary-airports (legs)
  "Return unique airports from resolved LEGS in encounter order."
  (declare (ftype (function (list) list))
           (important-return-value t)
           (side-effect-free t))
  (cl-delete-duplicates
   (cl-loop for leg in legs
            append (list (dotfiles--airbaltic-leg-origin leg)
                         (dotfiles--airbaltic-leg-destination leg)))
   :test #'dotfiles--airbaltic--airport-code-equal-p :from-end t))

(defun dotfiles--airbaltic--open-meteo-timezones (airports)
  "Resolve AIRPORTS to IANA time zones through Open-Meteo."
  (declare (ftype (function (list) list))
           (important-return-value t))
  (let* ((latitudes
          (mapconcat (lambda (airport) (plist-get airport :latitude))
                     airports ","))
         (longitudes
          (mapconcat (lambda (airport) (plist-get airport :longitude))
                     airports ","))
         (url
          (format "%s?latitude=%s&longitude=%s&timezone=auto&forecast_days=1"
                  dotfiles--airbaltic-open-meteo-url latitudes longitudes))
         (response (dotfiles--airbaltic--request-json url))
         (data
          (dotfiles--airbaltic--successful-response-data response "Open-Meteo"))
         (locations (if (hash-table-p data) (list data) data)))
    (unless (= (length airports) (length locations))
      (user-error "Open-Meteo returned %d locations for %d airports"
                  (length locations) (length airports)))
    (cl-mapcar
     (lambda (airport location)
       (let ((zone (gethash "timezone" location)))
         (unless (and (stringp zone) (not (string-empty-p zone)))
           (user-error "Open-Meteo returned no time zone for %s"
                       (plist-get airport :code)))
         (cons (plist-get airport :code) zone)))
     airports locations)))

(defun dotfiles--airbaltic--time-offset (local-time)
  "Return the numeric UTC offset from timetable LOCAL-TIME."
  (declare (ftype (function (string) string))
           (important-return-value t)
           (side-effect-free t))
  (unless (string-match-p
           "^[0-9]\\{2\\}:[0-9]\\{2\\}[+-][0-9]\\{2\\}:[0-9]\\{2\\}$"
           local-time)
    (error "Invalid timetable local time: %s" local-time))
  (substring local-time 5))

(defun dotfiles--airbaltic--apply-timezones (legs)
  "Resolve and validate the airport time zones for resolved LEGS."
  (declare (ftype (function (list) list))
           (important-return-value t))
  (let* ((zones
          (dotfiles--airbaltic--open-meteo-timezones
           (dotfiles--airbaltic--itinerary-airports legs)))
         zoned-legs)
    (dolist (leg legs)
      (let* ((origin-code
              (plist-get (dotfiles--airbaltic-leg-origin leg) :code))
             (destination-code
              (plist-get (dotfiles--airbaltic-leg-destination leg) :code))
             (departure-zone (alist-get origin-code zones nil nil #'string=))
             (arrival-zone
              (alist-get destination-code zones nil nil #'string=))
             (arrival-date
              (if (dotfiles--airbaltic-leg-next-day leg)
                  (dotfiles--airbaltic--next-date
                   (dotfiles--airbaltic-leg-date leg))
                (dotfiles--airbaltic-leg-date leg))))
        (unless departure-zone
          (user-error "No time zone resolved for %s" origin-code))
        (unless arrival-zone
          (user-error "No time zone resolved for %s" destination-code))
        (unless
            (dotfiles--airbaltic-timezone-matches-p
             (dotfiles--airbaltic-leg-date leg)
             (dotfiles--airbaltic-leg-departure-clock leg)
             (dotfiles--airbaltic--time-offset
              (dotfiles--airbaltic-leg-departure-time leg))
             departure-zone)
          (user-error "Open-Meteo time zone disagrees with %s departure offset"
                      (dotfiles--airbaltic-leg-flight-number leg)))
        (unless
            (dotfiles--airbaltic-timezone-matches-p
             arrival-date (dotfiles--airbaltic-leg-arrival-clock leg)
             (dotfiles--airbaltic--time-offset
              (dotfiles--airbaltic-leg-arrival-time leg))
             arrival-zone)
          (user-error "Open-Meteo time zone disagrees with %s arrival offset"
                      (dotfiles--airbaltic-leg-flight-number leg)))
        (let ((zoned (copy-dotfiles--airbaltic-leg leg)))
          (setf (dotfiles--airbaltic-leg-departure-zone zoned) departure-zone
                (dotfiles--airbaltic-leg-arrival-zone zoned) arrival-zone)
          (push zoned zoned-legs))))
    (nreverse zoned-legs)))

(defun dotfiles--airbaltic-event-data (leg)
  "Return Google Calendar event data for LEG."
  (declare (ftype (function (dotfiles--airbaltic-leg) list))
           (important-return-value t)
           (side-effect-free t))
  (let* ((origin (dotfiles--airbaltic-leg-origin leg))
         (destination (dotfiles--airbaltic-leg-destination leg))
         (date (dotfiles--airbaltic-leg-date leg))
         (arrival-date
          (if (dotfiles--airbaltic-leg-next-day leg)
              (dotfiles--airbaltic--next-date date)
            date)))
    `(("summary" . ,(format "%s %s → %s"
                            (dotfiles--airbaltic-leg-flight-number leg)
                            (plist-get origin :code)
                            (plist-get destination :code)))
      ("location" . ,(format "%s (%s) → %s (%s)"
                             (plist-get origin :apt)
                             (plist-get origin :code)
                             (plist-get destination :apt)
                             (plist-get destination :code)))
      ("description" . "Added from an airBaltic booking confirmation.")
      ("transparency" . "transparent")
      ("start"
       . (("dateTime"
           . ,(dotfiles--airbaltic--rfc3339
               date (dotfiles--airbaltic-leg-departure-time leg)))
          ("timeZone" . ,(dotfiles--airbaltic-leg-departure-zone leg))))
      ("end"
       . (("dateTime"
           . ,(dotfiles--airbaltic--rfc3339
               arrival-date (dotfiles--airbaltic-leg-arrival-time leg)))
          ("timeZone" . ,(dotfiles--airbaltic-leg-arrival-zone leg)))))))

(defun dotfiles--airbaltic--prepare-booking (html)
  "Validate every flight in airBaltic booking HTML and resolve its zones."
  (declare (ftype (function (string) list))
           (important-return-value t))
  (let* ((legs (dotfiles--airbaltic-parse-itinerary html))
         (airports (dotfiles--airbaltic--fetch-airports))
         (resolved
          (mapcar
           (lambda (leg) (dotfiles--airbaltic--resolve-leg leg airports))
           legs)))
    (dotfiles--airbaltic--apply-timezones resolved)))

(defun dotfiles--airbaltic--event-id (calendar-id leg)
  "Return a deterministic Google Calendar event ID for LEG on CALENDAR-ID."
  (declare (ftype (function (string dotfiles--airbaltic-leg) string))
           (important-return-value t)
           (side-effect-free t))
  (secure-hash
   'sha1
   (mapconcat
    #'identity
    (list calendar-id (dotfiles--airbaltic-leg-flight-number leg)
          (dotfiles--airbaltic-leg-date leg)
          (dotfiles--airbaltic-leg-departure-time leg)
          (plist-get (dotfiles--airbaltic-leg-origin leg) :code)
          (plist-get (dotfiles--airbaltic-leg-destination leg) :code))
    "\0")))

(defun dotfiles--airbaltic--insert-event
    (calendar-id access-token leg)
  "Insert LEG into CALENDAR-ID using ACCESS-TOKEN.
Return `created' or `existing'."
  (declare (ftype (function (string string dotfiles--airbaltic-leg) symbol))
           (important-return-value t))
  (let* ((event
          (cons `("id" . ,(dotfiles--airbaltic--event-id calendar-id leg))
                (dotfiles--airbaltic-event-data leg)))
         (url
          (format
           "https://www.googleapis.com/calendar/v3/calendars/%s/events?sendUpdates=none"
           (url-hexify-string calendar-id)))
         (response
          (dotfiles--airbaltic--request-json
           url "POST" (encode-coding-string (json-encode event) 'utf-8)
           `(("Content-Type" . "application/json")
             ("Accept" . "application/json")
             ("Authorization" . ,(format "Bearer %s" access-token))))))
    (cond
     ((<= 200 (car response) 299) 'created)
     ((= (car response) 409) 'existing)
     (t (user-error "Google Calendar returned HTTP %d" (car response))))))

(defun dotfiles--airbaltic--insert-events (calendar-id legs)
  "Insert LEGS into Google CALENDAR-ID and return the result symbols."
  (declare (ftype (function (string list) list))
           (important-return-value t))
  (require 'org-gcal)
  (let ((access-token (org-gcal--get-access-token calendar-id)))
    (mapcar
     (lambda (leg)
       (dotfiles--airbaltic--insert-event calendar-id access-token leg))
     legs)))

(defun dotfiles--airbaltic-add-booking-to-calendar (html calendar-id)
  "Validate airBaltic booking HTML and offer to add it to CALENDAR-ID."
  (declare (ftype (function (string string) t)))
  (let ((legs (dotfiles--airbaltic--prepare-booking html)))
    (when (y-or-n-p
           (format "Add %d airBaltic flight%s to Google Calendar? "
                   (length legs) (if (= (length legs) 1) "" "s")))
      (let* ((results
              (dotfiles--airbaltic--insert-events calendar-id legs))
             (created (seq-count (lambda (result) (eq result 'created))
                                 results))
             (existing (- (length results) created)))
        (message "airBaltic flights: %d created, %d already present"
                 created existing)))))

(provide 'my-airbaltic)
;;; my-airbaltic.el ends here
