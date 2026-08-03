;;; my-airbaltic-test.el --- Tests for my-airbaltic -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for airBaltic booking automation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'my-airbaltic)

(defun dotfiles--airbaltic-test-airport (code city)
  "Return a test airport with CODE and CITY."
  (list :code code :city city :apt city :latitude "1" :longitude "2"))

(defun dotfiles--airbaltic-test-timetable-entry
    (flight origin destination departure arrival date &optional next-day)
  "Return a test timetable entry for FLIGHT from ORIGIN to DESTINATION."
  (let ((entry (make-hash-table :test #'equal)))
    (puthash "flightNumber" flight entry)
    (puthash "orig" origin entry)
    (puthash "dest" destination entry)
    (puthash "depTime" departure entry)
    (puthash "arrTime" arrival entry)
    (puthash "dates" (list date) entry)
    (puthash "nextDay" next-day entry)
    entry))

(ert-deftest dotfiles--airbaltic-parse-itinerary-test ()
  (let* ((html
          (concat
           "<html><body><table>"
           "<tr><td>Užsakyta 11.01.2031</td></tr>"
           "<tr><td>Pn 14.03.2031</td></tr>"
           "<tr><td>Išvykimas</td><td>06:25 Talinas</td></tr>"
           "<tr><td>Atvykimas:</td><td>07:15 Stokholmas, Terminalas* 5</td></tr>"
           "<tr><td>Skrydis:</td><td>BT101 / Economy BASIC, Q</td></tr>"
           "<tr><td>Pn 21.03.2031</td></tr>"
           "<tr><td>Išvykimas</td><td>09:30 Stokholmas, Terminalas* 5</td></tr>"
           "<tr><td>Atvykimas:</td><td>11:45 Praha</td></tr>"
           "<tr><td>Skrydis:</td><td>BT202 / Economy BASIC, A</td></tr>"
           "<tr><td>Išvykimas</td><td>14:10 Praha</td></tr>"
           "<tr><td>Atvykimas:</td><td>16:00 Talinas</td></tr>"
           "<tr><td>Skrydis:</td><td>BT303 / Economy BASIC, A</td></tr>"
           "</table></body></html>"))
         (legs (dotfiles--airbaltic-parse-itinerary html)))
    (should (= (length legs) 3))
    (should
     (equal
      (mapcar
       (lambda (leg)
         (list (dotfiles--airbaltic-leg-date leg)
               (dotfiles--airbaltic-leg-departure-clock leg)
               (dotfiles--airbaltic-leg-departure-city leg)
               (dotfiles--airbaltic-leg-arrival-clock leg)
               (dotfiles--airbaltic-leg-arrival-city leg)
               (dotfiles--airbaltic-leg-flight-number leg)))
       legs)
      '(("2031-03-14" "06:25" "Talinas" "07:15" "Stokholmas" "BT101")
        ("2031-03-21" "09:30" "Stokholmas" "11:45" "Praha" "BT202")
        ("2031-03-21" "14:10" "Praha" "16:00" "Talinas" "BT303"))))))

(ert-deftest dotfiles--airbaltic-timezone-matches-timetable-offset-test ()
  (should
   (dotfiles--airbaltic-timezone-matches-p
    "2031-03-14" "06:25" "+02:00" "Europe/Tallinn"))
  (should
   (dotfiles--airbaltic-timezone-matches-p
    "2031-03-14" "07:15" "+01:00" "Europe/Stockholm"))
  (should-not
   (dotfiles--airbaltic-timezone-matches-p
    "2031-03-14" "06:25" "+01:00" "Europe/Tallinn"))
  (should
   (dotfiles--airbaltic-timezone-matches-p
    "2027-07-01" "10:00" "-04:00" "America/New_York"))
  (should
   (dotfiles--airbaltic-timezone-matches-p
    "2024-10-27" "02:30" "+02:00" "Europe/Berlin"))
  (should
   (dotfiles--airbaltic-timezone-matches-p
    "2024-10-27" "02:30" "+01:00" "Europe/Berlin")))

(ert-deftest dotfiles--airbaltic-rejects-incomplete-leg-at-eof-test ()
  (should-error
   (dotfiles--airbaltic-parse-itinerary
    (concat
     "<html><body><table>"
     "<tr><td>Pn 14.03.2031</td></tr>"
     "<tr><td>Išvykimas</td><td>06:25 Talinas</td></tr>"
     "<tr><td>Atvykimas:</td><td>07:15 Stokholmas</td></tr>"
     "<tr><td>Skrydis:</td><td>BT101 / Economy BASIC</td></tr>"
     "<tr><td>Pn 21.03.2031</td></tr>"
     "<tr><td>Išvykimas</td><td>09:30 Stokholmas</td></tr>"
     "<tr><td>Atvykimas:</td><td>11:45 Praha</td></tr>"
     "</table></body></html>"))
   :type 'user-error))

(ert-deftest dotfiles--airbaltic-rejects-malformed-leg-transitions-test ()
  (dolist
      (itinerary
       (list
        (concat
         "<tr><td>Pn 14.03.2031</td></tr>"
         "<tr><td>Išvykimas</td><td>06:25 Talinas</td></tr>"
         "<tr><td>Atvykimas:</td><td>07:15 Stokholmas</td></tr>"
         "<tr><td>Pn 21.03.2031</td></tr>")
        (concat
         "<tr><td>Pn 14.03.2031</td></tr>"
         "<tr><td>Išvykimas</td><td>06:25 Talinas</td></tr>"
         "<tr><td>Atvykimas:</td><td>07:15 Stokholmas</td></tr>"
         "<tr><td>Išvykimas</td><td>14:10 Stokholmas</td></tr>")
        (concat
         "<tr><td>Pn 14.03.2031</td></tr>"
         "<tr><td>Išvykimas</td><td>06:25 Talinas</td></tr>"
         "<tr><td>Atvykimas:</td><td>07:15 Stokholmas</td></tr>"
         "<tr><td>Skrydis:</td><td>flight unavailable</td></tr>")))
    (should-error
     (dotfiles--airbaltic-parse-itinerary
      (concat "<html><body><table>" itinerary "</table></body></html>"))
     :type 'user-error)))

(ert-deftest dotfiles--airbaltic-event-data-preserves-airport-zones-test ()
  (let* ((leg (dotfiles--airbaltic-leg-create
               :date "2031-03-14"
               :departure-clock "06:25"
               :arrival-clock "07:15"
               :flight-number "BT101"
               :origin '(:code "TLL" :apt "Talinas")
               :destination '(:code "ARN" :apt "Stokholmas")
               :departure-time "06:25+02:00"
               :arrival-time "07:15+01:00"
               :next-day nil
               :departure-zone "Europe/Tallinn"
               :arrival-zone "Europe/Stockholm"))
         (event (dotfiles--airbaltic-event-data leg))
         (start (alist-get "start" event nil nil #'string=))
         (end (alist-get "end" event nil nil #'string=)))
    (should (equal (alist-get "summary" event nil nil #'string=)
                   "BT101 TLL → ARN"))
    (should (equal (alist-get "dateTime" start nil nil #'string=)
                   "2031-03-14T06:25:00+02:00"))
    (should (equal (alist-get "timeZone" start nil nil #'string=)
                   "Europe/Tallinn"))
    (should (equal (alist-get "dateTime" end nil nil #'string=)
                   "2031-03-14T07:15:00+01:00"))
    (should (equal (alist-get "timeZone" end nil nil #'string=)
                   "Europe/Stockholm"))))

(ert-deftest dotfiles--airbaltic-resolve-leg-disambiguates-airports-test ()
  (let* ((leg (dotfiles--airbaltic-leg-create
               :date "2027-07-01"
               :departure-clock "10:00"
               :departure-city "Londonas"
               :arrival-clock "13:00"
               :arrival-city "Paryžius"
               :flight-number "BT100"))
         (lgw (dotfiles--airbaltic-test-airport "LGW" "Londonas"))
         (lcy (dotfiles--airbaltic-test-airport "LCY" "Londonas"))
         (cdg (dotfiles--airbaltic-test-airport "CDG" "Paryžius"))
         (airports (list lgw lcy cdg)))
    (cl-letf (((symbol-function 'dotfiles--airbaltic--fetch-timetable)
               (lambda (origin destination date)
                 (if (and (string= origin "LCY")
                          (string= destination "CDG")
                          (string= date "2027-07-01"))
                     (list
                      (dotfiles--airbaltic-test-timetable-entry
                       "BT100" "LCY" "CDG" "10:00+01:00" "13:00+02:00"
                       date))
                   nil))))
      (let ((resolved (dotfiles--airbaltic--resolve-leg leg airports)))
        (should (equal (plist-get (dotfiles--airbaltic-leg-origin resolved)
                                  :code)
                       "LCY"))
        (should (equal (dotfiles--airbaltic-leg-departure-time resolved)
                       "10:00+01:00"))))))

(ert-deftest dotfiles--airbaltic-apply-timezones-uses-open-meteo-result-test ()
  (let ((leg (dotfiles--airbaltic-leg-create
              :date "2027-07-01"
              :departure-clock "10:00"
              :arrival-clock "13:00"
              :flight-number "BT100"
              :origin (dotfiles--airbaltic-test-airport "LCY" "Londonas")
              :destination (dotfiles--airbaltic-test-airport "JFK" "Niujorkas")
              :departure-time "10:00+01:00"
              :arrival-time "13:00-04:00"
              :next-day nil)))
    (cl-letf (((symbol-function 'dotfiles--airbaltic--open-meteo-timezones)
               (lambda (_airports)
                 '(("LCY" . "Europe/London")
                   ("JFK" . "America/New_York")))))
      (let ((zoned (car (dotfiles--airbaltic--apply-timezones (list leg)))))
        (should (equal (dotfiles--airbaltic-leg-departure-zone zoned)
                       "Europe/London"))
        (should (equal (dotfiles--airbaltic-leg-arrival-zone zoned)
                       "America/New_York"))))))

(ert-deftest dotfiles--airbaltic-prepares-every-leg-before-calendar-write-test ()
  (let (calendar-write)
    (cl-letf (((symbol-function 'dotfiles--airbaltic--prepare-booking)
               (lambda (_html) (error "Second leg could not be validated")))
              ((symbol-function 'dotfiles--airbaltic--insert-events)
               (lambda (_calendar-id _legs) (setq calendar-write t))))
      (should-error
       (dotfiles--airbaltic-add-booking-to-calendar "<html>" "calendar-id")
       :type 'error)
      (should-not calendar-write))))

(provide 'my-airbaltic-test)
;;; my-airbaltic-test.el ends here
