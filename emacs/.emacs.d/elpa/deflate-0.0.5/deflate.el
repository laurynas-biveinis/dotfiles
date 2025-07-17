;;; deflate.el --- The DEFLATE compression algorithm in pure Emacs LISP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Maintainer: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: files, tools
;; Filename: deflate.el
;; Description: The DEFLATE compression algorithm in pure Emacs LISP
;; Compatibility: Tested with Emacs 25 through 30
;; Package-Version: 0.0.5
;; Package-Revision: 0.0.5-0-g4896cdf0c1d0
;; Package-Requires: ((dash "2.0.0") (emacs "25.1"))
;; Homepage: https://github.com/skuro/deflate

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The DEFLATE algorithm is specified by the RFC 1951.
;; See: https://datatracker.ietf.org/doc/html/rfc1951

;;; Change log:
;;
;; version 0.0.5, 2025-07-03 Fixed bug with single-nodes huffman trees
;; version 0.0.4, 2025-07-01 Added deflate-zlib-compress to facilitate zlib usage
;; version 0.0.3, 2025-06-12 Fixed critical bug with dynamic Huffman
;; version 0.0.2, 2025-06-11 Fixed a few warnings and bugs, preparing for actual release
;; version 0.0.1, 2025-06-11 Initial release with support for dynamic Huffman / no-compression blocks

;;; Code:

;; ---- Bit/byte level utility Functions ----

(require 'dash)

(defun deflate--bytes-to-bits (bytes)
  "Convert a list of BYTES into a list of bits (0 or 1)."
  (let ((bits '()))
    (dolist (byte bytes)
      (dotimes (i 8)
        (push (if (= (logand byte (ash 1 i)) 0) 0 1) bits)))
    (nreverse bits)))

(defun deflate--bits-to-bytes (bits)
  "Convert a list of BITS into a list of bytes.
The `car' of the list is considered to be the least position bit."
  (let ((bytes '())
        (current-byte 0)
        (bit-position 0))
    (dolist (bit bits)
      (when (= bit 1)
        (setq current-byte (logior current-byte (ash 1 bit-position))))
      (setq bit-position (1+ bit-position))
      (when (= bit-position 8)
        (push current-byte bytes)
        (setq current-byte 0
              bit-position 0)))
    ;; Handle any remaining bits
    (when (> bit-position 0)
      (push current-byte bytes))
    (nreverse bytes)))

(defun deflate--number->bits (num len)
  "Convert NUM to a list of binary digits (0 and 1) of length LEN."
  (let ((result nil))
    (while (< (length result) len)
      (push (mod num 2) result)
      (setq num (/ num 2)))
    result))

;; ---- LZ77 Compression ----

(defconst deflate--window-size 32768
  "Size of the sliding window for LZ77 compression.")

(defconst deflate--min-match 3
  "Minimum match length for LZ77 compression.")

(defconst deflate--max-match 258
  "Maximum match length for LZ77 compression.")


(defun deflate--find-match (data pos)
  "Find the longest match for DATA at POS within the sliding window.
Returns a list (offset length) or nil if no match found."
  (let ((best-length 0)
        (best-distance 0)
        (window-start (max 0 (- pos deflate--window-size)))
        (data-length (length data)))
    ;; Create a range of numbers from window-start to pos-1
    (let ((indices (-map (lambda (x) (+ x window-start))
                         (-iota (- pos window-start)))))
      ;; Process each index
      (--each indices
        (let* ((i it)  ;; Current index
               (max-match-len (min deflate--max-match (- data-length pos)))
               (match-len 0))
          ;; Find the maximum match length
          (while (and (< match-len max-match-len)
                      (< (+ pos match-len) data-length)
                      (< (+ i match-len) pos)
                      (= (nth (+ i match-len) data)
                         (nth (+ pos match-len) data)))
            (setq match-len (1+ match-len)))

          ;; Update the best match if this one is better
          (when (and (>= match-len deflate--min-match)
                     (> match-len best-length))
            (setq best-length match-len
                  best-distance (- pos i))))))

    (if (>= best-length deflate--min-match)
        (list best-length best-distance)
      nil)))

(defun deflate--lz77-compress (data)
  "Perform LZ77 compression on DATA.
Returns a list of tokens, where each token is either a literal byte
or a list (distance length) for a match."
  (let ((result '())
        (pos 0)
        (data-length (length data)))
    (while (< pos data-length)
      (let ((match (deflate--find-match data pos)))
        (if match
            (let ((length (car match))
                  (distance_ (cadr match)))
              (push (list length distance_) result)
              (setq pos (+ pos length)))
          (push (nth pos data) result)
          (setq pos (1+ pos)))))
    (nreverse result)))

;; ---- Huffman Coding ----

(defun deflate--get-length-code (length)
  "Convert a match LENGTH to the appropriate DEFLATE length code.
Returns a cons cell (code . extra-bits) where code is 257-285
and extra-bits is a cons of (num-bits . value)."
  (cond
   ;; Direct encoding for lengths 3-10 (codes 257-264)
   ((<= length 10)
    (cons (+ 257 (- length 3)) '(0 . 0)))

   ;; Lengths 11-12 (code 265, 1 extra bit)
   ((<= length 12)
    (cons 265 (cons 1 (- length 11))))

   ;; Lengths 13-14 (code 266, 1 extra bit)
   ((<= length 14)
    (cons 266 (cons 1 (- length 13))))

   ;; Lengths 15-16 (code 267, 1 extra bit)
   ((<= length 16)
    (cons 267 (cons 1 (- length 15))))

   ;; Lengths 17-18 (code 268, 1 extra bit)
   ((<= length 18)
    (cons 268 (cons 1 (- length 17))))

   ;; Lengths 19-22 (code 269, 2 extra bits)
   ((<= length 22)
    (cons 269 (cons 2 (- length 19))))

   ;; Lengths 23-26 (code 270, 2 extra bits)
   ((<= length 26)
    (cons 270 (cons 2 (- length 23))))

   ;; Lengths 27-30 (code 271, 2 extra bits)
   ((<= length 30)
    (cons 271 (cons 2 (- length 27))))

   ;; Lengths 31-34 (code 272, 2 extra bits)
   ((<= length 34)
    (cons 272 (cons 2 (- length 31))))

   ;; Lengths 35-42 (code 273, 3 extra bits)
   ((<= length 42)
    (cons 273 (cons 3 (- length 35))))

   ;; Lengths 43-50 (code 274, 3 extra bits)
   ((<= length 50)
    (cons 274 (cons 3 (- length 43))))

   ;; Lengths 51-58 (code 275, 3 extra bits)
   ((<= length 58)
    (cons 275 (cons 3 (- length 51))))

   ;; Lengths 59-66 (code 276, 3 extra bits)
   ((<= length 66)
    (cons 276 (cons 3 (- length 59))))

   ;; Lengths 67-82 (code 277, 4 extra bits)
   ((<= length 82)
    (cons 277 (cons 4 (- length 67))))

   ;; Lengths 83-98 (code 278, 4 extra bits)
   ((<= length 98)
    (cons 278 (cons 4 (- length 83))))

   ;; Lengths 99-114 (code 279, 4 extra bits)
   ((<= length 114)
    (cons 279 (cons 4 (- length 99))))

   ;; Lengths 115-130 (code 280, 4 extra bits)
   ((<= length 130)
    (cons 280 (cons 4 (- length 115))))

   ;; Lengths 131-162 (code 281, 5 extra bits)
   ((<= length 162)
    (cons 281 (cons 5 (- length 131))))

   ;; Lengths 163-194 (code 282, 5 extra bits)
   ((<= length 194)
    (cons 282 (cons 5 (- length 163))))

   ;; Lengths 195-226 (code 283, 5 extra bits)
   ((<= length 226)
    (cons 283 (cons 5 (- length 195))))

   ;; Lengths 227-257 (code 284, 5 extra bits)
   ((<= length 257)
    (cons 284 (cons 5 (- length 227))))

   ;; Length 258 (code 285, 0 extra bits)
   (t
    (cons 285 '(0 . 0)))))

;; NOTE: the formal parameter has the * character
(defun deflate--get-distance-code (distance*)
  "Convert a match DISTANCE* to the appropriate DEFLATE distance code.
Returns a cons cell (code . extra-bits) where code is 0-29
and extra-bits is a cons of (num-bits . value)."
  (cond
   ;; Direct encoding for distances 1-4 (codes 0-3)
   ((<= distance* 4)
    (cons (- distance* 1) '(0 . 0)))

   ;; Distances 5-6 (code 4, 1 extra bit)
   ((<= distance* 6)
    (cons 4 (cons 1 (- distance* 5))))

   ;; Distances 7-8 (code 5, 1 extra bit)
   ((<= distance* 8)
    (cons 5 (cons 1 (- distance* 7))))

   ;; Distances 9-12 (code 6, 2 extra bits)
   ((<= distance* 12)
    (cons 6 (cons 2 (- distance* 9))))

   ;; Distances 13-16 (code 7, 2 extra bits)
   ((<= distance* 16)
    (cons 7 (cons 2 (- distance* 13))))

   ;; Distances 17-24 (code 8, 3 extra bits)
   ((<= distance* 24)
    (cons 8 (cons 3 (- distance* 17))))

   ;; Distances 25-32 (code 9, 3 extra bits)
   ((<= distance* 32)
    (cons 9 (cons 3 (- distance* 25))))

   ;; Distances 33-48 (code 10, 4 extra bits)
   ((<= distance* 48)
    (cons 10 (cons 4 (- distance* 33))))

   ;; Distances 49-64 (code 11, 4 extra bits)
   ((<= distance* 64)
    (cons 11 (cons 4 (- distance* 49))))

   ;; Distances 65-96 (code 12, 5 extra bits)
   ((<= distance* 96)
    (cons 12 (cons 5 (- distance* 65))))

   ;; Distances 97-128 (code 13, 5 extra bits)
   ((<= distance* 128)
    (cons 13 (cons 5 (- distance* 97))))

   ;; Distances 129-192 (code 14, 6 extra bits)
   ((<= distance* 192)
    (cons 14 (cons 6 (- distance* 129))))

   ;; Distances 193-256 (code 15, 6 extra bits)
   ((<= distance* 256)
    (cons 15 (cons 6 (- distance* 193))))

   ;; Distances 257-384 (code 16, 7 extra bits)
   ((<= distance* 384)
    (cons 16 (cons 7 (- distance* 257))))

   ;; Distances 385-512 (code 17, 7 extra bits)
   ((<= distance* 512)
    (cons 17 (cons 7 (- distance* 385))))

   ;; Distances 513-768 (code 18, 8 extra bits)
   ((<= distance* 768)
    (cons 18 (cons 8 (- distance* 513))))

   ;; Distances 769-1024 (code 19, 8 extra bits)
   ((<= distance* 1024)
    (cons 19 (cons 8 (- distance* 769))))

   ;; Distances 1025-1536 (code 20, 9 extra bits)
   ((<= distance* 1536)
    (cons 20 (cons 9 (- distance* 1025))))

   ;; Distances 1537-2048 (code 21, 9 extra bits)
   ((<= distance* 2048)
    (cons 21 (cons 9 (- distance* 1537))))

   ;; Distances 2049-3072 (code 22, 10 extra bits)
   ((<= distance* 3072)
    (cons 22 (cons 10 (- distance* 2049))))

   ;; Distances 3073-4096 (code 23, 10 extra bits)
   ((<= distance* 4096)
    (cons 23 (cons 10 (- distance* 3073))))

   ;; Distances 4097-6144 (code 24, 11 extra bits)
   ((<= distance* 6144)
    (cons 24 (cons 11 (- distance* 4097))))

   ;; Distances 6145-8192 (code 25, 11 extra bits)
   ((<= distance* 8192)
    (cons 25 (cons 11 (- distance* 6145))))

   ;; Distances 8193-12288 (code 26, 12 extra bits)
   ((<= distance* 12288)
    (cons 26 (cons 12 (- distance* 8193))))

   ;; Distances 12289-16384 (code 27, 12 extra bits)
   ((<= distance* 16384)
    (cons 27 (cons 12 (- distance* 12289))))

   ;; Distances 16385-24576 (code 28, 13 extra bits)
   ((<= distance* 24576)
    (cons 28 (cons 13 (- distance* 16385))))

   ;; Distances 24577-32768 (code 29, 13 extra bits)
   (t
    (cons 29 (cons 13 (- distance* 24577))))))

(defun deflate--huffman-encode-token (token ll-codes dd-codes)
  "Encode TOKEN using Huffman codes.
Codes are provided separately for literal/length (as LL-CODES) and
distance (as DD-CODES).
Returns a list of alists of `code', `code-length', `num-extra-bits' and
`extra-bits-value'."
  (let ((result '()))
    (if (listp token)
        ;; This is a length-distance pair
        (let* (;; process length first
               (length-original (car token))
               (length-spec (deflate--get-length-code length-original))
               (length (car length-spec))
               (length-extra-bits (cdr length-spec))
               (length-code (car (gethash length ll-codes)))
               (length-code-length (cdr (gethash length ll-codes)))

               ;; then process distance
               (distance-original (cadr token))
               (distance-spec (deflate--get-distance-code distance-original))
               (distance* (car distance-spec))
               (distance-extra-bits (cdr distance-spec))
               (distance-code (car (gethash distance* dd-codes)))
               (distance-code-length (cdr (gethash distance* dd-codes))))

          ;; encode length
          (let* ((extra-bits (car length-extra-bits))
                 (extra-bits-value (cdr length-extra-bits))
                 (length-alist `((code . ,length-code)
                                 (code-length . ,length-code-length)
                                 (num-extra-bits . ,extra-bits)
                                 (extra-bits-value . ,extra-bits-value))))
            (setq result (append result (list length-alist))))

          ;; encode distance
          (let* ((extra-bits (car distance-extra-bits))
                 (extra-bits-value (cdr distance-extra-bits))
                 (distance-alist `((code . ,distance-code)
                                   (code-length . ,distance-code-length)
                                   (num-extra-bits . ,extra-bits)
                                   (extra-bits-value . ,extra-bits-value))))
            (setq result (append result (list distance-alist))))
          result)

      ;; This is a literal token
      (let* ((code-spec (gethash token ll-codes))
             (code (car code-spec))
             (code-length (cdr code-spec))
             (literal-alist `((code . ,code)
                              (code-length . ,code-length))))

        (append result (list literal-alist))))))

;; ---- Dynamic Huffman DEFLATE Implementation ----

(defun deflate--build-huffman-tree (freq-alist)
  "Build a Huffman tree from FREQ-ALIST.
Returns the root node of the tree."
  ;; we start by copying the `freq-alist' into a new list
  (let ((heap (mapcar (lambda (pair) (cons (car pair) (cdr pair)))
                      freq-alist)))
    (while (> (length heap) 1)
      ;; Sort heap by frequency ascending
      (setq heap (--sort (< (cdr it) (cdr other)) heap))
      ;; Take two lowest freq nodes
      (let* ((a (pop heap))
             (b (pop heap))
             (merged (cons (list a b) (+ (cdr a) (cdr b)))))
        ;; add a new node to the tree as a cons of `((left right) . sum-of-freqs)'
        (push merged heap)))
    ;; Return the single tree
    (car heap)))

(defun deflate--build-frequency-table (tokens)
  "Build a frequency table from TOKENS.
Returns an hash table of keys `literal-length' and `distance', with alists of
`(symbol . frequency)' as values."
  (let ((literal-length-freq-table (make-hash-table))
        (distance-freq-table (make-hash-table)))

    ;; Process each token
    (dolist (token tokens)
      (if (listp token)
          ;; This is a length-distance pair
          (let* ((length (car token))
                 (distance_ (cadr token))
                 ;; Get length code and extra bits according to DEFLATE spec
                 (length-result (deflate--get-length-code length))
                 (length-code (car length-result))
                 ;; Get distance code and extra bits according to DEFLATE spec
                 (distance-result (deflate--get-distance-code distance_))
                 (distance-code (car distance-result)))

            ;; Update length code frequency
            (puthash length-code (1+ (gethash length-code literal-length-freq-table 0)) literal-length-freq-table)

            ;; Update distance code frequency (offset by 286)
            (let ((dist-symbol distance-code))
              (puthash dist-symbol (1+ (gethash dist-symbol distance-freq-table 0)) distance-freq-table)))

        ;; This is a literal byte
        (puthash token (1+ (gethash token literal-length-freq-table 0)) literal-length-freq-table)))

    ;; Add EOF symbol
    (puthash 256 1 literal-length-freq-table)

    ;; Convert hash tables to alists
    (let ((literal-length-result '())
          (distance-result '())
          (result (make-hash-table)))
      (maphash (lambda (k v) (push (cons k v) literal-length-result)) literal-length-freq-table)
      (maphash (lambda (k v) (push (cons k v) distance-result)) distance-freq-table)
      (puthash 'literal-length literal-length-result result)
      (puthash 'distance distance-result result)

      result)))

(defun deflate--build-huffman-code-lengths (tree)
  "Build Huffman codes from TREE.
Returns an alist of `(symbol . length)' where `length' is the depth of `symbol'
in the `tree'.
Returns nil if TREE is nil."
  (when tree
    (letrec ((walk (lambda (node depth)
                     (if (consp (car node))
                         ;; internal node, e.g. `(((7 . 1) (5 . 1)) . 2)'
                         (append (funcall walk (car (car node)) (1+ depth))
                                 (funcall walk (cadr (car node)) (1+ depth)))
                       ;; leaf node, e.g. `(7 . 1)'
                       (list (cons (car node) (max 1 depth)))))))
      (funcall walk tree 0))))

(defun deflate--assign-huffman-codes (code-lengths)
  "Assign canonical Huffman codes from the CODE-LENGTHS alist.
Returns a map of `symbol' -> `(code . length)' where `code' is an integer."
  (let ((table (make-hash-table :test #'eq)))
    (if code-lengths
        (let* ((max-len (apply #'max (mapcar #'cdr code-lengths)))
               (bl-count (make-vector (1+ max-len) 0))
               (next-code (make-vector (1+ max-len) 0))
               (code 0))
          ;; Count number of codes for each length
          (dolist (pair code-lengths)
            (let ((len (cdr pair)))
              (when (> len 0)
                (aset bl-count len (1+ (aref bl-count len))))))
          ;; Compute starting code for each length
          (dotimes (bits max-len)
            (setq code (ash (+ code (aref bl-count bits)) 1))
            (aset next-code (1+ bits) code))
          ;; Sort symbols by (length . symbol)
          (dolist (pair (sort code-lengths
                              (lambda (a b)
                                (if (= (cdr a) (cdr b))
                                    (< (car a) (car b))
                                  (< (cdr a) (cdr b)))))
                        table)
            (let ((sym (car pair))
                  (len (cdr pair)))
              (when (> len 0)
                (let ((code (aref next-code len)))
                  (puthash sym (cons code len) table)
                  (aset next-code len (1+ code)))))))
      table)))

(defun deflate--encode-code-lengths-to-alphabet (code-lengths)
  "Encode the sequence of CODE-LENGTHS into the RLE alphabet, RFC 3.2.7.
Returns a list of (token . (num-extra-bits . extra-bits-value)) where token
comes from the custom alphabet and extra-bits-value is an integer."
  (let ((result '())
        (i 0)
        (len (length code-lengths)))
    (while (< i len)
      (let* ((current-length (nth i code-lengths))
             (run-length 1))

        ;; Count consecutive identical values
        (while (and (< (+ i run-length) len)
                    (= current-length (nth (+ i run-length) code-lengths)))
          (setq run-length (1+ run-length)))

        (cond
         ;; Handle zeros (use codes 17 and 18)
         ((= current-length 0)
          (cond
           ;; Use code 17 for runs of 3-10 zeros
           ((<= 3 run-length 10)
            (push (cons 17 (cons 3 (- run-length 3))) result)
            (setq i (+ i run-length)))
           ;; Use code 18 for runs of 11-138 zeros
           ((<= 11 run-length 138)
            (push (cons 18 (cons 7 (- run-length 11))) result)
            (setq i (+ i run-length)))
           ;; Use code 18 with full extra bits for runs of more than 138 zeros
           ((< 138 run-length)
            (push (cons 18 (cons 7 127)) result) ;; 127 'cause 138 - 11
            (setq i (+ i 138)))
           ;; For runs of 1-2 zeros or very short runs, just output individual zeros
           (t
            (let ((zeros-to-output (min run-length 2)))
              (dotimes (_ zeros-to-output)
                (push (cons 0 (cons 0 0)) result))
              (setq i (+ i zeros-to-output))))))

         ;; Handle non-zero values (use code 16 for repetitions)
         (t
          ;; Always output the first occurrence
          (push (cons current-length (cons 0 0)) result)
          (setq i (1+ i))
          (setq run-length (1- run-length))

          ;; Handle repetitions with code 16 (3-6 repetitions)
          (while (>= run-length 3)
            (let ((reps (min 6 run-length)))
              (push (cons 16 (cons 2 (- reps 3))) result)
              (setq run-length (- run-length reps))
              (setq i (+ i reps))))

          ;; Handle remaining 1-2 repetitions by outputting individual values
          (dotimes (_ run-length)
            (push (cons current-length (cons 0 0)) result)
            (setq i (1+ i)))))))

    (setq result (nreverse result))
    result))


(defun deflate--calculate-hlit-length (code-lengths-array)
  "Calculate the base length for the HLIT header.
The literal/lengths are given as CODE-LENGTHS-ARRAY whose indices are symbols
and values are code lengths."
  (let* ((hlit-min 257)
         (hlit-max-index (-find-last-index (lambda (x) (/= x 0))
                                           ;; -find-last-index requires a list
                                           (append code-lengths-array nil)))
         ;; HLIT is the count of codes (index + 1), but for literal/length codes we need at least 257 codes
         ;; From the spec:
         (hlit (if hlit-max-index
                   (max hlit-min (1+ hlit-max-index))
                 hlit-min)))
    hlit))

(defun deflate--calculate-hdist-length (code-lengths-array)
  "Calculate the base length for HDIST header.
The distances ar given as CODE-LENGTHS-ARRAY whose indices are symbols
and values are code lenghts."
  (let* ((hdist-min 1)
         (hdist-max-index (-find-last-index (lambda (x) (/= x 0))
                                            ;; -find-last-index requires a list
                                            (append code-lengths-array nil)))
         ;; HDIST is the count of codes (index + 1), but we need at least 1 distance code
         (hdist (if hdist-max-index
                    (max hdist-min (1+ hdist-max-index))
                  hdist-min)))
    hdist))

(defconst deflate--code-lengths-order
  '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)
  "Code length alphabet order as per DEFLATE spec.")

(defun deflate--calculate-hclen-length (cl-lengths-array)
  "Calculate HCLEN from the CL-LENGTHS-ARRAY."
  (let* ((hclen-min 4)
         (code-lengths-ordered-lengths (mapcar (lambda (i) (aref cl-lengths-array i))
                                               deflate--code-lengths-order))
         (last-nonzero-index (-find-last-index (lambda (x) (/= x 0)) code-lengths-ordered-lengths))
         ;; HCLEN, # of Code Length codes
         (hclen (if last-nonzero-index
                    (max hclen-min (1+ last-nonzero-index))
                  hclen-min)))
    hclen))

(defun deflate--pack-bits (bitstream bits &optional invert)
  "Packs BITS into the BITSTREAM, optionally inverting the bits if INVERT is t."
  (let ((bits-to-pack (if invert bits (seq-reverse bits))))
    (append bitstream bits-to-pack)))

(defun deflate--write-dynamic-header (bitstream final hlit hdist hclen cl-lengths-array)
  "Write the DEFLATE header into BITSTREAM.
The (meta) Huffman parameters HLIT / HDIST / HCLEN use the standard bit length.
The CL-LENGTHS-ARRAY array contains the code lengths for the code length
alphabet.
If the FINAL flag is non-nil it sets the BFINAL flag to 1."
  ;; Block header:
  ;; - First bit: Final block flag (1 for final block)
  (if final
      (setq bitstream (deflate--pack-bits bitstream '(1)))
    (setq bitstream (deflate--pack-bits bitstream '(0))))

  ;; - Next 2 bits: Block type (10 for dynamic Huffman)
  (setq bitstream (deflate--pack-bits bitstream '(1 0)))

  ;; Encode HLIT (5 bits)
  (setq bitstream
        (deflate--pack-bits
         bitstream
         (deflate--number->bits hlit 5)))

  ;; Encode HDIST (5 bits)
  (setq bitstream
        (deflate--pack-bits
         bitstream
         (deflate--number->bits hdist 5)))

  ;; Encode HCLEN (4 bits)
  (setq bitstream
        (deflate--pack-bits
         bitstream
         (deflate--number->bits hclen 4)))

  ;; Encode code lengths for the code length alphabet
  (dotimes (i (+ hclen 4))
    (let ((cl-index (nth i deflate--code-lengths-order)))
      (setq bitstream
            (deflate--pack-bits
             bitstream
             (deflate--number->bits
              (aref cl-lengths-array cl-index) 3)))))
  bitstream)

(defun deflate--write-huffman-code (bitstream code code-length &optional num-extra-bits extra-bits-value)
  "Write a single Huffman CODE into the BITSTREAM in invenrted bit order.
The CODE is going to be exactly CODE-LENGTH bits.
Optionally adds EXTRA-BITS-VALUE as a sequence of NUM-EXTRA-BITS bits."
  (setq bitstream (deflate--pack-bits bitstream
                                      (deflate--number->bits code code-length)
                                      t)) ;; <- huffman codes are written in MSB order
  (when (and num-extra-bits
             (> num-extra-bits 0))
    (let ((extra-bits (deflate--number->bits extra-bits-value num-extra-bits)))
      (setq bitstream (deflate--pack-bits bitstream
                                          extra-bits
                                          nil) ;; <- extra bits are written in LSB order
            )))
  bitstream)

(defun deflate--write-code-lengths (bitstream cl-encoded cl-huff-codes)
  "Write the Huffman code lengths into the BITSTREAM.
Code lengths are provided in the CL-ENCODED alist of
`(code-length . (num-extra-bits . extra-bits-value)'.
The CL-HUFF-CODES hashmap contains the Huffman codes for each code length."
  ;; Encode the code length sequence
  (dolist (cl-spec cl-encoded)
    (let* ((cl (car cl-spec)) ;; cons of (code-length . (num-extra-bits . extra-bits-value))
           (num-extra-bits (cadr cl-spec))
           (extra-bits-value (cddr cl-spec))
           (code-spec (gethash cl cl-huff-codes))
           (code (car code-spec))
           (code-length (cdr code-spec)))
      (setq bitstream
            (deflate--write-huffman-code bitstream
                                                       code
                                                       code-length
                                                       num-extra-bits
                                                       extra-bits-value))))
  bitstream)

(defun deflate--write-compressed-data (bitstream encoded-tokens)
  "Write compressed data into BITSTREAM.
ENCODED-TOKENS is a list of alists which represents either literal,
lengths or distances."
  (dolist (encoded-token encoded-tokens)
    (let* ((code (cdr (assoc 'code encoded-token)))
           (code-length (cdr (assoc 'code-length encoded-token)))
           (num-extra-bits (cdr (assoc 'num-extra-bits encoded-token)))
           (extra-bits-value (cdr (assoc 'extra-bits-value encoded-token))))
      (setq bitstream
            (deflate--write-huffman-code bitstream
                                                       code
                                                       code-length
                                                       num-extra-bits
                                                       extra-bits-value))))
  bitstream)

(defun deflate--encode-dynamic-huffman-block (lz77-tokens &optional final)
  "Encode LZ77-TOKENS using dynamic Huffman coding.
Header and compressed data is packed in LSB order, while Huffman codes in MSB.
Returns a list of bits representing the compressed data for a DEFLATE block.
If FINAL is non-nil it sets the BFINAL flag to 1 to signal it's the last block."
  (let* ((final (or final t))
         (freq-table (deflate--build-frequency-table lz77-tokens))

         ;; build the Huffman codes for literals/lengths
         (ll-huff-tree (deflate--build-huffman-tree (gethash 'literal-length freq-table)))
         (ll-code-lengths (deflate--build-huffman-code-lengths ll-huff-tree))
         (ll-huff-code (deflate--assign-huffman-codes ll-code-lengths))

         ;; build the Huffman codes for distances
         (dd-huff-tree (deflate--build-huffman-tree (gethash 'distance freq-table)))
         (dd-code-lengths (deflate--build-huffman-code-lengths dd-huff-tree))
         (dd-huff-code (deflate--assign-huffman-codes dd-code-lengths))

         ;; Encode all LZ77 tokens using the above Huffman codes
         (encoded-tokens (-mapcat (lambda (token)
                                    (deflate--huffman-encode-token token ll-huff-code dd-huff-code))
                                  lz77-tokens)))

    (let* ((bitstream '())

           ;; Build code length arrays (max 286 literal/length codes, max 30 distance codes)
           (ll-code-lengths-array (make-vector 286 0))
           (dd-code-lengths-array (make-vector 30 0)))

      ;; Fill code length arrays from the hash tables
      (dolist (pair ll-code-lengths)
        (aset ll-code-lengths-array (car pair) (cdr pair)))
      (dolist (pair dd-code-lengths)
        (aset dd-code-lengths-array (car pair) (cdr pair)))

      ;; Find the count (ie index + 1) of the highest non-zero code for each alphabet:
      (let* (;; - HLIT, amount of Literal/Length codes - 257 (257 - 286)
             (hlit-length (deflate--calculate-hlit-length ll-code-lengths-array))
             (hlit (- hlit-length 257))

             ;; - HDIST, amount of Distance codes - 1        (1 - 32)
             (hdist-length (deflate--calculate-hdist-length dd-code-lengths-array))
             (hdist (- hdist-length 1)))

        ;; Build combined code length sequence for encoding
        (let* ((ll-slice (mapcar (lambda (i) (aref ll-code-lengths-array i))
                                 (number-sequence 0 (1- hlit-length)))) ;; we need the index here, but `hlit' is a count
               (dd-slice (mapcar (lambda (i) (aref dd-code-lengths-array i))
                                 (number-sequence 0 (1- hdist-length)))) ;; we need the index here, but `hdist' is a count
               (code-lengths-combined (append ll-slice dd-slice))
               ;; Build frequency table for code lengths
               (cl-freq-table (make-hash-table))
               ;; RLE encoded code lengths
               (cl-encoded (deflate--encode-code-lengths-to-alphabet code-lengths-combined)))

          ;; Count frequencies of encoded code lengths
          (dolist (cl (mapcar #'car cl-encoded))
            (puthash cl (1+ (gethash cl cl-freq-table 0)) cl-freq-table))

          ;; Build Huffman tree for encoded code lengths
          (let* ((cl-freq-alist (let (result)
                                  (maphash (lambda (k v) (push (cons k v) result)) cl-freq-table)
                                  result))
                 (cl-huff-tree (deflate--build-huffman-tree cl-freq-alist))
                 (cl-code-lengths (deflate--build-huffman-code-lengths cl-huff-tree))
                 (cl-huff-codes (deflate--assign-huffman-codes cl-code-lengths))
                 ;; Build code length array for transmission
                 (cl-lengths-array (make-vector 19 0)))

            ;; Fill code length array
            (dolist (pair cl-code-lengths)
              (let ((sym (car pair))
                    (len (cdr pair)))
                (when (or (< sym 0) (>= sym 19))
                  (error "DEFLATE: illegal encoded code length code: %s" sym))
                (aset cl-lengths-array sym len)))

            (let* (;; - HCLEN, amount of code lengths codes - 4 (4 - 19)
                   (hclen-length (deflate--calculate-hclen-length cl-lengths-array))
                   (hclen (- hclen-length 4)))

              ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
              ;; We now have all of the data that needs to be put into the bitstream
              ;;
              ;; From the spec:
              ;;
              ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
              ;;
              ;; We can now define the format of the block:

              ;; 5 Bits: HLIT, # of Literal/Length codes - 257 (257 - 286)  (a)
              ;; 5 Bits: HDIST, # of Distance codes - 1        (1 - 32)     (b)
              ;; 4 Bits: HCLEN, # of Code Length codes - 4     (4 - 19)     (c)

              ;; (HCLEN + 4) x 3 bits: code lengths for the code length     (d)
              ;;    alphabet given just above, in the order: 16, 17, 18,
              ;;    0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15

              ;;    These code lengths are interpreted as 3-bit integers
              ;;    (0-7); as above, a code length of 0 means the
              ;;    corresponding symbol (literal/length or distance code
              ;;    length) is not used.

              ;; HLIT + 257 code lengths for the literal/length alphabet,   (e)
              ;;    encoded using the code length Huffman code

              ;; HDIST + 1 code lengths for the distance alphabet,          (f)
              ;;    encoded using the code length Huffman code

              ;; The actual compressed data of the block,                   (g)
              ;;    encoded using the literal/length and distance Huffman
              ;;    codes

              ;; The literal/length symbol 256 (end of data),               (h)
              ;;    encoded using the literal/length Huffman code
              ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

              ;; the header takes care of points a / b / c / d
              (setq bitstream (deflate--write-dynamic-header bitstream final hlit hdist hclen cl-lengths-array))

              ;; this is for points e / f
              (setq bitstream (deflate--write-code-lengths bitstream cl-encoded cl-huff-codes))

              ;; Encode the actual data tokens -- point g
              (setq bitstream (deflate--write-compressed-data bitstream encoded-tokens))

              ;; Add EOF symbol (256) -- point h
              (let* ((eof-code-spec (gethash 256 ll-huff-code))
                     (eof-code (car eof-code-spec))
                     (eof-code-length (cdr eof-code-spec)))
                (setq bitstream (deflate--write-huffman-code bitstream eof-code eof-code-length)))

              bitstream)))))))

;; ---- No-compression type ----

(defun deflate--encode-none-block (data final)
  "Writes out DATA as a non-compressed DEFLATE block (BTYPE=00).
When FINAL is non-nil (default) the block is marked as final."
  (let* ((final (or final t))
         (bitstream '())

         ;; LEN: amount of data bytes in the block
         (len (length data))

         ;; NLEN: 1-complement of LEN
         (nlen (- #xFFFF len)))
    ;; Block header:
    ;; - First bit: Final block flag (1 for final block)
    (if final
        (setq bitstream (deflate--pack-bits bitstream '(1)))
      (setq bitstream (deflate--pack-bits bitstream '(0))))

    ;; - Next 2 bits: Block type (00 for no compression)
    (setq bitstream (deflate--pack-bits bitstream '(0 0)))

    ;; - Next 5 bits: Padding (need to get to byte boundary)
    (setq bitstream (deflate--pack-bits bitstream (-repeat 5 0)))

    ;; - Next 16 bits: LEN
    (setq bitstream (deflate--pack-bits bitstream (deflate--number->bits len 16)))

    ;; - Next 16 bits: NLEN
    (setq bitstream (deflate--pack-bits bitstream (deflate--number->bits nlen 16)))

    ;; - Finally, the raw, uncompressed data bytes
    (dolist (data-byte data)
      (setq bitstream (deflate--pack-bits bitstream (deflate--number->bits data-byte 8))))

    bitstream))

;; ---- / ----

(defun deflate-compress--dynamic (data final)
  "Compress DATA into a block using the Dynamic Huffman coding DEFLATE variant.
See the RFC paragraph 3.2.7.
When FINAL is non-nil the block is marked as final."
  (let* ((lz77-tokens (deflate--lz77-compress data))
         ;; Use dynamic Huffman coding
         (compressed-bits (deflate--encode-dynamic-huffman-block lz77-tokens final)))
    compressed-bits))

(defun deflate-compress--static (_ _)
  "Compress DATA using the Static Huffman coding DEFLATE variant.
See the RFC paragraph 3.2.6.
When FINAL is non-nil the block is marked as final."
  (error "Static huffman codes are not supported yet (see https://github.com/skuro/deflate/issues/1)"))

(defun deflate-compress--none (data final)
  "Writes DATA using the non-compressed block DEFLATE variant.
See the RFC paragraph 3.2.4.
When FINAL is non-nil the block is marked as final."
  (deflate--encode-none-block data final))

;; ---- Public API follows ----

;;;###autoload
(defun deflate-compress (data &optional compression-type final)
  "Compress DATA using the DEFLATE algorithm.
DATA should be a string or a vector of bytes.
Returns a vector of compressed bytes.
COMPRESSION-TYPE is one of the following:
  `'dynamic' (default) - Use dynamic Huffman coding
  `'static' - Use static Huffman coding
  `'none' - Store without compression.
If FINAL is non-nil (default) it produces a final block (BFINAL=1)."
  (when (stringp data)
    (setq data (string-to-list data)))

  (when (> (length data) deflate--window-size)
    (error "Data cannot be longer than 32k"))

  ;; Perform LZ77 compression
  (let* ((final (or final t))
         (compression-type (or compression-type 'dynamic))
         (compressed-bits (cond ((eq compression-type 'dynamic) (deflate-compress--dynamic data final))
                                ((eq compression-type 'static) (deflate-compress--static data final))
                                ((eq compression-type 'none) (deflate-compress--none data final))
                                (t (error "Invalid compression type: %s" compression-type)))))
    (deflate--bits-to-bytes compressed-bits)))

;; ---- Minimal ZLIB compatibility layer Implementation ----

(defun deflate-zlib-adler32 (data)
  "Calculate Adler-32 checksum for DATA (string or list of bytes).
Returns a 4-bytes list checksum compatible with the zlib format."
  (let ((a 1)
        (b 0)
        (mod-adler 65521))
    (dolist (byte (if (stringp data)
                      (string-to-list data)
                    data))
      (setq a (% (+ a byte) mod-adler))
      (setq b (% (+ b a) mod-adler)))
    (let ((checksum (logior (ash b 16) a)))
      (list (logand (ash checksum -24) 255)
            (logand (ash checksum -16) 255)
            (logand (ash checksum -8) 255)
            (logand checksum 255)))))

(defconst deflate--zlib-cmf #x78
  "The CMF header byte for zlib compatibiliy.
CM=8 for DEFLATE, CINFO=7 for 32KB window.")

(defconst deflate--zlib-flg #x9C
  "The FLG header byte for zlib compatibility.
Chosen so that CMF*256 + FLG is divisible by 31).")

(defconst deflate-zlib-header
  (list deflate--zlib-cmf
        deflate--zlib-flg)
  "The fixed zlib compatibility header.")

(defun deflate-zlib-compress (instr block-type)
  "Compress INSTR using DEFLATE BLOCK-TYPE then add the zlib envelope."
  (let* ((block-type (or block-type 'dynamic))
         (compressed-bytes (deflate-compress instr block-type))
         (adler32 (deflate-zlib-adler32 instr)))
    (append deflate-zlib-header
            compressed-bytes
            adler32)))

;; ---- Only useful for debugging purposes ----

(defun deflate--debug (instr outpath &optional block-type)
  "Compresses instr and writes the result into OUTPATH for debugging purposes.
The INSTR string is compressed with DEFLATE and the bytes are stored in the file
 at OUTPATH.
The file at OUTPATH can be inspected with tools such as `infgen':
https://github.com/madler/infgen.
BLOCK-TYPE is one of `'dymamic', `'static' or `'none'."
  (with-temp-file outpath
      (set-buffer-file-coding-system 'binary)
      (dolist (byte (deflate-zlib-compress instr block-type))
        (insert-byte byte 1))))

(provide 'deflate)
;;; deflate.el ends here
