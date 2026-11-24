;;;; decode.lisp --- the decoder: base64url, JSON, the description of a token.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The half of the cell that touches no seam: a token in, the description of
;;;; its header, its claims, when it expires and what is worth warning about
;;;; out, and JWT-ERROR with the reason when it is not a token. Nothing here
;;;; verifies a signature, follows a key set URL or reads a file; the
;;;; description is the whole product. Every function that depends on the
;;;; current time takes it as an argument, so a test pins the clock.

(in-package #:nodecode-jwt)

(nlk:define-peripheral jwt :not-running t)

;;; --- the bounds the original hard-coded ------------------------------------------

(defparameter +max-token-chars+ 16384
  "A token longer than this is refused, not decoded.")

(defparameter +max-claims+ 60
  "Claims listed; the rest are counted.")

(defparameter +max-value-chars+ 200
  "One value's JSON is cut after this many characters.")

(defparameter +skew-seconds+ 60
  "Seconds of clock difference that are not worth a warning.")

(defparameter +day+ 86400
  "Seconds in a day, for a span's units.")

(defparameter +ms-like+ 100000000000
  "A NumericDate this large is milliseconds by mistake: year 5138 in seconds.")

(defparameter +max-depth+ 32
  "How deep a header or a payload may nest before it stops being worth reading.")

;;; --- what the names mean ---------------------------------------------------------

(defparameter +algorithms+
  '(("HS256" . "HMAC with SHA-256")
    ("HS384" . "HMAC with SHA-384")
    ("HS512" . "HMAC with SHA-512")
    ("RS256" . "RSA PKCS#1 v1.5 with SHA-256")
    ("RS384" . "RSA PKCS#1 v1.5 with SHA-384")
    ("RS512" . "RSA PKCS#1 v1.5 with SHA-512")
    ("PS256" . "RSA-PSS with SHA-256")
    ("PS384" . "RSA-PSS with SHA-384")
    ("PS512" . "RSA-PSS with SHA-512")
    ("ES256" . "ECDSA P-256 with SHA-256")
    ("ES384" . "ECDSA P-384 with SHA-384")
    ("ES512" . "ECDSA P-521 with SHA-512")
    ("ES256K" . "ECDSA secp256k1 with SHA-256")
    ("EdDSA" . "Edwards-curve signature (Ed25519 or Ed448)")
    ("none" . "no signature"))
  "The algorithms a row spells out, as RFC 7518 names them.")

(defparameter +names+
  '(("alg" . "algorithm") ("typ" . "type") ("cty" . "content type") ("kid" . "key id")
    ("enc" . "encryption") ("zip" . "compression") ("jku" . "key set URL")
    ("jwk" . "embedded key") ("x5u" . "certificate URL") ("x5c" . "certificate chain")
    ("x5t" . "certificate thumbprint") ("crit" . "must understand")
    ("iss" . "issuer") ("sub" . "subject") ("aud" . "audience") ("exp" . "expires")
    ("nbf" . "not before") ("iat" . "issued at") ("jti" . "token id")
    ("auth_time" . "signed in") ("azp" . "authorized party") ("nonce" . "nonce")
    ("sid" . "session id") ("scope" . "scopes") ("updated_at" . "profile updated"))
  "A name, and what it means in the header (RFC 7515, RFC 7516) or the claims
(RFC 7519, OpenID Connect).")

(defparameter +time-claims+ '("exp" "nbf" "iat" "auth_time" "updated_at")
  "The claims whose value is a NumericDate, shown as a date as well.")

(defparameter +key-pointers+ '("jku" "x5u" "jwk")
  "Header fields that point at the token's own key.")

;;; --- a token ---------------------------------------------------------------------

(defstruct (token (:constructor make-token (&key raw header claims payload signature
                                                 encrypted duplicates parts)))
  "One decoded token: RAW as it was written, HEADER as an object, CLAIMS as an
object or NIL, PAYLOAD and SIGNATURE as octets, ENCRYPTED when it is a JWE,
DUPLICATES the names that appear twice, and PARTS how many dotted parts it had."
  (raw "" :type string :read-only t)
  (header '(:object) :read-only t)
  (claims nil :read-only t)
  (payload nil :read-only t)
  (signature nil :read-only t)
  (encrypted nil :read-only t)
  (duplicates nil :read-only t)
  (parts 3 :type integer :read-only t))

(defun octets (&rest bytes)
  "BYTES as a simple octet vector."
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(defun noted (notes text)
  "Push TEXT onto the growable NOTES vector, unless it is already there."
  (unless (find text notes :test #'string=)
    (vector-push-extend text notes))
  text)

;;; --- base64url -------------------------------------------------------------------

(defparameter +base64url-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  "The URL-safe alphabet a JWT's parts are written in, as RFC 7515 §2 writes it.")

(defun base64url-value (character)
  "CHARACTER's six bits, or NIL when it is not in the alphabet."
  (when character
    (position character +base64url-alphabet+ :test #'char=)))

(defun base64url-octets (part what)
  "PART decoded as base64url, WHAT naming it in the reason; JWT-ERROR when it is not."
  (let* ((length-of (length part))
         (body (or (position #\= part) length-of)))
    (loop for index from body below length-of
          unless (char= (char part index) #\=)
            do (fail "the ~a is not base64url: only A-Z, a-z, 0-9, '-' and '_' belong in a JWT, and '=' only at the end"
                     what))
    (let* ((stripped (subseq part 0 body))
           (size (length stripped)))
      (loop for character across stripped
            unless (base64url-value character)
              do (fail "the ~a is not base64url: only A-Z, a-z, 0-9, '-' and '_' belong in a JWT"
                       what))
      (when (= 1 (mod size 4))
        (fail "the ~a is not valid base64url: its length (~d) cannot be decoded" what size))
      (let ((out (make-array (* 3 (ceiling size 4)) :element-type '(unsigned-byte 8)
                                                  :fill-pointer 0)))
        (loop for index from 0 below size by 4
              for group = (min 4 (- size index))
              for a = (base64url-value (char stripped index))
              for b = (and (> group 1) (base64url-value (char stripped (1+ index))))
              for c = (and (> group 2) (base64url-value (char stripped (+ index 2))))
              for d = (and (> group 3) (base64url-value (char stripped (+ index 3))))
              do (vector-push (logior (ash a 2) (ash b -4)) out)
                 (when c
                   (vector-push (logior (ash (logand b #x0F) 4) (ash c -2)) out))
                 (when d
                   (vector-push (logior (ash (logand c #x03) 6) d) out)))
        (coerce out '(simple-array (unsigned-byte 8) (*)))))))

;;; --- bytes as text ---------------------------------------------------------------

(defun utf-8-code-point (data index width first-byte)
  "The code point WIDTH bytes of DATA from INDEX spell, or NIL when one of them
is not a continuation byte, the form is overlong, or the point is no character."
  (flet ((tail (offset)
           (let ((byte (aref data (+ index offset))))
             (and (<= #x80 byte #xBF) (logand byte #x3F)))))
    (let ((code (case width
                  (1 first-byte)
                  (2 (let ((b (tail 1)))
                       (and b (logior (ash (logand first-byte #x1F) 6) b))))
                  (3 (let ((b (tail 1)))
                       (and b (let ((c (tail 2)))
                                (and c (logior (ash (logand first-byte #x0F) 12)
                                               (ash b 6) c))))))
                  (4 (let ((b (tail 1)))
                       (and b (let ((c (tail 2)))
                                (and c (let ((d (tail 3)))
                                         (and d (logior (ash (logand first-byte #x07) 18)
                                                        (ash b 12) (ash c 6) d))))))))
                  (t nil))))
      (when (and code
                 (case width (1 t) (2 (>= code #x80)) (3 (>= code #x800)) (t (>= code #x10000)))
                 (not (<= #xD800 code #xDFFF))
                 (<= code #x10FFFF))
        code))))

(defun utf-8-text (data)
  "DATA as a string, and whether its bytes are well-formed UTF-8."
  (let ((characters '())
        (index 0)
        (length (length data)))
    (loop while (< index length)
          do (let* ((first-byte (aref data index))
                    (width (cond ((<= first-byte #x7F) 1)
                                 ((<= #xC2 first-byte #xDF) 2)
                                 ((<= #xE0 first-byte #xEF) 3)
                                 ((<= #xF0 first-byte #xF4) 4)
                                 (t 0))))
               (unless (and (plusp width) (<= (+ index width) length))
                 (return-from utf-8-text (values nil nil)))
               (let ((code (utf-8-code-point data index width first-byte)))
                 (unless code (return-from utf-8-text (values nil nil)))
                 (push (code-char code) characters))
               (incf index width)))
    (values (coerce (nreverse characters) 'string) t)))

;;; --- a JSON reader ---------------------------------------------------------------
;;; Python's json module is the original's only dependency; this reads the same
;;; values, keeps a name's order, and reports a name that appears twice - which
;;; is the one thing a decoded token says about its writer that a hash table
;;; would lose.

(define-condition json-syntax (error)
  ((detail :initarg :detail :reader json-syntax-detail))
  (:report (lambda (condition stream) (write-string (json-syntax-detail condition) stream))))

(defun json-fail (control &rest arguments)
  "Signal a JSON syntax error, saying what is wrong."
  (error 'json-syntax :detail (apply #'format nil control arguments)))

(defstruct (json-reader (:constructor make-json-reader (text)))
  "A reader over TEXT: a value at a time, its position kept."
  (text "" :type string)
  (index 0 :type fixnum))

(defun json-at (reader)
  "The character the reader is at, or NIL at its end."
  (let ((index (json-reader-index reader)))
    (and (< index (length (json-reader-text reader)))
         (char (json-reader-text reader) index))))

(defun json-skip-space (reader)
  "Past the whitespace a JSON value may be wrapped in."
  (loop while (member (json-at reader) '(#\Space #\Tab #\Newline #\Return))
        do (incf (json-reader-index reader))))

(defparameter +json-escapes+
  '((#\" . #\") (#\\ . #\\) (#\/ . #\/) (#\b . #\Backspace) (#\f . #\Page)
    (#\n . #\Newline) (#\r . #\Return) (#\t . #\Tab))
  "The one-character escapes a JSON string may carry.")

(defun json-parse-hex4 (reader)
  "A \\uXXXX escape's code point, a surrogate pair read as one character."
  (let ((unit (json-hex reader 4)))
    (cond ((<= #xD800 unit #xDBFF)
           (unless (and (eql (json-at reader) #\\)
                        (progn (incf (json-reader-index reader))
                               (eql (json-at reader) #\u)))
             (json-fail "a JSON string holds half of a surrogate pair"))
           (incf (json-reader-index reader))
           (let ((low (json-hex reader 4)))
             (unless (<= #xDC00 low #xDFFF)
               (json-fail "a JSON string holds half of a surrogate pair"))
             (string (code-char (+ #x10000 (ash (- unit #xD800) 10) (- low #xDC00))))))
          ((<= #xDC00 unit #xDFFF)
           (json-fail "a JSON string holds half of a surrogate pair"))
          (t (string (code-char unit))))))

(defun json-hex (reader count)
  "COUNT hex digits at the reader's position, as a number."
  (let ((value 0))
    (loop repeat count
          do (let ((digit (and (json-at reader) (digit-char-p (json-at reader) 16))))
               (unless digit (json-fail "a \\u escape needs four hex digits"))
               (setf value (+ (* value 16) digit))
               (incf (json-reader-index reader))))
    value))

(defun json-parse-escape (reader)
  "The character after a backslash in a JSON string."
  (let ((character (json-at reader)))
    (unless character (json-fail "a JSON string ends in a backslash"))
    (incf (json-reader-index reader))
    (let ((one (assoc character +json-escapes+)))
      (cond (one (string (cdr one)))
            ((char= character #\u) (json-parse-hex4 reader))
            (t (json-fail "~s is not a JSON escape" character))))))

(defun json-parse-string (reader)
  "The JSON string at the reader's position, quotes included, as a string."
  (incf (json-reader-index reader))
  (let ((out (make-string-output-stream)))
    (loop
      (let ((character (json-at reader)))
        (cond ((null character) (json-fail "a JSON string is not closed"))
              ((char= character #\")
               (incf (json-reader-index reader))
               (return))
              ((char= character #\\)
               (incf (json-reader-index reader))
               (write-string (json-parse-escape reader) out))
              (t (write-char character out) (incf (json-reader-index reader))))))
    (get-output-stream-string out)))

(defparameter +number-chars+ "-+.eE0123456789"
  "The characters a JSON number is written in.")

(defun valid-number-p (raw)
  "Whether RAW is one JSON number, as RFC 8259 writes it."
  (let ((index 0)
        (length (length raw)))
    (labels ((peek () (and (< index length) (char raw index)))
             (digits ()
               (let ((start index))
                 (loop while (and (peek) (digit-char-p (peek))) do (incf index))
                 (> index start))))
      (when (eql (peek) #\-) (incf index))
      (unless (digits) (return-from valid-number-p nil))
      (when (eql (peek) #\.)
        (incf index)
        (unless (digits) (return-from valid-number-p nil)))
      (when (member (peek) '(#\e #\E))
        (incf index)
        (when (member (peek) '(#\+ #\-)) (incf index))
        (unless (digits) (return-from valid-number-p nil)))
      (= index length))))

(defun json-parse-number (reader)
  "The JSON number at the reader's position, as (:NUMBER . RAW)."
  (let ((start (json-reader-index reader)))
    (loop while (and (json-at reader) (find (json-at reader) +number-chars+))
          do (incf (json-reader-index reader)))
    (let ((raw (subseq (json-reader-text reader) start (json-reader-index reader))))
      (unless (valid-number-p raw) (json-fail "~s is not a JSON value" raw))
      (list :number raw))))

(defun json-parse-word (reader)
  "TRUE, FALSE or NULL at the reader's position, as its tag."
  (let ((start (json-reader-index reader)))
    (loop while (and (json-at reader) (alpha-char-p (json-at reader)))
          do (incf (json-reader-index reader)))
    (let ((word (subseq (json-reader-text reader) start (json-reader-index reader))))
      (cond ((string= word "true") '(:true))
            ((string= word "false") '(:false))
            ((string= word "null") '(:null))
            (t (json-fail "~s is not a JSON value" word))))))

(defun json-parse-object (reader duplicates prefix depth)
  "The JSON object at the reader's position, as (:OBJECT (NAME . VALUE) ...), in
the order it wrote them; a name that appears twice is noted in DUPLICATES."
  (incf (json-reader-index reader))
  (let ((pairs '())
        (seen '()))
    (json-skip-space reader)
    (if (eql (json-at reader) #\})
        (incf (json-reader-index reader))
        (loop
          (json-skip-space reader)
          (unless (eql (json-at reader) #\")
            (json-fail "a JSON object's name must be a string"))
          (let ((key (json-parse-string reader)))
            (when (member key seen :test #'string=)
              (noted duplicates (concatenate 'string prefix key)))
            (push key seen)
            (json-skip-space reader)
            (unless (eql (json-at reader) #\:)
              (json-fail "a colon was expected after a JSON object's name"))
            (incf (json-reader-index reader))
            (push (cons key (json-parse-value reader duplicates prefix depth)) pairs)
            (json-skip-space reader)
            (let ((character (json-at reader)))
              (cond ((eql character #\,) (incf (json-reader-index reader)))
                    ((eql character #\}) (incf (json-reader-index reader)) (return))
                    (t (json-fail "a comma or a closing brace was expected in a JSON object")))))))
    (cons :object (nreverse pairs))))

(defun json-parse-array (reader duplicates prefix depth)
  "The JSON array at the reader's position, as (:ARRAY VALUE ...)."
  (incf (json-reader-index reader))
  (let ((items '()))
    (json-skip-space reader)
    (if (eql (json-at reader) #\])
        (incf (json-reader-index reader))
        (loop
          (push (json-parse-value reader duplicates prefix depth) items)
          (json-skip-space reader)
          (let ((character (json-at reader)))
            (cond ((eql character #\,) (incf (json-reader-index reader)))
                  ((eql character #\]) (incf (json-reader-index reader)) (return))
                  (t (json-fail "a comma or a closing bracket was expected in a JSON array"))))))
    (cons :array (nreverse items))))

(defun json-parse-value (reader duplicates prefix depth)
  "The value at the reader's position, as a tagged list: (:STRING TEXT),
(:NUMBER RAW), (:TRUE), (:FALSE), (:NULL), (:ARRAY VALUE...) or
(:OBJECT (NAME . VALUE) ...)."
  (when (> depth +max-depth+)
    (json-fail "the JSON nests deeper than ~d levels" +max-depth+))
  (json-skip-space reader)
  (let ((character (json-at reader)))
    (cond ((null character) (json-fail "the JSON ends where a value was expected"))
          ((eql character #\{) (json-parse-object reader duplicates prefix (1+ depth)))
          ((eql character #\[) (json-parse-array reader duplicates prefix (1+ depth)))
          ((eql character #\") (list :string (json-parse-string reader)))
          ((member character '(#\t #\f #\n)) (json-parse-word reader))
          (t (json-parse-number reader)))))

(defun try-json (data duplicates prefix)
  "DATA read as one JSON value, or NIL when it is not one. PREFIX goes in front
of each name DUPLICATES notes, so a note says which part it was in."
  (multiple-value-bind (text ok) (utf-8-text data)
    (when ok
      (handler-case
          (let* ((reader (make-json-reader text))
                 (value (json-parse-value reader duplicates prefix 0)))
            (json-skip-space reader)
            (when (< (json-reader-index reader) (length text))
              (json-fail "trailing data after the JSON value"))
            value)
        (json-syntax () nil)))))

;;; --- a value as a row shows it ---------------------------------------------------

(defun write-json-string (text stream)
  "TEXT as a JSON string, its quotes included."
  (write-char #\" stream)
  (loop for character across text
        do (case character
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (#\Backspace (write-string "\\b" stream))
             (#\Page (write-string "\\f" stream))
             (t (if (< (char-code character) #x20)
                    (format stream "\\u~4,'0x" (char-code character))
                    (write-char character stream)))))
  (write-char #\" stream))

(defun write-json (value stream)
  "VALUE written as JSON, the way the original's dumps wrote it: ', ' between
members and ': ' after a name."
  (case (first value)
    (:string (write-json-string (second value) stream))
    (:number (write-string (second value) stream))
    (:true (write-string "true" stream))
    (:false (write-string "false" stream))
    (:null (write-string "null" stream))
    (:array (write-string "[" stream)
            (loop for rest on (rest value)
                  do (write-json (car rest) stream)
                     (when (cdr rest) (write-string ", " stream)))
            (write-string "]" stream))
    (:object (write-string "{" stream)
             (loop for rest on (rest value)
                   do (write-json-string (car (car rest)) stream)
                      (write-string ": " stream)
                      (write-json (cdr (car rest)) stream)
                      (when (cdr rest) (write-string ", " stream)))
             (write-string "}" stream))
    (t (write-string "null" stream))))

(defun json-text (value &key (max +max-value-chars+))
  "VALUE's JSON as a row shows it, cut after MAX characters with the rest counted."
  (let ((text (with-output-to-string (out) (write-json value out))))
    (if (and max (> (length text) max))
        (format nil "~a... (~:d characters)" (subseq text 0 max) (length text))
        text)))

(defun json-pair (object key)
  "OBJECT's member KEY - the last one where a name appears twice, as every
parser keeps - and whether it was there."
  (let ((value nil)
        (present nil))
    (dolist (pair (rest object))
      (when (string= (car pair) key)
        (setf value (cdr pair))
        (setf present t)))
    (values value present)))

;;; --- a moment, and a span --------------------------------------------------------

(defun clock-text (unix)
  "UNIX seconds as the description writes a moment: 2026-01-02 03:04:05 UTC."
  (multiple-value-bind (whole fraction) (floor unix)
    (multiple-value-bind (second minute hour day month year)
        (decode-universal-time (+ whole nlk:+unix-epoch+) 0)
      (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d~@[.~a~] UTC"
              year month day hour minute second
              (let ((micros (floor (* fraction 1000000))))
                (when (plusp micros)
                  (string-right-trim "0" (format nil "~6,'0d" micros))))))))

(defparameter +units+ '((86400 "day") (3600 "hour") (60 "minute") (1 "second"))
  "The units a duration is said in, largest first.")

(defun span (seconds)
  "SECONDS as a duration in words, two units at most: \"3 days 4 hours\"."
  (let ((whole (abs (coerce seconds 'double-float))))
    (if (< whole 1)
        "0 seconds"
        (let ((whole (floor whole)))
          (if (>= whole (* 365 +day+))
              (format nil "about ~,1f years (~:d days)"
                      (/ whole (* 365.2425 +day+)) whole)
              (let ((parts '()))
                (loop for (size label) in +units+
                      while (< (length parts) 2)
                      do (multiple-value-bind (count rest) (floor whole size)
                           (setf whole rest)
                           (when (or (plusp count) parts)
                             (push (format nil "~:d ~a~p" count label count) parts))))
                (when (and parts (eql 0 (search "0 " (first parts))))
                  (pop parts))
                (format nil "~{~a~^ ~}" (reverse parts))))))))

(defun relative-text (seconds now)
  "SECONDS said against NOW: \"in 3 hours\", \"2 minutes ago\", or \"now\"."
  (let ((delta (- seconds now)))
    (cond ((< (abs delta) 1) "now")
          ((plusp delta) (format nil "in ~a" (span delta)))
          (t (format nil "~a ago" (span delta))))))

(defun time-seconds (value)
  "VALUE, a JSON value, as a NumericDate in seconds; NIL when it is not a number."
  (when (and (consp value) (eql :number (first value)))
    (let ((raw (second value)))
      (handler-case
          (let ((*read-eval* nil))
            (multiple-value-bind (number end) (read-from-string raw)
              (when (and (= end (length raw)) (realp number))
                (coerce number 'double-float))))
        (error () nil)))))

;;; --- rows ------------------------------------------------------------------------

(defun key-label (key)
  "KEY as a row's label: its name, and what the name means when it is a known one."
  (let ((meaning (cdr (assoc key +names+ :test #'string=))))
    (if meaning (format nil "~a (~a)" key meaning) key)))

(defun clock-of (unix)
  "UNIX seconds as a moment, or NIL when no date can be made of them."
  (handler-case (clock-text unix) (error () nil)))

(defun time-value (value now max-value-chars)
  "VALUE as a NumericDate: the row showing it, the date and how near it is, or
NIL when the value is not a number."
  (let ((seconds (time-seconds value)))
    (when seconds
      (let ((text (json-text value :max max-value-chars)))
        (cond ((>= (abs seconds) +ms-like+)
               (let ((moment (clock-of (/ seconds 1000))))
                 (if moment
                     (format nil "~a (looks like milliseconds; JWT times are seconds) = ~a"
                             text moment)
                     (format nil "~a (not a date jwt can show)" text))))
              (t
               (let ((moment (clock-of seconds)))
                 (if moment
                     (format nil "~a = ~a, ~a" text moment (relative-text seconds now))
                     (format nil "~a (not a date jwt can show)" text)))))))))

(defun unique-pairs (pairs)
  "PAIRS with a name that appears twice kept once, at its first position and
with its last value: what a parser's object keeps."
  (let ((out '()))
    (dolist (pair pairs (nreverse out))
      (let ((cell (assoc (car pair) out :test #'string=)))
        (if cell
            (setf (cdr cell) (cdr pair))
            (push (cons (car pair) (cdr pair)) out))))))

(defun rows (object now header max-claims max-value-chars)
  "OBJECT's members as aligned rows, and how many were left out."
  (let* ((pairs (unique-pairs (rest object)))
         (shown (if (> (length pairs) max-claims) (subseq pairs 0 max-claims) pairs))
         (width 0)
         (lines '()))
    (dolist (pair shown)
      (setf width (max width (length (key-label (car pair))))))
    (dolist (pair shown)
      (let* ((key (car pair))
             (value (cdr pair))
             (text (or (and (member key +time-claims+ :test #'string=)
                            (time-value value now max-value-chars))
                       (and header (string= key "alg")
                            (let ((spelled (and (eql :string (first value))
                                                (cdr (assoc (second value) +algorithms+
                                                            :test #'string=)))))
                              (and spelled
                                   (format nil "~a = ~a"
                                           (json-text value :max max-value-chars) spelled))))
                       (json-text value :max max-value-chars))))
        (push (format nil "  ~va  ~a" width (key-label key) text) lines)))
    (values (nreverse lines) (max 0 (- (length pairs) max-claims)))))

;;; --- the status ------------------------------------------------------------------

(defun claim-seconds (object key)
  "OBJECT's member KEY as seconds, or NIL."
  (time-seconds (json-pair object key)))

(defun status-lines (claims now skew)
  "The lines that say whether the token is good now, in the order they read."
  (let* ((has-exp (nth-value 1 (json-pair claims "exp")))
         (exp (claim-seconds claims "exp"))
         (nbf (claim-seconds claims "nbf"))
         (iat (claim-seconds claims "iat"))
         (lines '()))
    (cond ((not has-exp)
           (push "no exp claim: the token does not expire on its own" lines))
          ((null exp)
           (push "exp is not a number, so the expiry cannot be read" lines))
          ((>= (abs exp) +ms-like+)
           (push "exp looks like milliseconds; a server reading it as seconds sees a date far in the future"
                 lines))
          ((<= exp now)
           (push (format nil "EXPIRED ~a ago" (span (- now exp))) lines))
          (t
           (push (format nil "valid for ~a more" (span (- exp now))) lines)))
    (when (and nbf (< (abs nbf) +ms-like+) (> nbf (+ now skew)))
      (push (format nil "NOT VALID YET: usable ~a" (relative-text nbf now)) lines))
    (when (and iat (< (abs iat) +ms-like+) (> iat (+ now skew)))
      (push (format nil "issued ~a: iat is in the future (a clock is off, or the token was made by hand)"
                     (relative-text iat now))
            lines))
    (when (and exp iat (< (abs exp) +ms-like+) (< (abs iat) +ms-like+) (>= exp iat))
      (push (format nil "lifetime ~a (exp - iat)" (span (- exp iat))) lines))
    (nreverse lines)))

;;; --- the notes -------------------------------------------------------------------

(defun note-lines (token max-value-chars)
  "What is worth warning about in TOKEN, in the order the original said it."
  (let ((header (token-header token))
        (signature (token-signature token))
        (notes '()))
    (multiple-value-bind (alg has-alg) (json-pair header "alg")
      (if (token-encrypted token)
          (push "encrypted (JWE): the claims cannot be read without the recipient's key; only the header is shown"
                notes)
          (progn
            (when (= 2 (token-parts token))
              (push "only 2 parts: the signature part is missing (an unsigned token ends in '.', which a composer may take for punctuation)"
                    notes))
            (cond ((not has-alg)
                   (push "the header has no alg, which a JWS requires" notes))
                  ((and (eql :string (first alg)) (string-equal (second alg) "none"))
                   (push "alg is \"none\": the token is unsigned, so anyone can write one; a server that accepts it accepts anything"
                         notes)
                   (when (plusp (length signature))
                     (push "yet part 3 is not empty, which an unsigned token's signature must be"
                           notes)))
                  ((plusp (length signature))
                   (push (format nil "signature: ~d bytes, not checked (jwt decodes; it never verifies)"
                                 (length signature))
                         notes))
                  ((= 3 (token-parts token))
                   (push (format nil "the signature (part 3) is empty although alg is ~a"
                                 (json-text alg :max max-value-chars))
                         notes))))))
    (let ((pointers (remove-if-not (lambda (name) (nth-value 1 (json-pair header name)))
                                   +key-pointers+)))
      (when pointers
        (push (format nil "the header points to its own key (~{~a~^, ~}); a server must not trust a key the token names"
                      pointers)
              notes)))
    (multiple-value-bind (crit has-crit) (json-pair header "crit")
      (when has-crit
        (push (format nil "crit lists header fields a server must understand or reject the token: ~a"
                      (json-text crit :max max-value-chars))
              notes)))
    (let ((duplicates (coerce (token-duplicates token) 'list)))
      (when duplicates
        (push (format nil "names that appear twice (JSON keeps the last one, other parsers may not): ~{~a~^, ~}"
                      duplicates)
              notes)))
    (nreverse notes)))

;;; --- the description -------------------------------------------------------------

(defun payload-lines (token max-value-chars)
  "A payload that is not a JSON object, as the lines that show it."
  (let ((preview (utf-8-text (subseq (token-payload token)
                                      0 (min (length (token-payload token)) max-value-chars)))))
    (append (list (format nil "Payload (not a JSON object, ~:d bytes)" (length (token-payload token)))
                  (format nil "  ~a" (json-text (list :string (or preview "")) :max nil)))
            (multiple-value-bind (cty has-cty) (json-pair (token-header token) "cty")
              (when (and has-cty (eql :string (first cty)) (string-equal (second cty) "JWT"))
                (list "  cty is JWT: the payload is itself a token; decode it with its own jwt_decode call"))))))

(defun describe-token (token &key now limits)
  "TOKEN as the description a reader acts on: the header's fields, the claims,
whether the token is good now, and what is worth warning about. Nothing is
verified."
  (let* ((max-claims (or (getf limits :max-claims) +max-claims+))
         (max-value (or (getf limits :max-value-chars) +max-value-chars+))
         (skew (or (getf limits :skew) +skew-seconds+))
         (now (or now (coerce (nlk:unix-now) 'double-float)))
         (claims (token-claims token))
         (kind (if (token-encrypted token)
                   "encrypted (JWE), 5 parts"
                   (format nil "signed (JWS), ~d parts; the signature is NOT verified"
                           (token-parts token))))
         (lines (list (format nil "JWT, ~a" kind) "Header")))
    (multiple-value-bind (header-rows more)
        (rows (token-header token) now t max-claims max-value)
      (setf lines (append lines header-rows))
      (when (plusp more)
        (setf lines (append lines (list (format nil "  ... and ~d more header fields" more))))))
    (unless (token-encrypted token)
      (if (null claims)
          (setf lines (append lines (payload-lines token max-value)))
          (progn
            (multiple-value-bind (claim-rows more) (rows claims now nil max-claims max-value)
              (setf lines (append lines (list (if claim-rows "Claims" "Claims (none)"))))
              (setf lines (append lines claim-rows))
              (when (plusp more)
                (setf lines (append lines (list (format nil "  ... and ~d more claims" more))))))
            (setf lines (append lines (list (format nil "Status at ~a"
                                                    (or (clock-of (floor now)) "")))))
            (setf lines (append lines (mapcar (lambda (line) (format nil "  ~a" line))
                                              (status-lines claims now skew)))))))
    (let ((notes (note-lines token max-value)))
      (when notes
        (setf lines (append lines (list "Notes")))
        (setf lines (append lines (mapcar (lambda (note) (format nil "  - ~a" note)) notes)))))
    (format nil "~{~a~^~%~}" lines)))

;;; --- a token in ------------------------------------------------------------------

(defun split-parts (raw)
  "RAW split at every dot, empty parts kept."
  (let ((parts '())
        (start 0))
    (loop (let ((dot (position #\. raw :start start)))
            (if dot
                (progn (push (subseq raw start dot) parts)
                       (setf start (1+ dot)))
                (progn (push (subseq raw start) parts)
                       (return (nreverse parts))))))))

(defun clean-token (target)
  "The token in what was handed over: surrounding space, one pair of quotes and
a Bearer prefix removed."
  (let ((text (nlk:trimmed (or target ""))))
    (when (and (>= (length text) 2)
               (char= (char text 0) (char text (1- (length text))))
               (find (char text 0) "\"'`"))
      (setf text (nlk:trimmed (subseq text 1 (1- (length text))))))
    (when (and (>= (length text) 6)
               (string-equal "bearer" (subseq text 0 6))
               (or (= 6 (length text))
                   (member (char text 6) '(#\Space #\Tab #\Newline #\Return))))
      (setf text (nlk:trimmed (subseq text 6))))
    text))

(defun decode-token (target &key (max-chars +max-token-chars+))
  "TARGET as a decoded token; JWT-ERROR with the reason when it is not one."
  (let* ((raw (clean-token target))
         (size (length raw)))
    (when (zerop size)
      (fail "no token given: a JWT is header.payload.signature, or a Bearer token"))
    (when (> size max-chars)
      (fail "the token is ~:d characters long; jwt reads at most ~:d" size max-chars))
    (let ((parts (split-parts raw)))
      (unless (member (length parts) '(2 3 5))
        (fail "a JWT has 3 parts (signed) or 5 parts (encrypted) separated by dots; this has ~d"
              (length parts)))
      (let* ((duplicates (make-array 0 :adjustable t :fill-pointer 0))
             (header (or (try-json (base64url-octets (first parts) "header (part 1)")
                                    duplicates "header ")
                         (fail "the header (part 1) decodes, but not to JSON, so this is not a JWT"))))
        (unless (eql :object (first header))
          (fail "the header (part 1) is JSON but not an object, so this is not a JWT"))
        (if (= 5 (length parts))
            (progn
              (loop for part in (rest parts)
                    for number from 2
                    do (base64url-octets part (format nil "part ~d" number)))
              (make-token :raw raw :header header :claims nil :payload (octets) :signature nil
                          :encrypted t :duplicates duplicates :parts 5))
            (let* ((payload (base64url-octets (second parts) "payload (part 2)"))
                   (signature (if (= 3 (length parts))
                                  (base64url-octets (third parts) "signature (part 3)")
                                  (octets)))
                   (decoded (try-json payload duplicates ""))
                   (claims (and decoded (eql :object (first decoded)) decoded)))
              (make-token :raw raw :header header :claims claims :payload payload
                          :signature signature :encrypted nil :duplicates duplicates
                          :parts (length parts))))))))

(defun expand-token (target &key now limits)
  "TARGET decoded and described; JWT-ERROR when it is not a token."
  (describe-token
   (decode-token target :max-chars (or (getf limits :max-token-chars) +max-token-chars+))
   :now now :limits limits))
