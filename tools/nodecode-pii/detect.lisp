;;;; detect.lisp --- the local detectors, and the masking over their spans.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reads a socket, a file or an environment variable: five
;;;; detectors over one string (email, phone, card, IBAN, IP), one over the
;;;; names the operator listed (PERSON), and one merge that replaces the union
;;;; of every overlap with a placeholder, the same shape the Presidio engine
;;;; this cell rewrites drew. Where spans overlap the structured type wins,
;;;; so a card number inside a phone-looking run is named a card.

(in-package #:nodecode-pii)

(defparameter +default-entities+ '("email" "phone" "card" "iban" "ip")
  "The detections that run unless the `pii.entities' setting narrows them.")

(defvar *entities* (copy-list +default-entities+)
  "The detectors the running cell applies: any of email, phone, card, iban, ip.")

(defvar *terms* '()
  "The words the running cell masks as <PERSON>: names, handles, codenames.")

;;; --- spans, ranks and the mask ------------------------------------------------

(defstruct (hit (:constructor make-hit (start end type)))
  "One detected span: [START, END) of the text, and the TYPE it was read as."
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (type "" :type string))

(defparameter +rank+ '(("EMAIL_ADDRESS" . 60) ("IBAN_CODE" . 55) ("CREDIT_CARD" . 50)
                       ("IP_ADDRESS" . 45) ("PHONE_NUMBER" . 40) ("PERSON" . 10))
  "Which placeholder names an overlap: structured data outranks a guessed name.")

(defun hit-rank (hit)
  "HIT's standing against another type over the same characters, higher wins."
  (or (cdr (assoc (hit-type hit) +rank+ :test #'string=)) 0))

(defun hit-before-p (a b)
  "Whether A sorts before B: by start, and the longer span first at one start."
  (or (< (hit-start a) (hit-start b))
      (and (= (hit-start a) (hit-start b)) (> (hit-end a) (hit-end b)))))

(defun in-range-p (hit length)
  "Whether HIT's offsets address LENGTH characters, neither inverted nor past."
  (and (<= 0 (hit-start hit)) (< (hit-start hit) (hit-end hit)) (<= (hit-end hit) length)))

(defun merge-hits (hits length)
  "HITS as disjoint groups in text order, (START END RANK TYPE): every overlap
covered whole, the highest-ranking type naming the group."
  (let ((groups '()))
    (dolist (hit (sort (remove-if-not (lambda (hit) (in-range-p hit length)) (copy-list hits))
                       #'hit-before-p))
      (let ((group (first groups)))
        (cond ((and group (< (hit-start hit) (second group)))
               (setf (second group) (max (second group) (hit-end hit)))
               (when (> (hit-rank hit) (third group))
                 (setf (third group) (hit-rank hit)
                       (fourth group) (hit-type hit))))
              (t (push (list (hit-start hit) (hit-end hit) (hit-rank hit) (hit-type hit))
                       groups)))))
    (nreverse groups)))

(defun mask-hits (text hits)
  "TEXT with every merged HIT replaced by <TYPE>; TEXT itself when there are none."
  (let ((groups (merge-hits hits (length text))))
    (if (null groups)
        text
        (with-output-to-string (out)
          (let ((cursor 0))
            (dolist (group groups)
              (write-string (subseq text cursor (first group)) out)
              (format out "<~a>" (fourth group))
              (setf cursor (second group)))
            (write-string (subseq text cursor) out))))))

;;; --- small readers over one string --------------------------------------------

(defun split-on (text delimiter)
  "TEXT cut at every DELIMITER character, the pieces in order."
  (let ((parts '()) (from 0))
    (loop for at = (position delimiter text :start from)
          do (push (subseq text from at) parts)
             (if at (setf from (1+ at)) (return)))
    (nreverse parts)))

(defun in-set-p (character set)
  "Whether CHARACTER is one of SET's, case-blind."
  (and character (find character set :test #'char-equal)))

(defun all-digits-p (text)
  (and (plusp (length text)) (every #'digit-char-p text)))

;;; --- email ---------------------------------------------------------------------

(defparameter +email-local-chars+ "abcdefghijklmnopqrstuvwxyz0123456789._%+-'"
  "What may sit left of the at sign.")

(defparameter +email-domain-chars+ "abcdefghijklmnopqrstuvwxyz0123456789.-"
  "What may sit right of it.")

(defun valid-domain-p (domain)
  "Whether DOMAIN reads as a host name: labels, a dotted last one of letters."
  (let* ((labels (split-on domain #\.))
         (tld (car (last labels))))
    (and (> (length labels) 1)
         (>= (length tld) 2)
         (every #'alpha-char-p tld)
         (every (lambda (label)
                  (and (plusp (length label))
                       (char/= #\- (char label 0))
                       (char/= #\- (char label (1- (length label))))))
                labels))))

(defun email-at (text at)
  "The email address the at sign at AT stands in, or NIL."
  (let ((start at) (end (1+ at)))
    (loop while (and (plusp start) (in-set-p (char text (1- start)) +email-local-chars+))
          do (decf start))
    (loop while (and (< end (length text)) (in-set-p (char text end) +email-domain-chars+))
          do (incf end))
    (let ((local (subseq text start at))
          (domain (string-right-trim "." (subseq text (1+ at) end))))
      (when (and (plusp (length local))
                 (char/= #\. (char local 0))
                 (char/= #\. (char local (1- (length local))))
                 (valid-domain-p domain))
        (make-hit start (+ at 1 (length domain)) "EMAIL_ADDRESS")))))

(defun email-hits (text)
  "Every email address in TEXT."
  (let ((hits '()) (at (position #\@ text)))
    (loop while at
          do (let ((hit (email-at text at)))
               (if hit
                   (progn (push hit hits)
                          (setf at (position #\@ text :start (hit-end hit))))
                   (setf at (position #\@ text :start (1+ at))))))
    (nreverse hits)))

;;; --- ip ------------------------------------------------------------------------

(defun ip-character-p (character)
  "What an IPv4 address is written with."
  (or (digit-char-p character) (char= #\. character)))

(defun valid-ip-p (token)
  "Whether TOKEN is four dotted decimal octets, none above 255."
  (let ((parts (split-on token #\.)))
    (and (= (length parts) 4)
         (every (lambda (part)
                  (and (<= 1 (length part) 3)
                       (all-digits-p part)
                       (<= (parse-integer part) 255)))
                parts))))

(defun ip-hits (text)
  "Every dotted IPv4 address in TEXT. A version number that reads as one is one:
four dotted octets below 256 already look like an address."
  (let ((hits '()) (i 0) (length (length text)))
    (loop while (< i length)
          do (let ((character (char text i)))
               (if (and (digit-char-p character)
                        (or (zerop i) (not (ip-character-p (char text (1- i))))))
                   (let ((end (loop for j from i below length
                                    while (ip-character-p (char text j)) finally (return j))))
                     (let ((token (string-right-trim "." (subseq text i end))))
                       (when (valid-ip-p token)
                         (push (make-hit i (+ i (length token)) "IP_ADDRESS") hits)))
                     (setf i end))
                   (incf i))))
    (nreverse hits)))

;;; --- card ----------------------------------------------------------------------

(defun luhn-p (digits)
  "Whether DIGITS passes the checksum a card number carries."
  (let ((sum 0) (double nil))
    (loop for i from (1- (length digits)) downto 0
          for digit = (digit-char-p (char digits i))
          do (if double
                 (let ((doubled (* 2 digit)))
                   (incf sum (if (> doubled 9) (- doubled 9) doubled)))
                 (incf sum digit))
             (setf double (not double)))
    (zerop (mod sum 10))))

(defun card-candidate (text start)
  "The digit group at START -- digits, single spaces or dashes between them.
=> (values END DIGITS)."
  (let ((i start) (end start) (digits (make-string-output-stream)))
    (loop while (< i (length text))
          do (let ((character (char text i)))
               (cond ((digit-char-p character)
                      (write-char character digits)
                      (setf end (1+ i))
                      (incf i))
                     ((and (member character '(#\Space #\-) :test #'char=)
                           (< (1+ i) (length text))
                           (digit-char-p (char text (1+ i))))
                      (incf i))
                     (t (return)))))
    (values end (get-output-stream-string digits))))

(defun card-hits (text)
  "Every card number in TEXT: 13 to 19 digits that pass the checksum."
  (let ((hits '()) (i 0) (length (length text)))
    (loop while (< i length)
          do (let ((character (char text i)))
               (if (and (digit-char-p character)
                        (or (zerop i) (not (digit-char-p (char text (1- i))))))
                   (multiple-value-bind (end digits) (card-candidate text i)
                     (when (and (<= 13 (length digits) 19) (luhn-p digits))
                       (push (make-hit i end "CREDIT_CARD") hits))
                     (setf i (max end (1+ i))))
                   (incf i))))
    (nreverse hits)))

;;; --- iban ----------------------------------------------------------------------

(defun iban-character-p (character)
  "What an IBAN's body is written with: capitals and digits."
  (or (digit-char-p character) (and (char<= #\A character) (char<= character #\Z))))

(defun iban-mod97 (iban)
  "IBAN's remainder modulo 97 over its rotated, letter-numbered form."
  (let ((mod 0))
    (loop for character across (concatenate 'string (subseq iban 4) (subseq iban 0 4))
          do (multiple-value-bind (value width)
                 (if (digit-char-p character)
                     (values (- (char-code character) (char-code #\0)) 1)
                     (values (+ 10 (- (char-code character) (char-code #\A))) 2))
               (setf mod (mod (if (= width 2) (+ (* mod 100) value) (+ (* mod 10) value))
                              97))))
    mod))

(defun iban-hits (text)
  "Every IBAN in TEXT: two letters, two digits, a body, and the check remainder."
  (let ((hits '()) (i 0) (length (length text)))
    (loop while (< i length)
          do (let ((character (char text i)))
               (if (and (upper-case-p character)
                        (or (zerop i) (not (alphanumericp (char text (1- i))))))
                   (let ((end (loop for j from i below length
                                    while (iban-character-p (char text j)) finally (return j))))
                     (let ((token (subseq text i end)))
                       (when (and (<= 15 (length token) 34)
                                  (alpha-char-p (char token 0))
                                  (alpha-char-p (char token 1))
                                  (digit-char-p (char token 2))
                                  (digit-char-p (char token 3))
                                  (= 1 (iban-mod97 token)))
                         (push (make-hit i (+ i (length token)) "IBAN_CODE") hits)))
                     (setf i (max end (1+ i))))
                   (incf i))))
    (nreverse hits)))

;;; --- phone ---------------------------------------------------------------------

(defparameter +phone-chars+ "+-(). 0123456789"
  "What a written phone number is made of besides its digits.")

(defun phone-hits (text)
  "Every phone number in TEXT: a run of at least 9 digits, written with a leading
plus or with separators, so a bare eight-digit id is left alone."
  (let ((hits '()) (i 0) (length (length text)))
    (loop while (< i length)
          do (let ((character (char text i)))
               (if (and (or (digit-char-p character) (char= #\+ character))
                        (or (zerop i) (not (alphanumericp (char text (1- i))))))
                   (let* ((end (loop for j from i below length
                                     while (in-set-p (char text j) +phone-chars+)
                                     finally (return j)))
                          (token (string-right-trim " ().-" (subseq text i end)))
                          (digits (count-if #'digit-char-p token)))
                     (when (and (<= 9 digits 15)
                                (or (char= #\+ (char token 0))
                                    (some (lambda (one) (find one " ().-" :test #'char=)) token))
                                (or (= end length) (not (alphanumericp (char text end)))))
                       (push (make-hit i (+ i (length token)) "PHONE_NUMBER") hits))
                     (setf i (max end (1+ i))))
                   (incf i))))
    (nreverse hits)))

;;; --- the names the operator listed ----------------------------------------------

(defun term-hits (text terms)
  "Every whole-word occurrence of each of TERMS, masked as a person."
  (let ((lower (string-downcase text)) (hits '()))
    (dolist (term terms)
      (let* ((needle (string-downcase (nlk:trimmed term)))
             (size (length needle)))
        (when (>= size 2)
          (loop for from = 0 then (1+ at)
                for at = (search needle lower :start2 from)
                while at
                do (let ((end (+ at size)))
                     (when (and (or (zerop at) (not (alphanumericp (char text (1- at)))))
                                (or (= end (length text))
                                    (not (alphanumericp (char text end)))))
                       (push (make-hit at end "PERSON") hits)))))))
    hits))

;;; --- one text in, one text out ------------------------------------------------

(defun detect-hits (text &key (entities *entities*) (terms *terms*))
  "Every detection in TEXT, from the detectors ENTITIES names and the words TERMS."
  (flet ((wants-p (name) (member name entities :test #'string-equal)))
    (append (when (wants-p "email") (email-hits text))
            (when (wants-p "card") (card-hits text))
            (when (wants-p "iban") (iban-hits text))
            (when (wants-p "ip") (ip-hits text))
            (when (wants-p "phone") (phone-hits text))
            (term-hits text terms))))

(defun redact-text (text &key (entities *entities*) (terms *terms*))
  "TEXT with its personal data masked, or TEXT itself when nothing was found.
Never signals: text a detector chokes on comes back as it was."
  (if (or (null text) (zerop (length text)))
      text
      (handler-case (mask-hits text (detect-hits text :entities entities :terms terms))
        (error () text))))
