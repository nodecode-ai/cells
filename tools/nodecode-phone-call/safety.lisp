;;;; safety.lisp --- the hard rules, fail-closed: E.164, the blocklist, the
;;;; allowlist, quiet hours, and what of a brief may be be said out loud.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original refused a call in three places (prepare, the handler, the
;;;; Twilio dialer) and each one re-ran every rule. This rewrite keeps the
;;;; rules and the repetition: PREPARE-CALL and PLACE-CALL both call
;;;; VALIDATE-NUMBER, ALLOWLIST-REFUSAL and QUIET-HOURS-REFUSAL, so no
;;;; argument the model writes can reach a number the operator did not allow.
;;;; Values here are strings in, refusal-or-NIL out: a rule that passes says
;;;; nothing.

(in-package #:nodecode-phone-call)

(defparameter +emergency-numbers+
  '("911" "112" "999" "000" "110" "119" "100" "101" "102" "103" "104" "105"
    "106" "107" "108" "109" "933" "988")
  "Never dialed, whatever the user says: the original's blocklist, unchanged.")

(defparameter +premium-prefixes+ '("900" "976")
  "North-American premium-rate area codes.")

(defparameter +min-national-digits+ 6
  "Shorter than this is a short code or an emergency-length number.")
(defparameter +quiet-start-hour+ 8 "Calls may start at this hour, callee time.")
(defparameter +quiet-end-hour+ 21 "and not at or after this one.")
(defparameter +min-gap-same-number-s+ 60
  "Sixty seconds between two calls to one number, the original's floor.")
(defparameter +daily-limit-default+ 10 "Calls per rolling twenty-four hours.")
(defparameter +approval-ttl-s+ 900 "Fifteen minutes: a card, and an approval.")
(defparameter +min-duration-s+ 30 "The shortest call the cell will place.")
(defparameter +max-duration-s+ 900 "The hard cap on one call, in seconds.")
(defparameter +default-duration-s+ 300 "The original's default call ceiling.")

(defparameter +sensitive-fields+ '("card" "address" "id_number" "bank" "email" "phone")
  "What may be shared only with the operator's per-call consent.")
(defparameter +forbidden-fields+ '("cvv" "password" "otp")
  "What is never shared, consent or not, in the original's words.")

;;; --- strings ------------------------------------------------------------------

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

(defun digits-of (text)
  "TEXT's digits, in order."
  (remove-if-not #'digit-char-p (or text "")))

(defun contains (haystack needle)
  "Is NEEDLE a substring of HAYSTACK, case-insensitively?"
  (and (stringp haystack) (stringp needle) (plusp (length needle))
       (not (null (search needle haystack :test #'char-equal)))))

(defun any-word (text words)
  "The first of WORDS that TEXT contains, or NIL."
  (find-if (lambda (word) (contains text word)) words))

(defun split-commas (text)
  "TEXT split on commas, trimmed, empties dropped."
  (let ((out '()) (start 0))
    (loop for comma = (position #\, text :start start)
          do (let ((piece (nlk:trimmed (subseq text start comma))))
               (when (plusp (length piece)) (push piece out)))
             (if comma (setf start (1+ comma)) (return)))
    (nreverse out)))

;;; --- the number ---------------------------------------------------------------

(defun clean-number (raw)
  "RAW without a tel: prefix, an extension, or surrounding space."
  (let* ((text (nlk:trimmed (or raw "")))
         (text (if (and (>= (length text) 4) (string-equal "tel:" (subseq text 0 4)))
                   (nlk:trimmed (subseq text 4))
                   text))
         (lower (string-downcase text))
         (cut (or (position #\; text)
                  (search "ext" lower)
                  (let ((x (position #\x lower)))
                    (and x (plusp x) (digit-char-p (char text (1- x))) x)))))
    (nlk:trimmed (if cut (subseq text 0 cut) text))))

(defun national-candidates (raw)
  "Every national digit string RAW could mean, for blocklist matching."
  (let* ((digits (digits-of (clean-number raw)))
         (out (list digits)))
    (when (and (> (length digits) +min-national-digits+) (char= #\1 (char digits 0)))
      (push (subseq digits 1) out))
    (when (and (> (length digits) 10) (string= "00" (subseq digits 0 2)))
      (push (subseq digits 2) out))
    (remove "" (remove-duplicates out :test #'equal) :test #'equal)))

(defun blocked-reason (raw)
  "Why RAW must never be dialed, or NIL. The blocklist the user cannot change."
  (let ((digits (digits-of (clean-number raw))))
    (when (zerop (length digits))
      (return-from blocked-reason "there are no digits in the phone number"))
    (dolist (candidate (national-candidates raw))
      (when (member candidate +emergency-numbers+ :test #'equal)
        (return-from blocked-reason
          (format nil "~a is an emergency number; this cell never dials one" candidate)))
      (when (< (length candidate) +min-national-digits+)
        (return-from blocked-reason
          (format nil "~a is a short code or an emergency-length number, which is blocked"
                  candidate)))
      (when (and (= (length candidate) 10)
                 (member (subseq candidate 0 3) +premium-prefixes+ :test #'equal))
        (return-from blocked-reason
          (format nil "~a is a premium-rate number, which is blocked" candidate))))
    nil))

(defun e164-candidate (candidate)
  "CANDIDATE checked as E.164 => (values e164 \"\") or (values NIL reason)."
  (let ((digits (subseq candidate 1)))
    (if (and (<= 8 (length digits) 15) (not (char= #\0 (char digits 0))))
        (values candidate "")
        (values nil "that does not look like a possible phone number"))))

(defun normalize-e164 (raw)
  "RAW as E.164, the original's normalization without libphonenumber:
+14155550123, 0044..., and an 11-digit North-American number all read.
=> (values e164 \"\") or (values NIL reason)."
  (let* ((text (clean-number raw))
         (digits (digits-of text)))
    (cond
      ((zerop (length digits))
       (values nil "there are no digits in the phone number"))
      ((and (plusp (length text)) (char= #\+ (char text 0)))
       (e164-candidate (concatenate 'string "+" digits)))
      ((and (>= (length digits) 8) (string= "00" (subseq digits 0 2)))
       (e164-candidate (concatenate 'string "+" (subseq digits 2))))
      ((and (= (length digits) 11) (char= #\1 (char digits 0)))
       (e164-candidate (concatenate 'string "+" digits)))
      (t (values nil
                 (format nil "~a is not in international E.164 format, e.g. +14155550123"
                         text))))))

(defun validate-number (raw)
  "The one entry point the tools use: blocklist, normalization, blocklist again.
=> (values e164 \"\") or (values NIL reason)."
  (let ((why (blocked-reason raw)))
    (when why (return-from validate-number (values nil why))))
  (multiple-value-bind (e164 reason) (normalize-e164 raw)
    (unless e164 (return-from validate-number (values nil reason)))
    (let ((why (blocked-reason e164)))
      (when why (return-from validate-number (values nil why))))
    (values e164 "")))

;;; --- the allowlist ------------------------------------------------------------

(defun allowlist ()
  "The numbers this cell may dial: the section's allowed_numbers, then the
variable, normalized. Unset or empty allows nothing."
  (let ((raw (append (let ((listed (ignore-errors (setting :allowed-numbers))))
                       (cond ((vectorp listed) (coerce listed 'list))
                             ((listp listed) listed)
                             (t nil)))
                     (split-commas (or (uiop:getenv "NODECODE_PHONE_CALL_ALLOWLIST") "")))))
    (remove-duplicates
     (remove nil (mapcar (lambda (item)
                           (multiple-value-bind (e164 reason) (normalize-e164 item)
                             (declare (ignore reason))
                             e164))
                         raw))
     :test #'equal)))

(defun allowlist-refusal (e164)
  "Why E164 may not be dialed under the allowlist, or NIL. Fails closed."
  (when (ignore-errors (setting :allow-all))
    (return-from allowlist-refusal nil))
  (let ((allowed (allowlist)))
    (cond ((null allowed)
           "dialing is disabled: no number is on allowed_numbers, so nothing may be dialed (list the numbers the operator may call in the phone-call section)")
          ((not (member e164 allowed :test #'equal))
           (format nil "~a is not on the allowlist (allowed_numbers in the phone-call section)" e164))
          (t nil))))

;;; --- quiet hours --------------------------------------------------------------

(defun quiet-hours-refusal (now override)
  "Why NOW is the wrong time to ring, or NIL. Opt-in, as in the original: on,
it fails closed when the callee's time zone is not given."
  (when (or override (not (ignore-errors (setting :enforce-quiet-hours))))
    (return-from quiet-hours-refusal nil))
  (let ((offset (ignore-errors (setting :callee-utc-offset))))
    (unless (integerp offset)
      (return-from quiet-hours-refusal
        (concatenate 'string
                     "quiet hours: the callee's time zone is unknown; give callee_utc_offset "
                     "in the phone-call section, or pass override_quiet_hours")))
    (let* ((hour (mod (+ (nth-value 2 (decode-universal-time now 0)) offset) 24)))
      (unless (<= +quiet-start-hour+ hour (1- +quiet-end-hour+))
        (format nil "quiet hours: it is ~2,'0d:00 for the callee, outside ~2,'0d:00 to ~2,'0d:00; pass override_quiet_hours or call later"
                hour +quiet-start-hour+ +quiet-end-hour+)))))

;;; --- what may be said ---------------------------------------------------------

(defun field-of-key (key)
  "The sensitive or forbidden field a fact KEY names, or NIL. Forbidden first."
  (let ((k (string-downcase (princ-to-string key))))
    (cond
      ((any-word k '("cvv" "cvc" "csc" "security code" "security-code" "card verification"
                     "card_verif" "card-verif"))
       "cvv")
      ((any-word k '("otp" "one-time" "one_time" "one time" "2fa" "mfa" "verification code"
                     "verification_code" "auth code" "auth_code" "authentication code"
                     "sms code" "sms_code"))
       "otp")
      ((any-word k '("password" "passcode" " pin" "pin " "secret" "credential" "api_key"
                     "api key" "apikey" "token"))
       "password")
      ((any-word k '("card" "credit" "debit" "expir")) "card")
      ((any-word k '("passport" "ssn" "social security" "social_security" "national id"
                     "national_id" "tax id" "tax_id" "id number" "id_number" "id-no"
                     "licence" "license" "nin" "itin"))
       "id_number")
      ((any-word k '("bank" "iban" "routing" "swift" " bic" "sort code" "sort_code"
                     "account number" "account_number" "account no"))
       "bank")
      ((any-word k '("address" "street" " zip" "zip " "postcode" "postal" " city" "city "
                     "apartment" " apt" "apt "))
       "address")
      ((any-word k '("email" "e-mail")) "email")
      ((any-word k '("phone" "mobile" " callback" "callback" "cell" "fax" " tel" "tel "))
       "phone")
      (t nil))))

(defun luhn-p (digits)
  "Does DIGITS (a string of decimal digits) pass the Luhn check?"
  (let ((sum 0) (double nil))
    (loop for i from (1- (length digits)) downto 0
          for n = (digit-char-p (char digits i))
          do (let ((value (if double (* 2 n) n)))
               (when (> value 9) (setf value (- value 9)))
               (incf sum value))
             (setf double (not double)))
    (zerop (mod sum 10))))

(defun card-in (text)
  "A card-number-shaped run in TEXT: 13 to 19 digits, separators allowed, Luhn-passing."
  (let ((start nil) (count 0))
    (labels ((flush ()
               (when (and start (<= 13 count 19))
                 (let ((digits (digits-of (subseq text start))))
                   (when (luhn-p digits) (return-from card-in t))))
               (setf start nil count 0)))
      (loop for i from 0 below (length text)
            for ch = (char text i)
            do (cond ((digit-char-p ch)
                      (unless start (setf start i))
                      (incf count))
                     ((and start (member ch '(#\Space #\-))) nil)
                     (t (flush))))
      (flush)
      nil)))

(defun email-in (text)
  "An address-shaped token in TEXT."
  (let ((at (position #\@ text)) (dot (position #\. text :from-end t)))
    (and at dot (> dot at) (> at 0))))

(defun ssn-in (text)
  "A ddd-dd-dddd shaped run in TEXT."
  (loop for i from 0 below (- (length text) 11)
        when (and (every #'digit-char-p (subseq text i (+ i 3)))
                  (char= #\- (char text (+ i 3)))
                  (every #'digit-char-p (subseq text (+ i 4) (+ i 6)))
                  (char= #\- (char text (+ i 6)))
                  (every #'digit-char-p (subseq text (+ i 7) (+ i 11))))
          return t))

(defun fields-in-text (text)
  "The sensitive fields TEXT's own values reveal."
  (let ((out '()))
    (when (card-in text) (push "card" out))
    (when (email-in text) (push "email" out))
    (when (ssn-in text) (push "id_number" out))
    out))

(defun fact-field (key value)
  "The one field a fact belongs to: its key first, then its value."
  (or (field-of-key key)
      (let ((text (princ-to-string value)))
        (first (fields-in-text text)))))

(defun value-text (value)
  "VALUE as one line of a brief or a transcript."
  (cond ((stringp value) value)
        ((hash-table-p value)
         (let ((parts '()))
           (maphash (lambda (k v) (push (format nil "~a: ~a" k (value-text v)) parts)) value)
           (format nil "{~{~a~^; ~}}" (nreverse parts))))
        ((vectorp value)
         (format nil "~{~a~^, ~}" (map 'list #'value-text value)))
        (t (princ-to-string value))))

(defun redact-text (text granted)
  "TEXT with the card, address-shaped and e-mail values of fields not GRANTED
replaced, so a goal can be spoken without leaking what the operator did not allow."
  (let ((text (or text "")))
    (unless (member "email" granted :test #'equal)
      (setf text
            (with-output-to-string (out)
              (let ((start 0))
                (loop for i from 0 below (length text)
                      when (member (char text i) '(#\Space #\Tab #\Newline) :test #'char=)
                        do (let ((token (subseq text start i)))
                             (format out "~a~c" (if (and (email-in token) (> (length token) 3))
                                                     "[redacted]"
                                                     token)
                                     (char text i)))
                           (setf start (1+ i)))
                (let ((token (subseq text start)))
                  (format out "~a" (if (and (email-in token) (> (length token) 3))
                                       "[redacted]"
                                       token)))))))
    (unless (member "id_number" granted :test #'equal)
      (when (ssn-in text) (setf text "[redacted]")))
    (when (and (not (member "card" granted :test #'equal)) (card-in text))
      (setf text "[redacted]"))
    text))

(defun build-safe-facts (facts granted)
  "FACTS — a hash table from the tool call — as (values SAFE WITHHELD), where
SAFE is an alist of what may be said and WITHHELD names what is held back
(never the values), the original's consent model."
  (let ((safe '()) (withheld '()))
    (when (hash-table-p facts)
      (maphash (lambda (key value)
                 (let ((field (fact-field key value)))
                   (cond ((member field +forbidden-fields+ :test #'equal)
                          (push (format nil "~a (~a, never shared)" key field) withheld))
                         ((and field (not (member field granted :test #'equal)))
                          (push (format nil "~a (~a, no consent)" key field) withheld))
                         (t (push (cons key value) safe)))))
               facts))
    (values (nreverse safe) (nreverse withheld))))

(defun sensitive-requested (value)
  "SENSITIVE_FIELDS from a tool call, lowercased and checked, or NIL."
  (let ((items (cond ((vectorp value) (coerce value 'list))
                     ((listp value) value)
                     (t nil))))
    (mapcar (lambda (item)
              (let ((name (string-downcase (nlk:trimmed (princ-to-string item)))))
                (cond ((member name +forbidden-fields+ :test #'equal)
                       (fail "~a can never be shared on a call, consent or not" name))
                      ((not (member name +sensitive-fields+ :test #'equal))
                       (fail "~a is not a sensitive field this cell knows: ~{~a~^, ~}"
                             name +sensitive-fields+))
                      (t name))))
            items)))
