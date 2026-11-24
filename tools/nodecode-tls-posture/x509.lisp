;;;; x509.lisp --- the certificate bytes: PEM to DER, and just enough X.509.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin leaned on Python's `cryptography` when it was
;;;; importable and on `ssl` otherwise, and recorded the checks it could not
;;;; run when neither was. Here one small decoder always answers the same
;;;; four facts -- validity, subject and issuer, key size, signature digest --
;;;; so no check is skipped for want of a library. It reads the DER a PEM
;;;; file holds and nothing else: no crypto is performed, because none is
;;;; needed to report a key length or a digest name.
;;;;
;;;; A number or a date read from a byte string is UNTRUSTED here, exactly
;;;; as every other value read off disk is: the caller marks the evidence.

(in-package #:nodecode-tls-posture)
;;; --- base64, the PEM body ---------------------------------------------------

(defparameter +base64+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The standard alphabet: a PEM body decodes without a library.")

(defun base64-decode (text)
  "TEXT's base64 characters as octets: whitespace is skipped, and `=' is
the padding it looks like, not data."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (acc 0) (bits 0))
    (loop for character across text
          for value = (position character +base64+ :test #'char=)
          when value
            do (setf acc (logior (ash acc 6) value)
                     bits (+ bits 6))
               (when (>= bits 8)
                 (decf bits 8)
                 (vector-push-extend (ldb (byte 8 bits) acc) out)))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun pem-certificates (text)
  "Every certificate body TEXT holds, as DER octet vectors, in order.
A file that holds two of them holds a chain; counting the blocks is how an
incomplete one is told from a complete one."
  (let ((certificates '())
        (body (make-string-output-stream))
        (inside nil))
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (let ((line (string-trim '(#\Space #\Tab #\Return) line)))
        (cond ((search "-----BEGIN CERTIFICATE-----" line)
               (setf inside t
                     body (make-string-output-stream)))
              ((search "-----END CERTIFICATE-----" line)
               (when inside
                 (let ((der (base64-decode (get-output-stream-string body))))
                   (when (plusp (length der)) (push der certificates))))
               (setf inside nil))
              (inside (write-string line body)))))
    (nreverse certificates)))

;;; --- DER, read strictly enough to refuse what it cannot read -----------------

(defun der-tlv (bytes position)
  "The element at POSITION => (values TAG CONTENT-START CONTENT-END NEXT).
A header longer than the bytes it sits in reads as far as it goes, which is
what an unreadable blob is."
  (let* ((tag (aref bytes position))
         (first (aref bytes (1+ position)))
         (count (logand first #x7f)))
    (if (< first 128)
        (values tag (+ position 2) (+ position 2 first) (+ position 2 first))
        (let ((length 0))
          (dotimes (index count)
            (setf length (logior (ash length 8) (aref bytes (+ position 2 index)))))
          (let ((start (+ position 2 count)))
            (values tag start (+ start length) (+ start length)))))))

(defun der-children (bytes start end)
  "Every element between START and END, as (TAG CONTENT-START NEXT)."
  (loop while (< start end)
        collect (multiple-value-bind (tag content-start content-end next)
                    (der-tlv bytes start)
                  (declare (ignore content-end))
                  (prog1 (list tag content-start next)
                    (setf start next)))))

(defun der-oid (bytes start end)
  "The object identifier at START as `1.2.840.113549.1.1.11'."
  (let* ((first (aref bytes start))
         (parts (list (floor first 40) (mod first 40)))
         (value 0))
    (loop for index from (1+ start) below end
          do (let ((byte (aref bytes index)))
               (setf value (logior (ash value 7) (logand byte #x7f)))
               (unless (logtest byte #x80)
                 (setf parts (append parts (list value))
                       value 0))))
    (format nil "~{~d~^.~}" parts)))

(defun der-string (bytes start end)
  "START..END as text: UTF-8 where it decodes, the octets as characters
where it does not. A name is reported, never trusted."
  (handler-case (sb-ext:octets-to-string (subseq bytes start end) :external-format :utf-8)
    (error () (map 'string #'code-char (subseq bytes start end)))))

(defparameter +attribute-names+
  '(("2.5.4.3" . "CN") ("2.5.4.6" . "C") ("2.5.4.7" . "L") ("2.5.4.8" . "ST")
    ("2.5.4.10" . "O") ("2.5.4.11" . "OU") ("2.5.4.4" . "SN") ("2.5.4.5" . "serialNumber")
    ("2.5.4.42" . "GN") ("1.2.840.113549.1.9.1" . "emailAddress"))
  "The attributes worth naming; any other is written as its own OID.")

(defun der-name (bytes start end)
  "An X.501 Name as `CN=example.com, O=Example': a fact about text, never a
path and never an instruction."
  (let ((parts '()))
    (dolist (set (der-children bytes start end))
      (dolist (sequence (der-children bytes (second set) (third set)))
        (let* ((fields (der-children bytes (second sequence) (third sequence)))
               (oid (first fields))
               (value (second fields)))
          (when (and oid value (= #x06 (first oid)))
            (let ((name (der-oid bytes (second oid) (third oid))))
              (push (format nil "~a=~a"
                            (or (cdr (assoc name +attribute-names+ :test #'string=)) name)
                            (der-string bytes (second value) (third value)))
                    parts))))))
    (format nil "~{~a~^, ~}" (nreverse parts))))

;;; --- time, as the audit compares it -----------------------------------------

(defun civil-days (year month day)
  "Days from 1970-01-01 to YEAR-MONTH-DAY (Howard Hinnant's epoch shift):
whole-day arithmetic with no calendar library and no leap-second table."
  (let* ((year (if (<= month 2) (1- year) year))
         (era (floor year 400))
         (year-of-era (- year (* era 400)))
         (day-of-year (+ (floor (* 153 (+ month (if (> month 2) -3 9))) 5) (1- day)))
         (day-of-era (+ (* year-of-era 365) (floor year-of-era 4)
                        (- (floor year-of-era 100)) day-of-year)))
    (- (+ (* era 146097) day-of-era) 719468)))

(defun cert-time (year month day hour minute second)
  "YEAR..SECOND as a universal time, so a validity bound compares with
(GET-UNIVERSAL-TIME) and nothing else."
  (+ 2208988800 (* 86400 (civil-days year month day))
     (* 3600 hour) (* 60 minute) second))

(defun parse-cert-time (text generalized-p)
  "TEXT as (values UNIVERSAL-TIME ISO-8601), or NIL when it is neither form.
UTCTime is the two-digit year X.509 still writes; GeneralizedTime the four."
  (let* ((digits (remove-if-not #'digit-char-p text))
         (width (if generalized-p 4 2))
         (wanted (+ width 10)))
    (when (>= (length digits) wanted)
      (let* ((year (if generalized-p
                        (parse-integer digits :start 0 :end 4)
                        (let ((short (parse-integer digits :start 0 :end 2)))
                          (if (< short 50) (+ 2000 short) (+ 1900 short)))))
             (at width)
             (month (parse-integer digits :start at :end (+ at 2)))
             (day (parse-integer digits :start (+ at 2) :end (+ at 4)))
             (hour (parse-integer digits :start (+ at 4) :end (+ at 6)))
             (minute (parse-integer digits :start (+ at 6) :end (+ at 8)))
             (second (parse-integer digits :start (+ at 8) :end (+ at 10))))
        (values (cert-time year month day hour minute second)
                (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
                        year month day hour minute second))))))

;;; --- the fields a posture check reads ---------------------------------------

(defparameter +signature-digests+
  '(("1.2.840.113549.1.1.4" . "md5") ("1.2.840.113549.1.1.5" . "sha1")
    ("1.2.840.10045.4.1" . "sha1") ("1.2.840.10040.4.3" . "sha1")
    ("1.2.840.113549.1.1.11" . "sha256") ("1.2.840.113549.1.1.12" . "sha384")
    ("1.2.840.113549.1.1.13" . "sha512")
    ("1.2.840.10045.4.3.2" . "sha256") ("1.2.840.10045.4.3.3" . "sha384")
    ("1.2.840.10045.4.3.4" . "sha512") ("2.16.840.1.101.3.4.3.2" . "sha256"))
  "Signature algorithms, by digest. Ed25519 and Ed448 have no separate
digest and are not weak for it, so they map to themselves below.")

(defparameter +curve-bits+
  '(("1.2.840.10045.3.1.7" . 256)    ; prime256v1 / P-256
    ("1.3.132.0.33" . 224)           ; secp224r1
    ("1.3.132.0.34" . 384)           ; secp384r1
    ("1.3.132.0.35" . 521)           ; secp521r1
    ("1.3.132.0.10" . 256))          ; secp256k1
  "Named curves and their field size. An unnamed curve leaves the key size
unknown rather than guessed.")

(defun integer-bits (bytes start end)
  "The bit length of the unsigned magnitude at START..END: leading zero
octets are the encoding's padding, and what is left is the number."
  (let ((at start))
    (loop while (and (< at end) (zerop (aref bytes at))) do (incf at))
    (if (>= at end)
        0
        (+ (* 8 (- end at 1))
           (let ((first (aref bytes at)) (bits 0))
             (loop while (plusp first) do (incf bits) (setf first (ash first -1)))
             bits)))))

(defun key-size-of (bytes algorithm bits)
  "The key ALGORITHM names, as (values TYPE BITS): BITS is NIL when the
encoding does not say, which a check records as skipped, never as passing."
  (let* ((fields (der-children bytes (second algorithm) (third algorithm)))
         (oid (first fields))
         (parameters (second fields))
         (name (and oid (der-oid bytes (second oid) (third oid)))))
    (cond
      ((equal name "1.2.840.113549.1.1.1")           ; RSA
       (let* ((payload (1+ (second bits)))
              (public (multiple-value-list (der-tlv bytes payload)))
              (integers (der-children bytes (second public) (third public)))
              (modulus (first integers)))
         (values "RSA" (and modulus (integer-bits bytes (second modulus) (third modulus))))))
      ((equal name "1.2.840.10045.2.1")              ; EC
       (values "EC"
               (and parameters (cdr (assoc (der-oid bytes (second parameters) (third parameters))
                                            +curve-bits+ :test #'string=)))))
      ((equal name "1.2.840.10040.4.1")              ; DSA
       (let* ((triple (and parameters (der-children bytes (second parameters) (third parameters))))
              (p (first triple)))
         (values "DSA" (and p (integer-bits bytes (second p) (third p))))))
      ((equal name "1.3.101.112") (values "Ed25519" 256))
      ((equal name "1.3.101.113") (values "Ed448" 448))
      (t (values (or name "unknown") nil)))))

(defstruct cert
  "One certificate, as much of it as a posture check reads. DETAIL is `full'
when every field came out of the DER, `dates' when only validity did."
  (subject "" :type string)
  (issuer "" :type string)
  (not-before nil)
  (not-after nil)
  (not-before-iso "" :type string)
  (not-after-iso "" :type string)
  (key-type "" :type string)
  (key-bits nil)
  (sig-algo "" :type string)
  (self-signed-p nil)
  (detail "dates" :type string))

(defun decode-certificate (der)
  "DER as a CERT, or NIL when it is not a certificate of the shape a TLS
server presents. Never signals: an unreadable file is a skipped check, not
an error in the operator's face."
  (handler-case
      (multiple-value-bind (tag start end) (der-tlv der 0)
        (unless (= tag #x30) (return-from decode-certificate nil))
        (let* ((outer (der-children der start end))
               (tbs (first outer))
               (signature (second outer)))
          (unless (and tbs signature (= #x30 (first tbs)) (= #x30 (first signature)))
            (return-from decode-certificate nil))
          (let* ((body (der-children der (second tbs) (third tbs)))
                 (body (if (and body (= #xa0 (first (first body)))) (rest body) body))
                 (serial (first body))
                 (signing (second body))
                 (issuer (third body))
                 (validity (fourth body))
                 (subject (fifth body))
                 (spki (sixth body)))
            (declare (ignore serial signing))
            (unless (and issuer validity subject spki (= #x30 (first validity)))
              (return-from decode-certificate nil))
            (let* ((times (der-children der (second validity) (third validity)))
                   (before (first times))
                   (after (second times))
                   (subject-name (der-name der (second subject) (third subject)))
                   (issuer-name (der-name der (second issuer) (third issuer)))
                   (signature-fields (der-children der (second signature) (third signature)))
                   (signature-oid (first signature-fields))
                   (signature-name (and signature-oid
                                         (der-oid der (second signature-oid) (third signature-oid))))
                   (algorithm (first (der-children der (second spki) (third spki))))
                   (key (second (der-children der (second spki) (third spki)))))
              (multiple-value-bind (before-time before-iso)
                  (and before (parse-cert-time (der-string der (second before) (third before))
                                               (= #x18 (first before))))
                (multiple-value-bind (after-time after-iso)
                    (and after (parse-cert-time (der-string der (second after) (third after))
                                                (= #x18 (first after))))
                  (multiple-value-bind (key-type key-bits)
                      (and algorithm key (key-size-of der algorithm key))
                    (make-cert
                     :subject subject-name
                     :issuer issuer-name
                     :not-before before-time
                     :not-after after-time
                     :not-before-iso (or before-iso "")
                     :not-after-iso (or after-iso "")
                     :key-type (or key-type "")
                     :key-bits key-bits
                     :sig-algo (or (cdr (assoc signature-name +signature-digests+ :test #'string=))
                                    (and (member signature-name '("1.3.101.112" "1.3.101.113")
                                                 :test #'string=)
                                         (if (string= signature-name "1.3.101.112")
                                             "ed25519" "ed448"))
                                    "")
                     :self-signed-p (and (plusp (length subject-name))
                                         (string= subject-name issuer-name))
                     :detail "full"))))))))
    (error () nil)))
