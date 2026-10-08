;;;; sigv4.lisp --- AWS Signature Version 4: the digests, the canonical request, the signature.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/ai/src/providers/
;;;; aws-sigv4.ts, which matches @smithy/signature-v4 for header-based
;;;; signing with a full SHA-256 payload hash (Bedrock requires it). The
;;;; signed headers are host, x-amz-date, x-amz-content-sha256,
;;;; x-amz-security-token when the credentials carry a session token, and
;;;; whatever signable headers the caller adds (content-type, accept); the
;;;; path is escaped once more per segment, as a service other than S3
;;;; expects.
;;;;
;;;; SHA-256 is the core's (OpenSSL's, through NLK:SHA256-TEXT); HMAC and
;;;; SHA-1 (the SSO cache's file names) are written out here.

(in-package #:nodecode-amazon-bedrock)

(defun octets (text)
  "TEXT's UTF-8 octets; octets as they are."
  (if (stringp text)
      (sb-ext:string-to-octets text :external-format :utf-8)
      (coerce text '(vector (unsigned-byte 8)))))

(defun hex (octets)
  "OCTETS as lower-case hex."
  (with-output-to-string (out)
    (loop for octet across octets do (format out "~(~2,'0x~)" octet))))

(defun sha256-hex (data)
  "The SHA-256 of DATA (text or octets), as lower-case hex."
  (subseq (nlk:sha256-text (octets data)) 7))

(defun sha256 (data)
  "The SHA-256 of DATA (text or octets), as octets."
  (let ((hex (sha256-hex data)))
    (coerce (loop for at from 0 below 64 by 2
                  collect (parse-integer hex :start at :end (+ at 2) :radix 16))
            '(vector (unsigned-byte 8)))))

(defun hmac-sha256 (key data)
  "HMAC-SHA256 of DATA under KEY (both text or octets), as octets (RFC 2104)."
  (let* ((key (octets key))
         (key (if (> (length key) 64) (sha256 key) key))
         (block (concatenate '(vector (unsigned-byte 8)) key (make-array (- 64 (length key)) :initial-element 0)))
         (inner (map '(vector (unsigned-byte 8)) (lambda (octet) (logxor octet #x36)) block))
         (outer (map '(vector (unsigned-byte 8)) (lambda (octet) (logxor octet #x5c)) block)))
    (sha256 (concatenate '(vector (unsigned-byte 8)) outer
                         (sha256 (concatenate '(vector (unsigned-byte 8)) inner (octets data)))))))

(defun sha1-hex (data)
  "The SHA-1 of DATA (text or octets), as lower-case hex (FIPS 180-4)."
  (let* ((message (octets data))
         (length (length message))
         (padded (make-array (* 64 (ceiling (+ length 9) 64)) :element-type '(unsigned-byte 8) :initial-element 0))
         (h (list #x67452301 #xEFCDAB89 #x98BADCFE #x10325476 #xC3D2E1F0))
         (w (make-array 80)))
    (replace padded message)
    (setf (aref padded length) #x80)
    (loop for shift from 0 below 8
          do (setf (aref padded (- (length padded) 1 shift)) (ldb (byte 8 (* 8 shift)) (* 8 length))))
    (flet ((rotl (value count) (logand #xFFFFFFFF (logior (ash value count) (ash value (- count 32))))))
      (loop for chunk from 0 below (length padded) by 64
            do (loop for i below 16
                     do (setf (aref w i) (loop with value = 0 for j below 4
                                               do (setf value (logior (ash value 8) (aref padded (+ chunk (* 4 i) j))))
                                               finally (return value))))
               (loop for i from 16 below 80
                     do (setf (aref w i) (rotl (logxor (aref w (- i 3)) (aref w (- i 8)) (aref w (- i 14)) (aref w (- i 16))) 1)))
               (destructuring-bind (a b c d e) h
                 (loop for i below 80
                       do (multiple-value-bind (f k)
                              (cond ((< i 20) (values (logior (logand b c) (logand (logxor b #xFFFFFFFF) d)) #x5A827999))
                                    ((< i 40) (values (logxor b c d) #x6ED9EBA1))
                                    ((< i 60) (values (logior (logand b c) (logand b d) (logand c d)) #x8F1BBCDC))
                                    (t (values (logxor b c d) #xCA62C1D6)))
                            (let ((temp (logand #xFFFFFFFF (+ (rotl a 5) f e k (aref w i)))))
                              (setf e d d c c (rotl b 30) b a a temp))))
                 (setf h (mapcar (lambda (x y) (logand #xFFFFFFFF (+ x y))) h (list a b c d e)))))
      (format nil "~(~{~8,'0x~}~)" h))))

(defun signing-key (secret short-date region service)
  "The SigV4 signing key: the HMAC chain from AWS4<SECRET> through the date,
REGION, SERVICE and aws4_request."
  (hmac-sha256 (hmac-sha256 (hmac-sha256 (hmac-sha256 (concatenate 'string "AWS4" secret) short-date)
                                         region)
                            service)
               "aws4_request"))

(defun amz-date (universal-time)
  "(values LONG SHORT): UNIVERSAL-TIME as YYYYMMDDTHHMMSSZ and YYYYMMDD, in UTC."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal-time 0)
    (let ((short (format nil "~4,'0d~2,'0d~2,'0d" year month day)))
      (values (format nil "~aT~2,'0d~2,'0d~2,'0dZ" short hour minute second) short))))

(defun encode-rfc3986 (text)
  "TEXT percent-encoded as RFC 3986 unreserved-only: encodeURIComponent, with
! ' ( ) * encoded too."
  (with-output-to-string (out)
    (loop for octet across (octets text)
          for char = (code-char octet)
          do (if (and (< octet 128) (or (alphanumericp char) (find char "-_.~")))
                 (write-char char out)
                 (format out "%~2,'0X" octet)))))

(defun percent-decode (text)
  "TEXT with its %XX escapes decoded as UTF-8."
  (let ((out (make-array (length text) :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (loop with at = 0
          while (< at (length text))
          do (let ((char (char text at)))
               (if (and (char= char #\%) (<= (+ at 3) (length text))
                        (digit-char-p (char text (+ at 1)) 16) (digit-char-p (char text (+ at 2)) 16))
                   (progn (vector-push (parse-integer text :start (1+ at) :end (+ at 3) :radix 16) out)
                          (incf at 3))
                   (progn (loop for octet across (octets (string char)) do (vector-push-extend octet out))
                          (incf at)))))
    (sb-ext:octets-to-string (coerce out '(vector (unsigned-byte 8))) :external-format :utf-8)))

(defun canonical-path (path)
  "PATH with each segment escaped once more, the slashes kept."
  (format nil "~{~a~^/~}" (mapcar (lambda (segment) (if (zerop (length segment)) "" (encode-rfc3986 segment)))
                                  (uiop:split-string path :separator "/"))))

(defun canonical-query (query)
  "QUERY's pairs each decoded and encoded again, then sorted by the encoded
name and value (the spec sorts the encoded form)."
  (let ((pairs (loop for part in (uiop:split-string (or query "") :separator "&")
                     when (plusp (length part))
                       collect (let ((equals (position #\= part)))
                                 (cons (encode-rfc3986 (percent-decode (subseq part 0 (or equals (length part)))))
                                       (encode-rfc3986 (if equals (percent-decode (subseq part (1+ equals))) "")))))))
    (format nil "~{~a~^&~}"
            (mapcar (lambda (pair) (format nil "~a=~a" (car pair) (cdr pair)))
                    (sort pairs (lambda (a b) (or (string< (car a) (car b))
                                                  (and (string= (car a) (car b)) (string< (cdr a) (cdr b))))))))))

(defparameter +unsignable+
  '("authorization" "cache-control" "connection" "expect" "from" "keep-alive" "max-forwards" "pragma"
    "referer" "te" "trailer" "transfer-encoding" "upgrade" "user-agent" "x-amzn-trace-id")
  "Headers the SDK never signs.")

(defun sign-request (&key (method "POST") host path query headers body region service
                          access-key secret-key session-token (time (get-universal-time)))
  "The headers that sign one request (omp's signRequest): host, x-amz-date,
x-amz-content-sha256, x-amz-security-token with a session token, and
authorization, as an alist. HEADERS are the caller's own, signed beside them."
  (multiple-value-bind (long-date short-date) (amz-date time)
    (let* ((payload-hash (sha256-hex (or body #())))
           (signed (append `(("host" . ,host) ("x-amz-date" . ,long-date) ("x-amz-content-sha256" . ,payload-hash))
                           (when session-token `(("x-amz-security-token" . ,session-token))))))
      (loop for (name . value) in headers
            for lower = (string-downcase name)
            unless (or (member lower +unsignable+ :test #'equal)
                       (uiop:string-prefix-p "proxy-" lower) (uiop:string-prefix-p "sec-" lower))
              do (setf signed (cons (cons lower (ppcre:regex-replace-all "\\s+" (string-trim " " value) " "))
                                    (remove lower signed :key #'car :test #'equal))))
      (let* ((sorted (sort (copy-list signed) #'string< :key #'car))
             (signed-names (format nil "~{~a~^;~}" (mapcar #'car sorted)))
             (canonical (format nil "~:@(~a~)~%~a~%~a~%~{~a~%~}~%~a~%~a"
                                method (canonical-path path) (canonical-query query)
                                (mapcar (lambda (pair) (format nil "~a:~a" (car pair) (cdr pair))) sorted)
                                signed-names payload-hash))
             (scope (format nil "~a/~a/~a/aws4_request" short-date region service))
             (to-sign (format nil "AWS4-HMAC-SHA256~%~a~%~a~%~a" long-date scope (sha256-hex canonical)))
             (signature (hex (hmac-sha256 (signing-key secret-key short-date region service) to-sign))))
        (append `(("host" . ,host)
                  ("x-amz-date" . ,long-date)
                  ("x-amz-content-sha256" . ,payload-hash)
                  ("authorization" . ,(format nil "AWS4-HMAC-SHA256 Credential=~a/~a, SignedHeaders=~a, Signature=~a"
                                              access-key scope signed-names signature)))
                (when session-token `(("x-amz-security-token" . ,session-token))))))))
