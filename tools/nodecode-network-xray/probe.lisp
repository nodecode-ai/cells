;;;; probe.lisp --- one URL, four stages, the evidence between them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin's collector is one direct socket walking DNS, TCP,
;;;; verified TLS and one HTTP HEAD, with what each stage answered kept as
;;;; evidence: what resolved, which address was selected, whether the
;;;; certificate verified, what the status line said. Nothing here is a
;;;; scanner -- one URL the user named, one address, no proxy, no redirect
;;;; followed, no body, no credential.
;;;;
;;;; The stages are functions in variables (*RESOLVER*, *CONNECTOR*,
;;;; *HANDSHAKER*, *HEAD*) so a test proves the ladder, the address the probe
;;;; selected and the sealing of a bundle without opening a socket; and the
;;;; bundle's field names are the original's, so a bundle the original
;;;; plugin's collector wrote reads and compares here.

(in-package #:nodecode-network-xray)

;;; --- a stage's refusal ------------------------------------------------------------

(define-condition stage-failure (error)
  ((code :initarg :code :reader stage-failure-code)
   (text :initarg :text :reader stage-failure-text))
  (:report (lambda (condition stream) (write-string (stage-failure-text condition) stream)))
  (:documentation "One stage's refusal, with the code the evidence carries."))

(defun stage-fail (code format-control &rest arguments)
  "Signal STAGE-FAILURE with CODE and a formatted detail."
  (error 'stage-failure :code code :text (apply #'format nil format-control arguments)))

;;; --- the target: what a user meant, canonicalised ----------------------------------

(defparameter *default-ports* '(("https" . 443) ("http" . 80))
  "The port each scheme means when the URL names none.")

(defun default-port (scheme)
  "The port SCHEME means when a URL names none."
  (or (cdr (assoc scheme *default-ports* :test #'string=)) 80))

(defun ipv4-parts (text)
  "TEXT's four octets when it is a dotted quad, else NIL."
  (let ((parts (uiop:split-string text :separator ".")))
    (when (and (= 4 (length parts))
               (every (lambda (part)
                        (and (plusp (length part)) (<= (length part) 3)
                             (every #'digit-char-p part)
                             (or (= 1 (length part)) (not (char= #\0 (char part 0))))
                             (<= (parse-integer part) 255)))
                      parts))
      (mapcar #'parse-integer parts))))

(defun numeric-host-p (text)
  "Whether TEXT looks like a number a resolver would read as an address."
  (and (plusp (length text))
       (every (lambda (character) (or (digit-char-p character) (char= character #\.))) text)))

(defun permitted-ip (text)
  "TEXT as a canonical IP, refusing what the original refused: link-local,
multicast and unspecified destinations say nothing about the service meant."
  (cond
    ((find #\% text) (stage-fail "invalid_address" "an unscoped numeric address is required"))
    ((numeric-host-p text)
     (let ((parts (ipv4-parts text)))
       (unless parts (stage-fail "invalid_address" "~a is not a dotted-quad address" text))
       (destructuring-bind (a b c d) parts
         (declare (ignore c d))
         (when (= a 0) (stage-fail "invalid_address" "the unspecified address is not a probe target"))
         (when (and (= a 169) (= b 254))
           (stage-fail "invalid_address" "a link-local address is not a probe target"))
         (when (<= 224 a 239) (stage-fail "invalid_address" "a multicast address is not a probe target")))
       (format nil "~{~d~^.~}" parts)))
    ((find #\: text)
     (let ((host (string-downcase text)))
       (when (string= host "::")
         (stage-fail "invalid_address" "the unspecified address is not a probe target"))
       (when (uiop:string-prefix-p "ff" host)
         (stage-fail "invalid_address" "a multicast address is not a probe target"))
       (when (member (subseq host 0 (min 4 (length host))) '("fe80") :test #'string=)
         (stage-fail "invalid_address" "a link-local address is not a probe target"))
       host))
    (t nil)))

(defun check-host (text)
  "TEXT as a host this probe may dial: a canonical IP, or a lowercase name.
=> (values HOST LITERAL-P)."
  (let ((ip (permitted-ip text)))
    (cond (ip (values ip t))
          ((numeric-host-p text)
           (stage-fail "invalid_host" "~a reads as a number but not as an address" text))
          (t
           (let ((host (string-downcase text)))
             (unless (<= 1 (length host) 253)
               (stage-fail "invalid_host" "a host name is 1..253 characters"))
             (dolist (label (uiop:split-string (string-right-trim "." host) :separator "."))
               (unless (and (<= 1 (length label) 63)
                            (every (lambda (character)
                                     (or (alphanumericp character) (char= character #\-)))
                                   label)
                            (alphanumericp (char label 0))
                            (alphanumericp (char label (1- (length label)))))
                 (stage-fail "invalid_host" "~a is not a valid host name" text)))
             (values host nil))))))

(defparameter *path-characters*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._~!$&'()*+,;=:@/%-"
  "The characters a path this probe will send may hold.")

(defun check-path (path)
  "PATH as the original's rule admits it: URI path characters and
well-formed percent escapes."
  (unless (and (plusp (length path)) (char= #\/ (char path 0)))
    (stage-fail "invalid_path" "a path begins with a slash"))
  (loop for index from 0 below (length path)
        for character = (char path index)
        do (unless (find character *path-characters* :test #'char=)
             (stage-fail "invalid_path" "~s is not a URI path character" character))
           (when (char= character #\%)
             (unless (and (< (+ index 2) (length path))
                          (digit-char-p (char path (1+ index)) 16)
                          (digit-char-p (char path (+ index 2)) 16))
               (stage-fail "invalid_path" "a percent escape carries two hex digits"))))
  path)

(defun split-authority (authority)
  "AUTHORITY as (values HOST PORT), the port NIL when it names none."
  (if (and (plusp (length authority)) (char= #\[ (char authority 0)))
      (let ((close (position #\] authority)))
        (unless close (stage-fail "invalid_host" "a bracketed host is closed with ]"))
        (let ((host (subseq authority 1 close))
              (rest (subseq authority (1+ close))))
          (unless (find #\: host) (stage-fail "invalid_host" "a bracketed host is an IPv6 address"))
          (cond ((zerop (length rest)) (values host nil))
                ((and (> (length rest) 1) (char= #\: (char rest 0))
                      (every #'digit-char-p (subseq rest 1)))
                 (values host (parse-integer (subseq rest 1))))
                (t (stage-fail "invalid_port" "~a is not a port" rest)))))
      (let ((colon (position #\: authority :from-end t)))
        (cond ((null colon) (values authority nil))
              ((and (> (length authority) (1+ colon))
                    (every #'digit-char-p (subseq authority (1+ colon))))
               (when (find #\: (subseq authority 0 colon))
                 (stage-fail "invalid_host" "a host with a colon is bracketed"))
               (values (subseq authority 0 colon) (parse-integer (subseq authority (1+ colon)))))
              (t (values authority nil))))))

(defun parse-target (url &optional connect-ip)
  "URL as the target a probe dials, or a refusal saying why it cannot be one.
=> the target as the JSON object a bundle records."
  (unless (and (stringp url) (<= 1 (length url) 2048))
    (stage-fail "invalid_url" "a URL is 1..2048 characters"))
  (when (some (lambda (character) (or (<= (char-code character) 32) (= (char-code character) 127)))
              url)
    (stage-fail "invalid_url" "a URL carries no whitespace or control characters"))
  (when (find-if (lambda (character) (find character "\\?#" :test #'char=)) url)
    (stage-fail "invalid_url"
                "a URL carries no query, fragment or backslash: the probe sends a HEAD for the path alone"))
  (let* ((mark (search "://" url))
         (scheme (and mark (string-downcase (subseq url 0 mark))))
         (rest (and mark (subseq url (+ mark 3)))))
    (unless (and rest (member scheme '("http" "https") :test #'string=))
      (stage-fail "invalid_url" "the URL must begin http:// or https://"))
    (let* ((slash (position #\/ rest))
           (authority (subseq rest 0 slash))
           (path (if slash (subseq rest slash) "/")))
      (unless (plusp (length authority)) (stage-fail "invalid_url" "the URL names no host"))
      (when (find #\@ authority) (stage-fail "invalid_url" "the URL carries no credentials"))
      (multiple-value-bind (host port) (split-authority authority)
        (multiple-value-bind (host literal) (check-host host)
          (declare (ignore literal))
          (let ((port (or port (default-port scheme))))
            (unless (<= 1 port 65535) (stage-fail "invalid_port" "a port is 1..65535"))
            (check-path path)
            (let ((override (and connect-ip (plusp (length connect-ip))
                                 (permitted-ip connect-ip))))
              (let* ((shown (if (find #\: host) (format nil "[~a]" host) host))
                     (shown (if (= port (default-port scheme)) shown
                                (format nil "~a:~d" shown port)))
                     (canonical (format nil "~a://~a~a" scheme shown path)))
                (when (> (length canonical) 2048)
                  (stage-fail "invalid_url" "the canonical URL is over 2048 characters"))
                (nlk:make-json-object
                 "url" canonical "scheme" scheme "host" host "port" port
                 "path" path "connect_ip" (or override :null))))))))))

;;; --- the seal: a digest over the canonical bytes -----------------------------------

(defun write-json-string (text stream)
  "TEXT as a JSON string, ASCII escapes for the controls and the two quotes."
  (write-char #\" stream)
  (loop for character across text
        do (case character
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (t (if (< (char-code character) 32)
                    (format stream "\\u~4,'0x" (char-code character))
                    (write-char character stream)))))
  (write-char #\" stream))

(defun canonical-json (value)
  "VALUE as canonical JSON text: object keys sorted, nothing between them.
The digest that seals a bundle is taken over this, so the same evidence is
the same bytes in any image, whatever order a table hands its keys in."
  (with-output-to-string (out)
    (labels ((walk (value)
               (cond ((hash-table-p value)
                      (write-char #\{ out)
                      (loop for (key . rest) on (sort (loop for key being the hash-keys of value
                                                           collect key)
                                                      #'string<)
                            do (when rest (write-char #\, out))
                               (write-json-string key out)
                               (write-char #\: out)
                               (walk (gethash key value)))
                      (write-char #\} out))
                     ((and (vectorp value) (not (stringp value)))
                      (write-char #\[ out)
                      (loop for index from 0 below (length value)
                            do (when (plusp index) (write-char #\, out))
                               (walk (aref value index)))
                      (write-char #\] out))
                     ((stringp value) (write-json-string value out))
                     ((eq value t) (write-string "true" out))
                     ((eq value :null) (write-string "null" out))
                     ((null value) (write-string "false" out))
                     ((integerp value) (format out "~d" value))
                     (t (write-json-string (princ-to-string value) out)))))
      (walk value))))

(defun sha256-hex (text)
  "TEXT's SHA-256, lowercase hex, over its UTF-8 bytes."
  (string-downcase
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence :sha256
                              (sb-ext:string-to-octets text :external-format :utf-8)))))

(defun seal (bundle)
  "BUNDLE with its integrity member: SHA-256 over its own canonical bytes."
  (remhash "integrity" bundle)
  (setf (gethash "integrity" bundle)
        (nlk:make-json-object "algorithm" "sha256"
                               "digest" (sha256-hex (canonical-json bundle))))
  bundle)

(defun digest-matches-p (bundle)
  "Whether BUNDLE's recorded digest is the digest of its own content: the
integrity member is left out of the bytes it seals."
  (let ((digest (nlk:json-value bundle :string "integrity" "digest"))
        (content (nlk:copy-json-object bundle)))
    (remhash "integrity" content)
    (and (stringp digest) (string= digest (sha256-hex (canonical-json content))))))

;;; --- the evidence a record holds ----------------------------------------------------

(defparameter *stage-names* #("dns" "tcp" "tls" "http")
  "The ladder every bundle records, in the order a probe walks it.")

(defparameter *policy*
  (nlk:make-json-object "method" "HEAD" "redirects" "not_followed"
                         "tls_verification" t "proxy" "direct" "body_capture" nil)
  "What every probe here did, in the bundle's own words.")

(defun utc-now ()
  "Now as the ISO instant a bundle records."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun empty-stage (name)
  "NAME's stage, before it has seen anything."
  (nlk:make-json-object "name" name "status" "unknown" "duration_ms" 0
                         "summary" "No observation received"
                         "details" (nlk:make-json-object) "error" :null))

(defun empty-bundle (target label)
  "A bundle for TARGET taken under LABEL, every stage still unknown."
  (nlk:make-json-object
   "schema_version" 1
   "kind" "network_xray.probe"
   "id" (format nil "~(~32,'0x~)" (random (expt 16 32)))
   "captured_at" (utc-now)
   "vantage" (nlk:make-json-object "label" label "os" (string-downcase (software-type)))
   "target" target
   "policy" (nlk:copy-json-object *policy*)
   "capture" (nlk:make-json-object "status" "failed" "duration_ms" 0 "error" :null)
   "stages" (map 'vector #'empty-stage *stage-names*)))

(defun stage-at (bundle index)
  "BUNDLE's stage INDEX."
  (aref (gethash "stages" bundle) index))

(defun note-stage (stage status summary details error)
  "Record what a stage answered; => whether it answered."
  (setf (gethash "status" stage) status
        (gethash "summary" stage) summary
        (gethash "details" stage) details
        (gethash "error" stage) error)
  (equal status "ok"))

(defun error-object (code message)
  "One stage error as the bundle records it."
  (nlk:make-json-object "code" code "message" message))

(defparameter *text-limit* 240 "How much of an error's own words one stage keeps.")

(defun safe-text (text)
  "TEXT's printable characters, clipped: a peer's bytes never reach a record whole."
  (let ((text (princ-to-string text)))
    (subseq (map 'string (lambda (character)
                            (if (graphic-char-p character) character #\Space))
                 text)
            0 (min *text-limit* (length text)))))

(defun condition-code (condition)
  "The code and message a stage's CONDITION is recorded under."
  (cond
    ((typep condition 'stage-failure)
     (values (stage-failure-code condition) (stage-failure-text condition)))
    ((typep condition 'usocket:ns-error)
     (values "dns_failed" "Name resolution failed"))
    ((typep condition 'usocket:connection-refused-error)
     (values "connection_refused" "Connection refused"))
    ((or (typep condition 'usocket:timeout-error)
         (typep condition 'sb-sys:deadline-timeout))
     (values "socket_timeout" "Socket operation exceeded its deadline"))
    ((typep condition 'cl+ssl::cl+ssl-error)
     ;; CL+SSL exports ssl-error-verify but not the class every negotiation
     ;; failure inherits from, so the root is named once, here.
     (if (or (typep condition 'cl+ssl:ssl-error-verify)
             (search "verif" (princ-to-string condition) :test #'char-equal))
         (values "certificate_invalid" "Certificate verification failed")
         (values "tls_failed" "TLS negotiation failed")))
    (t (values "network_error" (safe-text condition)))))

(defparameter *milliseconds-per-unit* (/ 1000 internal-time-units-per-second)
  "Internal time units to milliseconds.")

(defun elapsed-ms (started)
  "The milliseconds since STARTED."
  (round (* (- (get-internal-real-time) started) *milliseconds-per-unit*)))

(defun run-stage (bundle index function)
  "Call FUNCTION, the stage's own work, and record what it answered in
BUNDLE's stage INDEX => whether it answered."
  (let ((stage (stage-at bundle index))
        (started (get-internal-real-time)))
    (prog1
        (handler-case
            (multiple-value-bind (summary details) (funcall function)
              (note-stage stage "ok" summary details :null))
          (condition (condition)
            (let ((name (string-upcase (gethash "name" stage))))
              (multiple-value-bind (code message) (condition-code condition)
                (note-stage stage "error" (format nil "~a failed" name)
                            (nlk:make-json-object) (error-object code message))))))
      (setf (gethash "duration_ms" stage) (elapsed-ms started)))))

;;; --- the four stages, over real sockets --------------------------------------------

(defun address-text (octets)
  "A resolver's address OCTETS as the text a record keeps."
  (if (= 4 (length octets))
      (format nil "~{~d~^.~}" (coerce octets 'list))
      (format nil "~(~{~4,'0x~^:~}~)"
              (loop for index from 0 below 16 by 2
                    collect (+ (* 256 (aref octets index)) (aref octets (1+ index)))))))

(defparameter *address-limit* 8 "How many resolved addresses one record keeps.")

(defun resolve-host (target)
  "TARGET's addresses from this machine's resolver => (values ADDRESSES TRUNCATED)."
  (let* ((host (gethash "host" target))
         (literal (or (ipv4-parts host) (find #\: host) (permitted-ip host))))
    (if literal
        (values (list host) nil)
        (handler-case
            (let ((addresses '()) (truncated nil))
              (dolist (octets (usocket:get-hosts-by-name host))
                (let ((address (address-text octets)))
                  (cond ((member address addresses :test #'string=))
                        ((< (length addresses) *address-limit*) (push address addresses))
                        (t (setf truncated t)))))
              (unless addresses
                (stage-fail "dns_failed" "the resolver answered no usable address for ~a" host))
              (values (nreverse addresses) truncated))
          (usocket:ns-error ()
            (stage-fail "dns_failed" "Name resolution failed"))))))

(defun open-socket (target address &optional (timeout 3))
  "One TCP connection to ADDRESS on TARGET's port => the socket."
  (sb-sys:with-deadline (:seconds timeout)
    (usocket:socket-connect address (gethash "port" target)
                            :element-type '(unsigned-byte 8)
                            :nodelay t :timeout timeout)))

(defun issuer-organization (oneline)
  "The O= of an X509_NAME_oneline string, else the whole of it, clipped."
  (let ((start (search "/O=" oneline)))
    (safe-text
     (if start
         (let* ((from (+ start 3)) (end (position #\/ oneline :start from)))
           (subseq oneline from (or end (length oneline))))
         oneline))))

(defun peer-facts (stream)
  "STREAM's handshake => (values PROTOCOL ISSUER), either NIL when OpenSSL
will not say: evidence is never invented."
  (let ((certificate (ignore-errors (cl+ssl:ssl-stream-x509-certificate stream))))
    (values
     (ignore-errors
       (cffi:foreign-funcall "SSL_get_version"
                              :pointer (cl+ssl::ssl-stream-handle stream) :string))
     (when (and certificate (not (cffi:null-pointer-p certificate)))
       (unwind-protect
            (ignore-errors
              (issuer-organization
               (cffi:with-foreign-pointer-as-string (buffer 512)
                 (cffi:foreign-funcall
                  "X509_NAME_oneline"
                  :pointer (cffi:foreign-funcall "X509_get_issuer_name"
                                                  :pointer certificate :pointer)
                  :pointer buffer :int 512 :pointer))))
         (cffi:foreign-funcall "X509_free" :pointer certificate :void))))))

(defun tls-handshake (target socket &optional (timeout 3))
  "One verified handshake over SOCKET under TARGET's host name
=> (values STREAM DETAILS)."
  (let* ((host (gethash "host" target))
         (stream (sb-sys:with-deadline (:seconds timeout)
                   (cl+ssl:make-ssl-client-stream (usocket:socket-stream socket)
                                                  :hostname host :verify :required))))
    (multiple-value-bind (protocol issuer) (peer-facts stream)
      (values stream
              (nlk:make-json-object "protocol" (or protocol :null)
                                     "issuer" (or issuer :null)
                                     "server_name" host
                                     "verified" t)))))

(defparameter *header-limit* 16384 "The most bytes of one response head a probe reads.")

(defun read-head (stream &optional (timeout 3))
  "STREAM's bytes through the blank line that ends a response head, read one
at a time so no body byte is taken => the head as text."
  (sb-sys:with-deadline (:seconds timeout)
    (let ((bytes (make-array 256 :element-type '(unsigned-byte 8)
                                 :fill-pointer 0 :adjustable t)))
      (loop
        (let ((byte (read-byte stream nil nil)))
          (unless byte (stage-fail "invalid_response" "Incomplete HTTP headers"))
          (vector-push-extend byte bytes)
          (when (> (length bytes) *header-limit*)
            (stage-fail "invalid_response" "HTTP headers exceed 16 KiB"))
          (let ((count (length bytes)))
            (when (and (>= count 4)
                       (= 13 (aref bytes (- count 4))) (= 10 (aref bytes (- count 3)))
                       (= 13 (aref bytes (- count 2))) (= 10 (aref bytes (1- count))))
              (return (sb-ext:octets-to-string bytes :external-format :latin-1)))))))))

(defun status-of (line)
  "The status code and reason of a head's first LINE => (values CODE REASON)."
  (let ((space (position #\Space line)))
    (unless (and space (uiop:string-prefix-p "HTTP/1." line))
      (stage-fail "invalid_response" "Malformed or unsupported HTTP status line"))
    (let* ((rest (subseq line (1+ space)))
           (code (ignore-errors (parse-integer rest :end (min 3 (length rest)))))
           (reason (if (> (length rest) 3) (string-trim " " (subseq rest 3)) "")))
      (unless (and code (<= 200 code 599))
        (stage-fail "invalid_response" "Malformed or unsupported HTTP status line"))
      (values code (safe-text reason)))))

(defun head-of (target)
  "The Host header TARGET's request carries, port included when it is not the default."
  (let ((host (gethash "host" target))
        (port (gethash "port" target)))
    (let ((shown (if (find #\: host) (format nil "[~a]" host) host)))
      (if (= port (default-port (gethash "scheme" target)))
          shown
          (format nil "~a:~d" shown port)))))

(defun http-head (target stream &optional (timeout 3))
  "One HEAD over STREAM for TARGET's path => (values SUMMARY DETAILS).
No query, no credential, no body, and a redirect is recorded, not followed."
  (let ((crlf (coerce (list #\Return #\Newline) 'string)))
    (write-sequence
     (sb-ext:string-to-octets
      (format nil "HEAD ~a HTTP/1.1~aHost: ~a~aUser-Agent: nodecode-network-xray/1~aAccept: */*~aConnection: close~a~a"
              (gethash "path" target) crlf (head-of target)
              crlf crlf crlf crlf crlf)
      :external-format :latin-1)
     stream)
    (force-output stream)
    (let* ((lines (remove "" (uiop:split-string (read-head stream timeout)
                                                :separator '(#\Return #\Newline))
                          :test #'string=)))
      (multiple-value-bind (code reason) (status-of (first lines))
        (let ((server "") (content-type ""))
          (dolist (line (rest lines))
            (let ((colon (position #\: line)))
              (when colon
                (let ((key (string-downcase (subseq line 0 colon)))
                      (value (string-trim " " (subseq line (1+ colon)))))
                  (cond ((string= key "server") (setf server (safe-text value)))
                        ((string= key "content-type")
                         (setf content-type (safe-text value)))))))
          (values (format nil "HTTP ~d" code)
                  (nlk:make-json-object "status_code" code "reason" reason
                                         "server" server "content_type" content-type
                                         "redirect" (<= 300 code 399)))))))))

;;; --- one probe, four stages --------------------------------------------------------

(defparameter *resolver* #'resolve-host
  "How a probe resolves the target's host: (target) => (values ADDRESSES TRUNCATED).")

(defparameter *connector* #'open-socket
  "How a probe opens its socket: (target address timeout) => a socket.")

(defparameter *handshaker* #'tls-handshake
  "How a probe verifies a certificate: (target socket timeout) => (values STREAM DETAILS).")

(defparameter *head* #'http-head
  "How a probe asks for the head: (target stream timeout) => (values SUMMARY DETAILS).")

(defparameter *closer* (lambda (socket) (when socket (usocket:socket-close socket)))
  "How a probe lets go of what it opened.")

(defun connect-ip (target)
  "TARGET's explicit connect_ip, or NIL."
  (let ((value (gethash "connect_ip" target)))
    (unless (eq value :null) value)))

(defun collect (target label)
  "TARGET, a parsed target, probed once from this machine under LABEL => the
sealed bundle. A capture that could not finish says so and keeps what it
saw: no stage is ever filled in from a guess."
  (let ((bundle (empty-bundle target label))
        (started (get-internal-real-time))
        (timeout (or (setting :timeout) 3))
        (override (connect-ip target))
        (address nil) (socket nil) (stream nil))
    (unwind-protect
         (progn
           ;; An explicit address is the address, whatever DNS answers, so a
           ;; resolution that hangs or fails cannot stop the experiment.
           (setf address override)
           (run-stage bundle 0
                      (lambda ()
                        (multiple-value-bind (addresses truncated)
                            (funcall *resolver* target)
                          (setf address (or override (first addresses)))
                          (values (format nil "Resolved ~d recorded address~:p"
                                          (length addresses))
                                  (nlk:make-json-object
                                   "addresses" (coerce addresses 'vector)
                                   "truncated" (and truncated t))))))
           (when (and address (or override (equal "ok" (gethash "status" (stage-at bundle 0)))))
             (run-stage bundle 1
                        (lambda ()
                          (setf socket (funcall *connector* target address timeout))
                          (values "Connected"
                                  (nlk:make-json-object
                                   "address" address "port" (gethash "port" target)
                                   "family" (if (find #\: address) "ipv6" "ipv4")
                                   "selection" (if override "override" "dns"))))))
           (cond
             ((string= "http" (gethash "scheme" target))
              (note-stage (stage-at bundle 2) "skipped" "Not applicable: HTTP"
                          (nlk:make-json-object) :null))
             ((equal "ok" (gethash "status" (stage-at bundle 1)))
              (run-stage bundle 2
                         (lambda ()
                           (multiple-value-bind (opened details)
                               (funcall *handshaker* target socket timeout)
                             (setf stream opened)
                             (values "Certificate verified" details))))))
           (when (or (and stream (equal "ok" (gethash "status" (stage-at bundle 2))))
                     (and (null stream)
                          (equal "skipped" (gethash "status" (stage-at bundle 2)))
                          (equal "ok" (gethash "status" (stage-at bundle 1)))))
             (run-stage bundle 3
                        (lambda ()
                          (let ((stage (stage-at bundle 3)))
                            (declare (ignore stage))
                            (multiple-value-bind (summary details)
                                (funcall *head* target (or stream (usocket:socket-stream socket))
                                         timeout)
                              (if (>= (gethash "status_code" details) 400)
                                  (progn
                                    (note-stage (stage-at bundle 3) "error" summary details
                                                (error-object "http_status"
                                                              "HTTP application error status"))
                                    (values "HTTP error status recorded" details))
                                  (values summary details)))))))
           (loop for index from 0 below (length *stage-names*)
                 for stage = (stage-at bundle index)
                 when (equal "unknown" (gethash "status" stage))
                   do (note-stage stage "skipped" "Not attempted after earlier failure"
                                  (nlk:make-json-object) :null))
           (setf (gethash "capture" bundle)
                 (nlk:make-json-object "status" "complete"
                                        "duration_ms" (elapsed-ms started)
                                        "error" :null))
           (seal bundle))
      (when stream (ignore-errors (close stream :abort t)))
      (when socket (ignore-errors (funcall *closer* socket))))))
