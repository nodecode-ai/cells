;;;; client.lisp --- the one network seam, the origin, the auth value, GOWA's records.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through *HTTP* -- NLK:HTTP -- so a test
;;;; stubs that one name and never opens a socket. GOWA (go-whatsapp-web-
;;;; multidevice) answers a handful of routes under one origin, and these are
;;;; the original's: GET /devices, GET /devices/{id}, POST /devices,
;;;; GET /devices/{id}/login, POST /devices/{id}/login/code?phone=, DELETE
;;;; /devices/{id}. The one other address is the QR image GOWA itself answers,
;;;; which must be on that same origin -- the original's rule, kept, so a
;;;; server cannot walk a Basic value to another host through it.

(in-package #:nodecode-gowa)

(defparameter *http* #'nlk:http
  "The cell's one network call. A test binds this, so nothing dials GOWA.")

(defparameter *max-json* (* 2 1024 1024)
  "How many bytes one JSON answer may carry.")

(defparameter *max-qr* (* 1024 1024)
  "How many bytes one QR image may carry.")

(defparameter *login-timeout* 135
  "A pairing request's ceiling in seconds: the account holder has to act.")

(defun text-value (value)
  "VALUE as a non-empty trimmed string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

;;; --- the origin ---------------------------------------------------------------

(defun loopback-host-p (host)
  "Whether HOST is one the machine itself answers on: the original's three."
  (member host '("127.0.0.1" "localhost" "::1") :test #'string-equal))

(defun split-origin (text)
  "TEXT as (values SCHEME HOST PORT) when it is an http(s) origin and nothing
else -- no path, query, fragment or credentials -- else NIL."
  (let* ((scheme (cond ((uiop:string-prefix-p "https://" text) "https")
                        ((uiop:string-prefix-p "http://" text) "http")))
         (authority (and scheme (subseq text (+ (length scheme) 3)))))
    (when (and authority (plusp (length authority))
               (not (find-if (lambda (c) (or (find c "/?#@") (member c '(#\Space #\Tab))))
                             authority)))
      (let* ((bracketed (eql (char authority 0) #\[))
             (close (and bracketed (position #\] authority)))
             (seam (if bracketed (and close (1+ close)) 0))
             (colon (and seam (<= seam (length authority))
                          (position #\: authority :start seam))))
        (when (or (not bracketed) close)
          (let* ((host (string-trim "[]" (subseq authority 0 (or colon (length authority)))))
                 (port (and colon (subseq authority (1+ colon) (length authority)))))
            (when (and (plusp (length host))
                       (or (null port)
                           (and (plusp (length port))
                                (every #'digit-char-p port)
                                (<= 1 (parse-integer port) 65535))))
              (values scheme host
                      (if port (parse-integer port)
                          (if (string= scheme "https") 443 80))))))))))

(defun base-url ()
  "The origin the section names, without a trailing slash. A refusal names the
member to set. A plain-http origin outside loopback is refused unless the
section says plain_http: there, the Authorization value would travel in clear."
  (let ((raw (text-value (setting :base-url))))
    (unless raw
      (fail "the gowa section has no base_url: set it in /settings to the origin the GOWA ~
             server answers on, such as http://127.0.0.1:3000"))
    (let ((raw (string-right-trim "/" raw)))
      (multiple-value-bind (scheme host port) (split-origin raw)
        (unless scheme
          (fail "base_url must be an http(s) origin and nothing more, and ~s is not" raw))
        (when (and (string= scheme "http")
                   (not (or (loopback-host-p host) (setting :plain-http))))
          (fail "base_url ~s is plain HTTP outside loopback: use https, or set plain_http true ~
                 to let the Authorization value travel in clear" raw))
        (format nil "~a://~a~@[:~a~]" scheme host
                (and (/= port (if (string= scheme "https") 443 80)) port))))))

(defun url-origin (url)
  "URL as (values SCHEME HOST PORT), or NIL when it is not an http(s) URL."
  (let* ((scheme (cond ((uiop:string-prefix-p "https://" url) "https")
                        ((uiop:string-prefix-p "http://" url) "http")))
         (rest (and scheme (subseq url (+ (length scheme) 3))))
         (end (and rest (or (position-if (lambda (c) (find c "/?#")) rest) (length rest)))))
    (and rest (split-origin (concatenate 'string scheme "://" (subseq rest 0 end))))))

(defun same-origin-p (left right)
  "Whether LEFT and RIGHT name one scheme, host and port."
  (multiple-value-bind (scheme host port) (url-origin left)
    (multiple-value-bind (other-scheme other-host other-port) (url-origin right)
      (and scheme other-scheme
           (string= scheme other-scheme) (string= host other-host) (= port other-port)))))

;;; --- the Authorization value ------------------------------------------------------

(defun environment-file-value (file name)
  "NAME's value in FILE -- KEY=VALUE lines, # a comment -- or NIL. The GOWA
setup writes its Authorization value into such a file, 0600."
  (let ((path (and (text-value file) (nlk:expand-home (text-value file)))))
    (when (and path (probe-file path))
      (with-open-file (in path :if-does-not-exist nil)
        (when in
          (loop for line = (read-line in nil)
                while line
                for trimmed = (string-left-trim '(#\Space #\Tab) line)
                for equals = (position #\= trimmed)
                when (and equals (plusp equals) (not (eql (char trimmed 0) #\#))
                          (string= (string-trim '(#\Space #\Tab) (subseq trimmed 0 equals)) name))
                  return (string-trim '(#\Space #\Tab #\" #\') (subseq trimmed (1+ equals)))))))))

(defun auth-value ()
  "The whole Authorization value the server wants: the variable auth_env names
(GOWA_AUTH_HEADER by default), else that same name in auth_file. NIL when
neither holds one -- a GOWA without auth takes every call."
  (let* ((name (or (text-value (setting :auth-env)) "GOWA_AUTH_HEADER"))
         (value (or (text-value (uiop:getenv name))
                    (text-value (environment-file-value (setting :auth-file) name)))))
    (when value
      (when (or (find #\Return value) (find #\Linefeed value))
        (fail "~a holds more than one line: it must be the whole Authorization value" name))
      (unless (or (uiop:string-prefix-p "Basic " value) (uiop:string-prefix-p "Bearer " value))
        (fail "~a must hold a whole Basic or Bearer Authorization value, as the server prints it"
              name))
      value)))

;;; --- the one exchange ------------------------------------------------------------

(defun ok-status-p (status)
  "Whether STATUS is a success: a transport that answered no status answered nothing."
  (and (integerp status) (< status 300)))

(defun answer-detail (text)
  "What a failed answer said, in GOWA's own words: its message or error, else the body."
  (let ((object (and (stringp text) (ignore-errors (nlk:decode-json text)))))
    (or (and (hash-table-p object)
             (or (nlk:json-value object :string "message")
                 (nlk:json-value object :string "error")))
        (nlk:one-line (if (stringp text) text "") :cap 300))))

(defun http-failure (method path status text)
  "Refuse a call GOWA answered with a status from 300 up, saying what it said."
  (fail "GOWA answered ~@[HTTP ~a ~]to ~a ~a~@[: ~a~]"
        status (string-downcase (string method)) path (answer-detail text)))

(defun request (method path &key body login)
  "METHOD PATH on the configured origin, BODY its JSON object. => (values
DECODED TEXT STATUS): DECODED the decoded answer or NIL, TEXT what arrived,
STATUS the number. A transport failure refuses; a status from 300 up is the
caller's to judge, because one of them means \"no such slot yet\"."
  (let* ((url (concatenate 'string (base-url) path))
         (timeout (if login *login-timeout* (setting :timeout-seconds)))
         (auth (auth-value))
         (headers (append (list (cons "accept" "application/json"))
                          (and auth (list (cons "authorization" auth)))
                          (and body (list (cons "content-type" "application/json")))))
         (text nil)
         (status nil))
    (handler-case
        (multiple-value-setq (text status)
          (funcall *http* method url :headers headers
                   :content (and body (nlk:encode-json-object body))
                   :timeout timeout :connect-timeout (min 10 timeout)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "GOWA did not answer ~a ~a: ~a" (string-downcase (string method)) path
              (nle:transport-failure-label condition url))))
    (let ((text (and (stringp text) text)))
      (when (and text (> (length text) *max-json*))
        (fail "GOWA answered ~a ~a with more than ~d bytes" method path *max-json*))
      (values (and text (plusp (length text)) (ignore-errors (nlk:decode-json text)))
              text (and (integerp status) status)))))

;;; --- what a record says ----------------------------------------------------------

(defun records (payload &optional (what "device"))
  "PAYLOAD's RESULTS object -- GOWA's envelope -- or a refusal naming WHAT."
  (let ((results (and (hash-table-p payload) (nlk:json-value payload :object "results"))))
    (or results (fail "GOWA's answer carried no ~a record" what))))

(defun field (object name)
  "OBJECT's string at NAME, or NIL."
  (nlk:json-value object :string name))

(defun json-list (object path)
  "OBJECT's array at PATH as a list: GOWA's answers carry their records and QR
links in one, and an absent or empty one is the empty list."
  (coerce (or (nlk:json-value object :array path) #()) 'list))

(defun device-line (device)
  "One slot as one line: its id, its state, and the account when it is linked."
  (let ((id (field device "id"))
        (state (field device "state")))
    (format nil "~a  ~a~@[  ~a~]~@[  ~a~]"
            (or id "?")
            (or state "unknown")
            (field device "phone_number")
            (field device "jid"))))

(defun phone-digits (value)
  "VALUE as the digits of an international number, or NIL: a JID's user part
when it is a WhatsApp one, a bare number, a leading + dropped."
  (let* ((text (and (stringp value) (nlk:trimmed value)))
         (text (if (and text (find #\@ text))
                   (let* ((at (position #\@ text :from-end t))
                          (server (subseq text (1+ at))))
                     (if (string= server "s.whatsapp.net")
                         (subseq text 0 (or (position #\: text) at))
                         nil))
                   text))
         (digits (and text (string-left-trim "+" text))))
    (and digits (<= 7 (length digits) 15) (every #'digit-char-p digits)
         (not (eql (char digits 0) #\0))
         digits)))

(defun device-id (value)
  "VALUE as a slot id: non-empty, unpadded, at most 256 characters, no control
characters -- the original's rule, which keeps one id from naming two slots."
  (let ((text (and (stringp value) value)))
    (unless (and text (plusp (length text)) (string= text (nlk:trimmed text)) (<= (length text) 256))
      (fail "device_id must be non-empty, unpadded, and at most 256 characters"))
    (when (find-if (lambda (c) (or (< (char-code c) 32) (= (char-code c) 127))) text)
      (fail "device_id cannot contain control characters"))
    text))

(defun phone-number (value)
  "VALUE as the digits of a phone number to pair with, or a refusal."
  (let* ((text (and (stringp value) (nlk:trimmed value)))
         (digits (and text (string-left-trim "+" text))))
    (unless (and text (string= text (nlk:trimmed text))
                 (or (string= text digits) (uiop:string-prefix-p "+" text))
                 digits (<= 7 (length digits) 15) (every #'digit-char-p digits)
                 (not (eql (char digits 0) #\0)))
      (fail "phone must be an international number with 7-15 digits and an optional leading +"))
    digits))

(defun missing-device-p (status text id)
  "Whether STATUS and TEXT say slot ID is not there: GOWA documents 404 and
answers 500 with \"device ID not found\", and the original read both."
  (or (eql status 404)
      (and (eql status 500) (search (format nil "device ~a not found" id) (or text "")))))

(defun ensure-unpaired (device id)
  "Refuse to pair slot ID again when DEVICE says it is linked or in the middle of it."
  (let ((state (or (field device "state") "")))
    (when (or (field device "jid") (field device "phone_number")
              (member state '("connecting" "connected" "logged_in") :test #'string=))
      (fail "device ~a is already linked or pairing (~a): remove it first, or pair another slot"
            id state))))

(defun duplicate-phone (digits exclude-id)
  "The id of another slot already linked to the number DIGITS, or NIL."
  (multiple-value-bind (payload text status) (request :get "/devices")
    (unless (ok-status-p status) (http-failure :get "/devices" status text))
    (loop for device in (json-list payload "results")
          for id = (field device "id")
          when (and (hash-table-p device) (not (equal id exclude-id))
                    (member digits (list (phone-digits (field device "phone_number"))
                                         (phone-digits (field device "jid")))
                            :test #'equal))
            return (or id "unknown"))))

(defun uri-quote (text)
  "TEXT with everything but an unreserved character percent-encoded, as the
original quoted a slot id into a path."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets text :external-format :utf-8)
          do (let ((c (code-char octet)))
               (if (or (and c (alphanumericp c)) (and c (find c "-._~")))
                   (write-char c out)
                   (format out "%~2,'0X" octet))))))

;;; --- the QR image --------------------------------------------------------------

(defun qr-directory ()
  "Where a pairing QR is written: state_dir, else <home>/gowa/."
  (let ((dir (text-value (setting :state-dir))))
    (if dir
        (uiop:ensure-directory-pathname (nlk:expand-home dir))
        (nlk:home "gowa/"))))

(defun file-slug (text)
  "TEXT as one file name's worth: letters, digits, - and _."
  (map 'string (lambda (c) (if (and (alphanumericp c) (< (char-code c) 128)) c #\-)) text))

(defun qr-path (id)
  "Where slot ID's pairing QR goes."
  (namestring (merge-pathnames (format nil "~a-qr.png" (file-slug id)) (qr-directory))))

(defun write-private (path octets)
  "OCTETS written to PATH, readable by its owner alone."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create :element-type '(unsigned-byte 8))
    (write-sequence octets out))
  (ignore-errors (sb-posix:chmod (namestring path) #o600))
  path)

(defun png-p (octets)
  "Whether OCTETS start with the PNG signature."
  (and (vectorp octets) (>= (length octets) 8)
       (equalp (subseq octets 0 8) #(137 80 78 71 13 10 26 10))))

(defun save-qr (link id)
  "LINK downloaded to ID's private PNG. The link must be on the server's own
origin, so the Authorization value cannot be walked to another host."
  (unless (same-origin-p link (base-url))
    (fail "GOWA answered a QR link on a different origin than base_url: ~s" link))
  (let ((auth (auth-value))
        (timeout (setting :timeout-seconds))
        (png nil)
        (status nil))
    (handler-case
        (multiple-value-setq (png status)
          (funcall *http* :get link :headers (and auth (list (cons "authorization" auth)))
                   :binary t :timeout timeout :connect-timeout (min 10 timeout)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "the pairing QR did not arrive from ~a: ~a" (base-url)
              (nle:transport-failure-label condition link))))
    (unless (ok-status-p status)
      (fail "GOWA answered HTTP ~a to the pairing QR" status))
    (when (and (vectorp png) (> (length png) *max-qr*))
      (fail "the pairing QR is bigger than ~d bytes" *max-qr*))
    (unless (png-p png)
      (fail "GOWA's pairing QR was not a PNG"))
    (write-private (qr-path id) png)))
