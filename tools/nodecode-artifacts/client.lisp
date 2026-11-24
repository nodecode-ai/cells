;;;; client.lisp --- the service origin, the credential, and one call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through *HTTP* -- NLK:HTTP -- so a test
;;;; stubs that one name and never opens a socket. The original moved the same
;;;; calls through urllib against /internal/v1 of a self-hosted artifact
;;;; service: publish, list, get, download, share, revoke. Its three rules are
;;;; kept here: the credential is a bearer header and nothing else, an id is a
;;;; path segment and never a path, and every refusal reads the service's own
;;;; error object.

(in-package #:nodecode-artifacts)

(defparameter *http* #'nlk:http
  "The cell's one network call. A test binds this, so nothing dials a service.")

;;; --- readers ---------------------------------------------------------------------

(defun present-text (value)
  "VALUE as trimmed text when it is a non-empty string, else NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun json-text (object &rest path)
  "OBJECT's string at PATH, or NIL."
  (apply #'nlk:json-value object :string path))

(defun body-text (body)
  "An HTTP BODY -- text, or octets -- as a string."
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun decode-answer (text)
  "TEXT as decoded JSON, or NIL when it is not one."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (when (and text (plusp (length text)))
      (handler-case (nlk:decode-json text) (error () nil)))))

(defun utf8 (text)
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun octets-of (body)
  "BODY as octets, whichever shape the wire handed back."
  (typecase body
    ((vector (unsigned-byte 8)) body)
    (string (utf8 body))
    (null (make-array 0 :element-type '(unsigned-byte 8)))
    (t (utf8 (princ-to-string body)))))

;;; --- the origin ----------------------------------------------------------------

(defstruct (origin (:constructor %make-origin (text)) (:copier nil))
  "The artifact service as this cell names it: the text every call prefixes."
  (text "" :read-only t))

(defun loopback-host-p (host)
  "Whether HOST is this machine, where plain http is tolerated."
  (let ((host (string-downcase (or host ""))))
    (or (string= host "localhost")
        (string= host "::1")
        (string= host "[::1]")
        (uiop:string-prefix-p "127." host))))

(defun base-origin ()
  "The artifact service's origin, from the section's base_url: an absolute
http(s) origin, https unless it is this machine."
  (let ((text (present-text (setting :base-url))))
    (unless text
      (fail "Artifacts is not configured: set base_url in the artifacts section to the artifact service's own URL, e.g. http://127.0.0.1:8080"))
    (let* ((uri (ignore-errors (quri:uri text)))
           (scheme (and uri (string-downcase (or (quri:uri-scheme uri) ""))))
           (host (and uri (string-downcase (or (quri:uri-host uri) ""))))
           (path (and uri (quri:uri-path uri))))
      (unless (and uri
                   (member scheme '("http" "https") :test #'string=)
                   host (plusp (length host))
                   (null (quri:uri-userinfo uri))
                   (or (null path) (string= path "") (string= path "/"))
                   (let ((query (quri:uri-query uri)))
                     (or (null query) (string= query ""))))
        (fail "base_url must be an absolute http(s) origin with no path, e.g. http://127.0.0.1:8080: ~s" text))
      (unless (or (string= scheme "https") (loopback-host-p host))
        (fail "base_url must use https, or plain http on this machine: ~s" text))
      (let ((port (quri:uri-port uri))
            (default (if (string= scheme "https") 443 80)))
        (%make-origin (if (or (null port) (= port default))
                          (format nil "~a://~a" scheme host)
                          (format nil "~a://~a:~d" scheme host port)))))))

;;; --- the credential ------------------------------------------------------------

(defun token-file-path ()
  "The file the credential is read from, when the section names one."
  (let ((named (present-text (setting :token-file))))
    (and named (nlk:expand-home named))))

(defun token ()
  "The machine credential: the token file first, then the variable token_env
names. The token itself never reaches a message."
  (or (let ((path (token-file-path)))
        (and path (present-text (ignore-errors (uiop:read-file-string path)))))
      (let ((variable (present-text (setting :token-env))))
        (and variable (present-text (uiop:getenv variable))))
      (fail "no artifact service credential: set token_env to the variable holding it (~a), or token_file to a file holding it"
            (or (present-text (setting :token-env)) +default-token-variable+))))

;;; --- a URL ---------------------------------------------------------------------

(defun url-encode (text)
  "TEXT as one query or path value: everything but unreserved ASCII escaped."
  (with-output-to-string (out)
    (loop for octet across (utf8 (princ-to-string text))
          do (if (or (<= 65 octet 90) (<= 97 octet 122) (<= 48 octet 57)
                     (member octet '(45 46 95 126)))
                 (write-char (code-char octet) out)
                 (format out "%~2,'0X" octet)))))

(defun query-string (params)
  "PARAMS -- (NAME . VALUE) ... -- as ?A=B&..., or NIL when none is set."
  (let ((parts (loop for (name . value) in params
                     when (and value (not (and (stringp value) (zerop (length value)))))
                       collect (format nil "~a=~a" (url-encode name) (url-encode value)))))
    (and parts (format nil "?~{~a~^&~}" parts))))

(defun segment (value)
  "VALUE as one path segment: never a path of its own."
  (url-encode (or (present-text (and (stringp value) value)) "")))

;;; --- one exchange -------------------------------------------------------------

(defun refusal-line (json status)
  "The service's own words for a refused status, when it sent any."
  (format nil "the artifact service answered HTTP ~a~@[: ~a~]"
          status
          (let ((words (json-text json "error" "message")))
            (and (present-text words) (nlk:one-line words :cap 200)))))

(defun answer-json (body status)
  "BODY and STATUS as the decoded object, or the refusal they name."
  (let ((json (decode-answer (body-text body))))
    (unless (and (integerp status) (<= 200 status 299))
      (fail "~a" (refusal-line json (or status "none"))))
    (unless (hash-table-p json)
      (fail "the artifact service answered ~a with something that is not a JSON object" status))
    json))

(defun call (method path &key params json content content-type (binary nil))
  "One exchange with the service: (values BODY STATUS). CONTENT is octets
for a publish; JSON, when given, is the body and its content type."
  (let* ((origin (base-origin))
         (url (format nil "~a/internal/v1~a~@[~a~]"
                       (origin-text origin) path (query-string params)))
         (body (cond (json (utf8 (nlk:encode-json-object json)))
                     (content content)))
         (headers (list (cons "authorization" (format nil "Bearer ~a" (token)))
                        (cons "accept" "application/json")))
         (timeout (setting :timeout-seconds)))
    (when json
      (push (cons "content-type" "application/json") headers))
    (when (and content (null json))
      (push (cons "content-type" (or content-type "application/octet-stream")) headers))
    (handler-case
        (funcall *http* method url :headers headers :content body
                          :timeout timeout :connect-timeout (min 10 timeout) :binary binary)
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "the artifact service did not answer: ~a" (nle:transport-failure-label condition url))))))

(defun request (method path &key params json content content-type)
  "One exchange whose answer must be a JSON object."
  (multiple-value-bind (body status)
      (call method path :params params :json json :content content :content-type content-type)
    (answer-json body status)))

(defun download-bytes (path)
  "PATH's original bytes, or the refusal the service answered with."
  (multiple-value-bind (body status) (call :get path :binary t)
    (unless (and (integerp status) (<= 200 status 299))
      (fail "~a" (refusal-line (decode-answer (body-text body)) (or status "none"))))
    (octets-of body)))

(defun file-bytes (path)
  "PATH's whole contents as octets, read in one go."
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))
