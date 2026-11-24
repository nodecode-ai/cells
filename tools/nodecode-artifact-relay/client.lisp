;;;; client.lisp --- the service origin, the token, and the two calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through *HTTP* -- NLK:HTTP -- so a test
;;;; stubs that one name and never opens a socket. The original moved the same
;;;; two calls through urllib: a multipart publish, and a read of one
;;;; artifact's source back. Three of its rules are kept here: the service is
;;;; an https origin (plain http on loopback only), the viewer URL a publish
;;;; answers with must be on that same origin under /a/, and an id is a
;;;; literal of the relay's alphabet -- never a path.

(in-package #:nodecode-artifact-relay)

(defparameter *http* #'nlk:http
  "The cell's one network call. A test binds this, so nothing dials a relay.")

;;; --- readers ---------------------------------------------------------------------

(defun secret-text (value)
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

;;; --- an origin, and what may be one --------------------------------------------

(defstruct (origin (:constructor %make-origin (scheme host port text)) (:copier nil))
  "A service as this cell names it: its scheme, host and port, and the text
the two calls prefix."
  (scheme "https" :read-only t)
  (host "" :read-only t)
  (port nil :read-only t)
  (text "" :read-only t))

(defun loopback-host-p (host)
  "Whether HOST is this machine, where plain http is tolerated."
  (let ((host (string-downcase (or host ""))))
    (or (string= host "localhost")
        (string= host "::1")
        (string= host "[::1]")
        (uiop:string-prefix-p "127." host))))

(defun effective-port (scheme port)
  (or port (if (string= (or scheme "") "https") 443 80)))

(defun blankp (text)
  (or (null text) (string= text "")))

(defun origin-of (value &key (label "the service URL"))
  "VALUE -- an absolute http(s) origin -- as an ORIGIN: https, or http on
loopback, with no path, query, fragment or userinfo. Anything else refuses."
  (let* ((text (secret-text value))
         (uri (and text (ignore-errors (quri:uri text))))
         (scheme (and uri (string-downcase (or (quri:uri-scheme uri) ""))))
         (host (and uri (string-downcase (or (quri:uri-host uri) ""))))
         (path (and uri (quri:uri-path uri))))
    (unless (and uri
                 (member scheme '("http" "https") :test #'string=)
                 host (plusp (length host))
                 (null (quri:uri-userinfo uri))
                 (or (blankp path) (string= path "/"))
                 (blankp (quri:uri-query uri))
                 (blankp (quri:uri-fragment uri)))
      (fail "~a must be an absolute http(s) origin, e.g. https://relay.example: ~s"
            label (or text "none")))
    (unless (or (string= scheme "https") (loopback-host-p host))
      (fail "~a must use https (plain http is accepted only on loopback): ~s" label text))
    (let ((port (quri:uri-port uri)))
      (if (or (null port) (= port (effective-port scheme nil)))
          (%make-origin scheme host nil (format nil "~a://~a" scheme host))
          (%make-origin scheme host port (format nil "~a://~a:~d" scheme host port))))))

(defun same-origin-p (origin uri)
  "Whether URI is ORIGIN's own: the same scheme, host and port."
  (and uri
       (string= (string-downcase (or (quri:uri-scheme uri) "")) (origin-scheme origin))
       (string= (string-downcase (or (quri:uri-host uri) "")) (origin-host origin))
       (= (effective-port (quri:uri-scheme uri) (quri:uri-port uri))
          (effective-port (origin-scheme origin) (origin-port origin)))))

(defun artifact-id-p (text)
  "Whether TEXT is one artifact id the relay could have minted: base64url's
alphabet, and a length of its own."
  (and (stringp text)
       (<= +min-id-length+ (length text) +max-id-length+)
       (every (lambda (character)
                (or (alphanumericp character)
                    (member character '(#\- #\_))))
              text)))

(defun artifact-id-of (value &key origin)
  "VALUE -- an artifact id, or a viewer URL -- as the id. A URL that is not
ORIGIN's own /a/<id> refuses before anything is sent, so a relay cannot hand
back a link somewhere else and have the token follow it."
  (let ((text (secret-text value)))
    (unless text (fail "an artifact id or URL is required"))
    (if (artifact-id-p text)
        text
        (let ((uri (ignore-errors (quri:uri text))))
          (unless uri (fail "not an artifact id or a URL: ~s" text))
          (unless (same-origin-p origin uri)
            (fail "the artifact URL must be on the configured service (~a): ~s"
                  (origin-text origin) text))
          (let* ((path (or (quri:uri-path uri) ""))
                 (id (and (uiop:string-prefix-p "/a/" path) (subseq path 3))))
            (when (and id (uiop:string-suffix-p "/" id))
              (setf id (subseq id 0 (1- (length id)))))
            (unless (and id (artifact-id-p id))
              (fail "an artifact URL looks like ~a/a/<id>: ~s" (origin-text origin) text))
            id)))))

;;; --- the token, and where it is kept --------------------------------------------

(defun state-directory ()
  "The folder holding the token and the saved service: the section's
state_dir, else <home>/artifact-relay/."
  (let ((named (secret-text (setting :state-dir))))
    (if named
        (uiop:ensure-directory-pathname (nlk:expand-home named))
        (nlk:home "artifact-relay/"))))

(defun token-path ()
  "The file a token is read from, and the one /artifact-relay setup writes."
  (let ((named (secret-text (setting :token-file))))
    (if named (nlk:expand-home named) (merge-pathnames "token" (state-directory)))))

(defun file-text (path)
  "PATH's whole text, trimmed, or NIL when it cannot be read."
  (let ((text (ignore-errors (uiop:read-file-string path))))
    (and text (nlk:trimmed text))))

(defun token ()
  "The bearer token: the token file first, then the variable token_env names.
A refusal says where one goes; the token itself never reaches a log."
  (or (secret-text (file-text (token-path)))
      (let ((variable (secret-text (setting :token-env))))
        (and variable (secret-text (uiop:getenv variable))))
      (fail "no Artifact Relay token: run /artifact-relay setup, or set the environment variable ~a to a token, or point token_file at a file holding one"
            (or (secret-text (setting :token-env)) +token-variable+))))

(defun keep-token (text)
  "Write TEXT to the token file, private from the first byte."
  (let ((path (token-path)))
    (nlk:write-file-atomically path (format nil "~a~%" text) :mode :private :directory-mode #o700)
    (namestring path)))

;;; --- the service a publish goes to ----------------------------------------------

(defun setup-path ()
  (merge-pathnames "setup.json" (state-directory)))

(defun saved-service ()
  "The service a finished sign-in saved, as text, or NIL."
  (let ((text (ignore-errors (uiop:read-file-string (setup-path)))))
    (let ((object (and text (decode-answer text))))
      (and (hash-table-p object) (json-text object "base_url")))))

(defun save-service (url)
  "Save URL -- already an origin's text -- where a cell without base_url
looks for it, beside the token."
  (nlk:write-file-atomically (setup-path)
                              (nlk:encode-json-object (nlk:make-json-object "base_url" url))
                              :mode :private :directory-mode #o700)
  url)

(defun base-url-text ()
  "The service this cell publishes to: the section's base_url, else the one
a sign-in saved."
  (or (secret-text (setting :base-url))
      (secret-text (saved-service))
      (fail "Artifact Relay is not configured: set base_url in the artifact-relay section, or run /artifact-relay setup to sign in and learn your service URL")))

(defun base-origin ()
  (origin-of (base-url-text)))

;;; --- one exchange ---------------------------------------------------------------

(defun ok-p (status)
  (and (integerp status) (<= 200 status 299)))

(defun relay-words (json)
  "The relay's own words for why it refused, if it sent any."
  (and (hash-table-p json)
       (or (json-text json "error_description") (json-text json "error")
           (json-text json "detail") (json-text json "message"))))

(defun refusal-line (json status)
  (format nil "Artifact Relay answered HTTP ~a~@[: ~a~]"
          status
          (let ((words (ignore-errors (relay-words json))))
            (and (secret-text words) (nlk:one-line words :cap 200)))))

(defun call (method url &key headers content (auth t))
  "METHOD URL on the wire: (values DECODED STATUS). Every status is a value;
a transport failure refuses in the cell's own condition."
  (let ((limit (setting :timeout-seconds))
        ;; resolved here, outside the transport handler below, so a refusal to
        ;; name a token is the refusal the operator reads and not a dial failure
        (headers (append (when auth
                           (list (cons "authorization" (format nil "Bearer ~a" (token)))))
                         (list (cons "accept" "application/json"))
                         headers))
        (text nil) (status nil))
    (handler-case
        (multiple-value-setq (text status)
          (funcall *http* method url
                   :headers headers
                   :content content
                   :timeout limit :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "the Artifact Relay did not answer: ~a" (nle:transport-failure-label condition url))))
    (values (decode-answer (body-text text)) status)))

(defun answer-json (json status)
  "JSON, or the refusal STATUS names."
  (unless (ok-p status) (fail "~a" (refusal-line json status)))
  (unless (hash-table-p json)
    (fail "Artifact Relay answered ~a with something that is not a JSON object" status))
  json)

;;; --- one multipart publish ------------------------------------------------------

(defun random-boundary ()
  (format nil "----nodecode-artifact-~(~{~2,'0x~}~)"
          (coerce (nlk:random-bytes 18) 'list)))

(defun multipart-body (fields content filename content-type)
  "FIELDS -- (NAME . VALUE) ... -- and CONTENT as one multipart/form-data
body: (values OCTETS BOUNDARY)."
  (let ((boundary (random-boundary))
        (break (format nil "~c~c" #\Return #\Newline)))
    (values
     (sb-ext:string-to-octets
      (with-output-to-string (out)
        (dolist (field fields)
          (format out "--~a~aContent-Disposition: form-data; name=\"~a\"~a~a~a~a"
                  boundary break (car field) break break (cdr field) break))
        (format out "--~a~aContent-Disposition: form-data; name=\"content\"; filename=\"~a\"~aContent-Type: ~a~a~a~a~a"
                boundary break filename break content-type break break content break)
        (format out "--~a--~a" boundary break))
      :external-format :utf-8)
     boundary)))

(defun text-field (value name limit)
  "VALUE as trimmed text of at most LIMIT characters, or NIL when absent."
  (let ((text (secret-text value)))
    (when text
      (when (> (length text) limit)
        (fail "~a is longer than ~d characters" name limit))
      text)))

(defun required-field (value name limit)
  (or (text-field value name limit) (fail "~a is required" name)))

(defun format-field (value)
  (let ((format (string-downcase (or (secret-text value) "markdown"))))
    (unless (member format +formats+ :test #'string=)
      (fail "format must be markdown or html, not ~s" (or value "none")))
    format))

(defun expiry-field (value)
  (let ((days (cond ((null value) 30)
                    ((integerp value) value)
                    ((and (stringp value) (secret-text value))
                     (or (ignore-errors
                          (parse-integer (secret-text value) :junk-allowed nil))
                         (fail "expires_days must be a whole number of days, not ~s" value)))
                    (t (fail "expires_days must be a whole number of days, not ~s" value)))))
    (unless (<= 0 days +max-expires-days+)
      (fail "expires_days must be between 0 and ~d, not ~d" +max-expires-days+ days))
    days))

(defun provenance-fields ()
  "The session a publish came from, when the section asks for it. The
original also sent the platform and a chat name; a Nodecode session has an
id and no chat."
  (when (setting :include-provenance)
    (let* ((turn (ignore-errors (nle:turn)))
           (session (secret-text (getf turn :session-id))))
      (when session
        (list (cons "session_id" session)
              (cons "platform" "nodecode"))))))

(defun publish (&key title content summary format expires-days)
  "Publish CONTENT to the configured relay: (values JSON ID VIEWER-URL)."
  (let* ((origin (base-origin))
         (title (required-field title "title" +max-title+))
         (content (required-field content "content" +max-content-bytes+))
         (format (format-field format))
         (summary (text-field summary "summary" +max-summary+))
         (days (expiry-field expires-days))
         (bytes (length (sb-ext:string-to-octets content :external-format :utf-8))))
    (when (> bytes +max-content-bytes+)
      (fail "content is ~:d bytes; the relay takes at most ~:d" bytes +max-content-bytes+))
    (let ((fields (append (list (cons "title" title)
                                (cons "summary" (or summary ""))
                                (cons "format" format)
                                (cons "expires_in_days" (princ-to-string days)))
                          (provenance-fields))))
      (multiple-value-bind (body boundary)
          (multipart-body fields content
                          (if (string= format "html") "artifact.html" "artifact.md")
                          (if (string= format "html")
                              "text/html; charset=utf-8"
                              "text/markdown; charset=utf-8"))
        (multiple-value-bind (json status)
            (call :post (format nil "~a/api/artifacts" (origin-text origin))
                  :content body
                  :headers (list (cons "content-type"
                                       (format nil "multipart/form-data; boundary=~a" boundary))
                                 (cons "user-agent" "nodecode-artifact-relay/0.1")))
          (let* ((json (answer-json json status))
                 (url (secret-text (json-text json "url"))))
            (unless url (fail "the Artifact Relay's publish answer has no viewer URL"))
            (values json (artifact-id-of url :origin origin) url)))))))

(defun read-artifact (url-or-id)
  "One artifact's source, as the relay answers it: (values JSON ID)."
  (let* ((origin (base-origin))
         (id (artifact-id-of url-or-id :origin origin)))
    (multiple-value-bind (json status)
        (call :get (format nil "~a/api/artifacts/~a" (origin-text origin) id)
              :headers (list (cons "user-agent" "nodecode-artifact-relay/0.1")))
      (let ((json (answer-json json status)))
        (unless (secret-text (json-text json "content"))
          (fail "the Artifact Relay's answer has no source content"))
        (values json id)))))
