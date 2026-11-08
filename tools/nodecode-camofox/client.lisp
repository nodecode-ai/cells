;;;; client.lisp --- the Camofox REST seam: the address, the token, the tabs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Camofox-browser (https://github.com/jo-inc/camofox-browser) is a
;;;; self-hosted Node server wrapping Camoufox, a Firefox fork with C++
;;;; fingerprint spoofing. Its REST API maps onto browser tools one for one:
;;;; accessibility snapshots carrying element refs, click and type by ref, a
;;;; screenshot. This file is the only place that talks to it.
;;;;
;;;; Every exchange goes through *HTTP* (NLK:HTTP by default), so a test
;;;; binds that one name and no socket is opened. A refused status is a
;;;; VALUE here, not a condition: (CALL ...) answers the body and the status,
;;;; and the caller decides what the status means for its tool. Only a
;;;; transport failure (the server is not running) signals, with the address
;;;; and the hint.
;;;;
;;;; A tab belongs to a session: browser_navigate opens one, every other
;;;; tool works in the one the session already has. The identity behind it
;;;; follows the `camofox' section: a userId the operator names (an app that
;;;; owns the visible browser), else a profile-scoped one when
;;;; managed_persistence is on, else a random one per session, which is the
;;;; original plugin's rule.

(in-package #:nodecode-camofox)

;;; --- the seam -------------------------------------------------------------

(defparameter +server-help+
  "Camofox is a server of your own: `npm install && npm start' in a camofox-browser
checkout, or `docker run -d -p 9377:9377 -e CAMOFOX_PORT=9377 ghcr.io/jo-inc/camofox-browser'."
  "Where the server comes from, said in every message that needs it.")

(defvar *http* #'nlk:http
  "The cell's one network seam: NLK:HTTP's own argument list, answering
(values BODY STATUS HEADERS TRUNCATED-P). A test binds this name.")

(defun http (method url &key headers content timeout binary)
  "One exchange through *HTTP*, connect and read alike under TIMEOUT."
  (funcall *http* method url :headers headers :content content
                    :timeout timeout :connect-timeout (min 10 timeout) :binary binary))

(defun task-key ()
  "The session whose tabs these are: the session this turn is for."
  (or nlk:*scribe-session-id* "default"))

;;; --- the server -----------------------------------------------------------

(defun base-url ()
  "The configured server address, trailing slashes off; \"\" when unset."
  (or (setting :url) ""))

(defun require-server ()
  "The address, or a refusal naming the member that is missing."
  (let ((url (base-url)))
    (if (plusp (length url))
        url
        (fail "camofox.url is not set: put your camofox-browser server's address in the ~
camofox section of ~~/.nodecode/config.jsonc, e.g. {\"camofox\": {\"url\": \"http://localhost:9377\"}}. ~
~a" +server-help+))))

(defun auth-headers ()
  "The bearer token the server was started with, when one was."
  (let ((key (setting :api-key)))
    (if (and key (plusp (length key)))
        (list (cons "authorization" (format nil "Bearer ~a" key)))
        '())))

(defun endpoint (path &optional params)
  "PATH on the configured server, PARAMS an alist of query arguments."
  (format nil "~a~a~@[?~a~]" (require-server) path
          (and params (quri:url-encode-params params))))

(defun redact (text)
  "TEXT with the configured bearer token taken out of it."
  ;; Read off the settings plist, not through SETTING: FAIL runs this while
  ;; the cell may not be started at all.
  (let ((key (getf *camofox* :api-key)))
    (if (and (stringp text) (stringp key) (plusp (length key)))
        (uiop:frob-substrings text (list key) "[redacted]")
        text)))

(defun explained (method path status body)
  "The server's own words for a refused exchange: HTTP status and its detail."
  (let* ((object (and (stringp body) (plusp (length body)) (ignore-errors (nlk:decode-json body))))
         (detail (and (hash-table-p object)
                      (format nil "~{~a~^; ~}"
                              (loop for key in '("error" "code" "recovery")
                                    for text = (nlk:json-value object :string key)
                                    when (and text (plusp (length text)))
                                      collect (format nil "~a: ~a" key text))))))
    (format nil "Camofox answered HTTP ~d to ~(~a~) ~a~@[: ~a~]"
            status method path (and (plusp (length detail)) detail))))

(defun call (method path &key params object timeout binary)
  "One exchange with the server => (values BODY STATUS).
A transport failure signals CAMOFOX-ERROR naming the address and the hint."
  (let ((timeout (or timeout (setting :timeout-seconds))))
    (handler-case
        (multiple-value-bind (body status) (http method (endpoint path params)
                                                 :headers (append '(("accept" . "application/json"))
                                                                  (auth-headers))
                                                 :content (and object (nlk:encode-json-object object))
                                                 :timeout timeout :binary binary)
          (values body status))
      (camofox-error (condition) (error condition))
      (nlk:turn-cancelled-condition (condition) (error condition))
      (error (condition)
        (fail "cannot reach the Camofox server at ~a (~a). Is it running? ~a"
              (base-url) condition +server-help+)))))

(defun answer-object (body)
  "BODY decoded, when it is a JSON object; NIL for an empty or other answer."
  (let ((object (and (stringp body) (plusp (length (string-trim '(#\Space #\Return #\Newline) body)))
                      (ignore-errors (nlk:decode-json body)))))
    (and (hash-table-p object) object)))

(defun call-object (method path &key params object timeout)
  "The JSON object the server answers, or a refusal naming the status."
  (multiple-value-bind (body status) (call method path :params params :object object :timeout timeout)
    (if (>= status 300)
        (fail "~a" (explained method path status body))
        (answer-object body))))

(defun json-member (object key &optional default)
  "KEY's member of OBJECT — a JSON object — or DEFAULT when the member is
absent, JSON null, or OBJECT is no object at all. JSON false answers NIL,
which a caller reading a boolean tells from the default by passing none."
  (multiple-value-bind (value present) (and (hash-table-p object) (gethash key object))
    (if (and present (not (eq value :null))) value default)))

(defun text-member (object key &optional (default ""))
  "KEY's member of OBJECT as text, DEFAULT when it is absent or not a string."
  (let ((value (json-member object key)))
    (if (stringp value) (redact value) default)))

;;; --- identities and sessions ----------------------------------------------

(defstruct (tab-session (:constructor make-tab-session ()))
  "One browser identity and the tab this session drives in it."
  user-id session-key tab-id
  (managed nil)
  (adopt nil))

(defvar *sessions* (make-hash-table :test #'equal)
  "Task key -> TAB-SESSION: the tab each session's tools work in.")

(defvar *session-lock* (bt2:make-lock :name "camofox sessions")
  "Guards *SESSIONS*: two sessions' turns run at once.")

(defmacro with-session-lock (&body body)
  `(bt2:with-lock-held (*session-lock*) ,@body))

(defun hex-of (text &optional (count 10))
  "COUNT hex characters: the front of TEXT's SHA-256."
  (subseq (nlk:sha256-text text) 7 (+ 7 count)))

(defun random-hex (count)
  "COUNT hex characters from the operating system's entropy source."
  (let ((text (format nil "~{~2,'0x~}" (coerce (nlk:random-bytes (ceiling count 2)) 'list))))
    (subseq text 0 (min count (length text)))))

(defun state-dir ()
  "The profile-scoped folder a persistent Camofox identity is derived from."
  (namestring (uiop:ensure-directory-pathname (nlk:home "browser_auth/camofox"))))

(defun managed-identity (task)
  "The profile-scoped user and the task's session key: a userId the same
across restarts, so the server keeps one browser profile for this home."
  (let ((root (state-dir)))
    (list :user-id (format nil "hermes_~a" (hex-of (format nil "camofox-user:~a" root) 10))
          :session-key (format nil "task_~a"
                               (hex-of (format nil "camofox-session:~a:~a" root task) 16)))))

(defun external-identity (task)
  "The identity an app that owns the visible browser names, or NIL."
  (let ((user (setting :user-id)))
    (when (and user (plusp (length user)))
      (let ((key (setting :session-key)))
        (list :user-id user
              :session-key (if (and key (plusp (length key)))
                               key
                               (format nil "task_~a" (subseq task 0 (min 16 (length task))))))))))

(defun fresh-identity (task)
  "A random Camofox user for this session: the default, cookies do not
survive the session."
  (list :user-id (format nil "hermes_~a" (random-hex 10))
        :session-key (format nil "task_~a" (subseq task 0 (min 16 (length task))))))

(defun make-session (task)
  "The identity and the tab state a task starts with."
  (let* ((external (external-identity task))
         (identity (or external (and (setting :managed-persistence) (managed-identity task))
                       (fresh-identity task)))
         (session (make-tab-session)))
    (setf (tab-session-user-id session) (getf identity :user-id)
          (tab-session-session-key session) (getf identity :session-key)
          (tab-session-managed session) (and (or external (setting :managed-persistence)) t)
          (tab-session-adopt session) (and external (setting :adopt-existing-tab)))
    session))

(defun user-params (session)
  "The query arguments a route scoped to a user takes."
  (list (cons "userId" (tab-session-user-id session))))

(defun tab-path (session suffix)
  "The route of SESSION's tab, SUFFIX naming the action."
  (format nil "/tabs/~a~@[/~a~]" (tab-session-tab-id session) suffix))

(defun listed-tabs (session)
  "The tabs the server holds for SESSION's user, newest last."
  (let* ((answer (ignore-errors (call-object :get "/tabs" :params (user-params session) :timeout 5)))
         (tabs (json-member answer "tabs")))
    (if (vectorp tabs)
        (remove-if-not #'hash-table-p (coerce tabs 'list))
        '())))

(defun adopt-tab (session)
  "Take an already open tab carrying this session's key, when one is there."
  (when (and (null (tab-session-tab-id session))
             (tab-session-adopt session)
             (plusp (length (base-url))))
    (let ((mine (remove-if-not (lambda (tab)
                                 (equal (json-member tab "listItemId")
                                        (tab-session-session-key session)))
                               (listed-tabs session))))
      (let ((tab (json-member (car (last mine)) "tabId")))
        (when (and (stringp tab) (plusp (length tab)))
          (setf (tab-session-tab-id session) tab)))))
  session)

(defun session-for ()
  "The task's session: taken from the table, else made and adopted into."
  (let ((task (task-key)))
    (or (with-session-lock (gethash task *sessions*))
        (let ((session (adopt-tab (make-session task))))
          (with-session-lock (setf (gethash task *sessions*) session))
          session))))

