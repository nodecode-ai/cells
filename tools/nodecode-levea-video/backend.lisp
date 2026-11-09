;;;; backend.lisp --- the Levea call path: one bearer call, and the outcome read
;;;; from the answer's own success flag rather than from HTTP 200.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The editor is a cloud backend: it plans, executes, verifies and exports,
;;;; and answers JSON. HTTP 200 is not success -- a run can come back
;;;; `partial', `pending' or `failed' behind it -- so the two predicates are
;;;; read separately, which is what the plugin this was rewritten from did
;;;; (normalize_backend_response). Every call carries the operator's key and
;;;; nothing else leaves the machine except the prompt, the source URLs and
;;;; the ids the backend handed back.

(in-package #:nodecode-levea-video)

;;; --- what a call runs on ----------------------------------------------------------

(defun api-key ()
  "The key a call is billed to: the section's, else the variable api_key_env
names. A refusal here costs nothing, so it is raised before any request."
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (when (zerop (length key))
      (setf key (nlk:trimmed (or (funcall *getenv* (setting :api-key-env)) ""))))
    (when (zerop (length key))
      (fail "no Levea key: paste one into the levea-video section's api_key, or set the ~
             variable ~a (generate a key at ~a). Nothing was sent and nothing was billed."
            (setting :api-key-env) +key-portal+))
    (unless (every (lambda (char) (<= 33 (char-code char) 126)) key)
      (fail "the Levea key holds characters an API key cannot have; enter it again. ~
             Nothing was sent and nothing was billed."))
    key))

(defun checked-api-url (text)
  "TEXT as the editor's host, without its trailing slash, or a CONFIG-REFUSAL.
Plain http is allowed only for a loopback host: anywhere else it would send
the key in clear."
  (let ((url (string-right-trim "/" (nlk:trimmed (or text "")))))
    (unless (plusp (length url))
      (nlk:config-error "levea-video.api_url is empty; it is ~a unless another host is wanted"
                        +default-api-url+))
    (let* ((secure (uiop:string-prefix-p "https://" url))
           (plain (uiop:string-prefix-p "http://" url))
           (rest (and (or secure plain) (subseq url (if secure 8 7))))
           (host (and rest (subseq rest 0 (or (position-if (lambda (char) (find char "/:?#")) rest)
                                               (length rest))))))
      (unless (and rest (plusp (length host)))
        (nlk:config-error "levea-video.api_url is ~s; it must be an https URL, such as ~a"
                          url +default-api-url+))
      (when (and plain (not (member (string-downcase host)
                                    '("localhost" "127.0.0.1" "::1" "[::1]") :test #'string=)))
        (nlk:config-error "levea-video.api_url is ~s; plain http would send the key in clear, ~
                           so it is allowed only for a localhost stand-in" url))
      url)))

(defparameter +marketing-hosts+
  '("https://livecore.ai" "https://www.livecore.ai" "http://livecore.ai" "http://www.livecore.ai")
  "The product's own web host, which callers paste when they mean the API: it
becomes the API host, keeping any path after it.")

(defun api-root ()
  "The root every call is made under: the configured host with the editor's
API path on it, however much of that path was already written in."
  (let* ((raw (checked-api-url (or (setting :api-url) +default-api-url+)))
         (host (find-if (lambda (candidate)
                          (or (string-equal raw candidate)
                              (uiop:string-prefix-p (concatenate 'string candidate "/") raw)))
                        +marketing-hosts+)))
    (when host
      (setf raw (concatenate 'string +default-api-url+ (subseq raw (length host)))))
    (cond ((uiop:string-suffix-p (concatenate 'string +api-suffix+ "/execute") raw)
           (subseq raw 0 (- (length raw) (length "/execute"))))
          ((uiop:string-suffix-p +api-suffix+ raw) raw)
          ((uiop:string-suffix-p "/api/v1/misc/openclaw" raw) (concatenate 'string raw "/v1"))
          ((uiop:string-suffix-p "/api/v1/misc" raw) (concatenate 'string raw "/openclaw/v1"))
          (t (concatenate 'string raw +api-suffix+)))))

(defun url-segment (text)
  "TEXT as one URL path segment: everything outside the unreserved set is
percent-encoded, so an id with a slash in it cannot walk the path."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets (or text "") :external-format :utf-8)
          for char = (code-char octet)
          do (if (or (alphanumericp char) (find char "-._~"))
                 (write-char char out)
                 (format out "%~2,'0X" octet)))))

;;; --- one call --------------------------------------------------------------------

(defun decode-answer (answer)
  "ANSWER, an HTTP body, as a decoded JSON value; a body that is not JSON is
kept as the text it was, so the model can read what the host said."
  (cond ((stringp answer)
         (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
           (cond ((zerop (length text)) (nlk:json-object))
                 (t (handler-case (nlk:decode-json text)
                      (error () (subseq text 0 (min 2000 (length text)))))))))
        ((hash-table-p answer) answer)
        ((vectorp answer) answer)
        (t (nlk:json-object))))

(defun http-call (method path body)
  "METHOD PATH with BODY (a JSON object, or NIL) => (values PAYLOAD STATUS).
A refusal below HTTP is answered as a payload of this cell's own, never raised:
the model reads what happened and can poll by the id it already has."
  (let ((url (concatenate 'string (api-root) path))
        (key (api-key))
        (limit (setting :timeout-seconds)))
    (handler-case
        (multiple-value-bind (answer status)
            (nlk:http method url
                      :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                                     (cons "content-type" "application/json")
                                     (cons "accept" "application/json")
                                     (cons "x-levea-client" +client-header+))
                      :content (and body (nlk:encode-json-object body))
                      :timeout limit
                      :connect-timeout (min 20 limit))
          (values (decode-answer answer) status))
      (error (condition)
        (values (nlk:json-object "code" "BACKEND_UNREACHABLE"
                                  "message" (format nil "cannot reach the Levea backend: ~a"
                                                     (nle:transport-failure-label condition url)))
                nil)))))

;;; --- what came back --------------------------------------------------------------

(defun result-object (payload)
  "PAYLOAD's members as a result of this cell's own (never the backend's object
mutated), or an object holding the body it could not read."
  (let ((object (if (hash-table-p payload) (nlk:copy-json-object payload) (nlk:json-object))))
    (when (and payload (not (hash-table-p payload)))
      (setf (gethash "rawBody" object) payload))
    object))

(defun normalize (payload status &key label require-message require-verification
                                   fallback-project accept-active)
  "PAYLOAD, the backend's answer at HTTP STATUS, as the result a tool answers
with: success is what the answer says it is, and every way the two can
contradict each other is named rather than smoothed over."
  (let* ((object (and (hash-table-p payload) payload))
         (out (result-object payload))
         (project (or (nlk:json-value object :string "projectId")
                      (nlk:json-value object :string "project_id")
                      fallback-project))
         (declared (and object (gethash "success" object)))
         (active-key (and accept-active object (nth-value 1 (gethash "active" object))))
         (active (and active-key (gethash "active" object)))
         (state (and object (nlk:json-value object :string "status")))
         (message (or (nlk:json-value object :string "message")
                      (nlk:json-value object :string "reply")
                      (nlk:json-value object :string "error"))))
    (when project
      (setf (gethash "projectId" out) project
            (gethash "project_id" out) project))
    (labels ((say (text &optional (state "error"))
               (setf (gethash "success" out) nil
                     (gethash "status" out) state
                     (gethash "message" out) text)
               out))
      (cond
        ((null object)
         (say (format nil "~a returned a malformed non-object answer~@[ (HTTP ~a)~]."
                      label status)))
        ((null status)
         (say (or message (format nil "no answer from the Levea backend for ~a." label))))
        ((or (< status 200) (>= status 300))
         (say (cond ((and (member status '(401 403)) (null message))
                     (format nil "authentication failed (HTTP ~a): check the Levea key, or ~
                                  generate one at ~a" status +key-portal+))
                    (message (format nil "~a (HTTP ~a)" message status))
                    (t (format nil "~a failed with HTTP ~a." label status)))))
        ((and (not (eq declared t)) (not active-key))
         (say (format nil "~a answered HTTP ~a without an explicit success outcome."
                      label status)))
        ((and (eq declared t) (eq t (nlk:json-value object :any "partial")))
         (say (format nil "~a answered success and partial at once." label) "partial"))
        ((and (eq declared t) require-verification
              (not (eq t (nlk:json-value object :any "verificationPassed"))))
         (say (format nil "~a answered success without authoritative verification; the ~
                          edit is not confirmed. Poll the job, or read the message." label)))
        ((and require-message (eq declared t) (null message))
         (say (format nil "~a answered success without a human-readable outcome." label)))
        (t
         (setf (gethash "success" out) (eq declared t)
               (gethash "status" out)
               (cond (state state)
                     (active-key (if active "running" "idle"))
                     (t (if (eq declared t) "accepted" "failed")))
               (gethash "message" out)
               (or message
                   (format nil "~a: ~a." label (gethash "status" out))))
         out)))))

(defun call (method path &key body label require-message require-verification
                               fallback-project accept-active)
  "The normalized result of one call, with a state the backend reported as a
failure read as one whatever its success flag said."
  (multiple-value-bind (payload status)
      (http-call method path body)
    (let ((result (normalize payload status
                             :label label :require-message require-message
                             :require-verification require-verification
                             :fallback-project fallback-project
                             :accept-active accept-active)))
      (let ((state (string-downcase (or (gethash "status" result) ""))))
        (when (and (eq t (gethash "success" result))
                   (member state +failed-states+ :test #'string=))
          (setf (gethash "success" result) nil
                (gethash "message" result)
                (or (nlk:json-value result :string "error")
                    (format nil "~a ended ~a." label state)))))
      result)))

(defun pending-p (result)
  "RESULT reports an edit still running."
  (member (string-downcase (or (gethash "status" result) ""))
          +pending-states+ :test #'string=))

(defun poll (path label &key wait fallback-project accept-active)
  "One GET of PATH, or with WAIT the same GET every poll_seconds until the
state is terminal or timeout_seconds have passed, so a long render is
followed in the turn that started it."
  (let ((result (call :get path :label label :fallback-project fallback-project
                             :accept-active accept-active)))
    (when wait
      (let ((deadline (+ (get-internal-real-time)
                          (* (setting :timeout-seconds) internal-time-units-per-second))))
        (loop while (and (eq t (gethash "success" result)) (pending-p result))
              do (if (>= (get-internal-real-time) deadline)
                     (progn
                       (setf (gethash "success" result) nil
                             (gethash "message" result)
                             (format nil "timed out after ~a seconds waiting for ~a; it is still ~
                                          ~a. Poll again with the same id."
                                     (setting :timeout-seconds) label
                                     (gethash "status" result)))
                       (return))
                     (progn
                       (sleep (setting :poll-seconds))
                       (setf result (call :get path :label label
                                               :fallback-project fallback-project
                                               :accept-active accept-active)))))))
    result))

;;; --- what the model reads -------------------------------------------------------

(defparameter +answer-keys+
  '(("projectId" . "project_id") ("videoUrl" . "video_url") ("jobId" . "job_id")
    ("taskId" . "task_id") ("status" . "status") ("durationSeconds" . "duration_seconds"))
  "The members of an answer worth one line each above the JSON, in this order.")

(defun value-text (value)
  "VALUE as one line, a nested object as its JSON."
  (cond ((stringp value) value)
        ((eq value t) "true")
        ((null value) "false")
        ((numberp value) (format nil "~a" value))
        (t (nlk:encode-json-object value))))

(defun bounded-json (result &optional (cap 6000))
  "RESULT as JSON text, cut at CAP characters with a note, so one answer cannot
bury the turn in a scene."
  (let ((text (nlk:encode-json-object result)))
    (if (> (length text) cap)
        (format nil "~a... (~a more characters; ask for the project or job by id for the rest)"
                (subseq text 0 cap) (- (length text) cap))
        text)))

(defun answer-text (result &key label)
  "RESULT as the text the model reads: the state, what the backend said, the one
line per field the model needs next, then the whole answer as JSON."
  (let ((success (eq t (gethash "success" result))))
    (with-output-to-string (out)
      (format out "~a: ~a~@[ -- ~a~]~%"
              label (or (gethash "status" result) "error")
              (nlk:json-value result :string "message"))
      (dolist (pair +answer-keys+)
        (let ((value (gethash (car pair) result)))
          (when (and value (not (eq value t)))
            (format out "~a: ~a~%" (cdr pair) (value-text value)))))
      (unless success
        (format out "~@[code: ~a~%~]" (nlk:json-value result :string "code")))
      (format out "answer: ~a~%" (bounded-json result)))))

(defun answer (result &key label)
  "RESULT answered to the model, marked a failed call when it did not succeed."
  (let ((text (answer-text result :label label)))
    (if (eq t (gethash "success" result)) text (nle:failure text))))
