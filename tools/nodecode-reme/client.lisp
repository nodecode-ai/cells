;;;; client.lisp --- one POST per ReMe action, and the envelope every one answers.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; ReMe's HTTP action service takes a JSON object at <endpoint>/<action> and
;;;; answers {"success": bool, "answer": text, "metadata": {...}}; a failure
;;;; is that same envelope with success false, or an HTTP status. Everything
;;;; here is plain nlk:http, so nothing of the original's Python rides along.

(in-package #:nodecode-reme)

(defparameter +default-endpoint+ "http://127.0.0.1:2333"
  "Where a ReMe action service listens unless it is told otherwise -- the same
loopback default the original plugin ships.")

;;; --- the endpoint ---------------------------------------------------------------

(defun endpoint-url (text)
  "TEXT as a service base URL: absolute, http or https, no credentials, no
query and no fragment. Anything else refuses in words the operator can act on."
  (let* ((url (string-right-trim "/" (string-trim '(#\Space #\Tab #\Newline) (or text ""))))
         (rest (cond ((uiop:string-prefix-p "https://" url) (subseq url (length "https://")))
                     ((uiop:string-prefix-p "http://" url) (subseq url (length "http://")))
                     (t nil))))
    (unless (and rest (plusp (length rest)))
      (fail "~s is not a ReMe endpoint: it is the base URL of a ReMe action service, ~
             http://127.0.0.1:2333 when it runs on this machine" text))
    (when (or (find #\@ rest) (find #\? rest) (find #\# rest))
      (fail "the endpoint ~s carries credentials, a query or a fragment; it is a base URL only" url))
    url))

;;; --- one action -----------------------------------------------------------------

(defun call-action (action payload &key timeout)
  "POST ACTION with PAYLOAD, a JSON object or NIL, and answer the decoded
response envelope. Refuses (REME-ERROR) on a transport failure, a failed
status, a body that is not an object, or an envelope that is not a success."
  (let* ((base (endpoint-url (setting :endpoint)))
         (url (format nil "~a/~a" base action))
         (body (nlk:encode-json-object (or payload (nlk:json-object))))
         (seconds (or timeout (setting :request-timeout))))
    (multiple-value-bind (text status)
        (handler-case
            (nlk:http :post url
                      :headers '(("Accept" . "application/json")
                                 ("Content-Type" . "application/json"))
                      :content body :timeout seconds)
          (error (condition)
            (fail "ReMe at ~a did not answer ~a within ~a s: ~a"
                  base action seconds (nlk:one-line (princ-to-string condition)))))
      (unless (and (integerp status) (<= 200 status 299))
        (fail "ReMe at ~a answered HTTP ~a to ~a: ~a"
              base status action (nlk:clip (nlk:one-line (princ-to-string text)) 240)))
      (let ((response (handler-case (nlk:decode-json text) (error () nil))))
        (unless (hash-table-p response)
          (fail "ReMe at ~a answered ~a with something that is not a JSON object" base action))
        (unless (eq t (nlk:json-value response :boolean "success"))
          (fail "ReMe ~a did not report success: ~a" action
                (or (nlk:json-value response :string "answer") "no reason given")))
        response))))

(defun answer-text (response)
  "RESPONSE's answer as trimmed text, or NIL when it carries none."
  (let ((answer (nlk:json-value response :string "answer")))
    (when answer
      (let ((answer (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
        (and (plusp (length answer)) answer)))))

(defun healthy-p (response)
  "Whether RESPONSE reports a healthy component snapshot -- what the original
plugin requires of a health check, not only a 200."
  (eq t (nlk:json-value response :boolean "metadata" "health" "healthy")))

(defun search-notes (query limit)
  "The notes ReMe holds about QUERY, as its answer text; a line saying none did
when it answered nothing."
  (let* ((response (call-action "search"
                                 (nlk:json-object "query" query "limit" limit)
                                 :timeout (setting :recall-timeout)))
         (answer (answer-text response)))
    (setf *last* (format nil "searched ~s, ~d notes at most" (nlk:clip query 60) limit))
    (or answer (format nil "ReMe holds no note about ~s" (nlk:clip query 80)))))

(defun record-turn (session user assistant)
  "Send one completed turn to ReMe for extraction, and answer one line saying so."
  (call-action "auto_memory"
               (nlk:json-object
                "session_id" (memory-session session)
                "messages" (vector (nlk:json-object "name" "user" "role" "user" "content" user)
                                   (nlk:json-object "name" "assistant" "role" "assistant"
                                                    "content" assistant)))
               :timeout (setting :request-timeout))
  (setf *last* (format nil "recorded a turn of ~a" (memory-session session)))
  (format nil "ReMe recorded the turn under ~a" (memory-session session)))

;;; --- session identity ----------------------------------------------------------------
;;; ReMe files memory per session id, so it has to be filename-safe and
;;; collision-free without the caller thinking about it: a slug and a digest
;;; of the whole id, the original's `_scoped_session_id` read back as Lisp.

(defun slug (text limit)
  "TEXT as lowercase letters, digits, dot, underscore and dash, at most LIMIT
characters: everything else is one dash."
  (let ((out (make-string-output-stream)))
    (loop for char across (string-downcase (or text ""))
          do (write-char (if (or (alphanumericp char) (member char '(#\. #\_ #\-)))
                             char
                             #\-)
                         out))
    (let ((s (string-trim "-._" (get-output-stream-string out))))
      (subseq s 0 (min limit (length s))))))

(defun memory-session (session)
  "SESSION as ReMe files it: this harness, the session's slug, and a digest of
the whole id so two long ids that end alike never share a folder."
  (let* ((session (or session ""))
         (name (slug session 48)))
    (format nil "nodecode-~a-~a"
            (if (plusp (length name)) name "session")
            (subseq (nlk:sha256-text (format nil "~a" session)) 0 12))))
