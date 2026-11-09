;;;; wire.lisp --- the service's four requests, and the one place a session lives.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; /call-me is a phone number for an agent: the service mints one session
;;;; per agent (POST /sessions), texts a number (POST /messages) and rings one
;;;; (POST /calls), the spoken answer coming back on the call itself.
;;;;
;;;; This file is the wire and nothing else: every request and every status
;;;; becomes a value or a CALL-ME-ERROR, and the one piece of state the cell
;;;; keeps -- the session token -- lives in a 0600 file under the nodecode home,
;;;; exactly as the original kept it under HERMES_HOME.

(in-package #:nodecode-call-me)

;;; --- the session file -------------------------------------------------------------

(defun load-state ()
  "The saved session as an object, or NIL when there is none to read."
  (let ((path (state-file)))
    (when (probe-file path)
      (handler-case
          (let ((object (nlk:decode-json (uiop:read-file-string path))))
            (and (hash-table-p object) object))
        (error () nil)))))

(defun save-state (state)
  "Write STATE -- the session token and the agent's number -- mode 0600."
  (let ((path (state-file)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string (nlk:encode-json-object state) out))
    #+sbcl (ignore-errors (sb-posix:chmod (uiop:native-namestring path) #o600))
    path))

;;; --- one exchange ------------------------------------------------------------------

(defun api-detail (object status text)
  "The service's own words for a refusal, or NIL for a request it took."
  (when (and (integerp status) (>= status 300))
    (or (and (hash-table-p object) (nonblank (nlk:json-value object :string "detail")))
        (and (stringp text) (plusp (length text)) (nlk:clip text 300)))))

(defun post-json (path payload &key timeout)
  "POST PAYLOAD, an object, to PATH on the service => the object it answered.
A refusal is a CALL-ME-ERROR carrying the service's words; a lost session is
a SESSION-GONE, which the caller answers by minting another."
  (let ((seconds (or timeout (setting :timeout))))
    (handler-case
        (multiple-value-bind (body status)
            (nlk:http :post (concatenate 'string +api+ path)
                      :headers '(("content-type" . "application/json")
                                 ("accept" . "application/json")
                                 ("user-agent" . +user-agent+))
                      :content (nlk:encode-json-object payload)
                      :timeout seconds
                      :connect-timeout (min 10 seconds))
          (let* ((object (ignore-errors (nlk:decode-json body)))
                 (detail (api-detail object status body)))
            (cond ((null detail) (or (and (hash-table-p object) object)
                                     (nlk:make-json-object)))
                  ((and (= status 401) (search "session token" (string-downcase detail)))
                   (error 'session-gone :detail detail))
                  (t (fail "the /call-me service answered HTTP ~a: ~a" status detail)))))
      (session-gone (condition) (error condition))
      (call-me-error (condition) (error condition))
      (error (condition)
        (fail "/call-me did not answer: ~a" (nlk:one-line (princ-to-string condition)))))))

(defun string-field (object key)
  (or (and (hash-table-p object) (nlk:json-value object :string key)) ""))

;;; --- the session -------------------------------------------------------------------

(defun mint-session ()
  "Create the agent's session => the object saved, with its token and number."
  (let* ((object (post-json "/sessions" (nlk:json-object "label" (setting :label))))
         (token (nonblank (string-field object "session_token")))
         (number (nonblank (string-field object "session_number"))))
    (unless (and token number)
      (fail "the /call-me service created a session with no token or number"))
    (let ((state (nlk:json-object "token" token "number" number "label" (setting :label))))
      (save-state state)
      state)))

(defun session-state ()
  "The session the cell runs on: the saved one, else a freshly minted one."
  (let ((state (load-state)))
    (if (and state (nonblank (string-field state "token")))
        state
        (mint-session))))

(defmacro with-session ((token) &body body)
  "BODY with TOKEN bound to the session's token; a session the service has
forgotten is minted again and BODY runs once more."
  `(let ((,token (string-field (session-state) "token")))
     (handler-case (progn ,@body)
       (session-gone ()
         (setf ,token (string-field (mint-session) "token"))
         ,@body))))

;;; --- the three verbs on the wire ---------------------------------------------------

(defun send-message (to message)
  "Text MESSAGE to the allowed number TO => the object the service answered."
  (with-session (token)
    (post-json "/messages"
               (nlk:json-object "session_token" token
                                 "to" to
                                 "body" (nlk:clip message +max-text+)))))

(defun place-call (to text wait)
  "Ring TO, speak TEXT, wait WAIT seconds => the object, carrying the transcript."
  (let ((wait (max +min-wait+ (min +max-wait+ (or wait +max-wait+)))))
    (with-session (token)
      (post-json "/calls"
                 (nlk:json-object "session_token" token
                                   "to" to
                                   "text" (nlk:clip text +max-text+)
                                   "timeout_s" wait)
                 :timeout (+ wait 30)))))
