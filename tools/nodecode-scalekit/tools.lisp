;;;; tools.lisp --- the four tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin published four tools; the names, the argument names and the
;;;; endpoints are its own, unchanged. Each of the three that take arguments
;;;; is one NLK:DEFINE-CONTRACT, so the shape the model is shown and the shape
;;;; the body reads are one declaration; the fourth takes none, and its schema
;;;; is written by hand.
;;;;
;;;; Every tool answers text: the model reads it. Provider tokens stay in
;;;; Scalekit, and what comes back has the token-bearing keys taken out.

(in-package #:nodecode-scalekit)

;;; --- reading what came back ------------------------------------------------------

(defparameter +token-keys+ '("access_token" "refresh_token" "id_token" "client_secret" "token")
  "The keys a result never carries back: they are Scalekit's, not the model's.")

(defun without-tokens (value)
  "VALUE — a decoded Scalekit answer — with every token-bearing key dropped."
  (cond ((stringp value) value)
        ((hash-table-p value)
         (let ((kept (nlk:json-object)))
           (maphash (lambda (key item)
                      (unless (member key +token-keys+ :test #'string=)
                        (setf (gethash key kept) (without-tokens item))))
                    value)
           kept))
        ((vectorp value) (map 'vector #'without-tokens value))
        (t value)))

(defun value-text (value &optional (indent 0))
  "VALUE — a decoded JSON value — as the lines the model reads."
  (let ((pad (make-string indent :initial-element #\Space))
        (nested (lambda (item) (string-right-trim '(#\Newline) (value-text item (+ indent 2))))))
    (cond ((stringp value) value)
          ((hash-table-p value)
           (if (zerop (hash-table-count value))
               "{}"
               (with-output-to-string (out)
                 (maphash (lambda (key item)
                            (format out "~a~a: ~a~%" pad key (funcall nested item)))
                          value))))
          ((vectorp value)
           (if (zerop (length value))
               "[]"
               (with-output-to-string (out)
                 (loop for item across value
                       do (format out "~a- ~a~%" pad (funcall nested item))))))
          ((eq value t) "true")
          ((null value) "null")
          (t (princ-to-string value)))))

(defun clip (text limit)
  "TEXT, cut at LIMIT characters with a note of how much is left."
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (format nil "~a... (~d more characters)" (subseq text 0 limit) (- (length text) limit)))))

(defun proxy-disabled-p (payload)
  "Whether a proxy answer says the catalog does not cover the action."
  (search "TOOL_PROXY_DISABLED" (if (stringp payload) payload (value-text payload))))

;;; --- the gate the three acting tools share ----------------------------------------

(defun authorize-gate (connection-name identifier)
  "What approve conn AUTHORIZE answers, as a plist: :STATUS is ACTIVE, or
NEEDS_AUTHORIZATION with the :LINK that approves it. A connection missing or
unfinished signals the refusal to repair."
  (let ((connection (find-connection connection-name)))
    (unless connection
      (fail "scalekit: there is no connection named ~a on ~a. Create it in the Scalekit ~
dashboard under AgentKit, Connections, then call again."
            connection-name (root)))
    (let ((status (or (nlk:json-value connection :text "status") "UNKNOWN"))
          (type (or (nlk:json-value connection :text "type") "OAUTH")))
      (unless (equal status "COMPLETED")
        (fail "scalekit: the ~a connection is ~a; finish it in the Scalekit dashboard under ~
AgentKit, Connections, then call again."
              connection-name status))
      (if (string-equal type "OAUTH")
          (let ((account (ensure-connected-account connection-name identifier)))
            (if (equal account "ACTIVE")
                (list :status "ACTIVE" :connection connection-name :identifier identifier)
                (let ((link (authorization-link connection-name identifier)))
                  (unless link
                    (fail "scalekit: ~a answered no authorization link; open the Scalekit dashboard ~
and finish the connection."
                          connection-name))
                  (list :status "NEEDS_AUTHORIZATION" :connection connection-name
                        :identifier identifier :link link))))
          (let ((account (connected-account-status connection-name identifier)))
            (unless (equal account "ACTIVE")
              (fail "scalekit: the ~a connection has no active account for ~a (status ~a); finish ~
it in the Scalekit dashboard under AgentKit, Connections."
                    connection-name identifier (or account "none")))
            (list :status "ACTIVE" :connection connection-name :identifier identifier))))))

(defun format-gate (gate)
  "GATE — what AUTHORIZE-GATE answered — as the text the model reads."
  (if (equal (getf gate :status) "ACTIVE")
      (format nil "~a is ACTIVE for ~a: call scalekit_execute_tool with connection_name \"~a\"."
              (getf gate :connection) (getf gate :identifier) (getf gate :connection))
      (format nil "~a is not authorized yet for ~a. Open this link to approve it, then call again:~%~a"
              (getf gate :connection) (getf gate :identifier) (getf gate :link))))

(defun person (identifier)
  "The identifier a call names, else this machine's stored one."
  (person-id identifier))

;;; --- scalekit_list_connections ----------------------------------------------------

(defparameter +list-schema+
  (list :description
        (concatenate 'string
                     "List this Scalekit environment's connections: the connection name you pass to "
                     "the other tools, its type, and its status. No provider tokens come back. "
                     "Run it first when you do not know what is connected.")
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:json-object)))
  "The one tool the plugin gives no arguments: a schema written by hand, since a
contract with no member types nothing.")

(defun tool-list-connections (arguments)
  "scalekit_list_connections: the environment's connections, name, type and status."
  (declare (ignore arguments))
  (let ((connections (all-connections)))
    (if (null connections)
        (format nil "scalekit: no connections yet on ~a. Create one in the Scalekit dashboard under ~
AgentKit, Connections."
                (root))
        (with-output-to-string (out)
          (format out "scalekit: ~d connection~:p on ~a~%" (length connections) (root))
          (dolist (connection connections)
            (format out "  ~a  ~a  ~a~%"
                    (or (nlk:json-value connection :text "key_id") "(no name)")
                    (or (nlk:json-value connection :text "type") "OAUTH")
                    (or (nlk:json-value connection :text "status") "UNKNOWN")))))))

;;; --- scalekit_authorize ------------------------------------------------------------

(nlk:define-contract authorize-call
  (:doc "Approve one Scalekit connection for this assistant. Answers ACTIVE, or the link that
approves it: show the link, and do not run a tool until it is approved. Use the connection name
from AgentKit, Connections (gmail-connect), not a conn_ id.")
  ("connection_name" :text :required t
   :doc "the connection name from AgentKit, Connections, for example gmail-connect")
  ("identifier" :string
   :doc "the person id to act for; omit to use the one this machine keeps"))

(defun tool-authorize (call)
  "scalekit_authorize: whether the connection is approved, or the link that does it."
  (format-gate (authorize-gate (authorize-call-connection-name call)
                                (person (authorize-call-identifier call)))))

;;; --- scalekit_execute_tool ----------------------------------------------------------

(nlk:define-contract execute-call
  (:doc "Call one Scalekit tool, once the connected account is ACTIVE. Ask scalekit_authorize
first when you are unsure; an account that is not approved answers the link instead of running the
tool. Provider tokens never come back.")
  ("connection_name" :text :required t
   :doc "the connection name from AgentKit, Connections, for example github-connect")
  ("tool_name" :text :required t
   :doc "the tool's name, as that connector's schema gives it")
  ("tool_input" :object
   :doc "the arguments to call it with, as that tool's schema describes them")
  ("identifier" :string
   :doc "the person id to act for; omit to use the one this machine keeps"))

(defun tool-execute (call)
  "scalekit_execute_tool: the gate, the tool's existence, then the call."
  (let* ((connection (execute-call-connection-name call))
         (tool-name (execute-call-tool-name call))
         (identifier (person (execute-call-identifier call)))
         (gate (authorize-gate connection identifier)))
    (unless (equal (getf gate :status) "ACTIVE")
      (return-from tool-execute (format-gate gate)))
    (unless (known-tool-p tool-name)
      (fail "scalekit: no tool named ~a was found on ~a; the tools a connection offers are listed ~
in the Scalekit dashboard under AgentKit, Tools."
            tool-name connection))
    (clip (value-text (without-tokens
                       (execute-tool-request tool-name connection identifier
                                              (execute-call-tool-input call))))
          (setting :max-chars))))

;;; --- scalekit_proxy ------------------------------------------------------------------

(nlk:define-contract proxy-call
  (:doc "Call a connector's API through Scalekit, for the call no named tool covers. Answers the
API's own JSON or text; an action the catalog does not cover answers an error rather than waiting.
Ask scalekit_authorize first when you are unsure.")
  ("connection_name" :text :required t
   :doc "the connection name from AgentKit, Connections")
  ("path" :text :required t
   :doc "the API path, for example /user")
  ("method" :string :slot http-method
   :doc "the HTTP method: GET unless the API needs another")
  ("query_params" :object
   :doc "the query string as an object of string values")
  ("body" :object
   :doc "the JSON body, for a method that takes one")
  ("identifier" :string
   :doc "the person id to act for; omit to use the one this machine keeps"))

(defun proxy-answer (payload status)
  "PAYLOAD, what the proxy answered, as the text the model reads."
  (cond ((proxy-disabled-p payload)
         "scalekit: the catalog does not cover this action, so the proxy did not wait on it; use a named tool instead.")
        ((and (integerp status) (>= status 400))
         (format nil "scalekit: the proxy answered HTTP ~a~%~a"
                 status (clip (value-text (without-tokens payload)) (setting :max-chars))))
        ((stringp payload) (clip payload (setting :max-chars)))
        ((or (hash-table-p payload) (vectorp payload))
         (clip (value-text (without-tokens payload)) (setting :max-chars)))
        (t "scalekit: this response is a file, and saving files is not available here yet.")))

(defun tool-proxy (call)
  "scalekit_proxy: the gate, then PATH on the connector's API."
  (let* ((connection (proxy-call-connection-name call))
         (path (proxy-call-path call))
         (identifier (person (proxy-call-identifier call)))
         (gate (authorize-gate connection identifier)))
    (unless (equal (getf gate :status) "ACTIVE")
      (return-from tool-proxy (format-gate gate)))
    (unless (and (plusp (length path)) (char= #\/ (char path 0)))
      (fail "scalekit: the path is the API's own, from its root: /user, not user or a whole url."))
    (multiple-value-bind (payload status)
        (proxy-request connection identifier path
                        (or (nonempty (proxy-call-http-method call)) "GET")
                        (proxy-call-query-params call)
                        (proxy-call-body call))
      (proxy-answer payload status))))
