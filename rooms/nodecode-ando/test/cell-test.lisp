;;;; cell-test.lisp --- the Ando cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every address is a stubbed *HTTP*, every inbox row a JSON object written
;;;; here, every turn a stubbed NLE:SUBMIT and every credential a scratch file
;;;; of the test's own: nothing reaches Ando or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "ando" "ANDO-CELL-" :start nodecode-ando:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defun ando-credential-json ()
  "A connection as the invitation would have answered it."
  (nlk:make-json-object "contract_version" 1
                         "api_key" "key-from-the-invitation"
                         "workspace_id" "workspace-1"
                         "agent_membership_id" "8f0c3b2e-1111-4a2b-9c3d-0123456789ab"
                         "connected_by_membership_id" "installer-1"
                         "mcp_url" "https://mcp.ando.so/mcp"
                         "origin" "https://api.ando.so"
                         "display_name" "Scout"
                         "receiver_id" "receiver-1"))

(defun write-fixture-credential (path)
  "PATH holding a private connection file; => PATH."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (write-string (nlk:encode-json-object (ando-credential-json)) stream))
  (sb-posix:chmod (namestring path) #o600)
  path)

(defvar *fixture-credential* nil
  "The scratch credential file the lifecycle fixture wrote.")

(defmacro with-ando-fixture ((var) &body body)
  "BODY with VAR a private credential file in a scratch folder of its own."
  (let ((directory (gensym "DIRECTORY")))
    `(let* ((,directory (merge-pathnames
                         (format nil "nodecode-ando-~36r/" (random most-positive-fixnum))
                         (uiop:temporary-directory)))
            (,var (write-fixture-credential (merge-pathnames "connection.json" ,directory))))
       (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree ,directory :validate t)))
         ,@body))))

(defmacro with-ando-cell ((path &rest config) &body body)
  "BODY with the cell started on the credential file PATH, polling far enough
away that no test waits on it, and every address stubbed."
  `(let ((*fixture-credential* ,path)
         (nodecode-ando::*http* (lambda (method url &key headers content)
                                   (declare (ignore method url headers content))
                                   (values "{}" 200 nil))))
     (with-cell-stop ((ando-start "credential_file" (namestring ,path) "poll_seconds" 600 ,@config))
       ,@body)))

(defmacro with-ando-template (&body body)
  "Every lifecycle test: a credential file, and no address reached."
  `(with-ando-fixture (*fixture-credential*)
     (let ((nodecode-ando::*http* (lambda (method url &key headers content)
                                    (declare (ignore method url headers content))
                                    (values "{}" 200 nil))))
       ,@body)))

(define-cell-lifecycle-tests "ando"
  (:config "credential_file" (namestring *fixture-credential*) "poll_seconds" 600)
  (:help :ando)
  (:command "ando")
  (:fixture with-ando-template)
  (:running (is (nodecode-ando::ando-connection-p nodecode-ando::*connection*) "the credential file is the running connection"))
  (:refused ("reply_timeout_seconds" 5) ("poll_seconds" 1))
  (:idle ando:ando-error (ando:reply "x")))

;;; --- the invitation ---------------------------------------------------------------

(deftest ando-cell-redeems-an-invitation-once ()
  (with-ando-fixture (path)
    (let* ((requests '())
           (nodecode-ando::*http*
            (lambda (method url &key headers content)
              (declare (ignore headers))
              (push (list method url content) requests)
              (cond ((equal method "GET")
                     (values "(agent 8f0c3b2e-1111-4a2b-9c3d-0123456789ab); keep this identity"
                             200 nil))
                    ((search "/connect" url)
                     (values (nlk:encode-json-object (ando-credential-json)) 200 nil))
                    (t (values "{}" 404 nil))))))
      (let ((connection (nodecode-ando::redeem
                         "https://agents.ando.so/invite/abcdefghjk" nil)))
        (is (equal "Scout" (nodecode-ando::ando-connection-display-name connection)))
        (let ((origin (nodecode-ando::ando-connection-origin connection)))
          (is (equal "https://api.ando.so" origin) "the endpoints the invitation's host stands for"))
        (let ((post (first requests)))
          (is (equal :post (first post)))
          (let ((body (nlk:decode-json (third post))))
            (is (equal "abcdefghjk" (nlk:json-value body :string "passcode")))
            (is (equal "nodecode" (nlk:json-value body :string "harness")))
            (is (null (nlk:json-value body :string "name")) "an existing agent keeps its own name")))
        (nodecode-ando::write-credential connection path)
        (let ((mode (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring path))))))
          (is (= #o600 mode) "the key lands in a private file"))
        (let ((again (nodecode-ando::read-credential path)))
          (is (equal "key-from-the-invitation" (nodecode-ando::ando-connection-api-key again)))
          (is (equal "receiver-1" (nodecode-ando::ando-connection-receiver-id again))))))))

(deftest ando-cell-refuses-a-foreign-link ()
  (with-stubbed-fdefinition (nodecode-ando::dial (method url &key headers content)
                              (declare (ignore method url headers content))
                              (values "{}" 200 nil))
    (is (signals-error ando:ando-error
          (nodecode-ando::redeem "https://elsewhere.test/invite/abcdefghjk" "Nodecode")) "another host is not an Ando invitation")
    (is (signals-error ando:ando-error
          (nodecode-ando::redeem "https://agents.ando.so/invite/short" "Nodecode")) "a path with no code is not one")))

;;; --- the wire ------------------------------------------------------------------

(deftest ando-cell-speaks-mcp-over-one-post-per-message ()
  (let* ((requests '())
         (nodecode-ando::*http*
          (lambda (method url &key headers content)
            (declare (ignore method))
            (push (list url headers (nlk:decode-json content)) requests)
            (let ((asked (nlk:json-value (nlk:decode-json content) :string "method")))
              (cond ((equal asked "initialize")
                     (values (nlk:encode-json-object
                              (nlk:make-json-object
                               "jsonrpc" "2.0" "id" 1
                               "result" (nlk:make-json-object "protocolVersion" "2025-06-18")))
                             200 '(("mcp-session-id" . "session-1"))))
                    ((equal asked "notifications/initialized")
                     (values "" 202 nil))
                    ((equal asked "tools/call")
                     (values (format nil "event: message~%data: ~a~%~%"
                                     (nlk:encode-json-object
                                      (nlk:make-json-object
                                       "jsonrpc" "2.0" "id" 2
                                       "result" (nlk:make-json-object
                                                 "structuredContent"
                                                 (nlk:make-json-object "items" #()
                                                                       "scope" "direct")))))
                             200 '(("content-type" . "text/event-stream"))))
                    (t (values "{}" 202 nil)))))))
    (let* ((connection (nodecode-ando::connection-for-stored (ando-credential-json) "/none"))
           (result (nodecode-ando::call-tool
                    connection "get_agent_inbox" (nlk:make-json-object "scope" "direct"))))
      (is (equal "direct" (nlk:json-value result :string "scope")) "a server-sent answer is read as its result")
      (let ((call (first requests)))
        (is (equal "https://mcp.ando.so/mcp" (first call)))
        (is (equal "session-1" (nodecode-ando::header-value (second call) "mcp-session-id")) "the handshake's session rides the call")
        (is (equal "Bearer key-from-the-invitation"
                   (nodecode-ando::header-value (second call) "authorization")))
        (is (equal "get_agent_inbox" (nlk:json-value (third call) :string "params" "name")))))))

;;; --- one message, one answer -------------------------------------------------------

(defun ando-inbox-item ()
  "One direct message, as the inbox answers it."
  (nlk:make-json-object "event_id" "event-1" "type" "direct_message"
                         "message_id" "message-1" "conversation_id" "conversation-1"
                         "revision" 1))

(defun ando-message ()
  "The message that row points at."
  (nlk:make-json-object "data" (nlk:make-json-object
                                 "id" "message-1" "conversation_id" "conversation-1"
                                 "authorWorkspaceMembershipId" "member-1"
                                 "content" "hello from the workspace")))

(defun ando-inbox-stub (name arguments posts)
  "CALL-TOOL's answers for the sweep: one row, its message, and a posted reply."
  (declare (ignorable arguments))
  (cond ((equal name "get_agent_inbox")
         (nlk:make-json-object "items" (vector (ando-inbox-item))
                                "page" (nlk:make-json-object "has_more" nil)))
        ((equal name "get_message") (ando-message))
        ((member name '("send_message" "reply_to_message") :test #'string=)
         ;; POSTS is a box -- (list '()) -- because a parameter cannot be PUSHed
         ;; back into its caller.
         (push (nlk:json-value arguments :string "markdown_content") (car posts))
         (nlk:make-json-object "message_id" "posted-1"))
        (t (nlk:make-json-object))))

(deftest ando-cell-answers-a-message-in-its-conversation ()
  (with-ando-fixture (path)
    (let ((calls '())
          (prompts '())
          (posts (list nil)))
      (with-ando-cell (path)
        (with-stubbed-fdefinitions
            ((nle:submit (session prompt &key command-id source source-id &allow-other-keys)
               (push (list session prompt command-id source source-id) prompts)
               (ando:reply "hello back from the agent")
               (list :disposition :started))
             (nodecode-ando::call-tool (conn name &optional arguments)
               (declare (ignore conn))
               (push (cons name arguments) calls)
               (ando-inbox-stub name arguments posts)))
          (let ((items (nodecode-ando::sweep nodecode-ando::*connection*)))
            (is (= 1 (length items)) "the sweep reads the message row")
            (nodecode-ando::answer-item nodecode-ando::*connection* (first items)))
          (is (= 1 (length prompts)) "one message, one turn")
          (is (search "hello from the workspace" (second (first prompts))) "the turn carries the words sent")
          (is (equal "ando" (first (first prompts))) "in the section's session")
          (is (equal "ando" (fourth (first prompts))) "the turn names the cell as its source")
          (let ((sent (cdr (find "send_message" calls :key #'car :test #'string=))))
            (is (equal "conversation-1" (nlk:json-value sent :string "conversation_id")))
            (is (equal "hello back from the agent"
                       (nlk:json-value sent :string "markdown_content"))))
          (is (equal '("hello back from the agent") (car posts)) "exactly one answer was posted")
          (let ((claim (cdr (find "acknowledge_agent_inbox_item" calls
                                  :key #'car :test #'string=))))
            (is (equal "handled" (nlk:json-value claim :string "state"))))
          (nodecode-ando::poll-step)
          (is (= 1 (length prompts)) "the same row is never answered twice"))))))

(deftest ando-cell-says-so-when-a-turn-stops ()
  (with-ando-fixture (path)
    (let ((posts (list nil))
          (answered nil))
      (with-ando-cell (path)
        (with-stubbed-fdefinitions
            ((nle:submit (session prompt &key command-id source source-id &allow-other-keys)
               (declare (ignore session prompt command-id source source-id))
               (list :disposition :started))
             (nodecode-ando::wait-for-reply () answered)
             (nodecode-ando::call-tool (conn name &optional arguments)
               (declare (ignore conn))
               (ando-inbox-stub name arguments posts)))
          (let ((items (nodecode-ando::sweep nodecode-ando::*connection*)))
            (is (= 1 (length items)) "the sweep reads the message row")
            (nodecode-ando::answer-item nodecode-ando::*connection* (first items)))
          (is (= 1 (length (car posts))))
          (let ((said (first (car posts))))
            (is (search "I stopped before I could finish" said) "a turn that said nothing still says so, once")))))))

;;; --- the command -----------------------------------------------------------------

(deftest ando-cell-status-names-the-agent-and-the-workspace ()
  (with-ando-fixture (path)
    (let ((nodecode-ando::*http*
            (lambda (method url &key headers content)
              (declare (ignore method url headers))
              (let ((asked (nlk:json-value (nlk:decode-json content) :string "method")))
                (cond ((equal asked "initialize")
                       (values (nlk:encode-json-object
                                (nlk:make-json-object "jsonrpc" "2.0" "id" 1
                                                       "result" (nlk:make-json-object)))
                               200 '(("mcp-session-id" . "session-1"))))
                      ((equal asked "notifications/initialized") (values "" 202 nil))
                      (t (values (nlk:encode-json-object
                                   (nlk:make-json-object
                                    "jsonrpc" "2.0" "id" 2
                                    "result" (nlk:make-json-object
                                              "structuredContent"
                                              (nlk:make-json-object
                                               "identity" (nlk:make-json-object
                                                           "display_name" "Scout")
                                               "workspace" (nlk:make-json-object
                                                            "name" "Launch")))))
                                 200 nil)))))))
      (with-cell-stop ((ando-start "credential_file" (namestring path) "poll_seconds" 600))
        (let ((answer (cell-entry "nodecode-ando" "ando" "")))
          (is (search "Scout" answer))
          (is (search "Launch" answer)))))))
