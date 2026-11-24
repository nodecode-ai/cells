;;;; cell.lisp --- Ambassly's hosted MCP server as two tools, /ambassly and a manual entry.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of MelMayssonOwen's `ambassly' plugin for Hermes Agent (see
;;;; NOTICE). The original was a portable manifest: an MCP server at
;;;; https://ambassly.com/api/mcp reached with a Bearer scoped key, plus a
;;;; workflow skill. This cell speaks that server's streamable HTTP itself --
;;;; initialize, notifications/initialized, tools/list, tools/call -- so no
;;;; MCP client has to be installed, and it puts the skill's playbook on
;;;; (help :ambassly).
;;;;
;;;; Two tools, because the server's tool list is its own and changes there,
;;;; not here: `ambassly_tools' lists what the key may call, `ambassly_call'
;;;; calls one by name with a JSON object of arguments. /ambassly does both
;;;; from the operator's side. Everything that moves money on the server is a
;;;; write, and the playbook asks a person to decide each commission.

(defpackage #:nodecode-ambassly
  (:nicknames #:ambassly)
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:ambassly-tools #:ambassly-call #:ambassly-text
           #:tool-list-tool #:tool-call-tool #:run-slash
           #:decode-ambassly-tool-list #:decode-ambassly-tool-call
           #:ambassly-error #:ambassly-error-detail))

(in-package #:nodecode-ambassly)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks.")

(defparameter +result-limit+ 8000
  "Characters of a tool's answer kept before it is cut.")

(defvar *connection* nil
  "The started connection, or NIL while the cell is stopped.")

;;; --- conditions ---------------------------------------------------------------

(nlk:define-peripheral ambassly :not-running t)

;; AMBASSY-ERROR carries every refusal the model reads: the server's own
;; words, a status, a missing key. A verb called while the cell is stopped
;; refuses the same way, naming the cell, so nothing reaches the wire.
;; (AMBASSLY-ERROR-DETAIL) reads the detail out of the condition.

;;; --- the connection -----------------------------------------------------------

(nlk:define-record (connection (:copier nil))
  "One conversation with the server: where it is, what it holds."
  (url "https://ambassly.com/api/mcp" :type string)
  (key-env "AMBASSLY_API_KEY" :type string)
  (timeout 30 :type integer)
  (session-id nil)
  (next-id 0 :type integer)
  (initialized-p nil)
  (tools '() :type list)
  (tools-read-p nil))


(defun current-connection ()
  "The started connection, or a refusal naming the cell as stopped."
  (or *connection*
      (fail "the ambassly cell is not running: check (cells) and the ambassly section of the config")))

(defun connection-key (connection)
  "The scoped key CONNECTION sends, or a refusal naming the variable to set."
  (or (nle::credential-env connection.key-env)
      (fail "no key -- set ~a to a scoped Ambassly API key" connection.key-env)))

;;; --- one HTTP exchange --------------------------------------------------------

(defun http-post (url headers content timeout)
  "POST CONTENT to URL. => (values STREAM STATUS HEADERS), the body still open."
  (nlk:http :post url :headers headers :content content :timeout timeout :stream t))

(defun response-header (headers name)
  "NAME's value in HEADERS, a dexador header table or an alist."
  (cond ((hash-table-p headers)
         (or (gethash name headers) (gethash (string-downcase name) headers)))
        ((listp headers)
         (cdr (assoc name headers :test #'string-equal)))))

(defun body-text (body)
  "BODY -- a stream or a string -- as its whole text, the stream closed."
  (cond ((null body) "")
        ((streamp body)
         (unwind-protect (or (ignore-errors (alexandria:read-stream-content-into-string body)) "")
           (ignore-errors (close body :abort t))))
        (t (princ-to-string body))))

(defun reply-message-p (message)
  "Whether MESSAGE answers a request: an id and no method."
  (and (hash-table-p message)
       (nth-value 1 (gethash "id" message))
       (null (nlk:json-value message :string "method"))))

(defun response-messages (body headers status)
  "The JSON-RPC messages one response carries, BODY read or a stream.
An SSE body is read only up to the frames that answer a request."
  (cond
    ((member status '(202 204)) '())
    ((search "text/event-stream" (string-downcase (or (response-header headers "content-type") "")))
     (let ((messages '())
           (stream (if (streamp body) body (make-string-input-stream (or body "")))))
       (unwind-protect
            (progn
              (nlk:read-events stream
                                (lambda (text)
                                  (let ((message (ignore-errors (nlk:decode-json text))))
                                    (when (hash-table-p message) (push message messages))
                                    (reply-message-p message)))
                                :limit (* 4 1024 1024))
              (nreverse messages))
         (when (streamp body) (ignore-errors (close body :abort t))))))
    (t (let ((value (ignore-errors (nlk:decode-json (body-text body)))))
         (cond ((hash-table-p value) (list value))
               ((vectorp value) (coerce value 'list))
               (t '()))))))

(defun request-headers (connection)
  "The headers of one request on CONNECTION, its key as the Bearer."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream"))
          (when connection.session-id
            (list (cons "mcp-session-id" connection.session-id)))
          (list (cons "authorization" (format nil "Bearer ~a" (connection-key connection))))))

(defun post-frame (connection frame)
  "POST one JSON-RPC FRAME; the messages its answer carries."
  (multiple-value-bind (body status headers)
      (http-post connection.url (request-headers connection) frame
                 connection.timeout)
    (let ((session (response-header headers "mcp-session-id")))
      (when (and (stringp session) (plusp (length session)))
        (setf connection.session-id session)))
    (unless (<= 200 status 299)
      (fail "the server answered HTTP ~d~@[ (~a)~]"
            status
            (let ((text (string-trim '(#\Space #\Newline #\Return #\Tab) (body-text body))))
              (and (plusp (length text)) (subseq text 0 (min 200 (length text)))))))
    (response-messages body headers status)))

(defun rpc (method &optional params)
  "Send METHOD with PARAMS on the started connection; answer its result."
  (let* ((connection (current-connection))
         (id (incf connection.next-id))
         (frame (nlk:encode-json-object
                 (nlk:json-object "jsonrpc" "2.0" "id" id "method" method
                                  "params" (or params (nlk:make-json-object)))))
         (messages (post-frame connection frame)))
    (let ((reply (find-if (lambda (message) (eql id (gethash "id" message))) messages)))
      (unless reply
        (fail "no answer to ~a (the exchange carried none)" method))
      (when (nth-value 1 (gethash "error" reply))
        (let ((error (gethash "error" reply)))
          (fail "~a refused -- ~a" method
                (or (nlk:json-value error :string "message") "no message"))))
      (gethash "result" reply))))

(defun notify (method &optional params)
  "Send notification METHOD; its answer carries nothing."
  (post-frame (current-connection)
               (nlk:encode-json-object
                (nlk:json-object "jsonrpc" "2.0" "method" method
                                 "params" (or params (nlk:make-json-object))))))

(defun ensure-ready (connection)
  "Introduce this client to CONNECTION's server once."
  (unless connection.initialized-p
    (rpc "initialize"
         (nlk:json-object "protocolVersion" +protocol-version+
                          "capabilities" (nlk:make-json-object)
                          "clientInfo" (nlk:json-object "name" "nodecode" "version" "0.1.0")))
    (notify "notifications/initialized")
    (setf connection.initialized-p t)))

;;; --- the server's tools -------------------------------------------------------

(defun list-server-tools (connection)
  "The server's own catalog, one plist per tool, in name order."
  (let* ((result (rpc "tools/list"))
         (tools (sort (loop for tool across (or (nlk:json-array result "tools") #())
                            for name = (nlk:json-value tool :text "name")
                            when name
                              collect (list :name name
                                            :description (or (nlk:json-value tool :string "description")
                                                             "")))
                     #'string< :key (lambda (tool) (getf tool :name)))))
    (setf connection.tools tools
          connection.tools-read-p t)
    tools))

(defun ambassly-tools (&key refresh)
  "What the key may call on the server: (:NAME :DESCRIPTION) plists."
  (let ((connection (current-connection)))
    (ensure-ready connection)
    (when (or refresh (not connection.tools-read-p))
      (list-server-tools connection))
    connection.tools))

(defun tools-text (tools)
  "TOOLS, as the list a person and a model read."
  (if (null tools)
      "ambassly: the server listed no tools for this key"
      (format nil "~d tool~:p on the Ambassly server:~%~{  ~a~^~%~}"
              (length tools)
              (mapcar (lambda (tool)
                        (let ((description (getf tool :description)))
                          (if (plusp (length description))
                              (format nil "~a -- ~a" (getf tool :name) description)
                              (getf tool :name))))
                      tools))))

(defun truncated (text)
  "TEXT, cut at +RESULT-LIMIT+ characters."
  (if (> (length text) +result-limit+)
      (concatenate 'string (subseq text 0 +result-limit+) " ... (cut)")
      text))

(defun result-text (result)
  "A tools/call RESULT as the text the model reads: its content parts."
  (let* ((parts (loop for part across (or (nlk:json-array result "content") #())
                      for type = (nlk:json-value part :string "type")
                      for text = (nlk:json-value part :string "text")
                      when (and text (or (null type) (equal type "text")))
                        collect text))
         (failed (let ((flag (nlk:json-value result :boolean "isError"))) (and flag t)))
         (text (truncated (if parts
                               (format nil "~{~a~^~%~}" parts)
                               (or (ignore-errors (nlk:encode-json-object result)) "")))))
    (cond ((zerop (length text))
           (if failed "ambassly: the tool failed and said nothing" "ambassly: the tool answered nothing"))
          (failed (format nil "ambassly: the tool failed -- ~a" text))
          (t text))))

(defun ambassly-call (tool &optional arguments)
  "Call TOOL on the server with ARGUMENTS, an object; answer its text."
  (let ((connection (current-connection)))
    (ensure-ready connection)
    (result-text (rpc "tools/call"
                      (nlk:json-object "name" tool
                                       "arguments" (or arguments (nlk:make-json-object)))))))

;;; --- the model's two tools ----------------------------------------------------

(nlk:define-contract ambassly-tool-list
  (:doc "List the tools this Ambassly key may call on the hosted Ambassly server: affiliate programs, per-content referral links, referrals, commissions and payouts, either the merchant side (a company key) or one creator's portal (an affiliate key). Call it once before ambassly_call, to see the names and what each one takes; the list is the server's own and changes there, not here.")
  ("refresh" :boolean :default nil
             :doc "read the catalog again instead of the copy this session already has"))

(nlk:define-contract ambassly-tool-call
  (:doc "Call one Ambassly tool by name, with the arguments object it documents. Use ambassly_tools first for the names. Write tools on a company key create programs and affiliates, approve or reject commissions and add Stripe webhook secrets, so confirm the exact values with the person before a write, and treat every commission as their decision. The answer is the tool's own text.")
  ("tool" :text :required t
          :doc "the tool name as ambassly_tools lists it, for example list_programs")
  ("arguments" :object
               :doc "the arguments object the tool takes; omit it for a tool that takes none"))

(defun tool-list-tool (query)
  "The ambassly_tools tool: the server's catalog as text."
  (tools-text (ambassly-tools :refresh (ambassly-tool-list-refresh query))))

(defun tool-call-tool (call)
  "The ambassly_call tool: one tool's own answer."
  (ambassly-call (ambassly-tool-call-tool call)
                 (ambassly-tool-call-arguments call)))

(defun ambassly-text (&key refresh)
  "The catalog as text, for a script or the command."
  (tools-text (ambassly-tools :refresh refresh)))

;;; --- /ambassly ----------------------------------------------------------------

(defun parse-arguments (text)
  "TEXT as a JSON object, or NIL when it is empty; a refusal otherwise."
  (let ((text (string-trim '(#\Space #\Tab) text)))
    (cond ((zerop (length text)) nil)
          (t (let ((value (ignore-errors (nlk:decode-json text))))
               (cond ((hash-table-p value) value)
                     ((and (vectorp value) (zerop (length value))) (nlk:make-json-object))
                     (t (fail "the arguments must be one JSON object"))))))))

(defun run-slash (args session-id)
  "/ambassly [TOOL [JSON]]: the server's tools, or one tool's answer."
  (declare (ignore session-id))
  (handler-case
      (let ((args (string-trim '(#\Space #\Tab) (or args ""))))
        (if (zerop (length args))
            (values (ambassly-text) nil)
            (let* ((space (position #\Space args))
                   (name (if space (subseq args 0 space) args))
                   (rest (if space (subseq args (1+ space)) "")))
              (values (ambassly-call name (parse-arguments rest)) nil))))
    (ambassly-error (condition) (values (princ-to-string condition) nil))))

;;; --- the manual ---------------------------------------------------------------

(defparameter +manual+
  "Ambassly runs a Stripe-based affiliate program: programs, per-content referral links, referrals,
commissions and payouts. Every call goes to Ambassly's hosted MCP server (ambassly.com/api/mcp)
with the scoped key the `ambassly' section names; a company key operates the merchant side, an
affiliate key one creator's portal. The server owns the tool list, so read it first:

  (nodecode-ambassly:ambassly-text)            the tools and what each is for
  (nodecode-ambassly:ambassly-call \"list_programs\")   one tool, no arguments
  (nodecode-ambassly:ambassly-call \"create_link\" (nlk:json-object \"program_id\" \"...\"))
  /ambassly                                     the same list from the shell
  /ambassly list_pending_commissions            one tool's answer
  /ambassly create_program {\"name\":\"...\",\"destination_url\":\"...\",\"rate\":20}

The model has the same two moves as the tools ambassly_tools and ambassly_call.

Pick the right side first: merchant work uses the company tools (list_programs, create_program,
list_affiliates, list_referrals, list_commissions, get_payout_batch); creator work uses my_stats,
my_links and create_link. A refused tool usually means the key is for the other side -- say so
rather than retrying.

Launch a program: list_programs first, so a product never gets a second program; confirm the name,
destination URL, commission rate, duration and cookie window with the person; create_program only
with those values; then point them at the tracking snippet and the Stripe webhook, because
attribution needs both; invite with create_affiliate_invite (the affiliate accepts by email) or
enrol with create_affiliate.

Review money carefully: commission and payout actions move real money, so they are the person's
decisions. Show each pending commission with its affiliate, amount and source referral; ask for an
explicit decision on each one, or on a group they named; approve_commission or reject_commission
only for what they named, a rejection with a reason; get_payout_batch prepares a list to pay and
sends nothing.

Links and reporting: create_link or create_affiliate_link once per video, post or newsletter, so
each piece of content is measured; get_program_stats, list_referrals and list_commissions report
-- report the numbers returned and never estimate a missing one.

Secrets: keep the key and any Stripe signing secret in the section, not in a message. Call
add_stripe_webhook_secret only with a value the person supplied through a secure route. The key is
a Bearer header to ambassly.com and to no other host."
  "What (help :ambassly) answers while the cell runs.")

;;; --- the cell -----------------------------------------------------------------

(defun start-connection ()
  "Open this session's connection from the section; its stop closes it."
  (let ((connection (make-connection :url (setting :url)
                                     :key-env (setting :api-key)
                                     :timeout (setting :timeout-seconds))))
    (setf *connection* connection)
    (nle:on-stop (lambda () (setf *connection* nil)))))

(nle:define-cell ambassly
  (:section ("ambassly")
    (:guide "api_key names the environment variable holding a scoped Ambassly key (create one in Ambassly's dashboard); a company key runs the merchant side, an affiliate key one creator's portal; next step: /ambassly lists the tools the key may call")
    ("api_key" :env :default "AMBASSLY_API_KEY"
               :doc "the variable holding a scoped Ambassly API key, sent as a Bearer header to ambassly.com and nowhere else")
    ("url" :string :default "https://ambassly.com/api/mcp"
           :doc "the hosted MCP endpoint")
    ("timeout_seconds" :integer :default 30 :min 1
                       :doc "one request's ceiling in seconds"))
  (:tool "ambassly_tools" (nlk:contract 'ambassly-tool-list) 'tool-list-tool)
  (:tool "ambassly_call" (nlk:contract 'ambassly-tool-call) 'tool-call-tool)
  (:command "ambassly" 'run-slash
            :description "Ambassly: the affiliate-program tools, or one of them"
            :argument-hint "TOOL [JSON]")
  (:help :ambassly
         "ambassly: Stripe affiliate programs, links, referrals and commissions through Ambassly's server"
         +manual+)
  (:start #'start-connection))
