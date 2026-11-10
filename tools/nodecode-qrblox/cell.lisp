;;;; cell.lisp --- the wire, the section, the seven tools and /qrblox.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin had no code of its own: a plugin.json, an mcp.json
;;;; naming Qrblox's hosted MCP server (streamable HTTP, a bearer sk-qrblox-
;;;; key) and one skill that taught the model the workflow. This cell speaks
;;;; that server's JSON-RPC itself -- initialize, tools/list, tools/call, an
;;;; answer that is either a JSON object or a one-event stream -- through
;;;; nlk:http, and gives the model the same reach as purpose-named tools, with
;;;; the skill's rules as (help :qrblox).
;;;;
;;;; Config, a sibling top-level key next to `websearch':
;;;;   "qrblox": {"api_key": "sk-qrblox-...",
;;;;              "api_key_env": "QRBLOX_API_KEY",
;;;;              "api_key_file": "~/.nodecode/secrets/qrblox-key",
;;;;              "server_url": "https://api.qrblox.com/mcp/v1",
;;;;              "timeout_seconds": 30, "max_chars": 6000}
;;;; The key is the one member no merchant call works without -- as the
;;;; original read QRBLOX_API_KEY -- and a section naming none refuses the
;;;; start. A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-qrblox)

;;; --- the manual ---------------------------------------------------------------

(defparameter *primer*
  "Qrblox holds a merchant's QR codes, audio codes, menus and scan analytics. Every tool here
goes to one host, Qrblox's own MCP server, with the operator's merchant key; every creation
is a draft the merchant publishes in the dashboard.
1. qrblox_ping first. It confirms the server is up and names the workspace the key belongs
   to; an ERROR from it is the network, the server or the key -- report it, do not retry in
   a loop.
2. Read before writing. qrblox_codes lists the codes already on the account and qrblox_menu
   op list the menus; look for an existing code for the same address before drafting a
   second one. qrblox_scans answers the scan totals and the by-day series.
3. Write only what the person asked for, and only once they have named the merchant and what
   to change: qrblox_create drafts a QR code from destination_url and title, qrblox_audio
   drafts an audio code, qrblox_menu op create drafts a menu and op add_item extends one
   (menu_id is the menu's id).
4. After a write, read the answer back: the code's id, its title and the address it opens.
   The server answers drafts; never tell the person a code is live until the answer says so.
5. qrblox_call reaches the rest of the server: qrblox.challenge.create for scan streaks and
   loyalty campaigns, qrblox.merchant.get_profile for the workspace, the voice-agent
   directory lookups. qrblox_call with no tool answers the server's current tool list.
Rules: the key is the operator's -- never invent one, and when it is refused say so and
point at the qrblox section's api_key. The `arguments` member of any tool is passed to the
server as-is, for a member these tools do not name. A destination, a price and a menu's
items are the person's words, never guessed."
  "What (help :qrblox) answers while the cell runs: the skill as one workflow.")

;;; --- answers, and how a refusal reaches the model ---------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on, as the call's failure."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal or a dead network becomes that text, never an
unhandled error."
  `(handler-case (progn ,@body)
     (qrblox-error (condition)
       (nle:failure (format nil "ERROR: ~a" (qrblox-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: qrblox: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun clip (text limit)
  "TEXT, cut at LIMIT characters with a note of how much is left."
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (format nil "~a... (~d more characters)" (subseq text 0 limit) (- (length text) limit)))))

;;; --- reading the section ---------------------------------------------------------

(defparameter +default-url+ "https://api.qrblox.com/mcp/v1"
  "Qrblox's streamable HTTP MCP endpoint, as the plugin's mcp.json names it:
the one host this cell talks to. The repository's README quotes an older
path, https://app.qrblox.com/api/mcp; server_url changes it for a while.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client offers at initialize; the server may answer another.")

(defparameter +client-version+ "0.1.0"
  "What this client calls itself in clientInfo.")

(defun read-key-file (path)
  "The key in PATH's first non-empty line, or NIL when the file is not there."
  (let ((truename (probe-file path)))
    (when truename
      (handler-case
          (with-open-file (in truename :external-format :utf-8)
            (loop for line = (read-line in nil nil)
                  while line
                  for text = (nonblank line)
                  when text return text))
        (error () nil)))))

(defun read-settings (values table)
  "The declared VALUES, with the key resolved once, here: the inline secret,
else the file api_key_file names, else the variable api_key_env names."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :server-url) (or (nonblank (getf values :server-url)) +default-url+)
          (getf plist :timeout-seconds) (or (getf values :timeout-seconds) 30)
          (getf plist :max-chars) (or (getf values :max-chars) 6000))
    (let* ((inline (nonblank (getf values :api-key)))
           (file (nonblank (getf values :api-key-file)))
           (env (nonblank (getf values :api-key-env)))
           (from-env (and env (nonblank (nle::credential-env env))))
           (from-file (and file (read-key-file file))))
      (setf (getf plist :api-key)
            (or inline from-file from-env
                (nlk:config-error
                 "no key: paste the merchant API key (sk-qrblox-...) into api_key, write it ~
into the file api_key_file names, or name the variable holding it in api_key_env~@[ (~a is ~
unset or empty)~]; mint one at create.qrblox.com/settings/api-keys"
                 env))
            (getf plist :key-source)
            (cond (inline "the api_key member of the qrblox section")
                  (from-file "the file api_key_file names")
                  (from-env (format nil "the variable ~a" env))
                  (t "none"))))
    plist))

(defun key-source ()
  "Where the key comes from, in words: never the key itself."
  (or (setting :key-source) "none"))

(defun check-config ()
  "Refuse a section that cannot reach Qrblox, naming the member to set."
  (unless (search "https://" (setting :server-url))
    (error 'nlk:config-refusal
           :detail (format nil "qrblox: server_url must be an https address, ~a by default, not ~a"
                            +default-url+ (setting :server-url)))))

;;; --- the wire: JSON-RPC over HTTP ---------------------------------------------------
;; One POST per message, and an answer that is either the bare object or a
;; one-event stream (data: {...}). A server that hands a session id at
;; initialize wants it back on every request after that; a 404 means the
;; session it gave is gone, so the next call signs in again.

(defvar *session-id* nil
  "The Mcp-Session-Id this image was given at initialize, or NIL.")

(defvar *negotiated* nil
  "The protocol version the server answered at initialize, sent with every request.")

(defvar *initialized* nil
  "Whether this image signed in to the server since the cell started.")

(defvar *request-id* 0)

(defun reset-session ()
  (setf *session-id* nil *negotiated* nil *initialized* nil *request-id* 0))

(defun next-id ()
  (incf *request-id*))

(defun http (method url &key headers content timeout connect-timeout)
  "The cell's one way to the network. Every call goes through this function,
which is the seam a test stubs."
  (nlk:http method url :headers headers :content content
                   :timeout timeout :connect-timeout connect-timeout))

(defun header-value (headers name)
  "NAME's value in the response HEADERS nlk:http answers, or NIL."
  (cond ((hash-table-p headers)
         (or (gethash name headers) (gethash (string-downcase name) headers)))
        ((listp headers) (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun request-headers ()
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "authorization" (format nil "Bearer ~a" (or (setting :api-key) ""))))
          (when *session-id* (list (cons "mcp-session-id" *session-id*)))
          (when *negotiated* (list (cons "mcp-protocol-version" *negotiated*)))))

(defun post-json (body)
  "One HTTPS POST of BODY to the server => (values TEXT STATUS HEADERS)."
  (http :post (setting :server-url)
        :headers (request-headers)
        :content body
        :timeout (setting :timeout-seconds)
        :connect-timeout (min 20 (setting :timeout-seconds))))

(defun stream-json (text)
  "The last JSON object a one-event stream body carries, or NIL."
  (let ((last nil))
    (loop for line in (uiop:split-string (or text "") :separator '(#\Newline))
          do (let ((line (string-right-trim '(#\Return) line)))
               (when (and (> (length line) 5) (string-equal "data:" (subseq line 0 5)))
                 (let ((payload (nlk:trimmed (subseq line 5))))
                   (when (and (plusp (length payload)) (char= (char payload 0) #\{))
                     (setf last payload))))))
    last))

(defun rpc-object (text)
  "The JSON-RPC object in TEXT: a bare body, or the last data line of a stream."
  (let* ((text (and (stringp text) (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
         (text (cond ((null text) nil)
                     ((zerop (length text)) nil)
                     ((char= (char text 0) #\{) text)
                     (t (stream-json text)))))
    (when (and text (plusp (length text)))
      (ignore-errors (nlk:decode-json text)))))

(defun rpc-error-text (object)
  "The words a JSON-RPC error object carries, or NIL."
  (let ((error (and (hash-table-p object) (nlk:json-value object :object "error"))))
    (when error
      (let ((message (nlk:json-value error :text "message"))
            (code (nlk:json-value (nlk:json-value error :object "data") :text "code")))
        (cond ((and message code) (format nil "~a (~a)" message code))
              (message message)
              (t (nlk:one-line (nlk:encode-json-object error))))))))

(defun fail-on-status (status object)
  "A status outside 2xx as the model reads it, naming the key when it is the key."
  (cond
    ((member status '(401 403))
     (fail "Qrblox refused the key (HTTP ~a)~@[: ~a~]. ~
Check the api_key member of the qrblox section: the key is ~
missing, wrong or revoked, and a new one is minted at ~
create.qrblox.com/settings/api-keys. After changing it, run ~
(restart-cells \"nodecode-qrblox\")."
           status (rpc-error-text object)))
    (t
     (fail "Qrblox's MCP server answered HTTP ~a~@[: ~a~]" status (rpc-error-text object)))))

(defun send-rpc (method params)
  "One JSON-RPC request of METHOD with PARAMS => (values OBJECT STATUS)."
  (let ((id (next-id)))
    (multiple-value-bind (text status headers)
        (post-json (nlk:encode-json-object
                    (nlk:json-object "jsonrpc" "2.0" "id" id "method" method
                                     "params" (or params (nlk:make-json-object)))))
      (let ((session (header-value headers "mcp-session-id")))
        (when session
          (setf *session-id* (if (stringp session) session (princ-to-string session)))))
      (values (rpc-object text) status))))

(defun send-note (method)
  "The one no-answer message MCP wants after initialize; a 202 is the answer."
  (ignore-errors
    (post-json (nlk:encode-json-object
                (nlk:json-object "jsonrpc" "2.0" "method" method
                                 "params" (nlk:make-json-object))))
    t))

(defun initialize ()
  "Sign in to the server: the revision, the capabilities and the client's name.
A server that answers no session and no revision is taken as stateless."
  (multiple-value-bind (object status)
      (send-rpc "initialize"
                (nlk:json-object "protocolVersion" +protocol-version+
                                  "capabilities" (nlk:make-json-object)
                                  "clientInfo" (nlk:json-object "name" "nodecode-qrblox"
                                                                 "version" +client-version+)))
    (unless (and (integerp status) (<= 200 status 299))
      (fail-on-status status object))
    (setf *negotiated*
          (or (nlk:json-value (nlk:json-value object :object "result")
                              :text "protocolVersion")
              +protocol-version+))
    (unless (rpc-error-text object)
      (send-note "notifications/initialized"))
    (setf *initialized* t)
    t))

(defun ensure-session ()
  (unless *initialized* (initialize)))

(defun mcp-call (method &optional params)
  "One MCP method, signing in first and once more after a session the server
forgot => the result object. A refusal, a dead key or a dead network signals
QRBLOX-ERROR, which every verb turns into the model's answer."
  (ensure-session)
  (multiple-value-bind (object status) (send-rpc method params)
    (when (equal status 404)
      (reset-session)
      (ensure-session)
      (multiple-value-bind (again again-status) (send-rpc method params)
        (setf object again status again-status)))
    (unless (and (integerp status) (<= 200 status 299))
      (fail-on-status status object))
    (let ((reason (rpc-error-text object)))
      (when reason
        (fail "Qrblox refused ~a: ~a" method reason)))
    (or (nlk:json-value object :object "result")
        (nlk:make-json-object))))

;;; --- what one tools/call answers -----------------------------------------------------

(defun content-text (result)
  "The text parts of one tools/call RESULT, joined by a newline."
  (with-output-to-string (out)
    (let ((first t))
      (loop for part across (nlk:json-array result "content")
            when (equal "text" (nlk:json-value part :text "type"))
              do (let ((text (nlk:json-value part :string "text")))
                   (when text
                     (unless first (terpri out))
                     (setf first nil)
                     (write-string text out)))))))

(defun render-result (result)
  "One tools/call RESULT as the model reads it => (values TEXT ERROR-P)."
  (let* ((structured (nlk:json-value result :object "structuredContent"))
         (errorp (and (nlk:json-value result :boolean "isError") t))
         (text (or (and structured (nlk:encode-json-object structured))
                   (content-text result))))
    (values (if (and text (plusp (length text))) text (nlk:encode-json-object result))
            errorp)))

(defun call-tool-raw (tool &optional arguments)
  "One tools/call of TOOL => the result object."
  (mcp-call "tools/call"
             (nlk:json-object "name" tool
                               "arguments" (or arguments (nlk:make-json-object)))))

(defun tool-answer (tool &optional arguments)
  "One server tool call as the model reads it: its text, clipped, or the
failure the server reported."
  (multiple-value-bind (text errorp) (render-result (call-tool-raw tool arguments))
    (if errorp
        (nle:failure (format nil "ERROR: ~a" text))
        (clip text (setting :max-chars)))))

(defun list-tools ()
  "The server's own tool list, one line each: the answer of qrblox_call with
no tool named."
  (let ((result (mcp-call "tools/list" nil)))
    (with-output-to-string (out)
      (format out "The tools Qrblox's server offers now:~%")
      (loop for entry across (nlk:json-array result "tools")
            for name = (nlk:json-value entry :text "name")
            when name
              do (format out "  ~a~@[ - ~a~]~%"
                         name
                         (let ((description (nlk:json-value entry :string "description")))
                           (and description (nlk:one-line description :cap 200)))))
      (format out "Call any of them by name with qrblox_call."))))

;;; --- arguments ---------------------------------------------------------------------

(defun argument (arguments key)
  (nlk:json-value arguments :text key))

(defun require-argument (arguments key)
  (or (argument arguments key)
      (error "qrblox: ~a is required" key)))

(defun spread (payload arguments)
  "PAYLOAD with every present member of ARGUMENTS' own `arguments' object
added, and none of PAYLOAD's keys overwritten: the escape hatch for a member
these tools do not name."
  (let ((extra (nlk:json-value arguments :object "arguments")))
    (when (hash-table-p extra)
      (loop for key being the hash-keys of extra using (hash-value value)
            do (unless (gethash key payload)
                 (setf (gethash key payload) value))))
    payload))

(defun sent (arguments)
  "What a call that names no member of its own sends: its `arguments' object."
  (spread (nlk:json-object) arguments))

;;; --- the seven tools ----------------------------------------------------------------

(defparameter +ping-schema+
  (list :description
        (concatenate 'string
                     "Check that Qrblox's MCP server is reachable and that the key in use "
                     "belongs to a workspace. Call it first: its answer names what the key "
                     "reaches, and an ERROR from it is the network, the server or the key.")
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:json-object))))

(defun ping (arguments)
  "qrblox_ping: qrblox.ping, the health and identity call."
  (answering (tool-answer "qrblox.ping" (sent arguments))))

(defparameter +codes-schema+
  (list :description
        (concatenate 'string
                     "List the recent QR codes on the merchant account: what is already "
                     "there, with the ids and the addresses they open. Read it before "
                     "drafting a second code for the same address. `arguments` is passed "
                     "through for a filter these tools do not name.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:json-object
                       "arguments" (nlk:json-object "type" "object"
                                                      "description" "extra members the server documents, sent as-is")))))

(defun codes (arguments)
  "qrblox_codes: qrblox.qr.list."
  (answering (tool-answer "qrblox.qr.list" (sent arguments))))

(defparameter +create-schema+
  (list :description
        (concatenate 'string
                     "Draft a QR code on the merchant account. destination_url is the "
                     "address the code opens, title what the merchant calls it; the answer "
                     "carries the code's id and url, which is what to read back to the "
                     "person. It is a draft: publishing stays in the dashboard. `arguments` "
                     "is passed through as-is, for a member these tools do not name (an "
                     "offer, a campaign, a style).")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:json-object
                       "title" (nlk:json-object "type" "string"
                                                  "description" "what the merchant calls this code")
                       "destination_url" (nlk:json-object "type" "string"
                                                          "description" "the address the code opens, https://...")
                       "arguments" (nlk:json-object "type" "object"
                                                      "description" "extra members the server documents, sent as-is")))))

(defun create-code (arguments)
  "qrblox_create: qrblox.qr.create, draft-first as the server is."
  (answering
    (let* ((title (argument arguments "title"))
           (destination (argument arguments "destination_url"))
           (payload (nlk:json-object)))
      (when title (setf (gethash "title" payload) title))
      (when destination (setf (gethash "destinationUrl" payload) destination))
      (spread payload arguments)
      (when (zerop (hash-table-count payload))
        (return-from create-code
          (refuse "qrblox_create needs something to draft: destination_url (the address the code opens) and a title, or an `arguments` member the server documents")))
      (tool-answer "qrblox.qr.create" payload))))

(defparameter +audio-schema+
  (list :description
        (concatenate 'string
                     "Draft an audio code: a near-ultrasonic payload a phone can hear, kept "
                     "beside the merchant's QR codes. title names it; the payload itself and "
                     "any other member the server documents go in `arguments`.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:json-object
                       "title" (nlk:json-object "type" "string"
                                                  "description" "what the merchant calls this code")
                       "arguments" (nlk:json-object "type" "object"
                                                      "description" "the payload member, and any other the server documents, sent as-is")))))

(defun audio (arguments)
  "qrblox_audio: qrblox.audio.create."
  (answering
    (let ((payload (nlk:json-object))
          (title (argument arguments "title")))
      (when title (setf (gethash "title" payload) title))
      (spread payload arguments)
      (when (zerop (hash-table-count payload))
        (return-from audio
          (refuse "qrblox_audio needs a title, or an `arguments` member the server documents")))
      (tool-answer "qrblox.audio.create" payload))))

(defparameter +menu-schema+
  (list :description
        (concatenate 'string
                     "The merchant's menus. op list answers the menus on the account; op "
                     "create drafts a new one and takes title; op add_item adds an item to the "
                     "menu menu_id names, and takes the item in `arguments`. Every other "
                     "member the server documents goes in `arguments`, as-is.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:json-object
                       "op" (nlk:json-object "type" "string"
                                               "enum" (vector "list" "create" "add_item")
                                               "description" "list, create or add_item")
                       "menu_id" (nlk:json-object "type" "string"
                                                    "description" "the menu's id, for add_item")
                       "title" (nlk:json-object "type" "string"
                                                  "description" "the menu's name, for create")
                       "arguments" (nlk:json-object "type" "object"
                                                      "description" "the item to add, and any other member the server documents, sent as-is"))
         "required" (vector "op"))))

(defun menu (arguments)
  "qrblox_menu: qrblox.menu.list, qrblox.menu.create or qrblox.menu.add_item."
  (answering
    (let* ((op (string-downcase (or (argument arguments "op") "")))
           (payload (nlk:json-object))
           (menu-id (argument arguments "menu_id"))
           (title (argument arguments "title")))
      (cond
        ((member op '("" "list") :test #'string=)
         (tool-answer "qrblox.menu.list" (sent arguments)))
        ((string= op "create")
         (when title (setf (gethash "title" payload) title))
         (spread payload arguments)
         (when (zerop (hash-table-count payload))
           (return-from menu (refuse "op create needs a title, or an `arguments` member the server documents")))
         (tool-answer "qrblox.menu.create" payload))
        ((string= op "add_item")
         (unless menu-id
           (return-from menu (refuse "op add_item needs menu_id: the menu the item joins, from qrblox_menu op list")))
         (setf (gethash "menuId" payload) menu-id)
         (when title (setf (gethash "title" payload) title))
         (spread payload arguments)
         (tool-answer "qrblox.menu.add_item" payload))
        (t (refuse "op is list, create or add_item, not ~a" op))))))

(defparameter +scans-schema+
  (list :description
        (concatenate 'string
                     "Scan analytics: the total and the by-day series the account has seen. "
                     "`arguments` is passed through as-is, for a range or a code filter the "
                     "server documents.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:json-object
                       "arguments" (nlk:json-object "type" "object"
                                                      "description" "extra members the server documents, sent as-is")))))

(defun scans (arguments)
  "qrblox_scans: qrblox.analytics.scan_summary."
  (answering (tool-answer "qrblox.analytics.scan_summary" (sent arguments))))

(defparameter +call-schema+
  (list :description
        (concatenate 'string
                     "Call any tool Qrblox's server offers, by the name its list gives "
                     "(qrblox.challenge.create for scan streaks and loyalty campaigns, "
                     "qrblox.merchant.get_profile for the workspace, the voice-agent "
                     "directory lookups). Named no tool, it answers the server's current "
                     "tool list, which is the way to find one.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:json-object
                       "tool" (nlk:json-object "type" "string"
                                                 "description" "the server tool's name, such as qrblox.challenge.create; empty lists them")
                       "arguments" (nlk:json-object "type" "object"
                                                      "description" "that tool's arguments, sent as-is")))))

(defun call (arguments)
  "qrblox_call: any tool the server lists, or the list itself."
  (answering
    (let ((tool (argument arguments "tool")))
      (if tool
          (tool-answer tool (spread (nlk:json-object) arguments))
          (list-tools)))))

;;; --- the operator's command ----------------------------------------------------------

(defun run-command (args session-id)
  "One /qrblox invocation: with no argument the codes on the account, with
ping the health call, with key where the key in use comes from."
  (declare (ignore session-id))
  (let ((wanted (nlk:trimmed (or args ""))))
    (cond ((zerop (length wanted)) (codes nil))
          ((member wanted '("ping" "health") :test #'string-equal) (ping nil))
          ((member wanted '("key" "login") :test #'string-equal)
           (format nil "Qrblox: the key in use comes from ~a. ~
Paste a merchant key (sk-qrblox-...) into api_key in the qrblox section of /settings, ~
or name the variable holding it in api_key_env, then (restart-cells \"nodecode-qrblox\")."
                   (key-source)))
          (t (format nil "/qrblox shows the recent codes; /qrblox ping checks the server; ~
/qrblox key says where the key came from. \"~a\" is none of those." wanted)))))

;;; --- the cell --------------------------------------------------------------------------

(nle:define-cell qrblox
  (:section ("qrblox")
    (:guide "mint a merchant key at create.qrblox.com/settings/api-keys, then paste it into api_key (or name the variable holding it in api_key_env); server_url is Qrblox's MCP endpoint unless you were given another; then /qrblox shows the codes on the account")
    ("api_key" :secret
               :doc "the merchant API key, sk-qrblox-...; empty reads api_key_file, then api_key_env")
    ("api_key_env" :env :default "QRBLOX_API_KEY"
                   :doc "the environment variable holding the key, read when api_key and the file are empty")
    ("api_key_file" :path
                    :doc "a file whose first line is the key, instead of the api_key member")
    ("server_url" :string :default "https://api.qrblox.com/mcp/v1"
                  :doc "Qrblox's MCP endpoint, https")
    ("timeout_seconds" :integer :default 30 :min 5
                       :doc "one call's ceiling")
    ("max_chars" :integer :default 6000 :min 500
                 :doc "how much of one answer the model reads"))
  (:settings #'read-settings)
  (:tool "qrblox_ping" +ping-schema+ #'ping)
  (:tool "qrblox_codes" +codes-schema+ #'codes)
  (:tool "qrblox_create" +create-schema+ #'create-code)
  (:tool "qrblox_audio" +audio-schema+ #'audio)
  (:tool "qrblox_menu" +menu-schema+ #'menu)
  (:tool "qrblox_scans" +scans-schema+ #'scans)
  (:tool "qrblox_call" +call-schema+ #'call)
  (:help :qrblox
         "qrblox: draft QR codes, audio codes and menus on a merchant account, and read their scans - (help :qrblox)"
         *primer*)
  (:command "qrblox" 'run-command
            :description "Qrblox: the recent codes, or ping the server"
            :argument-hint "[ping|key]"
            :session nil)
  (:start (lambda ()
            (check-config)
            (reset-session)
            (nle:on-stop #'reset-session))))
