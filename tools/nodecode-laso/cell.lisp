;;;; cell.lisp --- Laso Finance's tools, the device sign-in, the /laso command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was 66 Python tools generated from the Laso API contract, one
;;;; pre-tool-call hook that asked the human to approve the calls that move
;;;; money, and a section of guidance for the model. Here the 66 arrive as
;;;; catalog.lisp's data, reached through two tools: LASO_TOOLS searches the
;;;; catalog, LASO_CALL runs one entry. The guidance is +PRIMER+, which the
;;;; model reads with (help :laso). The approval hook becomes CONFIRM: a tool
;;;; the original gated refuses to run until the model passes confirm true,
;;;; which it can only honestly do after the operator agreed.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "laso": {"agent_name": "Nodecode", "api_key": "lasoak_...",
;;;;            "api_key_env": "LASO_API_KEY", "base_url": "https://laso.finance",
;;;;            "timeout_seconds": 120}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-laso)

(defparameter +base-url+ "https://laso.finance"
  "The Laso host every request goes to, unless the section names another.")

(defparameter +client-id-path+ "/oauth/clients/hermes.json"
  "The client metadata document Laso hosts for this integration, so its approval page names it.")

(defparameter +user-agent+ "Nodecode (nodecode-laso cell)"
  "What the two OAuth exchanges and every tool call announce themselves as.")

(defparameter +key-file-name+ "laso-api-key"
  "Where an approved key is kept under the home: one line, 0600, dropped when Laso rejects it.")

(defparameter *pending* nil
  "The device-code sign-in waiting for the operator: a plist, in this process only.")

(defparameter *fresh-key* nil
  "The key this process just received, used even before the file write is read back.")

;;; --- small helpers --------------------------------------------------------------

(defun nonempty (value)
  "VALUE trimmed when it is a string with anything in it, else NIL."
  (when (stringp value)
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
      (and (plusp (length trimmed)) trimmed))))

(defun clip (text limit)
  "TEXT cut to LIMIT characters."
  (let ((text (or text "")))
    (subseq text 0 (min (length text) limit))))

(defun jv (object type &rest path)
  "PATH read out of the JSON OBJECT as TYPE, NIL when it is not there."
  (ignore-errors (apply #'nlk:json-value object type path)))

(defun parse-int (text fallback)
  "TEXT as an integer, or FALLBACK."
  (or (and (integerp text) text)
      (and (stringp text) (parse-integer text :junk-allowed t))
      fallback))

(defun url-encode (text)
  "TEXT escaped for a form body, byte by byte over its UTF-8 octets."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets text :external-format :utf-8)
          do (let ((char (code-char octet)))
               (if (or (and char (alphanumericp char)) (find char "-._~"))
                   (write-char char out)
                   (format out "%~2,'0X" octet))))))

(defun form-body (fields)
  "FIELDS, an alist of (NAME . VALUE), as x-www-form-urlencoded text."
  (format nil "~{~a~^&~}"
          (loop for (name . value) in fields
                collect (format nil "~a=~a" (url-encode name) (url-encode value)))))

;;; --- the key the cell runs on -----------------------------------------------------

(defun key-file ()
  "Where the approved key is kept, this home's own."
  (nlk:home +key-file-name+))

(defun saved-key ()
  "The key kept from an approved sign-in, or NIL."
  (let ((path (key-file)))
    (and (probe-file path) (nonempty (ignore-errors (uiop:read-file-string path))))))

(defun key-variable ()
  "The environment variable a key is read from."
  (or (nonempty (setting :api-key-env)) "LASO_API_KEY"))

(defun api-key ()
  "The key every call carries: this process's, the section's, the environment's, then the file's."
  (or *fresh-key*
      (nonempty (setting :api-key))
      (nonempty (uiop:getenv (key-variable)))
      (saved-key)))

(defun keep-key (key)
  "Remember KEY for this process and keep it for the next one."
  (setf *fresh-key* key)
  (let ((path (key-file)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (write-string key out))
    #+sb-posix (ignore-errors (sb-posix:chmod (namestring path) #o600))
    key))

(defun forget-key ()
  "Drop a key Laso rejected, so the next call signs in again."
  (setf *fresh-key* nil)
  (let ((path (key-file)))
    (when (probe-file path) (ignore-errors (delete-file path)))))

;;; --- the host the key may travel to ------------------------------------------------

(defun root ()
  "The Laso root this cell talks to."
  (string-right-trim "/" (or (nonempty (setting :base-url)) +base-url+)))

(defun check-config ()
  "Refuse a start that could not reach Laso, naming the member to set."
  (unless (nonempty (setting :agent-name))
    (error 'nlk:config-refusal
           :detail (format nil "laso: set \"agent_name\" in the laso section: it is what the Laso ~
dashboard calls this agent, and what laso_call passes to announce_connection")))
  (let ((given (root)))
    (unless (uiop:string-prefix-p "https://" given)
      (error 'nlk:config-refusal
             :detail (format nil "laso: base_url ~a is not https; use ~a" given +base-url+)))
    (let ((rest (subseq given (length "https://"))))
      (when (or (zerop (length rest))
                (find #\@ rest) (find #\/ rest) (find #\: rest))
        (error 'nlk:config-refusal
               :detail (format nil "laso: base_url ~a must be one host, with no path, port or ~
credentials; use ~a" given +base-url+))))))

;;; --- the device sign-in (RFC 8628) --------------------------------------------------

(defun post-form (path fields)
  "One x-www-form-urlencoded POST to PATH on the Laso host. => (values OBJECT STATUS)."
  (multiple-value-bind (body status)
      (nlk:http :post (concatenate 'string (root) path)
                :headers (list (cons "content-type" "application/x-www-form-urlencoded")
                               (cons "accept" "application/json")
                               (cons "user-agent" +user-agent+))
                :content (form-body fields)
                :timeout 30 :connect-timeout 10)
    (values (or (and (stringp body)
                     (plusp (length (string-trim " " body)))
                     (ignore-errors (nlk:decode-json body)))
                (nlk:json-object))
            status)))

(defun client-id ()
  "The client the sign-in is approved for."
  (concatenate 'string (root) +client-id-path+))

(defun not-connected (pending note)
  "The answer that hands the operator the link and the code."
  (format nil "~a Open ~a and approve. Check that the page shows the code ~a. Once the operator ~
says they approved, call this tool again to finish connecting."
          note (getf pending :uri) (getf pending :user-code)))

(defun device-start ()
  "Begin a sign-in: a link and a short code for the operator."
  (multiple-value-bind (body status) (post-form "/oauth/device_authorization"
                                                 (list (cons "client_id" (client-id))))
    (let ((code (jv body :text "device_code")))
      (if (and (integerp status) (= status 200) (nonempty code))
          (let ((pending (list :device-code code
                               :user-code (jv body :text "user_code")
                               :uri (jv body :text "verification_uri_complete")
                               :interval (parse-int (jv body :any "interval") 5)
                               :expires-at (+ (get-universal-time)
                                              (parse-int (jv body :any "expires_in") 600))
                               :last-polled (get-universal-time))))
            (setf *pending* pending)
            (not-connected pending "To connect Laso Finance,"))
          (format nil "ERROR: could not start the Laso sign-in: ~a"
                  (or (nonempty (jv body :text "error_description"))
                      (format nil "HTTP ~a" status)))))))

(defun device-poll (pending)
  "One poll of a sign-in waiting for the operator."
  (multiple-value-bind (body status)
      (post-form "/oauth/token"
                 (list (cons "grant_type" "urn:ietf:params:oauth:grant-type:device_code")
                       (cons "device_code" (getf pending :device-code))
                       (cons "client_id" (client-id))))
    (let ((token (jv body :text "access_token")))
      (cond
        ((and (integerp status) (= status 200) (nonempty token))
         (setf *pending* nil)
         (keep-key token)
         nil)
        ((member (jv body :text "error") '("authorization_pending" "slow_down") :test #'string=)
         (setf (getf pending :last-polled) (get-universal-time))
         (when (string= (jv body :text "error") "slow_down")
           (incf (getf pending :interval) 5))
         (setf *pending* pending)
         (not-connected pending "Still waiting for approval."))
        (t
         (setf *pending* nil)
         (if (string= (jv body :text "error") "access_denied")
             "The Laso connection was declined. Ask again to connect."
             (device-start)))))))

(defun connect ()
  "NIL once a key is available, else the answer that tells the model what to do."
  (if (api-key)
      nil
      (let ((pending *pending*)
            (now (get-universal-time)))
        (cond ((or (null pending) (<= (getf pending :expires-at) now))
               (device-start))
              ((< (- now (getf pending :last-polled)) (getf pending :interval))
               (not-connected pending "Still waiting for approval."))
              (t (device-poll pending))))))

;;; --- one tool call at the Laso MCP server -------------------------------------------

(defun reply-object (text)
  "TEXT as the JSON-RPC reply it is, whether the server sent JSON or an event stream."
  (let ((text (or text "")))
    (if (search "data:" text)
        (last-sse-message text)
        (or (ignore-errors (nlk:decode-json text)) (nlk:json-object)))))

(defun last-sse-message (text)
  "The last `message' event of a Streamable HTTP response sent as server-sent events."
  (let ((reply nil)
        (data '()))
    (dolist (line (append (uiop:split-string text :separator '(#\Newline)) (list "")))
      (cond ((uiop:string-prefix-p "data:" line)
             (push (string-left-trim '(#\Space) (subseq line 5)) data))
            ((and (zerop (length line)) data)
             (let ((joined (format nil "~{~a~^~%~}" (reverse data))))
               (setf reply (or (ignore-errors (nlk:decode-json joined)) reply)
                     data '())))))
    (or reply (nlk:json-object))))

(defun answer-text (result)
  "The text parts of a tool result's content, joined."
  (let ((content (jv result :array "content")))
    (format nil "~{~a~^~%~}"
            (loop for part across (if (vectorp content) content
                                      (coerce (or content '()) 'vector))
                  when (string= (or (jv part :text "type") "") "text")
                    collect (or (jv part :text "text") "")))))

(defun rpc-call (name arguments)
  "Tool NAME, at the server, with ARGUMENTS; the text the model reads back."
  (let* ((limit (setting :timeout-seconds))
         (body (nlk:json-object "jsonrpc" "2.0" "id" 1 "method" "tools/call"
                                 "params" (nlk:json-object "name" name
                                                            "arguments" arguments))))
    (multiple-value-bind (text status)
        (nlk:http :post (concatenate 'string (root) "/mcp")
                  :headers (list (cons "authorization" (format nil "Bearer ~a" (api-key)))
                                 (cons "content-type" "application/json")
                                 (cons "accept" "application/json, text/event-stream")
                                 (cons "user-agent" +user-agent+))
                  :content (nlk:encode-json-object body)
                  :timeout limit :connect-timeout (min 10 limit))
      (when (and (integerp status) (= status 401))
        (forget-key)
        (fail "ERROR: Laso rejected the saved key (HTTP 401) and it was dropped; call again to ~
sign in with a fresh link and code."))
      (when (and (integerp status) (>= status 300))
        (fail "ERROR: Laso answered HTTP ~d: ~a" status (clip text 300)))
      (let ((reply (reply-object text)))
        (when (jv reply :object "error")
          (fail "ERROR: ~a" (or (nonempty (jv reply :object "error" "message")) "unknown error")))
        (let* ((result (or (jv reply :object "result") (nlk:json-object)))
               (answer (answer-text result)))
          (when (jv result :boolean "isError")
            (fail "ERROR: ~a" answer))
          (if (plusp (length answer))
              answer
              "The tool answered with nothing."))))))

;;; --- the catalog, as data the model reads --------------------------------------------

(defun catalog-entry (name)
  "NAME's row of the catalog, or NIL."
  (assoc (string-downcase (or name "")) +catalog+ :test #'string=))

(defun approval-p (name)
  "Does NAME move money or change the account?"
  (and (catalog-entry name) (second (catalog-entry name))))

(defun matches-p (row query)
  "Does ROW answer QUERY? An empty query matches every row."
  (or (zerop (length query))
      (search query (first row) :test #'char-equal)
      (search query (third row) :test #'char-equal)))

(defun catalog-line (row)
  "One catalog row as the model reads it."
  (format nil "  ~a - ~a~@[ (needs confirm)~]" (first row) (third row) (second row)))

(defun summarize (arguments)
  "The scalar arguments of a call, as one readable line, for a human to judge."
  (let ((parts '()))
    (when (hash-table-p arguments)
      (maphash (lambda (key value)
                 (when (and (or (stringp value) (numberp value))
                            (not (and (stringp value) (zerop (length value)))))
                   (push (format nil "~a=~a" key (if (stringp value) value value)) parts)))
               arguments))
    (clip (format nil "~{~a~^, ~}" (nreverse parts)) 300)))

;;; --- the two tools ----------------------------------------------------------------

(nlk:define-contract tools-lookup
  (:doc "Read the Laso Finance catalog: the 66 tools this cell reaches on the user's own Laso
account, each with its one-line title. Search it by name or by words of what you want to do (for
example \"gift card\", \"balance\", \"payout\"). Answers the rows that match, and whether the account
is connected yet. Run one of them with laso_call, with the name exactly as written here.")
  ("query" :string
   :doc "words to match against a tool's name and title; leave it out to list the whole catalog"))

(nlk:define-contract call-tool
  (:doc "Run one Laso Finance tool against the user's own Laso account: prepaid cards, gift cards,
push-to-card, Venmo/PayPal and bank payouts, balances, and the agent wallet, all paid in USDC.
Name the tool as laso_tools writes it, without a laso_ prefix, and pass its arguments as a JSON
object. A tool that moves money or changes the account refuses until you pass confirm true: say what
it will do, in the operator's terms, get their agreement, then call once with confirm true. The
first call with no key started hands back a link and a code for the operator; open nothing yourself.
Call the same tool again after they say they approved.")
  ("tool" :text :required t
   :doc "the tool's name, exactly as laso_tools writes it: order_card, get_account_balance, send_payment")
  ("arguments" :object
   :doc "the tool's arguments as one JSON object; laso_tools names the tool, and the server's refusal names an argument it wanted")
  ("confirm" :boolean
   :doc "true once the operator agreed to a call that moves money or changes the account; the 19 tools marked (needs confirm) refuse without it"))

(defun tool-laso-tools (call)
  "The catalog, searched, and the connection state."
  (let* ((query (string-downcase (or (nonempty (tools-lookup-query call)) "")))
         (rows (remove-if-not (lambda (row) (matches-p row query)) +catalog+))
         (key (api-key)))
    (format nil "laso: ~a~a~%~{~a~%~}"
          (if key "connected to Laso; " "not connected yet: the first laso_call starts the sign-in; ")
          (format nil "~d of ~d tools match~a, ~d of them need confirm"
                  (length rows) (length +catalog+)
                  (if (zerop (length query)) "" (format nil " \"~a\"" query))
                  (count-if #'second rows))
          (loop for row in rows collect (catalog-line row)))))

(defun tool-laso-call (call)
  "One catalog tool, at the Laso MCP server."
  (let* ((name (string-downcase (or (nonempty (call-tool-tool call)) "")))
         (arguments (or (call-tool-arguments call) (nlk:json-object)))
         (entry (catalog-entry name)))
    (unless entry
      (fail "ERROR: no Laso tool named ~s; laso_tools lists the 66 names, without the laso_ prefix."
            (call-tool-tool call)))
    (when (and (second entry) (not (call-tool-confirm call)))
      (fail "ERROR: ~a moves money or changes the account: ~a.~@[ Arguments: ~a.~] Tell the ~
operator what it will do in their terms, wait for their agreement, then call it once with confirm ~
true." name (third entry) (summarize arguments)))
    (let ((had-key (api-key))
          (said (connect)))
      (cond (said)
            ((not had-key) "Laso is now connected. Call the tool again to run it.")
            (t (rpc-call name arguments))))))

;;; --- the operator's command ---------------------------------------------------------

(defun status-text ()
  "What this cell is connected to, without ever printing the key."
  (format nil "laso: ~a~%agent name: ~a~%key: ~a~%tools: ~d, ~d of them need confirm~%~@[~a~]"
          (root)
          (or (nonempty (setting :agent-name)) "(unset)")
          (cond ((nonempty (setting :api-key)) "set in the section")
                ((nonempty (uiop:getenv (key-variable))) (format nil "read from ~a" (key-variable)))
                ((saved-key) "kept from an approved sign-in")
                (t "none yet: /laso signin starts one"))
          (length +catalog+) +approval-count+
          (and *pending* (format nil "a sign-in is waiting: ~a, code ~a"
                                 (getf *pending* :uri) (getf *pending* :user-code)))))

(defun run-command (args session-id)
  "One /laso invocation: no argument reports the connection, `signin' starts or
polls one, `signout' drops the key."
  (declare (ignore session-id))
  (let ((word (string-downcase (nlk:trimmed (or args "")))))
    (cond ((member word '("signout" "out") :test #'string=)
           (forget-key)
           "laso: signed out; the key was dropped. /laso signin connects again.")
          ((member word '("signin" "in" "connect") :test #'string=)
           (or (connect) "laso: connected. laso_tools lists what you can do now."))
          (t (status-text)))))

;;; --- the declaration ---------------------------------------------------------------

(defparameter +primer+
  "Laso Finance lets the agent move real dollars from the operator's own Laso account: prepaid cards,
gift cards, push-to-card, Venmo/PayPal payouts and bank payments, funded with USDC. Two tools reach
all of it on laso.finance.
  laso_tools        the catalog: 66 tools, each a name and a line. Search it (\"gift card\",
                    \"balance\", \"payout\") instead of guessing.
  laso_call         run one: tool (its name), arguments (a JSON object), confirm.
FIRST CALL: run announce_connection, once per setup. It names the agent in the operator's dashboard,
starts an activity session and answers the agent wallet's funding state, so it doubles as a setup
check. Pass agent_name (the operator's name for this agent) and a short message.
Sign-in: until the operator has connected their Laso account, the first call answers a link and a
short code. Give both to the operator, exactly as returned, in the next reply, and open nothing
yourself. When they say they approved, call the same tool again: it finishes the sign-in and runs.
Never ask the operator for their key or pass one as an argument.
Approval: 19 of the 66 tools move money, withdraw, delete a recipient or mint a key. Each refuses
without confirm true. Before one, say what it will do in the operator's terms (amount, recipient,
product), get their agreement, then call once with confirm true. laso_buy_with_saved_card is the
one the original did not prompt for: the Laso page asks the holder for a passkey on their own device.
Paid routes: a few tools settle a USDC price from the agent wallet; their description on the server
marks them PAID ROUTE. Before the first one, call get_agent_wallet (create_agent_wallet when there is none) and fund the Solana
address it answers with USDC. What each tool's arguments are is in its own description, on the
server: laso_call forwards one and the refusal names it. Everything else - fees, limits, webhooks,
identity checks, refunds, error handling - is search_docs."
  "What (help :laso) answers while the cell runs: the original's guidance, as one topic.")

(defun start ()
  "Refuse a section the key could not travel on, and forget any half-finished sign-in."
  (check-config)
  (setf *pending* nil *fresh-key* nil)
  (nle:on-stop (lambda () (setf *pending* nil *fresh-key* nil))))

(nle:define-cell laso
  (:section ("laso")
    (:guide "the agent signs itself in from chat with a link and a code, so no key is needed; agent_name is how the Laso dashboard names it, and the only member to set; put a lasoak_ key in api_key (or name the variable in api_key_env) to skip the sign-in; base_url only for another Laso host")
    ("agent_name" :string :required t
     :doc "what the Laso dashboard calls this agent, at most 60 characters")
    ("api_key" :secret
     :doc "a lasoak_ key from the Laso agent dashboard; sent to laso.finance and nowhere else")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (LASO_API_KEY is read either way)")
    ("base_url" :string
     :doc "the Laso host: https://laso.finance unless you use another")
    ("timeout_seconds" :integer :default 120 :min 5
     :doc "one tool call's ceiling; paid tools settle on-chain before they answer"))
  (:help :laso
         "laso: spend USDC on prepaid cards, gift cards and payouts, from chat - (help :laso)"
         +primer+)
  (:command "laso" 'run-command
            :description "Laso Finance: the connection, sign in, or sign out"
            :argument-hint "SIGNIN|SIGNOUT"
            :session nil)
  (:tool "laso_tools" (nlk:contract 'tools-lookup) #'tool-laso-tools)
  (:tool "laso_call" (nlk:contract 'call-tool) #'tool-laso-call)
  (:start #'start))
