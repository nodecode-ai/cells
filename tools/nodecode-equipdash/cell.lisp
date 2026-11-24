;;;; cell.lisp --- the skill's rules, the section, the wire, three tools and one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin had no code: an MCP server declaration
;;;; (https://app.equipdash.com/mcp/v1, Streamable HTTP) and one skill that
;;;; taught a model how to run a rental or activity shop with it. This cell
;;;; speaks that server's JSON-RPC itself -- initialize, the Mcp-Session-Id,
;;;; tools/list, tools/call, an answer that is either a JSON object or a
;;;; one-event stream -- so every tool the account's plan and permissions offer
;;;; is reachable by name, the skill is the manual topic (help :equipdash), and
;;;; the two-step write its server asks for (a preview, then the person's yes)
;;;; is one flag on the cell's own call tool.
;;;;
;;;; The credential is an EquipDash API key with the mcp:access scope
;;;; (Settings > API & Integrations), pasted into the section or named by an
;;;; environment variable or a file. A person who signed in to the same server
;;;; in /mcp needs nothing: the shipped mcp cell keeps that token, and this
;;;; cell reuses it.
;;;;
;;;; Config, a top-level key of its own:
;;;;   "equipdash": {"server_url": "https://app.equipdash.com/mcp/v1",
;;;;                 "token_env": "EQUIPDASH_API_KEY",
;;;;                 "timeout_seconds": 30, "max_chars": 12000}

(in-package #:nodecode-equipdash)

;;; --- the manual -----------------------------------------------------------------

(defparameter +primer+
  "EquipDash is booking and rental software for businesses that rent out equipment and run
activities, lessons and classes. This cell reaches the shop's own EquipDash MCP server:
equipdash_tools lists every tool the account offers -- names follow the shop's plan and the
signed-in person's permissions -- with what each does, whether it is read-only and which
arguments it takes; equipdash_call runs one by name with that tool's own arguments object;
equipdash_day answers the day as the Today screen shows it, in one call.

Rules that keep the shop's data right
- Dates are in the shop's own timezone. Send YYYY-MM-DD, or YYYY-MM-DD HH:MM:SS when the
  person named a time. When they named only days, send days: the shop's usual pickup and
  return times are filled in and the answer says which times were used. Never convert to UTC.
- Money is in the shop's currency. Repeat amounts as the tools return them and add no symbol.
- Ids come from this session. Get a booking, customer, item or experience id from a list or
  search tool before using it; a booking's number goes to get_booking as reference.
- Preview, ask, then do. Most tools that change something answer a preview first: show the
  price, the refund or the email that goes out, wait for a clear yes, then call equipdash_call
  again with confirmed true, which sends user_confirmed true (or confirmed true on
  send_payment_link and process_refund). record_payment saves straight away, so check the
  amount with the person before calling it. Never confirm on the person's behalf.
- A refusal is an answer. An error naming a plan means the business's plan lacks the feature;
  a permission error means the signed-in person cannot do it in EquipDash either. Say which,
  and do not retry.

What is on today
get_today is the day as the Today screen shows it: sessions with booked places and check-ins,
items to get out by time, rentals to pick up, out now and due back, anything waiting on the
owner. Pass date for another day, section out_now or people to narrow it. equipdash_day
answers get_today and get_needs_you together. get_needs_you lists what needs the owner now
(booking requests, departures with no guide, late returns, unpaid bookings, changes waiting for
an OK); get_late_pickups and get_late_returns are the overdue lists alone;
get_bookings_missing_waivers lists unsigned waivers, and send_waiver_reminders chases them
after the person agrees.

Finding a booking
search_customers takes a name, organization, email or phone, then get_customer. list_bookings
takes date_from, date_to, status, booking_type (rental, experience or store), customer_search
or unpaid true, ten at a time: pass next_offset back as offset. get_booking takes booking_id
or reference.

Is it free, and what does it cost
list_products finds an item, check_product_availability takes dates and quantity, and
calculate_rental_price prices one or more items over a period with the shop's own pricing.
list_experiences then check_experience_availability takes experience_id, date and participants;
list_items_by_date answers every experience with its open slots on one date. Quote the price
the tool returned, and offer the nearest free option when a slot is full.

Making or changing a booking
1. search_customers, or create_customer. 2. create_booking without confirmed answers the
preview (items, times, price, anything still missing): show it, get a yes, then call again with
confirmed true. save_as_draft saves without emailing anyone; confirm_booking confirms a draft
later. 3. move_booking_dates previews first and emails the customer and the shop once it goes
through; update_booking edits; add_booking_note leaves staff a note. 4. cancel_booking without
confirmed answers the cancellation policy and the refund that applies: read it out before
confirming.

Payments
get_amount_due is what is left; get_payment_transactions is every payment and refund on a
booking. send_payment_link emails the customer a link for the balance or a stated part of it.
record_payment takes cash, bank transfer or a card in person and saves straight away.
process_refund refunds to the card. get_revenue_summary takes a period such as this_week, and
compare true adds the period before.

When you are stuck
search_help_centre searches EquipDash's own help articles and returns links: answer from the
article and give the person the link.

Done when every date and amount you report came from a tool result in this session, nothing was
created, moved, cancelled, charged, refunded or emailed without the person's yes to the preview,
and any refusal was passed on with its reason. The operator reads the day with /equipdash."
  "What (help :equipdash) answers while the cell runs: the original's skill, as one manual page.")

(defparameter +default-url+ "https://app.equipdash.com/mcp/v1"
  "EquipDash's hosted MCP server: the one host this cell talks to.")

(defparameter +default-token-env+ "EQUIPDASH_API_KEY"
  "The environment variable a key is read from when the section names none.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client offers at initialize; the server may answer another.")

(defparameter +client-version+ "0.1.0")

(defparameter +own-confirmation+
  '("send_payment_link" "process_refund")
  "The server's tools that spell the person's yes as confirmed, not user_confirmed.")

;;; --- answers, and how a refusal reaches the model ---------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on, as the call's failure."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal or a dead network becomes that text, never an
unhandled error."
  `(handler-case (progn ,@body)
     (equipdash-error (condition)
       (nle:failure (format nil "ERROR: ~a" (equipdash-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: equipdash: ~a"
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

(defun json-member (object key)
  "OBJECT's member KEY => (values VALUE PRESENT-P), a false member counting as present."
  (if (hash-table-p object)
      (gethash key object)
      (values nil nil)))

(defun read-token-file (path)
  "The token in PATH's first non-empty line, or NIL when the file is not there."
  (let ((truename (probe-file path)))
    (when truename
      (handler-case
          (with-open-file (in truename :external-format :utf-8)
            (loop for line = (read-line in nil nil)
                  while line
                  for text = (nonblank line)
                  when text return text))
        (error () nil)))))

(defun url-host (url)
  "URL's host, lowercased, or NIL."
  (let* ((start (or (search "//" url) -1))
         (rest (subseq url (min (length url) (+ start 2))))
         (end (or (position #\/ rest) (length rest))))
    (string-downcase (subseq rest 0 end))))

(defun mcp-token (url)
  "The access token the shipped mcp cell keeps for URL, when it has signed in,
else NIL. That store is its sign-in, not this cell's: it is only read."
  (let ((path (nlk:home "mcp/oauth.json")))
    (when (probe-file path)
      (let* ((host (ignore-errors (url-host url)))
             (store (ignore-errors (nlk:decode-json (uiop:read-file-string path)))))
        (when (and host (hash-table-p store))
          (loop for key being the hash-keys of store using (hash-value entry)
                when (and (stringp key) (search host (string-downcase key)))
                  return (nonblank (nlk:json-value entry :text "tokens" "access_token"))))))))

(defun environment-token (name)
  "The value of the environment variable NAME, or NIL."
  (and name (ignore-errors (uiop:getenv name)) (nonblank (uiop:getenv name))))

(defun read-settings (values table)
  "The declared VALUES, with the key resolved once, here: the section's, the
environment's, a file's, else the sign-in the mcp cell keeps."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :server-url) (or (nonblank (getf values :server-url)) +default-url+)
          (getf plist :timeout-seconds) (or (getf values :timeout-seconds) 30)
          (getf plist :max-chars) (or (getf values :max-chars) 12000))
    (let* ((inline (nonblank (getf values :token)))
           (from-env (environment-token (or (nonblank (getf values :token-env))
                                             +default-token-env+)))
           (file (nonblank (getf values :token-file)))
           (from-file (and file (read-token-file file)))
           (stored (mcp-token (getf plist :server-url))))
      (setf (getf plist :token) (or inline from-env from-file stored))
      (setf (getf plist :token-source)
            (cond (inline "the key in the token member of the equipdash section")
                  (from-env "the environment variable token_env names")
                  (from-file "the file token_file names")
                  (stored "the sign-in the mcp cell keeps for this server")
                  (t "none"))))
    plist))

(defun token-source ()
  "Where the key comes from, in words: never the key itself."
  (or (setting :token-source) "none"))

(defun check-config ()
  "Refuse a section that cannot reach EquipDash, naming the member to set."
  (unless (search "https://" (setting :server-url))
    (error 'nlk:config-refusal
           :detail "equipdash: server_url must be an https address; it defaults to https://app.equipdash.com/mcp/v1"))
  (unless (nonblank (setting :token))
    (error 'nlk:config-refusal
           :detail (format nil "equipdash: no API key. Create one in EquipDash under Settings > API & Integrations with the mcp:access scope, and paste it into the token member of the equipdash section, name it in token_env or token_file, or sign in to ~a once in /mcp (that sign-in is reused); then (restart-cells \"nodecode-equipdash\")"
                            (setting :server-url)))))

;;; --- the wire: MCP over streamable HTTP -------------------------------------------
;; One POST per JSON-RPC message, and an answer that is either the bare object
;; or a stream whose events are data: {...}. The server hands a session id at
;; initialize and wants it back on every request after that; a server that
;; hands none is stateless, and a 404 means the session it gave is gone.

(defvar *session-id* nil
  "The Mcp-Session-Id this image was given at initialize, or NIL.")

(defvar *negotiated* nil
  "The MCP revision the server answered at initialize, sent with every request.")

(defvar *initialized* nil
  "Whether this image signed in to the server since the cell started.")

(defvar *offered* nil
  "The tools the server last listed: ((:name :description :read-only) ...).")

(defvar *request-id* 0)

(defun reset-session ()
  (setf *session-id* nil *negotiated* nil *initialized* nil *offered* nil *request-id* 0))

(defun next-id ()
  (incf *request-id*))

(defun header-value (headers name)
  "NAME's value in the response HEADERS nlk:http answers, or NIL."
  (cond ((hash-table-p headers)
         (or (gethash name headers) (gethash (string-downcase name) headers)))
        ((listp headers) (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun request-headers ()
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "authorization" (format nil "Bearer ~a" (or (setting :token) ""))))
          (when *session-id* (list (cons "mcp-session-id" *session-id*)))
          (when *negotiated* (list (cons "mcp-protocol-version" *negotiated*)))))

(defun post-json (body)
  "One HTTPS POST of BODY to the server => (values TEXT STATUS HEADERS)."
  (nlk:http :post (setting :server-url)
            :headers (request-headers)
            :content body
            :timeout (setting :timeout-seconds)
            :connect-timeout (min 20 (setting :timeout-seconds))))

(defun stream-json (text)
  "The last JSON object a stream body carries, or NIL."
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
      (or (nlk:json-value error :text "message")
          (nlk:one-line (nlk:encode-json-object error))))))

(defun fail-on-status (status object)
  (cond
    ((member status '(401 403))
     (fail "EquipDash refused the API key (HTTP ~a): it is missing, expired or lacks the mcp:access scope. Set a key in the equipdash section, or sign in again in /mcp, then (restart-cells \"nodecode-equipdash\")." status))
    (t
     (fail "EquipDash's MCP server answered HTTP ~a~@[: ~a~]" status (rpc-error-text object)))))

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
  "The one no-answer message MCP wants after initialize; a 202 is the whole answer."
  (ignore-errors
    (post-json (nlk:encode-json-object
                (nlk:json-object "jsonrpc" "2.0" "method" method
                                 "params" (nlk:make-json-object))))
    t))

(defun initialize ()
  (multiple-value-bind (object status)
      (send-rpc "initialize"
                (nlk:json-object "protocolVersion" +protocol-version+
                                  "capabilities" (nlk:make-json-object)
                                  "clientInfo" (nlk:json-object "name" "nodecode-equipdash"
                                                                 "version" +client-version+)))
    (unless (and (integerp status) (<= 200 status 299))
      (fail-on-status status object))
    (setf *negotiated*
          (or (nlk:json-value (nlk:json-value object :object "result") :text "protocolVersion")
              +protocol-version+))
    (send-note "notifications/initialized")
    (setf *initialized* t)
    t))

(defun ensure-session ()
  (unless *initialized* (initialize)))

(defun mcp-call (method &optional params)
  "One MCP method, signing in first and once more after a session the server
forgot => the result object."
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
        (fail "EquipDash refused ~a: ~a" method reason)))
    (or (nlk:json-value object :object "result")
        (nlk:make-json-object))))

;;; --- what one tools/call answers ---------------------------------------------------

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

(defun call-tool-raw (tool arguments)
  "One tools/call of TOOL => the result object."
  (mcp-call "tools/call"
            (nlk:json-object "name" tool
                              "arguments" (or arguments (nlk:make-json-object)))))

(defun tool-answer (tool arguments)
  "One MCP tool call as the model reads it: its text, clipped, or the failure it reported."
  (multiple-value-bind (text errorp) (render-result (call-tool-raw tool arguments))
    (if errorp
        (nle:failure (format nil "ERROR: EquipDash said ~a" text))
        (clip text (setting :max-chars)))))

(defun call-text (tool arguments)
  "One tools/call as its plain text, or the refusal it reported."
  (multiple-value-bind (text errorp) (render-result (call-tool-raw tool arguments))
    (if errorp
        (fail "~a said ~a" tool text)
        text)))

;;; --- the tools the server offers ----------------------------------------------------
;; The server changes its tool list with the plan and the signed-in person, so
;; the list is read once per session and read again when a name it does not
;; carry is asked for. A tool's own readOnlyHint decides whether a call
;; changes the shop; a tool that carries none is treated as one that does.

(defun read-only-hint (tool)
  "T, NIL, or :UNKNOWN when the tool carries no readOnlyHint annotation."
  (let ((annotations (nlk:json-value tool :object "annotations")))
    (multiple-value-bind (value present) (json-member annotations "readOnlyHint")
      (if present (and value t) :unknown))))

(defun schema-arguments (tool)
  "The property names TOOL's input schema carries, and the required ones."
  (let* ((schema (nlk:json-value tool :object "inputSchema"))
         (properties (nlk:json-value schema :object "properties"))
         (required (nlk:json-value schema :array "required"))
         (names nil))
    (when (hash-table-p properties)
      (maphash (lambda (name value)
                 (declare (ignore value))
                 (push name names))
               properties))
    (values (sort names #'string<)
            (and required (coerce required 'list)))))

(defun tool-entry (tool)
  (multiple-value-bind (arguments required) (schema-arguments tool)
    (list :name (nlk:json-value tool :text "name")
          :description (or (nlk:json-value tool :text "description") "")
          :read-only (read-only-hint tool)
          :arguments arguments
          :required required)))

(defun read-tool-list ()
  "Every tool the server offers, read from tools/list."
  (let ((result (mcp-call "tools/list"))
        (entries nil))
    (loop for tool across (nlk:json-array result "tools")
          for entry = (tool-entry tool)
          when (getf entry :name)
            do (push entry entries))
    (nreverse entries)))

(defun offered-tools (&key refresh)
  "The tools the server offers, read once and kept until REFRESH."
  (when (or refresh (null *offered*))
    (setf *offered* (read-tool-list)))
  *offered*)

(defun find-offered (name &key refresh)
  "The entry for NAME among the tools the server offers, or NIL."
  (find name (offered-tools :refresh refresh) :test #'string= :key (lambda (entry) (getf entry :name))))

(defun first-sentence (text)
  "TEXT up to its first sentence's end, at most 300 characters."
  (let* ((text (or text ""))
         (end (or (search ". " text) (position #\Newline text) (length text))))
    (clip (subseq text 0 (min end 300)) 300)))

(defun tool-line (entry)
  (format nil "~a~a~%~a~@[~%  arguments: ~{~a~^, ~}~@[ (required: ~{~a~^, ~})~]~]"
          (getf entry :name)
          (if (eq t (getf entry :read-only)) " [read-only]" " [changes the shop]")
          (first-sentence (getf entry :description))
          (getf entry :arguments)
          (let ((required (getf entry :required)))
            (and required
                 (sort (intersection required (getf entry :arguments) :test #'string=)
                       #'string<)))))

(defun listing (entries filter)
  "The list of ENTRIES as the model reads it, or what to say when FILTER found none."
  (if (null entries)
      (if filter
          (format nil "No tool from ~a names or describes ~s; equipdash_tools with no filter lists all ~d."
                  (setting :server-url) filter (length (offered-tools)))
          (format nil "~a offered no tools." (setting :server-url)))
      (with-output-to-string (out)
        (format out "~d tools from ~a~2%" (length entries) (setting :server-url))
        (dolist (entry entries)
          (format out "~a~%" (tool-line entry))))))

(defparameter +list-schema+
  (list :description
        (concatenate 'string
                     "List the tools the EquipDash server offers this account: each name, what it does, "
                     "whether it is read-only or changes the shop, and the argument names it takes. "
                     "Call it before the first equipdash_call, and again when a name is refused as "
                     "unknown. Read-only.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "filter" (nlk:make-json-object
                                  "type" "string"
                                  "description" "only the tools whose name or description contains this word, such as booking or payment")))))

(defun list-tools (arguments)
  "equipdash_tools: the server's own tools/list, as one list the model reads."
  (answering
    (let* ((filter (nonblank (nlk:json-value arguments :text "filter")))
           (entries (if filter
                        (remove-if-not
                         (lambda (entry)
                           (search (string-downcase filter)
                                   (string-downcase (format nil "~a ~a"
                                                            (getf entry :name)
                                                            (getf entry :description)))))
                         (offered-tools))
                        (offered-tools))))
      (clip (listing entries filter) (setting :max-chars)))))

;;; --- the day, in one call ----------------------------------------------------------

(defparameter +day-schema+
  (list :description
        (concatenate 'string
                     "The shop's day as EquipDash's Today screen shows it: sessions with booked places and "
                     "check-ins, items to get out by time, rentals to pick up, out now and due back, and "
                     "(unless a section is given) what needs the owner now -- booking requests, late "
                     "returns, unpaid bookings, changes waiting for an OK. Read-only. Dates are the "
                     "shop's own timezone, YYYY-MM-DD.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "date" (nlk:make-json-object
                               "type" "string"
                               "description" "the day to read, YYYY-MM-DD in the shop's timezone; today when left out")
                       "section" (nlk:make-json-object
                                  "type" "string"
                                  "enum" (vector "out_now" "people")
                                  "description" "narrow get_today to what is out now, or to the people coming in and out")))))

(defun day-payload (arguments)
  "The arguments get_today takes, from the cell's own."
  (let ((payload (nlk:make-json-object)))
    (dolist (key '("date" "section"))
      (let ((value (nonblank (nlk:json-value arguments :text key))))
        (when value
          (setf (gethash key payload) value))))
    payload))

(defun day (arguments)
  "equipdash_day: get_today, and what needs the owner, in one answer."
  (answering
    (let* ((payload (day-payload arguments))
           (section (nlk:json-value payload :text "section"))
           (parts nil))
      (if (find-offered "get_today")
          (push (call-text "get_today" payload) parts)
          (push (format nil "This EquipDash account offers no get_today tool; equipdash_tools lists what it does offer.") parts))
      (when (null section)
        (if (find-offered "get_needs_you")
            (push (call-text "get_needs_you" (nlk:make-json-object)) parts)
            (push "This EquipDash account offers no get_needs_you tool." parts)))
      (clip (format nil "~{~a~^~2%~}" (nreverse parts)) (setting :max-chars)))))

;;; --- running one tool with the person's yes -----------------------------------------

(defparameter +call-schema+
  (list :description
        (concatenate 'string
                     "Run one EquipDash tool by the name equipdash_tools lists, with that tool's own "
                     "arguments object. A tool that changes the shop answers a preview first: call it "
                     "with preview true to see what it would do, show that to the person, and after a "
                     "clear yes call it again with confirmed true, which sends user_confirmed true (or "
                     "confirmed true on send_payment_link and process_refund). Without preview or "
                     "confirmed a tool that changes the shop is refused. record_payment saves straight "
                     "away, so check the amount first. Read-only tools need neither.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "name" (nlk:make-json-object
                               "type" "string"
                               "description" "the tool's name, such as get_today or create_booking")
                       "arguments" (nlk:make-json-object
                                    "type" "object"
                                    "additionalProperties" t
                                    "description" "the tool's own arguments object, as the server's schema describes them")
                       "preview" (nlk:make-json-object
                                  "type" "boolean"
                                  "description" "true to ask a tool that changes the shop what it would do, without sending the person's yes")
                       "confirmed" (nlk:make-json-object
                                    "type" "boolean"
                                    "description" "true only after the person said yes to this exact preview"))
         "required" (vector "name"))))

(defun confirmation-key (tool)
  "The argument TOOL's own schema spells the person's yes as."
  (if (member tool +own-confirmation+ :test #'string=) "confirmed" "user_confirmed"))

(defun with-confirmation (arguments key)
  "ARGUMENTS with KEY true, as a fresh object."
  (let* ((copy (if (hash-table-p arguments)
                   (nlk:decode-json (nlk:encode-json-object arguments))
                   (nlk:make-json-object)))
         (copy (if (hash-table-p copy) copy (nlk:make-json-object))))
    (setf (gethash key copy) t)
    copy))

(defun preview-note (tool)
  (format nil "~2%(preview: ~a has answered for now; show this to the person, and after a clear yes call equipdash_call again with confirmed true. record_payment is the one tool that saves before the yes: check the amount first.)"
          tool))

(defun yes-refusal (tool entry)
  (refuse "~a ~a. Call again with preview true to see exactly what it would do, or with confirmed true once the person has said yes to that preview~@[, sending user_confirmed true~]."
          tool
          (if (eq t (getf entry :read-only))
              "is read-only"
              "changes the shop's data and needs the person's yes")
          (not (member tool +own-confirmation+ :test #'string=))))

(defun call-tool (arguments)
  "equipdash_call: one tools/call of a name the server listed, on the person's yes."
  (answering
    (let* ((name (or (nonblank (nlk:json-value arguments :text "name"))
                     (return-from call-tool (refuse "name is required: the tool to run, as equipdash_tools lists it"))))
           (payload (or (nlk:json-value arguments :object "arguments") (nlk:make-json-object)))
           (confirmed (and (nlk:json-value arguments :boolean "confirmed") t))
           (preview (and (nlk:json-value arguments :boolean "preview") t))
           (entry (or (find-offered name) (find-offered name :refresh t))))
      (unless entry
        (return-from call-tool
          (refuse "~a is not a tool this EquipDash account offers; equipdash_tools lists the ~d it does"
                  name (length (offered-tools)))))
      (cond
        ((eq t (getf entry :read-only)) (tool-answer name payload))
        (confirmed (tool-answer name (with-confirmation payload (confirmation-key name))))
        (preview (concatenate 'string (tool-answer name payload) (preview-note name)))
        (t (yes-refusal name entry))))))

;;; --- the operator's command ----------------------------------------------------------

(defun command-arguments (text)
  "TEXT as the arguments object of one tool, empty when it says nothing."
  (if (zerop (length (nlk:trimmed text)))
      (nlk:make-json-object)
      (let ((object (ignore-errors (nlk:decode-json text))))
        (if (hash-table-p object)
            object
            (fail "~s is not a JSON object of arguments" text)))))

(defun command-call (words)
  "The operator's own yes: /equipdash call NAME [JSON], sent confirmed."
  (let ((name (first words)))
    (call-tool (nlk:json-object "name" (or name "")
                                 "arguments" (command-arguments (format nil "~{~a~^ ~}" (rest words)))
                                 "confirmed" t))))

(defun run-command (args session-id)
  "One /equipdash invocation: no argument answers the day; tools [WORD] lists
what the server offers; call NAME {json...} runs one tool, the operator's own
yes standing for the person's."
  (declare (ignore session-id))
  (let ((words (remove "" (uiop:split-string (nlk:trimmed (or args ""))
                                             :separator '(#\Space #\Tab))
                       :test #'string=)))
    (handler-case
        (cond ((null words) (day (nlk:make-json-object)))
              ((member (first words) '("tools" "list") :test #'string-equal)
               (list-tools (if (cdr words)
                               (nlk:json-object "filter" (format nil "~{~a~^ ~}" (rest words)))
                               (nlk:make-json-object))))
              ((string-equal (first words) "call") (command-call (rest words)))
              (t (list-tools (nlk:json-object "filter" (format nil "~{~a~^ ~}" words)))))
      (equipdash-error (condition)
        (format nil "equipdash: ~a" (equipdash-error-detail condition))))))

;;; --- the cell --------------------------------------------------------------------------

(nle:define-cell equipdash
  (:section ("equipdash")
    (:guide "create an API key in EquipDash under Settings > API & Integrations with the mcp:access scope, and name it in token_env or paste it into token; a key is only needed without the OAuth sign-in; then /equipdash shows today's pickups and returns")
    ("server_url" :string :default "https://app.equipdash.com/mcp/v1"
     :doc "EquipDash's MCP server, streamable HTTP")
    ("token" :secret
     :doc "an EquipDash API key; empty reuses an environment variable, a file, or the sign-in the mcp cell keeps")
    ("token_env" :env
     :doc "the environment variable holding an API key instead of the token member")
    ("token_file" :path
     :doc "a file holding an API key instead of the token member")
    ("timeout_seconds" :integer :default 30 :min 5
     :doc "one call's ceiling")
    ("max_chars" :integer :default 12000 :min 500
     :doc "how much of one answer the model reads: a tool list runs long"))
  (:settings 'read-settings)
  (:help :equipdash
         "equipdash: run a rental or activity shop - today's pickups and returns, bookings, availability, payments (help :equipdash)"
         +primer+)
  (:command "equipdash" 'run-command
            :description "equipdash: today's pickups, returns and what needs you, from EquipDash"
            :argument-hint "DAY | tools [WORD] | call NAME {json}"
            :session nil)
  (:tool "equipdash_tools" +list-schema+ 'list-tools)
  (:tool "equipdash_call" +call-schema+ 'call-tool)
  (:tool "equipdash_day" +day-schema+ 'day)
  (:start (lambda ()
            (check-config)
            (reset-session)
            (nle:on-stop #'reset-session))))
