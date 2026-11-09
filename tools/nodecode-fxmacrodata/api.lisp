;;;; api.lisp --- FXMacroData's REST and MCP wires, and the envelopes the tools answer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is one fixed-origin client over two hosts and a handler per
;;;; tool. This is the same two wires -- NLK:HTTP instead of requests, the same
;;;; routes, the same query and path members, the same MCP handshake -- and one
;;;; table of operations (operations.lisp) instead of a hundred functions.
;;;;
;;;; A tool call answers the JSON text the original answered: the operation,
;;;; the payload as the service sent it, a `records' projection of it, the
;;;; source URL, the provider URL, and any free-tier notice the payload
;;;; carries. The key rides X-API-Key on REST and a Bearer header on MCP, never
;;;; a URL or an argument.

(in-package #:nodecode-fxmacrodata)

;;; --- where it calls, and with what -------------------------------------------------

(defparameter +site-url+ "https://fxmacrodata.com/"
  "The provider page every envelope names, as the original's SITE_URL did.")

(defparameter +subscribe-url+ "https://fxmacrodata.com/subscribe"
  "Where the original sends a reader whose dataset needs a key.")

(defparameter +default-base-url+ "https://api.fxmacrodata.com"
  "The REST origin, fixed as the original fixed it.")

(defparameter +default-mcp-url+ "https://mcp.fxmacrodata.com/mcp"
  "The hosted MCP endpoint, fixed as the original fixed it.")

(defparameter +default-key-env+ "FXMACRODATA_API_KEY"
  "The variable the section names for a key kept out of the config file.")

(defparameter +max-response-bytes+ (* 10 1024 1024)
  "The most of one response this cell reads, as the original capped it.")

(defparameter +mcp-protocol-version+ "2025-03-26"
  "The MCP protocol version the original pins.")

(defparameter +user-agent+ "nodecode-fxmacrodata/0.1.0"
  "What the service sees this cell as.")

(defparameter +default-indicator+ "policy_rate"
  "The USD indicator the brief reads when the call names none.")

;;; --- the operation table --------------------------------------------------------------

(defun find-operation (name)
  "The operation called NAME -- its tool name (fxmd_forex) or the service's own
(forex), which is what the composed tools name."
  (or (find name +operations+ :key #'operation-name :test #'string=)
      (find (concatenate 'string "fxmd_" name) +operations+ :key #'operation-name :test #'string=)))

(defun operation-label (name op)
  "The operation name an envelope carries: the service's own, as the original's
result.operation did, not the tool's fxmd_ prefix."
  (if (and op (> (length (operation-name op)) 5))
      (subseq (operation-name op) 5)
      name))

(defparameter +composed-tools+ '("fxmd_usd_macro_brief" "fxmd_indicator_history_snapshot")
  "The two tools the original writes in code rather than in its inventory.")

(defun tool-names ()
  "Every tool this cell registers, in registration order."
  (append (mapcar #'operation-name +operations+) +composed-tools+))

(defun tool-schema (name)
  "The plist NLE:REGISTER-TOOL takes for NAME: a description and the schema object."
  (let ((op (find-operation name)))
    (list :description (and op (operation-description op))
          :parameters (cond ((null op) (nlk:decode-json "{\"type\":\"object\",\"properties\":{}}"))
                            ((string= name "fxmd_usd_macro_brief") (nlk:decode-json +brief-schema+))
                            ((string= name "fxmd_indicator_history_snapshot") (nlk:decode-json +snapshot-schema+))
                            (t (operation-schema op))))))

;;; --- the section's values ---------------------------------------------------------------

(defun text-setting (name default)
  "NAME from the section, trimmed, or DEFAULT when it is unset or blank."
  (let ((value (setting name)))
    (if (and (stringp value) (plusp (length (nlk:trimmed value))))
        (nlk:trimmed value)
        default)))

(defun without-trailing-slash (url)
  (if (and (plusp (length url)) (char= #\/ (char url (1- (length url)))))
      (subseq url 0 (1- (length url)))
      url))

(defun base-url ()
  "The REST origin this start dials, without a trailing slash."
  (without-trailing-slash (text-setting :base-url +default-base-url+)))

(defun mcp-url ()
  "The MCP endpoint this start dials."
  (text-setting :mcp-url +default-mcp-url+))

(defun timeout-seconds ()
  "One exchange's ceiling: the section's seconds, bounded to 1..120 as the original bound them."
  (let ((seconds (or (setting :timeout-seconds) 30)))
    (min 120 (max 1 seconds))))

(defun header-safe-p (text)
  "Whether TEXT can travel in an HTTP header at all."
  (every (lambda (char) (<= 33 (char-code char) 126)) text))

(defun api-key ()
  "The key: the secret in the section, else the environment variable the section names.
NIL when neither holds one, which is the public, no-key baseline."
  (let ((key (or (let ((secret (setting :api-key)))
                    (and (stringp secret) (nlk:trimmed secret)))
                  (let ((name (setting :api-key-env)))
                    (when (and (stringp name) (plusp (length (nlk:trimmed name))))
                      (let ((value (nlk:trimmed (or (uiop:getenv (nlk:trimmed name)) ""))))
                        (and (plusp (length value)) value)))))))
    (when (and key (not (header-safe-p key)))
      (fail "The FXMacroData key contains spaces or control characters. Set it to the key value only."))
    key))

;;; --- one exchange --------------------------------------------------------------------

(defun header-value (headers name)
  "NAME's value in HEADERS, an alist or a table, case-insensitively."
  (cond ((null headers) nil)
        ((hash-table-p headers)
         (or (gethash name headers)
             (loop for key being the hash-keys of headers
                   when (string-equal key name) return (gethash key headers))))
        (t (cdr (assoc name headers :test #'string-equal)))))

(defun http-exchange (method url &key headers content)
  "One exchange => (VALUES TEXT STATUS HEADERS). A transport failure is FAIL."
  (handler-case
      (multiple-value-bind (body status response-headers)
          (nlk:http method url :headers headers :content content
                    :timeout (timeout-seconds) :max-bytes +max-response-bytes+)
        (values (cond ((stringp body) body)
                      ((vectorp body) (babel:octets-to-string body :encoding :utf-8))
                      ((null body) "")
                      (t ""))
                status response-headers))
    (fxmacrodata-error (condition) (error condition))
    (error ()
      (fail "Unable to reach FXMacroData within the request timeout. Retry later."))))

(defun check-status (status)
  "A status this cell reads, or the refusal the original wrote for it."
  (cond ((<= 200 (or status 0) 299) status)
        ((<= 300 (or status 0) 399)
         (fail "FXMacroData answered with a redirect, which this cell does not follow. Retry later."))
        ((member status '(401 403))
         (fail "This dataset requires an authorized FXMacroData API key (~a). Public USD catalogue, history and calendar remain available."
               +subscribe-url+))
        ((eql status 429) (fail "FXMacroData rate limit reached. Retry later."))
        ((eql status 404) (fail "The requested FXMacroData resource is unavailable. Check catalogue discovery."))
        (t (fail "FXMacroData request failed (HTTP ~a)." status))))

(defun decode-body (text status)
  "TEXT, the JSON body of an answer that STATUS accepted."
  (check-status status)
  (let ((trimmed (nlk:trimmed (or text ""))))
    (when (zerop (length trimmed))
      (fail "FXMacroData answered HTTP ~a with an empty body." status))
    (or (ignore-errors (nlk:decode-json trimmed))
        (fail "FXMacroData answered HTTP ~a with a body that is not JSON." status))))

;;; --- the REST wire -------------------------------------------------------------------

(defun quoted (text)
  "TEXT as one URL-encoded piece."
  (quri:url-encode text :encoding :utf-8))

(defun query-text (value)
  "VALUE as the query string reads it, or NIL when the API should not be sent it."
  (cond ((eq value t) "true")
        ((null value) nil)
        ((stringp value) (if (zerop (length (nlk:trimmed value))) nil value))
        ((numberp value) (princ-to-string value))
        (t (princ-to-string value))))

(defun path-parameter (name value)
  "VALUE as one path member: letters, digits, dot, dash, colon and underscore only."
  (unless (and (stringp value)
               (plusp (length value))
               (not (member value '("." "..") :test #'string=))
               (every (lambda (char) (or (alphanumericp char) (member char '(#\. #\- #\: #\_)))) value))
    (fail "~a must be letters, digits, dot, dash, colon or underscore." name))
  (quoted value))

(defun rest-url (op arguments)
  "The URL OP's call goes to, with its path members substituted and the query built
from the arguments the operation declares as query members."
  (let ((path (copy-seq (operation-path op)))
        (query '()))
    (dolist (parameter (operation-parameters op))
      (let ((name (first parameter))
            (where (second parameter))
            (value (gethash (first parameter) arguments)))
        (when value
          (cond ((and where (string-equal where "path"))
                 (setf path (replace-all path (format nil "{~a}" name) (path-parameter name value))))
                ((and where (string-equal where "query"))
                 (if (and (vectorp value) (not (stringp value)))
                     (loop for one across value
                           for text = (query-text one)
                           when text do (push (cons name (quoted text)) query))
                     (let ((text (query-text value)))
                       (when text (push (cons name (quoted text)) query)))))))))
    (let ((url (concatenate 'string (base-url) path)))
      (if query
          (format nil "~a?~{~a~^&~}" url
                  (loop for (name . text) in (nreverse query) collect (format nil "~a=~a" name text)))
          url))))

(defun replace-all (text old new)
  "TEXT with every OLD replaced by NEW."
  (let ((at (search old text)))
    (if at
        (concatenate 'string (subseq text 0 at) new (replace-all (subseq text (+ at (length old))) old new))
        text)))

(defun rest-call (op arguments)
  "OP's call over the REST wire => (VALUES PAYLOAD SOURCE-URL)."
  (let* ((headers (append (list (cons "Accept" "application/json")
                                 (cons "User-Agent" +user-agent+))
                           (loop for parameter in (operation-parameters op)
                                 for name = (first parameter)
                                 for where = (second parameter)
                                 for value = (gethash name arguments)
                                 when (and value (stringp where) (string-equal where "header"))
                                   collect (let ((text (princ-to-string value)))
                                             (when (or (find #\Return text) (find #\Linefeed text))
                                               (fail "~a must not hold a line break." name))
                                             (cons name text)))
                           (let ((key (api-key))) (and key (list (cons "X-API-Key" key))))))
         (method (intern (string-upcase (string (operation-method op))) :keyword))
         (url (rest-url op arguments))
         (content nil))
    (when (eq method :post)
      (let ((body (gethash "body" arguments)))
        (when body
          (setf content (nlk:encode-json-object body)
                headers (cons (cons "Content-Type" "application/json") headers)))))
    (multiple-value-bind (text status)
        (http-exchange method url :headers headers :content content)
      (values (decode-body text status) url))))

;;; --- the MCP wire --------------------------------------------------------------------

(defun mcp-headers (session)
  "The headers one MCP request carries."
  (append (list (cons "Content-Type" "application/json")
                (cons "Accept" "application/json, text/event-stream")
                (cons "User-Agent" +user-agent+))
          (when session
            (list (cons "MCP-Protocol-Version" +mcp-protocol-version+)
                  (cons "Mcp-Session-Id" session)))
          (let ((key (api-key))) (and key (list (cons "Authorization" (concatenate 'string "Bearer " key)))))))

(defun sse-payloads (text)
  "The `data:' values of an event-stream capture, each decoded when it is JSON."
  (let ((events '()) (data '()))
    (flet ((flush ()
             (when data
               (let ((joined (format nil "~{~a~^~%~}" (nreverse data))))
                 (push (or (ignore-errors (nlk:decode-json joined)) joined) events))
               (setf data '()))))
      (with-input-from-string (stream text)
        (loop for line = (read-line stream nil nil)
              while line
              do (let ((line (string-right-trim '(#\Return) line)))
                   (cond ((zerop (length line)) (flush))
                         ((uiop:string-prefix-p "data:" line)
                          (push (string-left-trim " " (subseq line 5)) data))
                         ((or (uiop:string-prefix-p ":" line) (zerop (length (nlk:trimmed line))))
                          nil))))
        (flush)))
    (nreverse events)))

(defun rpc-answer (text id)
  "The JSON-RPC answer to this request's ID in TEXT, which is JSON or an event-stream capture."
  (let ((trimmed (nlk:trimmed (or text ""))))
    (when (zerop (length trimmed))
      (fail "FXMacroData MCP answered with no body."))
    (let ((single (ignore-errors (nlk:decode-json trimmed))))
      (cond ((hash-table-p single) single)
            (t (or (find-if (lambda (event) (and (hash-table-p event) (eql id (gethash "id" event))))
                             (sse-payloads text))
                   (fail "FXMacroData MCP returned no matching response.")))))))

(defun rpc-object (id method params &key notification)
  "One JSON-RPC request object."
  (let ((object (nlk:json-object "jsonrpc" "2.0" "method" method)))
    (unless notification (setf (gethash "id" object) id))
    (when params (setf (gethash "params" object) params))
    object))

(defun mcp-call (tool-name arguments)
  "TOOL-NAME called on the hosted MCP server => the result object, as the original
opens a session, initializes, and calls the one tool."
  (multiple-value-bind (text status headers)
      (http-exchange :post (mcp-url) :headers (mcp-headers nil)
                     :content (nlk:encode-json-object
                               (rpc-object 1 "initialize"
                                           (nlk:json-object
                                            "protocolVersion" +mcp-protocol-version+
                                            "capabilities" (nlk:json-object)
                                            "clientInfo" (nlk:json-object "name" "nodecode-fxmacrodata"
                                                                           "version" "0.1.0")))))
    (declare (ignore status))
    (let* ((answer (decode-body text 200))
           (session (header-value headers "Mcp-Session-Id")))
      (unless (and (hash-table-p answer) (gethash "result" answer)
                   (not (gethash "error" answer)))
        (fail "FXMacroData MCP returned an unsupported initialization response."))
      ;; The notification answers 202 with no body; a server that does not is
      ;; not worth failing a working call over.
      (ignore-errors
        (http-exchange :post (mcp-url) :headers (mcp-headers session)
                       :content (nlk:encode-json-object
                                 (rpc-object nil "notifications/initialized" (nlk:json-object)
                                             :notification t))))
      (multiple-value-bind (call-text call-status)
          (http-exchange :post (mcp-url) :headers (mcp-headers session)
                         :content (nlk:encode-json-object
                                   (rpc-object 2 "tools/call"
                                               (nlk:json-object "name" tool-name
                                                                "arguments" arguments))))
        (let ((result-answer (decode-body call-text call-status)))
          (when (and (hash-table-p result-answer) (gethash "error" result-answer))
            (fail "FXMacroData MCP could not complete this request."))
          (let ((result (and (hash-table-p result-answer) (gethash "result" result-answer))))
            (unless (hash-table-p result)
              (fail "FXMacroData MCP returned an invalid tool result."))
            (when (gethash "isError" result)
              (fail "FXMacroData MCP tool is unavailable. Check arguments and dataset access."))
            result))))))

;;; --- the envelope a tool answers ---------------------------------------------------------

(defun object-values-p (value)
  "Whether VALUE is a JSON object whose every member is an object."
  (and (hash-table-p value)
       (plusp (hash-table-count value))
       (loop for one being the hash-values of value always (hash-table-p one))))

(defun records-of (payload)
  "The rows PAYLOAD carries, as the original's additive projection read them."
  (cond ((null payload) '())
        ((and (vectorp payload) (not (stringp payload)))
         (coerce (loop for row across payload
                       collect (if (hash-table-p row) row (nlk:json-object "value" row)))
                 'vector))
        ((hash-table-p payload)
         (dolist (key '("structuredContent" "result" "data" "rows" "events" "announcements"
                         "predictions" "prices" "commodities" "results" "series" "indicators"))
           (let ((value (gethash key payload)))
             (when (and value (or (hash-table-p value) (and (vectorp value) (not (stringp value)))))
               (return-from records-of
                 (if (object-values-p value)
                     (coerce (loop for record-key being the hash-keys of value
                                    collect (nlk:json-object "record_key" record-key
                                                              "record" (gethash record-key value)))
                             'vector)
                     (records-of value))))))
         (let ((content (gethash "content" payload)))
           (when (and content (vectorp content) (not (stringp content)))
             (let ((rows '()))
               (loop for block across content
                     do (if (and (hash-table-p block) (string= "text" (or (gethash "type" block) "")))
                            (let ((decoded (ignore-errors (nlk:decode-json (or (gethash "text" block) "")))))
                              (if decoded
                                  (setf rows (append rows (coerce (records-of decoded) 'list)))
                                  (push (nlk:json-object "text" (gethash "text" block)) rows)))
                            (push block rows)))
               (when rows (return-from records-of (coerce (nreverse rows) 'vector))))))
         (vector payload))
        (t (vector payload))))

(defun response-body (payload)
  "PAYLOAD's structured content when it is an MCP result, else PAYLOAD."
  (let ((structured (and (hash-table-p payload) (gethash "structuredContent" payload))))
    (if (hash-table-p structured) structured payload)))

(defun access-notices (payload)
  "The free-tier window and delay messages PAYLOAD carries, so delayed data is never
presented as current."
  (let ((body (response-body payload))
        (notices '()))
    (when (hash-table-p body)
      (dolist (name '("freemium_window" "freemium_delay"))
        (let ((value (gethash name body)))
          (let ((message (if (hash-table-p value) (gethash "message" value) value)))
            (when (and (stringp message) (plusp (length (nlk:trimmed message))))
              (push (nlk:trimmed message) notices))))))
    (nreverse notices)))

(defun error-body-message (payload)
  "The refusal a successful status with an error body carries, or NIL."
  (let ((body (response-body payload)))
    (when (and (hash-table-p body) (null (gethash "data" body)))
      (let ((code (nlk:json-value body :text "error")))
        (when code
          (let ((code (string-downcase (nlk:trimmed code))))
            (if (member code '("api_key_required" "subscription_required" "invalid_api_key")
                        :test #'string=)
                (format nil "FXMacroData needs an authorized API key for this request (~a). Set one in the fxmacrodata section; keys are available at ~a."
                        code +subscribe-url+)
                (format nil "FXMacroData returned an error (~a). Check parameters and access, then retry." code))))))))

(defun citations-of (source-url payload)
  "The provider and original-publisher links the envelope carries."
  (let ((citations (list (nlk:json-object "title" "FXMacroData" "url" source-url)))
        (seen (make-hash-table :test #'equal)))
    (setf (gethash source-url seen) t)
    (loop for row across (records-of payload)
          do (let ((source (and (hash-table-p row) (nlk:json-value row :text "source_url"))))
               (when (and source (uiop:string-prefix-p "https://" source)
                          (not (gethash source seen)))
                 (setf (gethash source seen) t)
                 (push (nlk:json-object "title" "Original publisher source" "url" source) citations))))
    (coerce (nreverse citations) 'vector)))

(defun envelope (name payload source-url)
  "The answer one operation's call makes, shaped as the original shaped it."
  (let ((object (nlk:json-object "operation" name
                                  "data" payload
                                  "records" (records-of payload)
                                  "source_url" source-url
                                  "provider_url" +site-url+
                                  "citations" (citations-of source-url payload))))
    (let ((notices (access-notices payload)))
      (when notices (setf (gethash "notices" object) notices)))
    object))

(defun error-envelope (name message)
  (nlk:json-object "operation" name "error" message "provider_url" +site-url+))

;;; --- arguments -------------------------------------------------------------------------

(defun arguments-table (arguments)
  "ARGUMENTS as a string-keyed table: one already, a plist, or NIL."
  (cond ((hash-table-p arguments) arguments)
        ((null arguments) (make-hash-table :test #'equal))
        ((listp arguments)
         (let ((table (make-hash-table :test #'equal)))
           (loop for (key value) on arguments by #'cddr
                 do (setf (gethash (string-downcase (string key)) table) value))
           table))
        (t (make-hash-table :test #'equal))))

(defparameter +date-pattern+ '(:digit :digit :digit :digit #\- :digit :digit #\- :digit :digit)
  "The shape a date argument must have: YYYY-MM-DD.")

(defun date-p (text)
  "Whether TEXT reads as a YYYY-MM-DD date."
  (and (stringp text)
       (= 10 (length text))
       (loop for char across text for wanted in +date-pattern+
             always (if (eq wanted :digit) (digit-char-p char) (char= char wanted)))))

(defun date-argument (arguments name)
  "NAME's value when it is set, refusing a date that is not YYYY-MM-DD."
  (let ((value (gethash name arguments)))
    (when value
      (unless (date-p value)
        (fail "Dates must use the YYYY-MM-DD format."))
      value)))

(defun check-arguments (arguments)
  "The original's pre-flight: no credentials in arguments, currency codes three
letters, real dates in order, and no empty slug."
  (loop for key being the hash-keys of arguments
        when (and (stringp key)
                  (member (string-downcase key)
                          '("api_key" "access_token" "authorization" "url" "base_url") :test #'string=))
          do (fail "Credentials and endpoint overrides are not operation arguments."))
  (dolist (name '("currency" "base" "quote"))
    (let ((value (nlk:json-value arguments :string name)))
      (when value
        (unless (or (and (= 3 (length value)) (every #'alpha-char-p value))
                    (string-equal value "comm"))
          (fail "Currency codes must be 3 letters, for example usd or eur.")))))
  (let ((start (date-argument arguments "start_date"))
        (end (date-argument arguments "end_date")))
    (when (and start end (string> start end))
      (fail "start_date must not be later than end_date.")))
  (dolist (name '("indicator" "factor" "slug"))
    (multiple-value-bind (value present) (gethash name arguments)
      (when (and present (stringp value) (zerop (length (nlk:trimmed value))))
        (fail "Indicator and factor slugs must not be empty. Use catalogue discovery.")))))

;;; --- one operation, one answer ------------------------------------------------------------

(defun run-operation (name arguments)
  "NAME run with ARGUMENTS => (VALUES PAYLOAD SOURCE-URL)."
  (let ((op (find-operation name)))
    (unless op
      (fail "Unknown FXMacroData operation. Use operation discovery."))
    (check-arguments arguments)
    (cond ((eq (operation-method op) :mcp)
           (values (mcp-call (operation-mcp-name op) arguments) +site-url+))
          (t (rest-call op arguments)))))

(defun query (name &optional arguments)
  "The envelope one tool call answers: the operation's data, or the reason it did not come."
  (let* ((arguments (arguments-table arguments))
         (op (find-operation name))
         (label (operation-label name op)))
    (handler-case
        (multiple-value-bind (payload source-url) (run-operation name arguments)
          (let ((message (error-body-message payload)))
            (if message
                (error-envelope label message)
                (envelope label payload source-url))))
      (fxmacrodata-error (condition)
        (error-envelope label (fxmacrodata-error-detail condition))))))

(defun call-tool (name &optional arguments)
  "NAME called from eval or through the wire => the JSON text a model reads."
  (nlk:encode-json-object (query name arguments)))

;;; --- the two composed tools ---------------------------------------------------------------

(defun utc-day (&optional (offset-days 0))
  "Today, OFFSET-DAYS from now, in UTC, as YYYY-MM-DD."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (+ (get-universal-time) (* offset-days 86400)) 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun usd-brief (&optional arguments)
  "The catalogue, history and calendar brief the original composed, with its own scope."
  (let* ((arguments (arguments-table arguments))
         (indicator (or (nlk:json-value arguments :text "indicator") +default-indicator+))
         (today (utc-day))
         (end (utc-day 7))
         (sections (list (query "data_catalogue" (nlk:json-object "currency" "USD"))
                         (query "indicator_history" (nlk:json-object "currency" "USD"
                                                                      "indicator" indicator
                                                                      "limit" 10))
                         (query "release_calendar" (nlk:json-object "currency" "USD"
                                                                     "start_date" today
                                                                     "end_date" end
                                                                     "timezone" "UTC")))))
    (nlk:json-object
     "title" "USD macro research brief"
     "sections" (coerce sections 'vector)
     "complete" (every (lambda (section) (null (gethash "error" section))) sections)
     "scope" (nlk:json-object "currency" "USD" "history_limit" 10
                               "calendar_start" today "calendar_end" end
                               "timezone" "UTC" "history_complete" nil)
     "provider_url" +site-url+)))

(defparameter +snapshot-page-limit+ 100
  "Rows a snapshot asks of one page, as the original's PageTraversal did.")

(defparameter +snapshot-max-pages+ 10000
  "The most pages one snapshot reads, as the original capped it.")

(defun snapshot-page-arguments (arguments limit offset version)
  "The page call's arguments: the caller's, with this page's paging members."
  (let ((page (arguments-table arguments)))
    (setf (gethash "limit" page) limit
          (gethash "offset" page) offset)
    (if version
        (setf (gethash "dataset_version" page) version)
        (remhash "dataset_version" page))
    page))

(defun page-rows (payload)
  "PAYLOAD's data array, refused when it is not one."
  (let ((rows (and (hash-table-p payload) (gethash "data" payload))))
    (unless (and rows (vectorp rows) (not (stringp rows))
                 (loop for row across rows always (hash-table-p row)))
      (fail "Expected a data array of records."))
    rows))

(defun snapshot-advance (payload offset version pages seen)
  "One page checked against the traversal's state => (VALUES DONE NEXT-OFFSET VERSION PAGES)."
  (let* ((rows (page-rows payload))
         (pagination (gethash "pagination" payload))
         (page-version (nlk:json-value payload :string "dataset_version")))
    (unless (hash-table-p pagination)
      (if (and (zerop (length rows)) (null (gethash "requested_window_has_data" payload)))
          (return-from snapshot-advance (values t offset version pages))
          (fail "Missing pagination metadata; completeness is unknown.")))
    (let ((has-more (nlk:json-value pagination :boolean "has_more"))
          (returned (nlk:json-value pagination :integer "returned_count"))
          (reported-offset (nlk:json-value pagination :integer "offset")))
      (when (and reported-offset (/= reported-offset offset))
        (fail "Response offset does not match the requested page."))
      (when (and returned (/= returned (length rows)))
        (fail "Response row count does not match pagination metadata."))
      (when (and version (not (equal version page-version)))
        (fail "Dataset version changed or disappeared; restart the traversal."))
      (when (and (zerop pages) page-version) (setf version page-version))
      (let ((fingerprint (nlk:encode-json-object payload)))
        (when (gethash fingerprint seen)
          (fail "Repeated history page; refusing incomplete extraction."))
        (setf (gethash fingerprint seen) t))
      (cond ((not has-more) (values t offset version (1+ pages)))
            ((null version) (fail "Multi-page history needs a dataset version pin."))
            (t (let ((next (nlk:json-value pagination :integer "next_offset")))
                 (unless (and next (= next (+ offset (length rows))))
                   (fail "Missing integer next_offset."))
                 (values nil next version (1+ pages))))))))

(defun indicator-history-snapshot (&optional arguments)
  "The requested authorised window, read page by page under its first dataset version."
  (handler-case
      (let* ((arguments (arguments-table arguments))
             (rows '())
             (envelopes '())
             (seen (make-hash-table :test #'equal))
             (offset 0)
             (version nil)
             (pages 0))
        (loop
          (let ((section (query "indicator_history"
                                 (snapshot-page-arguments arguments +snapshot-page-limit+ offset version))))
            (let ((message (gethash "error" section)))
              (when message
                (return-from indicator-history-snapshot (error-envelope "fxmd_indicator_history_snapshot" message))))
            (let ((payload (gethash "data" section)))
              (multiple-value-bind (done next next-version next-pages)
                  (snapshot-advance payload offset version pages seen)
                (setf rows (append rows (coerce (page-rows payload) 'list))
                      offset next version next-version pages next-pages)
                (let ((metadata (make-hash-table :test #'equal)))
                  (loop for key being the hash-keys of payload
                        unless (string= key "data")
                          do (setf (gethash key metadata) (gethash key payload)))
                  (push metadata envelopes))
                (when done (return))
                (when (> pages +snapshot-max-pages+)
                  (fail "History traversal exceeded its bounded page budget."))))))
        (let ((result (make-hash-table :test #'equal))
              (first-page (car (last envelopes))))
          (loop for key being the hash-keys of first-page
                do (setf (gethash key result) (gethash key first-page)))
          (setf (gethash "data" result) (coerce rows 'vector)
                (gethash "snapshot" result)
                (nlk:json-object "complete" t "page_count" pages
                                  "scope" "requested window and authorised access"
                                  "dataset_version" version)
                (gethash "page_metadata" result) (coerce (nreverse envelopes) 'vector))
          (envelope "fxmd_indicator_history_snapshot" result
                    (concatenate 'string (base-url) "/v1/announcements"))))
    (fxmacrodata-error (condition)
      (error-envelope "fxmd_indicator_history_snapshot" (fxmacrodata-error-detail condition)))))

;;; --- the tools ------------------------------------------------------------------------

(defun run-tool (name arguments)
  "NAME called through the wire => the JSON text the model reads."
  (cond ((string= name "fxmd_usd_macro_brief")
         (nlk:encode-json-object (usd-brief arguments)))
        ((string= name "fxmd_indicator_history_snapshot")
         (nlk:encode-json-object (indicator-history-snapshot arguments)))
        (t (call-tool name arguments))))

(defun install-tools ()
  "Register every operation this cell carries, each with its own inverse in this start's scope."
  (dolist (name (tool-names))
    (nle:register-tool name (tool-schema name) (lambda (arguments) (run-tool name arguments)))
    (nle:on-stop (lambda () (nle:unregister-tool name))))
  (length (tool-names)))
