;;;; cell.lisp --- Makernice video and image generation as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of makernice/makernice-plugin (see NOTICE). The
;;;; original runs no code of its own: a plugin.json, an mcp.json naming
;;;; https://mcp.makernice.com/mcp, and four skills that teach the agent to
;;;; price a job, ask the user, run it and fetch the result. Every tool lives
;;;; on Makernice's remote MCP server.
;;;;
;;;; This cell is that client in Lisp. It speaks MCP's streamable-HTTP
;;;; protocol to the same server -- initialize, notifications/initialized, then
;;;; tools/call -- and registers the server's eight tools under makernice_*:
;;;; list_models, estimate_cost, generate, check_job, cancel_job, list_trends,
;;;; get_balance and get_topup_link. The skills' quote-then-consent flow is
;;;; the primer, (help :makernice), and is enforced where a local check can:
;;;; generate refuses without the price the user approved (max_cost_usd) and a
;;;; stable request_id, and the arguments are passed to the server unchanged.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "makernice": {"token": "...", "url": "https://mcp.makernice.com/mcp"}
;;;; The token is the bearer Makernice's sign-in issues; token_env names the
;;;; variable holding it and token_file a file. Without one the cell refuses at
;;;; start, naming what to set.

(defpackage #:nodecode-makernice
  (:use #:cl)
  (:nicknames #:makernice)
  (:export #:call #:account #:makernice-error))

(in-package #:nodecode-makernice)

(nlk:define-peripheral makernice :not-running t)

;;; --- what the original declares -----------------------------------------------

(defparameter +url-default+ "https://mcp.makernice.com/mcp"
  "The remote MCP server the plugin's mcp.json names, and the one this cell
talks to unless the section says otherwise.")

(defparameter +token-env-default+ "MAKERNICE_TOKEN"
  "The variable the token is read from when the section names none.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP protocol revision this client sends; the server answers on its own
if it speaks another.")

(defparameter +server-tools+
  '("list_models" "estimate_cost" "generate" "check_job" "cancel_job"
    "list_trends" "get_balance" "get_topup_link")
  "The eight tools the server serves, as the plugin's evals/mocks pin them.")

(defparameter +primer+
  "Makernice generates video and images on its own servers and charges a prepaid balance in USD.
Every job is priced first: estimate, show the price, and call generate only after the user says yes.
  makernice_list_models   the models with their modes, resolutions, durations and unit prices; pass
      model= one id for its full parameters. Prices change: never quote one from memory.
  makernice_estimate_cost  input= the generation parameters as an object -- {model, mode, prompt,
      duration, resolution, aspect_ratio, image_url, last_image_url, reference_images, seed, ...} or
      {template, images} for a trend template. It creates nothing and charges nothing. Compare two or
      three options with it before proposing one.
  makernice_generate   input= the same object, max_cost_usd= the amount_usd the user approved, and
      request_id= a printable id of your own, at most 64 characters. The job is refused when the
      price is above max_cost_usd. A retry after a network error, a timeout or a rate limit sends the
      same input, the same max_cost_usd and the same request_id: the same id returns the same job and
      is never charged twice. It answers a job_id.
  makernice_check_job   job_id= that id, wait_s= seconds to wait (0 to 55): call it with 55 rather
      than polling at once. It answers the status, the charged amount and the result links, which
      expire after 24 hours.
  makernice_cancel_job  job_id= a job still queued: the held amount is released in full. A job that
      has started cannot be canceled.
  makernice_list_trends  the trending templates with their total price per resolution, for
      generate with {template, images}.
  makernice_get_balance  the available balance, what is held by running jobs, today's spending, the
      daily limit and this connection's per-job limit.
  makernice_get_topup_link  the wallet page where the user adds balance themselves.
From eval the same work is (makernice:call \"estimate_cost\" (list :input ...)) and
(makernice:account), which answers the balance. Failed jobs are not charged; a finished output blocked
by output moderation is charged. ERROR: the answer names the problem; a refused token means the
Makernice sign-in is stale, and (help :makernice) / /makernice say what the cell runs with."
  "What (help :makernice) answers while the cell runs.")

;;; --- the section, read back ----------------------------------------------------

(defun trimmed-setting (name)
  "SETTING NAME as a trimmed string, or NIL when it is absent or blank."
  (let ((value (setting name)))
    (and (stringp value)
         (plusp (length (string-trim '(#\Space #\Tab) value)))
         (string-trim '(#\Space #\Tab) value))))

(defun endpoint ()
  "The MCP server's url, without a trailing slash."
  (let ((given (string-right-trim "/" (or (trimmed-setting :url) ""))))
    (if (plusp (length given)) given +url-default+)))

(defun token-env-name ()
  "The environment variable the token is read from."
  (or (trimmed-setting :token-env) +token-env-default+))

(defun file-token ()
  "The token a token_file holds, trimmed, or NIL."
  (let ((file (trimmed-setting :token-file)))
    (when file
      (let ((path (ignore-errors (probe-file (uiop:parse-native-namestring file)))))
        (when path
          (let ((text (ignore-errors
                        (with-open-file (in path :external-format :utf-8)
                          (let ((line (read-line in nil nil))) line)))))
            (and text (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
                 (string-trim '(#\Space #\Tab #\Newline #\Return) text))))))))

(defun token ()
  "The bearer: the section's token, else token_file, else the variable
token_env names -- read again at every call, so a rotated token takes effect."
  (or (trimmed-setting :token)
      (file-token)
      (let ((value (uiop:getenv (token-env-name))))
        (and value (plusp (length (string-trim '(#\Space) value)))
             (string-trim '(#\Space) value)))))

(defun token-source ()
  "Where the token in hand came from, in words -- never the token itself."
  (cond ((trimmed-setting :token) "the section's token")
        ((file-token) "the file token_file names")
        ((token) (format nil "~a" (token-env-name)))
        (t (format nil "missing: set token, or export ~a" (token-env-name)))))

(defun check-config ()
  "Refuse a section that could never reach the server, naming what to set."
  (unless (token)
    (error 'nlk:config-refusal
           :detail (format nil
                            "makernice: no token: export ~a, or put \"token\" in the makernice ~
                             section (the access token the Makernice sign-in issues; ~
                             https://mcp.makernice.com/mcp requires one)"
                            (token-env-name)))))

;;; --- the wire: MCP over streamable HTTP -----------------------------------------

(defstruct (session (:copier nil))
  id
  server)

(defvar *session* nil
  "The MCP session the cell keeps between calls, or NIL before the handshake.")

(defvar *next-id* 0
  "The JSON-RPC id the next request carries.")

(defvar *last-call* nil
  "The plist of the last tool call, so /makernice can name it: :tool, :ok, :at.")

(defun next-id ()
  (incf *next-id*))

(defun body-text (answer)
  "ANSWER -- a string, or the octets of one -- as text."
  (typecase answer
    (string answer)
    (vector (sb-ext:octets-to-string (coerce answer '(simple-array (unsigned-byte 8) (*)))
                                     :external-format :utf-8))
    (t (princ-to-string answer))))

(defun header-value (headers name)
  "NAME's value in HEADERS, a dexador table or an alist, or NIL."
  (cond ((hash-table-p headers)
         (or (gethash name headers) (gethash (string-downcase name) headers)))
        ((listp headers)
         (cdr (assoc name headers :test #'string-equal)))))

(defun request-headers (session)
  "The headers every request carries: the bearer, what it accepts, and the
session the handshake issued."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "mcp-protocol-version" +protocol-version+)
                (cons "authorization" (format nil "Bearer ~a" (or (token) ""))))
          (let ((id (and session (session-id session))))
            (when (and id (plusp (length id))) (list (cons "mcp-session-id" id))))))

(defun message (id method params)
  "One JSON-RPC message; ID NIL makes it a notification."
  (let ((object (nlk:json-object "jsonrpc" "2.0" "method" method)))
    (when id (setf (gethash "id" object) id))
    (when params (setf (gethash "params" object) params))
    object))

(defun post (object session)
  "OBJECT POSTed => (values TEXT STATUS HEADERS)."
  (let ((answer nil) (status nil) (headers nil)
        (timeout (setting :request-timeout))
        (url (endpoint)))
    (handler-case
        (multiple-value-setq (answer status headers)
          (nlk:http :post url
                    :headers (request-headers session)
                    :content (nlk:encode-json-object object)
                    :timeout timeout
                    :connect-timeout (min 10 timeout)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" url (nle:transport-failure-label condition url))))
    (values (if (stringp answer) answer (body-text answer)) status headers)))

(defun decode-message (text)
  "TEXT decoded as a JSON object, or NIL when it is not one."
  (let ((value (and text
                    (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
                    (handler-case (nlk:decode-json text) (error () nil)))))
    (and (hash-table-p value) value)))

(defun event-messages (text)
  "Every JSON message in TEXT -- an event stream -- in the order they came."
  (let ((messages '()) (data '()))
    (labels ((flush ()
               (when data
                 (let ((value (decode-message (format nil "~{~a~^~%~}" (nreverse data)))))
                   (when value (push value messages)))
                 (setf data '()))))
      (with-input-from-string (in text)
        (loop for line = (read-line in nil nil)
              while line
              do (let ((line (string-right-trim '(#\Return) line)))
                   (cond ((zerop (length line)) (flush))
                         ((uiop:string-prefix-p "data:" line)
                          (push (string-left-trim " " (subseq line 5)) data)))))
        (flush)))
    (nreverse messages)))

(defun reply-messages (text content-type)
  "The JSON-RPC messages TEXT carries: one body, a batch, or an event stream."
  (cond ((and content-type (search "text/event-stream" content-type))
         (event-messages text))
        (t (let ((value (and text
                             (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
                             (handler-case (nlk:decode-json text) (error () nil)))))
             (cond ((hash-table-p value) (list value))
                   ((vectorp value) (loop for entry across value
                                          when (hash-table-p entry) collect entry))
                   (t '()))))))

(defun reply-for (messages id)
  "The reply in MESSAGES whose id is ID."
  (find-if (lambda (entry)
              (and (nth-value 1 (gethash "id" entry))
                   (equal id (gethash "id" entry))))
           messages))

(defun rpc-error-text (reply)
  "REPLY's error, in words, or NIL when it carries none."
  (let ((error (and reply (gethash "error" reply))))
    (when (hash-table-p error)
      (or (nlk:json-value error :string "message")
          (nlk:json-value error :string "code")
          "the server refused the call"))))

(defun short-text (text &optional (limit 200))
  "TEXT cut to LIMIT characters, for one line."
  (if (and (stringp text) (> (length text) limit))
      (concatenate 'string (subseq text 0 limit) "...")
      (or text "")))

(defun check-status (status text)
  "Refuse a status the server should not have answered, naming what to do."
  (cond ((= status 401)
         (fail "~a refused the token from ~a: sign in to Makernice again and put the new access token there"
               (endpoint) (token-source)))
        ((>= status 300)
         (fail "~a answered HTTP ~d: ~a" (endpoint) status (short-text text)))))

(defun initialize-params ()
  (nlk:json-object "protocolVersion" +protocol-version+
                    "capabilities" (nlk:json-object)
                    "clientInfo" (nlk:json-object "name" "nodecode" "version" "0.1.0")))

(defun open-session ()
  "The initialize handshake => the SESSION the cell keeps for its calls."
  (setf *session* nil)
  (let ((id (next-id)))
    (multiple-value-bind (text status headers)
        (post (message id "initialize" (initialize-params)) nil)
      (check-status status text)
      (let* ((reply (reply-for (reply-messages text (header-value headers "content-type")) id))
             (error (rpc-error-text reply)))
        (when error (fail "~a refused the handshake: ~a" (endpoint) error))
        (setf *session* (make-session :id (header-value headers "mcp-session-id")
                                       :server (and reply
                                                    (nlk:json-value reply :string "result" "serverInfo" "name"))))))
    (let ((text nil) (status nil))
      (multiple-value-setq (text status)
        (post (message nil "notifications/initialized" nil) *session*))
      (unless (or (null status) (< status 300))
        (fail "~a answered HTTP ~d to the handshake's notification" (endpoint) status)))
    *session*))

(defun rpc (method params &key (retry t))
  "METHOD with PARAMS on the cell's session => the reply's RESULT."
  (let* ((session (or *session* (open-session)))
         (id (next-id)))
    (multiple-value-bind (text status headers)
        (post (message id method params) session)
      (cond
        ((= status 404)
         ;; The server forgot the session: hand it a new one and ask once more.
         (if retry
             (progn (setf *session* nil)
                    (rpc method params :retry nil))
             (fail "~a answered HTTP 404 to ~a twice" (endpoint) method)))
        (t
         (check-status status text)
         (let* ((reply (reply-for (reply-messages text (header-value headers "content-type")) id))
                (error (rpc-error-text reply)))
           (when error (fail "~a: ~a" method error))
           (unless reply (fail "~a answered nothing for ~a" (endpoint) method))
           (gethash "result" reply)))))))

(defun content-text (result)
  "RESULT's content blocks as text, or the result itself when it carries none."
  (let ((parts '()))
    (loop for item across (or (nlk:json-value result :array "content") #())
          for text = (nlk:json-value item :string "text")
          when text do (push text parts))
    (if parts
        (format nil "~{~a~^~%~}" (nreverse parts))
        (nlk:encode-json-object (or result (nlk:json-object))))))

(defun call-remote (name arguments)
  "Server tool NAME with ARGUMENTS -- a JSON object -- => (values TEXT ERROR-P)."
  (let* ((result (rpc "tools/call" (nlk:json-object "name" name
                                                      "arguments" (or arguments (nlk:json-object)))))
         (text (content-text result)))
    (values text (and (nlk:json-value result :boolean "isError") t))))

;;; --- the inputs the model hands over --------------------------------------------

(defun argument-value (args name)
  "ARGS' NAME."
  (and (hash-table-p args) (gethash name args)))

(defun argument-absent-p (value)
  (or (null value)
      (and (stringp value) (zerop (length (string-trim '(#\Space) value))))))

(defun argument-object (args name &key (required t))
  "ARGS' NAME as the JSON object it must be, or a refusal naming it."
  (let ((value (argument-value args name)))
    (cond ((hash-table-p value) value)
          ((argument-absent-p value)
           (if required
               (fail "~a is required: the generation parameters as an object, for example {model, mode, prompt}"
                     name)
               (nlk:json-object)))
          (t (fail "~a must be an object, not ~a" name (type-of value))))))

(defun printable-id-p (text)
  "Whether TEXT is a request_id the server takes: 1 to 64 printable
characters, no space."
  (and (stringp text)
       (plusp (length text))
       (<= (length text) 64)
       (every (lambda (char) (<= 33 (char-code char) 126)) text)))

(defun approved-cost (args)
  "ARGS' max_cost_usd as a number the server takes, or a refusal: the price
the user approved, which the job is refused above."
  (let ((value (argument-value args "max_cost_usd")))
    (cond ((and (numberp value) (plusp value)) value)
          ((and (stringp value)
                (plusp (length (string-trim '(#\Space) value)))
                (ignore-errors (parse-number (string-trim '(#\Space) value)))))
          ((argument-absent-p value)
           (fail "max_cost_usd is required: the amount_usd the user approved from estimate_cost"))
          (t (fail "max_cost_usd must be a price in USD, not ~a" value)))))

(defun parse-number (text)
  "TEXT as a number, or NIL."
  (let ((*read-eval* nil))
    (multiple-value-bind (value end)
        (read-from-string text nil nil)
      (and (numberp value) (= end (length text)) value))))

(defun request-id (args)
  "ARGS' request_id, the id a retry repeats so a job is never charged twice."
  (let ((value (argument-value args "request_id")))
    (cond ((argument-absent-p value)
           (fail "request_id is required: a printable id of your own, at most 64 characters, repeated on a retry"))
          ((printable-id-p value) value)
          (t (fail "request_id must be 1 to 64 printable characters with no space, not ~s" value)))))

(defun server-arguments (args names)
  "A JSON object with only NAMES of ARGS, for the tools that pass their
arguments straight through."
  (let ((object (nlk:json-object)))
    (dolist (name names object)
      (let ((value (argument-value args name)))
        (unless (argument-absent-p value)
          (setf (gethash name object) value))))))

;;; --- the tools ------------------------------------------------------------------

(defun run-tool (server-name arguments-thunk)
  "Server tool SERVER-NAME with the object ARGUMENTS-THUNK builds => the text
the model reads; a refusal, from this side or the server's, is the ERROR line."
  (handler-case
      (multiple-value-bind (text error-p) (call-remote server-name (funcall arguments-thunk))
        (setf *last-call* (list :tool server-name :ok (not error-p) :at (get-universal-time)))
        (if error-p
            (nle:failure (format nil "ERROR: makernice ~a: ~a" server-name (short-text text 600)))
            text))
    ((or error sb-sys:deadline-timeout) (condition)
      (setf *last-call* (list :tool server-name :ok nil :at (get-universal-time)))
      (nle:failure (format nil "ERROR: makernice: ~a" condition)))))

(defun tool-list-models (args)
  (run-tool "list_models" (lambda () (server-arguments args '("model")))))

(defun tool-estimate-cost (args)
  (run-tool "estimate_cost" (lambda () (nlk:json-object "input" (argument-object args "input")))))

(defun tool-generate (args)
  (run-tool "generate"
            (lambda ()
              (nlk:json-object "input" (argument-object args "input")
                                "max_cost_usd" (approved-cost args)
                                "request_id" (request-id args)))))

(defun tool-check-job (args)
  (run-tool "check_job" (lambda ()
                          (let ((job (argument-value args "job_id")))
                            (when (argument-absent-p job)
                              (fail "job_id is required: the id generate answered"))
                            (server-arguments args '("job_id" "wait_s"))))))

(defun tool-cancel-job (args)
  (run-tool "cancel_job" (lambda ()
                           (let ((job (argument-value args "job_id")))
                             (when (argument-absent-p job)
                               (fail "job_id is required: the id generate answered"))
                             (server-arguments args '("job_id"))))))

(defun tool-list-trends (args)
  (declare (ignore args))
  (run-tool "list_trends" #'nlk:make-json-object))

(defun tool-get-balance (args)
  (declare (ignore args))
  (run-tool "get_balance" #'nlk:make-json-object))

(defun tool-get-topup-link (args)
  (declare (ignore args))
  (run-tool "get_topup_link" #'nlk:make-json-object))

;;; --- the schemas ----------------------------------------------------------------

(defun field (type description &rest extra)
  "One property of a tool's input schema."
  (apply #'nlk:make-json-object (append (list "type" type "description" description) extra)))

(defun schema (description properties &optional required)
  "A tool's (:description ... :parameters ...) from PROPERTIES, (NAME . OBJECT)."
  (list :description description
        :parameters (apply #'nlk:make-json-object
                           (append (list "type" "object"
                                         "properties"
                                         (apply #'nlk:make-json-object
                                                (loop for (name . object) in properties
                                                      append (list name object))))
                                   (when required
                                     (list "required" (coerce required 'vector)))))))

(defparameter *list-models-schema*
  (schema (concatenate 'string
                       "The Makernice video and image models with their modes, resolutions, "
                       "durations and unit prices in USD. Pass model= one id for its full "
                       "parameters before estimate_cost or generate. Read-only.")
          (list (cons "model" (field "string" "A model id to get the full parameters of.")))))

(defparameter *estimate-cost-schema*
  (schema (concatenate 'string
                       "Price a Makernice generation in USD without creating or charging anything: "
                       "compare models, resolutions and durations before generate. input= the "
                       "parameters as an object, {model, mode, prompt, duration, resolution, "
                       "aspect_ratio, image_url, last_image_url, reference_images, seed, "
                       "negative_prompt, watermark, ...}, or {template, images} for a trend "
                       "template. It answers amount_usd: show it to the user and use it as "
                       "max_cost_usd.")
          (list (cons "input" (field "object"
                                      (concatenate 'string
                                                   "The generation parameters. Either model parameters "
                                                   "with model, mode (t2v, i2v, flf, r2v, t2i or i2i) and "
                                                   "prompt required, or {template, images} with a "
                                                   "template id from makernice_list_trends and the "
                                                   "https urls of the photos."))))
          '("input")))

(defparameter *generate-schema*
  (schema (concatenate 'string
                       "Start a Makernice video or image job and charge the locked price from the "
                       "user's balance. Call estimate_cost first, show the amount_usd, and only "
                       "call this after the user agrees. The job is refused if the price is above "
                       "max_cost_usd. It answers a job_id: poll makernice_check_job. Reference "
                       "images must be public https urls.")
          (list (cons "input" (field "object"
                                      (concatenate 'string
                                                   "The generation parameters, exactly as estimate_cost "
                                                   "priced them: {model, mode, prompt, duration, ...} or "
                                                   "{template, images}.")))
                (cons "max_cost_usd" (field "number"
                                             "The maximum price in USD the user accepted: the amount_usd estimate_cost answered."))
                (cons "request_id" (field "string"
                                           (concatenate 'string
                                                        "A printable id of your own, at most 64 characters, no spaces. "
                                                        "A retry with the same id returns the same job and is never charged twice."))))
          '("input" "max_cost_usd" "request_id")))

(defparameter *check-job-schema*
  (schema (concatenate 'string
                       "The status, result links and charged amount of a Makernice job. Set wait_s "
                       "to wait for it to finish before answering (55 is the most); call it again "
                       "with 55 rather than polling at once. Result links expire after 24 hours.")
          (list (cons "job_id" (field "string" "The job_id generate answered."))
                (cons "wait_s" (field "integer" "Seconds to wait for completion, 0 to 55."
                                      "minimum" 0 "maximum" 55)))
          '("job_id")))

(defparameter *cancel-job-schema*
  (schema (concatenate 'string
                       "Cancel a Makernice job that is still queued and release the held amount in "
                       "full. A job that has started generating cannot be canceled.")
          (list (cons "job_id" (field "string" "The job_id generate answered.")))
          '("job_id")))

(defparameter *list-trends-schema*
  (schema (concatenate 'string
                       "The trending Makernice video templates -- preset effects that turn the "
                       "user's photos into a video -- with their total price in USD per "
                       "resolution, for generate with {template, images}.")
          '()))

(defparameter *get-balance-schema*
  (schema (concatenate 'string
                       "The Makernice balance in USD: available_usd, what running jobs hold, "
                       "today's spending, the daily limit left and this connection's per-job limit.")
          '()))

(defparameter *get-topup-link-schema*
  (schema "The link to the Makernice wallet page where the user adds balance themselves."
          '()))

;;; --- the verb called through eval --------------------------------------------------

(defun plist-key-name (key)
  "KEY as the wire name it stands for: :max-cost-usd is max_cost_usd."
  (string-downcase (substitute #\_ #\- (string key))))

(defun plist-value (value)
  "VALUE with every plist inside it -- one whose keys are keywords -- turned
into the JSON object it stands for, and every other list into a vector: what
a plist argument means on the wire."
  (cond ((and (consp value) (keywordp (first value)))
         (let ((object (nlk:json-object)))
           (loop for (key item) on value by #'cddr
                 do (setf (gethash (plist-key-name key) object) (plist-value item)))
           object))
        ((listp value) (coerce (mapcar #'plist-value value) 'vector))
        (t value)))

(defun coerce-arguments (arguments)
  "ARGUMENTS -- a JSON object, a plist, or nothing -- as a JSON object."
  (cond ((hash-table-p arguments) arguments)
        ((null arguments) (nlk:json-object))
        ((listp arguments) (plist-value arguments))
        (t (fail "arguments must be a plist or a JSON object, not ~a" (type-of arguments)))))

(defun call (tool &optional arguments)
  "The Makernice tool TOOL -- its server name, as in \"estimate_cost\" -- with
ARGUMENTS, a plist or a JSON object => the text the server answers. A refusal
signals MAKERNICE-ERROR."
  (running-settings)
  (let ((object (coerce-arguments arguments)))
    (multiple-value-bind (text error-p) (call-remote tool object)
      (setf *last-call* (list :tool tool :ok (not error-p) :at (get-universal-time)))
      (if error-p
          (fail "~a: ~a" tool text)
          text))))

(defun account ()
  "The Makernice balance and limits, as the server words them."
  (call "get_balance"))

;;; --- /makernice -------------------------------------------------------------------

(defun report ()
  "What the cell runs with, in lines, without touching the network."
  (with-output-to-string (out)
    (format out "makernice: ~a~%  token: ~a~%  session: ~a~%"
            (endpoint) (token-source)
            (if (and *session* (session-id *session*))
                (format nil "open (~a)" (or (session-server *session*) "unnamed server"))
                "none yet: the first call runs the handshake"))
    (format out "  tools: ~{~a~^ ~}~%"
            (loop for name in +server-tools+ collect (concatenate 'string "makernice_" name)))
    (format out "  last call: ~a~%"
            (if *last-call*
                (format nil "~a ~a" (getf *last-call* :tool)
                        (if (getf *last-call* :ok) "ok" "failed"))
                "none yet: (makernice:call \"estimate_cost\" (list :input ...)) prices one"))))

(defun run-command (args session-id)
  "One /makernice: what the cell runs with; /makernice balance asks the server."
  (declare (ignore session-id))
  (let ((argument (string-downcase (or (nlk:trimmed (or args "")) ""))))
    (if (string= argument "balance")
        (handler-case (account) (error (condition) (format nil "makernice: ~a" condition)))
        (report))))

;;; --- the entry: the one declaration ------------------------------------------------

(defun start ()
  "Refuse a section with no token, and forget the session on stop."
  (check-config)
  (setf *session* nil *last-call* nil)
  (nle:on-stop (lambda () (setf *session* nil *last-call* nil))))

(nle:define-cell makernice
  (:section ("makernice")
    (:guide "paste the Makernice access token into token, or name the variable holding it in token_env; next step: /makernice shows the endpoint, where the token came from and the last call")
    ("url" :string :default +url-default+
     :doc "the Makernice MCP server this cell talks to")
    ("token" :secret
     :doc "the Makernice access token itself, when it is not in the environment")
    ("token_env" :env :default +token-env-default+
     :doc "the environment variable holding the token")
    ("token_file" :path
     :doc "a file holding the token, read again at every call so a rotated token takes effect")
    ("request_timeout" :integer :default 30 :min 1
     :doc "one request's ceiling, connect included; check_job's wait_s sits inside it"))
  (:help :makernice
         "makernice: priced video and image generation - makernice_estimate_cost, then makernice_generate after the user agrees"
         +primer+)
  (:command "makernice" 'run-command
            :description "Show the Makernice endpoint, the token it read and the last call"
            :argument-hint "BALANCE")
  (:tool "makernice_list_models" *list-models-schema* 'tool-list-models)
  (:tool "makernice_estimate_cost" *estimate-cost-schema* 'tool-estimate-cost)
  (:tool "makernice_generate" *generate-schema* 'tool-generate)
  (:tool "makernice_check_job" *check-job-schema* 'tool-check-job)
  (:tool "makernice_cancel_job" *cancel-job-schema* 'tool-cancel-job)
  (:tool "makernice_list_trends" *list-trends-schema* 'tool-list-trends)
  (:tool "makernice_get_balance" *get-balance-schema* 'tool-get-balance)
  (:tool "makernice_get_topup_link" *get-topup-link-schema* 'tool-get-topup-link)
  (:start #'start))
