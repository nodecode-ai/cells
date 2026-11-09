;;;; verbs.lisp --- the three calls a model makes, and the operator's own door.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the original's MCP server offered in about sixty tools, this cell
;;;; offers in three: read the account, make any documented REST call, and
;;;; send a signal to a bot webhook. The first and the second are the same
;;;; requests the MCP server makes; the third is the one the skills called
;;;; the dangerous one, so it carries their rules -- TEST unless you mean it,
;;;; and say so in the answer.

(in-package #:nodecode-sigrex)

;;; --- the boundaries: what a model call carries -------------------------------

(nlk:define-contract strategies-query
  (:doc "List the trading automation on the signed-in Sigrex account: its Code
Strategies or its LLM Sessions. Read-only; nothing it does can trade.")
  ("kind" :text :default "code" :options (quote ("code" "llm"))
   :doc "code for Code Strategies (JavaScript that runs on a schedule), llm for LLM Sessions (an AI agent on a cron schedule)"))

(nlk:define-contract api-call
  (:doc "Make one signed call to the Sigrex REST API -- any route the platform
documents, from reading a webhook to pausing a bot. A POST, PUT or DELETE changes
the account, so confirm with the operator before one that trades, activates,
deletes or shares.")
  ("method" :text :slot http-method :default "GET" :options (quote ("GET" "POST" "PUT" "DELETE"))
   :doc "the HTTP method")
  ("path" :text :required t
   :doc "the path after the host, including /api/v2 and any query string, e.g. /api/v2/webhook/bot or /api/v2/bot/signal/cex/34/status")
  ("body" :object
   :doc "the JSON object a POST or PUT sends, e.g. {\"status\": \"INACTIVE\"}; it is signed and sent as written"))

(nlk:define-contract signal-order
  (:doc "Send a trading signal to a Sigrex Bot Webhook, the way TradingView or a
script does. Every bot attached to the webhook acts on it, and a live signal can
open or close a real position: send it with test true first, and confirm with the
operator before one with test false.")
  ("hash" :text :required t
   :doc "the bot webhook's hash: the last part of its https://hook.sigrex.io/bot/<hash> URL")
  ("symbol" :text :slot trade-symbol :required t
   :doc "the symbol the signal is for, which must equal the bot's own, e.g. BTCUSDT or BTCUSDC on Hyperliquid")
  ("side" :text :required t :options (quote ("BUY" "SELL"))
   :doc "BUY opens a long or closes a short; SELL closes a long or opens a short")
  ("size" :number
   :doc "quote currency when opening, base currency when closing; omit for the bot's own amount")
  ("key" :text
   :doc "the webhook's secret, when it is key-protected")
  ("test" :boolean :default t
   :doc "true sends flag [\"TEST\"] so the bots do not trade; false is a live signal")
  ("id" :string
   :doc "a label for the signal, kept in the bot's logs"))

;;; --- the account, read ---------------------------------------------------------

(defparameter +strategy-paths+ '(("code" . "/api/v2/strategy/code")
                                  ("llm" . "/api/v2/strategy/llm-session"))
  "Where the two lists live, as the API reference writes them.")

(defun strategies (&key kind)
  "List the account's Code Strategies or LLM Sessions, as the API answers
them. KIND is \"code\" (the default) or \"llm\"."
  (let* ((kind (string-downcase (or (nonblank kind) "code")))
         (path (or (cdr (assoc kind +strategy-paths+ :test #'string=))
                   (fail "kind is ~s: this call lists \"code\" strategies or \"llm\" sessions"
                         kind))))
    (multiple-value-bind (payload status) (request "GET" path)
      (answer (format nil "GET ~a" path) payload status))))

;;; --- the whole API, one call ------------------------------------------------------

(defun api (method path &key body)
  "One signed call to the Sigrex REST API, answered as the status and the
reply's own members. METHOD is a keyword or a string; PATH the route after the
host, including /api/v2; BODY the JSON object a POST or PUT sends."
  (multiple-value-bind (payload status) (request method path :body body)
    (answer (format nil "~a ~a" (method-of method) (api-path path)) payload status)))

;;; --- one signal, to one webhook ------------------------------------------------------

(defun signal-body (symbol side &key size key test id)
  "The JSON a Bot Webhook takes, as the webhook skill spells it: symbol and
side required, size, key and id when they are given, and the TEST flag the
call asked for."
  (let ((body (nlk:json-object "symbol" symbol "side" side)))
    (when size (setf (gethash "size" body) size))
    (when key (setf (gethash "key" body) key))
    (when id (setf (gethash "id" body) id))
    (when test (setf (gethash "flag" body) (vector "TEST")))
    body))

(defun signal (hash symbol side &key size key (test t) id)
  "Post one signal to a Bot Webhook. HASH is the webhook's hash, SYMBOL and
SIDE the trade, SIZE, KEY and ID as the payload takes them, TEST true (the
default) to send flag [\"TEST\"] and trade nothing."
  (let* ((hash (or (nonblank hash)
                    (fail "signal needs the bot webhook's hash: the last part of its hook URL")))
         (symbol (or (nonblank symbol) (fail "signal needs a symbol, e.g. BTCUSDT")))
         (side (string-upcase (or (nonblank side) (fail "signal needs a side: BUY or SELL")))))
    (unless (member side '("BUY" "SELL") :test #'string=)
      (fail "side is ~s: a signal is BUY or SELL" side))
    (let ((payload (nlk:encode-json-object (signal-body symbol side :size size :key key
                                                                  :test test :id id))))
      (multiple-value-bind (reply status)
          (http :post (format nil "~a/bot/~a" (hook-base-url) hash)
                :headers (list (cons "content-type" "application/json"))
                :content payload)
        (answer (format nil "~a ~a to bot webhook ~a, ~a"
                        side symbol (cut hash 8)
                        (if test "TEST (no trade)" "a live signal"))
                reply status)))))

;;; --- the tools the model calls -----------------------------------------------------

(defun strategies-tool (query)
  "The sigrex_strategies tool: the list the call asked for."
  (strategies :kind (strategies-query-kind query)))

(defun api-tool (call)
  "The sigrex_api tool: the route, the method and the body one call carries."
  (api (api-call-http-method call) (api-call-path call) :body (api-call-body call)))

(defun signal-tool (order)
  "The sigrex_signal tool: one signal, with the members one call carries."
  (signal (signal-order-hash order)
          (signal-order-trade-symbol order)
          (signal-order-side order)
          :size (signal-order-size order)
          :key (signal-order-key order)
          :test (signal-order-test order)
          :id (signal-order-id order)))

;;; --- the operator's own door --------------------------------------------------------

(defun redacted (secret)
  "SECRET as the skills say to show a credential: its first four characters."
  (let ((secret (or (nonblank secret) "")))
    (if (> (length secret) 4) (format nil "~a..." (subseq secret 0 4)) "(unset)")))

(defun status-line ()
  "What /sigrex says with no argument: where it points, and what it gives."
  (format nil "sigrex: ~a, signed as api-key ~a. Three tools: sigrex_strategies (read the account), ~
sigrex_api (any documented route), sigrex_signal (one signal to a bot webhook, TEST by default). ~
Try /sigrex GET /api/v2/strategy/code, or ask for your strategies in words."
          (base-url) (redacted (setting :api-key))))

(defun run-slash (args session-id)
  "/sigrex METHOD PATH [JSON]: one signed call of the operator's own, answered
as the model's tools answer. With no argument it says what the cell gives."
  (declare (ignore session-id))
  (let ((words (and (nonblank args)
                    (remove "" (uiop:split-string (nonblank args) :separator '(#\Space))
                            :test #'string=))))
    (if (null words)
        (status-line)
        (destructuring-bind (method &optional path body) words
          (api method path :body (and body (payload-of body)))))))
