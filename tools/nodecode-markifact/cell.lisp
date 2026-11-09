;;;; cell.lisp --- the declaration: two tools, one command, one manual topic.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this is ported from ships no code. It points an MCP client at
;;;; one hosted server and teaches it a trade: ten skills of performance
;;;; marketing, seven of them a workflow, and one four-step protocol before
;;;; anything that spends money. Here the server's eight tools are reached
;;;; through two of the model's own, and the trade is the (help :markifact)
;;;; topic the model reads before it calls them.

(in-package #:nodecode-markifact)

(defparameter +primer+
  "Markifact runs real advertising accounts from chat, through one hosted server the nodecode-markifact
cell speaks to. It covers Google Ads, Meta / Instagram, TikTok, LinkedIn, Pinterest, Snapchat,
Microsoft, Amazon and Reddit Ads, GA4, Shopify, Klaviyo and HubSpot, as some 1000 operations.
The model has two tools:
  markifact_tools                                   what the server exposes now: every tool's
      name, its purpose and the input schema its arguments follow.
  markifact_call  tool, arguments                   call one of them; arguments is exactly what
      that schema names. (markifact:call-tool \"find_operations\" arguments) is the same from eval.
The eight tools, and what each is for:
  find_operations        find operations by intent, in plain English (\"create google search
      campaign\", \"pull ga4 conversions last 7 days\"). Read the readOnlyHint and requires_approval
      it answers with.
  get_operation_inputs   an operation's required and optional fields, with examples. Never guess
      an input shape; inspect it.
  run_operation          run one with requires_approval false.
  run_write_operation    run one with requires_approval true - only after an explicit yes.
  list_connections       the OAuth logins on the account, only when a platform has several or the
      operator names one.
  get_file_url, read_file, upload_media   a signed url for a stored file, its contents, and an
      image or video as a creative asset.
Every request that touches a platform follows this order: discover with find_operations, inspect
with get_operation_inputs, resolve the account (a connection can hold many ad accounts, so call
the platform's *_select_accounts first; names match as substrings, and ask when several match),
and for a report call *_list_report_fields before *_get_report. Then run, then verify by fetching
the object's current state.
Writes spend real budgets. Before run_write_operation say what changes, how far it reaches, and
what it costs, then wait for an explicit yes in this turn; never more than five writes without
asking again; one platform per operation. Prefer a dedicated operation over a generic mutate
where one exists. Auth errors are not retried: point the operator at the Markifact app's
connections page. Workflows, each a skill in the original plugin: launch-google-search,
launch-pmax, launch-meta-campaign (all built paused for review), edit-meta-creative,
rotate-creative (pause the fatigued, ship variants paused), negative-keyword-sweep (find wasted
spend in search terms, add negatives), and diagnose-underperformer (read-only). /markifact
workflow NAME prints the steps of one, and /markifact login signs in."
  "What (help :markifact) answers while the cell runs.")

(defparameter +workflows+
  '(("launch-google-search"
     "1. find_operations \"create google search campaign\"; 2. get_operation_inputs for the
campaign, ad group, keywords and ads operations it names; 3. *_select_accounts for the Google Ads
account; 4. create the campaign paused, then the ad group, keywords, ads; 5. verify each by
fetching it. Budget, geo, language and bidding are inputs, not guesses: ask for what is missing.")
    ("launch-pmax"
     "1. find_operations \"create performance max campaign\"; 2. inspect the campaign and asset
group operations; 3. resolve the account; 4. create paused, then the asset group with its text,
image and logo assets; 5. verify. Ask for the final url, the business name and the asset copy.")
    ("launch-meta-campaign"
     "1. find_operations \"create meta campaign ad set ad\"; 2. inspect each; 3. resolve the ad
account; 4. create the campaign, the ad set and the ad paused; 5. verify. There is no generic
Meta update operation: pick one per concern.")
    ("edit-meta-creative"
     "1. find the existing ad and read the creative it uses; 2. create a new creative with the
change; 3. replace the ad's creative with it; 4. verify the ad shows the new creative. The old
creative is left in place, not deleted.")
    ("rotate-creative"
     "1. read the ads of the campaign; 2. name the fatigued ones and pause them; 3. create the
variants paused; 4. verify. Say which ads pause before pausing them.")
    ("negative-keyword-sweep"
     "1. *_list_report_fields, then the search-term report for the window; 2. find the terms with
spend and no conversions; 3. add them as negatives at the right scope (campaign, not account,
unless asked); 4. verify. Show the terms and their spend first.")
    ("diagnose-underperformer"
     "Read-only. Compare the window with the one before it: impressions, clicks, cost, conversions,
CTR, CPA. Then check, in this order: delivery (budget, bidding, status), audience and frequency,
creative fatigue, landing page, and tracking. Answer with the one cause the data supports and the
next operation to run."))
  "The plugin's seven workflow skills, condensed to their steps.")

;;; --- the boundary the model crosses ------------------------------------------------

(nlk:define-contract markifact-call
  (:doc "Call one of Markifact's MCP tools by name, with the arguments its input schema names.
markifact_tools lists them; find_operations and get_operation_inputs are the two to start with.")
  ("tool" :text :required t
          :doc "the MCP tool's name: find_operations, get_operation_inputs, run_operation,
run_write_operation, list_connections, get_file_url, read_file or upload_media")
  ("arguments" :object :required t
               :doc "the arguments that tool's input schema names, as an object; {} when it takes none"))

(nlk:define-contract markifact-tools-query
  (:doc "What Markifact's server exposes right now: every tool's name, its purpose and its input
schema. Ask once per conversation before the first call you are unsure of.")
  ("filter" :string :default ""
            :doc "a word to keep only the tools whose name or purpose carries it; empty lists them all"))

;;; --- the two tools ----------------------------------------------------------------

(defun tool-call (query)
  "The markifact_call tool: one MCP tool by name, answering its text."
  (call-tool (markifact-call-tool query) (markifact-call-arguments query)))

(defun tool-tools (query)
  "The markifact_tools tool: the server's tools, their purposes and schemas,
 narrowed to the ones the query's filter names."
  (tools (markifact-tools-query-filter query)))

;;; --- the command -------------------------------------------------------------------

(defun workflow-text (name)
  "The steps the plugin's NAME skill carries, or NIL."
  (let ((entry (assoc name +workflows+ :test #'string-equal)))
    (and entry (format nil "/markifact ~a - ~a" (first entry) (second entry)))))

(defun panel ()
  "The dialog bare /markifact answers."
  (nle:list-dialog
   :title "Markifact" :summary (status)
   :rows (list (list :label "Sign in" :run (list :enter "/markifact login")
                     :detail "in the browser, with OAuth; no key to paste")
               (list :label "Paste where the browser landed"
                     :ask (list :label "Address or code" :line "/markifact code"
                                :placeholder "the address the browser landed on" :verb "sign in")
                     :detail "when the browser cannot reach this machine")
               (list :label "Its tools" :run (list :enter "/markifact tools")
                     :detail "names, purposes and input schemas")
               (list :label "Sign out" :run (list :enter "/markifact logout")
                     :detail "forget the kept token"))))

(defun run-command (args session-id)
  "/markifact status|login|code TEXT|logout|tools|workflow NAME|call NAME {JSON}; alone,
 its panel."
  (declare (ignore session-id))
  (running-settings)
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (verb (if space (subseq text 0 space) text))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond ((zerop (length verb)) (values (format nil "~a~%~%(help :markifact) teaches the tools to the model." (status))
                                          (panel)))
          ((string-equal verb "status") (status))
          ((string-equal verb "login") (start-login))
          ((string-equal verb "code") (paste rest))
          ((string-equal verb "logout") (logout))
          ((string-equal verb "tools") (tools))
          ((string-equal verb "workflow")
           (or (workflow-text rest)
               (format nil "/markifact workflow: ~a is not one of ~{~a~^, ~}; the seven are ~{~a~^, ~}."
                       (if (plusp (length rest)) rest "(nothing)")
                       (mapcar #'first +workflows+) (mapcar #'first +workflows+))))
          ((string-equal verb "call")
           (let* ((space2 (position #\Space rest))
                  (name (if space2 (subseq rest 0 space2) rest))
                  (json (if space2 (nlk:trimmed (subseq rest space2)) "")))
             (if (zerop (length name))
                 "Give the tool to call: /markifact call find_operations {\"query\": \"list campaigns\"}"
                 (let ((arguments (if (zerop (length json))
                                      (nlk:make-json-object)
                                      (or (ignore-errors (nlk:decode-json json)) (nlk:make-json-object)))))
                   (call-tool name arguments)))))
          (t (or (workflow-text text)
                 (format nil "/markifact: ~a is not one of its choices (status, login, code, logout, tools, workflow, call)"
                         verb))))))

;;; --- start and stop -----------------------------------------------------------------

(defun start ()
  "Refuse without an endpoint, and forget the last session and any sign-in left over."
  (unless (plusp (length (nlk:trimmed (or (setting :endpoint) ""))))
    (fail "the markifact section names no endpoint: set markifact.endpoint to ~a"
          +default-endpoint+))
  (setf *session-id* nil *initialized* nil *flow* nil)
  (nle:on-stop (lambda () (setf *session-id* nil *initialized* nil *flow* nil))))

(nle:define-cell markifact
  (:section ("markifact")
    (:guide "endpoint is where Markifact's MCP server lives, https://api.markifact.com/mcp; a token is optional -- /markifact login signs in with OAuth 2.1, or set access_token, or token_env naming a variable that holds one")
    ("endpoint" :string :required t
                :doc "Markifact's MCP endpoint: https://api.markifact.com/mcp")
    ("access_token" :secret :default ""
                    :doc "an access token to send as the bearer, when no sign-in is kept")
    ("token_env" :env :default ""
                 :doc "the name of a variable holding an access token")
    ("timeout" :integer :default 60 :min 5
               :doc "seconds one exchange with Markifact may take")
    ("max_chars" :integer :default 20000 :min 500
                 :doc "how much of one answer the model is given"))
  (:tool "markifact_call" (nlk:contract 'markifact-call) 'tool-call)
  (:tool "markifact_tools" (nlk:contract 'markifact-tools-query) 'tool-tools)
  (:help :markifact "markifact: run Google, Meta, TikTok and other ad operations through Markifact's hosted MCP server" +primer+)
  (:command "markifact" 'run-command
            :description "Markifact: sign in, list its tools, or call one"
            :argument-hint "LOGIN | CODE | TOOLS | WORKFLOW NAME | CALL NAME {JSON}")
  (:start #'start))
