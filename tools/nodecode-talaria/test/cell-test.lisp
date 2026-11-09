;;;; cell-test.lisp --- the talaria cell against a stubbed API server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every /talaria/v1 route is a stubbed NLK:HTTP and the key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches a server, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it --
;;;; through the real dispatch, contract and all -- so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "talaria" "TALARIA-CELL-")

(defun talaria-config (&rest pairs)
  "A whole shared config holding the `talaria' section PAIRS make, with its
url and key variable defaulted."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "url" section "http://talaria.test:8080")
    (alexandria:ensure-gethash "key_env" section "TALARIA_KEY")
    (nlk:json-object "talaria" section)))

(defun talaria-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-talaria:start-cell (apply #'talaria-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-talaria-key (&body body)
  "BODY with the key environment answering one fixed key."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "TALARIA_KEY") "test-key"))
     ,@body))

(defmacro with-talaria-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL OPTIONS), oldest first,
once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url options) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected talaria request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defparameter +capabilities+
  (concatenate
   'string
   "{\"version\":1,\"revision\":\"f00dfeed\",\"plugin_version\":\"1.5.2\","
   "\"rewind\":true,\"history_search\":true,\"response_details\":true,"
   "\"commands\":[{\"name\":\"help\",\"mode\":\"web\"}],"
   "\"agent\":{\"name\":\"Talaria\"}}")
  "What GET /talaria/v1/capabilities answers.")

(defparameter +matches+
  (concatenate
   'string
   "{\"data\":[{\"id\":7,\"session_id\":\"20260101_ab12\",\"role\":\"user\","
   "\"snippet\":\"the kitchen light dimmer\",\"timestamp\":1767225600,"
   "\"title\":\"Kitchen wiring\"},"
   "{\"id\":8,\"session_id\":\"20260101_ab12\",\"role\":\"assistant\","
   "\"snippet\":\"the kitchen light drew 180\",\"timestamp\":1767225660,"
   "\"title\":\"Kitchen wiring\"}],\"has_more\":true,\"next_offset\":20}")
  "Two matches, as GET /talaria/v1/search answers them.")

(defparameter +reply+
  (concatenate
   'string
   "{\"message_id\":42,\"timestamp\":1767225660,\"finish_reason\":\"stop\","
   "\"has_reasoning\":true,\"status\":\"completed\",\"model\":\"claude-sonnet-4\","
   "\"provider\":\"anthropic\",\"usage\":{\"input_tokens\":1200,\"output_tokens\":340}}")
  "One observed reply, as GET /talaria/v1/sessions/<id>/response answers it.")

(defparameter +context+
  "{\"context\":{\"used\":1200,\"maximum\":200000,\"model\":\"claude-sonnet-4\",\"observed_at\":1767225660}}"
  "The context reading for a session that has replied once.")

(defun talaria-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun talaria-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (talaria-answer id tool arguments)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "talaria"
  (:config "key_env" "TALARIA_KEY")
  (:help :talaria)
  (:fixture with-talaria-key)
  (:refused ("url" 5) ("key_env" 5)))

(deftest talaria-cell-without-a-key-refuses ()
  ;; The one setting no verb can work without: the section names a key
  ;; variable, and one that the environment does not answer refuses at start.
  (let ((condition (signals-error nlk:config-refusal
                                  (with-talaria-key
                                    (talaria-start "key_env" "TALARIA_UNSET_KEY")))))
    (is (search "TALARIA_UNSET_KEY" (princ-to-string condition))
        "the refusal names the variable that answered nothing"))
  (is (null (nle::find-tool "talaria_capabilities")) "and installs nothing"))

(deftest talaria-cell-refuses-a-url-without-a-scheme (with-talaria-key)
  (let ((condition (signals-error nlk:config-refusal
                                  (talaria-start "url" "talaria.test:8080"))))
    (is (search "http" (princ-to-string condition)) "the refusal says what the url lacks")))

;;; --- talaria_capabilities -----------------------------------------------------------

(deftest talaria-cell-reads-what-the-server-supports (with-talaria-key)
  (with-cell-stop ((talaria-start))
    (with-talaria-wire (asked ("/talaria/v1/capabilities" (values +capabilities+ 200)))
      (let ((object (talaria-object "tal-1" "talaria_capabilities" "{}")))
        (is (equal "1.5.2" (nlk:json-value object :string "plugin_version")))
        (is (eq t (nlk:json-value object :boolean "history_search"))
            "the summary says where searching is on")
        (is (= 1 (nlk:json-value object :integer "commands")))
        (is (equal "Talaria" (nlk:json-value object :string "agent_name")))
        (is (= 1 (length asked)) "a capability read is one request")
        (is (equal :get (first (first asked))))
        (is (equal "http://talaria.test:8080/talaria/v1/capabilities"
                   (second (first asked))) "the plugin's own prefix rides the path")))))

(deftest talaria-cell-can-answer-the-whole-capability-object (with-talaria-key)
  (with-cell-stop ((talaria-start))
    (with-talaria-wire (asked ("/talaria/v1/capabilities" (values +capabilities+ 200)))
      (let ((object (talaria-object "tal-2" "talaria_capabilities" "{\"full\":true}")))
        (is (= 1 (nlk:json-value object :integer "version"))
            "full hands the server's object over as it stands")))))

;;; --- talaria_search_history -----------------------------------------------------------

(deftest talaria-cell-searches-history (with-talaria-key)
  (with-cell-stop ((talaria-start))
    (with-talaria-wire (asked ("/talaria/v1/search" (values +matches+ 200)))
      (let ((object (talaria-object "tal-3" "talaria_search_history"
                                     "{\"query\":\"kitchen light\"}")))
        (is (= 2 (nlk:json-value object :integer "count")))
        (is (equal "Kitchen wiring"
                   (nlk:json-value (aref (nlk:json-array object "results") 0)
                                   :string "title")) "a hit carries its session's title")
        (is (eq t (nlk:json-value object :boolean "has_more")))
        (is (= 20 (nlk:json-value object :integer "next_offset")) "the page's next offset comes back"))
      (let ((url (second (first asked))))
        (is (search "q=kitchen%20light" url) "the query is encoded into its slot")
        (is (search "offset=0" url) "and the first page names its offset")))))

(deftest talaria-cell-pages-a-search (with-talaria-key)
  (with-cell-stop ((talaria-start))
    (with-talaria-wire (asked ("/talaria/v1/search" (values +matches+ 200)))
      (talaria-object "tal-4" "talaria_search_history" "{\"query\":\"kitchen\",\"offset\":20}")
      (is (search "offset=20" (second (first asked))) "the caller's offset rides the query"))))

(deftest talaria-cell-refuses-an-empty-search (with-talaria-key)
  (with-cell-stop ((talaria-start))
    (with-talaria-wire (asked)
      (let ((answer (talaria-answer "tal-5" "talaria_search_history" "{}")))
        (is (search "preflight" answer) "the contract refuses it before the body"))
      (let ((answer (talaria-answer "tal-6" "talaria_search_history" "{\"query\":\"   \"}")))
        (is (search "at least one character" answer)))
      (is (null asked) "nothing was asked of the server"))))

;;; --- talaria_reply_details ------------------------------------------------------------

(deftest talaria-cell-reads-one-reply (with-talaria-key)
  (with-cell-stop ((talaria-start))
    (with-talaria-wire (asked ("/talaria/v1/sessions/20260101_ab12/response"
                               (values +reply+ 200)))
      (let ((object (talaria-object "tal-7" "talaria_reply_details"
                                     "{\"session_id\":\"20260101_ab12\",\"message_id\":42}")))
        (is (equal "claude-sonnet-4" (nlk:json-value object :string "model")))
        (is (equal "stop" (nlk:json-value object :string "finish_reason")))
        (is (= 1200 (nlk:json-value object :integer "input_tokens"))
            "the token counts come through")
        (is (eq t (nlk:json-value object :boolean "has_reasoning"))))
      (is (equal "http://talaria.test:8080/talaria/v1/sessions/20260101_ab12/response?message_id=42"
                 (second (first asked))) "the session and the message ride the path and the query"))))

(deftest talaria-cell-refuses-a-malformed-session-id (with-talaria-key)
  (with-cell-stop ((talaria-start))
    (with-talaria-wire (asked)
      (let ((answer (talaria-answer "tal-8" "talaria_reply_details"
                                     "{\"session_id\":\"2026/01/01\",\"message_id\":42}")))
        (is (search "session id" answer) "the refusal says what a session id is"))
      (is (null asked) "and no path is built from it"))))

(deftest talaria-cell-refuses-an-http-error (with-talaria-key)
  (with-cell-stop ((talaria-start))
    (with-talaria-wire (asked ("/talaria/v1/sessions/20260101_ab12/response"
                               (values "{\"error\":\"Hermes session storage is unavailable.\"}" 503)))
      (let ((answer (talaria-answer "tal-9" "talaria_reply_details"
                                     "{\"session_id\":\"20260101_ab12\",\"message_id\":42}")))
        (is (search "HTTP 503" answer) "the status is named")
        (is (search "session storage is unavailable" answer) "with the server's own words")))))

;;; --- talaria_context_usage --------------------------------------------------------------

(deftest talaria-cell-reads-context-use (with-talaria-key)
  (with-cell-stop ((talaria-start))
    (with-talaria-wire (asked ("/talaria/v1/sessions/20260101_ab12/context"
                               (values +context+ 200)))
      (let ((object (talaria-object "tal-10" "talaria_context_usage"
                                     "{\"session_id\":\"20260101_ab12\"}")))
        (is (= 1200 (nlk:json-value object :integer "used")))
        (is (= 200000 (nlk:json-value object :integer "maximum")))))))

(deftest talaria-cell-says-when-nothing-was-observed (with-talaria-key)
  (with-cell-stop ((talaria-start))
    (with-talaria-wire (asked ("/talaria/v1/sessions/20260101_cd34/context"
                               (values "{\"context\":null}" 200)))
      (let ((answer (talaria-answer "tal-11" "talaria_context_usage"
                                     "{\"session_id\":\"20260101_cd34\"}")))
        (is (search "observed" answer) "an unobserved session says so rather than answering zero")))))
