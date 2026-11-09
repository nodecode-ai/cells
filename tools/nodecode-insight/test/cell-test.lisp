;;;; cell-test.lisp --- the insight cell against a stubbed provider and a stubbed SearXNG.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange is a stubbed NLK:HTTP and the key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches a provider, an instance, the
;;;; environment or the operator's files. The tool is called the way a
;;;; model's call reaches it -- through the real dispatch, contract and all --
;;;; and the command through the registered entry, so both surfaces are
;;;; proved through the same seam a user's call uses.

(in-package #:nodecode.test)

(define-test-slice "insight" "INSIGHT-CELL-"
  :start nodecode-insight:start-cell
  :defaults (("base_url" "http://llm.test/v1")
             ("model" "test-model")
             ("api_key_env" "INSIGHT_KEY")))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-insight-token (&body body)
  "BODY with the key environment answering one fixed key."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "INSIGHT_KEY") "test-key"))
     ,@body))

(defmacro with-insight-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL CONTENT HEADERS),
oldest first, once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url
                                               (getf options :content)
                                               (getf options :headers))
                                         ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected insight request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun insight-header (headers name)
  "HEADERS' NAME, compared without case."
  (cdr (assoc name headers :test #'string-equal)))

(defun insight-body (asked &optional (index (1- (length asked))))
  "The JSON body of the ASKED exchange at INDEX (the last by default)."
  (nlk:decode-json (third (nth index asked))))

(defun insight-message (asked index)
  "The CONTENT of message INDEX of the last ASKED exchange."
  (nlk:json-value (aref (nlk:json-array (insight-body asked) "messages") index)
                  :string "content"))

(defparameter +definition+
  "**idempotent** - An operation that leaves the same result however often it runs."
  "The provider's answer in these tests.")

(defparameter +completion+
  (concatenate 'string
               "{\"model\":\"test-model\",\"choices\":[{\"message\":{\"role\":\"assistant\","
               "\"content\":\"**idempotent** - An operation that leaves the same result "
               "however often it runs.\"}}]}")
  "One OpenAI-compatible chat completion, as the wire answers it.")

(defparameter +searxng+
  (concatenate 'string
               "{\"results\":["
               "{\"title\":\"Idempotence - Wikipedia\","
               "\"url\":\"https://en.wikipedia.org/wiki/Idempotence\","
               "\"content\":\"Idempotence is the property of certain operations.\"},"
               "{\"title\":\"Idempotent - MDN\","
               "\"url\":\"https://developer.mozilla.org/en-US/docs/Glossary/Idempotent\","
               "\"content\":\"An HTTP method is idempotent when.\"},"
               "{\"title\":\"A third result\","
               "\"url\":\"https://example.org/third\","
               "\"content\":\"A third result the definition must not carry.\"}]}")
  "Three SearXNG rows: the cell takes the first two.")

(defun insight-answer (id arguments)
  "define_term's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id "define_term" arguments)))

(defun insight-object (id arguments)
  "define_term's answer to ARGUMENTS, decoded; a refusal is said whole."
  (let ((text (insight-answer id arguments)))
    (unless (and (plusp (length text)) (char= (char text 0) #\{))
      (error "define_term did not answer JSON: ~a" text))
    (nlk:decode-json text)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "insight"
  (:fixture with-insight-token)
  (:help :insight)
  (:command "insight")
  (:running (is (equal "test-model" (nodecode-insight::setting :model)) "the model the section names is what runs")
            (is (= 120 (nodecode-insight::setting :timeout)) "and the plugin's own timeout"))
  (:refused ("base_url" "") ("model" ""))
  (:idle insight:insight-error
         (insight:define "idempotent")))

;;; --- the tool ----------------------------------------------------------------------

(deftest insight-cell-defines-a-term-through-the-configured-provider (with-insight-token)
  (with-cell-stop ((insight-start))
    (with-insight-wire (asked ("/chat/completions" (values +completion+ 200)))
      (let ((object (insight-object "c1" "{\"term\":\"idempotent\"}")))
        (is (equal "idempotent" (nlk:json-value object :string "term")))
        (is (equal +definition+ (nlk:json-value object :string "definition")) "the provider's words, whole")
        (is (equal "test-model" (nlk:json-value object :string "model")))
        (is (= 0 (length (nlk:json-array object "sources"))) "no instance, no sources")
        (is (= 1 (length asked)) "one request, one answer")
        (is (equal :post (first (first asked))))
        (is (search "/chat/completions" (second (first asked))) "the OpenAI-compatible path")
        (is (equal "Bearer test-key" (insight-header (fourth (first asked)) "authorization")) "the key the section names, and no other header carries it")
        (is (equal "test-model" (nlk:json-value (insight-body asked) :string "model")))
        (is (equal 2 (length (nlk:json-array (insight-body asked) "messages"))) "a system message and the term, nothing of the conversation")
        (is (search "idempotent" (insight-message asked 0)) "the template's {term} was replaced")
        (is (search "no AI slop" (insight-message asked 0)) "and the plugin's writing rules travel with it")
        (is (equal "idempotent" (insight-message asked 1)) "the term is the whole user message")))))

(deftest insight-cell-sources-are-the-two-it-was-given (with-insight-token)
  (with-cell-stop ((insight-start "searxng_url" "http://searx.test"))
    (with-insight-wire (asked ("/search" (values +searxng+ 200))
                               ("/chat/completions" (values +completion+ 200)))
      (let* ((object (insight-object "c2" "{\"term\":\"idempotent\",\"template\":\"\"}"))
             (sources (nlk:json-array object "sources")))
        (is (= 2 (length asked)) "a search, then the completion")
        (is (equal :get (first (first asked))))
        (is (search "http://searx.test/search" (second (first asked))))
        (is (search "q=idempotent" (second (first asked))) "the query is the term, encoded")
        (is (search "format=json" (second (first asked))) "the JSON format a SearXNG instance must enable")
        (is (= 2 (length sources)) "the plugin hands over exactly two, however many came back")
        (is (equal "https://en.wikipedia.org/wiki/Idempotence"
                   (nlk:json-value (aref sources 0) :string "url")))
        (is (search "Wikipedia" (nlk:json-value (aref sources 0) :string "title")))
        (let ((prompt (insight-message asked 1)))
          (is (search "https://en.wikipedia.org/wiki/Idempotence" prompt) "the URLs go to the provider")
          (is (search "https://developer.mozilla.org/en-US/docs/Glossary/Idempotent" prompt))
          (is (search "cite EXACTLY TWO" prompt) "with the plugin's citation rule")
          (is (not (search "example.org/third" prompt)) "and never a third, uncited one")
          (is (not (search "Idempotence is the property" prompt)) "snippets stay here: only titles and URLs travel"))))))

(deftest insight-cell-no-query-string-without-an-instance (with-insight-token)
  (with-cell-stop ((insight-start "searxng_url" ""))
    (with-insight-wire (asked ("/chat/completions" (values +completion+ 200)))
      (let ((object (insight-object "c3" "{\"term\":\"idempotent\"}")))
        (is (= 1 (length asked)) "nothing is fetched without a searxng URL")
        (is (= 0 (length (nlk:json-array object "sources"))))
        (is (equal "idempotent" (insight-message asked 1)) "and the prompt is the bare term")))))

(deftest insight-cell-the-section-s-template-is-the-whole-system-instruction (with-insight-token)
  (with-cell-stop ((insight-start))
    (with-insight-wire (asked ("/chat/completions" (values +completion+ 200)))
      (insight-object "c4" "{\"term\":\"idempotent\",\"template\":\"Define {term} in one line.\"}")
      (is (equal "Define idempotent in one line." (insight-message asked 0)) "a caller's template wins, and its {term} is replaced"))))

(deftest insight-cell-a-refused-status-is-the-call-s-failure (with-insight-token)
  (with-cell-stop ((insight-start))
    (with-insight-wire (asked ("/chat/completions"
                               (values "{\"error\":{\"message\":\"Incorrect API key provided\"}}" 401)))
      (let ((text (insight-answer "c5" "{\"term\":\"idempotent\"}")))
        (is (search "ERROR:" text) "the answer is a refusal the transcript can draw")
        (is (search "HTTP 401" text) "naming the status")
        (is (search "Incorrect API key provided" text) "and the provider's own words")
        (is (= 1 (length asked)))))))

(deftest insight-cell-a-missing-provider-installs-nothing (with-insight-token)
  (let ((condition (signals-error nlk:config-refusal (insight-start "model" ""))))
    (is (search "no model" (princ-to-string condition)) "the refusal names what is missing")
    (is (null (nle::find-tool "define_term")) "and the tool is not registered")
    (is (null nodecode-insight::*insight*) "nor are any settings kept")))

;;; --- the command -------------------------------------------------------------------

(deftest insight-cell-slash-answers-with-its-sources (with-insight-token)
  (with-cell-stop ((insight-start "searxng_url" "http://searx.test"))
    (with-insight-wire (asked ("/search" (values +searxng+ 200))
                               ("/chat/completions" (values +completion+ 200)))
      (let ((text (cell-entry "nodecode-insight" "insight" "idempotent" nil)))
        (is (search "**idempotent**" text) "the definition, as the operator reads it")
        (is (search "Sources:" text))
        (is (search "https://en.wikipedia.org/wiki/Idempotence" text) "the sources after it")
        (is (search "(test-model)" text) "and the model that wrote it")))
    (let ((bare (cell-entry "nodecode-insight" "insight" "" nil)))
      (is (search "define_term" bare) "typed alone, it says what it does")
      (is (search "base_url" bare) "and what the section needs"))))
