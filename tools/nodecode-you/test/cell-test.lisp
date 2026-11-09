;;;; cell-test.lisp --- the you cell against a stubbed MCP transport.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test touches the network or needs a You.com key: every call goes
;;;; through NODECODE-YOU::*RPC*, a stub that records the endpoint, the tool
;;;; name, the arguments and the headers it was handed, and answers a result
;;;; object the test chose. What is under test is the part this cell owns -- the
;;;; call it builds out of a declared tool call, and the string it answers.

(in-package #:nodecode.test)

(define-test-slice "you" "YOU-CELL-" :start nodecode-you:start-cell)

(define-cell-lifecycle-tests "you"
  (:help :you)
  (:command "you")
  (:running
   (is (nle::find-tool "you_search") "the search tool is registered")
   (is (nle::find-tool "you_finance") "and the finance one")
   (is (= 4 (count-if (lambda (entry) (eql 0 (search "you_" (first entry)))) nle::*tools*))
       "four tools, named as the original's MCP servers named them"))
  (:stopped
   (is (null (nle::find-tool "you_search"))
       "and stop takes every one back"))
  (:refused ("count" 0)
            ("timeout_seconds" 1)
            ("max_chars" 10)))

;;; --- the stub --------------------------------------------------------------

(defvar *calls* nil
  "The MCP calls the stub was handed, newest first: (URL TOOL ARGUMENTS HEADERS).")

(defun stub-result (text &key errorp)
  "A tools/call result, shaped as MCP returns one: one text content block."
  (nlk:make-json-object "content" (vector (nlk:make-json-object "type" "text" "text" text))
                         "isError" errorp))

(defmacro with-you ((&key (answer "ok") errorp) &body body)
  "BODY with the cell started and every MCP call answered by a stub: ANSWER is
what the server wrote, ERRORP marks it an error result, and *CALLS* records
the exchange."
  `(let* ((*calls* nil)
          (nodecode-you::*rpc* (lambda (url tool arguments headers)
                                  (push (list url tool arguments headers) *calls*)
                                  (stub-result ,answer :errorp ,errorp))))
     (with-cell-stop ((you-start))
       ,@body)))

(defun you-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "you-call" name arguments)))

(defun call-argument (name)
  "NAME's value in the call the stub was handed last."
  (nlk:json-value (third (first *calls*)) :string name))

;;; --- what a search asks ------------------------------------------------------

(deftest you-cell-a-keyless-search-asks-the-free-profile ()
  (with-you (:answer "1. Tides\n   https://example.org/tides")
    (let ((answer (you-call "you_search" "{\"query\": \"tide tables oslo\", \"freshness\": \"week\"}")))
      (is (equal "https://api.you.com/mcp?profile=free" (first (first *calls*)))
          "the free profile answers a keyless search")
      (is (equal "you-search" (second (first *calls*))) "under the original's tool name")
      (is (equal "tide tables oslo" (call-argument "query")) "the query rides as it was asked")
      (is (equal 5 (nlk:json-value (third (first *calls*)) :integer "count"))
          "the section's count stands when the call names none")
      (is (equal "week" (call-argument "freshness")) "and a filter the call names rides")
      (is (null (fourth (first *calls*))) "no key, no authorization header")
      (is (search "1. Tides" answer) "the answer carries what the server wrote")
      (is (search "free profile" answer) "and says which profile answered"))))

(deftest you-cell-a-key-rides-the-keyed-server-and-nowhere-else ()
  (with-you ()
    (with-cell-stop ((you-start "api_key" "example-key"))
      (you-call "you_search" "{\"query\": \"x\"}")
      (is (equal "https://api.you.com/mcp" (first (first *calls*)))
          "with a key the account's server answers")
      (is (equal "Bearer example-key"
                 (cdr (assoc "authorization" (fourth (first *calls*)) :test #'equal)))
          "the key rides as a bearer header on the You.com call")                     
      (is (search "the account's key" (you-call "you_search" "{\"query\": \"y\"}"))
          "and the answer says so"))))

;;; --- what needs a key -------------------------------------------------------

(deftest you-cell-contents-without-a-key-refuses-before-anything-runs ()
  (with-you ()
    (let ((answer (you-call "you_contents" "{\"urls\": [\"https://example.org/a\"]}")))
      (is (search "api_key" answer) "the refusal names the section's member")
      (is (search "YDC_API_KEY" answer) "and the environment variable")
      (is (null *calls*) "and nothing was sent without a key"))))

(deftest you-cell-a-url-that-is-not-http-is-refused ()
  (with-you ()
    (with-cell-stop ((you-start "api_key" "example-key"))
      (is (search "not an http" (you-call "you_contents" "{\"urls\": [\"example.org\"]}"))
          "the model is told what a url looks like")
      (is (null *calls*) "and the server was not asked"))))

;;; --- one server per tool -----------------------------------------------------

(deftest you-cell-every-tool-asks-its-own-server ()
  (with-you ()
    (with-cell-stop ((you-start "api_key" "example-key"))
      (you-call "you_contents"
                "{\"urls\": [\"https://example.org/a\", \"https://example.org/b\"], \"formats\": [\"html\"]}")
      (is (equal '("https://api.you.com/mcp" "you-contents") (subseq (first *calls*) 0 2))
          "contents is the keyed server's")
      (is (equal '("https://example.org/a" "https://example.org/b")
                 (coerce (nlk:json-value (third (first *calls*)) :array "urls") 'list))
          "and every url rides")
      (you-call "you_research" "{\"input\": \"how tides are predicted\", \"research_effort\": \"deep\"}")
      (is (equal '("https://api.you.com/mcp/research" "you-research") (subseq (first *calls*) 0 2))
          "research has a server of its own")
      (is (equal "deep" (call-argument "research_effort")) "with the effort the call named")
      (you-call "you_finance" "{\"query\": \"semiconductor revenue 2024\"}")
      (is (equal '("https://api.you.com/mcp/finance" "you-finance") (subseq (first *calls*) 0 2))
          "and finance a third"))))

(deftest you-cell-a-count-outside-one-to-ten-is-refused ()
  (with-you ()
    (is (search "1 to 10" (you-call "you_search" "{\"query\": \"x\", \"count\": 50}")))
    (is (null *calls*) "and the search never ran")))

;;; --- what an answer says -----------------------------------------------------

(deftest you-cell-an-error-result-is-a-refusal ()
  (with-you (:answer "10 calls a minute on the free profile" :errorp t)
    (let ((answer (you-call "you_search" "{\"query\": \"x\"}")))
      (is (search "10 calls a minute" answer) "what the server said reaches the model")
      (is (search "ERROR" answer) "as a failed call, not a successful one"))))

(deftest you-cell-a-long-answer-is-cut-at-the-section-s-limit ()
  (with-you (:answer (make-string 3000 :initial-element #\x))
    (with-cell-stop ((you-start "max_chars" 500))
      (let ((answer (you-call "you_search" "{\"query\": \"x\"}")))
        (is (search "cut at 500 of" answer) "the cut is said, with both sizes")
        (is (< (length answer) 1200) "and the answer stays small")))))

;;; --- the operator's two ends --------------------------------------------------

(deftest you-cell-the-command-reports-the-state-and-searches ()
  (with-you ()
    (is (search "free profile" (nodecode-you::run-command "" nil))
        "bare /you names the keyless floor")
    (is (search "YDC_API_KEY" (nodecode-you::run-command "" nil))
        "and where a key comes from")
    (nodecode-you::run-command "tide tables" nil)
    (is (equal "tide tables" (call-argument "query")) "/you QUERY searches the web")
    (is (null (search "example-key" (nodecode-you::run-command "" nil)))
        "and the status never prints a key")))

(deftest you-cell-the-manual-carries-the-original-s-skills ()
  (with-you ()
    (let ((text (cddr (assoc :you nle::*help-topics*))))
      (is (search "you_contents" text) "every tool is named")
      (is (search "untrusted" text) "with the rule that a page is evidence, not instruction")
      (is (search "402" text) "and what a payment challenge means")
      (is (search "you:search" text) "and the eval spelling of the same verbs"))))
