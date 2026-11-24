;;;; cell-test.lisp --- the openrouter-everything cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, every key a section written for
;;;; that test, every environment variable the stub the test needs: nothing
;;;; reaches OpenRouter, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "openrouter-everything" "OPENROUTER-EVERYTHING-CELL-"
  :start nodecode-openrouter-everything:start-cell)

(define-cell-lifecycle-tests "openrouter-everything"
  (:help :openrouter-everything)
  (:command "openrouter-everything")
  (:config "api_key" "sk-test")
  (:refused ("api_key" "sk-test" "timeout_seconds" 0))
  (:idle nodecode-openrouter-everything:openrouter-everything-error
         (openrouter-everything:decide "one state")))

;;; --- fixtures --------------------------------------------------------------------

(defparameter +decide-answer+
  "{\"model\":\"typesafe/jev-1.14\",\"answers\":{\"decision\":{\"type\":\"choice\",\"choice\":\"yes\",\"probabilities\":{\"yes\":0.9,\"no\":0.1},\"confidence\":0.9}},\"usage\":{\"total_tokens\":12,\"cost\":0.00001}}"
  "A Decisions answer: one choice, its probabilities, and the spend.")

(defparameter +rerank-answer+
  "{\"model\":\"cohere/rerank-v3.5\",\"results\":[{\"index\":1,\"relevance_score\":0.91},{\"index\":0,\"relevance_score\":0.12}],\"usage\":{\"total_tokens\":30}}"
  "A Rerank answer: two documents, best first, and the spend.")

(defparameter +embed-answer+
  "{\"model\":\"google/gemini-embedding-2\",\"data\":[{\"embedding\":[1,0,0]},{\"embedding\":[0,1,0]}],\"usage\":{\"total_tokens\":8}}"
  "An Embeddings answer: two orthogonal vectors.")

(defparameter +search-answer+
  "{\"model\":\"deepseek/deepseek-v4.1-flash-1017\",\"choices\":[{\"message\":{\"content\":\"Both agree it holds.\",\"annotations\":[{\"type\":\"url_citation\",\"url_citation\":{\"url\":\"https://example.org/one\",\"title\":\"One\",\"content\":\"it holds\"}},{\"type\":\"url_citation\",\"url_citation\":{\"url\":\"https://example.org/two\",\"title\":\"Two\"}}]}}]}"
  "A chat answer carrying url_citation annotations, as the server tool returns them.")

(defun or-body (content)
  "A request's JSON CONTENT, decoded."
  (nlk:decode-json content))

(defun or-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-openrouter ((&key (answered '+decide-answer+) (status 200)) &body body)
  "BODY with NLK:HTTP stubbed, answering ANSWERED at STATUS and collecting
each (METHOD URL ARGS) it was asked, oldest first, into ASKED."
  `(let ((collected '()))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                 (push (list method url args) collected)
                                 (values ,answered ,status nil))
       (symbol-macrolet ((asked (reverse collected)))
         ,@body))))

;;; --- the verbs --------------------------------------------------------------------

(deftest openrouter-everything-cell-decide-asks-and-answers ()
  (with-cell-stop ((openrouter-everything-start "api_key" "sk-test"))
    (with-openrouter ()
      (multiple-value-bind (answers model usage)
          (openrouter-everything:decide "ship it?" :options '(:yes "it is ready" :no "not yet"))
        (is (equal "typesafe/jev-1.14" model) "the plugin's default decision model")
        (is (equal "yes" (nlk:json-value answers :string "decision" "choice")) "the answer is the API's own object")
        (is (= 12 (getf usage :tokens)) "the spend comes back with it"))
      (destructuring-bind (method url args) (first asked)
        (is (eq :post method))
        (is (equal "https://openrouter.ai/api/alpha/decisions" url) "decisions live under /api/alpha")
        (is (equal "Bearer sk-test" (or-header (getf args :headers) "authorization")) "the key rides every call")
        (let ((body (or-body (getf args :content))))
          (is (equal "ship it?" (nlk:json-value body :string "state")))
          (is (equal "choice" (nlk:json-value body :string "questions" "decision" "type")) "options become one choice question")
          (is (equal "it is ready" (nlk:json-value body :string "questions" "decision" "criteria" "yes")) "the criteria are the option's own words"))))))

(deftest openrouter-everything-cell-rerank-keeps-the-order-asked ()
  (with-cell-stop ((openrouter-everything-start "api_key" "sk-test"))
    (with-openrouter (:answered +rerank-answer+)
      (multiple-value-bind (results model) (openrouter-everything:rerank "which holds?" '("a" "b") :top-n 2)
        (is (equal "cohere/rerank-v3.5" model))
        (is (= 2 (length results)))
        (is (= 1 (nlk:json-value (first results) :integer "index")) "best first, as the service ranked them"))
      (destructuring-bind (method url args) (first asked)
        (is (eq :post method))
        (is (equal "https://openrouter.ai/api/v1/rerank" url))
        (let ((body (or-body (getf args :content))))
          (is (= 2 (nlk:json-value body :integer "top_n")))
          (is (equal "a" (aref (nlk:json-array body "documents") 0)) "both documents in one call"))))))

(deftest openrouter-everything-cell-embed-compares-for-the-caller ()
  (with-cell-stop ((openrouter-everything-start "api_key" "sk-test"))
    (with-openrouter (:answered +embed-answer+)
      (let ((answer (openrouter-everything:embed '("one" "two"))))
        (is (= 2 (getf answer :count)))
        (is (= 3 (getf answer :dimensions)))
        (let ((matrix (getf answer :similarity-matrix)))
          (is (= 2 (length matrix)) "a row per text")
          (is (< (abs (second (first matrix))) 1d-9) "orthogonal vectors have no similarity"))
        (is (null (getf answer :vectors)) "the vectors stay out unless asked for"))
      (destructuring-bind (method url args) (first asked)
        (is (eq :post method))
        (is (equal "https://openrouter.ai/api/v1/embeddings" url))
        (is (= 2 (length (nlk:json-value (or-body (getf args :content)) :array "input"))))))))

(deftest openrouter-everything-cell-search-answers-with-the-urls-it-read ()
  (with-cell-stop ((openrouter-everything-start "api_key" "sk-test"))
    (with-openrouter (:answered +search-answer+)
      (multiple-value-bind (hits answer) (openrouter-everything:search "is it true?" :limit 2)
        (is (equal "Both agree it holds." answer))
        (is (= 2 (length hits)) "both cited urls are kept")
        (is (equal "https://example.org/one" (getf (first hits) :url)))
        (is (equal "One" (getf (first hits) :title))))
      (destructuring-bind (method url args) (first asked)
        (is (eq :post method))
        (is (equal "https://openrouter.ai/api/v1/chat/completions" url))
        (let ((body (or-body (getf args :content))))
          (let ((tool (aref (nlk:json-array body "tools") 0)))
            (is (equal "openrouter:web_search" (nlk:json-value tool :string "type")) "the search is the server tool, not a second account")
            (is (= 2 (nlk:json-value tool :integer "parameters" "max_results")))))))))

;;; --- what it says when it cannot call -----------------------------------------------

(deftest openrouter-everything-cell-refuses-with-no-key ()
  (with-cell-stop ((openrouter-everything-start))
    (with-openrouter ()
      (let ((condition (handler-case (progn (openrouter-everything:rerank "q" '("a")) nil)
                         (openrouter-everything:openrouter-everything-error (condition) condition))))
        (is (search "OPENROUTER_API_KEY" (openrouter-everything:openrouter-everything-error-detail condition)) "the refusal names where a key comes from")
        (is (null asked) "and nothing was sent")))))

(deftest openrouter-everything-cell-refuses-a-base-that-cannot-hold-alpha ()
  (with-cell-stop ((openrouter-everything-start "api_key" "sk-test" "base_url" "https://openrouter.example/api"))
    (with-openrouter ()
      (let ((condition (handler-case (progn (openrouter-everything:decide "x" :options '(:a "b")) nil)
                         (openrouter-everything:openrouter-everything-error (condition) condition))))
        (is (search "end in /v1" (openrouter-everything:openrouter-everything-error-detail condition)))
        (is (null asked) "and nothing was sent")))))

(deftest openrouter-everything-cell-command-says-the-connection ()
  (with-cell-stop ((openrouter-everything-start))
    (let ((status (cell-entry "nodecode-openrouter-everything" "openrouter-everything" "")))
      (is (search "https://openrouter.ai/api/v1" status) "the base it sends to")
      (is (search "MISSING" status) "and that no key is set")
      (is (search "cohere/rerank-v3.5" (cell-entry "nodecode-openrouter-everything" "openrouter-everything" "models")) "the model each verb would use"))))
