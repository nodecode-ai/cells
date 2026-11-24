;;;; cell-test.lisp --- the onda cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Onda endpoint is a stubbed NLK:HTTP, every environment variable a
;;;; stubbed NLE::CREDENTIAL-ENV: nothing reaches Onda or the operator's env.

(in-package #:nodecode.test)

(define-test-slice "onda" "ONDA-CELL-" :start nodecode-onda:start-cell
  :defaults (("api_key" "onda-test-key")))

(define-cell-lifecycle-tests "onda"
  (:help :onda)
  (:command "onda")
  (:refused ("api_key" "") ("timeout_seconds" 2))
  (:idle onda:onda-error (onda:usage)))

;;; --- a stub for the one host ---------------------------------------------------

(defmacro with-onda-wire ((&rest routes) &body body)
  "BODY with NLK:HTTP answering by URL fragment: ROUTES are (FRAGMENT FORM),
FORM run with no arguments and answering (values TEXT STATUS). ASKED reads
the requests made, oldest first, as (METHOD URL HEADERS CONTENT)."
  (let ((log (gensym "LOG"))
        (answer (gensym "ANSWER")))
    `(let ((,log '()))
       (flet ((,answer (method url args)
                (push (list method url (getf args :headers) (getf args :content)) ,log)
                (cond ,@(loop for (fragment form) in routes
                              collect `((search ,fragment url) ,form))
                      (t (error "unexpected request to ~a" url)))))
         (with-stubbed-fdefinition (nlk:http (method url &rest args) (,answer method url args))
           (symbol-macrolet ((asked (reverse ,log)))
             ,@body))))))

(defun onda-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun onda-body (request)
  "The JSON content of REQUEST, decoded."
  (nlk:decode-json (fourth request)))

;;; --- reading, free --------------------------------------------------------------

(deftest onda-cell-usage-reads-the-plan ()
  (with-cell-stop ((onda-start "api_key" "k1"))
    (with-onda-wire (("/api/usage"
                      (values "{\"plan\":{\"name\":\"Pro\"},\"units\":{\"left\":7}}" 200)))
      (let ((answer (nodecode-onda::tool-usage (nlk:make-json-object))))
        (is (search "\"left\":7" answer) "the plan's numbers reach the model")
        (let ((request (first asked)))
          (is (eq :get (first request)))
          (is (equal "https://app.getonda.ru/api/usage" (second request)) "one host, the API's own path")
          (is (equal "k1" (onda-header (third request) "x-api-key")) "the key rides the X-API-Key header"))))))

(deftest onda-cell-hits-page-through-the-feed ()
  (with-cell-stop ((onda-start "api_key" "k1"))
    (with-onda-wire (("/api/radar/results"
                      (values "{\"total\":12,\"hits\":[{\"item_id\":\"i1\",\"platform\":\"instagram\",\"username\":\"someone\",\"viral_ratio\":41.5,\"url\":\"https://example.test/p/1\"}]}"
                              200)))
      (let ((answer (nodecode-onda::tool-hits
                     (nlk:make-json-object "folder" "f1" "limit" 5 "platform" "instagram"))))
        (is (search "instagram" answer))
        (is (search "41.5" answer) "the virality ratio is read, not invented")
        (is (search "of 12" answer) "the total says there are more pages")
        (let ((url (second (first asked))))
          (is (search "folder=f1" url))
          (is (search "limit=5" url))
          (is (search "scope=all" url))
          (is (search "platform=instagram" url)))))))

(deftest onda-cell-one-account-is-a-path-segment ()
  (with-cell-stop ((onda-start "api_key" "k1"))
    (with-onda-wire (("/api/radar/targets/t%201/hits"
                      (values "{\"hits\":[{\"username\":\"someone\",\"views\":1000}]}" 200)))
      (let ((answer (nodecode-onda::tool-target-hits
                     (nlk:make-json-object "target_id" "t 1" "period" "7"))))
        (is (search "someone" answer) "the row reaches the model, name and all")
        (is (search "1000" answer) "with its measured views")
        (is (equal "https://app.getonda.ru/api/radar/targets/t%201/hits?period=7" (second (first asked))) "the id is encoded into the path, the period into the query")))))

;;; --- what Onda refuses, in its words ------------------------------------------------

(deftest onda-cell-a-refusal-is-ondas-words ()
  (with-cell-stop ((onda-start "api_key" "k1"))
    (with-onda-wire (("/api/usage"
                      (values "{\"detail\":{\"code\":\"plan_missing\",\"message\":\"no active plan\"}}" 402)))
      (let ((answer (nodecode-onda::tool-usage (nlk:make-json-object))))
        (is (search "402" answer) "the status is passed on")
        (is (search "no active plan" answer) "and Onda's own sentence with it")
        (is (search "ERROR:" answer) "as the call's failure, not a success")))))

;;; --- spending, only with the ids that were asked for ---------------------------------

(deftest onda-cell-transcribe-spends-one-batch ()
  (with-cell-stop ((onda-start "api_key" "k1"))
    (with-onda-wire (("/api/transcribe" (values "{\"job_id\":\"j1\"}" 202)))
      (let ((answer (nodecode-onda::tool-transcribe
                     (nlk:make-json-object "item_ids" (vector "a1" "a1" "b2")))))
        (is (search "j1" answer) "the job id is passed on to poll")
        (let ((request (first asked)))
          (is (eq :post (first request)))
          (let ((body (onda-body request)))
            (is (equal 2 (nlk:json-value body :integer "limit")) "limit is the id count, not a guess")
            (is (equal "instagram" (nlk:json-value body :string "platform")))
            (is (equal '("a1" "b2") (coerce (nlk:json-value body :array "item_ids") 'list)) "the batch is the unique ids")))))))

;;; --- the section --------------------------------------------------------------------

(deftest onda-cell-the-key-can-come-from-the-environment ()
  (with-stubbed-fdefinition (nle::credential-env (name)
                             (and (equal name "ONDA_API_KEY") "from-env"))
    (with-cell-stop ((onda-start "api_key" "" ))
      (with-onda-wire (("/api/usage" (values "{}" 200)))
        (nodecode-onda::tool-usage (nlk:make-json-object))
        (is (equal "from-env" (onda-header (third (first asked)) "x-api-key")))
        (is (equal "the environment variable ONDA_API_KEY" (nodecode-onda::key-source)) "and /onda key says where it came from, never the key")))))

(deftest onda-cell-a-key-that-is-nowhere-refuses-at-start ()
  (with-stubbed-fdefinition (nle::credential-env (&rest names)
                             (when (member "ONDA_API_KEY" names :test #'equal) nil))
    (is (signals-error nlk:config-refusal (onda-start "api_key" "")) "no key anywhere is a refusal that names the member to set")))

;;; --- /onda ---------------------------------------------------------------------------

(deftest onda-cell-the-command-answers-usage ()
  (with-cell-stop ((onda-start "api_key" "k1"))
    (with-onda-wire (("/api/usage" (values "{\"plan\":{\"name\":\"Pro\"}}" 200)))
      (let ((answer (cell-entry "nodecode-onda" "onda" "usage")))
        (is (search "Pro" answer))
        (is (search "usage: /onda" (cell-entry "nodecode-onda" "onda" "nonsense")) "an unknown word answers what the command takes")))))
