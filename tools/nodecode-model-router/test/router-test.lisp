;;;; router-test.lisp --- the ranking and the three decisions, on bare rows.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here starts the cell: RANK takes a request and a list of rows and
;;;; answers a decision, so what a strategy does, what a constraint throws
;;;; out and when the router abstains are all one function call, with no
;;;; environment, no key and no network in sight.

(in-package #:nodecode.test)

(defun row (id &key (provider "demo") (model id) (name id) (base-url "https://demo.test/v1")
                      (key-envs '("DEMO_API_KEY")) (quality 0.5) (cost 0.5) (latency 0.5)
                      (capabilities '("chat")) (context 8192) (enabled t)
                      (fallback-id nil) (scope "default") (optional-key t))
  "One catalog row to rank."
  (nodecode-model-router::make-entry :id (if (find #\: id) id (format nil "~a:~a" provider id))
                                      :name (if (string= name id) id name)
                                      :provider provider :model model :base-url base-url
                                      :key-envs key-envs :quality quality :cost cost
                                      :latency latency :capabilities capabilities
                                      :context-window context :enabled enabled
                                      :fallback-id fallback-id :scope scope
                                      :optional-key optional-key))

(defun request (&rest arguments)
  (apply #'nodecode-model-router::make-route-request :task "summarise this letter" arguments))

(defun decide (rows &rest arguments)
  (nodecode-model-router::rank rows (apply #'request arguments)))

(defun chosen (result)
  (let ((selected (nodecode-model-router::route-result-selected result)))
    (and selected (nodecode-model-router::selection-id selected))))

(defun decision (result)
  (nodecode-model-router::route-result-decision result))

(defun abstention (result)
  (nodecode-model-router::route-result-abstain-reason result))

(defun reasons (result)
  (nodecode-model-router::route-result-reasons result))

(defun rejection (result id)
  (cdr (assoc id (nodecode-model-router::route-result-rejected result) :test #'string=)))

(deftest model-router-cell-a-strategy-ranks-by-its-own-number ()
  (let ((rows (list (row "cheap" :quality 0.30 :cost 0.05 :latency 0.60)
                    (row "dear" :quality 0.99 :cost 0.90 :latency 0.10)
                    (row "quick" :quality 0.50 :cost 0.40 :latency 0.02))))
    (is (string= "demo:cheap" (chosen (decide rows :prefer "cheapest"))) "cheapest buys the lowest cost")
    (is (string= "demo:dear" (chosen (decide rows :prefer "best"))) "best buys the highest quality")
    (is (string= "demo:quick" (chosen (decide rows :prefer "fastest"))) "fastest buys the lowest latency")
    (is (string= "select" (decision (decide rows :prefer "balanced"))) "balanced still decides")))

(deftest model-router-cell-a-constraint-rejects-with-its-reason ()
  (let ((rows (list (row "wide" :capabilities '("chat" "coding" "vision") :context 200000
                               :quality 0.9 :cost 0.4)
                    (row "narrow" :capabilities '("chat") :context 8000 :quality 0.2 :cost 0.1))))
    (let ((result (decide rows :required-capabilities '("coding") :min-context 100000)))
      (is (string= "demo:wide" (chosen result)) "the row with the capability and the window")
      (is (equal "missing_capabilities" (rejection result "demo:narrow")) "the other says why"))
    (is (equal "above_max_cost" (rejection (decide rows :max-cost 0.3) "demo:wide")))
    (is (string= "abstain" (decision (decide rows :max-cost 0.05))) "nothing under it abstains")
    (is (string= "no_candidate" (abstention (decide rows :max-cost 0.05))) "and says which reason")))

(deftest model-router-cell-a-pin-selects-or-abstains ()
  (let ((rows (list (row "wide" :quality 0.9) (row "other" :quality 0.2))))
    (let ((result (decide rows :prefer "named" :model "demo:other")))
      (is (string= "demo:other" (chosen result)) "a pin is taken whatever it scores")
      (is (member "named_pin" (reasons result) :test #'string=) "and says it was pinned")
      (is (equal "pin_mismatch" (rejection result "demo:wide")) "the rest are filtered out")
      (is (null (nodecode-model-router::route-result-fallback result)) "and nothing sits under it"))
    (is (string= "named_without_pin" (abstention (decide rows :prefer "named"))) "unpinned abstains")
    (is (string= "named_unavailable" (abstention (decide rows :prefer "named" :model "demo:absent"))))))

(deftest model-router-cell-a-failure-falls-back-one-hop ()
  (let* ((rows (list (row "first" :quality 0.9 :fallback-id "demo:third")
                     (row "second" :quality 0.7)
                     (row "third" :quality 0.2)))
         (result (decide rows :failed-model-id "demo:first")))
    (is (string= "fallback" (decision result)) "a failed row is answered, not re-picked")
    (is (string= "demo:third" (chosen result)) "its own fallback model, not the top")
    (is (equal "already_failed" (rejection result "demo:first")) "the failed row is out, by name")
    (is (member "bounded_fallback" (reasons result) :test #'string=) "and one hop is declared")
    (is (string= "fallback_disabled" (abstention (decide rows :failed-model-id "demo:first"
                                                                  :allow-fallback nil))))
    (let ((result (decide (list (row "second" :fallback-id "demo:absent") (row "top"))
                          :failed-model-id "demo:second")))
      (is (equal "fallback_unavailable" (rejection result "demo:absent")) "a lost fallback is named")
      (is (string= "demo:top" (chosen result)) "and the ranking still answers"))))

(deftest model-router-cell-the-task-words-are-scored ()
  (let* ((rows (list (row "writer" :quality 0.6)
                     (row "coder" :quality 0.6 :capabilities '("chat" "coding"))))
         (letter (decide rows :task "write a haiku about rain")))
    (is (string= "select" (decision letter)) "a task the rows can serve is decided")
    (is (member "coding" (nodecode-model-router::tokenize "a coding task") :test #'string=))
    (let ((selected (nodecode-model-router::route-result-selected letter)))
      (is (member "relevance=0.00" (nodecode-model-router::selection-reasons selected)
                  :test #'string=)))
    (is (string= "select" (decision (decide rows :task "fix this coding bug"))) "and still decides")))
