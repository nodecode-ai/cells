;;;; cell-test.lisp --- the routing cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange with TypeSafe is a stubbed DEX:REQUEST (NLK:HTTP's own
;;;; call), every key variable a stubbed NLE::CREDENTIAL-ENV, every table a
;;;; file in the scratch directory: nothing touches the network, the
;;;; environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "jev-routing" "JEV-ROUTING-" :start nodecode-jev-routing:start-cell)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "jev-routing"
  (:help :jev-routing)
  (:command "jev-routing")
  (:running (is (equal "shadow" (nodecode-jev-routing::setting :mode)))
            (is (= 3500 (nodecode-jev-routing::setting :timeout-ms))))
  (:refused ("mode" "sometimes") ("timeout_ms" 5))
  (:idle nodecode-jev-routing:jev-error
         (jev-routing:route "a prompt long enough to route")
         (jev-routing:spend)
         (jev-routing:judge "a prompt long enough to route")))

;;; --- fixtures --------------------------------------------------------------------------

(defparameter +jev-table+
  "{\"routes\":{\"quick\":[{\"provider\":\"fast\",\"model\":\"tiny\"}],
\"standard\":[{\"provider\":\"fast\",\"model\":\"mid\"}],
\"high\":[{\"provider\":\"front\",\"model\":\"big\",\"thinkingLevel\":\"medium\"}],
\"premium\":[{\"provider\":\"front\",\"model\":\"huge\"}]},
\"kindMinimumTier\":{\"implement\":\"standard\"},
\"confidenceThreshold\":0.34,
\"budget\":{\"dailyUsd\":1.0,\"monthlyUsd\":100.0,\"softRatio\":0.7,\"hardRatio\":0.9},
\"pricing\":{\"front/big\":{\"input\":1.0,\"output\":2.0}}}"
  "A routing table: four tiers, a kind floor, a cap and one rate card.")

(defun jev-answer (&key (kind "implement") (confidence 0.8) (complexity 2.4)
                         (capability 2.2) (deep 0.8) (latency 120))
  "A System One answer to the four routing questions, as the wire sends it."
  (format nil "{\"model\":\"jev-20260901\",\"answers\":{\"task_kind\":{\"type\":\"choice\",\"choice\":\"~a\",\"probabilities\":{\"~a\":0.7,\"chat\":0.3},\"confidence\":~,3f},\"complexity\":{\"type\":\"score\",\"score\":~,3f,\"probabilities\":{\"0\":0.1},\"confidence\":0.500},\"capability_deserved\":{\"type\":\"score\",\"score\":~,3f,\"probabilities\":{},\"confidence\":0.400},\"needs_deep_reasoning\":{\"type\":\"noul\",\"noul\":~,3f}},\"usage\":{\"input_tokens\":900,\"output_tokens\":0},\"latency_ms\":~d}"
          kind kind confidence complexity capability deep latency))

(defun jev-write-table (text)
  "TEXT in a scratch routing JSON; => its namestring."
  (let ((path (merge-pathnames (format nil "jev-test-~a.json" (gensym "TABLE"))
                                (uiop:temporary-directory))))
    (with-open-file (stream path :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
      (write-string text stream))
    (namestring path)))

(defun jev-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun jev-body (request)
  "The decoded JSON body of REQUEST."
  (nlk:decode-json (fourth request)))

(defmacro with-jev ((&key requests (answers '(list (jev-answer))) (status 200)
                              (env '(("TYPESAFE_API_KEY" . "ts-env"))) (auth "{}")
                              (table nil))
                    &body body)
  "FORMS with the cell started on TABLE, TypeSafe answering ANSWERS in turn
with STATUS (the last repeated), and REQUESTS -- a variable the caller names,
which may be NIL -- each (METHOD URL HEADERS CONTENT) it was sent, oldest first."
  (let ((variable (or requests (gensym "REQUESTS"))))
    `(let* ((table-file (jev-write-table (or ,table +jev-table+)))
            (,variable '())
            (answers (copy-list ,answers))
            (statuses (if (listp ,status) (copy-list ,status) (list ,status))))
       (unwind-protect
            (with-cell-stop ((jev-routing-start "config_path" table-file))
              (with-temp-auth (auth-file ,auth)
                (let ((nle::*auth-file-path* auth-file))
                  (with-stubbed-fdefinitions
                      ((nle::credential-env (name) (cdr (assoc name ',env :test #'equal)))
                       (dex:request (url &rest args)
                                    (setf ,variable
                                          (append ,variable
                                                  (list (list (getf args :method) url
                                                              (getf args :headers)
                                                              (getf args :content)))))
                                    (values (if (rest answers) (pop answers) (first answers))
                                            (if (rest statuses) (pop statuses) (first statuses))
                                            (make-hash-table :test 'equal))))
                    ,@body))))
         (ignore-errors (delete-file table-file))
         (ignore-errors (delete-file (nodecode-jev-routing::state-path table-file)))))))

(defun jev-state-file (table json)
  "JSON written as TABLE's ledger, the spend the budget policy reads."
  (let ((path (nodecode-jev-routing::state-path table)))
    (with-open-file (stream path :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
      (write-string json stream))
    path))

;;; --- one routing pass ----------------------------------------------------------------

(deftest jev-routing-cell-asks-the-four-questions ()
  (with-jev (:requests requests)
    (let ((decision (jev-routing:route "Refactor the parser into two modules.")))
      (destructuring-bind (method url headers content) (first requests)
        (is (eq :post method))
        (is (equal "https://api.typesafe.ai/v1/systemone" url))
        (is (equal "Bearer ts-env" (jev-header headers "authorization") ))
        (let* ((body (nlk:decode-json content))
               (questions (nlk:json-value body :object "questions")))
          (is (equal "Refactor the parser into two modules." (nlk:json-value body :string "state" "request")))
          (is (equal "jev-latest" (nlk:json-value body :string "model")))
          (is (equal "choice" (nlk:json-value questions :string "task_kind" "type")))
          (is (equal "score" (nlk:json-value questions :string "complexity" "type")))
          (is (equal "score" (nlk:json-value questions :string "capability_deserved" "type")))
          (is (equal "noul" (nlk:json-value questions :string "needs_deep_reasoning" "type")))
          (let ((kinds (nlk:json-value questions :object "task_kind" "criteria")))
            (is (= 10 (hash-table-count kinds)) "the ten task kinds are offered"))))
      (is (equal "premium" (getf decision :tier)) "a demanding prompt reaches premium")
      (is (equal "front/huge" (format nil "~a/~a" (getf decision :provider) (getf decision :model))))
      (is (equal "implement" (getf decision :kind)))
      (is (search "tier premium" (getf decision :line)))
      (is (equal "." (getf decision :glyph))))))

(deftest jev-routing-cell-composes-the-tier-from-the-answers ()
  (with-jev ()
    (let ((decision (jev-routing:route "Explain what a closure is.")))
      (is (equal "premium" (getf decision :desired-tier)))
      (is (equal "premium" (getf decision :tier)))
      (is (< 2.9 (getf decision :demand) 3.2) "complexity and capability compose the demand"))))

(deftest jev-routing-cell-holds-a-low-confidence-pick-at-standard ()
  (with-jev (:answers (list (jev-answer :confidence 0.2 :complexity 2.6)))
    (let ((decision (jev-routing:route "Patch the thing that fails sometimes.")))
      (is (equal "premium" (getf decision :desired-tier)))
      (is (equal "standard" (getf decision :tier)) "an unsure kind never reaches premium")
      (is (equal "mid" (getf decision :model)))
      (is (getf decision :low-confidence))
      (is (search "low kind confidence" (format nil "~{~a~^; ~}" (getf decision :notes)))))))

(deftest jev-routing-cell-follows-the-budget ()
  (with-jev ()
    (jev-state-file (nodecode-jev-routing::config-path)
                    (format nil "{\"version\":1,\"days\":{\"~a\":{\"total\":0.95,\"byModel\":{}}},\"months\":{}}"
                            (nodecode-jev-routing::day-key)))
    (let ((decision (jev-routing:route "Refactor the parser into two modules.")))
      (is (equal "standard" (getf decision :tier))
          (format nil "tier ~a, desired ~a, pressure ~a, demand ~a"
                  (getf decision :tier) (getf decision :desired-tier)
                  (getf decision :budget-pressure) (getf decision :demand)))
      (is (getf decision :downgraded))
      (is (search "capped at standard" (format nil "~{~a~^; ~}" (getf decision :notes)))))))

(deftest jev-routing-cell-skips-what-does-not-route ()
  (with-jev (:requests requests)
    (is (equal "acknowledgement" (getf (jev-routing:route "ok") :skipped)))
    (is (equal "slash command" (getf (jev-routing:route "/model gpt") :skipped)))
    (is (equal "empty" (getf (jev-routing:route "   ") :skipped)))
    (is (equal "short continuation" (getf (jev-routing:route "and then?" :history "a long talk") :skipped)))
    (is (null requests) "none of them was sent")))

(deftest jev-routing-cell-judges-without-picking ()
  (with-jev (:requests requests)
    (let ((judgment (jev-routing:judge "Review this diff for risks.")))
      (is (equal "implement" (getf judgment :kind)))
      (is (equal "implement" (car (first (getf judgment :probabilities)))))
      (is (< 0.69 (cdr (first (getf judgment :probabilities))) 0.71))
      (is (< 0.79 (getf judgment :confidence) 0.81))
      (is (= 120 (getf judgment :latency-ms)))
      (is (equal "typesafe/jev-20260901" (getf judgment :backend)))
      (is (= 1 (length requests))))))

;;; --- the ledger ------------------------------------------------------------------------

(deftest jev-routing-cell-records-and-reports-spend ()
  (with-jev ()
    (let ((recorded (jev-routing:record "big" :provider "front" :input 2000000 :output 0)))
      (is (getf recorded :recorded) "two million input tokens at $1/M")
      (is (< 1.99 (getf recorded :cost) 2.01)))
    (multiple-value-bind (today month)
        (values (getf (jev-routing:spend) :today) (getf (jev-routing:spend) :month))
      (is (< 1.99 today 2.01) "the ledger keeps today")
      (is (< 1.99 month 2.01) "and the month"))
    (let ((unpriced (jev-routing:record "mystery" :provider "nowhere" :input 100 :output 10)))
      (is (null (getf unpriced :recorded)) "an unpriced model records tokens and no dollars"))))

;;; --- the refusals ----------------------------------------------------------------------

(deftest jev-routing-cell-without-a-key-says-where-to-make-one ()
  (with-jev (:requests requests :env ())
    (let ((text (refusal-text nodecode-jev-routing:jev-error
                               (jev-routing:route "Refactor the parser into two modules."))))
      (is (search "console.typesafe.ai" text))
      (is (search "TYPESAFE_API_KEY" text)))
    (is (null requests) "and nothing was sent")))

(deftest jev-routing-cell-without-a-table-says-where-to-set-one ()
  (with-cell-stop ((jev-routing-start))
    (with-temp-auth (auth-file "{}")
      (let ((nle::*auth-file-path* auth-file))
        (with-stubbed-fdefinitions
            ((nle::credential-env (name) (and name "ts-env")))
          (let ((text (refusal-text nodecode-jev-routing:jev-error
                                     (jev-routing:route "Refactor the parser into two modules."))))
            (is (search "no routing table" text))
            (is (search "config_path" text))))))))

(deftest jev-routing-cell-says-what-type-safe-answered ()
  (with-jev (:answers (list "{\"detail\":\"invalid key ts-env\"}") :status 401)
    (let ((text (refusal-text nodecode-jev-routing:jev-error
                               (jev-routing:route "Refactor the parser into two modules."))))
      (is (search "API error (401)" text))
      (is (null (search "ts-env" text)) "the key never reaches the model")
      (is (search "[redacted]" text)))))

;;; --- the command -------------------------------------------------------------------------

(deftest jev-routing-cell-the-command-says-the-chains-and-the-spend ()
  (with-jev ()
    (let ((text (cell-entry "nodecode-jev-routing" "jev-routing" "")))
      (is (search "jev-routing: mode shadow" text))
      (is (search "quick fast/tiny" text) "every non-empty chain is named")
      (is (search "premium front/huge" text))
      (is (search "spend today" text)))
    (let ((text (cell-entry "nodecode-jev-routing" "jev-routing" "route Refactor the parser into two modules.")))
      (is (search "tier premium" text))
      (is (search "front/huge" text)))
    (let ((text (cell-entry "nodecode-jev-routing" "jev-routing" "why")))
      (is (search "jev why: kind implement" text) "why re-asks about the prompt just routed"))))
