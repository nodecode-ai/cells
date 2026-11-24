;;;; cell-test.lisp --- the adaptive-effort cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here touches the network, the environment or a real session: the
;;;; scorer is a scripted DEX:REQUEST (NLK:HTTP's own call), the key a
;;;; stubbed ENV-VALUE, the rung ladder a stubbed TARGET-LADDER, the session's
;;;; task a stubbed SESSION-TASK, and the pick a stubbed NLE:SELECT-EFFORT
;;;; that records what it was asked to land.

(in-package #:nodecode.test)

(define-test-slice "adaptive-effort" "ADAPTIVE-EFFORT-CELL-"
  :start nodecode-adaptive-effort:start-cell)

(define-cell-lifecycle-tests "adaptive-effort"
  (:help :adaptive-effort)
  (:command "adaptive")
  (:hooks :round)
  (:refused ("mode" "sometimes") ("timeout_seconds" 0))
  (:idle aeffort:adaptive-effort-error
         (aeffort:status)
         (aeffort:history)
         (aeffort:probe "a task")))

;;; --- the fixture ---------------------------------------------------------------------

(defun ae-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun ae-answer (score)
  "One System One answer carrying SCORE on the effort question, or none."
  (nlk:encode-json-object
   (nlk:json-object "model" "jev-20260901"
                     "answers" (nlk:json-object
                                "effort" (if score
                                             (nlk:json-object "type" "score" "score" score)
                                             (nlk:json-object "type" "score")))
                     "usage" (nlk:json-object "input_tokens" 212))))

(defun ae-body (request)
  "The decoded body of REQUEST."
  (nlk:decode-json (fourth request)))

(defun call-with-scorer (thunk &key (score 1.4) (status 200) (key "ts-key")
                                (ladder '("low" "medium" "high" "xhigh"))
                                (task "Do the thing") config)
  "Call THUNK with the cell started on CONFIG, the environment holding KEY as
TYPESAFE_API_KEY, the scorer answering SCORE with STATUS, a stubbed ladder and
session task, and every landing recorded. => (values REQUESTS LANDED): the
(METHOD URL HEADERS CONTENT) of each call, oldest first, and each
(EFFORT SESSION-ID) landed."
  (let ((requests '()) (landed '()) (answer (ae-answer score)))
    (flet ((recording (url &rest args)
             (push (list (getf args :method) url (getf args :headers) (getf args :content))
                   requests)
             (values answer status (make-hash-table :test 'equal))))
      (with-stubbed-fdefinitions
          ((nodecode-adaptive-effort::env-value (name)
             (and (equal name "TYPESAFE_API_KEY") (and key (stringp key) key)))
           (nodecode-adaptive-effort::target-ladder (session)
             (declare (ignorable session))
             ladder)
           (nodecode-adaptive-effort::session-task (session)
             (declare (ignorable session))
             task)
           (nle:select-effort (effort &key session-id)
             (declare (ignorable effort session-id))
             (push (list effort session-id) landed)
             (format nil "reasoning effort for ~a: -> ~a, applies from the next turn"
                     session-id effort))
           (dex:request (url &rest args)
             (declare (ignorable url args))
             (apply #'recording url args)))
        (with-cell-stop ((apply #'adaptive-effort-start config))
          (funcall thunk))))
    (values requests landed)))

;;; --- the one question ----------------------------------------------------------------

(deftest adaptive-effort-cell-probe-asks-the-rubric ()
  (multiple-value-bind (requests landed)
      (call-with-scorer
       (lambda ()
         (let ((line (aeffort:probe "Refactor the parser and run the suite")))
           (is (search "medium" line) line)
           (is (search "jev/jev-latest" line) "the scorer is named in the answer"))))
    (is (equal 1 (length requests)) "one call")
    (is (null landed) "a probe lands nothing")
    (destructuring-bind (method url headers content) (first requests)
      (is (eq :post method))
      (is (equal "https://api.typesafe.ai/v1/systemone" url))
      (is (equal "Bearer ts-key" (ae-header headers "authorization")))
      (is (equal "application/json" (ae-header headers "content-type")))
      (let ((body (nlk:decode-json content)))
        (is (equal "Refactor the parser and run the suite"
                   (nlk:json-value body :string "state" "prompt")) "the task is the state")
        (is (equal "jev-latest" (nlk:json-value body :string "model")))
        (is (equal "score" (nlk:json-value body :string "questions" "effort" "type")))
        (is (= 3 (length (nlk:json-value body :array "questions" "effort" "criteria"))) "the plugin's three-level rubric")))))

(deftest adaptive-effort-cell-scores-follow-the-plugin-thresholds ()
  (loop for (score label) in '((0.4 "low") (1.0 "medium") (1.49 "medium") (1.5 "high"))
        do (call-with-scorer (lambda () (is (search label (aeffort:probe "Say hi"))
                                             (format nil "~a is ~a" score label)))
                              :score score)))

;;; --- landing the pick ------------------------------------------------------------------

(deftest adaptive-effort-cell-choose-lands-the-rung ()
  (multiple-value-bind (requests landed)
      (call-with-scorer
       (lambda ()
         (let ((line (aeffort:choose "Rewrite the scheduler" :session-id "s1")))
           (is (search "high" line) line)
           (is (search "applies from the next turn" line))
           (is (search "high" (aeffort:history)) "and the ledger kept it")))
       :score 1.8)
    (declare (ignore requests))
    (is (equal '(("high" "s1")) landed) "the rung the target admits, landed on the session")))

(deftest adaptive-effort-cell-a-narrow-route-keeps-its-nearest-rung ()
  (multiple-value-bind (requests landed)
      (call-with-scorer (lambda () (aeffort:choose "x" :session-id "s1"))
                         :score 1.8 :ladder '("low" "medium"))
    (declare (ignore requests))
    (is (equal '(("medium" "s1")) landed) "the nearest rung the route admits"))
  (multiple-value-bind (requests landed)
      (call-with-scorer (lambda () (aeffort:choose "x" :session-id "s1"))
                         :score 1.8 :ladder nil)
    (declare (ignore requests))
    (is (equal '(("high" "s1")) landed) "a ladder the image will not name is tried as the label says")))

(deftest adaptive-effort-cell-dry-run-lands-nothing ()
  (multiple-value-bind (requests landed)
      (call-with-scorer (lambda () (aeffort:choose "x" :session-id "s1" :dry-run t))
                         :score 1.8)
    (declare (ignore requests))
    (is (null landed))))

(deftest adaptive-effort-cell-custom-endpoint-and-its-key ()
  (multiple-value-bind (requests landed)
      (call-with-scorer (lambda () (aeffort:probe "x"))
                         :config (list "scorer" "custom" "custom_endpoint" "http://127.0.0.1:8011/v1/systemone"
                                       "custom_auth" "none" "model" "dgemma"))
    (declare (ignore landed))
    (is (equal "http://127.0.0.1:8011/v1/systemone" (second (first requests))))
    (is (null (ae-header (third (first requests)) "authorization")) "a keyless endpoint gets no bearer")
    (is (equal "dgemma" (nlk:json-value (ae-body (first requests)) :string "model")))))

;;; --- what it refuses -------------------------------------------------------------------

(deftest adaptive-effort-cell-without-a-key-says-where-to-make-one ()
  (multiple-value-bind (requests landed)
      (call-with-scorer (lambda ()
                          (let ((text (refusal-text aeffort:adaptive-effort-error
                                                    (aeffort:probe "x"))))
                            (is (search "console.typesafe.ai" text))
                            (is (search "TYPESAFE_API_KEY" text))))
                         :key nil)
    (declare (ignore landed))
    (is (null requests) "and nothing was sent")))

(deftest adaptive-effort-cell-custom-needs-an-endpoint ()
  (multiple-value-bind (requests landed)
      (call-with-scorer (lambda ()
                          (let ((text (refusal-text aeffort:adaptive-effort-error
                                                    (aeffort:probe "x"))))
                            (is (search "custom_endpoint" text) text)))
                         :config '("scorer" "custom"))
    (declare (ignore landed))
    (is (null requests))))

(deftest adaptive-effort-cell-says-a-refused-status ()
  (call-with-scorer (lambda ()
                      (let ((text (refusal-text aeffort:adaptive-effort-error
                                                (aeffort:probe "x"))))
                        (is (search "answered 500" text) text)))
                     :status 500))

(deftest adaptive-effort-cell-a-malformed-answer-is-a-refusal ()
  (call-with-scorer (lambda ()
                      (let ((text (refusal-text aeffort:adaptive-effort-error
                                                (aeffort:probe "x"))))
                        (is (search "without an effort score" text) text)))
                     :score nil))

;;; --- doing it unasked --------------------------------------------------------------------

(deftest adaptive-effort-cell-always-decides-once-a-turn ()
  (multiple-value-bind (requests landed)
      (call-with-scorer
       (lambda ()
         (flet ((this-round (turn)
                  (nodecode-adaptive-effort::observe-round
                   (list :session-id "s1" :turn turn :round 1) (lambda (op) (declare (ignore op)) nil))))
           (this-round :turn-a)
           (this-round :turn-a)
           (this-round :turn-b)))
       :score 1.8 :config '("mode" "always"))
    (is (= 2 (length requests)) "one classification per turn, not per round")
    (is (= 2 (length landed)))))

(deftest adaptive-effort-cell-once-decides-once-a-session ()
  (multiple-value-bind (requests landed)
      (call-with-scorer
       (lambda ()
         (flet ((this-round (turn)
                  (nodecode-adaptive-effort::observe-round
                   (list :session-id "s1" :turn turn :round 1) (lambda (op) (declare (ignore op)) nil))))
           (this-round :turn-a)
           (this-round :turn-b)))
       :score 0.4 :config '("mode" "once"))
    (is (= 1 (length requests)) "one decision for the session")
    (is (equal '(("low" "s1")) landed))))

(deftest adaptive-effort-cell-off-decides-nothing ()
  (multiple-value-bind (requests landed)
      (call-with-scorer
       (lambda ()
         (nodecode-adaptive-effort::observe-round
          (list :session-id "s1" :turn :turn-a :round 1) (lambda (op) (declare (ignore op)) nil))))
    (is (null requests))
    (is (null landed))))

;;; --- /adaptive ---------------------------------------------------------------------------

(deftest adaptive-effort-cell-status-and-history-answer ()
  (call-with-scorer
   (lambda ()
     (is (search "mode off" (cell-entry "nodecode-adaptive-effort" "adaptive" "")))
     (is (search "key set" (cell-entry "nodecode-adaptive-effort" "adaptive" "status")))
     (is (search "no decision yet" (cell-entry "nodecode-adaptive-effort" "adaptive" "history")))
     (cell-entry "nodecode-adaptive-effort" "adaptive" "choose Rewrite the scheduler")
     (is (search "Rewrite the scheduler"
                 (cell-entry "nodecode-adaptive-effort" "adaptive" "history")) "the ledger reads back what was decided")
     (is (search "usage: /adaptive probe" (cell-entry "nodecode-adaptive-effort" "adaptive" "probe")))
     (is (search "is not one of" (cell-entry "nodecode-adaptive-effort" "adaptive" "nope"))))
   :score 1.8))
