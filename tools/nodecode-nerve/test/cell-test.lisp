;;;; cell-test.lisp --- the nerve cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every decision is a stubbed DEX:REQUEST (NLK:HTTP's own call) and every
;;;; key a stubbed NLE::CREDENTIAL-ENV: nothing touches the network or the
;;;; operator's environment.

(in-package #:nodecode.test)

(define-test-slice "nerve" "NERVE-CELL-" :start nodecode-nerve:start-cell)

(define-cell-lifecycle-tests "nerve"
  (:hooks :tool)
  (:help :nerve)
  (:command "nerve")
  (:refused ("gate_mode" "sometimes") ("timeout_seconds" 0))
  (:idle nerve:nerve-error
         (nerve:decide "a state" "which one?" '("A" "B"))
         (nerve:stats)))

;;; --- fixtures -------------------------------------------------------------------

(defparameter +nerve-allow+
  "{\"id\":\"req-1\",\"model\":\"jev-test\",\"answers\":{\"decision\":{\"type\":\"choice\",\"choice\":\"ALLOW\",\"probabilities\":{\"ALLOW\":0.93,\"BLOCK\":0.07},\"confidence\":0.93}},\"usage\":{\"input_tokens\":12,\"output_tokens\":0}}"
  "A decision of ALLOW, as the wire answers it.")

(defparameter +nerve-block+
  "{\"id\":\"req-2\",\"model\":\"jev-test\",\"answers\":{\"gate\":{\"type\":\"choice\",\"choice\":\"BLOCK\",\"probabilities\":{\"ALLOW\":0.06,\"APPROVAL\":0.14,\"BLOCK\":0.80},\"confidence\":0.80}},\"usage\":{}}"
  "A gate verdict of BLOCK, as the wire answers it.")

(defparameter +nerve-gate-allow+
  "{\"id\":\"req-3\",\"model\":\"jev-test\",\"answers\":{\"gate\":{\"type\":\"choice\",\"choice\":\"ALLOW\",\"probabilities\":{\"ALLOW\":0.97,\"APPROVAL\":0.02,\"BLOCK\":0.01},\"confidence\":0.97}},\"usage\":{}}"
  "A gate verdict of a confident ALLOW.")

(defparameter +nerve-verify+
  "{\"id\":\"req-4\",\"model\":\"jev-test\",\"answers\":{\"decision\":{\"type\":\"choice\",\"choice\":\"PASS\",\"probabilities\":{\"PASS\":0.9,\"RETRY\":0.1},\"confidence\":0.9}},\"usage\":{}}"
  "A verify that passes.")

(defmacro with-nerve ((requests &key (answers '(list +nerve-allow+)) (statuses '(list 200))
                                     (env '(("TYPESAFE_API_KEY" . "ts-key"))) config)
                      &body body)
  "FORMS with the cell started on CONFIG, the environment ENV, and the
provider answering ANSWERS with STATUSES in turn (the last repeated):
REQUESTS each (METHOD URL HEADERS CONTENT) it was sent, oldest first."
  `(with-cell-stop ((nerve-start ,@config))
     (let ((,requests '()) (answers ,answers) (statuses ,statuses))
       (with-stubbed-fdefinitions
           ((nle::credential-env (name) (cdr (assoc name ',env :test #'equal)))
            (dex:request (url &rest args)
                         (setf ,requests (append ,requests
                                                 (list (list (getf args :method) url (getf args :headers)
                                                             (getf args :content)))))
                         (values (if (rest answers) (pop answers) (first answers))
                                 (if (rest statuses) (pop statuses) (first statuses))
                                 (make-hash-table :test 'equal))))
         ,@body))))

(defun nerve-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun nerve-body (request)
  "The decoded JSON body of REQUEST."
  (nlk:decode-json (fourth request)))

(defun nerve-hook ()
  "The installed :TOOL hook."
  (cell-hook "nodecode-nerve" :tool))

(defun nerve-call (form &key (name "eval"))
  "An op for one tool call of NAME carrying FORM."
  (list :name name
        :arguments (nlk:json-object "form" form)
        :call-id "c"))

;;; --- one decision ------------------------------------------------------------------

(deftest nerve-cell-posts-the-decisions-request ()
  (with-nerve (requests)
    (let ((result (nerve:decide "The branch is red after one file was touched."
                                 "Which of these should the agent do next?"
                                 '("ALLOW" "BLOCK")
                                 :criteria '(("ALLOW" . "the change is close and safe")
                                             ("BLOCK" . "the change is not understood")))))
      (is (equal "ALLOW" (getf result :value)) "the answer's label")
      (is (string= "jev-test" (getf result :model)) "the model that answered")
      (destructuring-bind (method url headers content) (first requests)
        (is (eq :post method))
        (is (equal "https://api.typesafe.ai/v1/systemone" url) "the auto provider at its own path")
        (is (equal "Bearer ts-key" (nerve-header headers "authorization")))
        (is (equal "application/json" (nerve-header headers "accept")))
        (let* ((body (nerve-body (first requests)))
               (questions (nlk:json-value body :object "questions"))
               (criteria (nlk:json-value questions :object "decision" "criteria")))
          (is (search "\"state\":" content) "the body leads with the state")
          (is (equal "jev-1.14.0" (nlk:json-value body :string "model")) "the provider's own default")
          (is (equal "choice" (nlk:json-value questions :string "decision" "type")))
          (is (equal "the change is close and safe" (nlk:json-value criteria :string "ALLOW"))))))))

(deftest nerve-cell-ranks-by-probability ()
  (with-nerve (requests)
    (let ((result (nerve:rank "Two ways to reach the same file."
                              "Which is the better first move?"
                              '(("ALLOW" . "one file, no risk") ("BLOCK" . "runs the test suite")))))
      (is (equal "ALLOW" (getf result :value)))
      (is (equal "ALLOW" (caar (getf result :ranking))) "highest probability first")
      (is (string= "BLOCK" (caadr (getf result :ranking))) "and the other after it"))))

(deftest nerve-cell-verifies-against-the-ladder ()
  (with-nerve (requests :answers (list +nerve-verify+))
    (let ((result (nerve:verify "Tests pass; the diff is two lines." "The suite is green.")))
      (is (equal "PASS" (getf result :value)))
      (let* ((questions (nlk:json-value (nerve-body (first requests)) :object "questions"))
             (criteria (nlk:json-value questions :object "decision" "criteria"))
             (labels '()))
        (maphash (lambda (label rubric) (declare (ignore rubric)) (push label labels)) criteria)
        (is (equal '("ESCALATE" "PASS" "REPLAN" "RETRY") (sort labels #'string<))
            "the ladder is the original's, and each rung carries its rubric")))))

(deftest nerve-cell-redacts-secrets-before-they-leave ()
  (with-nerve (requests)
    (nerve:decide (nlk:json-object "api_key" "sk-live-abcdefghijklmnop"
                                    "note" "the header was Authorization: Bearer abcdefghijkl"
                                    "form" "(sh \"ls\")")
                   "Is this safe?"
                   '("ALLOW" "BLOCK"))
    (let ((state (nlk:json-value (nerve-body (first requests)) :object "state")))
      (is (equal "[REDACTED]" (nlk:json-value state :string "api_key")) "a member named like a secret")
      (is (null (search "abcdefghijkl" (nlk:json-value state :string "note"))) "a token inside text")
      (is (equal "(sh \"ls\")" (nlk:json-value state :string "form")) "everything else stands"))))

(deftest nerve-cell-refuses-without-a-key ()
  (with-nerve (requests :env nil)
    (is (signals-error nerve:nerve-error
          (nerve:decide "a state" "which?" '("A" "B"))))
    (is (null requests) "nothing was sent")))

;;; --- the gate ------------------------------------------------------------------------

(deftest nerve-cell-gate-refuses-a-blocked-call ()
  (with-nerve (requests :answers (list +nerve-block+) :config ("gate_mode" "enforce"))
    (let ((answer (funcall (nerve-hook) (nerve-call "(sh \"rm -rf build\")")
                            (lambda (op) (declare (ignore op)) :ran))))
      (is (stringp answer) "a refusal string")
      (is (search "BLOCK" answer)))
    (is (= 1 (length requests)) "the provider was asked once")))

(deftest nerve-cell-gate-runs-a-confident-allow ()
  (with-nerve (requests :answers (list +nerve-gate-allow+) :config ("gate_mode" "enforce"))
    (is (eq :ran (funcall (nerve-hook) (nerve-call "(sh \"mkdir build\")")
                           (lambda (op) (declare (ignore op)) :ran))))))

(deftest nerve-cell-gate-lets-a-read-only-call-pass-unasked ()
  (with-nerve (requests :config ("gate_mode" "enforce"))
    (is (eq :ran (funcall (nerve-hook) (nerve-call "(+ 1 2)")
                           (lambda (op) (declare (ignore op)) :ran))))
    (is (null requests) "a call that changes nothing is never put to the model")))

(deftest nerve-cell-gate-advisory-never-refuses ()
  (with-nerve (requests :answers (list +nerve-block+) :config ("gate_mode" "advisory"))
    (is (eq :ran (funcall (nerve-hook) (nerve-call "(sh \"rm -rf build\")")
                           (lambda (op) (declare (ignore op)) :ran))))))

;;; --- the operator's command -------------------------------------------------------------

(deftest nerve-cell-command-reports-and-switches-the-gate ()
  (with-nerve (requests)
    (let ((entry (nle::find-registered-command "nerve")))
      (is (not (null entry)) "/nerve is in the catalog")
      (when entry
        (let ((run (nodecode.evolved::slash-command-headless entry)))
          (is (search "gate enforce" (funcall run "enforce" "s1")) "/nerve enforce switches the gate")
          (is (equal "enforce" (nodecode-nerve::gate-mode)) "the run's gate follows /nerve")
          (let ((text (funcall run "" "s1")))
            (is (search "typesafe" text) "where it asks")
            (is (search "key set" text) "whether a key is there")))))))
