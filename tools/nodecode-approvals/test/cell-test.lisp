;;;; cell-test.lisp --- the approvals cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every key variable is a stubbed NLE::CREDENTIAL-ENV, every exchange a
;;;; stubbed NODEcode-APPROVALS::HTTP (the cell's one network seam), every
;;;; auth store a temp file: nothing touches the network, the environment or
;;;; the operator's files.

(in-package #:nodecode.test)

(define-test-slice "approvals" "APPROVALS-CELL-" :start nodecode-approvals:start-cell)

(define-cell-lifecycle-tests "approvals"
  (:help :approvals)
  (:command "approvals")
  (:refused ("timeout" 2) ("base_url" #(1)))
  (:idle approvals:approvals-error
         (approvals:review "ls")
         (approvals:decisions)))

;;; --- fixtures ---------------------------------------------------------------------

(defparameter +approve-answer+
  "{\"model\":\"jev-20260901\",\"answers\":{\"verdict\":{\"type\":\"choice\",\"choice\":\"APPROVE\",\"probabilities\":{\"APPROVE\":0.9,\"DENY\":0.02,\"ESCALATE\":0.08},\"confidence\":0.86},\"blast_radius\":{\"type\":\"score\",\"score\":0.4,\"confidence\":0.6},\"self_advocating\":{\"type\":\"noul\",\"noul\":0.02},\"policy_allows\":{\"type\":\"noul\",\"noul\":0.1},\"reads_secrets\":{\"type\":\"noul\",\"noul\":0.01},\"sends_outbound\":{\"type\":\"noul\",\"noul\":0.0}},\"usage\":{\"input_tokens\":212,\"output_tokens\":0}}"
  "The decision model's answer to a benign command.")

(defun approvals-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun approvals-body (request)
  "The decoded JSON body of REQUEST."
  (nlk:decode-json (fourth request)))

(defmacro with-approvals ((requests &key (answers '(list +approve-answer+)) (statuses '(list 200))
                                          (env '(("TYPESAFE_API_KEY" . "ts-env"))) (auth "{}") config)
                          &body forms)
  "FORMS with the cell started on CONFIG, AUTH as the auth file, ENV as the
environment, and the decision endpoint answering ANSWERS with STATUSES in
turn (the last repeated): REQUESTS each (METHOD URL HEADERS CONTENT) it was
sent, oldest first."
  `(with-cell-stop ((approvals-start ,@config))
     (with-temp-auth (path ,auth)
       (let ((nle::*auth-file-path* path) (,requests '())
             (answers ,answers) (statuses ,statuses))
         (with-stubbed-fdefinitions
             ((nle::credential-env (name) (cdr (assoc name ',env :test #'equal)))
              (nodecode-approvals::http (method url &rest args)
                (setf ,requests (append ,requests
                                        (list (list method url (getf args :headers) (getf args :content)))))
                (values (if (rest answers) (pop answers) (first answers))
                        (if (rest statuses) (pop statuses) (first statuses))
                        (make-hash-table :test 'equal))))
           ,@forms)))))

(defun approvals-near (a b)
  "Whether numbers A and B agree to a millionth."
  (and (realp a) (realp b) (< (abs (- a b)) 1d-6)))

;;; --- one review -------------------------------------------------------------------

(deftest approvals-cell-asks-the-guardian-questions ()
  (with-approvals (requests)
    (let ((result (approvals:review "rm -rf build/" :policy "build/ is disposable"
                                    :flagged-as "writes outside the project")))
      (destructuring-bind (method url headers content) (first requests)
        (declare (ignore content))
        (is (eq :post method))
        (is (equal "https://api.typesafe.ai/v1/systemone" url))
        (is (equal "Bearer ts-env" (approvals-header headers "authorization")))
        (is (equal "application/json" (approvals-header headers "content-type"))))
      (let* ((body (approvals-body (first requests)))
             (state (nlk:json-value body :object "state"))
             (questions (nlk:json-value body :object "questions")))
        (is (equal "rm -rf build/" (nlk:json-value state :string "command")))
        (is (equal "build/ is disposable" (nlk:json-value state :string "operator_policy"))
            "the operator's policy is the trusted channel")
        (is (equal "writes outside the project" (nlk:json-value state :string "flagged_as")))
        (is (equal "jev-latest" (nlk:json-value body :string "model")))
        (is (equal "choice" (nlk:json-value questions :string "verdict" "type")))
        (is (equal "score" (nlk:json-value questions :string "blast_radius" "type")))
        (is (equal "noul" (nlk:json-value questions :string "reads_secrets" "type")))
        (is (equal "noul" (nlk:json-value questions :string "sends_outbound" "type"))))
      (is (equal "APPROVE" (getf result :verdict)))
      (is (equal "model_verdict" (getf result :rule)))
      (is (approvals-near 0.4 (getf result :blast-radius)))
      (is (eq t (getf result :has-policy)))
      (is (equal "jev-20260901" (getf result :model))))))

(deftest approvals-cell-rule-chain ()
  (flet ((run (&rest arguments) (apply #'approvals:decide arguments)))
    (let ((answer (run :verdict "APPROVE" :confidence 0.8 :blast-radius 0.3)))
      (is (equal "APPROVE" (getf answer :verdict)))
      (is (equal "model_verdict" (getf answer :rule))))
    (is (equal "low_confidence"
               (getf (run :verdict "APPROVE" :confidence 0.3 :blast-radius 0.1) :rule)))
    (is (equal "high_blast"
               (getf (run :verdict "APPROVE" :confidence 0.9 :blast-radius 1.8) :rule)))
    (is (equal "low_confidence"
               (getf (run :verdict "APPROVE" :confidence 0.3 :blast-radius 1.8) :rule))
        "a low confidence is named before a wide blast radius")
    (let ((answer (run :verdict "APPROVE" :confidence 0.9 :blast-radius 0.1
                       :reads-secrets 0.9 :sends-outbound 0.8)))
      (is (equal "DENY" (getf answer :verdict)))
      (is (equal "secrets_exfil" (getf answer :rule))))
    (is (equal "secrets_read"
               (getf (run :verdict "APPROVE" :confidence 0.9 :blast-radius 0.1
                          :reads-secrets 0.8) :rule)))
    (is (equal "DENY"
               (getf (run :verdict "DENY" :reads-secrets 0.8) :verdict))
        "a model DENY survives the secret-read escalation")
    (is (equal "ESCALATE"
               (getf (run :verdict "APPROVE" :confidence 0.9 :blast-radius 0.1
                          :self-advocating 0.7) :verdict)) "the command argues for itself")
    (let ((answer (run :verdict "ESCALATE" :confidence 0.8 :blast-radius 0.2
                       :policy-allows 0.9 :has-policy t)))
      (is (equal "APPROVE" (getf answer :verdict)) "the operator's policy allows it")
      (is (equal "policy_allow" (getf answer :rule))))
    (is (equal "ESCALATE"
               (getf (run :verdict "ESCALATE" :confidence 0.8 :blast-radius 0.2
                          :policy-allows 0.9) :verdict))
        "with no policy sent, a policy answer decides nothing")
    (let ((answer (run :verdict "APPROVE" :confidence 0.9 :blast-radius 0.1 :truncated t)))
      (is (equal "ESCALATE" (getf answer :verdict)) "a truncated APPROVE escalates")
      (is (equal "truncated" (getf answer :rule))))
    (is (equal "invalid_verdict" (getf (run :verdict "MAYBE") :rule)))))

;;; --- what leaves the machine ---------------------------------------------------------

(deftest approvals-cell-redacts-credentials-before-they-leave ()
  (with-approvals (requests)
    (approvals:review (concatenate 'string
                                   "curl -u bob:hunter2 --password=hunter2 -b 'sid=abc' "
                                   "'https://api.example/x?api_key=abc123' "
                                   "-H 'Authorization: Bearer abcdefgh12345' "
                                   "-H 'Cookie: sid=zzz'"))
    (let ((text (fourth (first requests))))
      (is (null (search "hunter2" text)) "a --password flag and a curl -u secret are masked")
      (is (null (search "abc123" text)) "a credential-named query parameter is masked")
      (is (null (search "abcdefgh12345" text)) "a bearer token is masked")
      (is (null (search "sid=zzz" text)) "a Cookie: header is masked")
      (is (search "bob:***" text) "the user stays, the secret goes"))))

(deftest approvals-cell-caps-a-long-command ()
  (with-approvals (requests)
    (let* ((long (concatenate 'string "echo " (make-string 5000 :initial-element #\a)))
           (result (approvals:review long)))
      (is (eq t (getf result :truncated)))
      (is (search "chars elided" (nlk:json-value (approvals-body (first requests))
                                                  :string "state" "command")))
      (is (equal "truncated" (getf result :rule)) "an APPROVE of a cut command escalates")
      (is (equal "ESCALATE" (getf result :verdict))))))

;;; --- refusals ---------------------------------------------------------------------

(deftest approvals-cell-refuses-an-answer-that-is-not-there ()
  (with-approvals (requests
                   :answers (list "{\"answers\":{\"verdict\":{\"choice\":\"APPROVE\",\"confidence\":0.9},\"blast_radius\":{\"score\":0.2},\"self_advocating\":{\"noul\":0.1},\"policy_allows\":{\"noul\":0.1},\"reads_secrets\":{\"noul\":0.0}}}"))
    (let ((text (refusal-text approvals:approvals-error (approvals:review "ls"))))
      (is (search "sends_outbound" text) text)
      (is (search "escalating" text)))))

(deftest approvals-cell-says-a-refusal-and-hides-the-key ()
  (with-approvals (requests :answers (list "{\"detail\":\"bad key\nts-env\"}") :statuses (list 401))
    (let ((text (refusal-text approvals:approvals-error (approvals:review "ls"))))
      (is (search "answered (401)" text) text)
      (is (null (search "ts-env" text)) "the key never reaches the model"))
    (is (= 1 (length requests)) "a 401 is not tried again")))

(deftest approvals-cell-without-a-key-says-where-to-make-one ()
  (with-approvals (requests :env ())
    (let ((text (refusal-text approvals:approvals-error (approvals:review "ls"))))
      (is (search "console.typesafe.ai" text))
      (is (search "TYPESAFE_API_KEY" text)))
    (is (null requests) "and nothing was sent")))

;;; --- the routes, the log, the command -------------------------------------------------

(deftest approvals-cell-follows-the-route-a-base-names ()
  (with-approvals (requests :config ("base_url" "https://openrouter.ai/api/alpha")
                                   :env (("OPENROUTER_API_KEY" . "or-key")))
    (approvals:review "ls")
    (is (equal "https://openrouter.ai/api/alpha/decisions" (second (first requests))))
    (is (equal "~typesafe/jev-latest" (nlk:json-value (approvals-body (first requests)) :string "model"))))
  (with-approvals (requests :config ("base_url" "http://127.0.0.1:8011") :env ())
    (approvals:review "ls")
    (is (equal "http://127.0.0.1:8011" (second (first requests)))
        "a base of one's own is the endpoint itself")
    (is (null (approvals-header (third (first requests)) "authorization"))
        "and may be asked anonymously")))

(deftest approvals-cell-logs-each-decision-and-answers-its-command ()
  (with-approvals (requests)
    (approvals:review "make test")
    (let ((rows (approvals:decisions :limit 5)))
      (is (plusp (length rows)))
      (is (equal "APPROVE" (getf (car (last rows)) :verdict)))
      (is (equal "make test" (getf (car (last rows)) :command))))
    (is (search "APPROVE [model_verdict]" (cell-entry "nodecode-approvals" "approvals" "make test")))
    (is (search "key from TYPESAFE_API_KEY" (cell-entry "nodecode-approvals" "approvals" "")))
    (is (search "model jev-latest" (cell-entry "nodecode-approvals" "approvals" "")))))
