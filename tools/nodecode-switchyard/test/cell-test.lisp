;;;; cell-test.lisp --- the switchyard cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json bound as the auth file, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every decision a stubbed
;;;; DEX:REQUEST (NLK:HTTP's own call): nothing touches the network, the
;;;; environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "switchyard" "SWITCHYARD-CELL-" :start nodecode-switchyard:start-cell)

(define-cell-lifecycle-tests "switchyard"
  (:help :switchyard)
  (:command "switchyard")
  (:refused ("provider" "nope") ("effort_cap" "ultra") ("timeout_seconds" 0))
  (:idle switchyard:switchyard-error
         (switchyard:route "fix the parser")
         (switchyard:skills)))

;;; --- fixtures -------------------------------------------------------------------

(defun sw-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defparameter +sw-answer+
  "{\"model\":\"jev-20260901\",\"answers\":{\"skill\":{\"type\":\"choice\",\"choice\":\"writing-tests\",\"probabilities\":{\"writing-tests\":0.86,\"release\":0.02,\"__jev_none_of_these__\":0.12},\"confidence\":0.82},\"needs_skill\":{\"type\":\"noul\",\"noul\":0.91},\"effort\":{\"type\":\"choice\",\"choice\":\"low\",\"probabilities\":{\"none\":0.02,\"minimal\":0.05,\"low\":0.60,\"medium\":0.25,\"high\":0.08},\"confidence\":0.55}},\"usage\":{\"input_tokens\":212,\"output_tokens\":0}}"
  "One decision: writing-tests fits, low effort, needs_skill 0.91.")

(defparameter +sw-unsure+
  "{\"model\":\"jev\",\"answers\":{\"skill\":{\"type\":\"choice\",\"choice\":\"writing-tests\",\"probabilities\":{\"writing-tests\":0.41,\"release\":0.02,\"__jev_none_of_these__\":0.57},\"confidence\":0.30},\"needs_skill\":{\"type\":\"noul\",\"noul\":0.20},\"effort\":{\"type\":\"choice\",\"choice\":\"minimal\",\"probabilities\":{\"none\":0.30,\"minimal\":0.50,\"low\":0.20},\"confidence\":0.40}},\"usage\":{}}"
  "A decision that fits nothing: low needs_skill, low confidence.")

(defparameter +sw-none+
  "{\"model\":\"jev\",\"answers\":{\"skill\":{\"type\":\"choice\",\"choice\":\"__jev_none_of_these__\",\"probabilities\":{\"writing-tests\":0.20,\"release\":0.10,\"__jev_none_of_these__\":0.70},\"confidence\":0.90},\"needs_skill\":{\"type\":\"noul\",\"noul\":0.95},\"effort\":{\"type\":\"choice\",\"choice\":\"medium\",\"probabilities\":{\"none\":0.10,\"medium\":0.70},\"confidence\":0.50}},\"usage\":{}}"
  "A decision that chooses the explicit none option outright.")

(defmacro with-switchyard ((requests &key (answers '(list +sw-answer+)) (statuses '(list 200))
                                          (env '(("TYPESAFE_API_KEY" . "ts-env"))) (auth "{}") config)
                           &body forms)
  "FORMS with the cell started on CONFIG, AUTH as the auth file, ENV as the
environment, and the endpoint answering ANSWERS with STATUSES in turn (the
last repeated): REQUESTS each (METHOD URL HEADERS CONTENT) it was sent, oldest first."
  `(with-cell-stop ((switchyard-start ,@config))
     (with-temp-auth (auth ,auth)
       (let ((nle::*auth-file-path* auth) (,requests '())
             (answers ,answers) (statuses ,statuses))
         (with-stubbed-fdefinitions
             ((nle::credential-env (name) (cdr (assoc name ',env :test #'equal)))
              (dex:request (url &rest args)
                           (setf ,requests (append ,requests
                                                   (list (list (getf args :method) url (getf args :headers)
                                                               (getf args :content)))))
                           (values (if (rest answers) (pop answers) (first answers))
                                   (if (rest statuses) (pop statuses) (first statuses))
                                   (make-hash-table :test 'equal))))
           ,@forms)))))

(defun sw-body (request)
  "The decoded JSON body of REQUEST."
  (nlk:decode-json (fourth request)))

(defun sw-skill-sets ()
  "The candidates every routing test names."
  '("writing-tests" "release"))

;;; --- the one decision -------------------------------------------------------------

(deftest switchyard-cell-posts-one-decision-request ()
  (with-switchyard (requests)
    (switchyard:route "the release is red: the parser test fails on windows"
                      :skills (sw-skill-sets))
    (is (eq :post (first (first requests))))
    (is (equal "https://api.typesafe.ai/v1/systemone" (second (first requests)))
        "auto prefers TypeSafe when its key is set")
    (is (equal "Bearer ts-env" (sw-header (third (first requests)) "authorization")))
    (is (equal "application/json" (sw-header (third (first requests)) "content-type")))
    (let* ((body (sw-body (first requests)))
           (questions (nlk:json-value body :object "questions"))
           (criteria (nlk:json-value questions :object "skill" "criteria")))
      (declare (ignore criteria))
      (is (= 2 (nlk:json-value body :integer "state" "candidate_count")))
      (is (search "parser test fails" (nlk:json-value body :string "state" "task")))
      (is (equal "high" (nlk:json-value body :string "state" "effort_cap")) "the section's cap")
      (is (equal "choice" (nlk:json-value questions :string "skill" "type")))
      (let ((wire (fourth (first requests))))
        (is (search "\"writing-tests\":" wire) "a candidate is one of the options")
        (is (search "__jev_none_of_these__" wire) "and the none option is always offered")
        (is (search "\"type\":\"noul\"" wire) "needs_skill is a noul")
        (is (search "\"effort\":{\"type\":\"choice\"" wire))
        (is (not (search "\"xhigh\"" wire)) "the effort choice stops at the section's cap"))
      (is (null (nlk:json-value body :any "provider")) "TypeSafe takes no fallback hint"))))

(deftest switchyard-cell-reads-the-decision ()
  (with-switchyard (requests)
    (let ((verdict (switchyard:route "the release is red" :skills (sw-skill-sets))))
      (is (equal "writing-tests" (getf verdict :skill)) "adopted above every threshold")
      (is (eq :adopted (getf verdict :skill-reason)))
      (is (equal "low" (getf verdict :effort)))
      (is (equal "high" (getf verdict :effort-in-effect)) "the cap is reported")
      (is (equal "typesafe/jev-20260901" (getf verdict :model)))
      (is (= 212 (getf (getf verdict :usage) :input-tokens)))
      (is (< (abs (- 0.91 (getf verdict :needs-skill))) 1d-4) "the needs_skill probability"))))

(deftest switchyard-cell-abstains-below-the-thresholds ()
  (with-switchyard (requests :answers (list +sw-unsure+))
    (let ((verdict (switchyard:route "something" :skills (sw-skill-sets))))
      (is (null (getf verdict :skill)))
      (is (eq :below-needs-threshold (getf verdict :skill-reason))))
    (is (equal "minimal" (getf (switchyard:route "x" :skills nil) :effort)) "the effort still answers"))
  (with-switchyard (requests :answers (list +sw-none+))
    (let ((verdict (switchyard:route "something" :skills (sw-skill-sets))))
      (is (null (getf verdict :skill)))
      (is (eq :none-chosen (getf verdict :skill-reason)) "the none option is an answer, not a skill"))))

;;; --- where it goes, and with which key ---------------------------------------------

(deftest switchyard-cell-openrouter-when-it-is-the-only-key ()
  (with-switchyard (requests :env (("OPENROUTER_API_KEY" . "or-env")))
    (switchyard:route "cut a release" :skills nil)
    (is (equal "https://openrouter.ai/api/alpha/decisions" (second (first requests))))
    (is (equal "Bearer or-env" (sw-header (third (first requests)) "authorization")))
    (let ((body (sw-body (first requests))))
      (is (equal "typesafe/jev-1.13" (nlk:json-value body :string "model")) "OpenRouter's alias")
      (is (eq :null (gethash "allow_fallbacks" (nlk:json-value body :object "provider")))))))

(deftest switchyard-cell-names-its-own-endpoint-and-model ()
  (with-switchyard (requests :config ("provider" "typesafe" "endpoint" "https://gateway.example/systemone"
                                      "model" "jev-next" "effort_cap" "low"))
    (let ((verdict (switchyard:route "x" :skills nil :effort "medium")))
      (is (equal "https://gateway.example/systemone" (second (first requests))))
      (is (equal "jev-next" (nlk:json-value (sw-body (first requests)) :string "model")))
      (is (equal "medium" (getf verdict :effort-in-effect)) "the call's level, never above the section's")
      (is (equal "low" (getf verdict :effort)) "the level the decision chose"))))

(deftest switchyard-cell-without-a-key-says-where-to-make-one ()
  (with-switchyard (requests :env ())
    (let ((text (refusal-text switchyard:switchyard-error (switchyard:route "x" :skills nil))))
      (is (search "no key for openrouter" text) text)
      (is (search "OPENROUTER_API_KEY" text))
      (is (search "openrouter.ai/keys" text)))
    (is (null requests) "and nothing was sent")))

;;; --- bounds and refusals -----------------------------------------------------------

(deftest switchyard-cell-bounds-and-refuses-what-it-cannot-ask ()
  (with-switchyard (requests)
    (switchyard:route (make-string 4100 :initial-element #\x) :skills nil)
    (is (= 4000 (length (nlk:json-value (sw-body (first requests)) :string "state" "task")))
        "the task is cut at 4,000 characters")
    (let ((before (length requests)))
      (flet ((refused (&rest arguments)
               (refusal-text switchyard:switchyard-error (apply #'switchyard:route arguments))))
        (is (search "route needs a task" (refused "" :skills nil)))
        (is (search "not a reasoning level" (refused "x" :skills nil :effort "extreme")))
        (is (search "reserved" (refused "x" :skills '("__jev_none_of_these__"))))
        (is (search "at most 200 skills"
                    (refused "x" :skills (loop for i below 201 collect (format nil "skill-~d" i)))))
        (is (search "not a skill name" (refused "x" :skills '((#\a . "b"))))))
      (is (= before (length requests)) "nothing was sent"))))

;;; --- the tool, the command -----------------------------------------------------------

(deftest switchyard-cell-tool-answers-in-one-line ()
  (with-switchyard (requests)
    (let* ((contract (nlk:contract 'switchyard::route-request))
           (request (nlk:contract-decode contract
                                          (nlk:make-json-object "task" "the release is red"
                                                                 "skills" #("writing-tests" "release"))))
           (text (switchyard::route-tool request)))
      (is (search "skill: writing-tests" text) text)
      (is (search "effort: low" text))
      (is (search "typesafe/jev-20260901" text))))
  (with-switchyard (requests :env ())
    (is (search "no key for openrouter"
                (switchyard::route-tool (nlk:contract-decode (nlk:contract 'switchyard::route-request)
                                                              (nlk:make-json-object "task" "x"))))
        "a refusal is a line, never a turn's failure")))

(deftest switchyard-cell-command-says-where-the-key-comes-from ()
  (with-switchyard (requests :answers (list +sw-answer+))
    (let ((text (cell-entry "nodecode-switchyard" "switchyard" "status")))
      (is (search "switchyard: typesafe key from TYPESAFE_API_KEY" text) text)
      (is (search "model typesafe/jev-latest" text))
      (is (search "effort cap high" text)))
    (is (search "switchyard: typesafe" (cell-entry "nodecode-switchyard" "switchyard" ""))
        "bare /switchyard is the status too")
    (is (null requests) "status asks no one")
    (is (search "answered in" (cell-entry "nodecode-switchyard" "switchyard" "check")))
    (is (search "nope is not one of its choices" (cell-entry "nodecode-switchyard" "switchyard" "nope"))))
  (with-switchyard (requests :env ())
    (is (search "no key: make one at https://openrouter.ai/keys"
                (cell-entry "nodecode-switchyard" "switchyard" "status")))))

(deftest switchyard-cell-lists-the-skills-it-keeps ()
  (with-switchyard (requests :answers (list +sw-answer+))
    (let ((text (switchyard:skills)))
      (is (stringp text))
      (is (or (search "keeps no prose skills" text) (search "-" text))))
    (is (null requests) "the listing asks no one")))
