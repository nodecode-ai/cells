;;;; cell-test.lisp --- Skill Court against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test starts the cell on a record file of its own under the
;;;; scratch home, so nothing here reads or writes the operator's court.
;;;; There is no network in this slice: the cell has none.

(in-package #:nodecode.test)

(define-test-slice "skill-court" "SKILL-COURT-CELL-" :start nodecode-skill-court:start-cell)

(define-cell-lifecycle-tests "skill-court"
  (:config "min_tasks" 3)
  (:help :skill-court)
  (:command "skill-court")
  (:refused ("min_tasks" 0)))

;;; --- fixtures -------------------------------------------------------------------

(defvar *court-counter* 0 "How many record files this run has made.")

(defun fresh-court-file ()
  "A record file of this test's own, empty."
  (let ((path (nlk:home (format nil "test/court-~d.sexp" (incf *court-counter*)))))
    (ensure-directories-exist path)
    (when (probe-file path) (delete-file path))
    path))

(defmacro with-court ((&rest config) &body body)
  "Run BODY with the cell started on CONFIG and a record file of its own."
  `(let ((nodecode-skill-court::*state-file* (fresh-court-file))
         (nodecode-skill-court::*state* nil))
     (unwind-protect
          (with-cell-stop ((skill-court-start ,@config)) ,@body)
       (ignore-errors (delete-file nodecode-skill-court::*state-file*)))))

(defun court-args (name pairs)
  "PAIRS decoded through contract NAME, the way a tool call arrives."
  (nlk:contract-decode (nlk:contract name) (apply #'nlk:make-json-object pairs)))

(defun add-case (skill prompt)
  "One case through the tool's own body."
  (nodecode-skill-court::add-case
   (court-args 'nodecode-skill-court::court-case
               (list "skill" skill "prompt" prompt "check" ""))))

(defun seed-cases (skill &key (count 3))
  "COUNT cases on SKILL, exactly what the model would add."
  (dotimes (i count)
    (add-case skill (format nil "task ~d for ~a" (1+ i) skill))))

(defun add-pair (skill case with without &key (with-tokens 0) (without-tokens 0))
  "One pair through the tool's own body: WITH and WITHOUT are T or NIL."
  (nodecode-skill-court::record-pair
   (court-args 'nodecode-skill-court::court-record
               (list "skill" skill "case" case
                     "with_success" (if with t nil)
                     "without_success" (if without t nil)
                     "with_tokens" with-tokens "without_tokens" without-tokens))))

(defun report-of (skill)
  "The report text for one skill, through the tool's own body."
  (nodecode-skill-court::report-tool
   (court-args 'nodecode-skill-court::court-report (list "skill" skill))))

;;; --- the arithmetic ---------------------------------------------------------------

(deftest skill-court-cell-calls-a-skill-neutral ()
  (let ((pairs (list (nodecode-skill-court:make-pair "1" :pass :pass)
                      (nodecode-skill-court:make-pair "2" :pass :pass)
                      (nodecode-skill-court:make-pair "3" :pass :pass))))
    (let ((result (nodecode-skill-court:score pairs)))
      (is (equal "neutral" (getf result :verdict)))
      (is (= 3 (getf result :tasks)))
      (is (equal "no measurable difference in success or token use" (getf result :reason))))))

(deftest skill-court-cell-wants-more-cases ()
  (let ((result (nodecode-skill-court:score
                 (list (nodecode-skill-court:make-pair "1" :pass :fail)
                       (nodecode-skill-court:make-pair "2" :pass :fail)))))
    (is (equal "inconclusive" (getf result :verdict)))
    (is (search "at least 3 are needed" (getf result :reason)))))

(deftest skill-court-cell-averages-repeats-per-case ()
  ;; One case run three times, passing twice: the case is worth two thirds,
  ;; not two, so a repeated pair is never mistaken for three cases.
  (let ((result (nodecode-skill-court:score
                 (list (nodecode-skill-court:make-pair "1" :pass :fail)
                       (nodecode-skill-court:make-pair "1" :pass :fail)
                       (nodecode-skill-court:make-pair "1" :fail :fail)
                       (nodecode-skill-court:make-pair "2" :pass :fail)
                       (nodecode-skill-court:make-pair "3" :pass :fail)))))
    (is (= 5 (getf result :pairs)))
    (is (= 3 (getf result :tasks)))
    (is (equal "helps" (getf result :verdict)))))

;;; --- a skill on trial ---------------------------------------------------------------

(deftest skill-court-cell-a-helping-skill-helps ()
  (with-court ()
    (seed-cases "queue-records")
    (loop for case in '("1" "2" "3") do (add-pair "queue-records" case t nil))
    (let ((text (report-of "queue-records")))
      (is (search "helps" text))
      (is (search "rose from 0% without the skill to 100% with it" text))
      (is (search "3 cases, 3 pairs" text)))))

(deftest skill-court-cell-a-hurting-skill-hurts ()
  (with-court ()
    (seed-cases "noisy")
    (loop for case in '("1" "2" "3") do (add-pair "noisy" case nil t))
    (let ((text (report-of "noisy")))
      (is (search "hurts" text))
      (is (search "fell from 100% without the skill to 0% with it" text)))))

(deftest skill-court-cell-token-cost-alone-can-hurt ()
  (with-court ()
    (seed-cases "expensive")
    (loop for case in '("1" "2" "3")
          do (add-pair "expensive" case t t :with-tokens 2000 :without-tokens 1000))
    (let ((text (report-of "expensive")))
      (is (search "hurts" text) text)
      (is (search "success was the same" text) text)
      (is (search "+100% tokens" text)))))

(deftest skill-court-cell-token-savings-alone-can-help ()
  (with-court ()
    (seed-cases "frugal")
    (loop for case in '("1" "2" "3")
          do (add-pair "frugal" case t t :with-tokens 700 :without-tokens 1000))
    (let ((text (report-of "frugal")))
      (is (search "helps" text) text)
      (is (search "-30% tokens" text) text))))

(deftest skill-court-cell-refuses-an-unknown-case ()
  (with-court ()
    (seed-cases "one-case" :count 1)
    (let ((text (add-pair "one-case" "7" t nil)))
      (is (search "has no case 7" text)))
    (is (= 1 (length (nodecode-skill-court::court-cases "one-case"))))))

(deftest skill-court-cell-quarantines-and-releases ()
  (with-court ()
    (seed-cases "noisy")
    (add-pair "noisy" "1" nil t)
    (let ((text (nodecode-skill-court::quarantine-tool
                 (court-args 'nodecode-skill-court::court-quarantine
                             (list "skill" "noisy" "note" "the line misled it")))))
      (is (search "is quarantined" text))
      (is (search "the line misled it" text)))
    (is (nodecode-skill-court::quarantined "noisy"))
    (is (search "[quarantined]"
                (nodecode-skill-court::report-tool
                 (court-args 'nodecode-skill-court::court-report '()))))
    (let ((text (nodecode-skill-court::quarantine-tool
                 (court-args 'nodecode-skill-court::court-quarantine
                             (list "skill" "noisy" "action" "release")))))
      (is (search "visible again" text)))
    (is (null (nodecode-skill-court::quarantined "noisy")))))

(deftest skill-court-cell-keeps-its-record-across-a-restart ()
  (with-court ()
    (seed-cases "keeper")
    (loop for case in '("1" "2" "3") do (add-pair "keeper" case t nil))
    (is (probe-file nodecode-skill-court::*state-file*) "the record is on disk")
    (let ((nodecode-skill-court::*state* nil))
      (setf nodecode-skill-court::*state* (nodecode-skill-court::load-state))
      (is (= 3 (length (nodecode-skill-court::court-cases "keeper"))))
      (is (= 3 (length (nodecode-skill-court::court-pairs "keeper"))))
      (is (equal "helps" (getf (nodecode-skill-court::court-score "keeper") :verdict))))))

;;; --- the model's and the operator's side ---------------------------------------------

(deftest skill-court-cell-registers-its-four-tools ()
  (with-court ()
    (dolist (name '("skill_court_case" "skill_court_record"
                     "skill_court_report" "skill_court_quarantine"))
      (is (nle::find-tool name) name))
    (let ((mine (remove-if-not (lambda (tool) (uiop:string-prefix-p "skill_court_" (first tool)))
                               nle::*tools*)))
      (is (= 4 (length mine)) "the court registers four tools and no more"))))

(deftest skill-court-cell-answers-its-command ()
  (with-court ()
    (is (search "min_tasks 3" (nodecode-skill-court::run-slash "" nil)))
    (seed-cases "queue-records")
    (let ((text (nodecode-skill-court::run-slash
                 "task queue-records queue the record" nil)))
      (is (search "case 4" text) text))
    (let ((text (nodecode-skill-court::run-slash "cases queue-records" nil)))
      (is (search "4 cases" text) text))
    (is (search "quarantined" (nodecode-skill-court::run-slash
                                "quarantine queue-records too noisy" nil)))
    (is (search "visible again" (nodecode-skill-court::run-slash
                                  "release queue-records" nil)))
    (is (search "queue-records" (nodecode-skill-court::run-slash "list" nil)))
    (is (search "evidence is gone" (nodecode-skill-court::run-slash
                                     "reset queue-records" nil)))
    (is (null (nodecode-skill-court::skill-names)) "and the court is empty again")))
