;;;; cell-test.lisp --- plan mode's lifecycle, its refusals and its approval.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the mode's own behaviour: a mutating eval call is
;;;; refused while planning and runs once the operator approves, a plan file
;;;; under the plans directory is the one write that gets through, the
;;;; standing note rides every request while the mode is on, and the section's
;;;; two shapes (a directory, an integer) are what a start refuses on.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "plan-mode"
  (:hooks 'nle::read-harness-sections)
  (:command "plan")
  (:help :plan-mode)
  (:running (is (nle::find-tool "plan_mode") "the plan_mode tool is registered"))
  (:refused ("plans_dir" 42)))

;;; --- the lock: a mutating call waits for the approval ----------------------

(deftest plan-mode-cell-blocks-mutating-calls-while-planning
  (with-cell-stop ((plan-mode-start "plans_dir" (plan-mode-scratch-text)))
    (plan-mode-command "on Fix the parser")
    (is (search "write-file" (plan-mode-verdict "(write-file \"README.md\" \"x\")")) "a write outside the plans directory is refused")
    (is (search "sh runs" (plan-mode-verdict "(sh \"ls\")")) "a command is refused")
    (let ((verdict (plan-mode-verdict "(delete-file \"/tmp/x\")")))
      (is (search "refused by nodecode-plan-mode" verdict)
          (format nil "REFUSAL: ~a" verdict))
      (is (search "Do not retry" verdict) "and that the call did not run"))
    (is (eq :allowed (plan-mode-verdict "(* 6 7)")) "a read is allowed")
    (is (eq :allowed (plan-mode-verdict "(with-open-file (in \"x\") (read in))")) "a form that only reads is allowed")
    (is (search "does not show" (plan-mode-verdict "(write-file (merge-pathnames \"p.md\" (plans-dir)) \"x\")")) "a write whose path the form does not show is refused, failing closed")))

(deftest plan-mode-cell-lets-the-plan-file-through
  (let* ((dir (plan-mode-scratch))
         (inside (merge-pathnames "2026-01-02_030405-plan.md" dir)))
    (with-cell-stop ((plan-mode-start "plans_dir" (uiop:native-namestring dir)))
      (plan-mode-command "on")
      (is (eq :allowed (plan-mode-verdict (format nil "(write-file ~s \"# Plan\")" (uiop:native-namestring inside)))) "the plan file is the one write that gets through")
      (is (eq :allowed (plan-mode-verdict (format nil "(write-file ~s \"# Plan\")"
                                                   (uiop:native-namestring (merge-pathnames "src/x.lisp" dir))))) "anything under the plans directory is the plan's own workspace")
      (is (stringp (plan-mode-verdict "(sh \"ls\")")) "and the lock is still on")
      (plan-mode-command "off")
      (is (eq :allowed (plan-mode-verdict "(sh \"ls\")")) "off lifts it"))))

;;; --- submit: the operator's card is the only way out -----------------------

(deftest plan-mode-cell-submit-asks-the-operator-and-implements
  (let ((dir (plan-mode-scratch))
        (asked nil))
    (plan-mode-plan dir)
    (call-with-plan-mode-operator
     (lambda (title &rest keys)
       (setf asked (cons title keys))
       (values "Approve, implement" :answered))
     (lambda ()
       (with-cell-stop ((plan-mode-start "plans_dir" (uiop:native-namestring dir)))
         (plan-mode-command "on")
         (let ((answer (plan-mode-tool-call "submit" "summary" "Make the parser lenient")))
           (is (search "Approved" answer) "the model is told it may implement")
           (is (search "Implement it now" answer) "and that this turn is the one")
           (is (search "Make the parser lenient" (first asked)) "the card names the plan")
           (is (equal '("Approve, implement" "Keep planning") (getf (rest asked) :choices)) "offering exactly the two answers the mode acts on"))
         (is (eq :allowed (plan-mode-verdict "(sh \"make test\")")) "and the lock is off"))))))

(deftest plan-mode-cell-submit-with-nobody-to-ask-stands-waiting
  (let ((dir (plan-mode-scratch)))
    (plan-mode-plan dir)
    (call-with-plan-mode-operator
     (lambda (&rest args) (declare (ignore args)) (values nil :timeout))
     (lambda ()
       (with-cell-stop ((plan-mode-start "plans_dir" (uiop:native-namestring dir)))
         (let ((answer (progn (plan-mode-command "on") (plan-mode-tool-call "submit"))))
           (is (search "/plan approve" answer) "the model is told how the operator approves"))
         (is (search "awaiting" (plan-mode-command "status")) "and the mode still waits")
         (is (stringp (plan-mode-verdict "(sh \"ls\")")) "with the lock still on")
         (is (search "Approved" (plan-mode-command "approve")) "until the operator approves it by name")
         (is (eq :allowed (plan-mode-verdict "(sh \"ls\")")) "which lifts it"))))))

(deftest plan-mode-cell-keep-planning-keeps-the-lock-and-carries-the-note
  (let ((dir (plan-mode-scratch)))
    (plan-mode-plan dir)
    (call-with-plan-mode-operator
     (lambda (&rest args) (declare (ignore args)) (values "Keep planning" :answered))
     (lambda ()
       (with-cell-stop ((plan-mode-start "plans_dir" (uiop:native-namestring dir)))
         (plan-mode-command "on")
         (is (search "keep planning" (plan-mode-tool-call "submit")) "the model is told to revise")
         (is (stringp (plan-mode-verdict "(sh \"ls\")")) "and nothing else may change")
         (plan-mode-command "reject make the second step smaller")
         (is (search "make the second step smaller" (plan-mode-verdict "(sh \"ls\")")) "the operator's note rides the next refusal")
         (is (search "make the second step smaller" (format nil "~s" (plan-mode-section "s1"))) "and the next request"))))))

;;; --- the standing note, and the rest of the command -------------------------

(deftest plan-mode-cell-the-note-rides-every-request-while-on
  (let ((dir (plan-mode-scratch)))
    (with-cell-stop ((plan-mode-start "plans_dir" (uiop:native-namestring dir)))
      (is (notany (lambda (entry) (equal "plan-mode" (car entry))) (plan-mode-section "s1")) "nothing rides a request while the mode is off")
      (plan-mode-command "on Add a --dry-run flag")
      (let ((sections (plan-mode-section "s1")))
        (is-present (entry (assoc "plan-mode" sections :test #'equal))
          "the plan-mode section rides the request"
          (is (search "Plan mode is ON" (cdr entry)) "the note says the mode is on")
          (is (search "Add a --dry-run flag" (cdr entry)) "and what it is for")
          (is (search "submit" (cdr entry)) "and how to submit it"))
        (is (search "nothing here" (cdr (assoc "index" sections :test #'equal))) "the next reader's own entries stay beneath it"))
      (plan-mode-command "done")
      (is (notany (lambda (entry) (equal "plan-mode" (car entry))) (plan-mode-section "s1")) "and it is taken back when the mode ends"))))

(deftest plan-mode-cell-status-show-and-the-plan-file
  (let ((dir (plan-mode-scratch)))
    (let ((plan (plan-mode-plan dir "# Plan\n\n1. Read the parser.\n")))
      (with-cell-stop ((plan-mode-start "plans_dir" (uiop:native-namestring dir)))
        (is (search "off" (plan-mode-command "status")) "status says the mode is off")
        (plan-mode-command "on")
        (is (search "planning" (plan-mode-command "status")) "then planning")
        (is (search "2026-01-02_030405-plan.md" (plan-mode-command "status")) "naming the plan files")
        (is (search "Read the parser" (plan-mode-command "show")) "show prints the plan")
        (is (search (uiop:native-namestring plan) (plan-mode-command "edit")) "edit names the path to change")
        (is (search "off" (plan-mode-command "off")) "and off leaves the mode off")
        (is (search "off" (plan-mode-command "status")))))))

(deftest plan-mode-cell-submit-refuses-without-a-plan-file
  (let ((dir (plan-mode-scratch)))
    (with-cell-stop ((plan-mode-start "plans_dir" (uiop:native-namestring dir)))
      (is (search "No plan file yet" (progn (plan-mode-command "on")
                                             (plan-mode-tool-call "submit"))) "submitting with nothing written says what to write")
      (is (search "off" (plan-mode-tool-call "off")) "and the agent may leave before it submits"))))
