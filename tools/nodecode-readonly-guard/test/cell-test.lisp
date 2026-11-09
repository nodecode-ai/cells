;;;; cell-test.lisp --- the guard's lifecycle, its refusals and its lease.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the mode itself: a read-only session's writes,
;;;; commands and unknown tools are refused before they run, its reads and
;;;; single read-only commands pass, plan mode lets the plan markdown through
;;;; and nothing else, writable blocks nothing and lapses back on its own, and
;;;; the switch is one session's.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "readonly-guard"
  (:hooks :tool 'nle::read-harness-sections)
  (:command "readonly")
  (:help :readonly-guard)
  (:running (is (search "read-only" (readonly-guard-command "status")) "a new session starts read-only"))
  (:refused ("default_mode" "sometimes")
            ("plans_dir" 42)))

;;; --- read-only: what passes, what does not --------------------------------

(deftest readonly-guard-cell-readonly-refuses-changes
  (with-cell-stop ((readonly-guard-start))
    (is (eq :allowed (readonly-guard-verdict "(* 6 7)")) "arithmetic reads")
    (is (eq :allowed (readonly-guard-verdict "(car (list 1 2))")) "a list read")
    (is (eq :allowed (readonly-guard-verdict "(help :eval)")) "the manual is a read")
    (is (eq :allowed (readonly-guard-verdict "(gethash \"k\" (list))")) "a lookup reads")
    (let ((verdict (readonly-guard-verdict "(write-file \"x.txt\" \"y\")")))
      (is (search "refused by nodecode-readonly-guard" verdict) (format nil "REFUSAL: ~a" verdict))
      (is (search "write-file is not a read-only call" verdict) "a write is named")
      (is (search "Do not retry" verdict) "and the call did not run"))
    (is (search "setf is not a read-only call" (readonly-guard-verdict "(setf x 1)")) "an assignment is refused")
    (is (search "defun is not a read-only call" (readonly-guard-verdict "(defun helper () 1)")) "a definition is refused")
    (is (search "helper is not a read-only call" (readonly-guard-verdict "(helper)")) "and so is a call to what it would have defined")
    (is (search "read-only mode" (readonly-guard-verdict "(sh \"rm -rf /tmp/x\")")) "a shell command that writes is refused")
    (is (search "not a read-only tool" (readonly-guard-tool-verdict "remember")) "a tool that is not an inspection is refused")
    (is (eq :allowed (readonly-guard-tool-verdict "web_search")) "a search may read")))

(deftest readonly-guard-cell-readonly-lets-probes-through
  (with-cell-stop ((readonly-guard-start))
    (is (eq :allowed (readonly-guard-verdict "(sh \"ls -la\")")) "a listing")
    (is (eq :allowed (readonly-guard-verdict "(sh \"git log -n 2\")")) "git log")
    (is (eq :allowed (readonly-guard-verdict "(sh \"git branch -a\")")) "a git listing")
    (is (eq :allowed (readonly-guard-verdict "(sh \"rg -n todo src\")")) "a search")
    (is (eq :allowed (readonly-guard-verdict "(sh \"find . -name *.lisp -print\")")) "a find")
    (is (search "shell operator" (readonly-guard-verdict "(sh \"ls; rm -rf /tmp/x\")")) "composition is refused")
    (is (search "shell operator" (readonly-guard-verdict "(sh \"cat x | sh\")")) "a pipe is refused")
    (is (search "expansion" (readonly-guard-verdict "(sh \"cat $HOME/x\")")) "an expansion is refused")
    (is (search "is not a listing" (readonly-guard-verdict "(sh \"git branch new-thing\")")) "a git command that would create a branch is refused")
    (is (search "runs something" (readonly-guard-verdict "(sh \"git -c alias.x=!rm x\")")) "config injection is refused")
    (is (search "not a read-only primary" (readonly-guard-verdict "(sh \"find . -delete\")")) "find -delete is refused")
    (is (search "writes a file" (readonly-guard-verdict "(sh \"sort -o out.txt in.txt\")")) "a writing flag is refused")
    (is (search "not one of the read-only programs" (readonly-guard-verdict "(sh \"python3 x.py\")")) "a program off the list is refused")
    (is (search "does not show" (readonly-guard-verdict "(sh (format nil \"ls\"))")) "a command the form hides is refused, failing closed")))

;;; --- plan mode: the plan markdown is the one write ------------------------

(deftest readonly-guard-cell-plan-lets-the-plan-file-through
  (let* ((dir (readonly-guard-scratch))
         (inside (merge-pathnames "2026-01-02-plan.md" dir))
         (outside (uiop:merge-pathnames* "src/cell.lisp" (uiop:getcwd))))
    (ensure-directories-exist inside)
    (with-cell-stop ((readonly-guard-start "default_mode" "plan" "plans_dir" (uiop:native-namestring dir)))
      (is (eq :allowed (readonly-guard-verdict (format nil "(write-file ~s \"# Plan\")" (uiop:native-namestring inside)))) "the plan file gets through")
      (is (search "outside" (readonly-guard-verdict (format nil "(write-file ~s \"x\")" (uiop:native-namestring outside)))) "a write outside the plans directory is refused")
      (is (eq :allowed (readonly-guard-verdict "(sh \"ls\")")) "a probe still runs")
      (is (search "shell operator" (readonly-guard-verdict "(sh \"ls && ls\")")) "and composition still does not")
      (is (search "Plan mode is ON" (readonly-guard-note "")) "the note says which mode is on")
      (is (eq :allowed (readonly-guard-verdict "(* 2 3)")) "reads pass"))))

;;; --- writable is a lease ---------------------------------------------------

(deftest readonly-guard-cell-writable-runs-anything-and-lapses
  (with-cell-stop ((readonly-guard-start "writable_expiry_minutes" 30))
    (readonly-guard-command "writable")
    (is (eq :allowed (readonly-guard-verdict "(write-file \"x.txt\" \"y\")")) "writable blocks nothing")
    (is (null (readonly-guard-note "")) "and carries no standing note")
    (let ((entry (nodecode-readonly-guard::state-for "")))
      (setf (getf entry :turn) "an-older-turn"
            (getf entry :expires) (- (get-universal-time) 60)))
    (is (search "read-only mode" (readonly-guard-verdict "(write-file \"x.txt\" \"y\")")) "a lapsed lease is read-only again")
    (is (search "read-only" (readonly-guard-command "status")) "and status agrees"))
  (with-cell-stop ((readonly-guard-start "writable_expiry_minutes" 0))
    (readonly-guard-command "writable")
    (is (search "stays on" (readonly-guard-command "status")) "0 keeps writable on")))

;;; --- the switch is one session's -------------------------------------------

(deftest readonly-guard-cell-the-switch-is-per-session
  (with-cell-stop ((readonly-guard-start))
    (is (search "read-only" (readonly-guard-command "status")) "starts read-only")
    (readonly-guard-command "plan")
    (is (search "plan" (readonly-guard-command "status")) "the command switches this session")
    (readonly-guard-command "readonly")
    (is (search "read-only mode" (readonly-guard-verdict "(write-file \"x.txt\" \"y\")")) "back to read-only")
    (nodecode-readonly-guard::set-mode "another-session" :writable)
    (is (search "read-only mode" (readonly-guard-verdict "(write-file \"x.txt\" \"y\")")) "another session's writable leaves this one read-only")
    (readonly-guard-command "toggle")
    (is (eq :allowed (readonly-guard-verdict "(write-file \"x.txt\" \"y\")")) "toggle leaves a writable session")
    (readonly-guard-command "toggle")
    (is (search "read-only mode" (readonly-guard-verdict "(write-file \"x.txt\" \"y\")")) "and comes back")
    (is (search "usage" (readonly-guard-command "nonsense")) "an unknown word answers the usage")
    (is (search "read-only" (readonly-guard-command "on")) "on is read-only")
    (is (search "read-only mode" (readonly-guard-verdict "(write-file \"x.txt\" \"y\")")) "and refuses a write again")
    (is (search "writable" (readonly-guard-command "off")) "off is writable")
    (is (eq :allowed (readonly-guard-verdict "(write-file \"x.txt\" \"y\")")) "and lets the write through")))
