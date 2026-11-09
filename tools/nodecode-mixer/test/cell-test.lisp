;;;; cell-test.lisp --- the four dials, the grammar, and the gate on a call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the mixer's own reading: which of a call's writes
;;;; and commands the dials see, what each stage refuses, and that the /mixer
;;;; desk changes the dial a session runs on.

(in-package #:nodecode.test)

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "mixer"
  (:hooks :tool)
  (:running (is (equal "auto" (getf nodecode-mixer::*saved* :edits)) "the section's edits value is the saved default")
            (is (equal 0 (getf nodecode-mixer::*saved* :lines)) "lines start off"))
  (:refused ("edits" "sometimes") ("lines" -3)))

;;; --- plan mode: read-only --------------------------------------------------------

(deftest mixer-cell-plan-blocks-writes (with-started-mixer ("edits" "plan")
  (let ((verdict (mixer-verdict "(write-file \"/tmp/mixer-plan/a.txt\" \"hello\")")))
    (is (search "plan mode is read-only" verdict) "a write names plan mode")
    (is (search "mixer:edits" verdict) "and the dial"))
  (is (search "read-only commands" (mixer-verdict "(sh \"git push origin main\")")) "a shell that writes is refused")
  (is (eq :allowed (mixer-verdict "(sh \"git status --short\")")) "a read stays read")
  (is (eq :allowed (mixer-verdict "(sh \"grep -rn mixer src\")")) "so does a grep")
  (is (eq :allowed (mixer-verdict "(* 6 7)")) "and a call that only computes")))

;;; --- scope -----------------------------------------------------------------------

(deftest mixer-cell-scope-holds-writes-inside-its-globs
    (with-mixer-cell (dir "scope" (vector (format nil "~a**" dir)))
  (let ((inside (format nil "~asrc/a.lisp" dir))
        (outside (format nil "~a../docs/b.md" dir))
        (redirect (format nil "(sh \"echo hi > ~a../escaped.txt\")" dir)))
    (is (eq :allowed (mixer-verdict (format nil "(write-file \"~a\" \"x\")" inside))) "a write inside the glob runs")
    (let ((verdict (mixer-verdict (format nil "(write-file \"~a\" \"x\")" outside))))
      (is (search "mixer:scope" verdict) "a write outside it is refused")
      (is (search "allowed globs" verdict) "and the refusal names the globs"))
    (is (search "mixer:scope" (mixer-verdict redirect)) "a redirect is read as a write too"))))

;;; --- the line budget --------------------------------------------------------------

(deftest mixer-cell-lines-budget-refuses-one-call-too-many (with-mixer-cell (dir "lines" 5)
  (let* ((nodecode-mixer::*session* "s-lines")
         (body (format nil "one~%two~%three~%four~%five~%six"))
         (form (format nil "(write-file \"~aa.txt\" \"~a\")" dir body)))
    (let ((verdict (mixer-verdict form)))
      (is (search "mixer:lines" verdict) "six lines do not fit a five-line budget")
      (is (search "budget" verdict) "and the refusal says how much is left"))
    (is (search "mixer:lines" (mixer-verdict form)) "and the reservation stands for the rest of the turn"))))

(deftest mixer-cell-lines-off-lets-it-through (with-mixer-cell (dir "lines" 0)
  (is (eq :allowed
          (mixer-verdict (format nil "(write-file \"~ab.txt\" \"a~%b~%c~%d~%e~%f~%g\")" dir))) "0 turns the budget off")))

;;; --- dependencies ------------------------------------------------------------------

(deftest mixer-cell-deps-guard-new-names-only (with-mixer-cell (dir "deps" "block")
  (let* ((manifest (mixer-file dir "package.json"
                                "{\"dependencies\": {\"left-pad\": \"^1.0.0\"}}"))
         (bumped "{\"dependencies\": {\"left-pad\": \"^2.0.0\"}}")
         (added "{\"dependencies\": {\"left-pad\": \"^1.0.0\", \"right-pad\": \"^1.0.0\"}}")
         (bump-form (format nil "(write-file \"~a\" ($ \"text\"))" manifest))
         (add-form bump-form)
         (bump-strings (nlk:json-object "text" bumped))
         (add-strings (nlk:json-object "text" added)))
    (is (eq :allowed (mixer-verdict bump-form bump-strings)) "a bump adds no name")
    (let ((verdict (mixer-verdict add-form add-strings)))
      (is (search "mixer:deps" verdict) "a new name is refused")
      (is (search "right-pad" verdict) "and the refusal names it"))
    (is (search "mixer:deps" (mixer-verdict "(sh \"npm install right-pad\")")) "an install command names a new package too")
    (is (eq :allowed (mixer-verdict "(sh \"npm install\")")) "a restore adds nothing"))))

;;; --- the operator's approval -------------------------------------------------------

(deftest mixer-cell-ask-refuses-when-nobody-answers (with-mixer-cell (dir "edits" "ask")
  (let* ((nodecode-mixer::*session* "s-ask")
         (path (format nil "~ac.txt" dir))
         (form (format nil "(write-file \"~a\" \"x\")" path)))
    (is (search "approval" (mixer-verdict form)) "with nobody to ask, a write waits and is refused")
    (nodecode-mixer::approve "s-ask" (format nil "edit:~a" (nodecode-mixer::normalize-path path)))
    (is (eq :allowed (mixer-verdict form)) "an approved file runs without asking again"))))

;;; --- the command -------------------------------------------------------------------

(deftest mixer-cell-command-draws-and-turns-the-board (with-mixer-cell (dir)
  (let ((board (cell-entry "nodecode-mixer" "mixer" "")))
    (is (search "edits" board) "the board names the dials")
    (is (search "scope" board) "the scope dial among them"))
  (let ((nodecode-mixer::*session* "s-cmd"))
    (is (search "edits plan" (cell-entry "nodecode-mixer" "mixer" "edits plan" "s-cmd")) "the reply says what changed")
    (is (search "read-only" (mixer-verdict (format nil "(write-file \"~ad.txt\" \"x\")" dir))) "and the session's next write is read-only")
    (is (search "auto" (cell-entry "nodecode-mixer" "mixer" "reset" "s-cmd")) "reset puts the saved default back"))
  (is (stringp (cell-entry "nodecode-mixer" "mixer" "edits nonsense" "s-cmd")) "an unknown value answers rather than signalling")))

(deftest mixer-cell-command-preset-and-scope (with-mixer-cell (dir)
  (let ((nodecode-mixer::*session* "s-preset"))
    (is (search "surgical" (cell-entry "nodecode-mixer" "mixer" "preset surgical" "s-preset")) "a preset answers the board")
    (is (search "mixer:scope" (mixer-verdict (format nil "(write-file \"~ax.lisp\" \"x\")" dir))) "a write outside the preset's scope is refused")
    (is (eq :allowed (mixer-verdict "(write-file \"src/mixer-probe.lisp\" \"x\")")) "and src/** is in it")
    (is (search "src/**" (cell-entry "nodecode-mixer" "mixer" "scope show" "s-preset")) "scope show lists what is allowed")
    (is (search "300" (cell-entry "nodecode-mixer" "mixer" "lines 300" "s-preset")) "lines takes a number"))))

(deftest mixer-cell-command-needs-a-session (with-started-mixer ()
  (is (search "cannot tell which session" (cell-entry "nodecode-mixer" "mixer" "edits plan" nil)) "a dial change with no session says so")
  (is (search "save" (cell-entry "nodecode-mixer" "mixer" "lines 20" nil)) "and points at /mixer save")))
