;;;; cell-test.lisp --- the roster, the six tools over stubbed runs, and the
;;;; command: what reaches the model, what a run is handed, and every refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here starts a session or opens a store. A run goes through
;;;; NODECODE-TEAMMATES::*LAUNCH*, *INSPECT*, *SEND* and *HALT*, which a test
;;;; binds to stubs recording what the cell resolved and handed on -- exactly
;;;; what a real run and its session would be given.

(in-package #:nodecode.test)

;;; --- the section, as a config holds it ----------------------------------------------

(defun teammate-object (description &key instructions provider model effort)
  "One roster entry's object."
  (apply #'nlk:make-json-object
         (append (list "description" description)
                 (when instructions (list "instructions" instructions))
                 (when provider (list "provider" provider))
                 (when model (list "model" model))
                 (when effort (list "reasoning_effort" effort)))))

(defun teammate-two-roster ()
  "The two teammates every test that wants a roster starts on: one pinned to
a provider and a model, one that follows the assigning session."
  (nlk:make-json-object
   "reviewer" (teammate-object "Slow and careful: reads a diff before anyone merges it."
                                :instructions "You review changes and name the file and the line."
                                :provider "anthropic" :model "claude-opus-5" :effort "high")
   "scout" (teammate-object "Cheap and quick: lookups, summaries, finding a caller.")))

(define-test-slice "teammates" "TEAMMATES-CELL-" :start nodecode-teammates:start-cell
  :defaults (("teammates" (teammate-two-roster))))

(defun teammates-tool (name)
  "The registered tool's function, or NIL."
  (third (nle::find-tool name)))

(defun teammate-answer (tool-name &rest arguments)
  "TOOL-NAME's answer to ARGUMENTS, so a test reads what the model would."
  (funcall (teammates-tool tool-name) (apply #'nlk:make-json-object arguments)))

(defun settled-run (answer &key (run "mate-reviewer-3KQ7") (state :done) (turns 2))
  (list :run run :state state :turns turns :answer answer))

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "teammates"
  (:config "teammates" (teammate-two-roster))
  (:help :teammates)
  (:command "teammates")
  (:running
   (is (equal '("reviewer" "scout") (nodecode-teammates::teammate-names)) "the roster, by name")
   (is (= 6 (count-if #'functionp
                       (mapcar #'teammates-tool
                               '("teammates_roster" "teammate_assign" "teammate_check"
                                 "teammate_wait" "teammate_message" "teammate_stop"))))
       "all six tools are registered")
   (is (= 900 (nodecode-teammates::setting :seconds)))
   (let ((answer (teammate-answer "teammates_roster")))
     (is (search "reviewer  Slow and careful" answer) "the roster names each teammate and its line")
     (is (search "claude-opus-5" answer) "the operator sees what a teammate runs on")
     (is (search "(none yet)" answer) "and no run yet")))
  (:refused ("seconds" 5))
  (:idle nodecode-teammates::teammates-error (nodecode-teammates::teammate-names)))

;;; --- the roster ---------------------------------------------------------------------

(deftest teammates-cell-only-names-and-lines-reach-the-model ()
  (with-cell-stop ((teammates-start "teammates" (teammate-two-roster)))
    (let* ((parameters (getf (second (nle::find-tool "teammate_assign")) :parameters))
           (member (gethash "teammate" (gethash "properties" parameters)))
           (description (gethash "description" member)))
      (is (equal '("reviewer" "scout") (coerce (gethash "enum" member) 'list)) "the enum is the roster, by name")
      (is (search "reviewer: Slow and careful" description) "each name with its line")
      (is (search "scout: Cheap and quick" description))
      (is (not (search "anthropic" description)) "no provider reaches the model")
      (is (not (search "claude-opus-5" description)) "no model id reaches the model")
      (is (equal '("teammate" "goal") (coerce (gethash "required" parameters) 'list))
          "a name and a goal are all the schema requires"))))

(deftest teammates-cell-a-malformed-entry-is-skipped-and-said-so ()
  (with-cell-stop ((teammates-start
                    "teammates"
                    (nlk:make-json-object
                     "scout" (teammate-object "Cheap and quick.")
                     "ghost" (teammate-object "")                      ; no description
                     "pinned" (teammate-object "Half pinned." :provider "anthropic")
                     "Bad Name" (teammate-object "A space in a name."))))
    (is (equal '("scout") (nodecode-teammates::teammate-names)) "only the usable entry stands")
    (let ((problems (format nil "~{~a~^; ~}" (nodecode-teammates::setting :problems))))
      (is (search "ghost needs a description" problems))
      (is (search "pinned names a provider without a model" problems))
      (is (search "Bad Name is not a teammate name" problems)))))

;;; --- assign -------------------------------------------------------------------------

(deftest teammates-cell-assign-hands-the-goal-to-the-named-teammate ()
  (with-cell-stop ((teammates-start "teammates" (teammate-two-roster)))
    (let* ((seen '())
           (nodecode-teammates::*launch*
             (lambda (mate goal context continue)
               (push (list (nodecode-teammates::teammate-name mate) goal context continue
                           (nodecode-teammates::teammate-provider mate)
                           (nodecode-teammates::teammate-model mate))
                     seen)
               (values "mate-reviewer-3KQ7" :running)))
           (answer (teammate-answer "teammate_assign"
                                     "teammate" "reviewer" "goal" "  review the patch  "
                                     "context" "it is in src/http.lisp")))
      (is (equal '(("reviewer" "review the patch" "it is in src/http.lisp" nil
                    "anthropic" "claude-opus-5"))
                 seen)
          "the roster entry resolved to its provider and model; the goal went whole, as one line")
      (is (search "mate-reviewer-3KQ7" answer) "the answer names the run to check")
      (is (search "teammate_check" answer) "and says what to call next"))))

(deftest teammates-cell-a-follow-up-goes-to-the-run-it-names ()
  (with-cell-stop ((teammates-start "teammates" (teammate-two-roster)))
    (let* ((seen '())
           (nodecode-teammates::*launch*
             (lambda (mate goal context continue)
               (declare (ignore context))
               (push (list (nodecode-teammates::teammate-name mate) goal continue) seen)
               (values continue :running))))
      (funcall (teammates-tool "teammate_assign")
               (nlk:make-json-object "teammate" "scout" "goal" "and the tests?"
                                      "continue" "mate-scout-9ZQ2"))
      (is (equal '(("scout" "and the tests?" "mate-scout-9ZQ2")) seen)
          "the same session, which is what carries the earlier work"))))

(deftest teammates-cell-an-unknown-teammate-or-run-is-refused ()
  (with-cell-stop ((teammates-start "teammates" (teammate-two-roster)))
    (is (search "ghost is not a teammate; the roster is reviewer, scout"
                (refusal-text nodecode-teammates::teammates-error
                  (teammate-answer "teammate_assign" "teammate" "ghost" "goal" "look"))))
    (is (search "reviewer is not a teammate run id"
                (refusal-text nodecode-teammates::teammates-error
                  (teammate-answer "teammate_check" "run" "reviewer")))
          "a bare name is not a run id")))

(deftest teammates-cell-an-empty-roster-refuses-and-says-where-to-write-it ()
  (with-cell-stop ((teammates-start "teammates" (nlk:make-json-object)))
    (is (search "the roster is empty"
                (refusal-text nodecode-teammates::teammates-error
                  (teammate-answer "teammate_assign" "teammate" "scout" "goal" "look"))))
    (is (search "(none yet: write them in the teammates section"
                (teammate-answer "teammates_roster")))))

;;; --- check, wait, message, stop -----------------------------------------------------

(deftest teammates-cell-check-reads-a-run-and-its-answer ()
  (with-cell-stop ((teammates-start "teammates" (teammate-two-roster)))
    (let ((nodecode-teammates::*inspect*
            (lambda (run) (settled-run "three callers, src/http.lisp:32 is the one" :run run))))
      (let ((answer (teammate-answer "teammate_check" "run" "mate-reviewer-3KQ7")))
        (is (search "mate-reviewer-3KQ7  done" answer))
        (is (search "three callers, src/http.lisp:32" answer) "the answer is the run's last words")))
    (setf nodecode-teammates::*inspect*
          (lambda (run) (settled-run nil :run run :state :running)))
    (is (search "running" (teammate-answer "teammate_check" "run" "mate-reviewer-3KQ7")))
    (setf nodecode-teammates::*inspect*
          (lambda (run) (list :run run :state :unknown :turns 0 :answer nil)))
    (is (search "not a run this image knows"
                (teammate-answer "teammate_check" "run" "mate-ghost-0000")))))

(deftest teammates-cell-wait-returns-when-its-runs-are-at-rest ()
  (with-cell-stop ((teammates-start "teammates" (teammate-two-roster)))
    (let* ((states (list :running :done))
           (nodecode-teammates::*inspect*
             (lambda (run)
               (if (string= run "mate-scout-1")
                   (list :run run :state (if states (pop states) :done)
                         :turns 1 :answer "found three callers")
                   (list :run run :state :done :turns 1 :answer "it reads clean")))))
      (let ((answer (teammate-answer "teammate_wait"
                                      "runs" (vector "mate-scout-1" "mate-reviewer-2")
                                      "mode" "all" "seconds" 2)))
        (is (search "2 of 2 settled" answer))
        (is (search "mate-scout-1  done" answer) "a run at rest is named")
        (is (search "found three callers" answer) "and shows its answer")
        (is (search "it reads clean" answer) "each run of the wait, not only the first")))))

(deftest teammates-cell-wait-times-out-and-says-how-many-settled ()
  (with-cell-stop ((teammates-start "teammates" (teammate-two-roster)))
    (let ((nodecode-teammates::*inspect*
            (lambda (run) (list :run run :state :running :turns 1 :answer nil))))
      (let ((answer (teammate-answer "teammate_wait" "runs" (vector "mate-scout-1") "seconds" 1)))
        (is (search "0 of 1 settled so far" answer) "nothing settled, and it says so")))))

(deftest teammates-cell-message-steers-and-stop-cancels ()
  (with-cell-stop ((teammates-start "teammates" (teammate-two-roster)))
    (let ((sent '()) (halted '()))
      (let ((nodecode-teammates::*send*
              (lambda (run text) (push (list run text) sent)
                (format nil "~a: steers the turn it runs" run)))
            (nodecode-teammates::*halt*
              (lambda (run reason) (push (list run reason) halted)
                (format nil "~a: its turn is being cancelled" run))))
        (is (search "steers the turn it runs"
                    (teammate-answer "teammate_message" "run" "mate-scout-1"
                                      "text" "skip the tests, just the callers")))
        (is (equal '(("mate-scout-1" "skip the tests, just the callers")) sent))
        (is (search "being cancelled"
                    (teammate-answer "teammate_stop" "run" "mate-scout-1" "reason" "enough")))
        (is (equal '(("mate-scout-1" "enough")) halted))))))

(deftest teammates-cell-message-needs-words-and-a-run-needs-its-prefix ()
  (with-cell-stop ((teammates-start "teammates" (teammate-two-roster)))
    (is (search "needs some words"
                (refusal-text nodecode-teammates::teammates-error
                  (teammate-answer "teammate_message" "run" "mate-scout-1" "text" "  "))))
    (is (search "is not a teammate run id"
                (refusal-text nodecode-teammates::teammates-error
                  (teammate-answer "teammate_stop" "run" "some-other-session"))))))

;;; --- the command --------------------------------------------------------------------

(deftest teammates-cell-the-command-shows-the-roster-and-one-teammate ()
  (with-cell-stop ((teammates-start "teammates" (teammate-two-roster)))
    (let ((shown (nle:slash "/teammates")))
      (is (search "Teammates (2)" shown))
      (is (search "reviewer  Slow and careful" shown) "a name and its line")
      (is (search "(runs on anthropic · claude-opus-5 · reasoning high)" shown)
          "what it runs on, for the operator"))
    (let ((one (nle:slash "/teammates reviewer")))
      (is (search "You review changes and name the file and the line" one) "its instructions")
      (is (search "No runs of reviewer from this session yet" one)))
    (is (search "ghost is not a teammate"
                (nle:slash "/teammates ghost")) "the command renders its own refusal")))
