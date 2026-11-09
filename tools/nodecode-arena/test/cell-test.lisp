;;;; cell-test.lisp --- lifecycle; the plan; a whole four-agent run; the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the start installs the manual, the command and the
;;;; tool, and the stop takes them back; plan writes nothing and gets the
;;;; arithmetic of the bracket right; a run deals distinct cards, and driven
;;;; phase by phase - briefs written, the sub-agents' outputs filled in,
;;;; verdicts collected, rounds advanced - it crowns exactly one champion and
;;;; reports what it beat; a run with a rejected answer ends in a blind
;;;; final check; and a verb with nothing to work on refuses in words rather
;;;; than a stack trace.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "arena"
  (:config)
  (:help :arena)
  (:command "arena")
  (:running
   (is (= 100 (arena::setting :agents)) "a hundred competitors by default")
   (is (= 10 (arena::setting :wave)) "ten sub-agent calls to a wave"))
  (:refused ("agents" 0) ("wave" 0)))

;;; --- helpers ---------------------------------------------------------------

(defun arena-test-dir (name)
  (uiop:ensure-directory-pathname
   (format nil "~aarena-cell-~a-~d-~d/"
           (uiop:temporary-directory) name (get-universal-time) (random 100000))))

(defun arena-write-verdicts (jobs)
  "One verdict per judge job, the first-listed side always the stronger."
  (dolist (job jobs)
    (let ((a (getf job :a)) (b (getf job :b)))
      (arena::write-text
       (first (getf job :outputs))
       (format nil "{\"match\": \"~a\", \"scores\": {\"~a\": {\"correctness\": 9, \"completeness\": 8, \"specificity\": 8, \"robustness\": 8, \"clarity\": 8, \"fatal\": false}, \"~a\": {\"correctness\": 5, \"completeness\": 5, \"specificity\": 5, \"robustness\": 5, \"clarity\": 5, \"fatal\": false}}, \"winner\": \"~a\", \"reason\": \"the stronger answer\", \"survived\": [\"one real attack\"], \"standing\": {\"~a\": [], \"~a\": [\"thin in places\"]}}~%"
               (getf job :match) a b a a b)))))

(defun arena-step (dir)
  "One phase of the run, done the way the orchestrator does it by hand: fill
in the outputs the phase waits for, then collect or advance as next says."
  (let* ((state (arena::load-state :dir dir))
         (phase (arena::next-action state)))
    (cond ((equal phase "spawn")
           (dolist (job (arena::phase-jobs state "spawn"))
             (arena::write-text (first (getf job :outputs))
                                (format nil "A solution from ~a.~%" (getf job :agent)))))
          ((equal phase "attack")
           (dolist (job (arena::phase-jobs state "attack"))
             (arena::write-text (first (getf job :outputs)) "ATTACK 1 [MINOR] thin\n")))
          ((equal phase "defend")
           (dolist (job (arena::phase-jobs state "defend"))
             (arena::write-text (first (getf job :outputs)) "ATTACK 1: CONCEDE.\n")
             (arena::write-text (second (getf job :outputs)) "The revised solution.\n")))
          ((equal phase "judge")
           (arena-write-verdicts (arena::phase-jobs state "judge"))
           (arena:run "collect" :dir dir))
          ((equal phase "collect") (arena:run "collect" :dir dir))
          ((equal phase "advance") (arena:run "advance" :dir dir))
          (t nil))))

(defun arena-final-result-p (dir)
  (let ((state (arena::load-state :dir dir)))
    (and (arena::champion-of state)
         (arena::jget (arena::jget state "final") "result")
         t)))

(defun arena-step-for-final (dir)
  "ARENA-STEP, plus the blind final check: its briefs, and its verdict."
  (let* ((state (arena::load-state :dir dir))
         (phase (arena::next-action state)))
    (if (equal phase "final")
        (progn
          (is (search "final.judge" (arena:run "prompts" :phase "final" :dir dir)) "the final check's brief is written")
          (dolist (job (arena::phase-jobs state "final"))
            (arena::write-text
             (first (getf job :outputs))
             "{\"scores\": {\"X\": {\"correctness\": 9, \"completeness\": 9, \"specificity\": 8, \"robustness\": 8, \"clarity\": 8, \"fatal\": false}, \"Y\": {\"correctness\": 4, \"completeness\": 4, \"specificity\": 4, \"robustness\": 4, \"clarity\": 4, \"fatal\": false}}, \"winner\": \"X\", \"reason\": \"the tournament's answer is the sharper one\", \"fixed\": [\"the definition is not a joke\"]}~%"))
          (arena:run "collect" :dir dir))
        (arena-step dir))))

;;; --- the plan --------------------------------------------------------------

(deftest arena-cell-plan-is-the-arithmetic-of-the-bracket ()
  (with-cell-stop ((arena-start))
    (let ((sixteen (arena:run "plan" :quick t))
          (hundred (arena:run "plan"))
          (four (arena:run "plan" :agents 4)))
      (is (search "16 agents, 4 rounds" sixteen) "four rounds for sixteen")
      (is (search "91" sixteen) "91 sub-agent calls: sixteen spawns and five per match")
      (is (search "alive per round: 16 -> 8 -> 4 -> 2 -> 1" sixteen))
      (is (search "100 agents, 7 rounds" hundred) "seven rounds for a hundred")
      (is (search "595" hundred) "595 calls, as the original disclosed")
      (is (search "yes" hundred) "an odd round carries a bye")
      (is (search "4 agents, 2 rounds" four))
      (is (search "19" four) "four spawns and fifteen calls in the matches"))))

;;; --- a whole run -----------------------------------------------------------

(deftest arena-cell-a-four-agent-arena-crowns-one-champion ()
  (with-cell-stop ((arena-start))
    (let ((dir (namestring (arena-test-dir "one"))))
      (unwind-protect
           (progn
             (let ((ready (arena:run "init" :task "Write a four-line poem about brackets."
                                     :agents 4 :seed 11 :dir dir)))
               (is (search "arena ready" ready) "init answers where the run is")
               (is (search "4 agents, 4 distinct cards" ready) "every competitor gets its own card")
               (is (search "2 rounds (4 -> 2 -> 1)" ready))
               (is (probe-file (arena::sub dir "task.md")) "the task is on disk")
               (is (probe-file (arena::sub dir "rubric.md")) "and the rubric, inside the run"))
             (let* ((state (arena::load-state :dir dir))
                    (cards (loop for id in (arena::agent-ids-of state)
                                 collect (format nil "~{~a~^/~}"
                                                 (loop for part in '("reasoning" "workflow" "strategy")
                                                       collect (nlk:json-value
                                                                (arena::card-part (arena::agent-of state id) part)
                                                                :string "id"))))))
               (is (= 4 (length (arena::phase-jobs state "spawn"))) "four competitors to spawn")
               (is (= 4 (length (remove-duplicates cards :test #'equal))) "four different cards, no repeat"))
             (is (search "round 0, spawn: 4 job(s)" (arena:run "prompts" :phase "spawn" :dir dir)) "the spawn briefs are written")
             (arena-step dir)
             (is (search "4 of 4 job(s) done" (arena:run "check" :phase "spawn" :dir dir)) "and counted as done once every competitor wrote its solution")
             (loop repeat 12
                   until (search "DONE" (arena:run "next" :dir dir))
                   do (arena-step dir))
             (is (search "DONE" (arena:run "next" :dir dir)) "the bracket ends")
             (let ((winner (arena:run "winner" :dir dir))
                   (status (arena:run "status" :dir dir)))
               (is (search "CHAMPION" winner) "one solution is left")
               (is (search "2 played, 4 agents in, 1 left" winner))
               (is (search "survived: one real attack" winner) "the attacks it beat are listed")
               (is (search "3 eliminated" status) "the other three are out")
               (is (search "champion" status)))
             (let* ((state (arena::load-state :dir dir))
                    (champion (arena::agent-of state (arena::champion-of state))))
               (is (probe-file (arena::text-of (arena::jget champion "solution"))) "the champion's solution is a file a person can read")))
        (ignore-errors (uiop:delete-directory-tree
                        (uiop:directory-exists-p dir) :validate t))))))

(deftest arena-cell-a-rejected-answer-is-checked-blind-at-the-end ()
  (with-cell-stop ((arena-start))
    (let ((dir (namestring (arena-test-dir "final"))))
      (unwind-protect
           (progn
             (arena:run "init" :task "Say what a bracket is, in one line."
                        :agents 2 :seed 3 :dir dir
                        :baseline "A bracket is a tree with extra steps.")
             (loop repeat 12
                   until (arena-final-result-p dir)
                   do (arena-step-for-final dir))
             (is (arena-final-result-p dir) "the final check is recorded")
             (is (probe-file (arena::sub dir "final/X.md")) "the two copies are blind files")
             (is (probe-file (arena::sub dir "final/Y.md")))
             (is (search "vs the answer you rejected" (arena:run "winner" :dir dir)) "the report says how the winner did against it"))
        (ignore-errors (uiop:delete-directory-tree
                        (uiop:directory-exists-p dir) :validate t))))))

;;; --- refusals -------------------------------------------------------------

(deftest arena-cell-a-verb-with-no-run-refuses-in-words ()
  (with-cell-stop ((arena-start))
    (flet ((refusal (thunk)
             (handler-case (funcall thunk) (condition (condition) (princ-to-string condition)))))
      (let ((dir (namestring (arena-test-dir "empty"))))
        (is (search "no arena.json" (refusal (lambda () (arena:run "status" :dir dir)))) "a folder with no arena says so")
        (is (search "the task is empty" (refusal (lambda () (arena:run "init" :agents 4 :dir dir)))) "init without a task refuses")
        (arena:run "init" :task "Anything at all, in one line." :agents 2 :seed 5 :dir dir)
        (is (search "unknown phase" (refusal (lambda () (arena:run "prompts" :phase "sideways" :dir dir)))) "a phase that is not one refuses")
        (is (search "is not one of the arena's commands" (refusal (lambda () (arena:run "dance" :dir dir)))) "a verb that is not one refuses")))))


(deftest arena-cell-the-tool-answers-through-the-wire ()
  (with-cell-stop ((arena-start))
    (let ((answer (nle::message-content
                   (execute-wire-call "arena-call-1" "arena"
                                      "{\"command\": \"plan\", \"agents\": 4}"))))
      (is (search "4 agents, 2 rounds" answer) "the tool dispatches the contract's command")
      (is (search "19" answer) "and its plan: four spawns and five calls a match"))))
