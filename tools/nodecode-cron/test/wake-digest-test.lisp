;;;; wake-digest-test.lisp --- the inward wake sources: turns and goals that ended, once.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The pure LINES halves driven with rows of the shapes the readers return,
;;;; then each reader against a temp store holding a real ended turn and a
;;;; real ended goal: a fact shows only inside (SINCE, now], and never for a
;;;; cron job's own session or a fork.

(in-package #:nodecode.test)

(deftest cron-cell-wake-digest-registered ()
  (dolist (source '(nodecode-cron::wake-source-turns nodecode-cron::wake-source-goals))
    (is (member source nodecode-cron::*wake-sources*) (format nil "~a is on *wake-sources*" source))))

(deftest cron-cell-wake-digest-title-drops-the-whole-bracket ()
  ;; A channel line's bracket closes on the time it was sent (#420): the title
  ;; drops it whole, and a title cut inside it drops what is there.
  (is (equal "alice fix the build"
             (nodecode-cron::clean-title "alice [m1 u2 r3 Tue 2026-11-10 15:39]: fix the build")))
  (is (equal "alice fix the build" (nodecode-cron::clean-title "alice [m1 u2]: fix the build")))
  (is (equal "alice" (nodecode-cron::clean-title "alice [m1 u2 Tue 2026-11"))))

(deftest cron-cell-wake-digest-turn-lines ()
  (let* ((now (nlk:iso-universal "2026-11-03T12:00:00Z"))
         (lines (nodecode-cron::turn-lines
                 '(("s-work" "turn.completed" 3 "2026-11-03T11:30:00.000Z")
                   ("s-work" "turn.failed" 1 "2026-11-03T11:50:00.000Z")
                   ("s-docs" "turn.completed" 1 "2026-11-03T09:00:00.000Z")
                   ("s-docs" "turn.cancelled" 2 "2026-11-03T08:00:00.000Z")
                   ("cron-wake" "turn.completed" 1 "2026-11-03T11:59:00.000Z")
                   ("fork-turn-abc-1" "turn.completed" 1 "2026-11-03T11:59:00.000Z"))
                 now :name (lambda (id) (if (equal id "s-work") "perf thread" id)))))
    (is (equal '("turns in perf thread: 4 ended, 1 failed, the last 10m ago"
                 "turns in s-docs: 3 ended, 2 stopped, the last 3h ago")
               lines) "one line a session, newest first; the wake's own session and forks left out")))

(deftest cron-cell-wake-digest-clean-title ()
  (is (equal "peasantry (operator)"
             (nodecode-cron::clean-title "peasantry (operator) [m1566895989359382701 u93170806531")))
  (is (equal "fix the lint" (nodecode-cron::clean-title "fix the lint")))
  (is (equal "" (nodecode-cron::clean-title nil))))

(deftest cron-cell-wake-digest-goal-lines ()
  (let ((lines (nodecode-cron::goal-lines
                (list (list "s-work" "{\"goal\":\"g1\",\"reason\":\"met\",\"by\":\"judge\",\"detail\":\"exit 0\"}")
                      (list "s-work" "{\"goal\":\"g2\",\"reason\":\"failed\",\"by\":\"cell\",\"detail\":\"\"}")
                      (list "cron-wake" "{\"goal\":\"g3\",\"reason\":\"met\"}"))
                :name (constantly "perf thread")
                :condition (lambda (goal) (and (equal goal "g1") "the bench passes")))))
    (is (equal '("goal met by judge in perf thread: the bench passes -- exit 0"
                 "goal failed by cell in perf thread")
               lines))))

(deftest cron-cell-wake-digest-reads-a-real-turn-once ()
  (with-temp-store ()
    (nlk:create-session :id "s-work")
    (with-durable-turn (turn "s-work" :input "fix the lint" :complete t))
    (let ((now (get-universal-time)))
      (let ((lines (nodecode-cron::wake-source-turns (- now 120))))
        (is (= 1 (length lines)))
        (is (search "1 ended" (first lines)))
        (is (search "fix the lint (s-work)" (first lines)) "named by its title and its id"))
      (is (null (nodecode-cron::wake-source-turns (+ now 5))) "past the window, nothing"))))

(deftest cron-cell-wake-digest-reads-a-real-goal ()
  (with-temp-store ()
    (nlk:create-session :id "s-work")
    (nlk:record-event "s-work" "goal.set" (list :goal "g1" :condition "the bench passes")
                      :aggregate-type "goal" :aggregate-id "g1")
    (nlk:record-event "s-work" "goal.ended" (list :goal "g1" :reason "met" :by "judge" :detail "exit 0")
                      :aggregate-type "goal" :aggregate-id "g1")
    (let ((lines (nodecode-cron::wake-source-goals (- (get-universal-time) 120))))
      (is (equal '("goal met by judge in s-work: the bench passes -- exit 0") lines)))))
