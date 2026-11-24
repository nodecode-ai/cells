;;;; cell-test.lisp --- the lifecycle, what speaks, what waits, what refuses, what outlives a restart.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "hyperactive"
  (:help :hyperactive)
  (:command "hyperactive")
  (:running (is (= 5 (nodecode-hyperactive::setting :pulse-minutes)) "a pulse every five minutes")
            (is (= 5 (nodecode-hyperactive::setting :max-items)) "five items a message")
            (is (string= "" (nodecode-hyperactive::setting :quiet-hours)) "never quiet")
            (is (let ((text (hyperactive:pulse-text))) (or (null text) (stringp text))) "a pulse answers"))
  (:refused ("pulse_minutes" 0) ("max_items" 0) ("card_minutes" 0) ("stale_days" 0))
  (:idle nodecode-hyperactive:hyperactive-error (nodecode-hyperactive:status)))

;;; --- a goal and a check-in ---------------------------------------------------------

(deftest hyperactive-cell-a-goal-and-its-check-in-speak-once ()
  (with-hyperactive ()
    (is-carrying (text (ha-tool "ha_goal_add" (ha-args "title" "Ship the deck" "horizon" "week")))
      ("goal 1 added: Ship the deck (week)" "a goal is filed"))
    (is-carrying (text (hyperactive:status))
      ("Ship the deck (week) [goal 1]") ("Scheduled:") ("(nothing)" "no follow-up yet"))
    (ha-tool "ha_followup" (ha-args "goal_id" 1 "text" "Ask about the deck" "when" "now"))
    (let ((spoken (hyperactive:pulse-text :at (1+ (ha-now)))))
      (is-carrying (text spoken) ("Ask about the deck") ("goal: Ship the deck (week)" "names the goal it serves")))
    (is (null (hyperactive:pulse-text :at (+ (ha-now) 5))) "said once, not again")
    (is-carrying (text (hyperactive:why))
      ("The last message") ("followup: Ask about the deck") ("goal: Ship the deck"))))

;;; --- cards: ask, re-ask, settle only what is low -------------------------------------

(deftest hyperactive-cell-one-check-in-is-scheduled-with-the-goal ()
  (with-hyperactive ()
    (is-carrying (text (ha-tool "ha_goal_add"
                                 (ha-args "title" "Open the PR" "horizon" "week" "follow_up_in" "2h")))
      ("goal 1 added") ("check-in #2 at" "the check-in scheduled in one call"))
    (is (equal 2 (getf (first (nodecode-hyperactive::rows :followups)) :id)) "one follow-up")))

(deftest hyperactive-cell-a-low-card-is-asked-twice-then-settled ()
  (with-hyperactive ()
    (ha-tool "ha_goal_add" (ha-args "title" "Stay fit" "horizon" "quarter"))
    (ha-tool "ha_followup" (ha-args "goal_id" 1 "text" "Gym at 7" "when" "in 2d"))
    (is-carrying (text (ha-tool "ha_card"
                                 (ha-args "question" "Move gym to 7am tomorrow?"
                                          "options" (vector "yes" "no")
                                          "recommended" "yes" "stakes" "low" "goal_id" 1
                                          "action" (ha-args "type" "reschedule" "followup_id" 2 "when" "in 3d"))))
      ("card #3, asked at") ("rec. yes, low stakes" "a card with its recommendation"))
    (let ((ask (hyperactive:pulse-text :at (1+ (ha-now)))))
      (is-carrying (text ask) ("Move gym to 7am tomorrow?  yes / no") ("I'd go with yes") ("Reply yes or no." "how to answer")))
    (let ((reask (hyperactive:pulse-text :at (+ (ha-now) 3700))))
      (is-carrying (text reask) ("Still open") ("I'll go with yes in 1h")))
    (let ((applied (hyperactive:pulse-text :at (+ (ha-now) 7400))))
      (is-carrying (text applied) ("No reply on") ("so I went with yes") ("moved \"Gym at 7\"" "the typed action ran")))
    (is (null (hyperactive:pulse-text :at (+ (ha-now) 10000))) "and it is not asked again")
    (is-carrying (text (hyperactive:why)) ("card.apply") ("goal: Stay fit (quarter)"))))

(deftest hyperactive-cell-a-money-card-waits-for-the-person ()
  (with-hyperactive ()
    (ha-tool "ha_goal_add" (ha-args "title" "Renew the licence" "horizon" "year"))
    (ha-tool "ha_card" (ha-args "question" "Buy the licence?" "recommended" "yes" "stakes" "money"
                                 "goal_id" 1 "action" (ha-args "type" "goal_status" "goal_id" 1 "status" "done")))
    (hyperactive:pulse-text :at (1+ (ha-now)))
    (hyperactive:pulse-text :at (+ (ha-now) 3700))
    (is-carrying (text (hyperactive:pulse-text :at (+ (ha-now) 7400)))
      ("still open") ("It involves money") ("won't decide it for you"))
    (is (null (hyperactive:pulse-text :at (+ (ha-now) 20000))) "noticed once, then silence")
    (is-carrying (text (hyperactive:status)) ("card 2") ("money stakes"))
    (is (equal "active" (getf (first (nodecode-hyperactive::rows :goals)) :status)) "the goal it would have closed is untouched")))

(deftest hyperactive-cell-an-answer-runs-the-cards-action ()
  (with-hyperactive ()
    (ha-tool "ha_goal_add" (ha-args "title" "Ship today" "horizon" "week"))
    (ha-tool "ha_card" (ha-args "question" "Ship today?" "recommended" "yes" "goal_id" 1
                                 "action" (ha-args "type" "goal_status" "goal_id" 1 "status" "done")))
    (hyperactive:pulse-text :at (1+ (ha-now)))
    (is-carrying (text (nodecode-hyperactive::run-slash "answer 2 yes" "s1"))
      ("card #2: yes") ("marked \"Ship today\" done"))
    (is (null (nodecode-hyperactive::open-cards)) "the card is closed")
    (is-carrying (text (refusal-text nodecode-hyperactive:hyperactive-error
                                      (ha-tool "ha_card" (ha-args "card_id" 2 "answer" "no"))))
      ("already answered (yes)" "an answered card says so"))))

;;; --- quiet hours, and a message that waits --------------------------------------------

(deftest hyperactive-cell-quiet-hours-defer-without-dropping ()
  (with-hyperactive ("quiet_hours" "00:00-23:59")
    (ha-tool "ha_goal_add" (ha-args "title" "Write the note" "horizon" "week"))
    (ha-tool "ha_followup" (ha-args "goal_id" 1 "text" "Ask about the note" "when" "now"))
    (is (null (hyperactive:pulse-text)) "quiet: nothing is said")
    (is-carrying (text (hyperactive:status)) ("Ask about the note" "still pending, nothing dropped")))
  (with-hyperactive ("quiet_hours" "22:00-07:00")
    (is (string= "22:00-07:00" (nodecode-hyperactive::setting :quiet-hours)) "the section reaches the pulse")))

;;; --- refusals -------------------------------------------------------------------------

(deftest hyperactive-cell-refusals-say-why ()
  (with-hyperactive ()
    (is-carrying (text (refusal-text nodecode-hyperactive:hyperactive-error
                                      (ha-tool "ha_goal_add" (ha-args "title" "  " "horizon" "week"))))
      ("a goal needs a title"))
    (is-carrying (text (refusal-text nodecode-hyperactive:hyperactive-error
                                      (ha-tool "ha_goal_add" (ha-args "title" "x" "horizon" "fortnight"))))
      ("horizon must be one of"))
    (is-carrying (text (refusal-text nodecode-hyperactive:hyperactive-error
                                      (ha-tool "ha_followup" (ha-args "text" "later" "when" "whenever"))))
      ("can't read time"))
    (is-carrying (text (refusal-text nodecode-hyperactive:hyperactive-error
                                      (ha-tool "ha_card" (ha-args "question" "Go?" "recommended" "maybe"))))
      ("recommended must be one of the options"))
    (is-carrying (text (refusal-text nodecode-hyperactive:hyperactive-error
                                      (ha-tool "ha_card" (ha-args "card_id" 9 "answer" "yes"))))
      ("no card #9"))
    (is (zerop (length (nodecode-hyperactive::rows :goals))) "nothing refused was recorded")))

;;; --- the file under the home -------------------------------------------------------------

(deftest hyperactive-cell-the-tree-outlives-a-restart ()
  (with-hyperactive-home
    (with-cell-stop ((hyperactive-start))
      (ha-tool "ha_goal_add" (ha-args "title" "Keep me" "horizon" "year")))
    (is (probe-file (nodecode-hyperactive::state-file)) "one file under the home")
    (with-cell-stop ((hyperactive-start))
      (is-carrying (text (hyperactive:status)) ("Keep me (year) [goal 1]" "read back from the file")))))
