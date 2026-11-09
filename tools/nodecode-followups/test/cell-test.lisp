;;;; cell-test.lisp --- the lifecycle, the board, the gate, the reminder, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring and its arithmetic: that a start
;;;; installs the four tools, /followups, the manual topic and the round hook
;;;; and a stop takes every one back; that a follow-up written down comes back
;;;; out of the file with its words, its category and its time; that the same
;;;; key updates one item rather than adding a second; that backoff moves a
;;;; checked item forward; that the gate holds a due item for the quiet window,
;;;; the cooldown and a muted category, and lets it through otherwise; that one
;;;; reminder carries at most followups.max_items of them, with the framing
;;;; line; that the command answers text and a panel; and that each refusal
;;;; says why.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "followups"
  (:hooks :round)
  (:help :followups)
  (:command "followups")
  (:running (is (equal nodecode-followups::+primer+ (nle:help :followups)))
            (is (= 3 (nodecode-followups::setting :max-items)) "max_items defaults to three")
            (is (= 90 (nodecode-followups::setting :cooldown-minutes))
                "the cooldown defaults to ninety minutes"))
  (:refused ("max_items" 0) ("quiet_start" -1))
  (:idle followups:followups-error
         (followups:add :subject "the sore throat" :why "care" :at "+18h")
         (followups:board)))

;;; --- the board -------------------------------------------------------------------

(deftest followups-cell-a-follow-up-comes-back-out-of-the-file ()
  (with-followups ()
    (let ((text (followups:add :subject "the sore throat" :why "care" :at "+18h"
                               :quote "my throat is killing me" :hint "ask if it is better")))
      (is-carrying text ("added #1 care · the sore throat" "the answer names the item")
                   ("in 18h" "and when it comes back")))
    (is-carrying (text (followups:board)) ("#1 [active] care · the sore throat" "the board shows it")
                                     ("check" "with its check count"))
    (is (= 1 (followup-count)) "one item on the board, in the file")))

(deftest followups-cell-the-same-key-updates-one-item ()
  (with-followups ()
    (followups:add :subject "the trip" :why "utility" :at "+2d" :key "trip")
    (let ((text (followups:add :subject "the trip to Oslo" :why "risk" :at "+1d" :key "trip")))
      (is-carrying text ("updated #1 risk · the trip to Oslo" "the second write updates")))
    (is (= 1 (followup-count)) "and adds nothing")
    (is-carrying (text (followups:board)) ("risk · the trip to Oslo" "the newest words stand"))))

(deftest followups-cell-checked-moves-an-item-forward ()
  (with-followups ()
    (followup-add-due "waiting on the checkup results")
    (is (= 1 (length (getf (nodecode-followups::gate) :due))) "it is due")
    (is-carrying (text (followups:checked 1)) ("checked #1 waiting on the checkup results" "it is marked")
                                        ("back in 1d" "a day of backoff"))
    (is (= 0 (length (getf (nodecode-followups::gate) :due))) "and no longer due")
    (is-carrying (text (followups:checked 1)) ("back in 2d" "the second check doubles it"))))

(deftest followups-cell-retire-and-outcome ()
  (with-followups ()
    (followup-add-due "the sore throat")
    (is-carrying (text (followups:retire 1 :note "it healed")) ("closed #1 the sore throat" "it retires")
                                                    ("it healed" "with the note"))
    (is (search "it healed" (princ-to-string (nodecode-followups::item-by-id
                                         (followup-board-raw) 1)))
        "the note is kept")))

;;; --- the gate --------------------------------------------------------------------

(deftest followups-cell-the-quiet-window-holds-a-due-item ()
  (with-followups ("quiet_start" 0 "quiet_end" 24)
    (followup-add-due "the sore throat")
    (let ((verdict (nodecode-followups::gate)))
      (is (getf verdict :night) "every hour is inside a window of 0 to 24")
      (is (equal "night" (getf verdict :hold)) "and that is the hold the operator reads")
      (is (null (followups:reminder)) "so no reminder goes out"))))

(deftest followups-cell-an-open-window-lets-the-reminder-out ()
  (with-followups ("quiet_start" 0 "quiet_end" 0)
    (followup-add-due "the sore throat")
    (let ((text (followups:reminder)))
      (is-present (text (followups:reminder))
        (is-carrying text ("[followups: 1 due" "the framing line")
                          ("#1 care · the sore throat" "and the item itself"))))))

(deftest followups-cell-the-cooldown-holds-a-second-reminder ()
  (with-followups ("quiet_start" 0 "quiet_end" 0)
    (followup-add-due "the sore throat")
    (is (followups:reminder) "the first reminder goes out")
    (let ((board (followup-board-raw)))
      (setf (nodecode-followups::board-get board "last_notice") (nlk:unix-now))
      (nodecode-followups::write-board board))
    (let ((verdict (nodecode-followups::gate)))
      (is (getf verdict :cooling) "a reminder just went out")
      (is (null (followups:reminder)) "so the next one waits for the cooldown"))
    (with-followups ("quiet_start" 0 "quiet_end" 0 "cooldown_minutes" 0)
      (is (followups:reminder) "and a cooldown of zero lets it out at once"))))

(deftest followups-cell-a-muted-category-is-held ()
  (with-followups ("quiet_start" 0 "quiet_end" 0)
    (followup-add-due "the sore throat")
    (let ((board (followup-board-raw)))
      (setf (getf (first (getf board :items)) :outcome) "annoyed"
            (getf (first (getf board :items)) :sent-at) (- (nlk:unix-now) 600))
      (nodecode-followups::write-board board))
    (is (equal "muted" (getf (nodecode-followups::gate) :hold))
        "one annoyed mutes its category")
    (is (null (followups:reminder)) "so nothing goes out")))

(deftest followups-cell-one-reminder-carries-at-most-max-items ()
  (with-followups ("quiet_start" 0 "quiet_end" 0 "max_items" 1)
    (followup-add-due "the sore throat" :key "throat")
    (followup-add-due "the trip" :key "trip")
    (let ((text (followups:reminder)))
      (is-carrying text ("[followups: 2 due" "both are due")
                        ("#1 care · the sore throat" "the first one is carried")
                        ("1 more due" "the second is counted")
                        (:absent "#2 care · the trip" "and not carried")))))

;;; --- the tools and the command ---------------------------------------------------

(deftest followups-cell-the-tool-writes-what-the-contract-says ()
  (with-followups ()
    (let ((entry (nodecode-followups::decode-followup-entry
                  (nlk:make-json-object "subject" "the flight on Friday"
                                         "why" "utility" "at" "+1d"
                                         "quote" "I fly out Friday"))))
      (is-carrying (text (nodecode-followups::tool-add entry))
                    ("added #1 utility · the flight on Friday" "the tool writes it down")))
    (is-carrying (text (nodecode-followups::tool-list
                         (nodecode-followups::decode-followup-query
                          (nlk:make-json-object "which" "all"))))
                  ("#1 [active] utility · the flight on Friday" "and the tool reads it back"))))

(deftest followups-cell-the-command-answers-text-and-a-panel ()
  (with-followups ()
    (followup-add-due "the sore throat")
    (multiple-value-bind (text dialog) (nodecode-followups::run-slash "" nil)
      (is-carrying text ("follow-ups: 1 active, 1 due" "the command summarises")
                        ("#1 care · the sore throat" "with the item"))
      (is dialog "and answers a panel"))
    (is (search "closed #1" (nodecode-followups::run-slash "close 1" nil))
        "close ID retires one from the command")))

(deftest followups-cell-a-bad-write-says-why ()
  (with-followups ()
    (is (search "not a category"
                (refusal-text followups:followups-error
                              (followups:add :subject "x" :why "mood" :at "+1h"))))
    (is (search "is not a delay"
                (refusal-text followups:followups-error
                              (followups:add :subject "x" :why "care" :at "+soon"))))
    (is (search "needs a subject"
                (refusal-text followups:followups-error
                              (followups:add :subject "  " :why "care" :at "+1h"))))
    (is (= 0 (followup-count)) "and nothing was written")))

(deftest followups-cell-an-iso-time-is-read ()
  (with-followups ()
    (followups:add :subject "the interview" :why "care" :at "2026-11-10T09:00")
    (is-carrying (text (followups:board)) ("at 2026-11-10T09:00" "the stamp is kept as written"))))
