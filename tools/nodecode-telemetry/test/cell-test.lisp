;;;; cell-test.lisp --- lifecycle, the ledger, the gate, /stats and /budget.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the config surface (a
;;;; healthy section installs three hooks, two commands and one tool; a vetoed
;;;; one installs nothing; a cap that is not a number is the loader's loud
;;;; refusal), that one provider round becomes one ledger row carrying the
;;;; tokens and the cost the core priced, that a tool call is rowed with its
;;;; wall time and failure, that enforce refuses a call past a cap while
;;;; observe lets it through, that /stats and /budget answer every verb, and
;;;; that a ledger outlives the cell that wrote it.

(in-package #:nodecode.test)

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "telemetry"
  (:config "mode" "observe" "daily_usd" "5")
  (:hooks :frame :tool :round)
  (:command "stats")
  (:running
   (is-present (budget (nle::find-registered-command "budget"))
     "/budget is registered too"
     (is (equal "nodecode-telemetry" (nle::slash-command-owner budget))))
   (is (equal "observe" (nodecode-telemetry:mode)) "the mode the section says")
   (is-present (ledger (nodecode-telemetry:ledger-path))
     "the ledger is a file under the home"
     (is (search "telemetry/" (namestring ledger)))))
  (:stopped (is (null nodecode-telemetry::*rows*) "stop forgets what it read"))
  (:refused ("daily_usd" "a lot")
            ("mode" "sometimes")))

;;; --- one round, one row ---------------------------------------------------------

(deftest telemetry-cell-a-round-is-a-row-with-the-core-price (with-empty-ledger)
  (with-telemetry ()
    (publish-round :session "s1" :turn "t1" :cost 0.25 :in 1200 :out 300)
    (let ((rows (nodecode-telemetry:rows)))
      (is (= 1 (length rows)) "one usage fact is one row")
      (is (equal "round" (nlk:json-value (first rows) :string "kind")))
      (is (equal "claude-telemetry-1" (nlk:json-value (first rows) :string "model")))
      (is (= 1200 (nlk:json-value (first rows) :integer "in")))
      (is (< (abs (- 0.25d0 (nodecode-telemetry::row-cost (first rows)))) 1d-9) "the cost the core priced is the row's")
      (is (< (abs (- 3.2d0 (nlk:json-value (first rows) :number "seconds"))) 1d-9) "the round's stream time rode in from its request fact"))
    (let ((line (cell-entry "nodecode-telemetry" "stats" "")))
      (is (search "$0.25" line) line)
      (is (search "1 round" line) line)
      (is (search "in 1,200" line) line))
    (is (search "claude-telemetry-1" (cell-entry "nodecode-telemetry" "stats" "models")) "models groups by the model the provider named")))

(deftest telemetry-cell-an-unpriced-round-is-counted-not-summed (with-empty-ledger)
  (with-telemetry ()
    (publish-round :cost 0 :known nil)
    (let ((line (cell-entry "nodecode-telemetry" "stats" "today")))
      (is (search "1 unpriced" line) line))))

(deftest telemetry-cell-a-tool-call-is-rowed-with-its-failure (with-empty-ledger)
  (with-telemetry ()
    (is (equal "done" (run-tool-call "sh" :answer "done")) "the call answers what it would have")
    (run-tool-call "eval" :answer "ERROR: DIVISION-BY-ZERO: /")
    (let ((line (cell-entry "nodecode-telemetry" "stats" "tools")))
      (is (search "sh" line) line)
      (is (search "eval" line) line)
      (is (search "1 call" line) line))
    (let ((rows (nodecode-telemetry:rows)))
      (is (= 2 (length rows)) "two calls, two rows")
      (is (null (nlk:json-value (second rows) :boolean "ok")) "the ERROR: result is a failed call"))))

(deftest telemetry-cell-a-signalling-tool-is-still-rowed (with-empty-ledger)
  (with-telemetry ()
    (is (signals-error error (run-tool-call "eval" :answer :signal)))
    (is (= 1 (length (nodecode-telemetry:rows))) "the failure is journaled as it unwinds")))

;;; --- the gate -------------------------------------------------------------------

(deftest telemetry-cell-enforce-refuses-a-call-past-the-cap (with-empty-ledger)
  (with-telemetry ("mode" "enforce" "daily_usd" "1")
    (publish-round :cost 1.5)
    (is-present (b (nodecode-telemetry:breach))
      "the day is past its cap"
      (is (equal "daily" (getf b :scope)))
      (is (= 1.5d0 (getf b :spent))))
    (let ((answer (run-tool-call "sh")))
      (is (search "budget: every tool call is refused" answer) answer)
      (is (null *ran*) "the tool never ran"))
    (is (equal "sh" (nlk:json-value (car (last (nodecode-telemetry:rows))) :string "tool")) "the refusal is its own row, beside the round")
    (is (search "blocked" (cell-entry "nodecode-telemetry" "stats" "today")))))

(deftest telemetry-cell-observe-lets-the-call-through-and-says-so (with-empty-ledger)
  (with-telemetry ("mode" "observe" "daily_usd" "1")
    (publish-round :cost 1.5)
    (is (equal "done" (run-tool-call "sh")) "observe never blocks")
    (is *ran* "the tool ran")
    (is-present (notice (cell-notice "nodecode-telemetry"))
      "one standing notice says the cap is spent"
      (is (search "daily cap of $1.00" (second notice)) (second notice)))))

(deftest telemetry-cell-a-cap-argued-under-the-spend-does-not-block (with-empty-ledger)
  (with-telemetry ("mode" "enforce" "daily_usd" "1")
    (publish-round :cost 0.5)
    (is (null (nodecode-telemetry:breach)) "half a cap is no breach")
    (is (equal "done" (run-tool-call "sh")))))

(deftest telemetry-cell-a-sessions-own-cap-blocks-that-session-alone (with-empty-ledger)
  (with-telemetry ("mode" "enforce" "daily_usd" "100" "session_usd" "1")
    (publish-round :session "quiet" :cost 0.2)
    (publish-round :session "loud" :cost 1.4)
    (is (equal "session" (getf (nodecode-telemetry:breach "loud") :scope)))
    (is (null (nodecode-telemetry:breach "quiet")) "the quiet session is under its own")))

;;; --- the commands ---------------------------------------------------------------

(deftest telemetry-cell-budget-sets-a-cap-and-takes-a-mode (with-empty-ledger)
  (with-telemetry ()
    (let ((line (cell-entry "nodecode-telemetry" "budget" "")))
      (is (search "budget (observe)" line) line)
      (is (search "of $5.00" line) line))
    (let ((line (cell-entry "nodecode-telemetry" "budget" "set daily 2.5")))
      (is (search "$2.50" line) line)
      (is (= 2.5d0 (nodecode-telemetry::daily-cap)) "the running cap moved with it"))
    (let ((line (cell-entry "nodecode-telemetry" "budget" "mode enforce")))
      (is (search "enforce" line) line)
      (is (nodecode-telemetry:enforcing-p) "and the cell enforces from now on"))
    (is (search "is not one of set | mode" (cell-entry "nodecode-telemetry" "budget" "frobnicate")) "an unknown verb is answered, never signalled")))

(deftest telemetry-cell-stats-answers-every-window (with-empty-ledger)
  (with-telemetry ()
    (publish-round :cost 0.25)
    (dolist (window '("today" "week" "month" "sessions" "tools" "models"))
      (let ((line (cell-entry "nodecode-telemetry" "stats" window)))
        (is (and (stringp line) (plusp (length line))) (format nil "~a answers" window))))
    (let ((line (cell-entry "nodecode-telemetry" "stats" "raw")))
      (is (search "kind" line) line)
      (is (search "claude-telemetry-1" line) line))))

(deftest telemetry-cell-the-tool-answers-with-the-line-and-the-cap (with-empty-ledger)
  (with-telemetry ()
    (publish-round :cost 0.25)
    (let ((answer (nodecode-telemetry:report-spend
                   (nodecode-telemetry::decode-telemetry-report
                    (nlk:json-object "window" "today")))))
      (is (search "$0.25" answer) answer)
      (is (search "budget (observe)" answer) "the budget line rides behind it"))))

;;; --- outliving a start ------------------------------------------------------------

(deftest telemetry-cell-a-ledger-outlives-the-cell (with-empty-ledger)
  (with-telemetry ()
    (publish-round :cost 0.4))
  ;; Start again without emptying the file: what a relaunch does.
  (with-cell-stop ((telemetry-start))
    (is (search "$0.40" (cell-entry "nodecode-telemetry" "stats" "today")) "the row is read back from the file")))
