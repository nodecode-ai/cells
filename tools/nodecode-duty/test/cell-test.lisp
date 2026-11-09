;;;; cell-test.lisp --- the lifecycle, the five sections, the tool, /duty.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here: the cell starts and stops cleanly and a section it
;;;; cannot honour refuses; the board is the five sections the original
;;;; plugin's summary had, read off stubbed sources with nothing touched, with
;;;; the stall flag on a turn that has gone quiet; a source that could not be
;;;; read is named in the footer rather than failing the read; the tool and
;;;; /duty answer the same board, narrowed on request, and /duty counts prints
;;;; the one line; and the three readers that reach a real store parse the
;;;; shapes the session directory, the cron cell and the goal cell write.

(in-package #:nodecode.test)

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "duty"
  (:config "limit" 5)
  (:command "duty")
  (:help :duty)
  (:running (is (= 5 (nodecode-duty::setting :limit)) "the section's row limit is live")
            (is (nle::find-tool "duty") "the duty tool is registered"))
  (:stopped (is (null (nle::find-tool "duty")) "stop unregisters the tool"))
  (:refused ("limit" 0)))

;;; --- the board ------------------------------------------------------------------

(deftest duty-cell-the-board-over-stubbed-sources ()
  (with-duty-sources (:sessions (duty-test-sessions) :active (duty-test-active)
                      :cron (duty-test-jobs) :goals (duty-test-goals))
    (let ((text (duty-view)))
      ;; the count line over the whole board
      (is (search "duty: 1 waiting on you, 1 running, 1 unanswered, 2 cron in trouble, 1 goal" text))
      ;; waiting on you: the question the harness recorded, and how long
      (is (search " Waiting on you (1)" text))
      (is (search "Plan the offsite" text))
      (is (search "waiting 5m" text))
      (is (search "which venue?" text))
      ;; running: the turn running now, flagged after duty.stall_minutes
      (is (search " Running (1)" text))
      (is (search "Refactor billing" text))
      (is (search "(stalled)" text) "two hours quiet is past ten minutes")
      ;; started and left no answer
      (is (search " Started, no answer (1)" text))
      (is (search "Nightly notes" text))
      (is (null (search "Nothing pending" text)) "a finished turn that asked nothing is not on the board")
      ;; cron trouble: the failure and the paused job with an answer owed
      (is (search " Cron trouble (2)" text))
      (is (search "last run FAILED: code 1 stderr: no target" text))
      (is (search "answer awaiting delivery" text))
      (is (null (search "weekly-review" text)) "a healthy job is not trouble")
      ;; standing goals
      (is (search " Standing goals (1)" text))
      (is (search "make test" text))
      (is (null (search "Not read:" text)) "every source answered"))))

(deftest duty-cell-an-empty-home-reads-as-all-clear ()
  (with-duty-sources (:sessions '() :active '() :cron '() :goals '())
    (let ((text (duty-view)))
      (is (search "duty: 0 waiting on you, 0 running, 0 unanswered, 0 cron in trouble, 0 goals" text))
      (is (search " Waiting on you (0)" text) "it always prints")
      (is (search "nothing waits on you" text))
      (is (search " Running (0)" text))
      (is (search "nothing is running" text))
      (is (null (search " Started" text)) "a section with nothing in it is left out")
      (is (null (search " Cron trouble" text)))
      (is (null (search " Standing goals" text))))))

(deftest duty-cell-a-source-that-was-not-read-is-named ()
  (with-duty-sources (:sessions '() :active '() :cron '() :goals '()
                      :notes '(("cron" . "absent (no cron jobs)")
                               ("sessions" . "absent (no store)")))
    (let ((text (duty-view)))
      (is (search "Not read:" text))
      (is (search "cron jobs (the cron registry row): absent (no cron jobs)" text))
      (is (search "the session directory (store): absent (no store)" text))
      (is (null (search "standing goals (the goal facts)" text)) "a source that was read is not named"))))

;;; --- the tool and the command ----------------------------------------------------

(deftest duty-cell-the-tool-answers-one-section-over-the-wire (with-session-store ("s-1"))
  (with-duty-sources (:sessions (duty-test-sessions) :active (duty-test-active)
                      :cron (duty-test-jobs) :goals (duty-test-goals))
    (with-cell-stop ((duty-start))
      (let ((text (princ-to-string
                   (nle::message-content
                    (execute-wire-call "call-1" "duty" "{\"section\":\"waiting\"}")))))
        (is (search "Waiting on you" text) "the section asked for")
        (is (search "which venue?" text) "and the question in it")
        (is (null (search "Cron trouble" text)) "and only that section"))
      (let ((text (princ-to-string
                   (nle::message-content (execute-wire-call "call-2" "duty" "{}")))))
        (is (search " Cron trouble" text) "an empty section argument is the whole board")))))

(deftest duty-cell-the-command-narrows-counts-and-refuses-a-stranger ()
  (with-duty-sources (:sessions (duty-test-sessions) :active (duty-test-active)
                      :cron (duty-test-jobs) :goals (duty-test-goals))
    (with-cell-stop ((duty-start))
      (is (search " Waiting on you" (cell-entry "nodecode-duty" "duty" "")))
      (let ((only (cell-entry "nodecode-duty" "duty" "cron")))
        (is (search " Cron trouble" only))
        (is (null (search "Waiting on you" only)) "the command narrows like the tool"))
      (let ((counts (cell-entry "nodecode-duty" "duty" "counts")))
        (is (search "duty: 1 waiting on you" counts))
        (is (null (search "Waiting on you" counts)) "counts is the one line and nothing else"))
      (is (search "bogus is not one of the sections" (cell-entry "nodecode-duty" "duty" "bogus"))))))

;;; --- the readers that reach a store ----------------------------------------------

(deftest duty-cell-reads-the-cron-registry-row-as-the-cron-cell-writes-it ()
  (let ((document (format nil
                           "{\"jobs\":[{\"id\":\"j1\",\"name\":\"nightly\",\"state\":\"active\",~\
                             \"prompt\":\"back up\",\"session\":\"cron-j1\",\"fires\":3,~\
                             \"last\":{\"at\":0,\"status\":\"failed\",\"line\":\"code 1\\nstderr: no target\"}},~\
                            {\"id\":\"j2\",\"name\":\"digest\",\"state\":\"active\",~\
                             \"prompt\":\"summarise\",\"session\":\"cron-j2\",\"fires\":1,~\
                             \"report\":{\"input\":\"the line\",\"answer\":\"the digest\",\"at\":0}}]}")))
    (with-stubbed-fdefinitions ((nlk:store-open-p () t)
                                (nlk:session-state-get (session key)
                                  (declare (ignore session key))
                                  document))
      (multiple-value-bind (jobs note) (nodecode-duty::read-cron)
        (is (equal "read" note))
        (is (= 2 (length jobs)))
        (is (equal "nightly" (getf (first jobs) :name)))
        (is (equal "failed" (getf (first jobs) :last-status)))
        (is (equal "code 1 stderr: no target" (getf (first jobs) :last-line)) "one line")
        (is (equal "the digest" (getf (second jobs) :report)) "the owed answer"))))
  ;; No row yet: absent, not an error.
  (with-stubbed-fdefinitions ((nlk:store-open-p () t)
                              (nlk:session-state-get (session key)
                                (declare (ignore session key))
                                nil))
    (multiple-value-bind (jobs note) (nodecode-duty::read-cron)
      (is (null jobs))
      (is (equal "absent (no cron jobs)" note)))))

(deftest duty-cell-reads-the-session-directory-as-it-is-written ()
  (with-stubbed-fdefinitions ((nlk:store-open-p () t)
                              (nlk:list-sessions ()
                                (list (list :id "s1" :title "Plan the offsite" :source "cli"
                                            :updated-at 1760000000 :turn-kind "turn.completed"
                                            :waits "which venue?")
                                      (list :id "s2" :title "" :source "tui"
                                            :updated-at 1760000000 :turn-kind "turn.started"
                                            :waits nil))))
    (multiple-value-bind (sessions note) (nodecode-duty::read-sessions)
      (is (equal "read" note))
      (is (= 2 (length sessions)))
      (is (equal "which venue?" (getf (first sessions) :waits)))
      (is (equal "turn.started" (getf (second sessions) :turn-kind)))
      (is (null (getf (second sessions) :waits))))))

(deftest duty-cell-reads-standing-goals-and-leaves-the-ended-ones-out ()
  (let ((set-row (list "s1" "{\"goal\":\"g-1\",\"condition\":\"the suite passes\",\"turns\":8}"))
        (ended-row (list "s1" "{\"goal\":\"g-2\",\"condition\":\"shipped\",\"turns\":4}")))
    (with-stubbed-fdefinitions ((nlk:store-open-p () t)
                                (nlk:events (&rest arguments)
                                  (cond ((equal (getf arguments :kind) "goal.set")
                                         (list set-row ended-row))
                                        ((equal (getf arguments :kind) "goal.ended")
                                         (if (equal (getf arguments :aggregate-id) "g-2") 1 0))
                                        (t 0))))
      (multiple-value-bind (goals note) (nodecode-duty::read-goals)
        (is (equal "read" note))
        (is (= 1 (length goals)) "the goal that ended is not standing")
        (is (equal "g-1" (getf (first goals) :id)))
        (is (equal "the suite passes" (getf (first goals) :condition)))
        (is (= 8 (getf (first goals) :turns)))))))

(deftest duty-cell-the-row-limit-bounds-every-section ()
  (with-duty-sources (:sessions (loop for i from 1 to 6
                                      collect (list :id (format nil "s~d" i)
                                                    :title (format nil "session ~d" i)
                                                    :source "cli"
                                                    :updated-at (- (+ (get-universal-time) 2208988800) 60)
                                                    :turn-kind "turn.completed"
                                                    :waits "shall I?"))
                      :active '() :cron '() :goals '())
    (let ((two (nodecode-duty:duty-text :limit 2))
          (all (nodecode-duty:duty-text :limit 20)))
      (is (search " Waiting on you (2)" two) "the limit is the section's")
      (is (search " Waiting on you (6)" all)))))
