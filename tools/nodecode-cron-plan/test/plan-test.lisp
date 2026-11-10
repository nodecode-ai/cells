;;;; plan-test.lisp --- what the plan finds, what it claims, and what it leaves alone.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Hermes-shaped cron folder is a file a test writes, so every test here
;;;; is offline and deterministic, on a clock the test gives. What is proved:
;;;; that three prompts of one job become one candidate with the right
;;;; keeper, that a merge's token ceiling comes from the audit and only from
;;;; inside the window, that a poller is left out of the collision check, that
;;;; a clock time two jobs share is one group, that a failure streak and an
;;;; unconfirmed delivery are listed, that a schedule it cannot parse is
;;;; listed rather than guessed, that --json answers the same plan, and that
;;;; no run writes into the profile.

(in-package #:nodecode.test)

(defparameter *now* (encode-universal-time 0 0 12 1 10 2026 0)
  "The clock these tests give the plan: noon, 1 October 2026, UTC.")

(defun prs-prompt (repository)
  "One job's prompt, written three times for three repositories."
  (format nil "List the open pull requests on the ~a repository that have had no activity ~
for a week and ask the author for a status."
          repository))

(defun crowded-home ()
  "A home whose cron folder has duplicates, a collision, a failure and a poller."
  (write-cron
   (list (job-row "a1" "stale-prs-api" (prs-prompt "api") "0 9 * * *"
                  :skills '("github") :deliver "slack" :created "2026-01-02T00:00:00Z")
         (job-row "a2" "stale-prs-web" (prs-prompt "website") "0 9 * * *"
                  :skills '("github") :deliver "slack" :created "2026-01-03T00:00:00Z")
         (job-row "a3" "stale-prs-docs" (prs-prompt "docs") "0 9 * * *"
                  :skills '("github") :deliver "slack" :created "2026-01-04T00:00:00Z")
         (job-row "b1" "rates" "Fetch the euro-dollar rate and post it to the finance channel."
                  "0 9 * * 1-5" :deliver "slack" :streak 5
                  :error "HTTP 502 from the exchange-rate API")
         (job-row "c1" "weather" "Tell me if I need an umbrella in Lyon today."
                  "0 7 * * *" :deliver "telegram" :unverified t)
         (job-row "d1" "disk-watch"
                  "Check disk usage on the build server and warn me above eighty percent."
                  "*/30 * * * *" :deliver "telegram")
         (job-row "e1" "nonsense" "Read a schedule nobody can parse."
                  "whenever I feel like it")
         (job-row "f1" "off-job" (prs-prompt "gists") "0 9 * * *" :enabled nil))
   :audit (list (usage-row "a2" 700 (- *now* 86400))
                (usage-row "a3" 1400 (- *now* 86400))
                (usage-row "a3" 900 (- *now* (* 40 86400))))))

(defun crowded-text (&rest arguments)
  "The plan CROWDED-HOME deserves, as the text /cron-plan answers."
  (let ((home (crowded-home)))
    (with-consolidator home
      (funcall #'nodecode-cron-plan::report
               (format nil "report~{ ~a~}" arguments)
               :now *now*))))

(deftest cron-plan-cell-finds-the-jobs-that-are-one-job ()
  (let ((text (crowded-text)))
    (is (search "# Cron consolidation plan" text) "the answer is a Markdown plan")
    (is (search "7 enabled job(s) of 8" text) "one job is off and not counted")
    (is (search "2 with token data in the last 14 days" text) "two jobs have usage")
    (is (search "Merge candidates (1)" text) "three prompts of one job are one candidate")
    (is (search "Keep `stale-prs-web`" text) "the one that fired and is oldest is kept")
    (is (search "fold in 2 job(s)" text) "the other two are folded in")
    (is (search "prompt 9" text) "the prompt they share is the first reason")
    (is (search "same skills (github) and destination (slack)" text) "and it says why")
    (is (search "Skills to carry over: github" text) "the skills to carry over")
    (is (search "Up to 100 prompt tokens/day" text) "the audit's tokens inside the window, a ceiling")
    (is (search "ceiling: the merged prompt is longer" text) "and that the figure is a ceiling")
    (is (search "leave **5**" text) "two jobs fewer than the seven enabled")))

(deftest cron-plan-cell-lists-collisions-failures-and-unconfirmed-deliveries ()
  (let ((text (crowded-text)))
    (is (search "Clock-time jobs that fire in the same minute (1)" text) "one clock time is shared")
    (is (search "at 09:00:" text) "at nine")
    (is (search "`rates` (0 9 * * 1-5)" text) "the weekday job is in it")
    (is (search "`stale-prs-docs` (0 9 * * *)" text) "so is each of the three")
    (is (not (search "`disk-watch`" text)) "the half-hourly poller is not")
    (is (search "`rates`: 5 failures in a row; last error: HTTP 502 from the exchange-rate API"
                text) "the failure streak, with its error")
    (is (search "Deliveries never confirmed (1)" text) "one unconfirmed delivery")
    (is (search "`weather` -> telegram" text) "named with its destination")
    (is (search "Schedules not understood" text) "the schedule it could not read is listed")
    (is (search "`nonsense`: whenever I feel like it" text) "as it was written")))

(deftest cron-plan-cell-answers-json-and-honours-the-window ()
  (let ((home (crowded-home)))
    (with-consolidator home
      (let* ((text (nodecode-cron-plan::report "report --json" :now *now*))
             (data (nlk:decode-json text))
             (jobs (nlk:json-value data :object "jobs"))
             (candidates (nlk:json-value data :array "merge_candidates")))
        (is (hash-table-p data) "the answer is one JSON object")
        (is (= 5 (nlk:json-value data :integer "after_merge")) "the same count as the text")
        (is (= 8 (nlk:json-value jobs :integer "total")) "every job is counted")
        (is (= 1 (length candidates)) "one candidate")
        (is (= 100 (nlk:json-value (nlk:json-value (aref candidates 0) :object "saving")
                                   :integer "prompt_tokens_per_day_ceiling")) "the ceiling is in the JSON, and the forty-day-old line is out of it"))
      (is (= 1400 (nlk:json-value
                   (nlk:json-value (aref (nlk:json-value
                                          (nlk:decode-json
                                           (nodecode-cron-plan::report "report --json --days 1"
                                                                       :now *now*))
                                          :array "merge_candidates")
                                         0)
                                  :object "saving")
                   :integer "prompt_tokens_per_day_ceiling")) "a one-day window counts that day's audit and no other"))))

(deftest cron-plan-cell-claims-no-saving-without-usage-data ()
  (let ((home (write-cron (list (job-row "g1" "digest-a" (prs-prompt "api") "0 8 * * mon"
                                         :skills '("gmail") :deliver "telegram")
                               (job-row "g2" "digest-b" (prs-prompt "docs") "0 8 * * wed"
                                        :skills '("gmail") :deliver "telegram")))))
    (with-consolidator home
      (let ((text (nodecode-cron-plan::report "report" :now *now*)))
        (is (search "Merge candidates (1)" text) "the two digests are one job")
        (is (search "No token data for these jobs, so no saving is claimed." text) "and it claims nothing")
        (is (search "0 with token data" text) "no job has usage")))))

(deftest cron-plan-cell-never-writes-into-the-profile ()
  (let* ((home (crowded-home))
         (cron (format nil "~a/cron" home))
         (snapshot (lambda ()
                     (loop for path in (sort (uiop:directory-files (format nil "~a/" cron))
                                             #'string< :key #'file-namestring)
                           collect (list (file-namestring path) (file-write-date path)
                                         (with-open-file (in path) (file-length in))))))
         (before (funcall snapshot)))
    (with-consolidator home
      (nodecode-cron-plan::report "report" :now *now*)
      (nodecode-cron-plan::report "report --json" :now *now*))
    (is (equal before (funcall snapshot)) "both files are the bytes they were")
    (is (= 2 (length before)) "and the folder holds nothing more")))
