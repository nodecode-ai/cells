;;;; cell-test.lisp --- the lifecycle, the section, and the command over a fixture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: that a start installs
;;;; /cron-plan and (help :cron-plan) and a stop takes both back, that a
;;;; second start leaves one of everything, that a vetoed section installs
;;;; nothing, that a bad `days' and a missing required `home' refuse as
;;;; nlk:config-refusal, and that the command answers a plan over a cron
;;;; folder this test writes.

(in-package #:nodecode.test)

(defun cron-plan-text (arguments)
  "What /cron-plan answers for ARGUMENTS in this shell."
  (cron-plan::run-slash arguments "s1"))

(define-cell-lifecycle-tests "cron-plan"
  (:config "home" (fixture-home "life") "days" 7)
  (:help :cron-plan)
  (:command "cron-plan")
  (:running (is (= 7 (nodecode-cron-plan::setting :days)) "days is read")
            (is (stringp (nodecode-cron-plan::setting :home)) "home is a path"))
  (:refused ("days" 0))
  (:idle cron-plan::cron-plan-error
         (cron-plan:plan "report")))

(deftest cron-plan-cell-with-no-home-says-what-it-could-not-read ()
  ;; `home' is the section's required setting: the setup panel asks for it, and
  ;; until it is there the plan says in words what it could not read rather
  ;; than inventing a report.
  (with-cell-stop ((cron-plan-start))
    (is (null (nodecode-cron-plan::setting :home)) "no home is set")
    (is (nle::find-registered-command "cron-plan") "the command is still there")
    (is (search "no cron/jobs.json" (cron-plan:plan "report")) "and it names the file it wanted")))

(deftest cron-plan-cell-answers-a-plan-over-a-cron-folder ()
  (let ((home (write-cron
               (list (job-row "j1" "digest-a"
                              "Summarise the unread newsletters in my inbox, keep what concerns my projects, and send a digest. A"
                              "0 8 * * mon")
                     (job-row "j2" "digest-b"
                              "Summarise the unread newsletters in my inbox, keep what concerns my projects, and send a digest. B"
                              "0 8 * * tue")))))
    (with-consolidator home
      (let ((text (cron-plan-text "report")))
                (is (search "Merge candidates (1)" text) "the two digests are one job")
        (is (search "Read-only: nothing was changed." text) "and nothing was applied")
        (is (search "`digest-a`" text) "the keeper is named"))
      (is (search "leave **1**" (cron-plan-text "")) "a bare call reports too"))))

(deftest cron-plan-cell-answers-its-manual-and-its-refusals ()
  (let ((home (fixture-home "manual")))
    (with-consolidator home
      (is (search "read-only plan" (cron-plan-text "help")) "help answers the manual")
      (is (search "no cron/jobs.json" (cron-plan-text "report")) "an empty home says what it could not read")
      (is (search "--days needs" (cron-plan-text "report --days x")) "a useless window is refused in words")
      (is (search "unknown argument" (cron-plan-text "report --apply")) "an option it does not take is refused")
      (is (search "report [--json]" (cron-plan-text "report --help")) "help lists the options"))))
