;;;; cell-test.lisp --- the lifecycle, the five sections, the tool, /inbox.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the cell starts and stops cleanly and a malformed
;;;; section refuses; the view is the five sections the original plugin
;;;; printed, read off stubbed sources with nothing touched; a source that
;;;; could not be read is named in the footer rather than failing the read;
;;;; the tool and /inbox answer the same view, narrowed on request; and the
;;;; two readers that reach a real store parse the shapes the cron cell and
;;;; the session directory write.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "inbox"
  (:config "limit" 5)
  (:command "inbox")
  (:help :inbox)
  (:running (is (= 5 (nodecode-inbox::setting :limit)) "the section's row limit is live")
            (is (nle::find-tool "inbox") "the inbox tool is registered"))
  (:stopped (is (null (nle::find-tool "inbox")) "stop unregisters the tool"))
  (:refused ("limit" 0)))

;;; --- the view ------------------------------------------------------------------

(deftest inbox-cell-five-sections-over-stubbed-sources ()
  (with-stubbed-sources (:notices (inbox-test-notices)
                         :sessions (inbox-test-sessions)
                         :cron (inbox-test-jobs)
                         :evals (inbox-test-evals))
    (let ((text (inbox-view)))
      ;; Needs attention: the notice, the waiting session, the failed job and
      ;; the paused one.
      (is (search "◆ Needs attention (4)" text))
      (is (search "notice nodecode-cron [error]  cron: job nightly-backup failed" text))
      (is (search "waiting on you: which venue?" text))
      (is (search "last run FAILED: Script exited with code 1" text))
      (is (search "cron 'morning-digest'  paused  answer awaiting delivery" text))
      (is (search "◆ In progress (1)" text) "only the running evaluation")
      (is (search "eval 7  [running]  (sh" text))
      (is (search "◆ Finished, result not yet delivered (1)" text))
      (is (search "◆ Scheduled (1 active job(s))" text) "the paused job schedules nothing")
      (is (search "next in " text))
      (is (search "◆ Open surfaces (2)" text))
      (is (search "session [cli]  Plan the offsite" text))
      (is (null (search "Not read:" text)) "every source answered"))))

(deftest inbox-cell-an-empty-home-reads-as-all-clear ()
  (with-stubbed-sources (:notices '() :sessions '() :cron '() :evals '())
    (let ((text (inbox-view)))
      (is (search "◆ Needs attention (0)" text) "it always prints")
      (is (search "nothing — all clear" text))
      (is (search "◆ In progress (0)" text))
      (is (search "nothing running in the background" text))
      (is (null (search "◆ Finished" text)) "a section with nothing in it is left out")
      (is (null (search "◆ Open surfaces" text))))))

(deftest inbox-cell-a-source-that-was-not-read-is-named ()
  (with-stubbed-sources (:notices '() :sessions '() :evals '()
                         :cron '()
                         :notes '(("cron" . "absent (no cron jobs)")
                                  ("sessions" . "absent (no store)")))
    (let ((text (inbox-view)))
      (is (search "Not read:" text))
      (is (search "cron jobs (the cron registry row): absent (no cron jobs)" text))
      (is (search "the session directory (store): absent (no store)" text))
      (is (null (search "notice board" text)) "a source that was read is not named"))))

;;; --- the tool and the command ----------------------------------------------------

(deftest inbox-cell-the-tool-answers-one-section-over-the-wire (with-session-store ("s1"))
  (with-stubbed-sources (:notices (inbox-test-notices) :sessions (inbox-test-sessions)
                         :cron (inbox-test-jobs) :evals (inbox-test-evals))
    (with-cell-stop ((inbox-start))
      (let ((text (princ-to-string
                   (nle::message-content
                    (execute-wire-call "call-1" "inbox" "{\"section\":\"attention\"}")))))
        (is (search "notice nodecode-cron" text) "the section asked for")
        (is (search "waiting on you: which venue?" text) "and the waiting session in it")
        (is (null (search "Open surfaces" text)) "and only that section"))
      (let ((text (princ-to-string
                   (nle::message-content (execute-wire-call "call-2" "inbox" "{}")))))
        (is (search "◆ Scheduled" text) "an empty section argument is the whole inbox")))))

(deftest inbox-cell-the-command-takes-a-section-and-refuses-a-stranger ()
  (with-stubbed-sources (:notices (inbox-test-notices) :sessions (inbox-test-sessions)
                         :cron (inbox-test-jobs) :evals (inbox-test-evals))
    (with-cell-stop ((inbox-start))
      (is (search "◆ Needs attention" (cell-entry "nodecode-inbox" "inbox" "")))
      (let ((only (cell-entry "nodecode-inbox" "inbox" "sessions")))
        (is (search "◆ Open surfaces" only))
        (is (null (search "Needs attention" only)) "the command narrows like the tool"))
      (is (search "bogus is not one of the sections"
                  (cell-entry "nodecode-inbox" "inbox" "bogus"))))))

;;; --- the two readers that reach a store -------------------------------------------

(deftest inbox-cell-reads-the-cron-registry-row-as-the-cron-cell-writes-it ()
  (let ((document (format nil
                           "{\"jobs\":[{\"id\":\"j1\",\"name\":\"nightly\",\"state\":\"active\",~\
                             \"prompt\":\"back up\",\"session\":\"cron-j1\",\"fires\":3,~\
                             \"next_at\":~d,~\
                             \"last\":{\"at\":0,\"status\":\"failed\",\"line\":\"code 1\\nstderr: no target\"}},~\
                            {\"id\":\"j2\",\"name\":\"digest\",\"state\":\"active\",~\
                             \"prompt\":\"summarise\",\"session\":\"cron-j2\",\"fires\":1,~\
                             \"report\":{\"input\":\"the line\",\"answer\":\"the digest\",\"at\":0}}]}"
                           (+ (- (get-universal-time) 2208988800) 600))))
    (with-stubbed-fdefinitions ((nlk:store-open-p () t)
                                (nlk:session-state-get (session key)
                                  (declare (ignore session key))
                                  document))
      (multiple-value-bind (jobs note) (nodecode-inbox::read-cron)
        (is (equal "read" note))
        (is (= 2 (length jobs)))
        (is (equal "nightly" (getf (first jobs) :name)))
        (is (equal "failed" (getf (first jobs) :last-status)))
        (is (equal "code 1 stderr: no target" (getf (first jobs) :last-line)) "a multi-line failure is one line")
        (is (equal "the digest" (getf (second jobs) :report)) "the owed answer"))))
  ;; No row yet: absent, not an error.
  (with-stubbed-fdefinitions ((nlk:store-open-p () t)
                              (nlk:session-state-get (session key)
                                (declare (ignore session key))
                                nil))
    (multiple-value-bind (jobs note) (nodecode-inbox::read-cron)
      (is (null jobs))
      (is (equal "absent (no cron jobs)" note)))))

(deftest inbox-cell-reads-the-session-directory-as-it-is-written ()
  (with-stubbed-fdefinitions ((nlk:store-open-p () t)
                              (nlk:list-sessions ()
                                (list (list :id "s1" :title "Plan the offsite"
                                            :source "cli" :updated-at "2026-10-21 09:12:00"
                                            :waits "which venue?")
                                      (list :id "s2" :title "" :source "tui"
                                            :updated-at "2026-10-21 08:00:00"
                                            :waits nil))))
    (multiple-value-bind (sessions note) (nodecode-inbox::read-sessions)
      (is (equal "read" note))
      (is (= 2 (length sessions)))
      (is (equal "which venue?" (getf (first sessions) :waits)))
      (is (null (getf (second sessions) :waits))))))

(deftest inbox-cell-the-row-limit-bounds-every-section ()
  (with-stubbed-sources (:notices '() :evals '() :cron '()
                         :sessions (loop for i from 1 to 6
                                         collect (list :kind "session" :id (format nil "s~d" i)
                                                       :title (format nil "session ~d" i)
                                                       :source "cli" :updated-at (get-universal-time)
                                                       :waits nil)))
    (is (search "◆ Open surfaces (2)" (nodecode-inbox:inbox-text :limit 2)) "the limit is the section's, not the source's")
    (is (search "◆ Open surfaces (6)" (nodecode-inbox:inbox-text :limit 20)))))
