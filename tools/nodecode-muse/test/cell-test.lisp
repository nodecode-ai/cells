;;;; cell-test.lisp --- lifecycle; the tool; /muse; the reminder that comes due.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own behaviour over a real store: that a
;;;; start installs the tool, the manual, /muse and the round, and a stop takes
;;;; every one back; that a goal is kept, read, closed with a note and
;;;; forgotten; that an article carries its sources and a stable key keeps it
;;;; from arriving twice; that a wrong action and a missing title are refused
;;;; with the words a model repairs from; that /muse answers alone and with a
;;;; line; and that a reminder that came due is one line said once.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "muse"
  (:fixture with-muse-store)
  (:hooks :round)
  (:help :muse)
  (:command "muse")
  (:running (is (nle::find-tool "muse_manage") "the tool is registered")
            (is (= 14 (nodecode-muse::setting :topic-days)) "an interest lasts two weeks")
            (is (= 4000 (nodecode-muse::setting :feed-chars)) "one article reads to 4000 characters"))
  (:refused ("topic_days" 0) ("remind_chars" 10))
  (:idle nodecode-muse:muse-error (muse:goals)))

;;; --- the tool -------------------------------------------------------------------

(deftest muse-cell-a-goal-is-kept-closed-and-forgotten (with-muse-store)
  (with-muse ()
    (let ((created (muse-tool "goal_create"
                               (nlk:json-object "title" "ship the rewrite"
                                                "criteria" "the suite is green"
                                                "deadline" "2026-12-01"))))
      (is (search "goal-1" created) "the goal is kept under its id")
      (is (search "the suite is green" created) "and says what done means"))
    (is (search "pick a bicycle" (muse-tool "goal_create" (nlk:json-object "title" "pick a bicycle"))))
    (is (search "goal-2" (nodecode-muse::store-text "goals")) "both goals are in the store")
    (let ((done (muse-tool "goal_update" (nlk:json-object "id" "goal-1" "status" "done"
                                                           "note" "1.0 tagged"))))
      (is (search "done" done))
      (is (search "1.0 tagged" done) "the note is kept with the goal"))
    (is (search "1 active, 1 done" (muse-tool "status")) "status counts both")
    (is (search "forgot goal-2" (muse-tool "forget" (nlk:json-object "id" "goal-2"))))
    (is (search "0 active, 1 done" (muse-tool "status")) "a forgotten goal is gone")))

(deftest muse-cell-an-article-carries-its-sources-and-keeps-one-key (with-muse-store)
  (with-muse ()
    (let ((kept (muse-tool "feed_add"
                            (nlk:json-object "key" "passport-2026" "title" "Passports, faster"
                                             "content" "The queue moved online."
                                             "sources" (vector "https://example.org/a")
                                             "why" "you renewed yours in March"))))
      (is (search "Feed feed-1" kept))
      (is (search "1 source" kept)))
    (let ((read (muse-tool "content_read" (nlk:json-object "id" "feed-1"))))
      (is (search "The queue moved online." read) "the body is read whole")
      (is (search "https://example.org/a" read) "with its sources")
      (is (search "you renewed yours in March" read) "and why it matters"))
    (is (search "already carries" (muse-tool "feed_add"
                                             (nlk:json-object "key" "passport-2026"
                                                              "title" "again" "content" "x"))) "a stable key is one article")
    (is (search "Passports, faster" (muse-tool "feed_list"))))
  (with-muse ()                                            ; the store outlives a stop
    (is (search "Passports, faster" (muse-tool "feed_list")) "the article is in the store, not in memory")))

(deftest muse-cell-a-refusal-names-what-to-send (with-muse-store)
  (with-muse ()
    (is (search "title is required" (muse-tool "goal_create" (nlk:json-object))))
    (muse:goal "pick a bicycle")
    (is (search "no goal is called" (muse-tool "goal_update" (nlk:json-object "id" "goal-9"))))
    (is (search "status is active, done or cancelled"
                (muse-tool "goal_update" (nlk:json-object "id" "goal-1" "status" "later"))))
    (is (signals-error nlk:contract-refusal
          (muse-tool "sing" (nlk:json-object))) "an action outside the declaration never reaches the body")))

(deftest muse-cell-interests-ideas-and-the-feed-brief (with-muse-store)
  (with-muse ()
    (is (search "until" (muse-tool "interest_record" (nlk:json-object "title" "bicycle touring"))))
    (is (search "topic-1" (nodecode-muse::store-text "topics")))
    (let ((topic (first (nodecode-muse::entries "topic"))))
      (is (string> (nodecode-muse::field topic "expires") (nlk:iso-now)) "the topic expires in the future, not at once"))
    (is (search "idea-1" (muse-tool "idea_add" (nlk:json-object "title" "a steel frame"
                                                               "rationale" "it takes a rack"))))
    (is (search "feed brief" (muse-tool "preferences" (nlk:json-object))))
    (is (search "kept" (muse-tool "preferences" (nlk:json-object "feed_brief" "bicycles, mostly"))))
    (is (search "bicycles, mostly" (muse-tool "context" (nlk:json-object "section" "preferences"))))))

;;; --- /muse ------------------------------------------------------------------------

(deftest muse-cell-slash-answers-alone-and-runs-its-lines (with-muse-store)
  (with-muse ()
    (muse:goal "renew the passport" :criteria "the new one is in hand")
    (let ((text (muse:run-slash "" nil)))
      (is (search "Muse, this profile's companion" text) "bare /muse is the store")
      (is (search "goals: 1" text)))
    (let ((panel (nth-value 1 (muse:run-slash "" nil))))
      (is (search "/muse done goal-1"
                  (format nil "~{~a~}" (mapcar (lambda (row) (getf (getf row :run) :enter))
                                                (getf panel :rows)))) "the panel's first line closes the goal"))
    (is (search "done" (muse:run-slash "done goal-1" nil)))
    (is (search "0 active" (muse:context "status")) "and the store says so")
    (is (signals-error nodecode-muse:muse-error (muse:run-slash "sing" nil)) "an unknown line refuses, and /muse answers it as muse: ...")))

;;; --- the due reminder -------------------------------------------------------------

(deftest muse-cell-a-due-reminder-is-one-line-said-once (with-muse-store)
  (with-muse ()
    (muse:remind "renew the passport" :due "2020-01-01")
    (muse:remind "read the Sundays" :due "2999-01-01")
    (let ((due (nodecode-muse::pending-due)))
      (is (= 1 (length due)) "only the one that came due")
      (let ((line (nodecode-muse::delivery-text due)))
        (is (search "1 reminder came due" line))
        (is (search "renew the passport" line) "the reminder's own words")
        (is (search "notification_resolve" line) "and how to close it"))
      (nodecode-muse::mark-delivered due)
      (is (null (nodecode-muse::pending-due)) "it is said once, never again"))
    (is (search "read the Sundays" (muse:context "reminders")) "the future one is still kept")
    (is (search "dismissed" (muse:resolve "reminder-2" "dismissed")))))
