;;;; cell-test.lisp --- lifecycle, capture, the merge, the budget, the three
;;;; tools and /cognition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every test runs over a store in a temporary directory, so no test touches
;;;; the home's memories and nothing here dials anything: the store is two JSON
;;;; files, and what a tool answers is what those files say.

(in-package #:nodecode.test)

;;; --- the slice and its fixtures ------------------------------------------------

(define-test-slice "cognition" "COGNITION-CELL-" :start nodecode-cognition:start-cell)

(defvar *cognition-dir* nil
  "The store directory the running test uses, under a temporary directory.")

(defmacro with-cognition-home (&body body)
  "BODY with the cell's store under a fresh 0700 directory, removed on unwind."
  `(with-temp-directory (dir "cognition")
     (let ((*cognition-dir* (namestring dir)))
       ,@body)))

(defun cognition-tool (name &rest pairs)
  "Run the registered tool NAME on PAIRS, as dispatch does: through its contract."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun cognition-entry (ref content &optional (created 1700000000))
  "One entry as the queue carries it: REF, CONTENT, and complete provenance."
  (nlk:json-object
   "ref" ref "content" content "salience" 0.5
   "provenance" (nlk:json-object "source_session" "s-test" "author" "tester"
                                  "created_at" created "origin_repo" "default"
                                  "trace_id" "trace-test")))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "cognition"
  (:fixture with-cognition-home)
  (:config "dir" *cognition-dir*)
  (:help :cognition)
  (:command "cognition")
  (:hooks 'nle:submit 'nle::read-harness-sections)
  (:running (is (nle::find-tool "cognition_search") "cognition_search is registered")
            (is (nle::find-tool "cognition_status") "and cognition_status")
            (is (nle::find-tool "cognition_dream") "and cognition_dream")
            (is (search "cognition" (namestring (nodecode-cognition::store-dir))) "the test's own store"))
  (:stopped (is (null (nle::find-tool "cognition_search")) "stop unregisters the tools")
            (is (null (nle::find-tool "cognition_dream")) "all three of them"))
  (:refused ("budget_tokens" 0) ("queue-max-entries" 0))
  (:idle nodecode-cognition:cognition-error (nodecode-cognition::run-command "" nil)))

;;; --- what is kept, and what a fact carries ---------------------------------------

(deftest cognition-cell-a-lasting-statement-commits-with-provenance ()
  (with-cognition-home
    (with-cell-stop ((cognition-start "dir" *cognition-dir*))
      (let ((refs (nodecode-cognition:capture "I prefer kamal for deployments, never kubernetes" "s-42")))
        (is (= 1 (length refs)) "one lasting statement is one entry")
        (is (search "cog-s-42-" (first refs)) "with the original's deterministic ref")
        (is (equal "no committed memory holds \"kamal\"" (cognition-tool "cognition_search" "query" "kamal")) "nothing is committed before the drain")
        (multiple-value-bind (pushed conflicts failed) (nodecode-cognition:drain-due)
          (is (= 1 pushed) "the drain pushes it")
          (is (= 0 conflicts) "with no conflict")
          (is (= 0 failed) "and nothing failed"))
        (is (search "kamal" (cognition-tool "cognition_search" "query" "kamal")) "and a search finds it by its words")
        (let* ((entry (gethash (first refs) (nodecode-cognition:committed-table)))
               (provenance (nlk:json-value entry :object "provenance")))
          (is (equal "s-42" (nlk:json-value provenance :string "source_session")) "the session that said it")
          (is (equal "default" (nlk:json-value provenance :string "origin_repo")) "the label it is filed under")
          (is (plusp (length (nlk:json-value provenance :string "author"))) "an author, configured or from git")
          (is (nlk:json-value provenance :number "created_at") "and when it was kept"))))))

(deftest cognition-cell-chatter-is-not-kept ()
  (with-cognition-home
    (with-cell-stop ((cognition-start "dir" *cognition-dir*))
      (is (equal '() (nodecode-cognition:capture "move that div two pixels left" "s-1")) "no lasting signal, no fact")
      (is (search "no committed memory" (cognition-tool "cognition_search" "query" "div")) "so a search holds nothing"))))

;;; --- the merge: a changed base keeps both ------------------------------------------

(deftest cognition-cell-a-stale-push-keeps-both ()
  (with-cognition-home
    (with-cell-stop ((cognition-start "dir" *cognition-dir*))
      (let ((table (make-hash-table :test #'equal)))
        (multiple-value-bind (status ref)
            (nodecode-cognition:push-entry table (cognition-entry "cog-s-1" "we deploy with kamal") nil)
          (is (equal "applied" status) "a fresh ref applies")
          (is (equal "cog-s-1" ref) "under its own ref"))
        (multiple-value-bind (status landed)
            (nodecode-cognition:push-entry table (cognition-entry "cog-s-1" "we deploy with rsync") nil)
          (is (equal "conflict" status) "a base that no longer matches keeps both")
          (is (search "cog-s-1-conflict-" landed) "the incoming version lands under a conflict ref")
          (is (equal "we deploy with kamal" (nodecode-cognition::field (gethash "cog-s-1" table) "content")) "the committed one is untouched"))
        (multiple-value-bind (status landed)
            (nodecode-cognition:push-entry table (cognition-entry "cog-s-2" "we agreed on kamal") nil)
          (is (equal "applied" status) "a new ref still applies")
          (is (equal "cog-s-2" landed) "under its own ref"))
        (is (= 3 (hash-table-count table)) "three versions, none lost")))))

;;; --- the budget, and the prompt ------------------------------------------------------

(deftest cognition-cell-recall-is-budgeted-with-a-marker ()
  (with-cognition-home
    (with-cell-stop ((cognition-start "dir" *cognition-dir* "budget_tokens" 64))
      (loop for index from 1 to 6
            do (nodecode-cognition:capture (format nil "I prefer option ~d for the deployment pipeline, it is simpler" index)
                                            "s-budget"))
      (nodecode-cognition:drain-due)
      (let ((block (nodecode-cognition:recall-block)))
        (is (search "## Cognition Memory" block) "recall names itself")
        (is (search "omitted — prefetch budget 64 tokens" block) "and says how much the budget dropped")
        (is (<= (length (nlk:lines block)) 4) "its header, two entries and the marker, no more")))))

(deftest cognition-cell-committed-memory-rides-every-prompt ()
  (with-cognition-home
    (with-cell-stop ((cognition-start "dir" *cognition-dir*))
      (nodecode-cognition:capture "I always review the diff before pushing" "s-ride")
      (nodecode-cognition:drain-due)
      (let ((section (cell-section "s-ride" "cognition")))
        (is (search "## Cognition Memory" section) "the committed fact is in the request's sections")
        (is (search "review the diff" section) "in the operator's own words")))))

;;; --- the tools and the command --------------------------------------------------------

(deftest cognition-cell-status-and-dream-answer ()
  (with-cognition-home
    (with-cell-stop ((cognition-start "dir" *cognition-dir*))
      (is (search "0 committed" (cognition-tool "cognition_status")) "a fresh store is empty")
      (nodecode-cognition:capture "we decided to ship on Tuesday" "s-3")
      (is (search "1 pending" (cognition-tool "cognition_status")) "pending means extracted and not yet committed")
      (nodecode-cognition:drain-due)
      (is (search "1 committed" (cognition-tool "cognition_status")) "the drain commits it")
      (let ((digest (cognition-tool "cognition_dream")))
        (is (search "Dream digest" digest) "dream reads committed state")
        (is (search "Tuesday" digest) "and says what is known")))))

(deftest cognition-cell-a-conflict-surfaces-in-the-digest ()
  (with-cognition-home
    (with-cell-stop ((cognition-start "dir" *cognition-dir*))
      (let ((table (make-hash-table :test #'equal)))
        (nodecode-cognition:push-entry table (cognition-entry "cog-c-1" "we deploy with kamal") nil)
        (nodecode-cognition:push-entry table (cognition-entry "cog-c-1" "we deploy with rsync") nil)
        (nodecode-cognition::write-json-file (nodecode-cognition:committed-path) table)
        (let ((digest (cognition-tool "cognition_dream")))
          (is (search "Unresolved conflicts" digest) "a keep-both conflict is surfaced")
          (is (search "needs a human choice" digest) "as the choice it is"))))))

(deftest cognition-cell-command-answers ()
  (with-cognition-home
    (with-cell-stop ((cognition-start "dir" *cognition-dir*))
      (multiple-value-bind (text dialog) (cell-entry "nodecode-cognition" "cognition" "")
        (is (search "committed" text) "bare /cognition says what the store holds")
        (is (not (null dialog)) "and answers a panel"))
      (multiple-value-bind (text dialog) (cell-entry "nodecode-cognition" "cognition" "remember I never force-push")
        (is (search "kept cog-" text) "remember keeps one by hand")
        (is (null dialog) "with no panel to it"))
      (nodecode-cognition:drain-due)
      (is (search "never force-push" (cell-entry "nodecode-cognition" "cognition" "search force-push")) "and a search finds it"))))
