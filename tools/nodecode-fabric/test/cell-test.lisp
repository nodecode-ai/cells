;;;; cell-test.lisp --- the six tools, the ranked recall, the round's memory,
;;;; the standing section, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's behaviour against a real folder of
;;;; markdown entries it owns: that each tool answers through its declared
;;;; contract, that what one write puts on disk is what the next recall ranks
;;;; first, that a search finds an exact word the ranking would not, that
;;;; pending and the brief read handoffs written by another agent, that curate
;;;; rewrites a header field in place, that the round hook hands the model what
;;;; the turn's own words match, and that each refusal says why. Nothing here
;;;; needs a network or touches ~/fabric.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "fabric"
  (:hooks :round 'nle::read-harness-sections)
  (:help :fabric)
  (:command "fabric")
  (:running
   (dolist (name '("fabric_recall" "fabric_write" "fabric_search"
                   "fabric_pending" "fabric_brief" "fabric_curate"))
     (is-present (entry (nle::find-tool name))
       (format nil "~a is registered" name)
       (is (hash-table-p (nlk:contract-schema (second entry))) "against a declared contract"))))
  (:refused ("agent" "") ("max_results" 0))
  (:idle fabric:fabric-error
         (fabric:recall "billing retry")
         (fabric:pending)
         (fabric:brief)))

;;; --- writing, then recalling ----------------------------------------------------

(deftest fabric-cell-what-one-write-puts-down-the-next-recall-finds ()
  (with-fabric ()
    (is (search "written to the fabric"
                (fabric:remember "decision"
                                 "We stopped retrying the billing webhook twice: the queue deduplicates by event id now, so a retry storm cannot double-charge."
                                 "billing retry removed from the webhook")) "a write says where it went")
    (is (= 1 (length (fabric-entries))) "one entry is one markdown file")
    (let* ((file (first (fabric-entries)))
           (agent (fabric-entry-field file "agent"))
           (id (fabric-entry-field file "id")))
      (is (equal "tester" agent) "the entry carries the section's agent")
      (is (= 8 (length id)) "and an id a handoff can name")
      (is (search "deduplicates by event id"
                  (uiop:read-file-string file)) "the body is on disk as it was written"))
    (let ((answer (fabric:recall "billing webhook retry")))
      (is (search "billing retry removed" answer) "the query's words find the entry")
      (is (search "relevance:" answer) "ranked, with the score it earned")
      (is (search "tester:" answer) "and the agent:id a review would name"))
    (is (equal nodecode-fabric::*nothing* (fabric:recall "kubernetes ingress")) "a query nothing matches says so")))

(deftest fabric-cell-the-best-match-ranks-first-and-a-budget-caps-the-rest ()
  (with-fabric ()
    (fabric:remember "decision" "the cache key includes the tenant id"
                      "cache key names the tenant")
    (fabric:remember "research" "unrelated notes about the release calendar and its dates"
                      "release calendar notes")
    (let ((answer (fabric:recall "tenant cache key")))
      (is (< (search "cache key names the tenant" answer)
             (or (search "release calendar notes" answer) most-positive-fixnum)) "the entry the words match ranks above the one they do not"))
    (is (search "release" (fabric:recall "tenant cache key release")) "both are returned when both match")
    (is (not (search "release calendar" (fabric:recall "tenant cache key release" :max-results 1))) "max_results caps the answer")))

(deftest fabric-cell-search-finds-an-exact-word-the-ranking-would-miss ()
  (with-fabric ()
    (fabric:remember "code-session" "the flag is --allow-unauthenticated on the deploy job"
                      "deploy job flag")
    (let ((answer (fabric:lookup "--allow-unauthenticated")))
      (is (search "tester:" answer) "the entry that holds the word comes back")
      (is (search "--allow-unauthenticated" answer) "with the line that matched"))
    (is (equal nodecode-fabric::*nothing* (fabric:lookup "no-such-word-here")) "a word no entry holds says so")))

;;; --- handoffs ---------------------------------------------------------------------

(deftest fabric-cell-pending-and-the-brief-read-a-handoff-from-another-agent ()
  ;; Three starts over one folder, the real way two sessions share it: this
  ;; agent records work, another hands work over and reviews that work, and
  ;; this one reads both back.
  (let ((dir (fabric-test-dir)) (work nil))
    (with-cell-stop ((fabric-start "dir" dir))
      (fabric:remember "decision" "the retry loop is in place and rate limited"
                        "retry loop landed")
      (is (= 1 (length (fabric-entries))) "the work is written")
      (setf work (format nil "tester:~a"
                         (fabric-entry-field (first (fabric-entries)) "id"))))
    (with-cell-stop ((fabric-start "agent" "other-agent" "dir" dir))
      (is (search "written to the fabric"
                  (fabric:remember "task" "finish the invoice export for the September close"
                                   "invoice export for September"
                                   :status "open" :assigned-to "tester")) "another agent hands work over")
      (is (search "written to the fabric"
                  (fabric:remember "review" "the retry loop looks right, but the timeout is not tested"
                                   "timeout untested" :review-of work)) "and reviews this agent's work by its reference"))
    (with-cell-stop ((fabric-start "dir" dir))
      (let ((pending (fabric:pending)))
        (is (search "1 task(s), 1 review(s)" pending) "what waits: the task assigned here, and the review of the work")
        (is (search "invoice export for September" pending) "the task is listed"))
      (let ((brief (fabric:brief)))
        (is (search "fabric brief for tester" brief) "the brief names whose it is")
        (is (search "pick up" brief) "and suggests the next action")
        (is (search "retry loop landed" brief) "with what this agent last did")))))

(deftest fabric-cell-curate-rewrites-the-field-in-the-entry-s-own-file ()
  (with-fabric ()
    (fabric:remember "decision" "the parser now rejects a bare flag" "bare flag rejected")
    (let* ((file (first (fabric-entries)))
           (id (fabric-entry-field file "id")))
      (is (search "training_value" (fabric:curate id "high")) "curate answers what it set")
      (is (equal "high" (fabric-entry-field file "training_value")) "the header carries it")
      (is (= 1 (length (fabric-entries))) "and no second entry was written")
      (is (equal "fabric: no entry has id \"nope\"." (fabric:curate "nope" "high")) "an id no entry has says so")
      (is (search "high, normal or low" (fabric:curate id "sometimes")) "and a value outside the three is refused"))))

;;; --- the turn's own memory -----------------------------------------------------

(deftest fabric-cell-the-round-hands-the-turn-what-its-own-words-match ()
  (with-fabric ()
    (fabric:remember "decision" "the billing webhook deduplicates by event id"
                      "billing retry removed")
    (let ((nodecode-fabric::*turn-input* (lambda (session)
                                           (declare (ignore session))
                                           "how did we fix the billing retry?")))
      (let ((text (nodecode-fabric::round-text "s1")))
        (is (search "[fabric:" text) "the framing line says it is reference data")
        (is (search "billing retry removed" text) "and the matching entry rides the turn"))
      (setf nodecode-fabric::*turn-input* (lambda (session) (declare (ignore session)) "nothing relevant"))
      (is (null (nodecode-fabric::round-text "s1")) "silence is the normal answer"))
    (is (equal nodecode-fabric::*standing* (fabric-standing "s1")) "every request carries the standing paragraph while the cell runs")))

;;; --- refusals --------------------------------------------------------------------

(deftest fabric-cell-a-write-that-cannot-be-read-back-is-refused-first ()
  (with-fabric ()
    (is (search "need a type" (fabric:remember "" "body" "summary")) "a type is required")
    (is (search "need a type" (fabric:remember "task" "" "summary")) "so is a content")
    (is (search "need a type" (fabric:remember "task" "body" "")) "and a summary")
    (is (search "needs assigned_to" (fabric:remember "task" "body" "summary" :status "open")) "an open task names who it is for")
    (is (search "review_of" (fabric:remember "review" "body" "summary")) "a review names the entry it reviews")
    (is (search "names no entry" (fabric:remember "review" "body" "summary"
                                                  :review-of "tester:deadbeef")) "and that entry must exist")
    (is (search "high, normal or low" (fabric:remember "decision" "body" "summary"
                                                         :training-value "maybe")) "training value is one of three")
    (is (= 0 (length (fabric-entries))) "nothing was written by any of them")))

;;; --- the boundary ------------------------------------------------------------

(deftest fabric-cell-each-tool-answers-through-its-contract ()
  (with-fabric ()
    (let* ((entry (nle::find-tool "fabric_write"))
           (write (third entry))
           (call (nlk:contract-decode (second entry)
                                      (nlk:make-json-object
                                       "type" "decision"
                                       "content" "the deploy job runs the flag first"
                                       "summary" "deploy order"
                                       "tags" "deploy,ci"))))
      (is (search "written to the fabric" (funcall write call)) "a tool call writes an entry"))
    (let* ((entry (nle::find-tool "fabric_recall"))
           (recall (third entry))
           (call (nlk:contract-decode (second entry)
                                      (nlk:make-json-object "query" "deploy order" "max_results" 2))))
      (is (search "deploy order" (funcall recall call)) "and a tool call reads it back"))
    (let* ((entry (nle::find-tool "fabric_brief"))
           (brief (third entry)))
      (is (search "fabric brief for tester"
                  (funcall brief (nlk:contract-decode (second entry) (nlk:make-json-object)))) "a tool with optional members answers with none given"))))
