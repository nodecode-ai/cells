;;;; cell-test.lisp --- lifecycle, the two tools, the round, /holographic.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a store in a
;;;; temporary file: that a start installs the two tools, /holographic, the
;;;; manual and the round hook, and a stop takes every one of them back; that
;;;; a wrong setting refuses; that a fact added is found by words it does not
;;;; say, that a change keeps the text it replaced, that an end hides a fact
;;;; from recall and a forget destroys it; that feedback moves its trust; and
;;;; that a turn's own message recalls the facts it holds into that request.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "holographic"
  (:hooks :round)
  (:help :holographic)
  (:command "holographic")
  (:config "db_path" (holographic-test-file))
  (:running (is (equal nodecode-holographic::+primer+ (nle:help :holographic)) "(help :holographic) is the primer")
            (is (nle::find-tool "fact_store") "the store tool is registered")
            (is (nle::find-tool "fact_feedback") "the feedback tool is registered")
            (is (equal "on" (nodecode-holographic::setting :recall)) "recall is on by default")
            (is (= 5 (nodecode-holographic::setting :recall-facts))))
  (:refused ("recall" "sometimes") ("recall_facts" 0))
  (:stopped (is (null (nle::find-tool "fact_store")) "stop unregisters the store tool")
            (is (null (nle::find-tool "fact_feedback")) "and the feedback tool")))

;;; --- the tools ---------------------------------------------------------------------

(deftest holographic-cell-add-then-search-finds-it-by-its-own-words ()
  (with-holographic ()
    (let ((answer (holographic-ask "action" "add" "content" "The build runs with just"
                                   "keywords" (vector "makefile" "compile")
                                   "origin" "said")))
      (is (search "stored #1" answer) "a new fact is numbered")
      (is (search "The build runs with just" answer)))
    (let ((found (holographic-ask "action" "search" "query" "how does the compile step run")))
      (is (search "The build runs with just" found) "a keyword the fact does not say finds it")
      (is (search "[1]" found) "and the line carries the id fact_feedback takes"))
    (is (search "The build runs with just"
                (holographic-ask "action" "search" "query" "build")) "a word of the text finds it too")
    (is (search "No relevant facts found."
                (holographic-ask "action" "search" "query" "weather forecast Lisbon tomorrow")) "a message it holds nothing of recalls nothing")))

(deftest holographic-cell-a-change-keeps-the-text-it-replaced ()
  (with-holographic ()
    (holographic-ask "action" "add" "content" "Alice lives in Paris")
    (is (search "changed: [1] Alice lives in Berlin"
                (holographic-ask "action" "change" "fact_id" 1
                                 "content" "Alice lives in Berlin"
                                 "valid_from" "2026-09-01")) "the fact says the new sentence")
    (is (search "No relevant facts found."
                (holographic-ask "action" "search" "query" "Paris")) "and Paris is no longer what it says")
    (let ((history (holographic-ask "action" "history" "fact_id" 1)))
      (is (search "Alice lives in Paris" history) "the version it replaced is kept")
      (is (search "Alice lives in Berlin" history) "and the current one is named"))))

(deftest holographic-cell-an-end-hides-a-fact-and-a-forget-destroys-it ()
  (with-holographic ()
    (holographic-ask "action" "add" "content" "The office is on Harbour Street")
    (holographic-ask "action" "add" "content" "The standup is at nine")
    (is (search "ended: [1]"
                (holographic-ask "action" "end" "fact_id" 1 "valid_to" "2026-10-01")) "ending names the fact")
    (is (not (search "Harbour Street"
                     (holographic-ask "action" "search" "query" "where is the office"))) "an ended fact is not recalled")
    (is (search "The office is on Harbour Street"
                (holographic-ask "action" "search" "query" "office" "as_of" "2026-09-01")) "but it held on a date before it ended")
    (is (search "forgot #2: The standup is at nine"
                (holographic-ask "action" "forget" "fact_id" 2)) "a forget answers what it destroyed")
    (is (search "No relevant facts found."
                (holographic-ask "action" "search" "query" "standup")) "and it is gone")
    (is (search "no fact #2" (holographic-ask "action" "history" "fact_id" 2)) "history of a forgotten fact refuses by id")))

(deftest holographic-cell-feedback-moves-the-trust-a-line-shows ()
  (with-holographic ("min_trust" "0.1")
    (holographic-ask "action" "add" "content" "The staging host is green")
    (is (search "trust now 0.60" (holographic-rate "helpful" 1)) "helpful raises it")
    (is (search "trust now 0.50" (holographic-rate "unhelpful" 1)) "unhelpful lowers it")
    (is (search "origin confirmed" (holographic-rate "confirmed" 1)) "confirmed records who stands behind it")
    (is (search "trust 0.65"
                (holographic-ask "action" "search" "query" "staging host")) "and the line the model reads says so")))

;;; --- the round ---------------------------------------------------------------------

(deftest holographic-cell-the-turns-own-message-recalls-into-its-request ()
  (with-holographic ()
    (holographic-ask "action" "add" "content" "The deploy key lives in the vault")
    (let ((recorded '()))
      (with-stubbed-fdefinitions
          ((nodecode-holographic::turn-input (turn)
             (declare (ignore turn))
             "where does the deploy key live")
           (nlk:store-open-p () t)
           (nlk:record-turn-context-prompt (turn &key text)
             (declare (ignore turn))
             (push text recorded)))
        (funcall (cell-hook "nodecode-holographic" :round)
                 (list :session-id "s1" :turn "t1")
                 (lambda (op) op)))
      (is (= 1 (length recorded)) "the block rides the request, once")
      (is (search "# Holographic memory" (first recorded)) "under its own heading")
      (is (search "The deploy key lives in the vault" (first recorded)) "with the fact the message recalls"))))

(deftest holographic-cell-recall-off-injects-nothing ()
  (with-holographic ("recall" "off")
    (holographic-ask "action" "add" "content" "The deploy key lives in the vault")
    (let ((recorded '()))
      (with-stubbed-fdefinitions
          ((nodecode-holographic::turn-input (turn)
             (declare (ignore turn))
             "where does the deploy key live")
           (nlk:store-open-p () t)
           (nlk:record-turn-context-prompt (turn &key text)
             (declare (ignore turn))
             (push text recorded)))
        (funcall (cell-hook "nodecode-holographic" :round)
                 (list :session-id "s1" :turn "t1")
                 (lambda (op) op)))
      (is (null recorded) "the switch turns the injection off, and the tools still answer"))))

;;; --- /holographic ------------------------------------------------------------------

(deftest holographic-cell-the-command-shows-the-store-and-searches ()
  (with-holographic ()
    (holographic-ask "action" "add" "content" "The release is cut on Thursdays")
    (let ((plain (nodecode-holographic::run-slash nil "s1")))
      (is (search "holographic: 1 facts (1 current)" plain) "bare /holographic is the store's line")
      (is (search "recall on" plain) "with the switch it runs under"))
    (is (search "The release is cut on Thursdays"
                (nodecode-holographic::run-slash "search release" "s1")) "/holographic search asks by hand")
    (is (search "stored #2" (nodecode-holographic::run-slash "add The docs live in the repo" "s1")) "/holographic add writes one")
    (is (search "is not one of its choices"
                (nodecode-holographic::run-slash "dance" "s1")) "a word it does not know says so")))
