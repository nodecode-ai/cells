;;;; cell-test.lisp --- lifecycle, the three tools, the round, /membase.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a stubbed Membase:
;;;; that a start installs the three tools, /membase, the manual and the round
;;;; hook, and a stop takes every one of them back; that a wrong setting
;;;; refuses; that each tool asks the configured endpoint with the key and
;;;; names what Membase said when it refuses; that a keyless cell refuses
;;;; without dialing; that a turn's input is searched into its own request
;;;; once, and captured only when the switch is on; and that /membase prints
;;;; the endpoint and asks by hand.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "membase"
  (:hooks :round)
  (:help :membase)
  (:command "membase")
  (:config "base_url" "https://membase.test" "api_key" "test-key")
  (:running (is (equal nodecode-membase::+primer+ (nle:help :membase)) "(help :membase) is the primer")
            (is (nle::find-tool "membase_search") "the search tool is registered")
            (is (nle::find-tool "membase_save") "the save tool is registered")
            (is (nle::find-tool "membase_profile") "the profile tool is registered")
            (is (equal "full" (nodecode-membase::setting :recall-mode)))
            (is (= 5 (nodecode-membase::setting :recall-limit))))
  (:refused ("recall_limit" 0) ("timeout" 0) ("recall_mode" "slow"))
  (:stopped (is (null (nle::find-tool "membase_search")) "stop unregisters the tools")
            (is (null (nle::find-tool "membase_save")))
            (is (null (nle::find-tool "membase_profile")))))

;;; --- the three verbs ---------------------------------------------------------------

(defun search-answer (&rest passages)
  "An ANSWER for a route that points each (MEMORY . TEXT) PASSAGE at its Memory."
  (lambda (url body)
    (declare (ignore url body))
    (values (membase-json
             (nlk:json-object
              "items"
              (coerce (mapcar (lambda (passage)
                                (nlk:json-object "memory_name" (car passage)
                                                 "content" (cdr passage)))
                              passages)
                      'vector)))
            200)))

(deftest membase-cell-search-asks-the-memories-with-the-key ()
  (with-membase ()
    (membase-asking "/v1/search"
                     (search-answer (cons "Project decisions" "We settled on Postgres for the ledger.")
                                    (cons "Membase" "The ledger is event-sourced.")))
    (let ((answer (membase:recall "what did we settle about the ledger" :limit 3)))
      (is (search "[Project decisions] We settled on Postgres for the ledger." answer) "a passage names the Memory it came from")
      (is (search "[Membase] The ledger is event-sourced." answer)))
    (let ((request (first (membase-requests))))
      (is (equal "POST" (first request)))
      (is (equal "https://membase.test/v1/search" (second request)) "the section's endpoint")
      (is (equal "Bearer test-key" (membase-header request "authorization")) "the section's key, as a bearer token")
      (let ((body (membase-body request)))
        (is (equal "what did we settle about the ledger" (gethash "q" body)))
        (is (equal "full" (gethash "mode" body)) "the section's recall mode")
        (is (= 3 (gethash "limit" body)) "the call's own limit")))
    (is (null nodecode-membase::*last-error*) "a good answer leaves no error standing")))

(deftest membase-cell-a-memory-that-could-not-answer-is-named ()
  (with-membase ()
    (membase-asking "/v1/search"
                     (lambda (url body)
                       (declare (ignore url body))
                       (values (membase-json
                                (nlk:json-object
                                 "items" (vector (nlk:json-object "memory_name" "Membase"
                                                                  "content" "one passage"))
                                 "partial" (vector (nlk:json-object "memory_name" "Work"
                                                                    "reason" "error")
                                                   (nlk:json-object "memory_name" "Notes"
                                                                    "reason" "truncated"))))
                               200)))
    (let ((answer (membase:recall "anything")))
      (is (search "one passage" answer))
      (is (search "Work" answer) "the Memory that failed is named")
      (is (not (search "Notes" answer)) "one that only lost to the limit did answer"))))

(deftest membase-cell-save-posts-one-note-and-reads-the-receipt ()
  (with-membase ()
    (membase-asking "/v1/memories/add"
                     (lambda (url body)
                       (declare (ignore url body))
                       (values (membase-json
                                (nlk:json-object "id" "doc_123" "status" "processing"
                                                 "learning" (vector (nlk:json-object "memory_name" "Work"))))
                               200)))
    (let ((answer (membase:remember "the build runs with just")))
      (is (search "Saved doc_123" answer) "the receipt's id")
      (is (search "into Work" answer) "the Memory that will learn it")
      (is (search "searchable after that" answer) "and that it is not instant"))
    (let* ((request (first (membase-requests)))
           (body (membase-body request)))
      (is (equal "https://membase.test/v1/memories/add" (second request)))
      (is (equal "the build runs with just" (gethash "content" body)))
      (is (stringp (gethash "title" body))))))

(deftest membase-cell-profile-lists-the-standing-facts ()
  (with-membase ()
    (membase-asking "/v1/profile-facts"
                     (lambda (url body)
                       (declare (ignore url body))
                       (values (membase-json
                                (nlk:json-object "items"
                                                 (vector (nlk:json-object "content" "prefers short answers")
                                                         (nlk:json-object "content" "maintains an event-sourced ledger"))))
                               200)))
    (let ((answer (membase:profile)))
      (is (search "prefers short answers" answer))
      (is (search "event-sourced ledger" answer)))
    (is (search "limit=50" (second (first (membase-requests)))) "one page of facts, newest first")))

(deftest membase-cell-without-a-key-refuses-and-never-dials ()
  (with-membase ("api_key" "")
    (let ((answer (membase:recall "anything")))
      (is (search "ERROR: MEMBASE-ERROR" answer))
      (is (search "no Membase API key" answer) "the refusal names what to set")
      (is (search "MEMBASE_API_KEY" answer) "and the variable it reads"))
    (is (null (membase-requests)) "nothing was dialed")
    (is (search "no Membase API key" (membase:remember "x")))))

(deftest membase-cell-a-refusal-names-what-membase-said ()
  (with-membase ()
    (membase-asking "/v1/search"
                     (lambda (url body)
                       (declare (ignore url body))
                       (values "{\"error\":{\"message\":\"memory unavailable\"}}" 503)))
    (let ((answer (membase:recall "anything")))
      (is (search "ERROR: MEMBASE-ERROR" answer) "the refusal is a failure the model reads")
      (is (search "HTTP 503" answer) "with the status")
      (is (search "memory unavailable" answer) "and Membase's own sentence"))
    (is (search "HTTP 503" nodecode-membase::*last-error*) "and /membase keeps it")))

;;; --- the tools through the real dispatch seam ----------------------------------------

(deftest membase-cell-the-tools-answer-through-the-dispatch-seam ()
  (with-membase ()
    (membase-asking "/v1/search" (search-answer (cons "Membase" "one passage")))
    (membase-asking "/v1/memories/add"
                     (lambda (url body)
                       (declare (ignore url body))
                       (values (membase-json (nlk:json-object "id" "doc_9" "status" "pending")) 200)))
    (membase-asking "/v1/profile-facts"
                     (lambda (url body)
                       (declare (ignore url body))
                       (values (membase-json (nlk:json-object "items"
                                                               (vector (nlk:json-object "content" "prefers short answers"))))
                               200)))
    (let ((result (execute-wire-call "mb-1" "membase_search" "{\"query\":\"memory\"}")))
      (is (search "[Membase] one passage" (nle::message-content result))))
    (let ((result (execute-wire-call "mb-2" "membase_save" "{\"content\":\"kept\"}")))
      (is (search "Saved doc_9" (nle::message-content result))))
    (let ((result (execute-wire-call "mb-3" "membase_profile" "{}")))
      (is (search "prefers short answers" (nle::message-content result)) "a tool with no members takes an empty object"))
    ;; a call that leaves out the contract's required member is refused before the body
    (let ((refused (execute-wire-call "mb-4" "membase_search" "{}")))
      (is (search "query" (nle::message-content refused)) "the refusal names the member"))
    (is (= 3 (length (membase-requests))) "one dial each, nothing else")))

;;; --- the round: recall, and the opt-in capture -------------------------------------

(deftest membase-cell-a-turn-is-recalled-into-its-own-request-once ()
  (with-session-store ("mb-round")
    (with-membase ()
      (membase-asking "/v1/search" (search-answer (cons "Project decisions" "We settled on Postgres.")))
      (with-durable-turn (turn "mb-round" :input "what did we settle about the ledger" :current t)
        (nodecode-membase::on-round (list :session-id "mb-round" :turn nle::*current-durable-turn* :round 0))
        (let ((text (prompt-text turn)))
          (is (search "## Membase" text) "the block says what it is")
          (is (search "[Project decisions] We settled on Postgres." text) "and carries what was recalled"))
        (nodecode-membase::on-round (list :session-id "mb-round" :turn nle::*current-durable-turn* :round 1))
        (is (= 1 (length (membase-requests))) "one search a conversation's input")
        (is (= 1 nodecode-membase::*recalled*) "one passage counted")))))

(deftest membase-cell-capture-is-off-until-the-switch-is-on ()
  (with-session-store ("mb-capture")
    (with-membase ("capture_turns" t)
      (membase-asking "/v1/search" (search-answer (cons "Membase" "one passage")))
      (with-durable-turn (turn "mb-capture" :input "remember that the build runs with just" :current t)
        (nodecode-membase::on-round (list :session-id "mb-capture" :turn nle::*current-durable-turn* :round 0))
        (let ((saves (remove-if-not (lambda (request) (search "/v1/memories/add" (second request)))
                                     (membase-requests))))
          (is (= 1 (length saves)) "the operator's words are saved once")
          (is (equal "remember that the build runs with just"
                     (gethash "content" (membase-body (first saves)))))
          (is (search "Nodecode conversation" (gethash "title" (membase-body (first saves))))))))))

;;; --- /membase ----------------------------------------------------------------------

(deftest membase-cell-slash-prints-the-endpoint-and-asks-by-hand ()
  (with-membase ()
    (let ((line (nodecode-membase::run-slash "" "s1")))
      (is (search "membase: https://membase.test" line) "the endpoint")
      (is (search "key set, auto-recall on, capture off" line) "the switches")
      (is (search "0 passages injected this session" line) "and nothing injected yet"))
    (is (search "membase: search what?" (nodecode-membase::run-slash "search" "s1")))
    (is (search "is not one of its choices" (nodecode-membase::run-slash "bogus" "s1")))
    (is (null (membase-requests)) "a status line and a bare verb dial nothing")
    (membase-asking "/v1/search" (search-answer (cons "Membase" "from the operator")))
    (is (search "[Membase] from the operator"
                (nodecode-membase::run-slash "search what about the schema" "s1")))
    (is (= 3 (length (getf (nth-value 1 (nodecode-membase::run-slash "" "s1")) :rows))) "bare /membase answers a dialog of its three asks")))
