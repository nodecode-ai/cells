;;;; cell-test.lisp --- lifecycle, the three tools, the round, /hindsight.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a stubbed Hindsight:
;;;; that a start installs the three tools, /hindsight, the manual and the
;;;; round hook, and a stop takes every one of them back; that a wrong
;;;; setting refuses; that each tool asks the configured bank and names what
;;;; Hindsight said when it refuses; that a bank nothing was retained to is
;;;; an empty memory, not a failure; that a keyless cell refuses without
;;;; dialing; that a turn's input is recalled into its own request once and
;;;; its operator half written once; and that /hindsight prints the bank and
;;;; asks by hand.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "hindsight"
  (:hooks :round)
  (:help :hindsight)
  (:command "hindsight")
  (:config "api_url" "https://hindsight.test" "api_key" "test-key")
  (:running (is (equal nodecode-hindsight::+primer+ (nle:help :hindsight)) "(help :hindsight) is the primer")
            (is (nle::find-tool "hindsight_retain") "the retain tool is registered")
            (is (nle::find-tool "hindsight_recall") "the recall tool is registered")
            (is (nle::find-tool "hindsight_reflect") "the reflect tool is registered")
            (is (equal "nodecode" (nodecode-hindsight::setting :bank-id)))
            (is (equal "mid" (nodecode-hindsight::setting :budget))))
  (:refused ("budget" "enormous") ("timeout" 0) ("api_url" 5))
  (:stopped (is (null (nle::find-tool "hindsight_recall")) "stop unregisters the tools")
            (is (null (nle::find-tool "hindsight_retain")))
            (is (null (nle::find-tool "hindsight_reflect")))))

;;; --- the tools ---------------------------------------------------------------------

(defun recall-answer (&rest texts)
  (lambda (url body)
    (declare (ignore url body))
    (values (nlk:encode-json-object
             (nlk:json-object "results"
                              (coerce (mapcar (lambda (text) (nlk:json-object "text" text)) texts)
                                      'vector)))
            200)))

(deftest hindsight-cell-recall-asks-the-configured-bank-with-the-key ()
  (with-hindsight ()
    (hindsight-asking "memories/recall" (recall-answer "the schema is event-sourced"
                                                        "the bank is nodecode"))
    (is (equal (format nil "1. the schema is event-sourced~%2. the bank is nodecode")
               (hindsight:recall "what did we settle about the schema")))
    (let ((request (first (hindsight-requests))))
      (is (equal "POST" (first request)))
      (is (equal "https://hindsight.test/v1/default/banks/nodecode/memories/recall"
                 (second request)) "the section's endpoint, workspace and bank")
      (is (equal "Bearer test-key" (hindsight-header request "authorization")) "the section's key, as a bearer token")
      (let ((body (hindsight-body request)))
        (is (equal "what did we settle about the schema" (gethash "query" body)))
        (is (equal "mid" (gethash "budget" body)) "the section's budget")
        (is (= 4096 (gethash "max_tokens" body)) "the section's recall cap")))
    (is (null nodecode-hindsight::*last-error*) "a good answer leaves no error standing")))

(deftest hindsight-cell-a-reflect-answers-the-synthesis ()
  (with-hindsight ()
    (hindsight-asking "reflect"
                      (lambda (url body)
                        (declare (ignore url body))
                        (values (nlk:encode-json-object
                                 (nlk:json-object "text" "we settled on an event-sourced schema"))
                                200)))
    (is (equal "we settled on an event-sourced schema"
               (hindsight:reflect "what have we settled")))
    (let ((body (hindsight-body (first (hindsight-requests)))))
      (is (equal "what have we settled" (gethash "query" body)))
      (is (equal "mid" (gethash "budget" body))))))

(deftest hindsight-cell-retain-posts-one-memory-with-its-label ()
  (with-hindsight ("tags" (vector "home"))
    (is (equal "Memory stored successfully."
               (hindsight:remember "the build runs with just"
                                   :context "build setup"
                                   :tags (vector "just"))))
    (let* ((request (first (hindsight-requests)))
           (body (hindsight-body request))
           (items (gethash "items" body))
           (item (aref items 0)))
      (is (equal "https://hindsight.test/v1/default/banks/nodecode/memories" (second request)))
      (is (= 1 (length items)) "one item a call")
      (is (equal "the build runs with just" (gethash "content" item)))
      (is (equal "build setup" (gethash "context" item)))
      (is (equal '("home" "just") (coerce (gethash "tags" item) 'list)) "the section's tags, then the call's")
      (is (equal "nodecode" (gethash "source" (gethash "metadata" item))))
      (is (stringp (gethash "timestamp" item)) "stamped with when it was said")
      (is (eq nil (gethash "async" body)) "a tool's store is done when it answers"))))

(deftest hindsight-cell-a-bank-nothing-was-retained-to-is-an-empty-memory ()
  (with-hindsight ()
    (hindsight-asking "recall" (lambda (url body) (declare (ignore url body)) (values "{\"detail\":\"bank_not_found\"}" 404)))
    (hindsight-asking "reflect" (lambda (url body) (declare (ignore url body)) (values "{}" 404)))
    (is (equal "No relevant memories found." (hindsight:recall "anything")))
    (is (equal "No relevant memories found." (hindsight:reflect "anything")))
    (is (null nodecode-hindsight::*last-error*) "404 is empty, not a failure")))

(deftest hindsight-cell-a-refusal-names-what-hindsight-said ()
  (with-hindsight ()
    (hindsight-asking "recall"
                      (lambda (url body)
                        (declare (ignore url body))
                        (values "{\"detail\":\"bank unavailable\"}" 503)))
    (let ((answer (hindsight:recall "anything")))
      (is (search "ERROR: HINDSIGHT-ERROR" answer) "the refusal is a failure the model reads")
      (is (search "HTTP 503" answer) "with the status")
      (is (search "bank unavailable" answer) "and Hindsight's own sentence"))
    (is (search "HTTP 503" nodecode-hindsight::*last-error*) "and /hindsight keeps it")))

(deftest hindsight-cell-without-a-key-refuses-and-never-dials ()
  (with-hindsight ("api_key" "")
    (let ((answer (hindsight:recall "anything")))
      (is (search "ERROR: HINDSIGHT-ERROR" answer))
      (is (search "no Hindsight API key" answer) "the refusal names what to set")
      (is (search "HINDSIGHT_API_KEY" answer) "and the variable it reads"))
    (is (null (hindsight-requests)) "nothing was dialed")
    (is (search "no Hindsight API key" (hindsight:remember "x")))))

;;; --- the tools through the real dispatch seam ----------------------------------------

(deftest hindsight-cell-the-tools-answer-through-the-dispatch-seam ()
  (with-hindsight ()
    (hindsight-asking "memories/recall" (recall-answer "one memory"))
    (let ((result (execute-wire-call "hs-1" "hindsight_recall" "{\"query\":\"memory\"}")))
      (is (equal "1. one memory" (nle::message-content result))))
    (let ((result (execute-wire-call "hs-2" "hindsight_retain"
                                     "{\"content\":\"kept\",\"context\":\"test\"}")))
      (is (equal "Memory stored successfully." (nle::message-content result))))
    ;; a call that leaves out the contract's required member is refused before the body
    (let ((refused (execute-wire-call "hs-3" "hindsight_recall" "{}")))
      (is (search "query" (nle::message-content refused)) "the refusal names the member"))
    (is (= 2 (length (hindsight-requests))) "one call each, nothing else")))

;;; --- the round: auto-recall and auto-retain -----------------------------------------

(deftest hindsight-cell-a-turn-is-recalled-into-its-own-request-once ()
  (with-session-store ("hs-round")
    (with-hindsight ()
      (hindsight-asking "memories/recall" (recall-answer "the schema is event-sourced"))
      (with-durable-turn (turn "hs-round" :input "what did we settle about the schema" :current t)
        (nodecode-hindsight::on-round (list :session-id "hs-round" :turn nle::*current-durable-turn* :round 0))
        (let ((text (prompt-text turn)))
          (is (search "# Hindsight memory" text) "the block says what it is")
          (is (search "- the schema is event-sourced" text) "and carries what was recalled"))
        ;; a later round of the same turn, same input: no second dial, nothing re-injected
        (nodecode-hindsight::on-round (list :session-id "hs-round" :turn nle::*current-durable-turn* :round 1))
        (let ((recalls (remove-if-not (lambda (r) (search "memories/recall" (second r)))
                                      (hindsight-requests)))
              (retains (remove-if-not (lambda (r) (let ((url (second r)))
                                                (and (search "/memories" url)
                                                     (not (search "memories/recall" url)))))
                                      (hindsight-requests))))
          (is (= 1 (length recalls)) "one recall a conversation's query")
          (is (= 1 (length retains)) "one retain a turn")
          (is (eq t (gethash "async" (hindsight-body (first retains)))) "the automatic half is async: it never holds the turn")
          (is (equal "User: what did we settle about the schema"
                     (gethash "content" (aref (gethash "items" (hindsight-body (first retains))) 0))))
          (is (equal '("session:hs-round")
                     (coerce (gethash "tags" (aref (gethash "items" (hindsight-body (first retains))) 0))
                             'list)) "the memory says which conversation it came from"))))))

(deftest hindsight-cell-the-switches-turn-the-automatic-halves-off ()
  (with-session-store ("hs-off")
    (with-hindsight ("auto_recall" nil "auto_retain" nil)
      (hindsight-asking "memories/recall" (recall-answer "never asked for"))
      (with-durable-turn (turn "hs-off" :input "a turn" :current t)
        (nodecode-hindsight::on-round (list :session-id "hs-off" :turn nle::*current-durable-turn* :round 0))
        (is (null (prompt-text turn)) "nothing injected")
        (is (null (hindsight-requests)) "and nothing dialed")))))

;;; --- /hindsight ----------------------------------------------------------------------

(deftest hindsight-cell-slash-prints-the-bank-and-asks-by-hand ()
  (with-hindsight ()
    (let ((line (nodecode-hindsight::run-slash "" "s1")))
      (is (search "hindsight: bank nodecode at https://hindsight.test" line) "the bank and where")
      (is (search "auto-recall on, auto-retain on" line) "the switches")
      (is (search "0 memories injected this session" line) "and nothing injected yet"))
    (is (search "hindsight: recall what?" (nodecode-hindsight::run-slash "recall" "s1")))
    (is (search "is not one of its choices"
                (nodecode-hindsight::run-slash "bogus" "s1")))
    (is (search "hindsight: bank nodecode" (nodecode-hindsight::run-slash "status" "s1")))
    (is (null (hindsight-requests)) "a status line and a bare verb dial nothing")
    (hindsight-asking "memories/recall" (recall-answer "from the operator"))
    (is (equal "1. from the operator"
               (nodecode-hindsight::run-slash "recall what about the schema" "s1")))
    (is (null nodecode-hindsight::*last-error*) "a recall leaves no error standing")
    (is (= 3 (length (getf (nth-value 1 (nodecode-hindsight::run-slash "" "s1")) :rows))) "bare /hindsight answers a dialog of its three asks")))
