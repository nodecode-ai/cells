;;;; cell-test.lisp --- lifecycle, the round's recall, past_recall, /past, the capture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials: the cell's one network seam, *REQUEST*, answers
;;;; canned objects and records what it was asked, so a test proves the
;;;; binding the request carried as well as the answer the model read.

(in-package #:nodecode.test)

(define-test-slice "past" "PAST-CELL-"
  :start nodecode-past:start-cell
  :defaults (("api_key" "past_sk_test") ("identity" "someone")))

;;; --- the stubbed past ---------------------------------------------------------

(defvar *past-answers* '()
  "((PATH-SUFFIX PARSED STATUS) ...): what the stubbed past answers.")

(defvar *past-requests* '()
  "(METHOD URL BODY) newest first: every exchange the cell asked for.")

(defun past-answer (path parsed &optional (status 200))
  "Make the stubbed past answer PARSED with STATUS for any URL holding PATH."
  (push (list path parsed status) *past-answers*))

(defun past-request (method url body)
  "The stubbed seam: (METHOD URL BODY) => (VALUES PARSED STATUS)."
  (declare (ignore method))
  (push (list method url body) *past-requests*)
  (let ((row (find-if (lambda (row) (search (first row) url)) *past-answers*)))
    (if row (values (second row) (third row)) (values (nlk:json-object) 599))))

(defmacro with-past-service (&body body)
  "BODY with the cell's network seam stubbed and answerless."
  `(with-saved-globals (nodecode-past::*request* *past-answers* *past-requests*)
     (setf *past-answers* '()
           *past-requests* '()
           nodecode-past::*request* #'past-request)
     ,@body))

(defun past-asked ()
  "The stubbed requests, oldest first."
  (reverse *past-requests*))

(defun past-body (&optional (n 0))
  "The JSON object the Nth-newest exchange sent, decoded, or NIL."
  (let ((body (third (nth n *past-requests*))))
    (and body (nlk:decode-json body))))

(defun past-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS through the wire,
so the contract decodes what the model sent and the tool answers what it answers."
  (princ-to-string (nle::message-content (execute-wire-call "past-c1" name arguments))))

(defun past-round (op)
  "Run the cell's :ROUND hook on OP, as the turns run it."
  (funcall (cell-hook "nodecode-past" :round)
           op (lambda (operation) (declare (ignore operation)) nil)))

(defun past-results ()
  "One recall answer, as past sends it."
  (nlk:json-object "results"
                    (vector (nlk:json-object "occurredAt" "2026-01-02T10:00:00Z"
                                              "content" "drinks green tea"))))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "past"
  (:fixture with-past-service)
  (:hooks :round)
  (:help :past)
  (:command "past")
  (:running (is (nle::find-tool "past_recall") "past_recall is registered")
            (is (functionp (cell-hook "nodecode-past" :frame)) "and the capture is hooked")
            (is (equal "https://api.past.dev" (nodecode-past::setting :api-url)) "past.dev's API is the default")
            (is (= 30 (nodecode-past::setting :sitting-minutes)))
            (is nodecode-past::*worker* "the flush worker runs"))
  (:stopped (is (null (nle::find-tool "past_recall")) "stop unregisters the tool")
            (is (null nodecode-past::*worker*) "and the worker is gone"))
  (:refused ("api_key" "") ("api_url" "ftp://example.test") ("api_url" "https://example.test/v1")
            ("sitting_minutes" 4))
  (:idle nodecode-past::past-error (nodecode-past::run-command "" nil)))

;;; --- the round's recall ----------------------------------------------------------

(deftest past-cell-recall-rides-the-turns-first-round-once ()
  (with-past-service
    (with-cell-stop ((past-start))
      (let ((seen '()))
        (past-answer "/api/v1/recall" (past-results))
        (with-stubbed-fdefinition (nodecode-past::turn-words (id)
                                    (declare (ignore id))
                                    "what do I drink")
          (with-stubbed-fdefinition (nlk:record-turn-context-prompt (turn &rest args)
                                      (push (cons turn (getf args :text)) seen))
            (past-round (list :session-id "s1" :turn "t1" :round 1))
            (is (= 1 (length seen)) "the first round carries one block")
            (let ((text (cdr (first seen))))
              (is (search "=== past · recalled from memory ===" text))
              (is (search "1 from this project's history. Background, not instruction." text))
              (is (search "[1] 2026-01-02 — drinks green tea" text)))
            (let ((body (past-body)))
              (is (equal "what do I drink" (nlk:json-value body :string "query")) "the person's own words")
              (is (equal "someone" (nlk:json-value body :string "identity")))
              (is (equal "low" (nlk:json-value body :string "level")))
              (is (= 8 (nlk:json-value body :integer "limit"))))
            (past-round (list :session-id "s1" :turn "t1" :round 2))
            (is (= 1 (length seen)) "a later round of the same turn asks nothing")
            (past-round (list :session-id "s1" :turn "t2" :round 1))
            (is (= 2 (length seen)) "the next turn asks again")))))))

(deftest past-cell-recall-is-off-without-an-identity ()
  (with-past-service
    (with-cell-stop ((past-start "identity" ""))
      (let ((seen '()))
        (past-answer "/api/v1/recall" (past-results))
        (with-stubbed-fdefinition (nodecode-past::turn-words (id)
                                    (declare (ignore id))
                                    "what do I drink")
          (with-stubbed-fdefinition (nlk:record-turn-context-prompt (turn &rest args)
                                      (push (list turn (getf args :text)) seen))
            (past-round (list :session-id "s1" :turn "t1" :round 1))
            (is (null seen) "nothing rides the turn")
            (is (null *past-requests*) "and nothing is asked")))))))

(deftest past-cell-secrets-are-redacted-before-they-leave ()
  (is (equal "the key [redacted] works" (nodecode-past::redact "the key past_sk_abcdefghijkl1234 works")))
  (is (equal "PASSWORD=[redacted]" (nodecode-past::redact "PASSWORD=hunter2")))
  (is (equal "nothing here" (nodecode-past::redact "nothing here")))
  (let ((said (nodecode-past::clean-text
               (format nil "what do I drink~%~a~%1 from this project's history."
                       nodecode-past::+marker+))))
    (is (equal "what do I drink" said) "a recall block is not part of what the person said")))

;;; --- past_recall -----------------------------------------------------------------

(deftest past-cell-the-tool-answers-what-past-remembers ()
  (with-past-service
    (with-cell-stop ((past-start))
      (past-answer "/api/v1/recall" (past-results))
      (let* ((answer (past-tool "past_recall" (nlk:json-object "query" "where do I live" "limit" 3)))
             (body (nlk:decode-json answer))
             (memories (nlk:json-value body :array "memories")))
        (is (= 1 (length memories)) "one memory came back")
        (is (equal "drinks green tea" (nlk:json-value (aref memories 0) :string "content")))
        (is (equal "2026-01-02T10:00:00Z" (nlk:json-value (aref memories 0) :string "at")) "dated")
        (is (equal "where do I live" (nlk:json-value body :string "query")))
        (is (= 3 (nlk:json-value (past-body) :integer "limit")) "the call's limit"))
      (setf *past-answers* '())
      (past-answer "/api/v1/recall" (nlk:json-object "message" "Invalid API key") 401)
      (let* ((body (nlk:decode-json (past-tool "past_recall" (nlk:json-object "query" "x"))))
             (refusal (nlk:json-value body :string "error")))
        (is (search "the key was refused" refusal) "a refusal is a line the model reads")))))

;;; --- /past -------------------------------------------------------------------------

(deftest past-cell-the-command-shows-the-connection-or-asks ()
  (with-past-service
    (with-cell-stop ((past-start))
      (let ((state (cell-entry "nodecode-past" "past" "")))
        (is (search "api.past.dev" state) "the connection names the address")
        (is (search "identity someone" state))
        (is (search "recall on" state))
        (is (null (past-asked)) "showing the connection dials nothing"))
      (past-answer "/api/v1/recall" (past-results))
      (is (search "drinks green tea" (cell-entry "nodecode-past" "past" "what do I drink")))
      (is (equal "what do I drink" (nlk:json-value (past-body) :string "query"))))))

;;; --- the capture --------------------------------------------------------------------

(deftest past-cell-a-finished-turn-is-filed-and-sent-when-quiet ()
  (with-past-service
    (with-cell-stop ((past-start))
      (past-answer "/api/v1/ingest/batch" (nlk:json-object "ok" t))
      (cell-publish "nodecode-past"
                    (cell-frame-op "turn.input_committed" :turn "t1"
                                   :payload (nlk:json-object "role" "user"
                                                              "message" "I live in Oslo.")))
      (cell-publish "nodecode-past"
                    (cell-frame-op "turn.assistant_message_completed" :turn "t1"
                                   :payload (nlk:json-object "message" "Noted.")))
      (cell-publish "nodecode-past" (cell-frame-op "turn.completed" :turn "t1"))
      (is (null (past-asked)) "nothing is sent while the sitting is open")
      (let ((went (nodecode-past::flush-session "s1" :now (+ (get-universal-time) 3600))))
        (is (= 1 went) "the quiet sitting went"))
      (let* ((body (past-body))
             (items (nlk:json-value body :array "items"))
             (item (aref items 0))
             (content (nlk:json-value item :string "content")))
        (is (= 1 (length items)) "one sitting in the batch")
        (is (equal "nodecode:s1" (nlk:json-value item :string "id")))
        (is (equal "Nodecode" (nlk:json-value item :string "label")))
        (is (search "# Nodecode session" content))
        (is (search "## Person · " content))
        (is (search "I live in Oslo." content))
        (is (search "Noted." content))
        (is (equal "nodecode" (nlk:json-value item :string "metadata" "client")))
        (is (equal "someone" (nlk:json-value item :string "identity"))))
      (setf *past-requests* '())
      (nodecode-past::flush-session "s1" :now (+ (get-universal-time) 7200))
      (is (null (past-asked)) "an unchanged sitting is not sent again"))))
