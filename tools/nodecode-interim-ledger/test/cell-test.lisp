;;;; cell-test.lisp --- the rows, the advice, the reports, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here touches the network or a real turn: the function the cell
;;;; advises is called through its advice with a stubbed NEXT, the turn and
;;;; the lane it reads are stubbed, the ledger is a file in the scratch home,
;;;; and the clock is the one thing the tests move.

(in-package #:nodecode.test)

(define-test-slice "interim-ledger" "INTERIM-LEDGER-CELL-"
  :start nodecode-interim-ledger:start-cell)

(define-cell-lifecycle-tests "interim-ledger"
  (:help :interim-ledger)
  (:command "interim-log")
  (:hooks 'nlk:record-assistant-message)
  (:running (is (= 5000 (nodecode-interim-ledger::setting :rows)) "rows defaults to five thousand"))
  (:refused ("rows" 0) ("rows" 4) ("rows" "many"))
  (:idle nodecode-interim-ledger:interim-ledger-error
         (interim-ledger:summary)
         (interim-ledger:recent)
         (interim-ledger:clear)))

;;; --- the fixture ---------------------------------------------------------------------

(defparameter +mid-turn-text+ "Let me check the config first."
  "What a model says on its way to a tool, the message a row counts.")

(defun il-ledger-file ()
  "A ledger file of this test's own, in the scratch home, empty."
  (let ((path (nlk:home "interim-ledger-test/interim.jsonl")))
    (ensure-directories-exist path)
    (when (probe-file path) (delete-file path))
    path))

(defmacro with-ledger ((&key (now 1700000000)) &body body)
  "BODY with the cell started, its ledger a fresh file, the clock at NOW, a
unix second, for every row written, and the turn and lane stubbed."
  (let ((path (gensym "PATH")))
    `(let ((,path (il-ledger-file)))
       (with-stubbed-fdefinitions
           ((nodecode-interim-ledger::ledger-path () ,path)
            (nodecode-interim-ledger::now () ,now)
            (nodecode-interim-ledger::turn-facts () (values "session-one" "turn-one" 2))
            (nodecode-interim-ledger::lane-of (session-id)
              (values "a-provider" "a-model")))
         (with-cell-stop ((interim-ledger-start))
           (unwind-protect (progn ,@body)
             (when (probe-file ,path) (delete-file ,path))))))))

(defun il-answer (&optional (answer "recorded"))
  "A stubbed NEXT for a function hook: it records nothing and answers ANSWER."
  (lambda (&rest arguments) (declare (ignore arguments)) answer))

(defun il-message (&key (text +mid-turn-text+) (calls 1))
  "A wire assistant message: TEXT beside CALLS tool calls."
  (nlk:json-object "role" "assistant" "content" text
                   :when (plusp calls)
                   "tool_calls"
                   (coerce (loop for call below calls
                                  collect (nlk:json-object
                                           "id" (format nil "call-~d" call) "type" "function"
                                           "function" (nlk:json-object "name" "sh"
                                                                        "arguments" "{}")))
                           'vector)))

(defun il-observe (message)
  "MESSAGE as one completed round would hand it to the cell's advice."
  (nodecode-interim-ledger::observe-message (il-answer) nil message))

(defun il-rows ()
  "The ledger's rows, oldest first."
  (nodecode-interim-ledger::rows))

(defun il-handler ()
  "/interim-log's RUN, as the catalog's own headless arm answers it."
  (nodecode.evolved::slash-command-headless (nle::find-registered-command "interim-log")))

;;; --- one row a mid-turn message ------------------------------------------------------

(deftest interim-ledger-cell-a-mid-turn-message-becomes-one-row ()
  (with-ledger ()
    (is (equal "recorded" (il-observe (il-message))) "the advice answers what the round answered")
    (let ((rows (il-rows)))
      (is (= 1 (length rows)) "one message, one row")
      (let ((row (first rows)))
        (is (= (length +mid-turn-text+) (nodecode-interim-ledger::row-chars row)) "the length of the text, which is all of it that is kept")
        (is (= 1 (nodecode-interim-ledger::row-calls row)) "and how many tool calls rode it")
        (is (= 2 (nodecode-interim-ledger::row-step row)) "at its step of the turn")
        (is (equal "a-model" (nodecode-interim-ledger::row-model row)) "the lane the session pinned")
        (is (equal "a-provider" (nodecode-interim-ledger::row-provider row)))))))

(deftest interim-ledger-cell-an-answer-or-a-silent-call-leaves-no-row ()
  (with-ledger ()
    (il-observe (il-message :calls 0))
    (il-observe (il-message :text ""))
    (is (null (il-rows)) "only text beside a tool call is a mid-turn message")
    (il-observe (il-message :calls 3))
    (is (= 1 (length (il-rows))) "and a message with three calls is one message")))

(deftest interim-ledger-cell-ids-are-kept-as-hashes ()
  (with-ledger ()
    (il-observe (il-message))
    (let* ((row (first (il-rows)))
           (session (nodecode-interim-ledger::row-session row))
           (turn (nodecode-interim-ledger::row-turn row))
           (file (nlk:read-text (nodecode-interim-ledger::ledger-path))))
      (is (= 12 (length session)) "twelve hex digits stand in for the session")
      (is (every (lambda (char) (digit-char-p char 16)) session) "and they are hex")
      (is (= 12 (length turn)) "so does the turn")
      (is (null (search "session-one" file)) "the session id itself is never written")
      (is (null (search "turn-one" file)) "nor the turn id")
      (is (null (search "config first" file)) "nor a word of the message"))))

;;; --- the reports --------------------------------------------------------------------

(deftest interim-ledger-cell-summary-counts-messages-and-turns ()
  (with-ledger ()
    (il-observe (il-message))
    (il-observe (il-message :calls 2))
    (is-carrying (text (interim-ledger:summary))
      ("Mid-turn messages, last 24 hours:") ("a-model" "a row for the model")
      ("messages") ("turns") ("per turn") ("median chars") ("2.0" "two messages in one turn")
      ("Counted for every turn" "the note under the table"))
    (is-carrying (text (interim-ledger:summary :by "provider")) ("provider") ("a-provider"))))

(deftest interim-ledger-cell-the-command-line-choices ()
  (with-ledger ()
    (il-observe (il-message))
    (let ((handler (il-handler)))
      (is-carrying (text (funcall handler "7d" "s1"))
        ("Mid-turn messages, last 7 days:") ("a-model" "the model that wrote it"))
      (is-carrying (text (funcall handler "days" "s1")) ("day") ("messages"))
      (is-carrying (text (funcall handler "recent" "s1"))
        ("newest 1 of 1") ("step" "a step column") ("calls" "a call-count column"))
      (is-carrying (text (funcall handler "all providers" "s1")) ("all recorded") ("provider"))
      (is-carrying (text (funcall handler "nonsense" "s1")) ("Unknown option: nonsense"))
      (is-carrying (text (funcall handler "help" "s1")) ("/interim-log: which models"))
      (is-carrying (text (funcall handler "clear" "s1")) ("deleted 1 recorded message(s)"))
      (is (null (il-rows)) "and the ledger is gone"))))

(deftest interim-ledger-cell-a-window-leaves-older-rows-behind ()
  (with-ledger ()
    (il-observe (il-message))
    (nlk:append-json-line (nodecode-interim-ledger::ledger-path)
                           (nlk:json-object "ts" (- 1700000000 (* 3 86400))
                                            "session" "" "turn" "" "model" "old-model"
                                            "provider" "a-provider" "step" 1 "chars" 9 "calls" 1))
    (is (not (search "old-model" (interim-ledger:summary))) "three days ago is outside a day")
    (is (search "old-model" (interim-ledger:summary :window "all")) "and inside everything")
    (is (search "last 4 days" (funcall (il-handler) "4d" "s1")) "a window word titles the report")))

;;; --- the ledger keeps its shape ---------------------------------------------------

(deftest interim-ledger-cell-a-full-ledger-keeps-the-newest-rows ()
  (with-ledger ()
    (let ((path (nodecode-interim-ledger::ledger-path)))
      (dotimes (n 8)
        (nlk:append-json-line path (nlk:json-object "ts" (+ 1700000000 n) "session" "" "turn" ""
                                                     "model" (format nil "m~d" n)
                                                     "provider" "a-provider" "step" 1
                                                     "chars" 1 "calls" 1)))
      (nodecode-interim-ledger::trim path 5)
      (let ((models (mapcar #'nodecode-interim-ledger::row-model (il-rows))))
        (is (= 5 (length models)) "the file holds the bound")
        (is (equal '("m3" "m4" "m5" "m6" "m7") models) "and the newest of them")))))

(deftest interim-ledger-cell-a-broken-write-never-breaks-the-turn ()
  (with-ledger ()
    (with-stubbed-fdefinitions
        ((nodecode-interim-ledger::append-row (row)
           (error "the disk is full")))
      (is (equal "recorded" (il-observe (il-message))) "the round records itself anyway"))
    (is (null (il-rows)) "and a message that could not be counted is not a failure")))
