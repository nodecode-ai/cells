;;;; cell-test.lisp --- the optchat cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No model is dialled: a summary is a stubbed NLE:COMPLETE, and every
;;;; message is a frame the gateway publishes (CELL-FRAME-OP). The worker is
;;;; off in every test (*USE-WORKER* NIL) so PUMP-ONCE runs on this thread and
;;;; the assertions see what it built.

(in-package #:nodecode.test)

(define-test-slice "optchat" "OPTCHAT-CELL-" :start nodecode-optchat:start-cell)

(define-cell-lifecycle-tests "optchat"
  (:hooks :frame 'nle::read-harness-sections)
  (:help :optchat)
  (:command "optchat")
  (:running
   (is (nle::find-tool "optchat_zoom") "the zoom tool is registered")
   (is (nle::find-tool "optchat_date") "the date tool is registered")
   (is (eq (nlk:contract 'nodecode-optchat::zoom-line)
           (second (nle::find-tool "optchat_zoom"))) "registered against its declaration"))
  (:stopped
   (is (null (nle::find-tool "optchat_zoom")) "stop unregisters the zoom tool")
   (is (null (nle::find-tool "optchat_date")) "and the date tool"))
  (:refused ("view_budget_chars" 100) ("node_bytes" 1) ("store_dir" 7)))

;;; --- the frames a turn publishes ----------------------------------------------------

(defun optchat-asked (session text)
  "The frame that carries TEXT as SESSION's committed ask."
  (cell-frame-op "turn.input_committed" :session session
                                        :payload (nlk:json-object "message" text)))

(defun optchat-answered (session text)
  "The frame that carries TEXT as SESSION's assistant round."
  (cell-frame-op "turn.assistant_message_completed"
                  :session session
                  :payload (nlk:json-object "message" (nlk:json-object "role" "assistant"
                                                                   "content" text))))

(defun optchat-called (session name arguments)
  "The frame that carries one tool call."
  (cell-frame-op "turn.tool_call_started" :session session
                                         :payload (nlk:json-object "tool-name" name
                                                                   "arguments" arguments)))

(defun optchat-result (session text)
  "The frame that carries one tool result."
  (cell-frame-op "turn.tool_result" :session session
                                     :payload (nlk:json-object "result" text)))

;;; --- the log, the line and the view --------------------------------------------------

(deftest optchat-cell-logs-a-turn-and-renders-it ()
  (let ((nodecode-optchat::*use-worker* nil))
    (with-cell-stop ((optchat-start "store_dir" "optchat-cell-log" "compact_enabled" nil))
      (cell-publish "nodecode-optchat" (optchat-asked "s1" "ASK ONE"))
      (cell-publish "nodecode-optchat" (optchat-answered "s1" "ANSWER ONE"))
      (cell-publish "nodecode-optchat" (optchat-called "s1" "read_file" "{}"))
      (cell-publish "nodecode-optchat" (optchat-result "s1" "RESULT ONE"))
      (is (= 4 (nodecode-optchat:remembered)) "every message of the turn is remembered")
      (let ((block (cell-section "s1" "optchat")))
        (is (search "4 messages remembered" block) "the block counts what is remembered")
        (is (search "(not summarized yet: zoom it)" block) "and says a line is not summarized yet"))
      (is (search "user: ASK ONE" (nodecode-optchat:zoom 0 1)) "a message reads back verbatim")
      (is (search "talk: ANSWER ONE" (nodecode-optchat:zoom 1 1)))
      (is (search "read_file({})" (nodecode-optchat:zoom 2 1)) "a tool call keeps its arguments")
      (is (search "RESULT ONE" (nodecode-optchat:zoom 3 1)))
      (is (plusp (length (nodecode-optchat:message-date 0))) "a message carries its date")
      (is (search "No line 9+1" (nodecode-optchat:zoom 9 1)) "a line past the log says so"))))

(deftest optchat-cell-remembers-across-a-restart ()
  (let ((nodecode-optchat::*use-worker* nil))
    (with-cell-stop ((optchat-start "store_dir" "optchat-cell-restart" "compact_enabled" nil))
      (cell-publish "nodecode-optchat" (optchat-asked "s1" "ASK BEFORE RESTART")))
    (with-cell-stop ((optchat-start "store_dir" "optchat-cell-restart" "compact_enabled" nil))
      (is (= 1 (nodecode-optchat:remembered)) "the log outlives the start")
      (nodecode-optchat::pump-once)
      (is (search "ASK BEFORE RESTART" (cell-section "s1" "optchat")) "and the next session reads it without asking"))))

;;; --- the tree: a free node, then a summarized one ------------------------------------

(deftest optchat-cell-a-short-message-is-its-own-line ()
  (let ((nodecode-optchat::*use-worker* nil))
    (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                               (error "no model call was due for a short message"))
      (with-cell-stop ((optchat-start "store_dir" "optchat-cell-free" "compact_enabled" nil))
        (cell-publish "nodecode-optchat" (optchat-asked "s1" "ASK ONE"))
        (is (nodecode-optchat::pump-once) "one line is due")
        (let ((block (cell-section "s1" "optchat")))
          (is (search "0+1|user: ASK ONE" block) "a short message is its own line, word for word")
          (is (null (search "(not summarized yet" block)) "and the line is built"))))))

(deftest optchat-cell-a-long-message-is-summarized ()
  (let ((nodecode-optchat::*use-worker* nil)
        (calls '()))
    (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                               (push (list system user) calls)
                               "user: a long ask about the deploy pipeline")
      (with-cell-stop ((optchat-start "store_dir" "optchat-cell-long" "compact_enabled" nil
                                       "node_bytes" 128))
        (cell-publish "nodecode-optchat" (optchat-asked "s1" (make-string 300 :initial-element #\a)))
        (is (nodecode-optchat::pump-once) "one line is due")
        (is (= 1 (length calls)) "one side call for one long message")
        (is (search "memory of Nodecode" (first (first calls))) "the summary prompt is the one the cell carries")
        (is (search "the deploy pipeline" (cell-section "s1" "optchat")) "the line is the summary")))))

;;; --- zoom ----------------------------------------------------------------------------

(deftest optchat-cell-zoom-opens-a-built-line ()
  (let ((nodecode-optchat::*use-worker* nil))
    (with-cell-stop ((optchat-start "store_dir" "optchat-cell-zoom" "compact_enabled" nil))
      (cell-publish "nodecode-optchat" (optchat-asked "s1" "ASK ONE"))
      (cell-publish "nodecode-optchat" (optchat-answered "s1" "ANSWER ONE"))
      (loop repeat 3 do (nodecode-optchat::pump-once))
      (let ((opened (nodecode-optchat:zoom 0 2)))
        (is (search "0+1|user: ASK ONE" opened) "the line opens into its first half")
        (is (search "1+1|talk: ANSWER ONE" opened) "and its second")
        (is (null (search "(not summarized" opened))))
      (is (search "No line 0+4" (nodecode-optchat:zoom 0 4)) "a line the tree has not built says so")
      (is (search "No line 3+2" (nodecode-optchat:zoom 3 2)) "and a line off the log says so"))))

;;; --- /optchat -------------------------------------------------------------------------

(deftest optchat-cell-slash-answers-status-and-zoom ()
  (let ((nodecode-optchat::*use-worker* nil))
    (with-cell-stop ((optchat-start "store_dir" "optchat-cell-slash" "compact_enabled" nil))
      (cell-publish "nodecode-optchat" (optchat-asked "s1" "ASK ONE"))
      (multiple-value-bind (text dialog) (cell-entry "nodecode-optchat" "optchat" "")
        (is (search "optchat:" text) "the command answers its status")
        (is (search "1 messages remembered" text))
        (is (not (null dialog)) "and a panel to act from"))
      (is (search "ASK ONE" (cell-entry "nodecode-optchat" "optchat" "zoom 0 1")) "a line opens from the command")
      (is (search "not one of its choices" (cell-entry "nodecode-optchat" "optchat" "wibble"))))))
