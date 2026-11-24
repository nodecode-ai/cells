;;;; cell.lisp --- endless memory in Nodecode: the log, the view, the two tools, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Where the original hands Hermes a MemoryProvider, this cell takes the two
;;;; seams a Nodecode cell has: the :FRAME point (every turn's messages, to
;;;; write them to the log) and NLE::READ-HARNESS-SECTIONS (what every
;;;; request carries, where the view rides). The summary worker never runs on
;;;; the publishing thread: a frame only appends a line and pokes it.
;;;;
;;;; Config, a top-level key next to `index' and `cron':
;;;;   "optchat": {"store_dir": "optchat", "view_budget_chars": 16000,
;;;;               "node_bytes": 512, "compact_enabled": true}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-optchat)

;;; --- the manual ------------------------------------------------------------------
;;; (help :optchat) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter +primer+
  "Every message of every turn is kept forever in a local append-only log (user words, talk,
tool calls, echoes), and every request carries a `optchat' block: the view, one line per
stretch of the log, oldest first, recent messages one line each and older ones coarser so the
whole history fits a fixed budget. A short message is its own line, word for word; a long one
is summarized by a cheap side call, in the background, in order. The view never shows a whole
message beyond its own summary: open a line before acting on it.
  (optchat:zoom ID N)   open line ID+N into the two lines of N/2 messages it was made from;
      N = 1 gives message ID in full. ID and N are read straight off a view line, 2184+8.
  (optchat:message-date ID)   when message ID was written.
  (optchat:status)   how much is remembered, how much is summarized, how the view fits.
A line whose node is not built yet reads (not summarized yet: zoom it); it is being written by
the worker. The log is the truth; the tree is only how the past is read."
  "What (help :optchat) answers while the cell runs.")

;;; --- the block every request carries -------------------------------------------------

(defun remembered-block ()
  "The `optchat' harness block: the count of what is remembered, then the
view. NIL when nothing is remembered yet."
  (with-optchat-lock
    (fold-to (length *entries*))
    (when (plusp (length *entries*))
      (format nil "[optchat memory: ~d messages remembered; older lines are coarser summaries - zoom before acting on one]~%~a"
              (length *entries*) (render-view)))))

(defun view-section (next session-id)
  "Advice on NLE::READ-HARNESS-SECTIONS: the view of the log rides every
request as the `optchat' block, so a fresh session starts knowing the past."
  (let ((sections (remove "optchat" (funcall next session-id) :key #'car :test #'string=)))
    (let ((text (and session-id (remembered-block))))
      (if text
          (acons "optchat" text sections)
          sections))))

;;; --- what a turn writes into the log -------------------------------------------------

(defun poke-worker ()
  "Ask the summary worker to look at the log soon, when one runs."
  (when *worker* (nlk:worker-poke *worker*)))

(defun observe-frame (op)
  "Fold one published frame into the log: the operator's ask, the agent's
replies, its tool calls and their results. A system row is the harness's,
not the chat, and is never logged."
  (nlk:bind (((type fact _ _ turn-id) (nlk:frame-fact op)))
    (declare (ignore turn-id))
    (let ((session (or (getf op :session-id) "")))
      (when type
        (cond
          ((equal type "turn.input_committed")
           (let ((text (nlk:json-value fact :string "message")))
             (when (and text (plusp (length (nlk:trimmed text))))
               (remember "user" text session)
               (poke-worker))))
          ((equal type "turn.assistant_message_completed")
           (let ((text (nlk:fact-message-content fact)))
             (when text
               (remember "talk" text session)
               (poke-worker))))
          ((equal type "turn.tool_call_started")
           (let ((name (nlk:json-value fact :string "tool-name"))
                 (arguments (nlk:json-value fact :string "arguments")))
             (when name
               (remember "tool" (format nil "~a(~a)" name (or arguments "")) session)
               (poke-worker))))
          ((equal type "turn.tool_result")
           (let ((result (nlk:json-value fact :string "result")))
             (when (and result (plusp (length result)))
               (remember "echo" (cap-echo result) session)
               (poke-worker)))))))))

;;; --- the two tools ---------------------------------------------------------------

(nlk:define-contract zoom-line
  (:doc "Open one line of your long-term memory into the two lines it was made from; n = 1 gives the message in full. Read the id and n off a view line (2184+8), and zoom before acting on a summary: what a line omits is lost above it.")
  ("id" :integer :required t
        :doc "the first message id of the line, as the view shows it")
  ("n" :integer :required t
        :doc "how many messages the line covers: 1, 2, 4, 8, ... as the view shows it"))

(nlk:define-contract dated-line
  (:doc "When a message in your long-term memory was written: the date and time of message id.")
  ("id" :integer :required t
        :doc "the message id, as a view line shows it"))

(defun tool-zoom (call)
  "The optchat_zoom tool."
  (zoom (zoom-line-id call) (zoom-line-n call)))

(defun tool-date (call)
  "The optchat_date tool."
  (message-date (dated-line-id call)))

;;; --- /optchat ---------------------------------------------------------------------

(defun status ()
  "One line: what is remembered, what is summarized, how the view fits."
  (format nil "optchat: ~d messages remembered, ~d summary lines built, view ~d lines in ~d of ~d characters, compaction ~:[off~;on~]~@[ - ~a~]"
          (remembered) (node-count) (view-lines) (view-size) *budget*
          *compaction* *last*))

(defun panel ()
  "The dialog bare /optchat answers."
  (nle:list-dialog
   :title "OptChat" :summary (status)
   :rows (list (nle:ask-row "Open a line" "Line" "/optchat zoom"
                             :placeholder "12 4" :verb "zoom"
                             :detail "id and span, as the view shows them: n = 1 gives the message in full")
               (nle:run-row "How much is remembered" "/optchat status"
                             :detail "the log, the summary tree and the view"))))

(defun line-number (text)
  "TEXT as an integer, or NIL when it is not one."
  (and text (parse-integer text :junk-allowed t)))

(defun run-slash (args session-id)
  "The answer to one /optchat invocation: its status and panel, a zoom, or a date."
  (declare (ignore session-id))
  (let ((words (nlk:split-words (or args ""))))
    (nlk:dispatch (or (first words) "") string=
      ("" (values (status) (panel)))
      ("status" (values (status) (panel)))
      ("zoom" (if (and (second words) (third words) (line-number (second words)) (line-number (third words)))
                  (zoom (line-number (second words)) (line-number (third words)))
                  (values "optchat: zoom wants an id and a span, as a view line shows them (/optchat zoom 12 4)" (panel))))
      ("date" (if (and (second words) (line-number (second words)))
                  (message-date (line-number (second words)))
                  (values "optchat: date wants a message id (/optchat date 12)" (panel))))
      (t (values (format nil "optchat: ~a is not one of its choices (date ID, status, zoom ID N)"
                         (first words))
                 (panel))))))

;;; --- the entry -------------------------------------------------------------------

(defun install ()
  "What the declaration's clauses do not carry: the log and the tree read
from the folder, the view folded to what they hold, and the summary worker
running."
  (clear-state)
  (nle:on-stop #'clear-state)
  (setf *directory* (or (setting :store-dir) "optchat")
        *budget* (setting :view-budget-chars)
        *node-size* (setting :node-bytes)
        *compaction* (setting :compact-enabled))
  (handler-case (progn (load-log) (load-tree))
    (error (condition)
      (nlk:config-error "optchat cannot read its folder under the home (~a): ~a"
                        *directory* condition)))
  (fold-to (length *entries*))
  (when (and *compaction* *use-worker*)
    (setf *worker* (nlk:worker-start "optchat-summaries" #'compactor-step :wake 5))
    (nle:on-stop (lambda () (setf *worker* (nlk:worker-stop *worker*)))))
  (setf *last* (format nil "~d messages remembered" (length *entries*))))

(nle:define-cell optchat
  (:section ("optchat")
    (:guide "nothing here is required: the log and its summaries live under the home in store_dir; view_budget_chars is how much of every request the past may take; next step: /optchat, then optchat_zoom on a line it shows")
    ("store_dir" :string :default "optchat"
                 :doc "the folder under the home this memory lives in; one log per folder")
    ("view_budget_chars" :integer :default 16000 :min 2000
                         :doc "characters of each request the view of the past may take")
    ("node_bytes" :integer :default 512 :min 128
                  :doc "characters one summary line may take (the OptChat NODE)")
    ("compact_enabled" :boolean :default t
                       :doc "summarize in the background, one side call per long message or merge"))
  (:tool "optchat_zoom" (nlk:contract 'zoom-line) 'tool-zoom)
  (:tool "optchat_date" (nlk:contract 'dated-line) 'tool-date)
  (:command "optchat" 'run-slash
            :description "Long-term memory: what is remembered, and a line opened")
  (:help :optchat "optchat:zoom opens a line of the operator's long-term memory; message-date dates one" +primer+)
  (:hook :frame (nlk:observer #'observe-frame "optchat"))
  (:hook 'nle::read-harness-sections #'view-section)
  (:start #'install))
