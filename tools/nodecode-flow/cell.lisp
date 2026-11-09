;;;; cell.lisp --- /flow, two tools, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a subcommand over a recorded session store: lineage,
;;;; handoffs and tool calls as a terminal tree and an offline page, plus a
;;;; trusted-paths baseline later runs are compared with. It read the
;;;; harness's own state.db in a read-only transaction and read no message
;;;; body. Nodecode's equivalent store is its event log, read here through
;;;; NLK:LIST-SESSIONS and NLK:EVENTS (store/sessions.lisp, store/events.lisp):
;;;; SESSION.CREATED carries the parent edge, TURN.TOOL_CALL_STARTED and
;;;; TURN.TOOL_RESULT carry the calls. Nothing is dialled and nothing leaves
;;;; the machine.

(in-package #:nodecode-flow)

;;; --- the manual ------------------------------------------------------------------

(defparameter +primer+
  "A flow is what the recorded sessions did, read from this machine's own event log.
  /flow                      the newest sessions, one a line, with their call counts
  /flow show SESSION         that session and its descendants as a tree: each
                             agent's lineage or handoff, its tools with counts,
                             failures and time
  /flow mermaid [SESSION]    the same as a mermaid flowchart to paste elsewhere
  /flow baseline [PATH]      write PATH (flow/trusted-paths.json under this home
                             by default) as the connections a later run is
                             compared with; review and edit it, it is only JSON
  /flow compare [PATH]       every connection the flow has that PATH does not
                             (a new handoff or a tool an agent never used) and
                             every trusted one it no longer has
  /flow report [PATH]        one self-contained HTML page of the flow, no
                             external resources
  SESSION is an id from /flow; the newest recorded session is the default.
Nothing here sends anything anywhere: the log is local, and the report is a
file you name.")

;;; --- where a baseline lives when the command names none --------------------------

(defun default-baseline ()
  (let ((configured (setting :baseline)))
    (if (and (stringp configured) (plusp (length configured)))
        configured
        (namestring (nlk:home "flow/trusted-paths.json")))))

(defun default-report ()
  (namestring (nlk:home "flow/flow.html")))

;;; --- the command ------------------------------------------------------------------

(defun command-text (text)
  "TEXT, the argument after /flow, answered as text."
  (let* ((words (remove "" (uiop:split-string (or text "") :separator '(#\Space #\Tab))
                        :test #'string=))
         (verb (string-downcase (or (first words) ""))))
    (cond
      ((member verb '("" "help" "?") :test #'string=) +primer+)
      ((member verb '("sessions" "list") :test #'string=) (sessions-text))
      ((member verb '("show" "tree") :test #'string=) (tree-text :session-id (second words)))
      ((string= verb "mermaid") (mermaid-text :session-id (second words)))
      ((string= verb "baseline")
       (baseline (or (second words) (default-baseline)) :session-id (third words)))
      ((string= verb "compare")
       (compare-baseline (or (second words) (default-baseline)) :session-id (third words)))
      ((string= verb "report")
       (report (or (second words) (default-report)) :session-id (third words)
               :baseline-path nil))
      (t (tree-text :session-id (first words))))))

(defun run-slash (args session-id)
  "The /flow command: TEXT, and no dialog."
  (declare (ignore session-id))
  (handler-case (command-text (nlk:one-line (or args "")))
    (error (condition) (values (format nil "flow: ~a" condition) nil))))

;;; --- the two tools ----------------------------------------------------------------

(nlk:define-contract flow-tree
  (:doc "Read this machine's recorded agent flows: one session and every session it spawned or forked, each with the tools it called, how often, which failed and how long they took. Call it when a person asks what agents ran, how one session led to another, or which tools a session used. Nothing leaves the machine and no message body is read.")
  ("session_id" :string :default ""
                :doc "the session whose flow to show; empty is the newest recorded one"))

(nlk:define-contract flow-compare
  (:doc "Compare a recorded agent flow with a trusted-paths baseline a person reviewed: every connection the flow has that the baseline does not (a new handoff, a tool an agent never used) and every trusted connection no longer observed. Make the baseline first with /flow baseline PATH.")
  ("baseline" :string :required t
              :doc "path to the trusted-paths JSON a person reviewed")
  ("session_id" :string :default ""
                :doc "the session to compare; empty is the newest recorded one"))

(defun tool-tree (call)
  (handler-case (tree-text :session-id (let ((id (flow-tree-session-id call)))
                                        (and id (plusp (length id)) id)))
    (error (condition) (format nil "flow: ~a" condition))))

(defun tool-compare (call)
  (handler-case
      (multiple-value-bind (text new missing)
          (compare-baseline (flow-compare-baseline call)
                            :session-id (let ((id (flow-compare-session-id call)))
                                          (and id (plusp (length id)) id)))
        (declare (ignore new missing))
        text)
    (error (condition) (format nil "flow: ~a" condition))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell flow
  (:section ("flow")
    (:guide "nothing is required: flow reads the recorded sessions and their tool calls. max_sessions bounds a listing, max_calls the tool rows under one agent, baseline is the trusted-paths JSON /flow compare uses when it names none. Next step: /flow")
    ("max_sessions" :integer :default 20 :min 1
                    :doc "sessions a /flow listing shows")
    ("max_calls" :integer :default 12 :min 1
                 :doc "tool rows shown under one agent")
    ("baseline" :string :default ""
                :doc "trusted-paths JSON compared when /flow compare names none; empty means flow/trusted-paths.json under this home"))
  (:help :flow "flow: recorded agent flows, their handoffs and tools, compared with trusted paths - /flow [show|mermaid|baseline|compare|report] [SESSION] [PATH]" +primer+)
  (:command "flow" 'run-slash
            :description "Recorded sessions as a tree of lineage, handoffs and tool use, and the connections a trusted-paths baseline does not have"
            :argument-hint "[show SESSION | mermaid [SESSION] | baseline [PATH] | compare [PATH] | report [PATH]]")
  (:tool "flow_tree" (nlk:contract 'flow-tree) 'tool-tree)
  (:tool "flow_compare" (nlk:contract 'flow-compare) 'tool-compare))
