;;;; model.lisp --- the recorded sessions, folded into one flow.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One read of Nodecode's own event log: a session is an agent, a parent
;;;; link is the edge that made it -- an anchored fork composes its parent's
;;;; history, a child spawned by another session begins fresh -- and the log's
;;;; turn.tool_call_started / turn.tool_result facts are the calls it made.
;;;; Nothing is inferred from adjacent timestamps, nothing is dialled and
;;;; nothing leaves the machine.

(in-package #:nodecode-flow)

;;; --- the shape -----------------------------------------------------------------

(defparameter +tool-kinds+ '("turn.tool_call_started" "turn.tool_result")
  "The two facts one call leaves: the request, and how it went.")

(defparameter +identity+ "exact-agent-label"
  "What a baseline matches on. One label is one agent, as in the original.")

(defstruct (call (:constructor make-call (tool)))
  (tool "" :type string)
  (count 0)
  (failed 0)
  (ms 0))

(defstruct (agent (:constructor make-agent (&key id label parent relation created updated)))
  (id "" :type string)
  (label "" :type string)
  (parent nil)
  (relation nil)
  (created nil)
  (updated nil)
  (calls nil))

(defstruct (flow-report (:constructor make-flow-report (&key agents root note)))
  (agents nil)
  (root nil)
  (note "Recorded lineage and tool use; an edge does not prove data transfer."))

;;; --- the log, read once ---------------------------------------------------------

(defun tool-facts ()
  "Every tool fact in the log, oldest first, as (SESSION TOOL KIND FAILED MS)."
  (loop for (session kind payload) in (nlk:events :kind +tool-kinds+ :as :rows
                                                  :columns '("session_id" "kind"
                                                             "cast(payload as text)")
                                                  :order :oldest)
        for object = (and payload
                           (handler-case (nlk:decode-json payload) (error () nil)))
        for tool = (or (and object (nlk:json-value object :string "tool-name")) "")
        unless (zerop (length tool))
          collect (list session tool kind
                        (and object (nlk:json-value object :boolean "failed"))
                        (and object (nlk:json-value object :integer "duration-ms")))))

(defun calls-by-session (facts)
  "FACTS folded into each session's calls, most-called first."
  (let ((calls (make-hash-table :test #'equal))
        (by-session (make-hash-table :test #'equal)))
    (loop for (session tool kind failed ms) in facts
          for call = (or (gethash (list session tool) calls)
                         (setf (gethash (list session tool) calls) (make-call tool)))
          do (if (string= kind "turn.tool_call_started")
                 (incf (call-count call))
                 (progn (when failed (incf (call-failed call)))
                        (when (and ms (integerp ms)) (incf (call-ms call) ms)))))
    (loop for (session nil) being the hash-keys of calls using (hash-value call)
          do (push call (gethash session by-session)))
    (loop for session being the hash-keys of by-session
          do (setf (gethash session by-session)
                   (stable-sort (gethash session by-session) #'> :key #'call-count)))
    by-session))

(defun label-of (session)
  "The one-line name SESSION is known by: its title, else its id."
  (let ((title (nlk:one-line (or (getf session :title) ""))))
    (if (zerop (length title)) (getf session :id) title)))

(defun when-text (iso)
  "ISO's date and minute, for a listing; NIL reads as a dash."
  (if (and (stringp iso) (>= (length iso) 16) (char= (char iso 10) #\T))
      (format nil "~a ~a" (subseq iso 0 10) (subseq iso 11 16))
      "-"))

;;; --- which sessions, in which order ---------------------------------------------

(defun children-index (sessions)
  (let ((index (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (let ((parent (getf session :parent)))
        (when parent (push session (gethash parent index)))))
    index))

(defun roots-of (sessions)
  "SESSIONS with no parent among them, newest first."
  (let ((known (make-hash-table :test #'equal)))
    (dolist (session sessions) (setf (gethash (getf session :id) known) t))
    (remove-if (lambda (session)
                 (let ((parent (getf session :parent)))
                   (and parent (gethash parent known))))
               sessions)))

(defun tree-of (sessions root)
  "ROOT and every recorded descendant, depth first, each once."
  (let ((children (children-index sessions))
        (seen (make-hash-table :test #'equal))
        (order '()))
    (labels ((walk (session)
               (unless (gethash (getf session :id) seen)
                 (setf (gethash (getf session :id) seen) t)
                 (push session order)
                 (dolist (child (gethash (getf session :id) children))
                   (walk child)))))
      (walk root)
      (nreverse order))))

(defun build-report (&key session-id (sessions (nlk:list-sessions)) (facts (tool-facts)))
  "The flow of SESSION-ID and its descendants, or of the newest roots."
  (when (null sessions)
    (error "flow: no recorded session yet -- one turn is enough to record one"))
  (let* ((by-session (calls-by-session facts))
         (root (and session-id
                     (or (find session-id sessions :test #'string= :key (lambda (s) (getf s :id)))
                         (error "flow: no recorded session ~a" session-id))))
         (selected (if root
                       (tree-of sessions root)
                       (loop for candidate in (roots-of sessions)
                             for taken = 0 then (+ taken (length (tree-of sessions candidate)))
                             while (< taken (setting :max-sessions))
                             append (tree-of sessions candidate))))
         (agents (loop for session in selected
                       for id = (getf session :id)
                       for parent = (getf session :parent)
                       collect (make-agent :id id :label (label-of session)
                                           :parent parent
                                           :relation (cond ((null parent) nil)
                                                           ((getf session :anchor-turn-id) "lineage")
                                                           (t "handoff"))
                                           :created (getf session :created-at)
                                           :updated (getf session :updated-at)))))
    (dolist (agent agents)
      (setf (agent-calls agent) (gethash (agent-id agent) by-session)))
    (make-flow-report :agents agents :root (and root (getf root :id)))))

(defun total-calls (report)
  (loop for agent in (flow-report-agents report)
        sum (loop for call in (agent-calls agent) sum (call-count call))))

(defun handoff-count (report)
  (count-if (lambda (agent) (equal "handoff" (agent-relation agent)))
            (flow-report-agents report)))

;;; --- the terminal tree -----------------------------------------------------------

(defun agent-children (agents)
  "AGENTS as (values ROOTS CHILDREN), ROOTS newest first."
  (let ((by-id (make-hash-table :test #'equal))
        (kids (make-hash-table :test #'equal))
        (roots '()))
    (dolist (agent agents) (setf (gethash (agent-id agent) by-id) agent))
    (dolist (agent agents)
      (if (and (agent-parent agent) (gethash (agent-parent agent) by-id))
          (push agent (gethash (agent-parent agent) kids))
          (push agent roots)))
    (values (nreverse roots) kids)))

(defun connections-key (source kind target)
  (format nil "~a|~a|~a" source kind target))

(defun agent-key (agent &optional aliases)
  (format nil "agent:~a" (or (and aliases (gethash (agent-label agent) aliases))
                              (agent-label agent))))

(defun mark-of (deviations key)
  (if (and deviations (member key deviations :test #'string=))
      " [NEW CONNECTION]"
      ""))

(defvar *indent* ""
  "The tree indentation of the agent being written.")

(defun render-agent (agent parent kids deviations out)
  "AGENT's tool rows and its children, wrote to OUT under the tree."
  (declare (ignore parent))
  (let ((shown (setting :max-calls))
        (calls (agent-calls agent)))
    (loop for call in calls
          for n from 1
          when (<= n shown)
            do (format out "~a· ~a x~d~@[ · ~d failed~]~@[ · ~,1fs~]~a~%"
                       *indent* (call-tool call) (call-count call)
                       (and (plusp (call-failed call)) (call-failed call))
                       (and (plusp (call-ms call)) (/ (call-ms call) 1000.0))
                       (mark-of deviations (connections-key (agent-key agent) "uses"
                                                            (format nil "tool:~a" (call-tool call))))))
    (when (> (length calls) shown)
      (format out "~a… ~d more tools; see the report~%" *indent* (- (length calls) shown)))
    (let ((children (reverse (gethash (agent-id agent) kids))))
      (loop for child in children
            for n from 0
            do (let ((*indent* (concatenate 'string *indent* (if (= n (1- (length children))) "    " "│   "))))
                 (format out "~a~a~a [~a]~@[ · ~a~]~@[ · updated ~a~]~a~%"
                         *indent* (if (= n (1- (length children))) "└── " "├── ")
                         (agent-label child) (agent-id child) (agent-relation child)
                         (when-text (agent-updated child))
                         (mark-of deviations
                                  (connections-key (agent-key agent)
                                                   (or (agent-relation child) "lineage")
                                                   (agent-key child))))
                 (render-agent child agent kids deviations out))))))

(defun render-tree (report out &optional deviations)
  "REPORT as the tree the original printed; DEVIATIONS marks new edges."
  (multiple-value-bind (roots kids) (agent-children (flow-report-agents report))
    (let ((*indent* ""))
      (loop for root in roots
            for n from 0
            do (format out "~a~a~a [~a]~@[ · updated ~a~]~%"
                       "" (if (= n (1- (length roots))) "└── " "├── ")
                       (agent-label root) (agent-id root) (when-text (agent-updated root)))
               (let ((*indent* (if (= n (1- (length roots))) "    " "│   ")))
                 (render-agent root nil kids deviations out))))))

(defun sessions-text (&key (limit (setting :max-sessions)))
  "The newest recorded sessions, each with its calls, newest first."
  (let* ((sessions (nlk:list-sessions))
         (by-session (calls-by-session (tool-facts)))
         (shown (subseq sessions 0 (min limit (length sessions)))))
    (if (null shown)
        "flow: no recorded session yet -- one turn is enough to record one"
        (with-output-to-string (out)
          (format out "Flow · ~d session~:p recorded~@[ (newest ~d shown)~]~%"
                  (length sessions) (and (> (length sessions) (length shown)) (length shown)))
          (dolist (session shown)
            (let ((calls (gethash (getf session :id) by-session)))
              (format out "~a [~a]~@[ · parent ~a~] · ~d call~:p · updated ~a~%"
                      (label-of session) (getf session :id) (getf session :parent)
                      (loop for call in calls sum (call-count call))
                      (when-text (getf session :updated-at)))
              (when calls
                (format out "     tools: ~{~a~^, ~}~%" (mapcar #'call-tool calls)))))))))

(defun tree-text (&key session-id)
  "One session's flow and its descendants, as the tree."
  (let ((report (build-report :session-id session-id)))
    (with-output-to-string (out)
      (format out "Flow · ~d agent~:p · ~d call~:p · ~d handoff~:p~%~a~%"
              (length (flow-report-agents report)) (total-calls report)
              (handoff-count report) (flow-report-note report))
      (render-tree report out))))

;;; --- mermaid --------------------------------------------------------------------

(defun escape-mermaid (text)
  (with-output-to-string (out)
    (loop for character across text
          do (case character
               (#\" (write-string "'" out))
               (#\[ (write-string "(" out))
               (#\] (write-string ")" out))
               (t (write-char character out))))))

(defun mermaid-text (&key session-id)
  "REPORT as a mermaid flowchart: sessions are agents, tool uses are leaves."
  (let* ((report (build-report :session-id session-id))
         (agents (flow-report-agents report))
         (ids (make-hash-table :test #'equal))
         (tools (make-hash-table :test #'equal)))
    (with-output-to-string (out)
      (format out "flowchart LR~%")
      (loop for agent in agents
            for n from 0
            do (setf (gethash (agent-id agent) ids) (format nil "a~d" n))
               (format out "  a~d[\"~a\"]~%" n (escape-mermaid (agent-label agent))))
      (dolist (agent agents)
        (let ((parent (and (agent-parent agent) (gethash (agent-parent agent) ids))))
          (when parent
            (format out "  ~a -->|~a| ~a~%" parent (or (agent-relation agent) "lineage")
                    (gethash (agent-id agent) ids)))))
      (dolist (agent agents)
        (dolist (call (agent-calls agent))
          (let ((key (call-tool call)))
            (unless (gethash key tools)
              (setf (gethash key tools) (format nil "t~d" (hash-table-count tools)))
              (format out "  ~a[(\"~a\")]~%" (gethash key tools) (escape-mermaid key)))
            (format out "  ~a -.-> ~a~%" (gethash (agent-id agent) ids) (gethash key tools))))))))

;;; --- connections and trusted paths -----------------------------------------------

(defun parent-key (agents parent &optional aliases)
  (let ((found (find parent agents :test #'string= :key #'agent-id)))
    (if found (agent-key found aliases) (format nil "unresolved-agent:~a" parent))))

(defun connections (report &optional aliases)
  "REPORT's observed connections as (SOURCE KIND TARGET) lists."
  (let ((agents (flow-report-agents report)))
    (append
     (loop for agent in agents
           when (agent-parent agent)
             collect (list (parent-key agents (agent-parent agent) aliases)
                           (or (agent-relation agent) "lineage")
                           (agent-key agent aliases)))
     (loop for agent in agents
           append (loop for call in (agent-calls agent)
                        collect (list (agent-key agent aliases) "uses"
                                      (format nil "tool:~a" (call-tool call))))))))

(defun connection-keys (rows)
  (mapcar (lambda (row) (apply #'connections-key row)) rows))

(defun baseline-object (report)
  "REPORT's connections as the JSON a person reviews and edits."
  (let ((rows (remove-duplicates (connections report) :test #'equal)))
    (nlk:json-object
     "schema" 1
     "identity" +identity+
     "agent_aliases" (nlk:json-object)
     "source_sessions" (coerce (mapcar #'agent-id (flow-report-agents report)) 'vector)
     "connections" (coerce (mapcar (lambda (row)
                                      (nlk:json-object "source" (first row)
                                                       "kind" (second row)
                                                       "target" (third row)))
                                    rows)
                            'vector))))

(defun baseline (path &key session-id)
  "Write PATH as the trusted connections of SESSION-ID's flow (or the newest)."
  (let* ((report (build-report :session-id session-id))
         (rows (remove-duplicates (connections report) :test #'equal))
         (pathname (pathname path)))
    (ensure-directories-exist pathname)
    (with-open-file (out pathname :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
      (write-string (nlk:encode-json-object (baseline-object report)) out))
    (format nil "Wrote ~d trusted connection~:p from ~d agent~:p to ~a"
            (length rows) (length (flow-report-agents report)) path)))

(defun read-baseline (path)
  "PATH read back as (values CONNECTIONS ALIASES), validating what it says."
  (let* ((text (with-open-file (in path :direction :input)
                  (let ((buffer (make-string (file-length in))))
                    (read-sequence buffer in)
                    buffer)))
         (object (handler-case (nlk:decode-json text)
                   (error ()
                     (error "flow: ~a is not a trusted-paths JSON file" path)))))
    (unless (and object
                 (eql (nlk:json-value object :integer "schema") 1)
                 (equal (nlk:json-value object :string "identity") +identity+))
      (error "flow: ~a is not a trusted-paths baseline (schema 1, ~a)" path +identity+))
    (let ((aliases (make-hash-table :test #'equal))
          (rows '()))
      (let ((object-aliases (nlk:json-value object :object "agent_aliases")))
        (when object-aliases
          (loop for name being the hash-keys of object-aliases using (hash-value value)
                when (stringp value) do (setf (gethash name aliases) value))))
      (loop for row across (nlk:json-array object "connections")
            for source = (nlk:json-value row :string "source")
            for kind = (nlk:json-value row :string "kind")
            for target = (nlk:json-value row :string "target")
            do (if (and source kind target)
                   (push (list source kind target) rows)
                   (error "flow: a baseline connection in ~a is missing source, kind or target"
                          path)))
      (values (nreverse rows) aliases))))

(defun compare-baseline (path &key session-id)
  "The connections SESSION-ID's flow has that PATH's baseline does not, and the reverse."
  (multiple-value-bind (trusted aliases) (read-baseline path)
    (let* ((report (build-report :session-id session-id))
           (keys (connection-keys (connections report aliases)))
           (trusted-keys (connection-keys trusted))
           (new (remove-duplicates
                 (remove-if (lambda (key) (member key trusted-keys :test #'string=)) keys)
                 :test #'string=))
           (missing (remove-duplicates
                     (remove-if (lambda (key) (member key keys :test #'string=)) trusted-keys)
                     :test #'string=)))
      (values
       (with-output-to-string (out)
         (format out "Trusted path comparison · ~d new connection~:p, ~d trusted connection~:p not observed~%"
                 (length new) (length missing))
         (dolist (key new) (format out "NEW     ~a~%" key))
         (dolist (key missing) (format out "MISSING ~a~%" key))
         (let ((labels (make-hash-table :test #'equal)))
           (dolist (agent (flow-report-agents report))
             (push (agent-id agent) (gethash (agent-label agent) labels)))
           (loop for label being the hash-keys of labels using (hash-value ids)
                 when (> (length ids) 1)
                   do (format out "Warning: baseline identity ~s is shared by ~d sessions; their connections are combined.~%"
                              label (length ids))))
         (when (and (null new) (null missing))
           (format out "Every observed connection is trusted, and every trusted one was observed.~%")))
       new missing))))

;;; --- the offline HTML report -------------------------------------------------------

(defun html-escape (text)
  (with-output-to-string (out)
    (loop for character across (princ-to-string text)
          do (case character
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char character out))))))

(defparameter +style+
  "body{margin:0;background:#f5f7f2;color:#193632;font:14px/1.6 system-ui,sans-serif}main{max-width:1100px;margin:auto;padding:28px}h1{font-size:24px;margin:0 0 4px}h2{font-size:17px;margin:26px 0 8px}.muted{color:#647973}.metric{display:inline-block;border:1px solid #dbe4dc;background:white;border-radius:10px;padding:10px 16px;margin:12px 10px 0 0}.metric b{font-size:22px;display:block}ul.tree{list-style:none;padding-left:18px;border-left:1px solid #dbe4dc}ul.tree>li{margin:6px 0}.who{font-weight:600}.id{font:11px monospace;color:#647973}.call{font:12px monospace;color:#33473f}.new{color:#c62828;font-weight:600}footer{margin-top:26px;font-size:12px;color:#647973}"
  "The page's whole styling: no external resource is fetched.")

(defun html-call-row (agent call deviations)
  (format nil "<li class=\"call\">~a x~d~@[ · ~d failed~]~@[ · ~,1fs~]~a</li>"
          (html-escape (call-tool call)) (call-count call)
          (and (plusp (call-failed call)) (call-failed call))
          (and (plusp (call-ms call)) (/ (call-ms call) 1000.0))
          (if (member (connections-key (agent-key agent) "uses"
                                       (format nil "tool:~a" (call-tool call)))
                      deviations :test #'string=)
              " <span class=\"new\">NEW CONNECTION</span>"
              "")))

(defun html-node (agent kids deviations)
  "AGENT and its descendants as one list item."
  (let ((children (reverse (gethash (agent-id agent) kids))))
    (with-output-to-string (out)
      (format out "<li><span class=\"who\">~a</span> <span class=\"id\">~a</span> <em>~a</em>~a"
              (html-escape (agent-label agent)) (html-escape (agent-id agent))
              (html-escape (or (agent-relation agent) "root"))
              (if (and (agent-parent agent)
                       (member (connections-key (agent-key agent) (or (agent-relation agent) "lineage")
                                                (agent-key agent))
                               deviations :test #'string=))
                  " <span class=\"new\">NEW CONNECTION</span>" ""))
      (when (agent-calls agent)
        (format out "<ul>~{~a~}</ul>"
                (mapcar (lambda (call) (html-call-row agent call deviations))
                        (agent-calls agent))))
      (when children
        (format out "<ul class=\"tree\">~{~a~}</ul>"
                (mapcar (lambda (child) (html-node child kids deviations)) children)))
      (write-string "</li>" out))))

(defun html-report (report &optional deviations)
  "REPORT as one self-contained page: the tree, its calls and the deviations."
  (multiple-value-bind (roots kids) (agent-children (flow-report-agents report))
    (with-output-to-string (out)
      (format out "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>Flow</title><style>~a</style></head><body><main>"
              +style+)
      (write-string "<h1>Flow</h1><p class=\"muted\">Recorded sessions, their handoffs and the tools they used. No transcript bodies, no external resources.</p>" out)
      (format out "<div class=\"metric\"><b>~d</b>agents</div>" (length (flow-report-agents report)))
      (format out "<div class=\"metric\"><b>~d</b>calls</div>" (total-calls report))
      (format out "<div class=\"metric\"><b>~d</b>handoffs</div>" (handoff-count report))
      (format out "<h2>Graph</h2><ul class=\"tree\">~{~a~}</ul>"
              (mapcar (lambda (root) (html-node root kids deviations)) roots))
      (format out "<footer>~a</footer></main></body></html>~%"
              (html-escape (flow-report-note report))))))

(defun report (path &key session-id baseline-path)
  "Write PATH as one self-contained HTML page of SESSION-ID's flow."
  (let* ((report (build-report :session-id session-id))
         (deviations (if baseline-path
                         (multiple-value-bind (text new missing)
                             (compare-baseline baseline-path :session-id session-id)
                           (declare (ignore text missing))
                           new)
                         '()))
         (page (html-report report deviations))
         (pathname (pathname path)))
    (ensure-directories-exist pathname)
    (with-open-file (out pathname :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
      (write-string page out))
    (format nil "Wrote ~a (~d agent~:p, ~d call~:p)"
            path (length (flow-report-agents report)) (total-calls report))))
