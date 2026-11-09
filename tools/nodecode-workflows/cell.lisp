;;;; cell.lisp --- the primer, the verbs, the tool, /workflow, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model authors a JSON graph and hands it to the `workflow' tool; one
;;;; worker thread runs it. A node is one lane: an agent turn in a child
;;;; session of its own, a constant (echo), or a gate that holds the run for
;;;; the operator's answer. AFTER orders nodes and feeds each ancestor's
;;;; committed output in under "## Inputs"; INPUTS pick a dotted path;
;;;; FANOUT runs one instance a item; FROM consolidates a whole fan-out.
;;;; Every output lands in the run's folder, so a run interrupted is resumed
;;;; where it stopped, never from the top.

(in-package #:nodecode-workflows)

;;; --- the manual ------------------------------------------------------------------
;;; (help :workflow) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter +primer+
  "Workflow graphs are available through the nodecode-workflows cell. A graph is JSON:
{\"name\":\"check\
 \"nodes\":[{\"id\":\"inspect\",\"type\":\"agent\",\"goal\":\"Inspect the target and answer with one JSON object.\"}
          {\"id\":\"report\",\"type\":\"agent\",\"goal\":\"Write the report.\",\"after\":[\"inspect\"]}]}
Call the `workflow' tool: workflow{action:\"run\",graph:{...}} starts it in the background -- END YOUR TURN
afterwards; one line wakes this session when the run settles. `status', `show', `stop', `answer' and
`list' read or move a run; `validate' lints a graph with no writes. The same verbs are Lisp:
(workflows:start GRAPH), (workflows:status \"id\"), (workflows:show \"id\" \"node\"), (workflows:stop \"id\"),
(workflows:answer \"id\" \"node\" \"text\"), (workflows:runs), (workflows:validate GRAPH). /workflow does
what the tool does, from the keyboard.

A node: \"id\" (unique, lowercase, . _ -), \"type\" (agent, echo or gate), \"goal\" (an agent's one job),
\"echo\" (a constant), \"question\"+\"options\" (a gate). Edges and data: \"after\":[\"id\"] orders a node and
feeds each direct ancestor's committed output in under ## Inputs; \"inputs\":[\"a.key\"] reads a dotted
path from any ancestor; \"requires\":[\"a.key\"] fails the node at spawn when that path is missing;
\"fanout\":{\"items\":[...]} or {\"items_from\":\"a.items\"} runs one lane an item, ids \"node.1\", \"node.2\";
\"from\":\"fan\" hands a consolidate node the whole array; \"schema\" (a JSON object) writes the reply
contract into the prompt. Independent work: separate nodes, or one fan-out. A node answers with one
fenced JSON object when its dependents read paths off it. \"budget_seconds\" caps one node's turn.
A gate holds the run until workflow{action:\"answer\",run_id,node_id,answer} or /workflow answer.

Every node's answer is written to its run's folder; a node that committed is never run again, so a
run resumed after a restart picks up where it stopped. Nothing a node does reaches this session until
the run settles: do not poll for it -- end your turn."
  "What (help :workflow) answers while the cell runs.")

(defparameter *worker* nil
  "The one thread that advances runs; stopped with the cell.")

;;; --- reading what a caller handed in --------------------------------------------------

(defun read-graph-file (path)
  "The graph the JSON file at PATH holds, or a refusal naming why not."
  (let ((path (nlk:expand-home path)))
    (unless (stringp path)
      (fail "a graph_path is a string"))
    (unless (probe-file path)
      (fail "no file at ~a" path))
    (let ((json (ignore-errors (nlk:decode-json (nlk:read-text path)))))
      (unless (hash-table-p json)
        (fail "~a does not hold one JSON object" path))
      json)))

(defun graph-argument (arguments)
  "The graph a tool call carries, as a decoded object."
  (let ((object (nlk:json-value arguments :object "graph"))
        (path (nlk:json-value arguments :string "graph_path")))
    (cond ((and object path) (fail "give a graph or a graph_path, not both"))
          (object object)
          (path (read-graph-file path))
          (t (fail "a run needs a \"graph\" object or a \"graph_path\"")))))

;;; --- the verbs ---------------------------------------------------------------------

(define-verb start (graph &key name origin)
  "Validate GRAPH and start it in the background. => what was started."
  (let ((json (if (stringp graph) (read-graph-file graph) graph)))
    (multiple-value-bind (parsed problems) (parse-graph json)
      (when problems (fail "~a" (problems-text problems)))
      (let* ((name (or name (graph-name parsed)))
             (origin (or origin nlk:*scribe-session-id*))
             (id (nlk:unused-name
                  (slug name)
                  (lambda (candidate)
                    (or (find-run candidate) (probe-file (run-dir candidate))))))
             (run (make-wrun :id id :name name :status "running"
                             :created-at (nlk:iso-now) :origin origin
                             :raw json :graph parsed)))
        (with-workflows-lock (push run *runs*))
        (save-run run)
        (nlk:worker-poke *worker*)
        (format nil "workflow ~a started: ~d node~:p~@[ and its fan-outs~]. It runs in the background: ~
end your turn and one line wakes this session when it settles; workflow{action:\"status\",run_id:\"~a\"} ~
reads it sooner, and its outputs are under workflows/~a/."
                id (length (graph-nodes parsed))
                (loop for node across (graph-nodes parsed)
                      thereis (or (node-items node) (node-items-from node)))
                id id)))))

(define-verb status (id)
  "One run: its state, every node, and what to do next."
  (let ((run (find-run id)))
    (unless run (fail "no run is called ~s: (workflows:runs) lists them" id))
    (with-output-to-string (out)
      (format out "workflow ~a \"~a\": ~a, started ~a~%~{~a~%}~}"
              (wrun-id run) (wrun-name run) (wrun-status run) (wrun-created-at run)
              (mapcar (lambda (node-id) (status-line run node-id)) (node-ids run)))
      (let ((held (loop for node-id in (node-ids run)
                        when (string= (node-state run node-id) "held") collect node-id)))
        (cond (held
               (format out "~%next: answer the gate~p ~{~s~^, ~}: workflow{action:\"answer\",run_id:\"~a\",node_id,answer} ~
 or /workflow answer ~a NODE TEXT" (length held) held (wrun-id run) (wrun-id run)))
              ((string= (wrun-status run) "running")
               (format out "~%next: it runs on its own; one line wakes this session when it settles"))
              (t (format out "~%done: workflow{action:\"show\",run_id:\"~a\",node_id} reads a node's output"
                         (wrun-id run))))))))

(define-verb runs ()
  "Every run this home holds, newest first."
  (if (null *runs*)
      "no workflow run yet: workflow{action:\"run\",graph:{...}} starts one"
      (format nil "~{~a~^~%~}"
              (loop for run in *runs*
                    collect (format nil "~a  ~a  \"~a\"  ~d/~d done  ~a"
                                    (wrun-id run) (wrun-status run) (wrun-name run)
                                    (count-if (lambda (id) (string= (node-state run id) "done"))
                                              (node-ids run))
                                    (length (node-ids run)) (wrun-created-at run))))))

(define-verb stop (id)
  "Stop RUN: nothing new starts; what committed stays committed."
  (let ((run (find-run id)))
    (unless run (fail "no run is called ~s" id))
    (cond ((not (string= (wrun-status run) "running"))
           (format nil "workflow ~a is already ~a" id (wrun-status run)))
          (t (setf (wrun-status run) "stopped")
             (save-run run)
             (format nil "workflow ~a stopped: ~d node~:p committed, the rest will not run"
                     id (count-if (lambda (node-id) (string= (node-state run node-id) "done"))
                                  (node-ids run)))))))

(define-verb answer (id node-id text)
  "Answer the gate NODE-ID of RUN, and let the run go on."
  (let* ((run (find-run id))
         (node (and run (node-def run node-id))))
    (unless run (fail "no run is called ~s" id))
    (unless node (fail "workflow ~a has no node ~s" id node-id))
    (unless (eq (node-type node) :gate)
      (fail "node ~s of workflow ~a is an ~(~a~), not a gate" node-id id (node-type node)))
    (unless (string= (node-state run node-id) "held")
      (fail "node ~s of workflow ~a is ~a, not holding for an answer"
            node-id id (node-state run node-id)))
    (with-workflows-lock
      (commit run node-id text)
      (save-run run)
      (nlk:worker-poke *worker*))
    (format nil "workflow ~a: ~s answered, and the run goes on" id node-id)))

(define-verb show (id node-id)
  "What node NODE-ID of RUN committed."
  (let ((run (find-run id)))
    (unless run (fail "no run is called ~s" id))
    (unless (node-def run node-id) (fail "workflow ~a has no node ~s" id node-id))
    (let ((text (run-output run node-id)))
      (cond ((null text) (format nil "workflow ~a, node ~s: nothing committed yet (~a)"
                                  id node-id (node-state run node-id)))
            (t (format nil "workflow ~a, node ~s: ~a~%~a" id node-id (node-state run node-id)
                       (nlk:clip text (setting :output-chars))))))))

(define-verb validate (graph)
  "Lint a graph with no writes: every problem in one answer."
  (let ((json (if (stringp graph) (read-graph-file graph) graph)))
    (multiple-value-bind (parsed problems) (parse-graph json)
      (if problems
          (format nil "refused:~%~a" (problems-text problems))
          (format nil "ok: ~d node~:p~@[, ~d fan-out~:p~]"
                  (length (graph-nodes parsed))
                  (length (remove-if-not (lambda (node)
                                           (or (node-items node) (node-items-from node)))
                                         (coerce (graph-nodes parsed) 'list))))))))

;;; --- the tool ------------------------------------------------------------------------

(defparameter +tool-schema+
  (list :description
        "Run an agent-authored workflow graph in the background. A graph is JSON: {\"name\", \"nodes\":[{\"id\",\"type\":\"agent\",\"goal\",\"after\":[ids],\"inputs\":[\"node.path\"],\"fanout\":{\"items\"|\"items_from\"},\"from\":\"fan\",\"schema\":{},\"budget_seconds\"}]}. Nodes are agent turns, echoes (\"echo\") or gates (\"question\"); after-edges order them and feed each ancestor's committed output in, and a fan-out runs one lane an item. run returns at once -- end your turn and one line wakes this session when the run settles; status, show, list, stop, answer and validate read or move a run. gate answers arrive with answer."
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "action" (nlk:make-json-object
                    "type" "string"
                    "enum" (vector "run" "validate" "status" "list" "show" "stop" "answer")
                    "description" "run or validate a graph; status, list, show, stop, answer a run")
          "graph" (nlk:make-json-object "type" "object"
                                         "description" "the graph itself, for run and validate")
          "graph_path" (nlk:make-json-object
                         "type" "string"
                         "description" "a file holding the graph, for run and validate, instead of graph")
          "name" (nlk:make-json-object "type" "string"
                                        "description" "the run's name, over the graph's own")
          "run_id" (nlk:make-json-object "type" "string" "description" "which run: the id run answered")
          "node_id" (nlk:make-json-object "type" "string"
                                           "description" "which node: for show and for a gate's answer")
          "answer" (nlk:make-json-object "type" "string"
                                          "description" "the operator's answer to a gate"))
         "required" (vector "action")))
  "What the model sees of the workflow tool, and what a call is held to.")

(defun workflow-tool (arguments)
  "The `workflow' tool: the verbs over a decoded arguments table."
  (flet ((field (key) (nlk:json-value arguments :string key))
         (needed (key)
           (or (nlk:json-value arguments :string key)
               (fail "action ~s needs \"~a\"" (field "action") key))))
    (handler-case
        (let ((action (string-downcase (or (field "action") "status"))))
          (cond ((string= action "run")
                 (start (graph-argument arguments) :name (field "name")
                                                    :origin (getf (nle:turn) :session-id)))
                ((string= action "validate") (validate (graph-argument arguments)))
                ((string= action "status") (status (needed "run_id")))
                ((string= action "list") (runs))
                ((string= action "show") (show (needed "run_id") (needed "node_id")))
                ((string= action "stop") (stop (needed "run_id")))
                ((string= action "answer") (answer (needed "run_id") (needed "node_id")
                                                    (needed "answer")))
                (t (fail "action ~s is none of run, validate, status, list, show, stop, answer" action))))
      (workflows-error (condition) (format nil "ERROR: ~a" condition)))))

;;; --- /workflow --------------------------------------------------------------------

(defun split-words (text)
  "TEXT's words, in order."
  (loop for start = 0 then (1+ at)
        for at = (position #\Space text :start start)
        collect (subseq text start at)
        while at))

(defun run-slash (args session-id)
  "/workflow alone: every run and a panel over them. /workflow status ID, show ID NODE,
run PATH, stop ID, answer ID NODE TEXT. => the answer's text, and the panel."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (words (split-words text))
         (head (string-downcase (or (first words) "")))
         (first-arg (second words))
         (second-arg (third words))
         (rest-text (nlk:trimmed (format nil "~{~a~^ ~}" (nthcdr 3 words)))))
    (cond ((string= text "") (values (runs) (run-panel)))
          ((member head '("list" "runs") :test #'string=) (values (runs) (run-panel)))
          ((string= head "status") (values (status (or first-arg (first-run-id))) (run-panel)))
          ((string= head "stop") (values (stop (or first-arg (first-run-id))) (run-panel)))
          ((string= head "run") (values (start (or first-arg (fail "run needs the path of a graph file")))
                                         (run-panel)))
          ((string= head "show")
           (values (show (or first-arg (first-run-id))
                         (or second-arg (fail "show needs a node id")))
                   (run-panel)))
          ((string= head "answer")
           (values (answer (or first-arg (first-run-id))
                           (or second-arg (fail "answer needs a node id"))
                           rest-text)
                   (run-panel)))
          (t (values (status head) (run-panel))))))

(defun first-run-id ()
  (or (wrun-id (first *runs*)) (fail "no workflow run yet")))

(defun run-panel ()
  (nle:list-dialog
   :title "Workflows"
   :summary (format nil "~d run~:p" (length *runs*))
   :rows (append
          (loop for run in *runs*
                collect (nle:run-row (format nil "~a  ~a" (wrun-id run) (wrun-status run))
                                     (format nil "/workflow status ~a" (wrun-id run))
                                     :detail (format nil "\"~a\", ~d of ~d done"
                                                     (wrun-name run)
                                                     (count-if (lambda (id)
                                                                 (string= (node-state run id) "done"))
                                                               (node-ids run))
                                                     (length (node-ids run)))))
          (list (nle:ask-row "Run a graph file" "Path" "/workflow run"
                             :placeholder "a JSON graph" :verb "run")))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell workflows
  (:section ("workflows")
    (:guide "Graphs run in the background: max_parallel lanes at once, node_timeout_seconds a lane's ceiling, tick_seconds how often the worker looks. Nothing here is required")
    ("max_parallel" :integer :default 3 :min 1
                    :doc "how many nodes of a run may be in flight at once")
    ("tick_seconds" :integer :default 2 :min 1
                    :doc "how often the worker advances the live runs")
    ("node_timeout_seconds" :integer :default 900 :min 1
                             :doc "how long one node's turn may run before it counts as failed")
    ("output_chars" :integer :default 4000 :min 200
                     :doc "how much of an ancestor's output a node is shown, and show answers"))
  (:tool "workflow" +tool-schema+ 'workflow-tool)
  (:help :workflow "workflow: an agent-authored DAG run in the background -- nodes, after-edges, fan-outs, gates, resume; /workflow" +primer+)
  (:command "workflow" 'run-slash
            :catches 'workflows-error
            :description "Workflow graphs: list runs, status ID, show ID NODE, run PATH, stop ID, answer ID NODE TEXT"
            :argument-hint "status | list | run PATH | stop ID | show ID NODE | answer ID NODE TEXT")
  (:start (lambda ()
            (load-runs)
            (setf *worker* (nlk:worker-start "workflows" #'tick
                                             :wake (lambda () (setting :tick-seconds))
                                             :prime t))
            (nle:on-stop (lambda ()
                           (setf *worker* (nlk:worker-stop *worker*))
                           (setf *runs* '()))))))
