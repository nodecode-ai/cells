;;;; run.lisp --- a run is a folder: run.json, one output a node, and the
;;;; graph it was made from.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The folder is the record: workflows/<run-id>/ under the home. RUN.JSON
;;;; holds the graph as it was submitted, each node's state, its session and
;;;; turn, and what it said when it failed; out/<node>.txt holds what a node
;;;; committed. Everything a resume needs is there, so a run outlives the
;;;; process that started it -- and a node that committed is never run again.

(in-package #:nodecode-workflows)

;;; --- names ---------------------------------------------------------------------

(defun slug (text)
  "TEXT as a run id: lowercase letters, digits, - and _, at most 40."
  (let* ((text (string-downcase (or text "")))
         (raw (with-output-to-string (out)
                (loop for c across text
                      do (write-char (if (or (and (< (char-code c) 128) (alphanumericp c))
                                              (find c "-_"))
                                          c #\-)
                                     out))))
         (trimmed (string-trim "-" raw))
         (name (if (plusp (length trimmed)) trimmed "run")))
    (subseq name 0 (min 40 (length name)))))

(defun run-dir (run)
  "Where RUN (an id or a run) keeps its folder, under the home."
  (nlk:home (format nil "workflows/~a/" (if (stringp run) run (wrun-id run)))))

(defun output-path (run id)
  "Where the committed output of node ID in RUN is written."
  (nlk:home (format nil "workflows/~a/out/~a.txt" (if (stringp run) run (wrun-id run)) id)))

;;; --- the records ----------------------------------------------------------------

(defstruct (wrun (:constructor make-wrun))
  "One run: the graph it was submitted from, the nodes' state, what they said."
  (id "" :type string)
  (name "run" :type string)
  (status "running" :type string)
  (created-at "" :type string)
  (origin nil)
  (woke nil)
  raw
  graph
  (nodes (make-hash-table :test 'equal))
  (instances (make-hash-table :test 'equal))
  (expanded (make-hash-table :test 'equal))
  (outputs (make-hash-table :test 'equal)))

(defvar *runs* '()
  "The runs this home holds, newest first; the worker advances the ones still
running. A vector would be one more thing to hold a lock over -- pushes and
reads are under WITH-WORKFLOWS-LOCK.")

(defun find-run (id)
  "The run called ID, or NIL."
  (and (stringp id) (find id *runs* :key #'wrun-id :test #'string=)))

(defun node-table (run id)
  "RUN's state table for node ID, made when it is first named."
  (or (gethash id (wrun-nodes run))
      (setf (gethash id (wrun-nodes run)) (nlk:make-json-object "state" "pending"))))

(defun node-state (run id)
  "Where node ID of RUN stands: pending, running, done, failed, skipped,
held or expanded."
  (or (nlk:json-value (node-table run id) :string "state") "pending"))

(defun set-node-state (run id state &key session turn error started-at)
  "Put node ID of RUN in STATE, with what the transition knows."
  (let ((table (node-table run id)))
    (setf (gethash "state" table) state)
    (when session (setf (gethash "session" table) session))
    (when turn (setf (gethash "turn" table) turn))
    (when error (setf (gethash "error" table) error))
    (when (or started-at (equal state "running"))
      (setf (gethash "started_at" table) (or started-at (nlk:iso-now))))
    (when (member state '("done" "failed" "skipped") :test #'equal)
      (setf (gethash "finished_at" table) (nlk:iso-now))))
  state)

(defun node-field (run id key)
  "KEY of node ID's state table, as a string, or NIL."
  (nlk:json-value (node-table run id) :string key))

(defun node-def (run id)
  "The node ID is declared as: an instance's own copy, else the graph's."
  (or (gethash id (wrun-instances run))
      (node-by-id (wrun-graph run) id)))

(defun dep-ids (run dep)
  "The node ids DEP stands for: a fan-out is all of its instances."
  (or (gethash dep (wrun-expanded run)) (list dep)))

(defun run-output (run id)
  "What node ID of RUN committed, from memory or from its file."
  (multiple-value-bind (text present) (gethash id (wrun-outputs run))
    (if present
        text
        (let ((path (output-path run id)))
          (when (probe-file path)
            (setf (gethash id (wrun-outputs run)) (nlk:read-text path)))))))

(defun set-output (run id text)
  (setf (gethash id (wrun-outputs run)) (or text ""))
  (nlk:write-file-atomically (output-path run id) (or text ""))
  (or text ""))

;;; --- the disk -------------------------------------------------------------------

(defun run-json (run)
  "RUN as the JSON its folder holds."
  (flet ((object () (make-hash-table :test 'equal)))
    (let ((nodes (object))
          (expanded (object)))
      (maphash (lambda (id table) (setf (gethash id nodes) table)) (wrun-nodes run))
      (maphash (lambda (id ids) (setf (gethash id expanded) (coerce ids 'vector)))
               (wrun-expanded run))
      (let ((json (object)))
        (setf (gethash "id" json) (wrun-id run)
              (gethash "name" json) (wrun-name run)
              (gethash "status" json) (wrun-status run)
              (gethash "created_at" json) (wrun-created-at run)
              (gethash "updated_at" json) (nlk:iso-now)
              (gethash "origin" json) (or (wrun-origin run) "")
              (gethash "graph" json) (wrun-raw run)
              (gethash "expanded" json) expanded
              (gethash "nodes" json) nodes)
        json))))

(defun save-run (run)
  "RUN to its folder: run.json whole, every output of the session in hand."
  (nlk:write-file-atomically (merge-pathnames "run.json" (run-dir run))
                              (nlk:encode-json-object (run-json run)))
  run)

(defun read-run (path)
  "The run PATH's run.json holds, or NIL."
  (let* ((json (ignore-errors (nlk:decode-json (nlk:read-text path))))
         (id (and (hash-table-p json) (nlk:json-value json :string "id")))
         (raw (and id (nlk:json-value json :object "graph"))))
    (when (and id raw)
      (multiple-value-bind (graph problems) (parse-graph raw)
        (declare (ignore problems))
        (when graph
          (let ((run (make-wrun :id id
                                :name (or (nlk:json-value json :string "name") id)
                                :status (or (nlk:json-value json :string "status") "running")
                                :created-at (or (nlk:json-value json :string "created_at") "")
                                :origin (let ((origin (nlk:json-value json :string "origin")))
                                          (and origin (plusp (length origin)) origin))
                                :raw raw :graph graph)))
            (let ((nodes (nlk:json-value json :object "nodes")))
              (when nodes
                (maphash (lambda (node-id table)
                           (when (hash-table-p table) (setf (gethash node-id (wrun-nodes run)) table)))
                         nodes)))
            (let ((expanded (nlk:json-value json :object "expanded")))
              (when expanded
                (maphash (lambda (fan ids)
                           (let ((instances (loop for instance across (or ids #())
                                                  when (stringp instance) collect instance)))
                             (when instances
                               (setf (gethash fan (wrun-expanded run)) instances)
                               (let ((definition (node-by-id graph fan)))
                                 (when definition
                                   (dolist (instance instances)
                                     (setf (gethash instance (wrun-instances run))
                                           (copy-node definition :id instance :parent fan
                                                            :items nil :items-from nil))))))))
                         expanded)))
            run))))))

(defun load-runs ()
  "Every run this home holds, newest first."
  (let ((runs '()))
    (dolist (path (ignore-errors (directory (nlk:home "workflows/*/run.json"))))
      (let ((run (ignore-errors (read-run path))))
        (when run (push run runs))))
    (setf *runs* (sort runs #'string> :key #'wrun-created-at))
    *runs*))

;;; --- the names a run's answer uses -----------------------------------------------

(defun status-line (run id)
  "One line for node ID, as a status answer reads it: id, state, what it said."
  (let* ((table (node-table run id))
         (state (node-state run id))
         (error (nlk:json-value table :string "error"))
         (session (nlk:json-value table :string "session")))
    (format nil "  ~a  ~a~@[ - ~a~]~@[ (~a)~]"
            id state (and error (nlk:clip (nlk:one-line error) 160))
            (and session (equal state "running") session))))

(defun node-ids (run)
  "Every node of RUN in declaration order, an instance right after its fan-out."
  (let ((ids '()))
    (loop for node across (graph-nodes (wrun-graph run))
          do (let ((id (node-id node)))
               (push id ids)
               (dolist (instance (gethash id (wrun-expanded run)))
                 (push instance ids))))
    (nreverse ids)))
