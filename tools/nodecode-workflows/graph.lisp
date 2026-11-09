;;;; graph.lisp --- a graph is data: nodes, after-edges, fan-outs, gates.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here writes: PARSE-GRAPH reads a decoded JSON graph into NODE
;;;; records and collects every problem in one answer, which is what the
;;;; `validate' action returns and what a `run' refuses on before it writes
;;;; anything. A node is one lane's work: an agent turn, a constant (echo),
;;;; or a gate that holds the run for the operator's answer.

(in-package #:nodecode-workflows)

;;; --- the records ---------------------------------------------------------------

(defstruct (node (:constructor make-node) (:copier nil))
  "One node of a graph, as declared. An instance of a fan-out is a COPY of
its parent's declaration with its own id and PARENT set, its own item in its
run's state table."
  (id "" :type string)
  (type :agent)
  goal echo question
  (options #())
  (after '())
  (inputs '())
  (requires '())
  from
  items items-from schema budget
  parent)

(defun copy-node (node &key (id (node-id node)) (parent (node-parent node))
                              (items (node-items node)) (items-from (node-items-from node)))
  "NODE copied, with the slots a fan-out instance changes."
  (make-node :id id :type (node-type node) :goal (node-goal node) :echo (node-echo node)
             :question (node-question node) :options (node-options node)
             :after (node-after node) :inputs (node-inputs node) :requires (node-requires node)
             :from (node-from node) :items items :items-from items-from
             :schema (node-schema node) :budget (node-budget node) :parent parent))

(defstruct (graph (:constructor make-graph))
  "A graph: its name and its nodes, in declaration order."
  (name "run" :type string)
  (nodes #() :type vector))

(defun node-by-id (graph id)
  "The node of GRAPH called ID, or NIL."
  (find id (graph-nodes graph) :key #'node-id :test #'string=))

(defun node-numbers (node)
  "The strings NODE names as ancestors: its AFTER edges, its FROM fan-out."
  (append (node-after node) (when (node-from node) (list (node-from node)))))

;;; --- reading a decoded graph ------------------------------------------------------

(defun string-list (object &rest path)
  "The array of strings at PATH, as a list; anything else reads NIL."
  (loop for value across (apply #'nlk:json-array object path)
        when (stringp value) collect value))

(defun node-id-p (id)
  "Whether ID is a node's name: lowercase letters, digits, . _ and -, at
most 64, a letter or digit first."
  (and (stringp id)
       (<= 1 (length id) 64)
       (every (lambda (c) (or (and (< (char-code c) 128) (alphanumericp c)) (find c "._-")))
              id)
       (alphanumericp (char id 0))))

(defun parse-node (object position problem)
  "The NODE OBJECT declares, or NIL; every fault is said through PROBLEM."
  (unless (hash-table-p object)
    (funcall problem "node ~d is not a JSON object" position)
    (return-from parse-node nil))
  (let* ((id (nlk:json-value object :string "id"))
         (type (string-downcase (or (nlk:json-value object :string "type") "agent")))
         (node (make-node :id (or id "")
                          :type (intern (string-upcase type) :keyword)
                          :goal (nlk:json-value object :text "goal")
                          :echo (nlk:json-value object :text "echo")
                          :question (nlk:json-value object :text "question")
                          :options (coerce (string-list object "options") 'vector)
                          :after (string-list object "after")
                          :inputs (string-list object "inputs")
                          :requires (string-list object "requires")
                          :from (nlk:json-value object :string "from")
                          :schema (nlk:json-value object :object "schema")
                          :budget (nlk:json-value object :integer "budget_seconds")
                          :items (nlk:json-value object :array "fanout" "items")
                          :items-from (nlk:json-value object :string "fanout" "items_from"))))
    (unless (and id (node-id-p id))
      (funcall problem
               "node ~d needs an \"id\": lowercase letters, digits, . _ and -, at most 64, a letter or digit first"
               position))
    (unless (member (node-type node) '(:agent :echo :gate))
      (funcall problem "node ~s has type ~s: a node is an \"agent\", an \"echo\" or a \"gate\""
               (node-id node) type))
    (case (node-type node)
      (:agent (unless (node-goal node)
                (funcall problem "agent node ~s needs a \"goal\": what that lane is asked" (node-id node))))
      (:echo (unless (node-echo node)
               (funcall problem "echo node ~s needs an \"echo\" text" (node-id node))))
      (:gate (unless (node-question node)
               (funcall problem "gate node ~s needs a \"question\" to hold the run on" (node-id node)))))
    (when (and (node-items node) (node-items-from node))
      (funcall problem "node ~s fans out over \"items\" or \"items_from\", not both" (node-id node)))
    (when (and (nlk:json-value object :object "fanout")
               (null (node-items node)) (null (node-items-from node)))
      (funcall problem "node ~s has a \"fanout\" with neither \"items\" nor \"items_from\"" (node-id node)))
    (when (and (node-from node) (zerop (length (node-from node))))
      (funcall problem "node ~s has an empty \"from\"" (node-id node)))
    node))

(defun graph-cycle (nodes)
  "One node id on a cycle of AFTER edges, or NIL. White/grey/black."
  (let ((grey (make-hash-table :test 'equal))
        (black (make-hash-table :test 'equal)))
    (labels ((visit (node)
               (let ((id (node-id node)))
                 (cond ((gethash id black) nil)
                       ((gethash id grey) id)
                       (t (setf (gethash id grey) t)
                          (let ((found (loop for dep in (node-numbers node)
                                             for next = (find dep nodes :key #'node-id :test #'string=)
                                             when next
                                               do (let ((cycle (visit next)))
                                                    (when cycle (return cycle))))))
                            (remhash id grey)
                            (setf (gethash id black) t)
                            found))))))
      (loop for node in nodes thereis (visit node)))))

(defun parse-graph (object)
  "OBJECT, a decoded JSON graph, as a GRAPH and the problems that refuse it.
=> (values GRAPH PROBLEMS); GRAPH is NIL when PROBLEMS is not."
  (let ((problems '()))
    (flet ((problem (control &rest args)
             (push (apply #'format nil control args) problems)))
      (unless (hash-table-p object)
        (problem "a graph is a JSON object {\"name\": ..., \"nodes\": [...]}, and this is not one")
        (return-from parse-graph (values nil (nreverse problems))))
      (let ((raw (nlk:json-array object "nodes"))
            (nodes '())
            (seen (make-hash-table :test 'equal)))
        (when (zerop (length raw))
          (problem "a graph needs a \"nodes\" array with at least one node"))
        (loop for element across raw
              for position from 1
              for node = (parse-node element position #'problem)
              when node do (push node nodes))
        (setf nodes (nreverse nodes))
        (dolist (node nodes)
          (if (gethash (node-id node) seen)
              (problem "two nodes are both called \"~a\"" (node-id node))
              (setf (gethash (node-id node) seen) t)))
        (dolist (node nodes)
          (dolist (dep (node-numbers node))
            (unless (gethash dep seen)
              (problem "node \"~a\" waits on \"~a\", and no node is called that"
                       (node-id node) dep)))
          (dolist (ref (node-inputs node))
            (let ((cut (position #\. ref)))
              (unless (and cut (gethash (subseq ref 0 cut) seen))
                (problem "node \"~a\" reads input \"~a\": an input is \"<node>.<path>\" and names an ancestor"
                         (node-id node) ref))))
          (when (node-from node)
            (let ((source (find (node-from node) nodes :key #'node-id :test #'string=)))
              (unless source
                (problem "node \"~a\" consolidates \"~a\", and no node is called that"
                         (node-id node) (node-from node)))
              (when (and source (null (node-items source)) (null (node-items-from source)))
                (problem "node \"~a\" has \"from\": \"~a\", which is no fan-out"
                         (node-id node) (node-from node))))))
        (let ((cycle (and nodes (graph-cycle nodes))))
          (when cycle
            (problem "the after-edges run in a circle through \"~a\": a graph is a DAG" cycle)))
        (values (and (null problems)
                     (make-graph :name (or (nlk:json-value object :string "name") "run")
                                 :nodes (coerce nodes 'vector)))
                (nreverse problems))))))

(defun problems-text (problems)
  "PROBLEMS as one refusal the model reads: one line each."
  (format nil "~{~a~^~%; ~}" problems))
