;;;; graph-test.lisp --- what a graph must be, before a run exists.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest workflows-cell-a-graph-is-read-into-nodes-and-edges ()
  (multiple-value-bind (graph problems)
      (nodecode-workflows::parse-graph (wf-graph "g" (wf-node "a" "agent" "inspect")
                                                    (wf-node "b" "agent" "report" :after '("a"))))
    (is (null problems))
    (is (equal "g" (nodecode-workflows::graph-name graph)))
    (is (= 2 (length (nodecode-workflows::graph-nodes graph))))
    (is (equal '("a") (nodecode-workflows::node-after
                        (nodecode-workflows::node-by-id graph "b"))))))

(deftest workflows-cell-every-fault-of-a-graph-is-said-at-once ()
  (multiple-value-bind (graph problems)
      (nodecode-workflows::parse-graph
       (wf-graph "g" (wf-node "a" "agent" "inspect" :after '("b"))
                     (wf-node "b" "agent" "report" :after '("a"))
                     (wf-node "b" "agent" "again")
                     (wf-node "c" "agent" "")))
    (is (null graph))
    (is-present (found (find-if (lambda (text) (search "two nodes are both called" text))
                                    problems))
        "two nodes with one name" (is t))
    (is-present (found (find-if (lambda (text) (search "circle" text)) problems))
        "a circle" (is t))
    (is-present (found (find-if (lambda (text) (search "needs a \"goal\"" text)) problems))
        "a node with no goal" (is t))))

(deftest workflows-cell-a-graph-that-is-not-one-is-refused ()
  (multiple-value-bind (graph problems) (nodecode-workflows::parse-graph (vector 1 2))
    (is (null graph))
    (is (equal 1 (length problems))))
  (multiple-value-bind (graph problems) (nodecode-workflows::parse-graph
                                          (nlk:make-json-object "name" "g"))
    (is (null graph))
    (is-present (found (find-if (lambda (text) (search "at least one node" text)) problems))
        "an empty graph" (is t))))

(deftest workflows-cell-a-consolidate-node-names-a-fan-out ()
  (multiple-value-bind (graph problems)
      (nodecode-workflows::parse-graph
       (wf-graph "g" (wf-node "a" "agent" "inspect")
                     (wf-node "b" "agent" "join" :from "a")))
    (is (null graph))
    (is-present (found (find-if (lambda (text) (search "which is no fan-out" text)) problems))
        "from names no fan-out" (is t)))
  (multiple-value-bind (graph problems)
      (nodecode-workflows::parse-graph
       (wf-graph "g" (wf-node "a" "agent" "inspect" :items '("x"))
                     (wf-node "b" "agent" "join" :from "a")))
    (is (null problems))
    (is (= 2 (length (nodecode-workflows::graph-nodes graph))))))
