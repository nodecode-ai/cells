;;;; cell-test.lisp --- the loop, the fan-out, the gate, the disk, the tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A node's turn is stubbed at the two seams (SUPPORT.LISP); everything
;;;; else is the cell's own: readiness, prompts, commits, skip-on-failure,
;;;; the verdict, and every refusal.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "workflows"
  (:help :workflow)
  (:command "workflow")
  (:running (is (listp nodecode-workflows::*runs*) "a fresh home holds no run")
            (is (not (null nodecode-workflows::*worker*)) "the worker is started"))
  (:refused ("max_parallel" 0) ("tick_seconds" 0) ("output_chars" 10))
  (:idle nodecode-workflows:workflows-error
         (workflows:status "none") (workflows:runs)
         (workflows:show "none" "a")))

;;; --- a graph, end to end ----------------------------------------------------------

(deftest workflows-cell-a-lane-starts-only-after-the-one-it-stands-on-committed ()
  (with-cell ()
    (with-runner ()
      (workflows:start (wf-graph "lane" (wf-node "a" "agent" "inspect the target")
                                       (wf-node "b" "agent" "report" :after '("a"))))
      (is-present (run (run-of "lane")) "the run is in the home"
        (drive "lane")
        (is (equal '("a") (launched-ids)) "only the node with no ancestor starts")
        (is (equal "pending" (state-of "lane" "b")))
        (drive "lane")
        (is (equal "done" (state-of "lane" "a")))
        (is (equal '("a" "b") (launched-ids)) "b starts once a committed")
        (is-carrying (text (launched-prompt "b")) "## Inputs" "### a" "done: a")
        (drive "lane")
        (is (equal "done" (status-of "lane")))
        (is-carrying (text (workflows:show "lane" "b")) "done: b")
        (is-carrying (text (workflows:status "lane")) "workflow lane" "done")))))

(deftest workflows-cell-a-node-behind-a-failure-is-skipped-not-run ()
  (with-cell ()
    (with-runner (:say (lambda (id) (if (string= id "a") :running (format nil "done: ~a" id))))
      (workflows:start (wf-graph "break" (wf-node "a" "agent" "inspect")
                                         (wf-node "b" "agent" "report" :after '("a"))
                                         (wf-node "c" "echo" "constant")))
      (drive "break")
      (is (equal "running" (state-of "break" "a")))
      (nodecode-workflows::fail-node (run-of "break") "a" "it broke")
      (drive "break")
      (is (equal "failed" (state-of "break" "a")))
      (is (equal "skipped" (state-of "break" "b")) "b stands behind a")
      (is (equal "done" (state-of "break" "c")) "an echo needs no turn")
      (is-carrying (text (workflows:show "break" "c")) "constant")
      (is (equal "partial" (status-of "break"))))))

(deftest workflows-cell-a-fan-out-runs-one-lane-an-item ()
  (with-cell ()
    (with-runner ()
      (workflows:start (wf-graph "fan" (wf-node "shard" "agent" "summarize the item"
                                                   :items '("alpha" "beta"))
                                      (wf-node "join" "agent" "merge" :from "shard")))
      (drive "fan")
      (is (equal '("shard.1" "shard.2") (launched-ids)) "one lane an item")
      (is (equal "expanded" (state-of "fan" "shard")))
      (is-carrying (text (launched-prompt "shard.2")) "beta" "fan-out")
      (drive "fan")
      (is (equal "done" (state-of "fan" "shard.1")))
      (is (equal "done" (state-of "fan" "shard.2")))
      (is-carrying (text (launched-prompt "join")) "The fan-out" "done: shard.1" "done: shard.2")
      (drive "fan")
      (is (equal "done" (status-of "fan"))))))

(deftest workflows-cell-a-gate-holds-the-run-until-it-is-answered ()
  (with-cell ()
    (with-runner ()
      (workflows:start (wf-graph "gate" (wf-node "ask" "gate" "Publish?")
                                        (wf-node "ship" "echo" "shipped" :after '("ask"))))
      (drive "gate")
      (is (equal "held" (state-of "gate" "ask")))
      (is (equal "pending" (state-of "gate" "ship")))
      (is (equal "running" (status-of "gate")) "a held gate is not an end")
      (is-carrying (text (workflows:status "gate")) "answer the gate" "ask")
      (is-carrying (text (workflows:answer "gate" "ask" "yes, ship it")) "answered")
      (drive "gate")
      (is (equal "done" (state-of "gate" "ship")))
      (is (equal "done" (status-of "gate")))
      (is (signals-p nodecode-workflows:workflows-error (workflows:answer "gate" "ship" "no")) "only a gate answers"))))

(deftest workflows-cell-a-run-resumes-where-it-stopped ()
  (with-cell ()
    (with-runner ()
      (workflows:start (wf-graph "resume" (wf-node "a" "agent" "inspect")
                                         (wf-node "b" "agent" "report" :after '("a"))))
      (drive "resume")
      (drive "resume")
      (is (equal '("a" "b") (launched-ids)))
      (nodecode-workflows::save-run (run-of "resume"))
      ;; a new process would find the same folder: nothing of a's turn is redone
      (let ((nodecode-workflows::*runs* '()))
        (nodecode-workflows::load-runs)
        (is (equal "done" (state-of "resume" "a")))
        (setf *launched* '())
        (drive "resume")
        (is (equal '() (launched-ids)) "a committed node is not launched again")
        (is-carrying (text (workflows:show "resume" "a"))
                     ("done: a" "its output came back off the disk"))))))

(deftest workflows-cell-a-run-that-is-not-in-the-home-is-refused ()
  (with-cell ()
    (with-runner ()
      (is (signals-p nodecode-workflows:workflows-error (workflows:status "nope")))
      (is (signals-p nodecode-workflows:workflows-error (workflows:stop "nope")))
      (let ((nodecode-workflows::*runs* '()))
        (is-carrying (text (workflows:runs)) "no workflow run"))
      (is-carrying (text (workflows:validate (wf-graph "refused" (wf-node "a" "agent" "x"))))
                   "ok:")
      (is (signals-p nodecode-workflows:workflows-error
            (workflows:start (wf-graph "refused" (wf-node "a" "agent" "x" :after '("b"))))))
      (is (null (run-of "refused")) "a refused graph writes nothing"))))

;;; --- the tool ---------------------------------------------------------------------

(defun tool-call (&rest pairs)
  (nodecode-workflows::workflow-tool (apply #'nlk:make-json-object pairs)))

(deftest workflows-cell-the-tool-runs-and-reads-a-graph ()
  (with-cell ()
    (with-runner ()
      (is-carrying (text (tool-call "action" "validate"
                                     "graph" (wf-graph "tool" (wf-node "a" "agent" "x"))))
                   "ok: 1 node")
      (is-carrying (text (tool-call "action" "validate"
                                     "graph" (wf-graph "tool" (wf-node "a" "agent" ""))))
                   "refused:" "goal")
      (is-carrying (text (tool-call "action" "run" "name" "check"
                                     "graph" (wf-graph "tool" (wf-node "a" "agent" "x"))))
                   "workflow check started" "background")
      (drive "check")
      (drive "check")
      (is-carrying (text (tool-call "action" "status" "run_id" "check")) "workflow check")
      (is-carrying (text (tool-call "action" "list")) "check")
      (is-carrying (text (tool-call "action" "show" "run_id" "check" "node_id" "a")) "done: a")
      (is-carrying (text (tool-call "action" "stop" "run_id" "check")) "workflow check")
      (is-carrying (text (tool-call "action" "bogus")) "ERROR:")
      (is-carrying (text (tool-call "action" "run")) "ERROR:" "graph"))))

;;; --- /workflow --------------------------------------------------------------------

(deftest workflows-cell-the-command-reads-the-runs ()
  (with-cell ()
    (with-runner ()
      (workflows:start (wf-graph "cmd" (wf-node "a" "agent" "x")))
      (drive "cmd")
      (drive "cmd")
      (multiple-value-bind (text dialog) (nodecode-workflows::run-slash "" nil)
        (is-carrying (text text) "cmd")
        (is-present (dialog dialog) "bare /workflow opens a panel over the runs" (is t)))
      (is-carrying (text (nodecode-workflows::run-slash "status cmd" nil)) "workflow cmd")
      (is-carrying (text (nodecode-workflows::run-slash "show cmd a" nil)) "done: a")
      (is-carrying (text (nodecode-workflows::run-slash "list" nil)) "cmd"))))
