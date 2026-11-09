;;;; cell-test.lisp --- lifecycle, the tree, the baseline, the page, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here, offline, is the cell's own wiring: a start installs
;;;; the command, the two tools and the manual topic and a stop takes them
;;;; back; a bad section refuses; a parent edge and the log's tool facts read
;;;; as one tree; a baseline written from one flow flags the connection it
;;;; never trusted and misses none; the page is one self-contained file; and
;;;; the command and the tools answer through the same readers a person uses.
;;;; Every test writes a store of its own under the scratch home and nothing
;;;; here dials anything.

(in-package #:nodecode.test)

(define-test-slice "flow" "FLOW-CELL-" :start nodecode-flow:start-cell)

(define-cell-lifecycle-tests "flow"
  (:help :flow)
  (:command "flow")
  (:refused ("max_sessions" 0) ("max_calls" 0)))

(defmacro with-flow (&body body)
  "BODY over a temp store, the cell started on its defaults and stopped on unwind."
  `(with-temp-store ()
     (with-cell-stop ((flow-start))
       ,@body)))

(defun flow-session (id &key parent)
  "One recorded session, under a scratch folder; PARENT makes it a spawned child."
  (nlk:create-session :id id :cwd (fixture-workspace)
                            :parent parent :detached (and parent t)))

(defun flow-call (turn tool &key failed ms)
  "One call and its result, recorded through the turn's own recorders."
  (let ((call-id (nlk::make-durable-id "call")))
    (nlk::record-tool-call turn call-id tool "{}")
    (nlk::record-tool-result turn call-id tool "ok" :duration-ms ms :failed (and failed t))))

;;; --- one tree, one baseline, one page ---------------------------------------------

(deftest flow-cell-a-tree-reads-a-handoff-and-its-tools ()
  (with-flow
    (flow-session "flow-parent")
    (flow-session "flow-child" :parent "flow-parent")
    (with-durable-turn (turn "flow-parent")
      (flow-call turn "sh")
      (flow-call turn "sh" :failed t :ms 260)
      (with-durable-turn (child "flow-child")
        (flow-call child "edit")))
    (let ((text (nodecode-flow:tree-text :session-id "flow-parent")))
      (is-carrying text
        "flow-parent" "2 agents" "3 calls" "1 handoff" "handoff"
        "sh x2" "1 failed" "0.3s" "edit x1"
        "an edge does not prove data transfer"))))

(deftest flow-cell-a-baseline-flags-the-connection-it-never-trusted ()
  (with-flow
    (flow-session "flow-a")
    (with-durable-turn (turn "flow-a") (flow-call turn "sh"))
    (let ((file (merge-pathnames "trusted-paths.json" (fixture-workspace))))
      (is-carrying (written (nodecode-flow:baseline (namestring file) :session-id "flow-a"))
        "trusted connection" "from 1 agent")
      (with-durable-turn (turn "flow-a") (flow-call turn "browser"))
      (multiple-value-bind (text new missing)
          (nodecode-flow:compare-baseline (namestring file) :session-id "flow-a")
        (is-carrying text "1 new connection" "NEW" "tool:browser")
        (is (= 1 (length new)) "one connection the baseline never trusted")
        (is (null missing) "every trusted connection was observed")))))

(deftest flow-cell-the-page-is-local-and-whole ()
  (with-flow
    (flow-session "flow-page")
    (with-durable-turn (turn "flow-page") (flow-call turn "sh"))
    (let ((file (merge-pathnames "flow.html" (fixture-workspace))))
      (is-carrying (written (nodecode-flow:report (namestring file) :session-id "flow-page"))
        "Wrote" "1 agent")
      (let ((page (uiop:read-file-string file)))
        (is-carrying page "<!doctype html" "flow-page" "sh x1" "external resources")
        (is (null (search "http" page)) "the page names no remote resource")))))

;;; --- the command and the tools -----------------------------------------------------

(deftest flow-cell-the-command-and-the-tools-answer ()
  (with-flow
    (flow-session "flow-cmd")
    (with-durable-turn (turn "flow-cmd") (flow-call turn "sh"))
    (is-carrying (text (nodecode-flow::command-text "sessions")) "flow-cmd" "1 call")
    (is-carrying (text (nodecode-flow::command-text "show flow-cmd")) "sh x1")
    (is-carrying (text (nodecode-flow::command-text "mermaid flow-cmd")) "flowchart LR" "t0")
    (is-carrying (text (nodecode-flow::command-text "help")) "/flow baseline")
    (let ((tool (nle::find-tool "flow_tree")))
      (is tool "flow_tree is registered")
      (is-carrying (answer (funcall (third tool)
                                    (nle::tool-body-arguments
                                     tool (nlk:json-object "session_id" "flow-cmd"))))
        "sh x1"))
    (is-carrying (text (nodecode-flow::run-slash "compare absent.json" nil)) "flow:")))
