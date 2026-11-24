;;;; cell-test.lisp --- the plan, the phasing rules, the dispatch and the activity.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs a GitHub account, a token or the network: every request goes
;;;; through NODECODE-AGENT-BATCH::*HTTP*, a stub that records the call it was
;;;; handed and answers what the test chose, and the plan lives in a scratch
;;;; folder under the run's own home. What is under test is what this cell owns
;;;; -- the document it reads and writes, the rules that keep every task in
;;;; exactly one phase, the four inputs a dispatch sends, and the words each
;;;; answer uses.

(in-package #:nodecode.test)

(define-test-slice "agent-batch" "AGENT-BATCH-CELL-" :start nodecode-agent-batch:start-cell)

(define-cell-lifecycle-tests "agent-batch"
  (:config "state_dir" (namestring (scratch-dir)))
  (:help :agent-batch)
  (:command "agent-batch")
  (:running
   (is (nle::find-tool "agent_batch_plan") "the plan tool is registered")
   (is (nle::find-tool "agent_batch_tasks") "the task-list tool too")
   (is (nle::find-tool "agent_batch_phase") "the phasing tool")
   (is (nle::find-tool "agent_batch_dispatch") "the dispatch tool")
   (is (nle::find-tool "agent_batch_runs") "and the activity one")
   (is (equal "agent-batch.yml" (nodecode-agent-batch::setting :workflow)) "the section's workflow file")
   (is (equal "main" (nodecode-agent-batch::setting :base-branch)) "and the branch a dispatch runs on"))
  (:stopped
   (is (null (nle::find-tool "agent_batch_plan")) "and stop takes every one back"))
  (:refused ("timeout_seconds" 0)
            ("timeout_seconds" "soon"))
  (:idle nodecode-agent-batch:agent-batch-error (nodecode-agent-batch::plan-text)))

;;; --- the fixture ------------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . OPTIONS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed *HTTP* answers: a function of METHOD and URL => (values TEXT STATUS).")

(defvar *scratch* 0
  "How many scratch plans this run has made, so each test's folder is its own.")

(defun scratch-dir ()
  "A fresh folder under the run's home, holding a token file: the plan lives here."
  (let ((dir (nlk:home (format nil "test-agent-batch/~d/" (incf *scratch*)))))
    (ensure-directories-exist dir)
    (with-open-file (out (merge-pathnames "token" dir) :direction :output
                                                     :if-exists :supersede
                                                     :if-does-not-exist :create)
      (write-string "test-token" out))
    dir))

(defmacro with-agent-batch ((&rest pairs) &body body)
  "BODY with the cell started on a scratch plan and a stubbed GitHub."
  `(let* ((dir (scratch-dir))
          (*requests* '())
          (*answer-fn* (lambda (method url)
                         (declare (ignore method url))
                         (error "unexpected GitHub request")))
          (nodecode-agent-batch::*http*
            (lambda (method url &rest options)
              (push (list* method url options) *requests*)
              (funcall *answer-fn* method url))))
     (with-cell-stop ((agent-batch-start "state_dir" (namestring dir)
                                         "token_file" (namestring (merge-pathnames "token" dir))
                                         "repo" "owner/name"
                                         ,@pairs))
       ,@body)))

(defun asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun request-header (request name)
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun request-body (request)
  (let ((content (getf (cddr request) :content)))
    (and content (nlk:decode-json content))))

(defun answer-with (text &optional (status 200))
  (setf *answer-fn* (lambda (method url) (declare (ignore method url)) (values text status))))

(defun answer-fn-for (runs pulls)
  "A stub that answers the runs read with RUNS and every other read with PULLS."
  (setf *answer-fn*
        (lambda (method url)
          (declare (ignore method))
          (if (search "/actions/runs" url) (funcall runs) (funcall pulls)))))

(defun wire (name arguments)
  "NAME called the way the model calls it, ARGUMENTS a JSON object, as the text the model reads back."
  (nle::message-content
   (execute-wire-call "ab-call" name (nlk:encode-json-object arguments))))

(defun store-three-tasks ()
  "The plan every test starts from: three tasks, phased (0 1) then (2)."
  (wire "agent_batch_tasks"
        (nlk:make-json-object "tasks" (vector "add the parser" "add the tests" "ship it")))
  (wire "agent_batch_phase"
        (nlk:make-json-object "phases" (vector (vector 0 1) (vector 2)))))

;;; --- the plan, and the phasing rules ------------------------------------------------

(deftest agent-batch-cell-a-plan-round-trips-through-its-json ()
  (with-agent-batch ()
    (let ((answer (wire "agent_batch_tasks"
                        (nlk:make-json-object
                         "tasks" (vector "add the parser" "add the tests" "ship it")))))
      (is (search "Stored 3 tasks for owner/name" answer) "the tool says what it stored, and where it goes"))
    (let ((answer (wire "agent_batch_plan" (nlk:make-json-object))))
      (is (search "[0] add the parser" answer) "the plan numbers the tasks, which is what phasing takes")
      (is (search "[2] ship it" answer) "all of them")
      (is (search "repo owner/name" answer) "and names the repository it would dispatch to")
      (is (search "Phases (0)" answer) "with no phasing yet"))
    (let ((answer (wire "agent_batch_phase"
                        (nlk:make-json-object "phases" (vector (vector 0 1) (vector 2))))))
      (is (search "Saved 2 phases" answer) "the phasing is saved")
      (is (search "phase 0  pending  2 tasks" answer) "two parallel tasks in the first group")
      (is (search "phase 1  pending  1 task" answer) "and the dependent one after them"))
    (let* ((saved (with-open-file (in (nodecode-agent-batch::state-path "plan.json"))
                                   (nlk:decode-json (uiop:read-file-string in))))
           (status (coerce (nlk:json-value saved :array "phase_status") 'list))
           (first-phase (coerce (first (coerce (nlk:json-value saved :array "phases") 'list))
                                'list)))
      (is (equal '("pending" "pending") status) "the file holds the original's phase_status, one pending per phase")
      (is (equal '("add the parser" "add the tests") first-phase) "and the phase's tasks as text, as the original wrote them"))))

(deftest agent-batch-cell-a-task-in-two-phases-or-none-is-refused ()
  (with-agent-batch ()
    (wire "agent_batch_tasks" (nlk:make-json-object "tasks" (vector "one" "two" "three")))
    (let ((answer (wire "agent_batch_phase"
                        (nlk:make-json-object "phases" (vector (vector 0) (vector 2))))))
      (is (search "tasks not placed in any phase: 1" answer) "a task left out is named by index")
      (is (search "ERROR: agent-batch:" answer) "and reads as the failure it is"))
    (is (search "appears in two phases" (wire "agent_batch_phase" (nlk:make-json-object "phases" (vector (vector 0 1) (vector 1 2))))) "the same task twice is refused")
    (is (search "out of range" (wire "agent_batch_phase" (nlk:make-json-object "phases" (vector (vector 0 1 2 3))))) "an index past the list is refused")
    (is (search "phases must be a non-empty list of groups" (wire "agent_batch_phase" (nlk:make-json-object "phases" (vector)))) "an empty phasing is refused too")
    (is (search "task not in the stored plan" (wire "agent_batch_phase" (nlk:make-json-object "phases" (vector (vector "one" "four" "three"))))) "a task the plan does not hold is refused by its text")
    (let ((answer (wire "agent_batch_phase"
                        (nlk:make-json-object "phases" (vector (vector "one" "two") (vector 2))))))
      (is (search "Saved 2 phases" answer) "and text and indices may be mixed, as the original allowed"))))

(deftest agent-batch-cell-a-new-task-list-clears-the-old-phasing ()
  (with-agent-batch ()
    (store-three-tasks)
    (let ((answer (wire "agent_batch_tasks" (nlk:make-json-object "tasks" (vector "only this one")))))
      (is (search "Stored 1 task" answer) "the list is replaced")
      (let ((plan (nodecode-agent-batch::load-plan)))
        (is (null (getf plan :phases)) "and the phasing the old list had is gone")
        (is (null (getf plan :status)) "with its status")))))

;;; --- dispatch ------------------------------------------------------------------------

(deftest agent-batch-cell-a-dispatch-posts-the-workflow-inputs ()
  (with-agent-batch ()
    (store-three-tasks)
    (answer-with "" 204)
    (let ((answer (wire "agent_batch_dispatch" (nlk:make-json-object "phase" 0 "context" "house rules: no force pushes"))))
      (is (search "Dispatched phase 0: 2 tasks on owner/name" answer) "the answer names the phase and the repo")
      (is (search "agent-batch.yml" answer) "and the workflow it launched"))
    (let* ((request (first (asked)))
           (body (request-body request)))
      (is (= 1 (length (asked))) "one request")
      (is (eq :post (first request)) "a POST")
      (is (equal "https://api.github.com/repos/owner/name/actions/workflows/agent-batch.yml/dispatches" (second request)) "to the repository's workflow dispatch, as the original addressed it")
      (is (equal "Bearer test-token" (request-header request "authorization")) "with the section's token")
      (is (equal "application/vnd.github+json" (request-header request "accept")) "and GitHub's media type")
      (is (equal "main" (nlk:json-value body :string "ref")) "the section's branch")
      (is (equal (format nil "add the parser~%add the tests") (nlk:json-value body :string "inputs" "tasks")) "one task per line, the input the workflow reads")
      (is (equal "house rules: no force pushes" (nlk:json-value body :string "inputs" "context")) "the call's context")
      (is (equal "" (nlk:json-value body :string "inputs" "model")) "no model means the runner's own")
      (is (equal "main" (nlk:json-value body :string "inputs" "base_branch")) "and the base branch"))
    (is (equal '("running" "pending") (getf (nodecode-agent-batch::load-plan) :status)) "the dispatched phase is marked running")
    (let ((runs (nodecode-agent-batch::load-runs)))
      (is (= 1 (length runs)) "and the dispatch is recorded locally")
      (is (equal '("add the parser" "add the tests") (getf (first runs) :tasks)) "with its tasks"))
    (answer-with "{\"message\":\"Not Found\"}" 404)
    (let ((answer (wire "agent_batch_dispatch" (nlk:make-json-object "phase" 1 "workflow" "gone.yml"))))
      (is (search "GitHub answered 404" answer) "a refused dispatch says the status")
      (is (search "Not Found" answer) "and what GitHub said")
      (is (search "/repos/owner/name/actions/workflows/gone.yml/dispatches" answer) "and the call it refused"))
    (is (equal '("running" "pending") (getf (nodecode-agent-batch::load-plan) :status)) "the refused dispatch left the plan's status as it was")))

(deftest agent-batch-cell-a-dispatch-needs-a-token-a-repo-and-a-phase ()
  (with-agent-batch ()
    (setf *answer-fn* (lambda (method url) (declare (ignore method url)) (values "" 204)))
    (store-three-tasks)
    (let ((answer (wire "agent_batch_dispatch" (nlk:make-json-object "phase" 7))))
      (is (search "phase 7 is not one of the 2 stored (0..1)" answer) "an index the plan does not have"))
    (is (search "phase" (wire "agent_batch_dispatch" (nlk:make-json-object))) "and a dispatch that leaves the phase out says so"))
  (with-agent-batch ("token_file" (namestring (merge-pathnames "absent" dir)))
    (store-three-tasks)
    (let ((answer (with-stubbed-fdefinition (uiop:getenv (&rest arguments)
                                                          (if (equal (first arguments) "GITHUB_TOKEN")
                                                              nil
                                                              (apply original arguments)))
                    (wire "agent_batch_dispatch" (nlk:make-json-object "phase" 0)))))
      (is (search "no GitHub token" answer) "no token is refused by name, saying where one goes")))
  (with-agent-batch ("repo" "")
    (store-three-tasks)
    (is (search "no repo set" (wire "agent_batch_dispatch" (nlk:make-json-object "phase" 0))) "a plan with no repo anywhere says so")))

;;; --- the activity -------------------------------------------------------------------

(deftest agent-batch-cell-runs-read-the-workflow-runs-and-the-pulls ()
  (with-agent-batch ()
    (answer-fn-for
     (lambda ()
       (values (nlk:encode-json-object
                (nlk:make-json-object
                 "workflow_runs" (vector (nlk:make-json-object
                                           "name" "Agent Batch" "status" "completed"
                                           "conclusion" "success" "head_branch" "main"
                                           "html_url" "https://github.com/owner/name/actions/runs/1"))))
               200))
     (lambda ()
       (values (nlk:encode-json-object
                (vector (nlk:make-json-object
                         "number" 12 "title" "add the parser"
                         "head" (nlk:make-json-object "ref" "batch/parser"))))
               200)))
    (let ((answer (wire "agent_batch_runs" (nlk:make-json-object))))
      (is (search "GitHub activity for owner/name" answer) "the answer names the repository")
      (is (search "completed success on main -- Agent Batch" answer) "the newest workflow run, with how it ended")
      (is (search "#12 add the parser (batch/parser)" answer) "and every open pull request")
      (is (= 2 (length (asked))) "one read of each")))
  (with-agent-batch ()
    (answer-fn-for (lambda () (values "{\"workflow_runs\":[]}" 200))
                   (lambda () (values "{\"message\":\"Bad credentials\"}" 401)))
    (let ((answer (wire "agent_batch_runs" (nlk:make-json-object "repo" "other/repo"))))
      (is (search "Workflow runs: none" answer) "an empty side still answers")
      (is (search "Open pull requests: GitHub answered 401" answer) "and the failing side says what GitHub said")
      (is (search "/repos/other/repo/pulls" answer) "naming the call that failed")
      (is (equal "https://api.github.com/repos/other/repo/actions/runs?per_page=10" (second (first (asked)))) "the call's own repo overrides the section's"))))

(deftest agent-batch-cell-a-repo-that-is-not-owner-name-is-refused ()
  (with-agent-batch ()
    (let ((answer (wire "agent_batch_runs" (nlk:make-json-object "repo" "just-a-name"))))
      (is (search "repo must look like owner/name" answer) "the shape is named")
      (is (null *requests*) "and nothing was asked"))))

;;; --- the operator's command -----------------------------------------------------------

(deftest agent-batch-cell-the-command-shows-dispatches-and-resets ()
  (with-agent-batch ()
    (store-three-tasks)
    (is (search "Tasks (3)" (nodecode-agent-batch::run-command "" nil)) "bare, it prints the plan")
    (answer-with "" 204)
    (let ((answer (nodecode-agent-batch::run-command "dispatch 1" nil)))
      (is (search "Dispatched phase 1: 1 task on owner/name" answer) "dispatch N launches that phase")
      (is (member "ship it" (getf (first (nodecode-agent-batch::load-runs)) :tasks) :test #'equal) "the phase's own task went with it"))
    (is (search "one of: dispatch N, reset" (nodecode-agent-batch::run-command "frobnicate" nil)) "a word it does not know lists what it takes")
    (is (search "dispatch takes a phase's number" (nodecode-agent-batch::run-command "dispatch soon" nil)) "and a phase that is not a number says where the numbers are, in words")
    (let ((answer (nodecode-agent-batch::run-command "reset" nil)))
      (is (search "Cleared the plan: 3 tasks and 2 phases forgotten" answer) "reset says what it cleared")
      (is (null (getf (nodecode-agent-batch::load-plan) :tasks)) "and the task list is gone")
      (is (null (nodecode-agent-batch::load-runs)) "with the dispatch records"))))

;;; --- the manual -------------------------------------------------------------------------

(deftest agent-batch-cell-the-manual-carries-the-workflow ()
  (with-agent-batch ()
    (let ((text (cddr (assoc :agent-batch nle::*help-topics*))))
      (is (search "agent_batch_phase" text) "the phasing tool is named")
      (is (search "every task into exactly one phase" text) "with the rule that keeps the list whole")
      (is (search "Actions minutes" text) "and what a dispatch costs, so the model asks first"))))
