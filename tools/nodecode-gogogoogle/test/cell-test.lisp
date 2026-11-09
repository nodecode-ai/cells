;;;; cell-test.lisp --- the tools over a stubbed CLI and a stubbed git, the
;;;; refusals, and the operator's view.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here starts a process: *RUN* answers a canned agy stream-json
;;;; conversation and *GIT* answers canned git output, so what the cell decides
;;;; from them is what these tests read. Every run writes its log line under
;;;; the scratch home, which is the cell's own data folder.

(in-package #:nodecode.test)

;;; --- the stubs ---------------------------------------------------------------------

(defun gogogoogle-stream (&key (response "did the work") (status "SUCCESS")
                                (conversation "conv-1") denied (usage t))
  "One agy stream-json conversation: an init event, then a result event."
  (with-output-to-string (out)
    (write-line (nlk:encode-json-object
                 (nlk:make-json-object "event" "init" "conversation_id" conversation))
                out)
    (write-line (nlk:encode-json-object
                 (nlk:make-json-object
                  "event" "result" "conversation_id" conversation
                  "result" (nlk:make-json-object
                            "status" status
                            "response" response
                            "conversation_id" conversation
                            "denied_actions" (coerce (or denied '()) 'vector)
                            "usage" (if usage
                                        (nlk:make-json-object "input_tokens" 11 "output_tokens" 7)
                                        (nlk:make-json-object)))))
                 out)))

(defvar *gogogoogle-calls* nil
  "Every (ARGS PROMPT) a stub was handed in the test that bound this.")

(defun gogogoogle-stub (&key worker verifier (worker-status :ok) (verifier-status :ok)
                             (worker-stderr "") (verifier-stderr ""))
  "A *RUN* stub: WORKER for a delegation, VERIFIER for a verification, and
every (ARGS PROMPT) pair it was handed pushed onto *GOGOGOOGLE-CALLS*."
  (lambda (args prompt seconds directory)
    (declare (ignore seconds directory))
    (push (list args prompt) *gogogoogle-calls*)
    (if (search "independent verifier" prompt)
        (values (or verifier (gogogoogle-stream :response "VERDICT: PASS"))
                verifier-stderr 0 verifier-status)
        (values (or worker (gogogoogle-stream :response "did the work"))
                worker-stderr 0 worker-status))))

(defun gogogoogle-stub-git (&key (diff "") (diffs nil) (status "") (untracked ""))
  "A *GIT* stub: DIFF, or the DIFFS list once per "diff" call, and the rest."
  (let ((rest diffs))
    (lambda (directory &rest args)
      (declare (ignore directory))
      (cond ((string= (first args) "diff") (if rest (pop rest) diff))
            ((string= (first args) "status") status)
            ((string= (first args) "ls-files") untracked)
            (t "")))))

(defun gogogoogle-workdir ()
  "A fresh folder to delegate into, inside the scratch home."
  (namestring (ensure-directories-exist (nlk:home "gogogoogle-test-work/"))))

(defun gogogoogle-wait-for (thunk)
  "THUNK's first true answer, polled for five seconds."
  (loop repeat 100
        for value = (funcall thunk)
        when value return value
        do (sleep 0.05)))

;;; --- the cell's own names ----------------------------------------------------------

(defun gogogoogle-tool (name)
  "The registered tool NAME's function."
  (third (nle::find-tool name)))

(defun gogogoogle-context (contract &rest pairs)
  "PAIRS decoded through CONTRACT, the way a tool call arrives."
  (nlk:contract-decode (nlk:contract contract) (apply #'nlk:make-json-object pairs)))

(defun gogogoogle-call (name contract &rest pairs)
  "NAME called with PAIRS, its answer as text."
  (funcall (gogogoogle-tool name) (apply #'gogogoogle-context contract pairs)))

(defun gogogoogle-delegate (&rest pairs)
  "antigravity_delegate answered with PAIRS and WORKDIR defaulted."
  (apply #'gogogoogle-call "antigravity_delegate" 'nodecode-gogogoogle::antigravity-delegate
         (append (list "workdir" (gogogoogle-workdir)) pairs)))

(defun gogogoogle-json (text)
  "TEXT as a JSON object."
  (nlk:decode-json text))

;;; --- the lifecycle ------------------------------------------------------------------

(define-test-slice "gogogoogle" "GOGOGOOGLE-CELL-" :start nodecode-gogogoogle:start-cell)

(define-cell-lifecycle-tests "gogogoogle"
  (:config "worker_model" "gemini-3.8-flash-high")
  (:help :gogogoogle)
  (:command "gogogoogle")
  (:running
   (is (string= "gemini-3.8-flash-high" (nodecode-gogogoogle::setting :worker-model)) "the worker model")
   (is (equal '("claude-sonnet-5-5-low" "gemini-4-argon-high" "gemini-3.8-flash-high") (nodecode-gogogoogle::verifier-models)) "the default verifier chain")
   (is (nle::find-tool "antigravity_delegate") "the delegate tool")
   (is (nle::find-tool "antigravity_job") "the job tool")
   (is (nle::find-tool "antigravity_stats") "the stats tool"))
  (:refused ("browser_mode" "wide") ("worker_model" "   ")))

;;; --- the delegate tool --------------------------------------------------------------

(deftest gogogoogle-cell-delegate-answers-the-run-and-the-verdict ()
  (let ((*gogogoogle-calls* nil))
   (with-cell-stop ((gogogoogle-start "worker_model" "gemini-3.8-flash-high"))
    (let* ((nodecode-gogogoogle::*git* (gogogoogle-stub-git :diff ""))
           (nodecode-gogogoogle::*run* (gogogoogle-stub))
           (answer (gogogoogle-delegate "task" "write the tests" "minutes" 3)))
      (is (search "did the work" answer) "the worker's reply is in the answer")
      (is (search "VERIFY PASS" answer) "and the verifier's verdict")
      (is (search "\"seconds\":" answer) "the answer carries the worker's time")
      (let ((worker (find-if (lambda (call) (search "Do the task." (second call)))
                             *gogogoogle-calls*)))
        (is (not (null worker)) "the role's brief reaches agy")
        (is (member "--print-timeout" (first worker) :test #'string=) "the run has a time limit")
        (is (member "180s" (first worker) :test #'string=) "of the minutes it was given")
        (is (member "--dangerously-skip-permissions" (first worker) :test #'string=) "a do run approves itself"))))))

(deftest gogogoogle-cell-a-read-only-role-that-edits-fails-verification ()
  (with-cell-stop ((gogogoogle-start "worker_model" "gemini-3.8-flash-high"))
    (let* ((nodecode-gogogoogle::*git* (gogogoogle-stub-git :diffs (list "" "diff --git a/x b/x")))
           (nodecode-gogogoogle::*run* (gogogoogle-stub))
           (answer (gogogoogle-delegate "task" "read the code" "role" "research")))
      (is (search "READ-ONLY VIOLATION" answer) "the change a research run made is named")
      (is (search "VERIFY FAIL" answer) "and no verdict passes over it"))))

(deftest gogogoogle-cell-verifier-fail-and-inconclusive-are-reported ()
  (with-cell-stop ((gogogoogle-start "worker_model" "gemini-3.8-flash-high"))
    (let ((nodecode-gogogoogle::*git* (gogogoogle-stub-git)))
      (let ((nodecode-gogogoogle::*run*
              (gogogoogle-stub
               :verifier (gogogoogle-stream :response (format nil "VERDICT: FAIL~%missed a case")))))
        (is (search "VERIFY FAIL" (gogogoogle-delegate "task" "x")) "a FAIL verdict is answered"))
      (let ((nodecode-gogogoogle::*run*
              (gogogoogle-stub :verifier (gogogoogle-stream :response "looks fine to me"))))
        (is (search "VERIFY INCONCLUSIVE" (gogogoogle-delegate "task" "x")) "no verdict line is inconclusive")))))

(deftest gogogoogle-cell-a-timeout-names-the-conversation-to_resume ()
  (with-cell-stop ((gogogoogle-start "worker_model" "gemini-3.8-flash-high"))
    (let* ((nodecode-gogogoogle::*git* (gogogoogle-stub-git))
           (nodecode-gogogoogle::*run*
             (gogogoogle-stub :worker-status :timeout
                               :worker-stderr "print timeout"
                               :worker (gogogoogle-stream :conversation "conv-9")))
           (answer (gogogoogle-delegate "task" "long job")))
      (is (search "TIMED OUT" answer) "the run is named as timed out")
      (is (search "conv-9" answer) "and the conversation id to continue it")
      (is (search "VERIFY SKIPPED" answer) "with nothing verified"))))

(deftest gogogoogle-cell-a-missing-workdir-is-refused ()
  (with-cell-stop ((gogogoogle-start "worker_model" "gemini-3.8-flash-high"))
    (let* ((nodecode-gogogoogle::*git* (gogogoogle-stub-git))
           (nodecode-gogogoogle::*run* (gogogoogle-stub))
           (answer (gogogoogle-call "antigravity_delegate" 'nodecode-gogogoogle::antigravity-delegate
                                    "task" "x" "workdir" "/no/such/folder/at/all")))
      (is (search "REFUSED" answer) "a workdir that is not a folder is refused")
      (is (search "not an existing folder" answer) "in words that name the reason"))))

(deftest gogogoogle-cell-a-blocked-root-is-refused ()
  (let ((root (namestring (ensure-directories-exist (nlk:home "gogogoogle-blocked/")))))
    (with-cell-stop ((gogogoogle-start "worker_model" "gemini-3.8-flash-high"
                                       "blocked_roots" (vector root)))
      (let* ((nodecode-gogogoogle::*git* (gogogoogle-stub-git))
             (nodecode-gogogoogle::*run* (gogogoogle-stub))
             (answer (gogogoogle-delegate
                      "task" "x"
                      "workdir" (namestring (ensure-directories-exist
                                              (nlk:home "gogogoogle-blocked/work/"))))))
        (is (search "REFUSED" answer) "a folder under a blocked root is refused")
        (is (search "blocked root" answer) "and the answer says which rule")))))

;;; --- background jobs, stats, the command --------------------------------------------

(deftest gogogoogle-cell-a-background-run-is-polled-as-a-job ()
  (with-cell-stop ((gogogoogle-start "worker_model" "gemini-3.8-flash-high"))
    (let* ((nodecode-gogogoogle::*git* (gogogoogle-stub-git))
           (nodecode-gogogoogle::*run* (gogogoogle-stub))
           (answer (gogogoogle-json (gogogoogle-delegate "task" "x" "background" t)))
           (id (nlk:json-value answer :string "job")))
      (is (stringp id) "the call answers a job id at once")
      (is (equal "RUNNING" (nlk:json-value answer :string "status")) "and says the job is running")
      (let ((record (gogogoogle-wait-for
                     (lambda ()
                       (let* ((polled (gogogoogle-json
                                       (gogogoogle-call "antigravity_job"
                                                         'nodecode-gogogoogle::antigravity-job
                                                         "id" id)))
                              (status (nlk:json-value polled :string "job_status")))
                         (and (equal status "DONE") polled))))))
        (is (hash-table-p record) "the job finishes and is readable")
        (is (search "did the work" (or (nlk:json-value record :string "result") "")) "with the run's reply")
        (is (search "VERIFY PASS" (or (nlk:json-value record :string "verify") "")) "and its verdict")))))

(deftest gogogoogle-cell-stats-and-the-command-read-the-run-log ()
  (with-cell-stop ((gogogoogle-start "worker_model" "gemini-3.8-flash-high"))
    (let ((nodecode-gogogoogle::*git* (gogogoogle-stub-git))
          (nodecode-gogogoogle::*run* (gogogoogle-stub)))
      (gogogoogle-delegate "task" "one")
      (gogogoogle-delegate "task" "two")
      (let ((stats (gogogoogle-json
                    (funcall (gogogoogle-tool "antigravity_stats") (nlk:make-json-object)))))
        (is (>= (nlk:json-value stats :integer "runs") 2) "every run is counted")
        (is (hash-table-p (nlk:json-value stats :object "by_status")) "with its status")
        (is (hash-table-p (nlk:json-value stats :object "tokens_handled_by_agy")) "and the tokens spent"))
      (is (search "gogogoogle:" (nodecode-gogogoogle::run-slash "" "s1")) "the command answers the operator"))))
