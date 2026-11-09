;;;; cell-test.lisp --- the coder cell against a stubbed deployment.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Coder endpoint is a stubbed NODECODE-CODER::*HTTP*, every terminal
;;;; run a stubbed NODECODE-CODER::*PTY-READ*, and the token a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches a deployment, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it —
;;;; through the real dispatch, contract and all — so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "coder" "CODER-CELL-")

(defun coder-config (&rest pairs) "A whole shared config holding the `coder' section PAIRS make, with its
url, token variable and workspace defaulted."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "url" section "https://coder.test")
    (alexandria:ensure-gethash "token_env" section "CODER_API_KEY")
    (alexandria:ensure-gethash "workspace" section "dev")
    (nlk:json-object "coder" section)))

(defun coder-start (&rest pairs) "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-coder:start-cell (apply #'coder-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-coder-token (&body body) "BODY with the token environment answering one fixed token."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "CODER_API_KEY") "test-token"))
     ,@body))

(defmacro with-coder-wire ((asked &rest routes) &body body) "BODY with the cell's HTTP seam answering by URL. ROUTES are (FRAGMENT
FORM), tried in order; FORM runs with the request bound and answers (values
TEXT STATUS), as NLK:HTTP does. ASKED collects (METHOD URL CONTENT), oldest
first, once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (symbol-macrolet ((,asked (reverse ,log)))
         (let ((nodecode-coder::*http*
                 (lambda (method url headers content timeout)
                   (declare (ignore headers timeout))
                   (push (list method url content) ,log)
                   (cond ,@(loop for (fragment form) in routes
                                 collect `((search ,fragment url) ,form))
                         (t (error "unexpected Coder request to ~a" url))))))
           ,@body)))))

(defmacro with-coder-pty ((seen) &body body) "BODY with the cell's PTY seam answering BODY-LINES, one line each, and the
exit line built from the marker it is handed: SEEN holds the socket URL."
  `(let ((,seen nil))
     (let ((nodecode-coder::*new-token* (lambda () "tok1"))
           (nodecode-coder::*pty-read*
             (lambda (url headers marker seconds)
               (declare (ignore headers seconds))
               (setf ,seen (list url marker))
               (format nil "hello from the workspace~%~a7~a~%" marker marker))))
       ,@body)))

(defparameter +workspaces+
  (concatenate 'string "{\"workspaces\":[" "{\"id\":\"ws-1\",\"name\":\"dev\",\"template_name\":\"docker\"," "\"latest_build\":{\"transition\":\"start\",\"status\":\"running\"}}," "{\"id\":\"ws-2\",\"name\":\"data\",\"template_name\":\"docker\"," "\"latest_build\":{\"transition\":\"stop\",\"status\":\"stopped\"}}]}") "What the workspace search answers.")

(defun workspace-payload (transition status agent-status agent-state)
  (format nil "{\"id\":\"ws-1\",\"name\":\"dev\",\"template_name\":\"docker\",\"latest_build\":{\"id\":\"build-1\",\"transition\":\"~a\",\"status\":\"~a\",\"resources\":[{\"agents\":[{\"id\":\"agent-1\",\"name\":\"main\",\"status\":\"~a\",\"lifecycle_state\":\"~a\",\"operating_system\":\"linux\",\"architecture\":\"amd64\"}]}]}}"
          transition status agent-status agent-state))

(defparameter +ready+ (workspace-payload "start" "running" "connected" "ready") "A running workspace whose agent can run a command.")

(defparameter +stopped+ (workspace-payload "stop" "stopped" "disconnected" "created") "A stopped workspace, its agent gone.")

(defparameter +started+ (workspace-payload "start" "running" "connected" "ready") "The same workspace once its start build finished.")

(defun coder-answer (id tool arguments) "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun coder-object (id tool arguments) "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (coder-answer id tool arguments)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "coder"
  (:config)
  (:help :coder)
  (:fixture with-coder-token)
  (:refused ("url" 5) ("token_env" 5) ("startup_timeout" 1)))

(deftest coder-cell-without-a-token-refuses ()
  ;; The one setting no verb can work without: the section names a token, and
  ;; a section that names none is refused at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal (coder-start "token_env" ""))))
    (is (search "no token" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "coder_exec")) "and installs nothing")))

(deftest coder-cell-refuses-an-origin-with-a-path ()
  ;; Coder's own API lives at the origin; a url with a path would answer
  ;; something that is not Coder, so it is refused at start by name.
  (let ((condition (signals-error nlk:config-refusal
                                  (coder-start "url" "https://coder.test/api"))))
    (is (search "has a path" (princ-to-string condition)) "the refusal names the fault")))

;;; --- coder_workspaces ---------------------------------------------------------------

(deftest coder-cell-lists-workspaces (with-coder-token)
  (with-cell-stop ((coder-start))
    (with-coder-wire (asked ("/api/v2/workspaces?" (values +workspaces+ 200)))
      (let* ((object (coder-object "coder-list-1" "coder_workspaces" "{}"))
             (workspaces (nlk:json-array object "workspaces")))
        (is (= 2 (nlk:json-value object :integer "count")) "both workspaces")
        (is (equal "dev" (nlk:json-value (aref workspaces 0) :string "name")))
        (is (equal "running" (nlk:json-value (aref workspaces 0) :string "status")) "the last build's status comes through")
        (is (equal "stopped" (nlk:json-value (aref workspaces 1) :string "status")))
        (is (= 1 (length asked)) "a list is one REST call")
        (is (search "owner%3Ame" (second (first asked))) "the search is limited to the caller's own workspaces")))))

(deftest coder-cell-narrows-the-list-by-name (with-coder-token)
  (with-cell-stop ((coder-start))
    (with-coder-wire (asked ("/api/v2/workspaces?" (values +workspaces+ 200)))
      (let ((object (coder-object "coder-list-2" "coder_workspaces" "{\"query\":\"data\"}")))
        (is (= 1 (nlk:json-value object :integer "count")) "the query kept one workspace")
        (is (equal "data" (nlk:json-value (aref (nlk:json-array object "workspaces") 0)
                                           :string "name")))
        (is (search "name%3Adata" (second (first asked))) "the name narrows the search on Coder's side")))))

;;; --- coder_workspace -----------------------------------------------------------------

(deftest coder-cell-reads-one-workspace (with-coder-token)
  (with-cell-stop ((coder-start))
    (with-coder-wire (asked ("/api/v2/workspaces/ws-1" (values +ready+ 200))
                             ("/api/v2/workspaces?" (values +workspaces+ 200)))
      (let ((object (coder-object "coder-read-1" "coder_workspace" "{\"workspace\":\"dev\"}")))
        (is (equal "dev" (nlk:json-value object :string "name")))
        (is (eq t (nlk:json-value object :boolean "ready")) "the agent is connected and ready")
        (is (equal "agent-1"
                   (nlk:json-value (aref (nlk:json-array object "agents") 0) :string "id")))))))

(deftest coder-cell-explains-a-stopped-workspace (with-coder-token)
  (with-cell-stop ((coder-start))
    (with-coder-wire (asked ("/api/v2/workspaces/ws-1" (values +stopped+ 200))
                             ("/api/v2/workspaces?" (values +workspaces+ 200)))
      (let* ((answer (coder-answer "coder-read-2" "coder_workspace" "{\"workspace\":\"dev\"}"))
            (object (nlk:decode-json answer)))
        (is (search "\"ready\":false" answer) "no agent is ready in a stopped workspace")
        (is (search "stopped" (nlk:json-value object :string "note")) "and the answer says coder_exec will start it")))))

(deftest coder-cell-refuses-a-workspace-it-does-not-own (with-coder-token)
  (with-cell-stop ((coder-start))
    (with-coder-wire (asked ("/api/v2/workspaces?" (values +workspaces+ 200)))
      (let ((answer (coder-answer "coder-read-3" "coder_workspace" "{\"workspace\":\"nope\"}")))
        (is (search "no workspace named" answer))
        (is (search "dev" answer) "the refusal names the workspaces there are")))))

;;; --- coder_exec ----------------------------------------------------------------------

(deftest coder-cell-runs-a-command (with-coder-token)
  (with-cell-stop ((coder-start))
    (with-coder-wire (asked ("/api/v2/workspaces/ws-1" (values +ready+ 200))
                             ("/api/v2/workspaces?" (values +workspaces+ 200)))
      (with-coder-pty (socket)
        (let ((object (coder-object "coder-exec-1" "coder_exec" "{\"command\":\"ls -la\"}")))
          (is (equal "hello from the workspace" (nlk:json-value object :string "output")) "the terminal output, markers stripped")
          (is (= 7 (nlk:json-value object :integer "exit_code")) "and the command's own status")
          (is (search "\"started\":false" (coder-answer "coder-exec-1" "coder_exec" "{\"command\":\"ls -la\"}")) "a running workspace is not started again")
          (is (search "workspaceagents/agent-1/pty" (first socket)) "the agent's own PTY socket")
          (is (search "command=" (first socket)) "the command rides the query string")
          (is (search "__NODECODE_EXIT_tok1__" (first socket)) "and its exit marker with it")
          (is (eq :get (first (first asked))) "the search comes first"))))))

(deftest coder-cell-starts-a-stopped-workspace (with-coder-token)
  ;; A stopped workspace is started at command time, as the original did: the
  ;; build is posted, polled to completion, and the payload read again.
  (with-cell-stop ((coder-start))
    (with-coder-wire (asked ("/api/v2/workspacebuilds/build-2" (values "{\"job\":{\"status\":\"succeeded\",\"completed_at\":\"2026-01-01T00:00:00Z\"}}" 200))
                             ("/api/v2/workspaces/ws-1/builds" (values "{\"id\":\"build-2\"}" 201))
                             ("/api/v2/workspaces/ws-1" (values (if (member :post asked :key #'first)
                                                                     +started+ +stopped+)
                                                                 200))
                             ("/api/v2/workspaces?" (values +workspaces+ 200)))
      (with-coder-pty (socket)
        (declare (ignore socket))
        (let* ((raw (coder-answer "coder-exec-2" "coder_exec" "{\"command\":\"make\",\"workspace\":\"dev\"}"))
               (object (ignore-errors (nlk:decode-json raw))))
          (is (eq t (nlk:json-value object :boolean "started")) "the workspace was started")
          (is (= 7 (nlk:json-value object :integer "exit_code")))
          (is (eq :post (first (third asked))) "the start build is posted")
          (is (search "/api/v2/workspaces/ws-1/builds" (second (third asked)))))))))

(deftest coder-cell-refuses-a-command-with-no-workspace (with-coder-token)
  (with-cell-stop ((coder-start "workspace" ""))
    (with-coder-wire (asked)
      (let ((answer (coder-answer "coder-exec-3" "coder_exec" "{\"command\":\"ls\"}")))
        (is (search "no workspace" answer) "the refusal says what to set")
        (is (null asked) "and no request goes out")))))

(deftest coder-cell-refuses-an-empty-command (with-coder-token)
  (with-cell-stop ((coder-start))
    (with-coder-wire (asked)
      (let ((answer (coder-answer "coder-exec-4" "coder_exec" "{\"command\":\"  \"}")))
        (is (search "the command is empty" answer) "the body refuses a blank command")
        (is (null asked) "and nothing is asked of the deployment")))))

(deftest coder-cell-reports-a-missing-exit-marker (with-coder-token)
  ;; A terminal that closed before the command finished says so through the
  ;; one exit code every shell agrees means failure.
  (with-cell-stop ((coder-start))
    (with-coder-wire (asked ("/api/v2/workspaces/ws-1" (values +ready+ 200))
                             ("/api/v2/workspaces?" (values +workspaces+ 200)))
      (let ((nodecode-coder::*pty-read*
              (lambda (url headers marker seconds)
                (declare (ignore url headers marker seconds)) "half a line")))
        (let ((object (coder-object "coder-exec-5" "coder_exec" "{\"command\":\"sleep 1\"}")))
          (is (= 1 (nlk:json-value object :integer "exit_code")))
          (is (equal "half a line" (nlk:json-value object :string "output"))))))))



