;;;; cell-test.lisp --- the hermes-herald cell against a stubbed API server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP: nothing reaches a profile, the
;;;; network or the operator's files. Each tool is called the way a model's
;;;; call reaches it — through the real dispatch, contract and all — so the
;;;; schema the provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "hermes-herald" "HERMES-HERALD-CELL-")

(defun hermes-herald-config (&rest pairs)
  "A whole shared config holding the `hermes' section PAIRS make, its url
defaulted to a profile that is not there."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "url" section "http://hermes.test:8651")
    (nlk:json-object "hermes" section)))

(defun hermes-herald-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-hermes-herald:start-cell (apply #'hermes-herald-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-hermes-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL OPTIONS), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url options) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected Hermes request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun hermes-herald-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun hermes-herald-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (hermes-herald-answer id tool arguments)))

(defun sent-body (call)
  "The decoded body of one asked CALL, or NIL when it carried none."
  (let ((content (getf (third call) :content)))
    (and content (nlk:decode-json content))))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "hermes-herald"
  (:config)
  (:help :hermes-herald)
  (:command "hermes")
  (:refused ("url" 5)))

(deftest hermes-herald-cell-without-a-base-url-refuses ()
  ;; No verb can work without an address, so a section that names no base URL
  ;; is refused at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal (hermes-herald-start "url" ""))))
    (is (search "base URL" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "hermes_dispatch")) "and installs nothing")))

;;; --- hermes_dispatch ---------------------------------------------------------------

(deftest hermes-herald-cell-dispatches-and-records-the-run ()
  (with-cell-stop ((hermes-herald-start))
    (with-hermes-wire (asked ("/v1/runs/" (values "{\"run_id\":\"4f2a9c\"}" 200))
                             ("/v1/runs" (values "{\"run_id\":\"4f2a9c\"}" 200)))
      (let ((object (hermes-herald-object "hh-1" "hermes_dispatch"
                                          "{\"message\":\"review the diff\"}")))
        (is (equal "4f2a9c" (nlk:json-value object :string "run_id")) "the run id is answered")
        (is (= 1 (length asked)) "one POST starts a run")
        (is (eq :post (first (first asked))))
        (let ((body (sent-body (first asked))))
          (is (equal "review the diff" (nlk:json-value body :string "input")))
          (is (null (nlk:json-value body :string "model")) "no model is pinned when none is asked for"))))
    (with-hermes-wire (asked)
      (let ((ledger (hermes-herald-object "hh-2" "hermes_runs" "{\"limit\":5}"))
            (rows nil))
        (setf rows (nlk:json-array ledger "runs"))
        (is (plusp (nlk:json-value ledger :integer "count")) "the local record kept the run")
        (is (find "4f2a9c" rows :test #'equal
                  :key (lambda (row) (nlk:json-value row :string "run_id")))
            "under the run id the target answered")))))

(deftest hermes-herald-cell-sends-the-configured-model-alias ()
  (with-cell-stop ((hermes-herald-start "model" "reviewer-fast"))
    (with-hermes-wire (asked ("/v1/runs" (values "{\"run_id\":\"9c1\"}" 200)))
      (hermes-herald-object "hh-3" "hermes_dispatch"
                            "{\"message\":\"summarise\",\"instructions\":\"be brief\"}")
      (let ((body (sent-body (first asked))))
        (is (equal "reviewer-fast" (nlk:json-value body :string "model")) "the section's default alias rides along")
        (is (equal "be brief" (nlk:json-value body :string "instructions")))))))

;;; --- hermes_check --------------------------------------------------------------------

(deftest hermes-herald-cell-reads-a-run-and-says-what-it-is-doing ()
  (with-cell-stop ((hermes-herald-start))
    (with-hermes-wire (asked ("/v1/runs/4f2a9c"
                               (values "{\"run_id\":\"4f2a9c\",\"status\":\"completed\",\"model\":\"reviewer-fast\",\"summary\":\"done: two files\"}" 200)))
      (let* ((object (hermes-herald-object "hh-4" "hermes_check" "{\"run_ids\":[\"4f2a9c\"]}"))
             (rows (nlk:json-array object "runs"))
             (row (aref rows 0)))
        (is (= 1 (length rows)) "one run id, one row")
        (is (equal "completed" (nlk:json-value row :string "status")))
        (is (equal "done: two files" (nlk:json-value row :string "summary")))
        (is (eq :get (first (first asked))))))))

(deftest hermes-herald-cell-refuses-a-run-id-that-is-a-path ()
  (with-cell-stop ((hermes-herald-start))
    (with-hermes-wire (asked)
      (let ((answer (hermes-herald-answer "hh-5" "hermes_check" "{\"run_ids\":[\"../sessions\"]}")))
        (is (search "is not a run id" answer) "a run id with a slash never reaches a request path")
        (is (null asked) "and nothing was asked")))))

;;; --- hermes_cancel --------------------------------------------------------------------

(deftest hermes-herald-cell-cancels-through-the-stop-route ()
  (with-cell-stop ((hermes-herald-start))
    (with-hermes-wire (asked ("/v1/runs/4f2a9c/stop" (values "{\"stopped\":true}" 200)))
      (let ((object (hermes-herald-object "hh-6" "hermes_cancel" "{\"run_id\":\"4f2a9c\"}")))
        (is (equal "4f2a9c" (nlk:json-value object :string "run_id")))
        (is (equal "cancelling" (nlk:json-value object :string "status")))
        (is (search "/stop" (second (first asked))) "the stop route is the one called")
        (is (eq :post (first (first asked))))))))

;;; --- hermes_models --------------------------------------------------------------------

(deftest hermes-herald-cell-lists-the-dispatchable-routes ()
  (with-cell-stop ((hermes-herald-start))
    (with-hermes-wire (asked ("/v1/models"
                               (values (concatenate 'string
                                                    "{\"data\":["
                                                    "{\"id\":\"hermes-agent\"},"
                                                    "{\"id\":\"reviewer-fast\",\"parent\":\"hermes-agent\",\"root\":\"glm-5\",\"provider\":\"ollama-cloud\"}]}")
                                       200)))
      (let* ((object (hermes-herald-object "hh-7" "hermes_models" "{}"))
             (aliases (nlk:json-array object "aliases")))
        (is (equal "hermes-agent" (nlk:json-value object :string "primary")) "the route with no parent is the identity")
        (is (= 1 (length aliases)) "only the aliases are dispatchable")
        (is (equal "reviewer-fast" (nlk:json-value (aref aliases 0) :string "alias")))
        (is (equal "glm-5" (nlk:json-value (aref aliases 0) :string "model")))))))

;;; --- hermes_chat ----------------------------------------------------------------------

(deftest hermes-herald-cell-chats-and-remembers-the-session ()
  (with-cell-stop ((hermes-herald-start))
    (with-hermes-wire (asked ("/v1/chat/completions"
                               (values (concatenate 'string
                                                    "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"ready\"}}],"
                                                    "\"session_id\":\"sess-1\"}")
                                       200)))
      (let ((object (hermes-herald-object "hh-8" "hermes_chat" "{\"message\":\"hello\"}")))
        (is (equal "ready" (nlk:json-value object :string "reply")) "the reply is the assistant's text")
        (is (equal "sess-1" (nlk:json-value object :string "session")))
        (let ((body (sent-body (first asked))))
          (is (nth-value 1 (gethash "stream" body)) "the request carries a stream flag")
          (is (null (gethash "stream" body)) "and the turn is asked for unstreamed")
          (is (equal "hello" (nlk:json-value (aref (nlk:json-array body "messages") 0) :string "content")))))
      (with-hermes-wire (asked ("/v1/chat/completions"
                                 (values "{\"choices\":[{\"message\":{\"content\":\"again\"}}]}" 200)))
        (hermes-herald-object "hh-9" "hermes_chat" "{\"message\":\"and again\"}")
        (is (equal "sess-1" (cdr (assoc "x-hermes-session-id" (getf (third (first asked)) :headers)
                                         :test #'string-equal)))
            "the saved session rides the next turn")))))

;;; --- the command -------------------------------------------------------------------------

(deftest hermes-herald-cell-command-reports-the-target ()
  (with-cell-stop ((hermes-herald-start "name" "reviewer"))
    (with-hermes-wire (asked ("/v1/health" (values "{\"status\":\"ok\"}" 200))
                             ("/v1/models" (values "{\"data\":[{\"id\":\"a\"}]}" 200)))
      (let ((text (nodecode-hermes-herald:report "" "s1")))
        (is (search "reviewer at http://hermes.test:8651" text) "the command names the target")
        (is (search "up (" text) "and says whether it answers")
        (is (search "1 model route" text))))))
