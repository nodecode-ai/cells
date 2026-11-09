;;;; cell-test.lisp --- the cell against a stubbed Hypotize MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Hypotize, no
;;;; token is needed and no experiment is touched. What is proved is the
;;;; cell's own wiring: the section (a healthy start, and a missing token
;;;; refused), the six tools through the dispatch the model calls them by --
;;;; the MCP tool each one names, the arguments it sends, the bearer token
;;;; and the session id on the wire -- and the /hypotize command.

(in-package #:nodecode.test)

(define-test-slice "hypotize" "HYPOTIZE-CELL-" :start nodecode-hypotize:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter *hypotize-calls* nil "Every request the stub answered, newest first, as (METHOD PARAMS HEADERS).")

(defparameter *hypotize-answer* nil "When set, (TEXT STATUS HEADERS): what the stub answers to tools/call.")

(defparameter *hypotize-record* nil "The updated_at the stubbed experiment record carries.")

(defun hz-envelope (id result) "One JSON-RPC answer carrying RESULT, as the wire spells it."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun hz-text-result (text &key errorp) "One tools/call result carrying TEXT."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text)) "isError" (and errorp t)))

(defun hz-call-result (params) "What the stub's tools/call answers: the tool's name and the arguments back,
except a record read, which answers the timestamp a write has to send back."
  (let ((tool (nlk:json-value params :string "name"))
        (arguments (nlk:json-value params :object "arguments")))
    (if (equal tool "hypotize_experiment_get")
        (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" (format nil "{\"updated_at\": \"~a\"}" *hypotize-record*))) "structuredContent" (nlk:json-object "id" (nlk:json-value arguments :text "experiment_id") "updated_at" *hypotize-record*))
        (hz-text-result (format nil "~a answered~@[ ~a~]" tool
                                (and arguments (nlk:encode-json-object arguments)))))))

(defun hz-session-headers ()
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash "mcp-session-id" headers) "sess-1")
    headers))

(defun hz-stub-wire (options) "NLK:HTTP, answering from the request's own method; nothing leaves the machine."
  (let* ((body (nlk:decode-json (or (getf options :content) "{}")))
         (name (nlk:json-value body :text "method"))
         (params (nlk:json-value body :object "params"))
         (id (nlk:json-value body :integer "id")))
    (push (list name params (getf options :headers)) *hypotize-calls*)
    (cond ((equal name "initialize")
           (values (hz-envelope id (nlk:json-object "protocolVersion" "2025-06-18" "capabilities" (nlk:make-json-object)))
                   200 (hz-session-headers) nil))
          ((equal name "notifications/initialized")
           (values "" 202 nil nil))
          ((equal name "tools/call")
           (if *hypotize-answer*
               (values (first *hypotize-answer*) (second *hypotize-answer*)
                       (third *hypotize-answer*) nil)
               (values (hz-envelope id (hz-call-result params)) 200 nil nil)))
          (t (values "{}" 404 nil nil)))))

(defmacro with-hypotize-wire (&body body) "BODY with NLK:HTTP stubbed: every request is logged and answered here."
  `(let ((*hypotize-calls* '())
         (*hypotize-answer* nil)
         (*hypotize-record* "2026-01-02T03:04:05Z"))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method url))
                                (hz-stub-wire options))
       ,@body)))

(defmacro with-hypotize-running (&body body) "BODY with the cell started on a literal section, stopped on unwind."
  `(with-cell-stop ((hypotize-start "token" "tok-1"))
     ,@body))

(defun hypotize-tool-call (&optional (index 0)) "The INDEX'th newest tools/call the cell sent, as (METHOD PARAMS HEADERS)."
  (nth index (remove-if-not (lambda (call) (equal (first call) "tools/call"))
                             *hypotize-calls*)))

(defun hypotize-sent-tool (&optional (index 0)) "The MCP tool name the INDEX'th newest tools/call named."
  (let ((call (hypotize-tool-call index)))
    (and call (nlk:json-value (second call) :string "name"))))

(defun hypotize-sent-arguments (&optional (index 0)) "The arguments object of the INDEX'th newest tools/call."
  (let ((call (hypotize-tool-call index)))
    (and call (nlk:json-value (second call) :object "arguments"))))

(defun hypotize-request-header (request name) "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun hypotize-called (name arguments) "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "hypotize-call" name arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "hypotize"
  (:fixture with-hypotize-wire)
  (:config "token" "tok-1")
  (:help :hypotize)
  (:command "hypotize")
  (:running
   (is (nle::find-tool "hypotize_experiments") "the backlog is a tool")
   (is (nle::find-tool "hypotize_learnings") "and the learnings")
   (is (nle::find-tool "hypotize_idea") "and a new idea")
   (is (nle::find-tool "hypotize_advance") "and one lifecycle step")
   (is (nle::find-tool "hypotize_outcome") "and the close with a result")
   (is (nle::find-tool "hypotize_call") "and the door to every other Hypotize tool")
   (is (= 6 (count-if (lambda (entry) (eql 0 (search "hypotize_" (first entry))))
                      nle::*tools*)) "six of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "hypotize_experiments")) "and stop takes every one back")
   (is (null (nle::find-tool "hypotize_call"))))
  (:refused ("token" "tok-1" "timeout_seconds" 0)
            ("token" "tok-1" "max_chars" 10)))

(deftest hypotize-cell-a-start-with-no-token-refuses (with-hypotize-wire)
  (with-stubbed-fdefinition (nodecode-hypotize::mcp-token (url)
                                (declare (ignore url))
                                nil)
    (let ((text (refusal-text nlk:config-refusal (hypotize-start))))
      (is-carrying text "token" "/mcp")
      (is (null (nle::find-tool "hypotize_experiments")) "nothing was installed"))))

(deftest hypotize-cell-the-mcp-cells-sign-in-is-reused (with-hypotize-wire)
  ;; The shipped mcp cell keeps its OAuth sign-in under the home; this cell
  ;; reads that token rather than asking for one of its own.
  (let ((path (nlk:home "mcp/oauth.json")))
    (unwind-protect
         (progn
           (ensure-directories-exist path)
           (with-open-file (out path :direction :output :if-exists :supersede
                                      :if-does-not-exist :create)
             (write-string (nlk:encode-json-object
                            (nlk:json-object "https://api.hypotize.ai/mcp"
                                             (nlk:json-object "tokens"
                                              (nlk:json-object "access_token" "tok-stored"))))
                           out))
           (with-cell-stop ((hypotize-start))
             (hypotize-called "hypotize_learnings" "{}")
             (is (equal "Bearer tok-stored"
                        (hypotize-request-header (hypotize-tool-call) "authorization")) "the token the mcp cell kept is the one sent")))
      (when (probe-file path) (ignore-errors (delete-file path))))))

;;; --- the wire ---------------------------------------------------------------------

(deftest hypotize-cell-one-session-serves-every-call (with-hypotize-wire)
  (with-hypotize-running
    (hypotize-called "hypotize_learnings" "{}")
    (hypotize-called "hypotize_experiments" "{}")
    (is (= 1 (count-if (lambda (call) (equal (first call) "initialize")) *hypotize-calls*)) "initialize happens once and is reused")
    (is (equal "Bearer tok-1" (hypotize-request-header (hypotize-tool-call) "authorization")) "with this cell's own token")
    (is (equal "sess-1" (hypotize-request-header (hypotize-tool-call) "mcp-session-id")) "and the session id the server handed is echoed")))

(deftest hypotize-cell-an-error-the-server-reports-fails-the-call (with-hypotize-wire)
  (with-hypotize-running
    (setf *hypotize-answer* (list (hz-envelope 1 (hz-text-result "no project on this account" :errorp t))
                                   200 nil))
    (let ((answer (hypotize-called "hypotize_learnings" "{}")))
      (is (search "no project on this account" answer) "the server's words reach the model"))))

;;; --- the tools say what the skills say ---------------------------------------------

(deftest hypotize-cell-the-backlog-passes-the-filters-it-was-given (with-hypotize-wire)
  (with-hypotize-running
    (hypotize-called "hypotize_experiments" "{\"statuses\": [\"Running\"], \"funnel\": \"Activation\"}")
    (is (equal "hypotize_experiments_list" (hypotize-sent-tool)) "the read the skills start from, by the server's own name")
    (let ((sent (hypotize-sent-arguments)))
      (is (equal "Running" (aref (nlk:json-value sent :array "statuses") 0)) "the stage filter goes through")
      (is (equal "Activation" (nlk:json-value sent :text "funnel")) "and the funnel"))))

(deftest hypotize-cell-an-idea-carries-its-fields-and-one-key (with-hypotize-wire)
  (with-hypotize-running
    (let ((call "{\"title\": \"Shorter signup\", \"short_idea\": \"cut the form to three fields\",
                  \"hypothesis\": \"we believe fewer fields will raise completion\",
                  \"target_metric\": \"signup completion\",
                  \"success_metric\": \"4.8% to 5.4%\",
                  \"funnel\": \"Activation\",
                  \"tags\": [\"signup\"]}"))
      (hypotize-called "hypotize_idea" call)
      (is (equal "hypotize_idea_create" (hypotize-sent-tool)))
      (let* ((first-args (hypotize-sent-arguments))
             (key (nlk:json-value first-args :text "idempotency_key")))
        (is (equal "cut the form to three fields" (nlk:json-value first-args :text "short_idea")))
        (is (equal "Activation" (nlk:json-value first-args :text "funnel")))
        (is (and key (>= (length key) 8)) "every write carries an idempotency key of at least eight characters")
        (hypotize-called "hypotize_idea" call)
        (is (equal key (nlk:json-value (hypotize-sent-arguments) :text "idempotency_key")) "and the same call again is the same write, not a second idea")))))

(deftest hypotize-cell-a-write-reads-the-timestamp-first (with-hypotize-wire)
  (with-hypotize-running
    (hypotize-called "hypotize_advance" "{\"experiment_id\": \"e-1\", \"stage\": \"Approved\"}")
    (is (equal "hypotize_experiment_get" (hypotize-sent-tool 1)) "the record is read immediately before the write")
    (is (equal "hypotize_experiment_transition" (hypotize-sent-tool)))
    (is (equal "2026-01-02T03:04:05Z"
               (nlk:json-value (hypotize-sent-arguments) :text "expected_updated_at")) "and the timestamp it read is the one sent back")))

(deftest hypotize-cell-an-outcome-closes-into-the-stage-its-result-names (with-hypotize-wire)
  (with-hypotize-running
    (hypotize-called "hypotize_outcome" "{\"experiment_id\": \"e-1\", \"result\": \"Lose\",
                       \"actual_result\": \"checkout completion 4.8% to 4.6%\",
                       \"learning_summary\": \"the shorter form lost trust at the price step\",
                       \"evidence_links\": [\"https://analytics.test/q3\"]}")
    (is (equal "hypotize_experiment_record_outcome" (hypotize-sent-tool)))
    (let ((sent (hypotize-sent-arguments)))
      (is (equal "Lose" (nlk:json-value sent :text "result")))
      (is (equal "Failed" (nlk:json-value sent :text "final_status")) "a loss is closed as Failed, never softened")
      (is (equal "https://analytics.test/q3"
                 (aref (nlk:json-value sent :array "evidence_links") 0))))))

(deftest hypotize-cell-a-stage-out-of-the-lifecycle-is-refused-before-anything-is-sent
    (with-hypotize-wire)
  (with-hypotize-running
    (let ((answer (hypotize-called "hypotize_advance" "{\"experiment_id\": \"e-1\", \"stage\": \"Live\"}")))
      (is (search "stage is one of" answer) "the lifecycle is said back to the model")
      (is (null (hypotize-tool-call)) "and nothing was written"))))

(deftest hypotize-cell-call-reaches-any-other-hypotize-tool (with-hypotize-wire)
  (with-hypotize-running
    (hypotize-called "hypotize_call" "{\"tool\": \"hypotize_vote_upsert\",
                       \"arguments\": {\"experiment_id\": \"e-1\", \"impact\": 8, \"confidence\": 4}}")
    (is (equal "hypotize_vote_upsert" (hypotize-sent-tool)))
    (is (= 8 (nlk:json-value (hypotize-sent-arguments) :integer "impact")) "the vote's dimensions go through untouched")
    (let ((answer (hypotize-called "hypotize_call" "{\"tool\": \"delete_everything\"}")))
      (is (search "hypotize_" answer) "a tool that is not Hypotize's is refused")
      (is (equal "hypotize_vote_upsert" (hypotize-sent-tool)) "and the last thing sent is still the vote"))))

;;; --- the operator's command ---------------------------------------------------------

(deftest hypotize-cell-the-command-says-what-is-running (with-hypotize-wire)
  (with-hypotize-running
    (nodecode-hypotize::run-command "" nil)
    (is (equal "hypotize_experiments_list" (hypotize-sent-tool)))
    (let ((statuses (nlk:json-value (hypotize-sent-arguments) :array "statuses")))
      (is (equal "Running" (aref statuses 0)) "what is live")
      (is (equal "Analyzing" (aref statuses 1)) "and what waits on a result")))
  (with-hypotize-running
    (nodecode-hypotize::run-command "ready" nil)
    (is (equal "Ready" (aref (nlk:json-value (hypotize-sent-arguments) :array "statuses") 0)) "a stage typed as a word is spelled as Hypotize spells it")))

(deftest hypotize-cell-the-command-says-where-the-token-came-from (with-hypotize-wire)
  (with-hypotize-running
    (let ((answer (nodecode-hypotize::run-command "login" nil)))
      (is (search "token in use comes from" answer) "the operator is told, in words")
      (is (not (search "tok-1" answer)) "and never the token itself")
      (is (null (hypotize-tool-call)) "and nothing was sent for it"))))
