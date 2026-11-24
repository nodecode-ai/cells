;;;; cell-test.lisp --- the glorya cell against a stubbed Glorya server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Glorya request is a stubbed NLK:HTTP and the token a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches platform.glorya.ai and no real
;;;; token is read. Each tool is called the way a model's call reaches it --
;;;; through the real dispatch, contract and all -- so the schema the
;;;; provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "glorya" "GLORYA-CELL-" :start nodecode-glorya:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter +endpoint+ "https://platform.glorya.test/mcp")

(defparameter +created+
  "{\"job_id\":\"job-1\",\"kind\":\"image_ad\",\"status\":\"running\",\"title\":\"summer serum\",\"open_url\":\"https://platform.glorya.test/jobs/job-1\"}")

(defparameter +asking+
  "{\"job_id\":\"job-1\",\"kind\":\"image_ad\",\"status\":\"needs_input\",\"open_url\":\"https://platform.glorya.test/jobs/job-1\",\"questions\":[{\"request_id\":\"q-1\",\"kind\":\"choose\",\"question\":\"Which headline?\",\"choices\":[\"Fresh\",\"Bold\"]}]}")

(defparameter +finished+
  "{\"job_id\":\"job-1\",\"kind\":\"image_ad\",\"status\":\"completed\",\"open_url\":\"https://platform.glorya.test/jobs/job-1\",\"assets\":[{\"id\":\"asset-1\",\"type\":\"image\",\"label\":\"Ad\",\"url\":\"https://cdn.glorya.test/one.png\"}]}")

(defmacro with-key ((&optional (key "glorya-secret")) &body body)
  "BODY with the credential seam answering KEY for GLORYA_TEST_TOKEN."
  `(with-stubbed-fdefinition (nle::credential-env (variable)
                              (and (equal variable "GLORYA_TEST_TOKEN") ,key))
     ,@body))

(defmacro with-glorya ((&rest pairs) &body body)
  "A started cell on the test endpoint, one file at most, following nothing."
  `(with-key ()
     (with-cell-stop ((glorya-start "endpoint" +endpoint+
                                     "token_env" "GLORYA_TEST_TOKEN"
                                     "poll_seconds" 600
                                     "max_files" 1
                                     ,@pairs))
       ,@body)))

;;; --- the stubbed wire -------------------------------------------------------------

(defun payload-of (request)
  "The JSON payload REQUEST, a log entry, sent."
  (fourth request))

(defmacro with-server ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering from ROUTES, (NAME FORM) tried in order, where
NAME is the JSON-RPC method PAYLOAD carries (or :ASSET for a bodyless GET).
ASKED collects (METHOD URL OPTIONS PAYLOAD), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (let* ((content (getf options :content))
                                          (payload (and content (ignore-errors (nlk:decode-json content))))
                                          (name (if payload
                                                    (nlk:json-value payload :string "method")
                                                    :asset)))
                                     (push (list method url options payload) ,log)
                                     (cond ,@(loop for (route form) in routes
                                                   collect `((equal ,route name) ,form))
                                           (t (error "unexpected Glorya request: ~a ~a" method name)))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun rpc-reply (request key &key (protocol "2025-06-18") (session "session-1"))
  "A JSON-RPC answer to REQUEST carrying the JSON text KEY under result."
  (let ((id (nlk:json-value request :integer "id")))
    (values (nlk:encode-json-object
             (nlk:json-object "jsonrpc" "2.0" "id" id
                              "result" (nlk:json-object
                                        "protocolVersion" protocol
                                        "content" (vector (nlk:json-object
                                                           "type" "text"
                                                           "text" key)))))
            200
            (and session (list (cons "mcp-session-id" session))))))

(defun handshake ()
  "The two answers every started session gets before its first tool call."
  (list (cons "initialize" (lambda (payload) (rpc-reply payload "{}")))
        (cons "notifications/initialized" (lambda (payload) (declare (ignore payload)) (values "" 202 nil)))))

(defun sent (asked name)
  "The log entries that carried the JSON-RPC method NAME."
  (remove name asked :test-not #'equal
          :key (lambda (request) (and (payload-of request)
                                      (nlk:json-value (payload-of request) :string "method")))))

(defun tool-requests (asked)
  (sent asked "tools/call"))

(defun tool-named (asked tool)
  "The tools/call entry that called TOOL."
  (find tool (tool-requests asked) :test #'equal
        :key (lambda (request) (nlk:json-value (payload-of request) :string "params" "name"))))

(defun arguments-of (request)
  (nlk:json-value (payload-of request) :object "params" "arguments"))

(defun header-of (request name)
  (cdr (assoc name (getf (third request) :headers) :test #'equal)))

(defun session-of (request)
  "The Mcp-Session-Id REQUEST carried, or NIL."
  (header-of request "mcp-session-id"))

(defun tool-text (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

;;; --- the lifecycle, from the one declaration ---------------------------------------

(define-cell-lifecycle-tests "glorya"
  (:config "endpoint" +endpoint+ "token_env" "GLORYA_TEST_TOKEN")
  (:hooks :round)
  (:help :glorya)
  (:command "glorya")
  (:running (is (equal +endpoint+ (nodecode-glorya::setting :endpoint)))
            (is (= 20 (nodecode-glorya::setting :poll-seconds)) "the follower wakes every 20s by default"))
  (:refused ("endpoint" "ftp://platform.glorya.test/mcp")
             ("follow_minutes" 1)
             ("max_files" 0))
  (:idle nodecode-glorya:glorya-error
         (nodecode-glorya:glorya-create nil)
         (nodecode-glorya:glorya-jobs nil)))

;;; --- starting a job ----------------------------------------------------------------

(deftest glorya-cell-creates-an-image-ad ()
  (with-glorya ()
    (with-server (asked
                  ("initialize" (rpc-reply payload "{}"))
                  ("notifications/initialized" (values "" 202 nil))
                  ("tools/call" (rpc-reply payload +created+)))
      (let ((text (tool-text "c1" "glorya_create"
                              "{\"kind\":\"image_ad\",\"brief\":\"summer serum, 30 percent off\"}")))
        (is (search "Image ad started, job job-1" text))
        (is (search "https://platform.glorya.test/jobs/job-1" text) "the Glorya link")
        (is (search "don't poll" text) "and it says not to poll")
        (is (equal '("session-1") (mapcar #'session-of (sent asked "tools/call"))) "the session rides the call")
        (let* ((call (tool-named asked "create_image_ad"))
               (arguments (arguments-of call)))
          (is (equal "summer serum, 30 percent off" (nlk:json-value arguments :string "brief")))
          (is (equal "Bearer glorya-secret" (header-of call "authorization")) "the token, as a bearer header")
          (is (search "/mcp" (second call)) "on the endpoint the section names")
          (is (null (nlk:json-value arguments :string "format")) "no format on an image ad"))))))

(deftest glorya-cell-a-campaign-carries-its-platform-and-budget ()
  (with-glorya ()
    (with-server (asked
                  ("initialize" (rpc-reply payload "{}"))
                  ("notifications/initialized" (values "" 202 nil))
                  ("tools/call" (rpc-reply payload "{\"job_id\":\"job-2\",\"kind\":\"campaign\",\"status\":\"running\"}")))
      (tool-text "c2" "glorya_create"
                 "{\"kind\":\"campaign\",\"brief\":\"new serum\",\"platform\":\"meta\",\"daily_budget\":30,\"review\":true}")
      (let ((arguments (arguments-of (tool-named asked "create_campaign"))))
        (is (equal "meta" (nlk:json-value arguments :string "platform")))
        (is (= 30 (nlk:json-value arguments :integer "daily_budget")))
        (is (eq t (nlk:json-value arguments :boolean "review")) "a review campaign waits for approval")))))

;;; --- the question, the answer, the change, the launch ------------------------------

(deftest glorya-cell-reports-the-question-a-job-waits-on ()
  (with-glorya ()
    (with-server (asked
                  ("initialize" (rpc-reply payload "{}"))
                  ("notifications/initialized" (values "" 202 nil))
                  ("tools/call" (rpc-reply payload +asking+)))
      (let ((text (tool-text "c3" "glorya_job" "{\"job_id\":\"job-1\",\"wait_seconds\":5}")))
        (is (search "needs_input" text))
        (is (search "Which headline?" text))
        (is (search "request_id q-1" text))
        (is (search "Fresh | Bold" text) "the choices, in the order Glorya gave them")
        (is (search "glorya_answer" text))
        (is (= 5 (nlk:json-value (arguments-of (tool-named asked "get_job")) :integer "wait_seconds")))))))

(deftest glorya-cell-answers-changes-and-publishes-a-job ()
  (with-glorya ()
    (with-server (asked
                  ("initialize" (rpc-reply payload "{}"))
                  ("notifications/initialized" (values "" 202 nil))
                  ("tools/call" (rpc-reply payload +created+)))
      (is (search "Sent \"Fresh\"" (tool-text "c4" "glorya_answer"
                                              "{\"job_id\":\"job-1\",\"answer\":\"Fresh\",\"request_id\":\"q-1\"}")))
      (let ((arguments (arguments-of (tool-named asked "respond"))))
        (is (equal "Fresh" (nlk:json-value arguments :string "answer")))
        (is (equal "q-1" (nlk:json-value arguments :string "request_id"))))
      (is (search "Asked Glorya to change job job-1"
                  (tool-text "c5" "glorya_refine"
                             "{\"job_id\":\"job-1\",\"instruction\":\"make the headline shorter\"}")))
      (is (equal "make the headline shorter"
                 (nlk:json-value (arguments-of (tool-named asked "refine")) :string "instruction")))
      (is (search "Publishing job job-1"
                  (tool-text "c6" "glorya_publish" "{\"job_id\":\"job-1\",\"slug\":\"serum\"}")))
      (is (equal "serum" (nlk:json-value (arguments-of (tool-named asked "publish_site")) :string "slug"))))))

;;; --- the files --------------------------------------------------------------------

(deftest glorya-cell-downloads-a-finished-job-s-file ()
  (with-glorya ()
    (with-server (asked
                  ("initialize" (rpc-reply payload "{}"))
                  ("notifications/initialized" (values "" 202 nil))
                  ("tools/call" (rpc-reply payload +finished+))
                  (:asset (values (coerce (map 'list #'char-code "PNGDATA") '(vector (unsigned-byte 8)))
                                  200 nil)))
      (let ((text (tool-text "c7" "glorya_attach" "{\"job_id\":\"job-1\"}")))
        (is (search "asset-1-1.png" text) "the file, saved under its own name")
        (is (search "![Ad](path)" text) "and how to show it")
        (let ((path (merge-pathnames "asset-1-1.png"
                                     (nodecode-glorya::download-folder
                                      (nodecode-glorya::job-record "job-1")))))
          (is (probe-file path) "the picture is on disk")
          (is (search (namestring path) text) "at the path the answer names"))))))

;;; --- the refusals -------------------------------------------------------------------

(deftest glorya-cell-refuses-a-call-with-no-token ()
  (with-key (nil)
    (with-cell-stop ((glorya-start "endpoint" +endpoint+ "token_env" "GLORYA_TEST_TOKEN"))
      (with-server (asked)
        (let ((text (tool-text "c8" "glorya_create"
                               "{\"kind\":\"image_ad\",\"brief\":\"a serum\"}")))
          (is (search "ERROR:" text))
          (is (search "no Glorya token" text))
          (is (search "GLORYA_TEST_TOKEN" text) "and names the variable to set")
          (is (null asked) "nothing was sent"))))))

;;; --- /glorya, without the model ----------------------------------------------------

(deftest glorya-cell-the-command-starts-and-reports-a-job ()
  (with-glorya ()
    (with-server (asked
                  ("initialize" (rpc-reply payload "{}"))
                  ("notifications/initialized" (values "" 202 nil))
                  ("tools/call" (rpc-reply payload +created+)))
      (multiple-value-bind (text dialog)
          (nodecode-glorya:glorya-command "image a serum for the summer" "s-1")
        (is (search "Image ad started, job job-1" text))
        (is (null dialog) "an answer, not a panel"))
      (multiple-value-bind (text dialog) (nodecode-glorya:glorya-command "jobs" "s-1")
        (is (search "job-1" text) "the job this conversation started")
        (is (null dialog)))
      (multiple-value-bind (text dialog) (nodecode-glorya:glorya-command "" "s-1")
        (is (search "on-brand ads and pages" text) "typed alone it explains itself")
        (is dialog "with a panel of the kinds")))))

(deftest glorya-cell-a-round-reminds-the-model-of-an-open-question ()
  (with-glorya ()
    (with-server (asked
                  ("initialize" (rpc-reply payload "{}"))
                  ("notifications/initialized" (values "" 202 nil))
                  ("tools/call" (rpc-reply payload +asking+)))
      (nodecode-glorya:glorya-command "image a serum" "s-1")
      (let* ((jobs (nodecode-glorya::recent-jobs "s-1"))
             (text (nodecode-glorya::reminder-text jobs)))
        (is (= 1 (length jobs)) "the job this conversation started")
        (is (search "Jobs this conversation started" text))
        (is (search "Which headline?" text))
        (is (search "glorya_answer" text)))))
  (with-glorya ()
    (is (null (nodecode-glorya::recent-jobs "s-2")) "a conversation with no jobs is told nothing")))
