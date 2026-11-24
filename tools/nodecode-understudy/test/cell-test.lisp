;;;; cell-test.lisp --- lifecycle, the two tools and /understudy, against stubs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP and every past decision a stubbed
;;;; HISTORY-DECISIONS, so nothing reaches an endpoint, needs a key or a
;;;; session store. What is proved is the cell's own wiring: the section (a
;;;; healthy start, wrong values and a missing judge refused), the replay and
;;;; its judge through the seam the model calls them by, and the operator's
;;;; command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-understudy-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials an endpoint."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected understudy request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun understudy-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun understudy-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun understudy-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun understudy-message (body index)
  "BODY's messages[INDEX] content, as the wire carries it."
  (let ((messages (nlk:json-value body :array "messages")))
    (and messages (< index (length messages))
         (nlk:json-value (aref messages index) :text "content"))))

(defun understudy-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "understudy-call" name arguments)))

(defun understudy-decisions ()
  "Two past decisions, the shape the real read answers."
  (list (nodecode-understudy::make-decision :prompt "What port does the gateway use?"
                                            :answer "The gateway listens on 7777 by default."
                                            :session-id "s-old")
        (nodecode-understudy::make-decision :prompt "Which file holds the config?"
                                            :answer "It is config.jsonc in the home."
                                            :session-id "s-old")))

(defmacro with-decisions ((decisions) &body body)
  "BODY with the history read stubbed to DECISIONS, so no store is needed."
  `(with-stubbed-fdefinition (nodecode-understudy::history-decisions (&rest args)
                               (declare (ignore args))
                               ,decisions)
     ,@body))

(defun understudy-wire-answers (verdicts)
  "A reply that answers a candidate call with text and each judge call with
the next of VERDICTS."
  (let ((left (copy-list verdicts)))
    (lambda (method url args)
      (declare (ignore method url))
      (let* ((body (nlk:decode-json (getf args :content)))
             (model (nlk:json-value body :text "model")))
        (if (equal model "judge/model")
            (values (nlk:encode-json-object
                     (nlk:json-object
                      "choices" (vector (nlk:json-object
                                         "message" (nlk:json-object
                                                    "role" "assistant"
                                                    "content" (or (pop left) "MISS"))))
                      "usage" (nlk:json-object "prompt_tokens" 40
                                                "completion_tokens" 10)))
                    200)
            (values (nlk:encode-json-object
                     (nlk:json-object
                      "choices" (vector (nlk:json-object
                                         "message" (nlk:json-object
                                                    "role" "assistant"
                                                    "content" "candidate answer here")))
                      "usage" (nlk:json-object "prompt_tokens" 100
                                                "completion_tokens" 20)))
                    200))))))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "understudy" "UNDERSTUDY-CELL-" :start nodecode-understudy:start-cell)

(define-cell-lifecycle-tests "understudy"
  (:fixture with-understudy-wire)
  (:config "api_key" "sk-test" "judge_model" "judge/model")
  (:help :understudy)
  (:command "understudy")
  (:running
   (is (nle::find-tool "understudy_audition") "audition is registered")
   (is (nle::find-tool "understudy_report") "and the report, the original's two"))
  (:stopped
   (is (null (nle::find-tool "understudy_audition")) "and stop takes both back"))
  (:refused ("sample" 0)
            ("days" 0)
            ("timeout_seconds" 1)))

(deftest understudy-cell-a-missing-judge-refuses-and-installs-nothing (with-understudy-wire)
  (let ((text (refusal-text nlk:config-refusal (understudy-start "api_key" "sk-test"))))
    (is-carrying text ("judge_model" "the refusal names the member to set"))
    (is (null (nle::find-tool "understudy_audition")) "nothing was installed")))

(deftest understudy-cell-a-missing-key-refuses (with-understudy-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignore name)) nil)
    (let ((text (refusal-text nlk:config-refusal
                               (understudy-start "judge_model" "judge/model"))))
      (is-carrying text ("API key" "the refusal says a key is missing")))))

;;; --- the replay ------------------------------------------------------------------

(deftest understudy-cell-audition-replays-each-decision-and-judges-it (with-understudy-wire)
  (with-decisions ((understudy-decisions))
    (with-cell-stop ((understudy-start "api_key" "sk-test" "judge_model" "judge/model"))
      (setf *reply* (understudy-wire-answers '("TAKEOVER same substance"
                                               "MISS the port is wrong")))
      (let ((answer (understudy-call "understudy_audition"
                                      "{\"models\": [\"cheap/model\"]}")))
        (is (search "cheap/model" answer) "the scorecard names the candidate")
        (is (search "1/2 takeover (50%)" answer) "one takeover of two decided the rate")
        (is (search "340 tokens" answer) "the judge's and the candidate's tokens add up")
        (is (= 4 (length (understudy-asked))) "two decisions, two calls each")
        (let ((first-request (first (understudy-asked))))
          (is (eq :post (first first-request)) "one POST")
          (is (search "https://openrouter.ai/api/v1/chat/completions" (second first-request)) "to the endpoint's chat-completions path")
          (is (equal "Bearer sk-test" (understudy-request-header first-request "authorization")) "with the section's key")
          (is (equal "cheap/model" (nlk:json-value (understudy-request-body first-request)
                                                    :text "model")) "naming the candidate")
          (is (search "What port does the gateway use?"
                      (understudy-message (understudy-request-body first-request) 1)) "and replaying the operator's own question"))
        (let ((judge (second (understudy-asked))))
          (is (equal "judge/model" (nlk:json-value (understudy-request-body judge) :text "model")) "the second call is the judge's")
          (let ((prompt (understudy-message (understudy-request-body judge) 1)))
            (is (search "The gateway listens on 7777" prompt) "it reads production")
            (is (search "candidate answer here" prompt) "beside the candidate's answer")))))))

(deftest understudy-cell-a-refused-endpoint-is-the-model-s-repair (with-understudy-wire)
  (with-decisions ((understudy-decisions))
    (with-cell-stop ((understudy-start "api_key" "sk-test" "judge_model" "judge/model"))
      (setf *reply* (lambda (method url args)
                      (declare (ignore method url args))
                      (values "{\"error\": {\"message\": \"model not found\"}}" 404)))
      (let ((answer (understudy-call "understudy_audition"
                                      "{\"models\": [\"cheap/model\"]}")))
        (is (search "stopped:" answer) "a failure is the scorecard's line, not a condition up the stack")
        (is (search "HTTP 404" answer) "and names the status")
        (is (search "model not found" answer) "and repeats what the endpoint said")))))

;;; --- the command and the shelf -----------------------------------------------------

(deftest understudy-cell-the-command-shows-the-connection (with-understudy-wire)
  (with-cell-stop ((understudy-start "api_key" "sk-test" "judge_model" "judge/model"
                                     "budget_usd" "2.00"))
    (let ((text (cell-entry "nodecode-understudy" "understudy" "")))
      (is (search "https://openrouter.ai/api/v1" text) "the endpoint it talks to")
      (is (search "judge/model" text) "the judge it uses")
      (is (search "key set" text) "and whether a key is set")
      (is (search "$2.00" text) "and the cap it will stop at")
      (is (null (understudy-asked)) "answered without a request"))))

(deftest understudy-cell-the-report-names-a-run-it-does-not-hold (with-understudy-wire)
  (with-cell-stop ((understudy-start "api_key" "sk-test" "judge_model" "judge/model"))
    (let ((answer (understudy-call "understudy_report" "{\"run\": \"123\"}")))
      (is (search "no run 123" answer) "the report says which id it looked for")
      (is (search "understudy_audition" answer) "and how to make one"))))
