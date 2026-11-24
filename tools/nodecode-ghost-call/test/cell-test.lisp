;;;; cell-test.lisp --- lifecycle, the seven tools and /ghost, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches a model, needs a
;;;; key or touches the network, and the clock is a counter: a phone call's
;;;; four-second interval is one binding away.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-ghost-wire (&body body)
  "BODY with NLK:HTTP stubbed and the cell's clock and call reset: a request
is logged in *REQUESTS* and answered by *REPLY*, so no test dials a model."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected ghost-call request")))
         (nodecode-ghost-call::*session* nil)
         (nodecode-ghost-call::*clock* (let ((now 1000)) (lambda () (incf now 10)))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun ghost-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun ghost-reply (text &optional (status 200))
  "Answer every request with TEXT and STATUS until the next reply."
  (setf *reply* (lambda (method url args)
                  (declare (ignore method url args))
                  (values text status))))

(defun ghost-answer (line)
  "A /chat/completions body whose one choice says LINE, as a model sends it."
  (format nil "{\"choices\": [{\"message\": {\"role\": \"assistant\", \"content\": ~s}}]}" line))

(defun ghost-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun ghost-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun ghost-prompt (request)
  "Every message of REQUEST's body, joined: what the model was told."
  (let ((messages (nlk:json-value (ghost-request-body request) :array "messages")))
    (format nil "~{~a~%~}" (map 'list (lambda (m) (nlk:json-value m :text "content")) messages))))

(defun ghost-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "ghost-call" name arguments)))

(defun ghost-started ()
  "ghost_start with a goal, a callee and a private target; answers its text."
  (ghost-call "ghost_start"
               "{\"goal\": \"book a table Friday 8pm for 4, budget $80\", \"callee_label\": \"Bella Napoli\", \"target\": \"$60\"}"))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "ghost-call" "GHOST-CALL-CELL-" :start nodecode-ghost-call:start-cell)

(define-cell-lifecycle-tests "ghost-call"
  (:fixture with-ghost-wire)
  (:config "api_key" "sk-test")
  (:help :ghost)
  (:command "ghost")
  (:running
   (is (nle::find-tool "ghost_start") "ghost_start is registered")
   (is (nle::find-tool "ghost_line") "and ghost_line")
   (is (nle::find-tool "ghost_status") "and ghost_status")
   (is (nle::find-tool "ghost_hint_latest") "and ghost_hint_latest")
   (is (nle::find-tool "ghost_intervene") "and ghost_intervene")
   (is (nle::find-tool "ghost_hangup") "and ghost_hangup")
   (is (nle::find-tool "ghost_summary") "and ghost_summary, the original's seven")
   (is (= 7 (count-if (lambda (entry) (eql 0 (search "ghost_" (first entry))))
                       nle::*tools*)) "seven of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "ghost_start")) "and stop takes every one back")
   (is (null (nle::find-tool "ghost_summary")) "down to the last"))
  (:refused ("max_hints" 0)
            ("min_hint_interval_s" 2)
            ("timeout_seconds" 0))
  (:idle nodecode-ghost-call:ghost-call-error (nodecode-ghost-call::tool-hints nil)))

(deftest ghost-call-cell-a-start-with-no-key-refuses-and-installs-nothing (with-ghost-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignore name)) nil)
    (let ((text (refusal-text nlk:config-refusal (ghost-call-start))))
      (is-carrying text "needs a key" "OPENAI_API_KEY")
      (is (null (nle::find-tool "ghost_start")) "nothing was installed"))))

;;; --- the goal, and a line that earns a hint ---------------------------------------

(deftest ghost-call-cell-a-line-that-matters-earns-a-numbered-hint (with-ghost-wire)
  (with-cell-stop ((ghost-call-start "api_key" "sk-test"))
    (let ((started (ghost-started)))
      (is (search "is listening" started) "the start answers that the call is on")
      (is (search "Bella Napoli" started) "with who is on the other side"))
    (ghost-reply (ghost-answer "they said Saturday only -> offer Friday 19:30 or Sunday"))
    (let ((answer (ghost-call "ghost_line"
                              "{\"speaker\": \"them\", \"text\": \"Friday is full, we only have Saturday, and the set menu is $95\"}")))
      (is (search "HINT #1" answer) "one numbered hint")
      (is (search "offer Friday 19:30" answer) "the model's line reaches the operator")
      (is (search "eyes only" answer) "and says the other party never hears it"))
    (let ((request (first (ghost-asked))))
      (is (eq :post (first request)) "one POST")
      (is (equal "https://api.openai.com/v1/chat/completions" (second request)) "one chat call")
      (is (equal "Bearer sk-test" (ghost-request-header request "authorization")) "the key")
      (let ((prompt (ghost-prompt request)))
        (is (search "book a table Friday 8pm" prompt) "the goal rides the prompt")
        (is (search "$60" prompt) "the private target is given to the model")
        (is (search "PRIVATE TARGET" prompt) "and marked private")
        (is (search "Friday is full" prompt) "the line that was said is there")))))

(deftest ghost-call-cell-a-greeting-never-reaches-the-model (with-ghost-wire)
  (with-cell-stop ((ghost-call-start "api_key" "sk-test"))
    (ghost-started)
    (let ((answer (ghost-call "ghost_line"
                              "{\"speaker\": \"them\", \"text\": \"hello, how can I help you today\"}")))
      (is (search "SILENT (greeting" answer) "a greeting is silent")
      (is (null (ghost-asked)) "and costs no request"))))

(deftest ghost-call-cell-the-operator-s-own-line-is-silent (with-ghost-wire)
  (with-cell-stop ((ghost-call-start "api_key" "sk-test"))
    (ghost-started)
    (let ((answer (ghost-call "ghost_line"
                              "{\"speaker\": \"you\", \"text\": \"I would like a table for four on Friday\"}")))
      (is (search "SILENT (not them" answer) "the operator's own line is not coached")
      (is (null (ghost-asked)) "and costs no request"))))

(deftest ghost-call-cell-the-interval-holds-a-second-hint-back (with-ghost-wire)
  (with-cell-stop ((ghost-call-start "api_key" "sk-test"))
    (let ((nodecode-ghost-call::*clock* (lambda () 1000)))
      (ghost-started)
      (ghost-reply (ghost-answer "they said Saturday only -> offer Friday 19:30"))
      (let ((first (ghost-call "ghost_line"
                                "{\"speaker\": \"them\", \"text\": \"Friday is full, we only have Saturday\"}")))
        (is (search "HINT #1" first) "the first line earns a hint"))
      (let ((second (ghost-call "ghost_line"
                                 "{\"speaker\": \"them\", \"text\": \"Sunday lunch is free, 12:00 or 14:00\"}")))
        (is (search "SILENT (4 s until" second) "the next one waits its four seconds")
        (is (= 1 (length (ghost-asked))) "and makes no second request")))))

(deftest ghost-call-cell-the-cap-ends-the-hints (with-ghost-wire)
  (with-cell-stop ((ghost-call-start "api_key" "sk-test" "max_hints" 1))
    (ghost-started)
    (ghost-reply (ghost-answer "they said Saturday only -> offer Friday 19:30"))
    (ghost-call "ghost_line"
                "{\"speaker\": \"them\", \"text\": \"Friday is full, we only have Saturday\"}")
    (let ((answer (ghost-call "ghost_line"
                              "{\"speaker\": \"them\", \"text\": \"Sunday lunch is free, 12:00 or 14:00\"}")))
      (is (search "SILENT (the cap of 1 hints is spent" answer) "the cap is the cap")
      (is (= 1 (length (ghost-asked))) "and nothing more is asked"))))

(deftest ghost-call-cell-a-repeat-of-the-same-line-is-not-news (with-ghost-wire)
  (with-cell-stop ((ghost-call-start "api_key" "sk-test"))
    (ghost-started)
    (ghost-reply (ghost-answer "they said Saturday only -> offer Friday 19:30"))
    (ghost-call "ghost_line"
                "{\"speaker\": \"them\", \"text\": \"Friday is full, we only have Saturday\"}")
    (let ((answer (ghost-call "ghost_line"
                              "{\"speaker\": \"them\", \"text\": \"Friday is full, we only have Saturday\"}")))
      (is (search "SILENT" answer) "the same line again is silent")
      (is (= 1 (length (ghost-asked))) "with no second request"))))

;;; --- intervene, hangup, summary -----------------------------------------------------

(deftest ghost-call-cell-intervene-waits-for-the-operator-s-allow (with-ghost-wire)
  (with-cell-stop ((ghost-call-start "api_key" "sk-test" "user_name" "Ada"))
    (ghost-started)
    (let ((refused (ghost-call "ghost_intervene" "{\"text\": \"One moment please\"}")))
      (is (search "ERROR: " refused) "intervene is refused by default")
      (is (search "/ghost allow" refused) "and names the one thing the operator types"))
    (is (search "intervene is allowed" (nodecode-ghost-call::run-command "allow" nil)) "allowed")
    (let ((first (ghost-call "ghost_intervene" "{\"text\": \"One moment please\"}")))
      (is (search "This is an AI assistant speaking on behalf of Ada" first) "said first")
      (is (search "One moment please" first) "and the words"))
    (let ((second (ghost-call "ghost_intervene" "{\"text\": \"We agreed on Friday\"}")))
      (is (search "We agreed on Friday" second) "a later line is the words alone")
      (is (null (search "This is an AI assistant" second)) "the disclosure is said once"))))

(deftest ghost-call-cell-hangup-answers-exactly-one-summary (with-ghost-wire)
  (with-cell-stop ((ghost-call-start "api_key" "sk-test"))
    (ghost-started)
    (ghost-reply (ghost-answer "they said Saturday only -> offer Friday 19:30"))
    (ghost-call "ghost_line"
                "{\"speaker\": \"them\", \"text\": \"Friday is full, we only have Saturday\"}")
    (ghost-reply
     (ghost-answer
      "{\"outcome\": \"agreed\", \"agreed\": \"Table for four on Friday at 20:00\", \"date\": \"Friday 20:00\", \"amount\": \"$78\", \"open_questions\": [\"confirm by SMS\"], \"follow_up_sms\": \"Hi, confirming Friday 20:00 for four. Ada\"}"))
    (let ((answer (ghost-call "ghost_hangup" "{}")))
      (is (search "Ghost call ended" answer) "one message")
      (is (search "Agreed: Table for four on Friday at 20:00" answer) "what was agreed")
      (is (search "Date: Friday 20:00" answer) "the date")
      (is (search "Amount: $78" answer) "the amount")
      (is (search "Open: confirm by SMS" answer) "the open item")
      (is (search "Draft message: \"Hi, confirming" answer) "and the draft follow-up")
      (is (search "1 hint" answer) "with how many hints the call got"))
    (let ((summary (ghost-call "ghost_summary" "{}")))
      (is (search "Ghost call ended" summary) "asking again answers the same summary")
      (is (= 2 (length (ghost-asked))) "built once: the hint and the summary, nothing more"))))

;;; --- the operator's command ---------------------------------------------------------

(deftest ghost-call-cell-the-command-runs-the-same-verbs (with-ghost-wire)
  (with-cell-stop ((ghost-call-start "api_key" "sk-test"))
    (is (search "no call yet" (nodecode-ghost-call::run-command "" nil)) "the state")
    (let ((started (nodecode-ghost-call::run-command "start book a table for four" nil)))
      (is (search "is listening" started) "/ghost start opens the call"))
    (ghost-reply (ghost-answer "they said Saturday only -> offer Friday 19:30"))
    (let ((hint (nodecode-ghost-call::run-command "hint Friday is full, we only have Saturday" nil)))
      (is (search "HINT #1" hint) "/ghost hint runs the same gate and model"))
    (let ((status (nodecode-ghost-call::run-command "status" nil)))
      (is (search "1/40 hints" status) "/ghost status counts the hints"))))
