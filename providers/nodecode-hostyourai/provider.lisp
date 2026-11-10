;;;; provider.lisp --- what HostYourAI is: its address, its key, its model, its hints.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from cogitor/hermes-hostyourai (see NOTICE): plugin.yaml's
;;;; HOSTYOURAI_API_KEY, __init__.py's API_BASE and AUTO_BASE, HYAI_TASKS,
;;;; HYAI_POLICIES, the hyai/auto provider row, the System One question auto
;;;; classifies a message with, and the /balance reading /hyai usage shows.
;;;;
;;;; HostYourAI speaks the OpenAI chat wire at https://hostyourai.com/api/v1
;;;; with a bearer key from its dashboard. What sets it apart from any other
;;;; compatible endpoint is the router: the model `hyai/auto' picks a warm
;;;; model per request, and a request may steer it with a task and a policy --
;;;; as an "hyai" object in the body, or as X-HYAI-Task / X-HYAI-Policy
;;;; headers. Task `auto' asks the service's System One model which task a
;;;; message is before the round goes out.

(in-package #:nodecode-hostyourai)

(defparameter +base+ "https://hostyourai.com/api/v1"
  "Where HostYourAI is served: the base the chat lane appends /chat/completions to.")

(defparameter +env+ "HOSTYOURAI_API_KEY"
  "The one variable a HostYourAI key is read from.")

(defparameter +key-page+ "https://hostyourai.com/app/docs"
  "Where a key is made and the router documented.")

(defparameter +model+ "hyai/auto"
  "The router: the one model this provider offers, and the only one a hint steers.")

(defparameter +tasks+ '("chat" "code" "summarise" "classify" "extract" "rag" "legal" "reason")
  "The work kinds hyai/auto takes, in the service's own spelling.")

(defparameter +policies+ '("balanced" "speed" "cost" "quality")
  "What hyai/auto optimizes for once it knows the task.")

(defparameter +router+ "router"
  "The section's word for `no hint': hyai/auto decides alone, per request.")

(defparameter +auto+ "auto"
  "The task that means `a small model picks one for each message'.")

(defparameter +auto-model+ "hyai/decider-2b"
  "The System One model auto classifies with.")

(defparameter +auto-threshold+ 0.8
  "Below this confidence the answer is not used and the round goes out as chat.")

(defparameter +auto-chars+ 200
  "How much of each of the last two user messages the classifier is sent.")

(defparameter +auto-min-chars+ 20
  "Both messages together shorter than this are not worth a round trip.")

(defparameter +auto-question+
  "`prompt` is the start of the latest message a user sent to an AI assistant. \
`previous_user` (if present) is the user message before it. Which kind of task \
is the user asking the assistant to do in `prompt`? Use the earlier message only to \
resolve vague replies such as 'ok', 'do that' or 'and the other one'."
  "The one question auto puts: the plugin's own words.")

(defparameter +auto-criteria+
  '(("chat" . "Casual conversation, advice, brainstorming, opinions, or general questions.")
    ("code" . "Writing, fixing, explaining or reviewing source code, scripts, or shell commands.")
    ("summarise" . "Condensing a given text, article, thread or meeting into a shorter version.")
    ("classify" . "Assigning a given item to a category or label, or sorting items into groups.")
    ("extract" . "Pulling specific fields or facts out of a given text into structured data.")
    ("rag" . "Answering a question from provided documents or notes, or searching a knowledge base.")
    ("legal" . "Contracts, regulations, compliance, terms of service, or legal interpretation.")
    ("reason" . "Multi-step maths, logic puzzles, planning or analysis that needs careful step-by-step thinking."))
  "What each task means, sent with the question so the classifier reads this plugin's line.")

;;; --- the catalog row -------------------------------------------------------------

(defun catalog-model ()
  "hyai/auto as the catalog keeps a model: text in, text out, tools, no published price."
  ;; A router's context is its warm model's, and the service does not publish
  ;; one: 128K is what every model it routes to carries at least.
  (nle::make-catalog-model +model+ 131072 32768 #("text") #("text")
                            nil nil nil t nil))

(defun catalog-row (&optional prior)
  "HostYourAI as a models.dev provider: the chat lane's package, this
section's base, the key variable, and hyai/auto over PRIOR's rows (the row
models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (let ((published (nlk:json-value prior :object "models")))
      (when published
        (maphash (lambda (id model) (setf (gethash id models) model)) published)))
    (setf (gethash +model+ models) (catalog-model))
    (nlk:json-object "name" "HostYourAI"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (vector +env+)
                     "models" models)))

;;; --- the service's other two endpoints --------------------------------------------

(defun api-root (base)
  "BASE without its trailing version segment: where the API's other endpoints hang."
  (let* ((base (string-right-trim "/" (or base +base+)))
         (length (length base)))
    (if (and (>= length 3) (string= base "/v1" :start1 (- length 3)))
        (subseq base 0 (- length 3))
        base)))

(defun balance-url (&optional (base (setting :base-url)))
  "The balance endpoint: the base's /v1/balance."
  (format nil "~a/v1/balance" (api-root base)))

(defun decide-url (&optional (base (setting :base-url)))
  "The System One endpoint auto classifies through."
  (format nil "~a/decision/v1/systemone" (api-root base)))

(defun api-key ()
  "The key this provider resolves to -- the config's, the store's /connect
saved, or HOSTYOURAI_API_KEY -- or NIL for the keyless stand-in."
  (handler-case
      (let ((credential (nle::resolve-provider-credential +provider+ :probe t)))
        (unless (member (nle:credential-source credential) '(:public "public") :test #'equal)
          (nle:credential-key credential)))
    (error () nil)))

(defun auth-headers (key)
  "The one header a key is ever sent in, to the one host that issued it."
  (list (cons "authorization" (format nil "Bearer ~a" key))))

;;; --- what auto sends ---------------------------------------------------------------

(defun message-text (message)
  "MESSAGE's content as text: a string, or the text parts of an array."
  (let ((content (and (hash-table-p message) (gethash "content" message))))
    (cond ((stringp content) content)
          ((vectorp content)
           (format nil "~{~a~^ ~}"
                   (loop for part across content
                         collect (or (and (hash-table-p part) (nlk:json-value part :text "text")) ""))))
          (t ""))))

(defun cut (text)
  "TEXT trimmed, at most the classifier's share of it."
  (let ((text (string-trim '(#\Space #\Newline #\Tab #\Return) (or text ""))))
    (subseq text 0 (min (length text) +auto-chars+))))

(defun texts (body)
  "(values PROMPT PREVIOUS): the start of the last user message in BODY and
of the user message before it, as auto sends them."
  (let ((users (loop for message in (coerce (nlk:json-array body "messages") 'list)
                     for text = (message-text message)
                     when (and (equal "user" (nlk:json-value message :text "role"))
                               (plusp (length (string-trim '(#\Space #\Newline #\Tab) text))))
                       collect text)))
    (values (cut (or (first (last users)) ""))
            (cut (or (first (last (butlast users))) "")))))

(defun criteria-object ()
  "The task criteria as the JSON object the question carries."
  (let ((object (nlk:json-object)))
    (dolist (criterion +auto-criteria+)
      (setf (gethash (car criterion) object) (cdr criterion)))
    object))

(defun classify (prompt previous key &optional (base (setting :base-url)))
  "Ask HostYourAI's System One which task PROMPT is: a task from +TASKS+, or
NIL when the answer is not one this plugin trusts."
  (let* ((state (if (plusp (length previous))
                    (nlk:json-object "prompt" prompt "previous_user" previous)
                    (nlk:json-object "prompt" prompt)))
         (questions (nlk:json-object "task" (nlk:json-object "type" "choice"
                                                              "instructions" +auto-question+
                                                              "criteria" (criteria-object))))
         (content (nlk:encode-json-object (nlk:json-object "model" +auto-model+
                                                           "state" state
                                                           "questions" questions))))
    (multiple-value-bind (answer status)
        (nle:http-fetch (decide-url base) :content content
                                           :headers (append (auth-headers key)
                                                            '(("content-type" . "application/json")))
                                           :timeout 2)
      (when (and answer (or (null status) (and (integerp status) (< status 400))))
        (let* ((payload (ignore-errors (nlk:decode-json answer)))
               (choice (nlk:json-value payload :text "answers" "task" "choice"))
               (confidence (nlk:json-value payload :number "answers" "task" "confidence")))
          (when (and (member choice +tasks+ :test #'equal)
                     (numberp confidence)
                     (>= confidence +auto-threshold+))
            choice))))))

;;; --- the balance --------------------------------------------------------------------

(defun euro (value)
  "VALUE as the account's own currency figure."
  (format nil "€~,2f" (coerce (or value 0) 'double-float)))

(defun number-of (object key)
  "OBJECT's KEY as a finite, non-negative number, else 0."
  (let ((value (nlk:json-value object :number key)))
    (if (and (numberp value) (>= value 0)) value 0)))

(defun balance-text (answer &optional (warn 5))
  "ANSWER, a /balance body, as the lines /hostyourai usage prints."
  (let ((data (ignore-errors (nlk:decode-json answer))))
    (if (not (hash-table-p data))
        "HostYourAI balance unavailable: an unexpected answer."
        (let* ((unlimited (and (gethash "unlimited" data) t))
               (balance (number-of data "balance"))
               (lines (list (if unlimited
                                "Balance: unlimited"
                                (format nil "Balance: ~a" (euro balance)))
                            (format nil "Spent this month: ~a" (euro (number-of data "spend_month")))
                            (format nil "Auto top-up: ~a"
                                    (if (gethash "auto_topup" data) "on" "off")))))
          (let ((as-of (nlk:json-value data :text "as_of")))
            (when as-of
              (setf lines (append lines (list (format nil "As of: ~a" (substitute #\Space #\T as-of)))))))
          (when (and (not unlimited) (plusp warn) (< balance warn))
            (setf lines (append lines (list (format nil "Low balance (below ~a)." (euro warn))))))
          (format nil "~{~a~^~%~}" lines)))))
