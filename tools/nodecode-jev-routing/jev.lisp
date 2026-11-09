;;;; jev.lisp --- the four routing questions, and the verbs that answer with a tier.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Mirrors jev.py, skip.py, notice.py and turn.py of the original: one POST
;;;; /v1/systemone asking four typed questions over the prompt, a short
;;;; history excerpt, the active model and the spend snapshot; then the
;;;; answers composed into a tier and a model from the operator's own table.
;;;; A missing key, a timeout or a bad payload refuses with JEV-ERROR and
;;;; changes nothing; the caller's turn is unaffected.

(in-package #:nodecode-jev-routing)

(defparameter +base+ "https://api.typesafe.ai"
  "TypeSafe's public API root (TYPESAFE_DEFAULT_BASE_URL).")

(defparameter +route+ "/v1/systemone"
  "The judgment route under the base.")

(defparameter +env+ "TYPESAFE_API_KEY"
  "The environment variable a key is read from.")

(defparameter +key-page+ "https://console.typesafe.ai/"
  "Where a key is made.")

(defparameter +complexity-levels+
  #("Trivial: one obvious step, no design decisions, answer is known or mechanical"
    "Moderate: a few dependent steps using familiar patterns, little ambiguity"
    "Complex: multiple files or interacting constraints, real tradeoffs to weigh"
    "Architectural: cross-cutting design, high stakes, long horizon, easy to get subtly wrong")
  "The complexity question's four levels, lowest first.")

(defparameter +capability-levels+
  #("Minimal: any fast small model answers this just as well"
    "Standard: a competent mid-tier model is enough"
    "High: a strong frontier model materially improves the outcome"
    "Maximum: correctness matters more than cost; use the best available")
  "The capability question's four levels, lowest first.")

(defparameter +acknowledgements+
  '("y" "yes" "yeah" "yep" "ok" "okay" "sure" "continue" "go on" "go ahead"
    "do it" "proceed" "nice" "thanks" "thank you" "ty")
  "A prompt that says nothing new does not re-route.")

;;; --- where, which, with what -----------------------------------------------------

(defun nonblank (value)
  "VALUE trimmed when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

(defun base-url ()
  "The section's base_url, else TYPESAFE_BASE_URL, else the public root; no trailing slash."
  (string-right-trim "/" (or (nonblank (setting :base-url))
                              (nle::credential-env "TYPESAFE_BASE_URL")
                              +base+)))

(defun model ()
  "The section's model, else TYPESAFE_DEFAULT_MODEL, else jev-latest."
  (or (nonblank (setting :model))
      (nle::credential-env "TYPESAFE_DEFAULT_MODEL")
      "jev-latest"))

(defun endpoint ()
  "The one URL this cell posts to."
  (format nil "~a~a" (base-url) +route+))

(defun stored-key (&optional (path nle::*auth-file-path*))
  "The key auth.json keeps under api_keys.typesafe, or NIL."
  (nle::auth-api-key (ignore-errors (nle::read-auth-file path)) +provider+))

(defun api-key (&optional (path nle::*auth-file-path*))
  "(values KEY SOURCE): the saved key over TYPESAFE_API_KEY, as a provider's
saved key outranks its variable; NIL when neither holds one."
  (let ((stored (stored-key path)))
    (if stored
        (values stored :auth)
        (let ((variable (nle::credential-env +env+)))
          (when variable (values variable :env))))))

(defun api-keys (&optional (path nle::*auth-file-path*))
  "Every key this cell would send: what a refusal's text is cleaned of."
  (remove-duplicates (remove nil (list (stored-key path) (nle::credential-env +env+))) :test #'string=))

(defun own-base-p ()
  "Whether requests go to a base_url other than TypeSafe's public API."
  (string/= (base-url) +base+))

(defun require-key ()
  "The key a request sends; NIL to a base of the operator's own with none set."
  (or (api-key)
      (unless (own-base-p)
        (fail "no TypeSafe key: make one at ~a, then set ~a or save it in auth.json under api_keys.typesafe"
              +key-page+ +env+))))

(defun timeout-seconds ()
  "The section's timeout_ms as seconds, never under a tenth."
  (/ (max (or (setting :timeout-ms) 3500) 100) 1000d0))

;;; --- the request ------------------------------------------------------------------

(defun questions-object (router)
  "The four questions, as the wire's questions object."
  (let ((kinds (make-hash-table :test 'equal)))
    (maphash (lambda (kind text) (setf (gethash kind kinds) text)) (router-task-kinds router))
    (nlk:json-object
     "task_kind"
     (nlk:json-object
      "type" "choice"
      "instructions"
      (concatenate 'string
                   "Which single kind of work does `request` ask for? Judge the work the user "
                   "wants done, not the topic they mention. Read `conversation_excerpt` when the "
                   "request is a short follow-up that only makes sense in context. Pick the closest "
                   "kind even when the request is ambiguous.")
      "criteria" kinds)
     "complexity"
     (nlk:json-object
      "type" "score"
      "instructions"
      (concatenate 'string
                   "How hard is `request` to do well, judged only on the work itself? Use the "
                   "conversation excerpt and environment to judge scope. Ignore how much any model costs.")
      "criteria" +complexity-levels+)
     "capability_deserved"
     (nlk:json-object
      "type" "score"
      "instructions"
      (concatenate 'string
                   "Setting price aside entirely, how much model capability does this request deserve "
                   "to get a good outcome? Judge by stakes, difficulty, and how much a stronger model "
                   "would measurably improve the result.")
      "criteria" +capability-levels+)
     "needs_deep_reasoning"
     (nlk:json-object
      "type" "noul"
      "instructions"
      (concatenate 'string
                   "Does answering `request` well require extended multi-step reasoning (algorithm "
                   "design, subtle debugging, proof, careful long-horizon planning) rather than recall, "
                   "lookup, or a short direct edit?")
      "criteria"
      (nlk:json-object "true" "The work hinges on reasoning through non-obvious steps or edge cases"
                        "false" "The work is recall, lookup, formatting, or a short direct change")))))

(defun state-object (prompt &key history active-model context-tokens spend)
  "The state the questions read: the prompt, an excerpt, the environment and the budget."
  (let ((excerpt (and history (plusp (length history)) (subseq history (max 0 (- (length history) 4000))))))
    (nlk:json-object
     "request" prompt
     "conversation_excerpt" (or excerpt :null)
     "environment"
     (nlk:json-object
      "cwd" (namestring (uiop:getcwd))
      "active_model" (or (nonblank active-model) :null)
      "context_tokens_used" (or context-tokens :null))
     "budget"
     (nlk:json-object
      "spent_today_usd" (float (or (getf spend :today) 0d0) 1d0)
      "spent_this_month_usd" (float (or (getf spend :month) 0d0) 1d0)
      "daily_cap_usd" (or (getf spend :daily) :null)
      "monthly_cap_usd" (or (getf spend :monthly) :null)
      "fraction_of_budget_used" (float (or (getf spend :pressure) 0d0) 1d0)))))

(defun headers (key)
  "What the request carries: the bearer KEY when there is one."
  `(,@(when key `(("authorization" . ,(format nil "Bearer ~a" key))))
    ("accept" . "application/json")
    ("content-type" . "application/json")))

(defun post-systemone (payload)
  "One POST to /v1/systemone, its decoded answer; anything else refuses."
  (multiple-value-bind (body status response-headers)
      (handler-case
          (nlk:http :post (endpoint) :headers (headers (require-key))
                     :content (nlk:encode-json-object payload)
                     :timeout (timeout-seconds) :connect-timeout (timeout-seconds))
        (error (condition)
          (fail "jev: TypeSafe could not be reached: ~a" condition)))
    (declare (ignore response-headers))
    (progn
      (let ((text (and body (nlk:body-text body))))
        (cond ((null status) (fail "jev: TypeSafe answered nothing"))
              ((<= 200 status 299)
               (handler-case (nlk:decode-json text)
                 (error () (fail "jev: TypeSafe answered with something that is not JSON"))))
              (t (fail "jev: TypeSafe API error (~d): ~a" status
                       (nlk:clip (nlk:one-line (or text "")) 300 :ellipsis "..."))))))))

;;; --- the answers --------------------------------------------------------------------

(defun num (value)
  "VALUE as a double when it is a JSON number (a boolean is not one)."
  (and (numberp value) (float value 1d0)))

(defun distribution (object)
  "A probabilities OBJECT as an alist of (label . probability), in the wire's order."
  (let ((pairs '()))
    (when (hash-table-p object)
      (maphash (lambda (label value)
                 (when (num value) (push (cons label (float value 1d0)) pairs)))
               object))
    (nreverse pairs)))

(defun parse-analysis (payload router)
  "PAYLOAD, System One's answer, as an ANALYSIS; an answer without a known
kind or a number where one belongs refuses."
  (let* ((answers (jget payload "answers"))
         (kind (jget answers "task_kind"))
         (complexity (jget answers "complexity"))
         (capability (jget answers "capability_deserved"))
         (reasoning (jget answers "needs_deep_reasoning"))
         (chosen (or (jstr (jget kind "choice")) "chat")))
    (unless (gethash chosen (router-task-kinds router))
      (fail "Jev returned an unknown task kind: ~a" chosen))
    (make-analysis
     :kind chosen
     :complexity (or (num (jget complexity "score")) 1d0)
     :capability (or (num (jget capability "score")) 1d0)
     :deep (or (num (jget reasoning "noul")) (num (jget reasoning "noul_score")) 0d0)
     :confidence (or (num (jget kind "confidence")) 0d0)
     :probabilities (distribution (jget kind "probabilities"))
     :complexity-confidence (or (num (jget complexity "confidence")) 0d0)
     :capability-confidence (or (num (jget capability "confidence")) 0d0)
     :latency-ms (round (or (num (jget payload "latency_ms")) (num (jget payload "latencyMs")) 0d0)))))

(defun classify (prompt router &key history active-model context-tokens spend)
  "Ask Jev the four questions about PROMPT. => (values ANALYSIS BACKEND)."
  (let ((payload (post-systemone
                  (nlk:json-object
                   "state" (state-object prompt :history history :active-model active-model
                                                  :context-tokens context-tokens :spend spend)
                   "model" (model)
                   "questions" (questions-object router)))))
    (values (parse-analysis payload router)
            (format nil "~a/~a" +provider+ (or (jstr (jget payload "model")) (model))))))

;;; --- what does not re-route -----------------------------------------------------------

(defun skip-reason (text router has-history)
  "Why TEXT must not be routed, or NIL when it should be."
  (let ((trimmed (nlk:trimmed (or text ""))))
    (cond ((zerop (length trimmed)) "empty")
          ((uiop:string-prefix-p "/" trimmed) "slash command")
          ((member (string-downcase (string-right-trim ".!" trimmed)) +acknowledgements+ :test #'equal)
           "acknowledgement")
          ((and has-history (< (length trimmed) (router-min-prompt-chars router))) "short continuation"))))

;;; --- the line ------------------------------------------------------------------------

(defun decision-line (decision analysis)
  "The one line the original prints under the composer."
  (format nil "~{~a~^ · ~}"
          (remove nil
                  (list (format nil "tier ~a" (getf decision :tier))
                        (format nil "~a/~a" (getf decision :provider) (getf decision :model))
                        (let ((thinking (getf decision :thinking)))
                          (and thinking (format nil "thinking ~a" thinking)))
                        (format nil "kind ~a" (getf decision :kind))
                        (format nil "complexity ~,2f/3" (analysis-complexity analysis))
                        (format nil "capability ~,2f/3" (analysis-capability analysis))
                        (format nil "reasoning ~,2f/1" (analysis-deep analysis))))))

(defun glyph (decision active-model)
  "How the pick stands: = when it is already the active model, else . for a pick not applied."
  (if (and active-model
           (search (format nil "/~a" (getf decision :model)) (format nil "~a" active-model)))
      "="
      "."))

;;; --- the verbs -----------------------------------------------------------------------

(define-verb route (prompt &key history active-model context-tokens)
  "Name the model tier PROMPT deserves and the model your routing table holds
for it, asking TypeSafe Jev four typed questions (POST /v1/systemone). PROMPT
is the text to route; HISTORY the recent conversation, at most 4000
characters go; ACTIVE-MODEL the model the session is on now; CONTEXT-TOKENS an
estimate of the context in use. => a plist:
  (:tier \"high\" :desired-tier \"high\" :provider \"openai-codex\" :model \"gpt-6-sol\"
   :thinking \"medium\" :kind \"implement\" :demand 2.31 :confidence 0.81
   :budget-pressure 0.0 :low-confidence nil :downgraded nil :notes (\"...\")
   :glyph \".\" :line \"tier high · openai-codex/gpt-6-sol · ...\" :latency-ms 120)
  (:skipped \"acknowledgement\") for a prompt that does not re-route.
What leaves the machine is the prompt, a short excerpt, the working
directory, the active model, the context estimate and the spend snapshot;
nothing else. An empty prompt, a slash command, an acknowledgement and a
short continuation with HISTORY behind it are not sent. The tier chains, the
kind floors and the rates come from the routing JSON the jev-routing section names.
A missing key, a timeout or a bad answer signals JEV-ERROR."
  (let* ((router (load-router (config-path)))
         (text (nlk:trimmed (or prompt "")))
         (skip (or (when (string-equal (or (setting :mode) "shadow") "off") "mode off")
                   (skip-reason text router (and history (plusp (length history)))))))
    (if skip
        (progn (setf +last+ (list :prompt text :skip skip))
               (list :skipped skip))
        (let* ((spend (spend-of router))
               (snapshot (list :today (budget-spend-today spend)
                               :month (budget-spend-month spend)
                               :pressure (budget-spend-pressure spend)
                               :daily (budget-spend-daily spend)
                               :monthly (budget-spend-monthly spend)))
               (analysis (classify text router :history history :active-model active-model
                                                :context-tokens context-tokens :spend snapshot))
               (decision (decide analysis router spend)))
          (setf +last+ (list :prompt text :analysis analysis :decision decision :backend +provider+))
          (if (null decision)
              (list :tier nil :notes (list "no chain in the routing table names a model for this tier"))
              (append decision
                      (list :glyph (glyph decision active-model)
                            :line (decision-line decision analysis)
                            :latency-ms (analysis-latency-ms analysis))))))))

(define-verb judge (prompt &key history active-model context-tokens)
  "Ask Jev the four routing questions about PROMPT and answer the judgment
alone, without picking a model: the full why behind JEV:ROUTE. => a plist
(:kind \"implement\" :probabilities ((\"implement\" . 0.71) ...) :confidence 0.71
 :complexity 2.4 :complexity-confidence 0.5 :capability 2.2
 :capability-confidence 0.4 :reasoning 0.8 :latency-ms 120 :backend
 \"typesafe/jev-latest\"). It reads the routing table for the task kinds, the
endpoint, the model and the spend; nothing else. JEV-ERROR when no key,
no table, or a bad answer."
  (let* ((router (load-router (config-path)))
         (text (nlk:trimmed (or prompt "")))
         (spend (spend-of router))
         (snapshot (list :today (budget-spend-today spend) :month (budget-spend-month spend)
                         :pressure (budget-spend-pressure spend)
                         :daily (budget-spend-daily spend) :monthly (budget-spend-monthly spend))))
    (multiple-value-bind (analysis backend)
        (classify text router :history history :active-model active-model
                           :context-tokens context-tokens :spend snapshot)
      (setf +last+ (list :prompt text :analysis analysis))
      (list :kind (analysis-kind analysis)
            :probabilities (analysis-probabilities analysis)
            :confidence (analysis-confidence analysis)
            :complexity (analysis-complexity analysis)
            :complexity-confidence (analysis-complexity-confidence analysis)
            :capability (analysis-capability analysis)
            :capability-confidence (analysis-capability-confidence analysis)
            :reasoning (analysis-deep analysis)
            :latency-ms (analysis-latency-ms analysis)
            :backend backend))))

(define-verb spend ()
  "What the spend ledger beside the routing table holds: today and this
month, the two caps, and the pressure they put on the tier rule. The ledger
is filled by JEV:RECORD and read by every route. => a plist
(:today 0.42 :month 3.10 :daily-cap 10.0 :monthly-cap 150.0 :pressure 0.042
 :line \"spend today $0.42, this month $3.10 (4% of the cap)\")."
  (let* ((router (load-router (config-path)))
         (spend (spend-of router))
         (today (budget-spend-today spend))
         (month (budget-spend-month spend))
         (pressure (budget-spend-pressure spend)))
    (list :today today :month month
          :daily-cap (budget-spend-daily spend) :monthly-cap (budget-spend-monthly spend)
          :pressure pressure
          :line (format nil "spend today ~a, this month ~a~@[ (~d% of the cap)~]"
                        (usd-text today) (usd-text month)
                        (when (plusp pressure) (round (* pressure 100d0)))))))

(define-verb record (model &key provider input output usd)
  "Record one provider response in the spend ledger beside the routing table,
so the budget policy can see it: MODEL and PROVIDER name it, INPUT and
OUTPUT are its token counts. A reported USD total wins; otherwise the
operator's own rate card prices it, and an unpriced model records tokens and
no dollars rather than inventing spend. => a plist (:recorded t :cost 0.0012
:today 0.42 :month 3.10 :pressure 0.042). A write that fails changes no
route and answers :recorded nil."
  (let* ((router (load-router (config-path)))
         (cost (multiple-value-bind (ledger cost)
                   (record-spend router provider model :input input :output output :usd usd)
                 (declare (ignore ledger))
                 cost)))
    (let ((spend (spend-of router)))
      (list :recorded (and cost t)
            :cost (or cost 0d0)
            :today (budget-spend-today spend)
            :month (budget-spend-month spend)
            :pressure (budget-spend-pressure spend)))))
