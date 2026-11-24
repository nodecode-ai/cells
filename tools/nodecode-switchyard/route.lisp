;;;; route.lisp --- one Switchyard decision: which skill fits, how much effort.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from bgrablin/hermes-switchyard (hermes_switchyard/routing.py,
;;;; two_stage_routing.py, reasoning_effort_adapter.py, client.py; see NOTICE).
;;;; Switchyard's decision engine is Jev: a System One request -- a state and a
;;;; map of typed questions -- answered as probabilities by TypeSafe, directly
;;;; or through OpenRouter's Decisions endpoint. The plugin asks it, on every
;;;; eligible turn, two things: which of the offered skills materially fits the
;;;; task, and which reasoning effort the next model request needs. This file
;;;; asks the same two questions in one request and reports the answer, the
;;;; way the plugin's own choice/noul primitives do.
;;;;
;;;;   {"model": "jev-latest",
;;;;    "state": {"task": "...", "candidate_count": N, "effort_cap": "medium"},
;;;;    "questions": {"needs_skill": {"type": "noul",   ...},
;;;;                  "skill":       {"type": "choice", "criteria": {...}},
;;;;                  "effort":      {"type": "choice", "criteria": {...}}}}
;;;;   => {"model": "...", "answers": {"<id>": {"type": ..., ...}}, "usage": {...}}
;;;;
;;;; The plugin's thresholds are its defaults, uncalibrated local policy and
;;;; said so here: needs_skill 0.80, the winning option 0.80, the choice's own
;;;; confidence 0.80 (routing.py DEFAULT_SKILL_*_THRESHOLD).
;;;;
;;;; What the plugin spreads over hooks, receipts and a two-stage fan-out, this
;;;; cell answers on demand: one bounded request, bounded task text (4,000
;;;; characters, automatic.py MAX_TASK_CHARS), at most 200 candidates
;;;; (routing.py _SKILL_PARTITION_SIZE), and nothing else of the session.

(in-package #:nodecode-switchyard)

(defparameter +typesafe-endpoint+ "https://api.typesafe.ai/v1/systemone"
  "TypeSafe's System One route: the plugin's TYPESAFE_ENDPOINT plus its
/v1/systemone path.")

(defparameter +openrouter-endpoint+ "https://openrouter.ai/api/alpha/decisions"
  "OpenRouter's Decisions endpoint (client.py DEFAULT_ENDPOINT).")

(defparameter +typesafe-model+ "jev-latest"
  "The model TypeSafe answers with when the section names none.")

(defparameter +openrouter-model+ "typesafe/jev-1.13"
  "The model OpenRouter answers with when the section names none.")

(defparameter +typesafe-env+ "TYPESAFE_API_KEY"
  "The environment variable a TypeSafe key is read from.")

(defparameter +openrouter-env+ "OPENROUTER_API_KEY"
  "The environment variable an OpenRouter key is read from.")

(defparameter +key-pages+
  '(("typesafe" . "https://console.typesafe.ai/keys")
    ("openrouter" . "https://openrouter.ai/keys"))
  "Where each provider's key is made.")

(defparameter +task-chars+ 4000
  "The most task text one request sends (automatic.py MAX_TASK_CHARS).")

(defparameter +candidates-max+ 200
  "The most skills one choice offers (routing.py _SKILL_PARTITION_SIZE: room
is kept for the explicit none option under Jev's 255-option contract).")

(defparameter +name-chars+ 128
  "The longest candidate name sent (automatic.py MAX_CANDIDATE_NAME_CHARS).")

(defparameter +none+ "__jev_none_of_these__"
  "The choice option standing for no candidate (routing.py _SKILL_NONE).")

(defparameter +needs-threshold+ 0.8
  "Jev's needs_skill probability under which no skill is adopted.")

(defparameter +winning-threshold+ 0.8
  "The chosen option's probability under which no skill is adopted.")

(defparameter +confidence-threshold+ 0.8
  "The choice's own confidence under which no skill is adopted.")

(defparameter +timeout+ 10
  "Seconds one attempt may take.")

(defparameter +attempts+ 3
  "How many times one decision is tried on a transient failure.")

(defparameter +efforts+ '("none" "minimal" "low" "medium" "high" "xhigh" "max")
  "The reasoning levels a request may carry, lowest first (Hermes's
VALID_REASONING_EFFORTS without the tier this image has none of).")

(defparameter +effort-criteria+
  '(("none" . "No extended reasoning; trivial lookup, ack, or formatting.")
    ("minimal" . "Very light thinking; short factual or mechanical reply.")
    ("low" . "Routine task with clear steps; little ambiguity.")
    ("medium" . "Default balanced effort for ordinary multi-step work.")
    ("high" . "Hard reasoning, debugging, or careful tradeoffs.")
    ("xhigh" . "Very hard; prior tools failed or deep analysis needed.")
    ("max" . "Maximum available effort; stuck or safety-critical judgment."))
  "What each level is for, verbatim from reasoning_effort_adapter.py
_EFFORT_CRITERIA: the rubric Jev chooses on.")

;;; --- where, which, with what -----------------------------------------------------

(defun nonblank (value)
  "VALUE trimmed when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

(defun provider-ids ()
  "The two providers, TypeSafe first: the plugin's `auto' order."
  '("typesafe" "openrouter"))

(defun provider-env (provider)
  "The environment variable PROVIDER's key is read from."
  (if (equal provider "typesafe") +typesafe-env+ +openrouter-env+))

(defun stored-key (provider &optional (path nle::*auth-file-path*))
  "The key auth.json keeps under api_keys.PROVIDER, or NIL."
  (nle::auth-api-key (ignore-errors (nle::read-auth-file path)) provider))

(defun env-key (provider)
  "The key PROVIDER's environment variable holds, or NIL."
  (nle::credential-env (provider-env provider)))

(defun key-for (provider)
  "PROVIDER's key, saved or in the environment: the saved one outranks it."
  (or (stored-key provider) (env-key provider)))

(defun api-keys ()
  "Every key this cell would send: what a refusal's text is cleaned of."
  (remove-duplicates
   (remove nil (loop for provider in (provider-ids)
                     append (list (stored-key provider) (env-key provider))))
   :test #'string=))

(defun provider ()
  "The provider that answers: the section's, or for auto the one whose key
is there -- TypeSafe first, as the plugin's jev_provider auto prefers it."
  (let ((requested (or (nonblank (setting :requested-provider)) "auto")))
    (if (equal requested "auto")
        (if (key-for "typesafe") "typesafe" "openrouter")
        requested)))

(defun api-key ()
  "(values KEY PROVIDER SOURCE) for the provider in use, or NIL: the saved
key over the environment's."
  (let ((provider (provider)))
    (alexandria:if-let (key (stored-key provider))
      (values key provider :auth)
      (alexandria:when-let (key (env-key provider))
        (values key provider :env)))))

(defun require-key ()
  "(values KEY PROVIDER): what a request sends, or the refusal naming where
to put one for the provider in use."
  (multiple-value-bind (key provider) (api-key)
    (if key
        (values key provider)
        (let ((provider (or provider (provider))))
          (fail "no key for ~(~a~): make one at ~a, then set ~a or save it in auth.json under api_keys.~a"
                provider
                (or (cdr (assoc provider +key-pages+ :test #'equal)) "the provider's console")
                (provider-env provider) provider)))))

(defun endpoint ()
  "The full URL one decision is posted to: the section's endpoint, else the
provider's own."
  (or (nonblank (setting :endpoint))
      (if (equal (provider) "typesafe") +typesafe-endpoint+ +openrouter-endpoint+)))

(defun model ()
  "The model the section names, else the provider's default."
  (or (nonblank (setting :model))
      (if (equal (provider) "typesafe") +typesafe-model+ +openrouter-model+)))

(defun label ()
  "PROVIDER and MODEL as one line: typesafe/jev-latest."
  (format nil "~a/~a" (provider) (model)))

;;; --- one exchange -----------------------------------------------------------------

(defun headers (key)
  "What every decision request carries."
  `(("authorization" . ,(format nil "Bearer ~a" key))
    ("accept" . "application/json")
    ("content-type" . "application/json")))

(defun retry-after-seconds (response-headers)
  "The integral seconds RESPONSE-HEADERS' Retry-After names, or NIL."
  (let ((value (nlk:json-value response-headers :any "retry-after")))
    (typecase value
      (integer value)
      (string (ignore-errors (parse-integer value :junk-allowed t))))))

(defun backoff (attempt response-headers)
  "Seconds to wait before attempt ATTEMPT+1: the server's hint, else 0.5
doubled per attempt; at most 5 either way (client.py's retry policy)."
  (let ((hinted (retry-after-seconds response-headers)))
    (min 5 (if hinted hinted (* 0.5 (expt 2 attempt))))))

(defun payload (state questions)
  "The request body: model, state, questions, and OpenRouter's no-fallback
provider hint where that is the endpoint."
  (let ((body (nlk:make-json-object "model" (model) "state" state "questions" questions)))
    (when (equal (endpoint) +openrouter-endpoint+)
      (setf (gethash "provider" body) (nlk:make-json-object "allow_fallbacks" :null)))
    body))

(defun exchange (state questions)
  "One decision, tried again on a transient failure: the decoded answer of
the first 2xx, or a failure naming the status and what came back."
  (let ((key (require-key))
        (content (nlk:encode-json-object (payload state questions)))
        (url (endpoint)))
    (loop for attempt from 0
          do (multiple-value-bind (body status response-headers)
                 (handler-case (nlk:http :post url :headers (headers key) :content content
                                              :timeout (setting :timeout-seconds)
                                              :connect-timeout (setting :timeout-seconds))
                   (error (condition)
                     (when (>= (1+ attempt) +attempts+)
                       (fail "~a could not be reached: ~a" (label) condition))
                     (values nil nil nil)))
               (let ((text (and body (nlk:body-text body))))
                 (cond ((null status) (sleep (backoff attempt nil)))
                       ((<= 200 status 299)
                        (return (handler-case (nlk:decode-json text)
                                  (error () (fail "~a answered with something that is not JSON" (label))))))
                       ((and (or (= status 408) (= status 429) (>= status 500))
                             (< (1+ attempt) +attempts+))
                        (sleep (backoff attempt response-headers)))
                       (t (fail "~a API error (~d): ~a" (label) status
                                (nlk:clip (nlk:one-line (or text "")) 400 :ellipsis "…")))))))))

;;; --- the questions ----------------------------------------------------------------

(defun candidate-pairs (skills)
  "SKILLS as (NAME . DESCRIPTION) pairs, checked: a name is a non-empty
string of at most 128 characters, a description its rubric when given. NIL
when SKILLS names none."
  (let ((pairs '()))
    (dolist (candidate skills)
      (let* ((name (etypecase candidate
                     (string candidate)
                     (cons (car candidate))
                     (symbol (symbol-name candidate))))
             (text (and (consp candidate) (cdr candidate)))
             (name (nonblank name)))
        (unless (and name (<= (length name) +name-chars+))
          (fail "~s is not a skill name: a non-empty string of at most ~d characters" candidate +name-chars+))
        (when (and text (not (stringp text)))
          (fail "the description of skill ~s must be a string" name))
        (push (cons name (and (nonblank text) (nlk:clip (nlk:trimmed text) 400 :ellipsis "…"))) pairs)))
    (let ((pairs (nreverse (remove-duplicates (nreverse pairs) :key #'car :test #'equal))))
      (when (> (length pairs) +candidates-max+)
        (fail "route takes at most ~d skills; you named ~d" +candidates-max+ (length pairs)))
      (when (assoc +none+ pairs :test #'equal)
        (fail "~s is reserved: the choice offers it as the none option" +none+))
      pairs)))

(defun cap-level (effort)
  "The level Jev may not go above: EFFORT when the call names one, else the
section's effort_cap -- the operator's level."
  (let ((index (or (and effort (position effort +efforts+ :test #'equal))
                   (position (setting :effort-cap) +efforts+ :test #'equal)
                   (1- (length +efforts+)))))
    (nth index +efforts+)))

(defun levels (cap)
  "The effort levels a decision may choose, lowest first: every level up to
CAP, which Jev never goes above (reasoning_effort_adapter.py: the level in
the request is the cap)."
  (subseq +efforts+ 0 (1+ (or (position cap +efforts+ :test #'equal)
                              (1- (length +efforts+))))))

(defun questions (pairs levels)
  "The question map one decision asks: which skill fits, whether one is
needed, and which level the next request should carry."
  (let ((map (make-hash-table :test 'equal)))
    (when pairs
      (let ((criteria (make-hash-table :test 'equal)))
        (dolist (pair pairs)
          (setf (gethash (car pair) criteria) (or (cdr pair) (car pair))))
        (setf (gethash +none+ criteria) "No candidate materially fits the task")
        (setf (gethash "skill" map)
              (nlk:make-json-object
               "type" "choice"
               "instructions" "Which offered skill best matches this task? Choose the none option when no skill adds material value. The result is advisory."
               "criteria" criteria))
        (setf (gethash "needs_skill" map)
              (nlk:make-json-object
               "type" "noul"
               "instructions" "Does this task need one of the offered skills?"
               "criteria" (nlk:make-json-object
                           "true" "A candidate provides specialized procedure or constraints needed for the task"
                           "false" "The task is simple or none of the candidates adds material value")))))
    (when (>= (length levels) 2)
      (let ((criteria (make-hash-table :test 'equal)))
        (dolist (level levels)
          (setf (gethash level criteria) (cdr (assoc level +effort-criteria+ :test #'equal))))
        (setf (gethash "effort" map)
              (nlk:make-json-object
               "type" "choice"
               "instructions" "Which reasoning effort should the next model request carry? The operator's level is the cap; choose lower for routine work and higher only when the task needs careful reasoning. The result is advisory."
               "criteria" criteria))))
    map))

;;; --- the answer -------------------------------------------------------------------

(defun distribution (object)
  "A probabilities OBJECT as an alist of (label . probability), in the wire's order."
  (let ((pairs '()))
    (when (hash-table-p object)
      (maphash (lambda (label value) (push (cons label value) pairs)) object))
    (nreverse pairs)))

(defun answer-of (answers id)
  "The wire answer ANSWERS carries under ID, or the refusal that it is missing."
  (or (nlk:json-value answers :object id)
      (fail "~a answered without a ~s answer" (label) id)))

(defun choose (answer)
  "ANSWER, a choice, as (:choice LABEL :probabilities ALIST :confidence C)."
  (list :choice (nlk:json-value answer :text "choice")
        :probabilities (distribution (nlk:json-value answer :object "probabilities"))
        :confidence (nlk:json-value answer :number "confidence")))

(defun decide (task &key skills effort)
  "The decision TASK, SKILLS and EFFORT make, as a plist: the plugin's two
questions answered by one request."
  (let* ((task (nonblank task))
         (pairs (candidate-pairs skills))
         (cap (cap-level (nonblank effort)))
         (levels (levels cap))
         (asked (questions pairs levels)))
    (unless task (fail "route needs a task: the request or the step the next call takes"))
    (when (> (length task) +task-chars+)
      (setf task (subseq task 0 +task-chars+)))
    (when (and (nonblank effort) (not (member (nonblank effort) +efforts+ :test #'equal)))
      (fail "~s is not a reasoning level; they are ~{~a~^, ~}" effort +efforts+))
    (when (= (length levels) 1)
      (setf (gethash "effort" asked) nil))
    (if (zerop (hash-table-count asked))
        ;; the cap is the lowest level and no skill was offered: nothing to ask
        (list :needs-skill nil :skills nil :skill nil :skill-reason nil
              :effort cap :effort-in-effect cap :provider (setting :provider)
              :model (model) :usage '(:input-tokens 0 :output-tokens 0))
    (let* ((state (nlk:make-json-object "task" task "candidate_count" (length pairs) "effort_cap" cap))
           (reply (exchange state asked))
           (answers (nlk:json-value reply :object "answers"))
           (needs (when pairs (answer-of answers "needs_skill")))
           (skill (when pairs (choose (answer-of answers "skill"))))
           (effort-answer (and (gethash "effort" asked) (choose (answer-of answers "effort"))))
           (usage (nlk:json-value reply :object "usage"))
           (needs-score (and needs (nlk:json-value needs :number "noul"))))
      (let ((verdict
              (list :needs-skill needs-score
                    :skills (loop for (name . probability) in (getf skill :probabilities)
                                  collect (list name probability)))))
        (flet ((adopted (name)
                 (let ((probability (and name (cdr (assoc name (getf skill :probabilities) :test #'equal)))))
                   (cond ((or (null name) (equal name +none+)) (values nil :none-chosen))
                         ((null needs-score) (values nil :asked-without-needs))
                         ((< needs-score +needs-threshold+) (values nil :below-needs-threshold))
                         ((or (null probability) (< probability +winning-threshold+))
                          (values nil :below-winning-probability))
                         ((< (or (getf skill :confidence) 0) +confidence-threshold+)
                          (values nil :below-choice-confidence))
                         (t (values name :adopted))))))
          (multiple-value-bind (name reason) (adopted (getf skill :choice))
            (setf (getf verdict :skill) name
                  (getf verdict :skill-reason) (and pairs reason)
                  (getf verdict :skill-confidence) (getf skill :confidence)
                  (getf verdict :effort) (or (getf effort-answer :choice) cap)
                  (getf verdict :effort-in-effect) cap
                  (getf verdict :effort-probabilities) (getf effort-answer :probabilities)
                  (getf verdict :effort-confidence) (getf effort-answer :confidence)
                  (getf verdict :provider) (provider)
                  (getf verdict :model) (format nil "~a/~a" (provider)
                                                (or (nlk:json-value reply :text "model") (model)))
                  (getf verdict :usage) (list :input-tokens (or (nlk:json-value usage :integer "input_tokens") 0)
                                              :output-tokens (or (nlk:json-value usage :integer "output_tokens") 0)))
            verdict)))))))

;;; --- the verbs ---------------------------------------------------------------------

(define-verb route (task &key skills effort)
  "Ask Switchyard which of SKILLS fits TASK, and which reasoning effort the
next model request should carry. One decision request to TypeSafe's System
One (or OpenRouter's Decisions endpoint) answers both. TASK is the request
or the step about to be taken, at most 4,000 characters. SKILLS names the
candidates -- strings, or (name . description) pairs whose description is
the rubric -- at most 200, else every skill this home keeps is offered.
EFFORT is the level now in effect, a cap the answer never exceeds. =>
a plist: :skill the name adopted (NIL when none materially fits, with
:skill-reason why), :skill-confidence, :skills ((name probability) ...),
:needs-skill the probability one is needed, :effort the level to send,
:effort-probabilities, :effort-in-effect the cap, :provider, :model, and
:usage. Advisory: nothing is loaded or changed; act on it as you see fit."
  (decide task :skills skills :effort effort))

(define-verb skills ()
  "The prose skills this home keeps, by name and one line, in the order the
index lists them: the candidates (switchyard:route TASK) offers when the
caller names none."
  (let ((entries (installed-skills)))
    (if (null entries)
        "this home keeps no prose skills: name candidates in the call, :skills '(\"name\" ...)"
        (format nil "~{~a~^~%~}"
                (loop for (name . description) in entries
                      collect (if (nonblank description)
                                  (format nil "~a - ~a" name (nlk:one-line description))
                                  name))))))

(defun installed-skills ()
  "The prose skills this home keeps, as (NAME . DESCRIPTION): the organism's
knowledge entries under DEFINE-SKILL, in index order. NIL when the image
keeps none."
  (loop for entry in (ignore-errors (nlk::visible-entries nil))
        when (and (ignore-errors (nlk::knowledge-entry-operator entry))
                  (equal "DEFINE-SKILL" (nlk::knowledge-entry-operator entry)))
          collect (cons (nlk::knowledge-entry-name entry)
                        (ignore-errors (nlk::knowledge-entry-description entry)))))
