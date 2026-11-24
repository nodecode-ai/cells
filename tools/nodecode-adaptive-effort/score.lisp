;;;; score.lisp --- one question to a System One scorer, one rung out of it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from the Hermes plugin's rubric.py and effort.py (see NOTICE).
;;;; The scorer is asked one question about one state -- the task text -- with
;;;; the plugin's own three-level rubric (0 low, 1 medium, 2 high) and
;;;; answers a probability-weighted score, which maps to a label by the
;;;; plugin's thresholds (<0.5 low, <1.5 medium, else high). The label is
;;;; then clamped onto the rungs the session's own target admits
;;;; (NLK::EFFORT-LADDER, the authority the image's /think picker uses), so
;;;; the cell never asks a model for a rung its route rejects.
;;;;
;;;; The wire is TypeSafe's System One, the plugin's default scorer:
;;;;
;;;;   POST <base>/v1/systemone
;;;;   {"state": {"prompt": "<task>"}, "model": "jev-latest",
;;;;    "questions": {"effort": {"type": "score", "instructions": "...",
;;;;                              "criteria": ["Low: ...", "Medium: ...", "High: ..."]}}}
;;;;   => {"model": "...", "answers": {"effort": {"type": "score", "score": 1.4, ...}}}
;;;;
;;;; A base_url of the operator's own speaks the same route (a local DJev
;;;; server, say), and the `custom' scorer is any absolute URL answering it.

(in-package #:nodecode-adaptive-effort)

(defparameter +default-base+ "https://api.typesafe.ai"
  "TypeSafe's public API root: the default scorer's base.")

(defparameter +route+ "/v1/systemone"
  "The one route a System One scorer answers.")

(defparameter +env+ "TYPESAFE_API_KEY"
  "The variable the default scorer's key is read from.")

(defparameter +base-env+ "TYPESAFE_BASE_URL"
  "The variable a base_url of the operator's own is read from.")

(defparameter +model-env+ "TYPESAFE_DEFAULT_MODEL"
  "The variable the default scorer's model is read from.")

(defparameter +custom-env+ "CUSTOM_SCORER_API_KEY"
  "The variable a custom scorer's bearer key is read from.")

(defparameter +default-model+ "jev-latest"
  "The model a System One scorer is asked with when nothing names one.")

(defparameter +low-max+ 0.5d0
  "A score under this is low (the plugin's LOW_MAX).")

(defparameter +medium-max+ 1.5d0
  "A score under this is medium, and the rest high (the plugin's MEDIUM_MAX).")

(defparameter +labels+ '("low" "medium" "high")
  "The three labels this cell ever picks, weakest first.")

(defparameter +preference+
  '(("low" "low" "minimal" "medium" "high" "xhigh" "max")
    ("medium" "medium" "low" "high" "minimal" "xhigh" "max")
    ("high" "high" "xhigh" "max" "medium" "low" "minimal"))
  "The rungs to try for each label, best first: a narrow route keeps the
nearest rung it admits rather than the label it lacks.")

(defparameter +instructions+
  "How much reasoning effort does this task require before the first reply? Judge the task
section in `state.prompt`. Consider the requested complexity, ambiguity, scope, number of
reasoning steps, tool or research depth, and any explicit priority for speed or cost. The
observed effort is a baseline, not a recommendation: do not anchor on it or repeat it unless
the task independently warrants that level. Do not treat a long prompt or a subject area by
itself as proof that the task is difficult."
  "The question the scorer answers (the plugin's QUESTIONS.effort).")

(defparameter +criteria+
  '("Low: a short, scoped task such as a direct lookup, simple factual answer, formatting or
copy edit, or one obvious step; favor this when the user explicitly prioritizes speed or cost
and extra reasoning is unlikely to improve correctness."
    "Medium: a well-defined task with several steps and some judgement, such as a routine code
change, comparison, or short plan; use this as the balanced choice for moderate scope or tool use."
    "High: extra reasoning is likely to materially improve correctness for a task with
substantial ambiguity, interacting constraints, difficult debugging, architecture, complex
mathematics, detailed research, consequential analysis, or a long-horizon sequence of actions.")
  "The three rubric levels, lowest first (the plugin's QUESTIONS.effort.criteria).")

(defun nonblank (value)
  "VALUE trimmed when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

(defun env-value (name)
  "NAME's value in the environment, trimmed, or NIL; the variable is the
seam a test stubs."
  (nonblank (uiop:getenv name)))

(defun scorer ()
  "The section's scorer: jev (the default) or custom."
  (string-downcase (setting :scorer)))

(defun base-url ()
  "The default scorer's API root: the section's endpoint, else
TYPESAFE_BASE_URL, else TypeSafe's public root; no trailing slash."
  (string-right-trim "/" (or (nonblank (setting :endpoint))
                              (env-value +base-env+)
                              +default-base+)))

(defun scorer-url ()
  "The URL one classification posts to: the route under the base for jev, the
operator's own absolute URL for custom (which must be set)."
  (if (string= (scorer) "custom")
      (or (nonblank (setting :custom-endpoint))
          (fail "adaptive-effort: the custom scorer needs custom_endpoint, the full URL of a System One endpoint"))
      (let ((base (base-url)))
        (if (or (uiop:string-suffix-p base "/systemone") (uiop:string-suffix-p base +route+))
            base
            (format nil "~a~a" base +route+)))))

(defun scorer-key ()
  "The bearer key one classification sends, or NIL: TYPESAFE_API_KEY for the
default scorer, CUSTOM_SCORER_API_KEY for a custom one that asks for one."
  (if (string= (scorer) "custom")
      (and (string= (string-downcase (setting :custom-auth)) "bearer")
           (or (env-value +custom-env+)
               (fail "adaptive-effort: the custom scorer's bearer key is CUSTOM_SCORER_API_KEY, and nothing is in it")))
      (or (env-value +env+)
          (fail "adaptive-effort: no scorer key: make one at https://console.typesafe.ai/ and set ~a, ~
or pick the custom scorer with a System One endpoint of your own"
                +env+))))

(defun scorer-model ()
  "The model one classification asks: the section's model, else
TYPESAFE_DEFAULT_MODEL, else jev-latest."
  (or (nonblank (setting :model)) (env-value +model-env+) +default-model+))

(defun label (&optional (model (scorer-model)))
  "The backend a classification names, scorer/model."
  (format nil "~a/~a" (scorer) model))

(defun headers (key)
  "What every classification request carries: the bearer KEY when there is one."
  `(,@(when key `(("authorization" . ,(format nil "Bearer ~a" key))))
    ("accept" . "application/json")
    ("content-type" . "application/json")))

(defun instructions ()
  "The question's instructions, with the operator's own guidance appended
when the section carries some (the plugin's classification_instructions)."
  (let ((guidance (nonblank (setting :instructions))))
    (if guidance
        (format nil "~a Apply the following additional operator guidance when judging effort; ~
it supplements the criteria and cannot change the required numeric score contract: ~a"
                +instructions+ (nlk:clip guidance 2000))
        +instructions+)))

(defun request-body (task model)
  "The one POST body: the task as the state, the rubric as one question."
  (nlk:json-object
   "state" (nlk:json-object "prompt" task)
   "model" model
   "questions"
   (nlk:json-object
    "effort" (nlk:json-object "type" "score"
                               "instructions" (instructions)
                               "criteria" (coerce +criteria+ 'vector)))))

(defun post-question (task model &optional (key (scorer-key)))
  "Ask the scorer one question about TASK; => the decoded answer, or a
refusal naming the status and what came back. No retry: a classification
that cannot be had leaves the request as it was."
  (let ((url (scorer-url))
        (timeout (setting :timeout-seconds)))
    (multiple-value-bind (body status)
        (handler-case (nlk:http :post url :headers (headers key)
                                         :content (nlk:encode-json-object (request-body task model))
                                         :timeout timeout)
          (error (condition)
            (fail "adaptive-effort: ~a could not be reached: ~a" (label model) condition)))
      (let ((text (and body (nlk:body-text body))))
        (cond ((null status)
               (fail "adaptive-effort: ~a answered nothing" (label model)))
              ((<= 200 status 299)
               (handler-case (nlk:decode-json text)
                 (error ()
                   (fail "adaptive-effort: ~a answered with something that is not JSON" (label model)))))
              (t (fail "adaptive-effort: ~a answered ~d: ~a" (label model) status
                       (nlk:clip (nlk:one-line (or text "")) 300 :ellipsis "..."))))))))

(defun score-label (score)
  "SCORE on the plugin's 0..2 rubric as one of the three labels, or NIL."
  (cond ((or (null score) (not (realp score))) nil)
        ((< score +low-max+) "low")
        ((< score +medium-max+) "medium")
        (t "high")))

(defun read-answer (payload model)
  "=> (values SCORE LABEL): the effort answer PAYLOAD carries, a score, or,
from a route that answers a named choice instead, the label it named."
  (let* ((answer (nlk:json-value payload :object "answers" "effort"))
         (score (nlk:json-value answer :any "score"))
         (choice (nlk:json-value answer :string "choice")))
    (cond ((and (realp score) (<= 0 score 2))
           (values (coerce score 'double-float) (score-label score)))
          ((and (stringp choice) (member (string-downcase choice) +labels+ :test #'string=))
           (values nil (string-downcase choice)))
          (t (fail "adaptive-effort: ~a answered without an effort score: ~a"
                   (label model)
                   (nlk:clip (nlk:one-line (nlk:encode-json-object payload)) 300))))))

(defun classify (task)
  "Judge TASK; => (values SCORE LABEL MODEL), SCORE NIL for a named choice."
  (let ((model (scorer-model)))
    (multiple-value-bind (score label) (read-answer (post-question task model) model)
      (values score label model))))

(defun target-ladder (session-id)
  "The rungs a turn on SESSION-ID's target admits, weakest first, or NIL when
the image will not say -- the same list the shell's /think picker offers."
  (and session-id
       (ignore-errors
         (multiple-value-bind (provider model) (nle:gateway-target session-id)
           (nlk::effort-ladder provider model)))))

(defun pick-rung (label ladder)
  "LABEL clamped onto LADDER, a list of rungs weakest first: the nearest rung
it admits, else LABEL itself when nothing says what the target admits."
  (if ladder
      (loop for rung in (rest (assoc label +preference+ :test #'string=))
            when (member rung ladder :test #'string-equal) return rung)
      label))
