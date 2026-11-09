;;;; cell.lisp --- the section, the manual, /skill-router, and the round hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a pre_llm_call hook: before the model answers, the
;;;; request is read and the one skill from the live roster that fits it is
;;;; named on the user message, or nothing is said. In this image the same
;;;; seam is the :ROUND point, and the line it names rides as the
;;;; <system-reminder> of the round about to be asked: a fact recorded
;;;; there is in that round's request, so the model reads the suggestion
;;;; before it picks anything. It never touches the system prompt, and an
;;;; empty answer costs the turn nothing.

(in-package #:nodecode-skill-router)

(defparameter +manual+
  "The skill router names the one skill worth loading, before the model call. While the cell
runs, each turn's first round reads the request and, when a skill this image keeps genuinely
fits it, one line goes into that round:
  <skill_relevance>
  Relevant to the current request: <skill>. Ignore this if it does not fit what the user
  actually asked for.
  </skill_relevance>
Nothing at all is added when nothing fits, when the request is a slash command, when it is
empty or longer than suggest_chars, or when no key is configured. The roster is the skills
this image keeps -- (index) lists the same ones, (help 'name) reads one.
  /skill-router status        where the key comes from, the roster size, the thresholds
  /skill-router <text>        route one request now and say what it named, without asking a model
It asks TypeSafe's System One (POST /v1/systemone): stage one puts the whole roster to one
choice, in chunks of `chunk' because the API caps a question at 255 options, plus three
judgments on the request; stage two puts the shortlist to one choice and one
does-this-skill-do-the-specific-thing judgment each. `gate' is the mean of the three request
judgments below which nothing is suggested; `fits' is the winner's own judgment below which
nothing is injected. The key never reaches the model.
Configure the section `skill-router' in ~/.nodecode/config.jsonc: api_key_env names the
variable holding the key, base_url and model override the endpoint, and the rest are the two
thresholds and the two sizes. Every one is optional; with no key the cell still starts and
stays quiet."
  "What (help :skill-router) answers while the cell runs.")

(defun current-roster ()
  "The roster a call ranks: what *ROSTER* holds, else the skills this image keeps."
  (cond ((null *roster*) (skill-roster))
        ((functionp *roster*) (funcall *roster*))
        (t *roster*)))

(defun key-line ()
  "Where the key comes from, in a few words, without asking anyone."
  (cond ((api-key) (format nil "key from ~a" (setting :api-key-env)))
        ((plusp (length (setting :base-url))) "no key, a base of its own")
        (t (format nil "no key (~a is not set)" (setting :api-key-env)))))

(defun status-text ()
  "The settings, the roster and the thresholds, in one line."
  (format nil "skill-router: ~a; ~d skill~:p kept; model ~a at ~a; gate ~a, fits ~a, shortlist ~d, excerpt ~d"
          (key-line) (length (current-roster)) (model) (base-url)
          (setting :gate) (setting :fits) (setting :shortlist) (setting :excerpt)))

(defun describe-result (result)
  "One routing decision as a line the operator reads."
  (if (getf result :skill)
      (format nil "~a — ~a (gate ~,2f)~@[, chunks ~d~]"
              (getf result :skill) (getf result :reason) (or (getf result :gate) 0.0)
              (getf result :chunks))
      (format nil "nothing fits — ~a" (or (getf result :reason) "no reason given"))))

(defun run-slash (args session-id)
  "/skill-router typed alone answers status; with a request after it, routes
that request now and says what it named."
  (declare (ignore session-id))
  (let ((text (string-trim " " (or args ""))))
    (cond ((zerop (length text)) (status-text))
          ((string-equal text "status") (status-text))
          (t (handler-case (describe-result (route-request text))
               (skill-router-error (condition) (format nil "skill-router: ~a" condition)))))))

(defun observe-round (op next)
  "The :ROUND point: op is (:session-id ID :turn TURN :round N). At a turn's
first round the skill that fits it is recorded as that round's reminder.
NEXT runs first, and a router failure is silent: it must never break a turn."
  (prog1 (funcall next op)
    (handler-case (record-suggestion (getf op :turn) (getf op :round))
      (error () nil))))

(nle:define-cell skill-router
  (:section ("skill-router")
    (:guide "optional: api_key_env names the variable holding a TypeSafe key (make one at https://console.typesafe.ai/), base_url and model override the endpoint, and the rest are two thresholds and two sizes; with no key the cell still starts and says nothing")
    ("api_key_env" :string :default "TYPESAFE_API_KEY"
     :doc "the environment variable holding the TypeSafe key; empty sends none")
    ("base_url" :string :default ""
     :doc "the API root; empty is https://api.typesafe.ai")
    ("model" :string :default ""
     :doc "the judgment model; empty is jev-latest")
    ("gate" :string :default "0.30"
     :doc "the mean of the three request judgments below which nothing is suggested")
    ("fits" :string :default "0.40"
     :doc "the winner's own fit judgment below which nothing is injected")
    ("shortlist" :integer :default 3 :min 1
     :doc "candidates carried into the second request")
    ("chunk" :integer :default 240 :min 1
     :doc "skills per request; the API caps one choice at 255 options")
    ("excerpt" :integer :default 700 :min 80
     :doc "characters of a candidate's own text the second request shows")
    ("timeout" :integer :default 10 :min 1
     :doc "seconds one request may take")
    ("suggest_chars" :integer :default 4000 :min 0
     :doc "requests longer than this are left alone; 0 routes every length"))
  (:help :skill-router
   "skill-router: at each turn's first round, the one kept skill that fits the request is named; /skill-router for status and one-off routing"
   +manual+)
  (:hook :round #'observe-round)
  (:command "skill-router" 'run-slash
            :description "Name the one kept skill that fits a request; status without one"
            :argument-hint "[status|TEXT]"))
