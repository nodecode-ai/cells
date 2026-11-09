;;;; cell.lisp --- the section, the manual, the skill_select tool and
;;;; /jev-skill-router.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a stdio MCP server with one tool: skill_select(request)
;;;; answers at most one skill name for a request, or an abstain, before the
;;;; agent loads anything. In this image the same tool is a term the model
;;;; calls, the same roster is the skills this image keeps, and the operator
;;;; reaches the same decision through /jev-skill-router.

(in-package #:nodecode-jev-skill-router)

(defparameter +manual+
  "The Jev skill router names at most one skill worth loading for a request, before anything
is loaded -- and an abstain is a valid answer.
  skill_select {request}
      The user's request, as they typed it. The answer is one JSON object: decision
      (suggest or abstain), suggested (one name or null), roster_size, gate and
      gate_values, shortlist, fits, a suggestion_block to put after the roster, and
      seconds. It costs two small Decisions calls.
  /jev-skill-router [status|TEXT]
      Typed alone it answers where the key comes from, how many skills are kept, the
      endpoint and the thresholds. With a request after it, it makes one selection now
      and says what it named.
The roster is every skill this image keeps -- what (index) lists, (help 'name) reads.
It asks TypeSafe's System One: stage one is one choice over the whole roster, on the
same one-line descriptions an index shows, plus three judgments on the request (does it
act on the user's own material, does it follow a documented procedure, would prose
suffice); stage two reranks the shortlist with each candidate's own description and the
start of its text, and one does-this-skill-do-the-specific-thing judgment each. `gate'
is the mean of the three request judgments below which nothing is suggested; `fits' is
the best judgment of the shortlist below which nothing is suggested either; `shortlist'
is how many candidates stage two sees.
Configure the section `jev-skill-router' in ~/.nodecode/config.jsonc: api_key_env names
the variable holding a TypeSafe key (make one at https://console.typesafe.ai/keys), or
adapter_url names an endpoint of your own that forwards the same {state, questions}
document, with its token in the variable adapter_token_env names. base_url and model
override the endpoint and the judgment model. Every member is optional; a call made with
no key and no token answers what to set, and spends nothing."
  "What (help :jev-skill-router) answers while the cell runs.")

(defun key-line ()
  "Where the credential comes from, in a few words, without asking anyone."
  (cond ((adapter-url)
         (if (adapter-token)
             (format nil "adapter ~a, token from ~a" (adapter-url) (setting :adapter-token-env))
             (format nil "adapter ~a, no token (~a is not set)"
                     (adapter-url) (setting :adapter-token-env))))
        ((api-key) (format nil "key from ~a" (setting :api-key-env)))
        (t (format nil "no key (~a is not set)" (setting :api-key-env)))))

(defun status-text ()
  "The settings, the roster and the thresholds, in one line."
  (format nil "jev-skill-router: ~a; ~d skill~:p kept; model ~a at ~a; gate ~a, fits ~a, shortlist ~d, excerpt ~d"
          (key-line) (length (current-roster)) (model) (endpoint-url)
          (setting :gate) (setting :fits) (setting :shortlist) (setting :excerpt)))

(defun describe-decision (result)
  "One decision as a line the operator reads."
  (let ((gate (cdr (assoc "gate" result :test #'equal)))
        (suggested (cdr (assoc "suggested" result :test #'equal)))
        (fits (cdr (assoc "fits" result :test #'equal)))
        (reason (cdr (assoc "reason" result :test #'equal)))
        (error (cdr (assoc "error" result :test #'equal))))
    (cond (error (format nil "jev-skill-router: ~a" error))
          (suggested
           (format nil "~a — fits ~,2f (gate ~,2f)~@[, shortlist ~{~a~^, ~}~]"
                   suggested
                   (or (cdr (assoc suggested fits :test #'equal)) 0.0)
                   (or gate 0.0)
                   (cdr (assoc "shortlist" result :test #'equal))))
          (t (format nil "nothing fits — ~a (gate ~,2f)"
                     (or reason "no reason given") (or gate 0.0))))))

(defun run-slash (args session-id)
  "/jev-skill-router typed alone answers status; with a request after it,
makes one selection now and says what it named."
  (declare (ignore session-id))
  (let ((text (string-trim '(#\Space #\Tab #\Newline) (or args ""))))
    (cond ((zerop (length text)) (status-text))
          ((string-equal text "status") (status-text))
          (t (handler-case (describe-decision (suggest text))
               (jev-skill-router-error (condition)
                 (format nil "jev-skill-router: ~a" condition)))))))

;;; --- the tool ---------------------------------------------------------------------

(nlk:define-contract skill-request
  (:doc "Ask which one skill fits a request, before anything is loaded. Answers at
most one name, or an abstain, as one JSON object.")
  ("request" :text :required t
   :doc "the user's request, as they typed it, verbatim"))

(defun skill-select (request)
  "The tool: one selection for REQUEST, as the JSON text the model reads."
  (select-json (skill-request-request request)))

(nle:define-cell jev-skill-router
  (:section ("jev-skill-router")
    (:guide "optional: api_key_env names the variable holding a TypeSafe key (make one at https://console.typesafe.ai/keys), or adapter_url names an endpoint of your own with its token in adapter_token_env's variable; base_url and model override the endpoint; the rest are two thresholds and two sizes; a call with no key answers what to set")
    ("api_key_env" :string :default "JEV_TYPESAFE_API_KEY"
     :doc "the environment variable holding the TypeSafe key; empty sends none")
    ("adapter_url" :string :default ""
     :doc "an endpoint of your own that forwards {state, questions} to TypeSafe; wins over base_url")
    ("adapter_token_env" :string :default "JEV_SKILL_ADAPTER_TOKEN"
     :doc "the environment variable holding the adapter's bearer token")
    ("base_url" :string :default ""
     :doc "the API root; empty is https://api.typesafe.ai")
    ("model" :string :default ""
     :doc "the judgment model; empty is jev-latest")
    ("gate" :string :default "0.30"
     :doc "the mean of the three request judgments below which nothing is suggested")
    ("fits" :string :default "0.30"
     :doc "the shortlist's best fit judgment below which nothing is suggested")
    ("shortlist" :integer :default 3 :min 1
     :doc "candidates carried into the second call")
    ("excerpt" :integer :default 700 :min 80
     :doc "characters of a candidate's own text the second call shows")
    ("timeout" :integer :default 60 :min 1
     :doc "seconds one call to the backend may take"))
  (:tool "skill_select" (nlk:contract 'skill-request) #'skill-select)
  (:help :jev-skill-router
   "jev-skill-router: skill_select names at most one kept skill that fits a request, before anything is loaded; /jev-skill-router for status and one selection"
   +manual+)
  (:command "jev-skill-router" #'run-slash
            :description "Name the one kept skill that fits a request; status without one"
            :argument-hint "[status|TEXT]"))
