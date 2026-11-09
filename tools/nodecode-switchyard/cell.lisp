;;;; cell.lisp --- the manual, the settings, /switchyard, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Switchyard is not a model and not a lane: it answers two questions the
;;;; model asks itself -- which skill fits this task, and how much reasoning
;;;; effort the next request needs -- and answers them the way the plugin did,
;;;; with one Jev decision. So it is a tool (switchyard_route), two verbs in
;;;; the switchyard: package the model calls through eval, the manual at
;;;; (help :switchyard) and /switchyard for the operator.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "switchyard": {"provider": "auto", "endpoint": "", "model": "",
;;;;                  "effort_cap": "high", "timeout_seconds": 10}
;;;; An empty endpoint is TypeSafe's System One route, or OpenRouter's
;;;; Decisions endpoint when provider resolves to openrouter; an empty model is
;;;; jev-latest, or typesafe/jev-1.13 there. The key is auth.json's
;;;; api_keys.typesafe (or .openrouter), else TYPESAFE_API_KEY (or
;;;; OPENROUTER_API_KEY).

(in-package #:nodecode-switchyard)

(defparameter +manual+
  "Switchyard's decision engine is available through the nodecode-switchyard cell:
which skill fits a task, and how much reasoning effort the next request needs, answered by one
Jev decision (TypeSafe's System One, or OpenRouter's Decisions endpoint), called through eval.
  (switchyard:route task &key skills effort)
      TASK is the request, or the step about to be taken, at most 4,000 characters -- the only
      session text that leaves this machine. SKILLS names the candidates, strings or
      (name . description) pairs whose description is the rubric, at most 200; omitted, every
      prose skill this home keeps is offered. EFFORT is the level now in effect: a cap the
      answer never exceeds, the way the plugin never raises the operator's level.
      => a plist, advisory and nothing more:
        :skill the name adopted, or NIL with :skill-reason (:none-chosen,
        :below-needs-threshold, :below-winning-probability, :below-choice-confidence),
        :skill-confidence, :skills ((name probability) ... including the none option),
        :needs-skill the probability one is needed, :effort the level to send and its
        :effort-probabilities and :effort-confidence, :effort-in-effect the cap, :provider,
        :model the backend that answered, :usage.
      Act on it as you see fit: nothing is loaded, and a rejected answer changes nothing.
  (switchyard:skills)   the prose skills this home keeps, one per line.
The switchyard_route tool asks the same in one call. ERROR: SWITCHYARD-ERROR names the
problem: no task, a bad candidate, no key, or the API's own answer."
  "What (help :switchyard) answers while the cell runs.")

;;; --- the tool -------------------------------------------------------------------

(nlk:define-contract route-request
  (:doc "Ask which skill fits this task and how much reasoning effort the next model request needs: one advisory decision from Switchyard's Jev route. Nothing is loaded; a refusal or a low probability means carry on as you were.")
  ("task" :text :required t
          :doc "the request, or the step about to be taken, at most 4,000 characters")
  ("skills" :array :of :string
            :doc "skill names to choose between; omit for every prose skill this home keeps")
  ("effort" :text
            :doc "the reasoning level now in effect: a cap the answer never exceeds"))

(defparameter +tool-name+ "switchyard_route"
  "What the model calls the decision tool.")

(defun summarize (verdict)
  "VERDICT as one line: the skill adopted or why none, the effort to send,
and the backend that answered."
  (let* ((skill (getf verdict :skill))
         (pairs (getf verdict :skills))
         (probability (and skill (second (assoc skill pairs :test #'equal))))
         (best (first (sort (copy-list pairs) #'> :key (lambda (pair) (or (second pair) 0))))))
    (format nil "skill: ~a; effort: ~a (cap ~a)~@[; needs_skill ~,2f~]; ~a"
            (if skill
                (format nil "~a (P ~,2f, confidence ~,2f)"
                        skill (or probability 0) (or (getf verdict :skill-confidence) 0))
                (format nil "none fits (~(~a~)~@[, best ~a at P ~,2f~])"
                        (getf verdict :skill-reason) (first best) (second best)))
            (getf verdict :effort) (getf verdict :effort-in-effect)
            (getf verdict :needs-skill)
            (getf verdict :model))))

(defun route-tool (request)
  "The switchyard_route tool: one decision over REQUEST's members, as one
line; a refusal is a line, never a turn's failure."
  (nlk:with-handlers ((switchyard-error (condition)
                        (format nil "switchyard: ~a" condition)))
    (summarize (route (route-request-task request)
                      :skills (coerce (or (route-request-skills request) #()) 'list)
                      :effort (route-request-effort request)))))

;;; --- /switchyard ----------------------------------------------------------------

(defun status ()
  "Where the key comes from, the provider, endpoint, model and cap, in lines; no network."
  (multiple-value-bind (key provider source) (api-key)
    (declare (ignore key))
    (format nil "switchyard: ~(~a~) ~a; model ~a; effort cap ~a~%endpoint ~a~%~a"
            (provider)
            (cond ((eq source :auth) "key from auth.json")
                  (source (format nil "key from ~a" (provider-env provider)))
                  (t "no key"))
            (label) (setting :effort-cap) (endpoint)
            (if source
                ""
                (format nil "no key: make one at ~a, set ~a, or save api_keys.~a in auth.json"
                        (or (cdr (assoc (provider) +key-pages+ :test #'equal)) "the provider's console")
                        (provider-env (provider)) (provider))))))

(defun check-answer ()
  "How the endpoint answered, in a line: one small decision, timed."
  (let ((start (get-internal-real-time)))
    (handler-case
        (let ((verdict (route "A health check of the routing cell."
                              :skills nil :effort nil)))
          (format nil "switchyard: ~a answered in ~d ms; it would send effort ~a (cap ~a)~@[; needs_skill ~,2f~]"
                  (getf verdict :model)
                  (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second)
                  (getf verdict :effort) (getf verdict :effort-in-effect)
                  (getf verdict :needs-skill)))
      (switchyard-error (condition) (format nil "switchyard: ~a" condition)))))

(defun run-slash (args session-id)
  "/switchyard typed alone: where the key comes from; `check' asks one
decision; `skills' lists the candidates this home keeps."
  (declare (ignore session-id))
  (let ((verb (string-downcase (or (nonblank args) ""))))
    (cond ((equal verb "") (status))
          ((equal verb "status") (status))
          ((equal verb "check") (check-answer))
          ((equal verb "skills") (skills))
          (t (format nil "switchyard: ~a is not one of its choices (status, check, skills)" verb)))))

;;; --- the entry: the one declaration -----------------------------------------------

(defun resolve-settings (values)
  "VALUES, the section's derived settings, with the provider the section
asked for recorded as it was written and the endpoint checked. Which provider
answers is settled per request (PROVIDER), so a key set after the start is
taken; the endpoint and model defaults follow that choice."
  (let ((requested (or (nonblank (getf values :provider)) "auto"))
        (endpoint (nonblank (getf values :endpoint))))
    (unless (member requested '("auto" "typesafe" "openrouter") :test #'equal)
      (nlk:config-error "switchyard.provider is ~s; it is auto, typesafe or openrouter" requested))
    (when (and endpoint
               (not (some (lambda (scheme) (uiop:string-prefix-p scheme endpoint))
                          '("https://" "http://"))))
      (nlk:config-error "switchyard.endpoint is ~s; an endpoint is a web address, http:// or https://"
                        endpoint))
    (setf (getf values :requested-provider) requested)
    values))

(nle:define-cell switchyard
  (:section ("switchyard")
    (:guide "no key needed to start: make one at console.typesafe.ai or openrouter.ai/keys; set TYPESAFE_API_KEY or OPENROUTER_API_KEY, or save it in auth.json under api_keys.typesafe; provider auto prefers TypeSafe; effort_cap is the level no answer goes above; /switchyard check proves it answers")
    ("provider" :choice :options '("auto" "typesafe" "openrouter") :default "auto"
     :doc "auto prefers direct TypeSafe when its key is set, else OpenRouter")
    ("endpoint" :string :default ""
     :doc "the full decision URL; empty is the resolved provider's own route")
    ("model" :string :default ""
     :doc "the Jev alias; empty is jev-latest, or typesafe/jev-1.13 on OpenRouter")
    ("effort_cap" :choice :options '("none" "minimal" "low" "medium" "high" "xhigh" "max")
     :default "high"
     :doc "the operator's reasoning level: no answer goes above it")
    ("timeout_seconds" :integer :default 10 :min 1
     :doc "one attempt's ceiling"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (resolve-settings values)))
  (:tool +tool-name+ (nlk:contract 'route-request) 'route-tool)
  (:help :switchyard "switchyard:route says which skill fits a task and what reasoning effort it needs; switchyard:skills lists the candidates" +manual+)
  (:command "switchyard" 'run-slash
   :description "Skill and effort routing: where the key comes from; check it"
   :argument-hint "status | check | skills"
   :session nil))
