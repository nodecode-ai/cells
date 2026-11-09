;;;; cell.lisp --- the manual, the section, /structured-aux.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A decision endpoint answers labels, not conversations, so this cell is no
;;;; lane and no catalog row: it is four functions in the structured-aux:
;;;; package the model calls through eval, the way it reaches the web through
;;;; web:search. While the cell runs, (help :structured-aux) answers the
;;;; manual, and every request's help section carries one line naming them.
;;;; /structured-aux says where the key comes from, and checks it.
;;;;
;;;; Rewritten from hermes-structured-aux-models: the original registered a
;;;; Hermes model-provider profile whose in-process client served three
;;;; auxiliary tasks (approval, MCP sampling, compression). Nodecode has no
;;;; auxiliary-task seam, so the same three contracts, and the transport under
;;;; them, are verbs a turn reaches for itself.

(in-package #:nodecode-structured-aux)

(defparameter +manual+
  "Bounded decisions are available through the nodecode-structured-aux cell: a
choice among labels you name in advance, answered with a probability by a
decision model on OpenRouter's /api/alpha/decisions, called through eval.
  (structured-aux:approve command &key policy model)
      Judge a shell command, or a description of what will run: :APPROVE, :DENY, :ESCALATE
      (:UNKNOWN when the provider answered outside the three -- never consent). => (values
      VERDICT CONFIDENCE PROBABILITIES MODEL).
  (structured-aux:pick-tool request candidates &key model)
      CANDIDATES: a list of tool names, or a plist of name and description, two or more.
      => (values NAME CONFIDENCE PROBABILITIES MODEL).
  (structured-aux:compress text &key max-blocks model)
      A digest built only from retained original blocks, under a provenance header; nothing
      is generated. => (values DIGEST KEPT TOTAL).
  (structured-aux:decide state questions &key model)
      The general form: STATE a string or a JSON object or array, QUESTIONS a plist
      (:id (:instructions TEXT :criteria ((LABEL . RUBRIC-OR-NIL) ...))), two or more
      labels each. => (values ANSWERS MODEL USAGE), ANSWERS a plist of each id's keyword
      and (:type :choice :choice LABEL :probabilities ((LABEL . P) ...) :confidence C).
Use them to decide, classify or compress from evidence you already hold: each call sends
only the state you pass, with credential-shaped text redacted. A request it cannot express
as a bounded choice is refused, not guessed, and an answer outside the contract fails.
ERROR: STRUCTURED-AUX-ERROR names the problem: a refused question, no key, or the
provider's own answer."
  "What (help :structured-aux) answers while the cell runs.")

;;; --- /structured-aux ----------------------------------------------------------------

(defun status ()
  "Where the key comes from, the endpoint and the model, in a line; no network."
  (multiple-value-bind (key source) (api-key)
    (format nil "structured-aux: ~a; ~a, model ~a"
            (cond ((null key)
                   (format nil "no key (make one at ~a, then set ~a or save api_keys.openrouter in auth.json)"
                           +key-page+ +env+))
                  ((eq source :auth) "key from auth.json")
                  (t (format nil "key from ~a" +env+)))
            (endpoint)
            (or (nonblank (setting :model)) +default-model+))))

(defun check-answer ()
  "How the endpoint answered, in a line: one bounded decision asked of it."
  (handler-case
      (let ((start (get-internal-real-time)))
        (multiple-value-bind (answers model usage)
            (ask (nlk:make-json-object "request" "A health check of the decision provider.")
                 (list (cons "up" (choice-question
                                     "Is this request a health check?"
                                     '(("YES" . "it is a health check")
                                       ("NO" . "it is something else"))))))
            (declare (ignore usage))
            (format nil "structured-aux: ~a answered as ~a in ~d ms (chose ~a, confidence ~,2f)"
                    (endpoint) model
                    (round (* 1000 (- (get-internal-real-time) start))
                           internal-time-units-per-second)
                    (getf (getf answers :up) :choice)
                    (or (getf (getf answers :up) :confidence) 0.0))))
    (structured-aux-error (condition)
      (format nil "structured-aux: ~a" condition))))

(defun check ()
  "CHECK-ANSWER on a thread of its own, said once as a notice."
  (if (null (api-key))
      (status)
      (let ((settings *structured-aux*))
        (bt2:make-thread
         (lambda ()
           (let ((*structured-aux* settings))
             (nle:notice (check-answer) :level :info)))
         :name "nodecode-structured-aux check")
        "structured-aux: asking the decision provider one question")))

(defun panel-row (label line &rest keys)
  "A row of the panel whose Enter runs LINE, KEYS the rest of a dialog row's words."
  (list* :label label :run (list :enter line) keys))

(defun panel (summary)
  "The dialog bare /structured-aux answers: SUMMARY, where the key comes from,
over the check it runs on asking."
  (nle:list-dialog
   :title "Structured aux" :summary summary
   :rows (list (panel-row "Check the key" "/structured-aux check"
                          :detail "ask the decision provider one bounded question"))))

(defun run-slash (args session-id)
  "/structured-aux typed alone: where the key comes from, and its panel; a row
runs one of its lines (status, check)."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((equal verb "") (let ((text (status))) (values text (panel text))))
          ((equal verb "status") (status))
          ((equal verb "check") (check))
          (t (values (format nil "structured-aux: ~a is not one of its choices (status, check)" verb)
                     (panel (status)))))))

(nle:define-cell structured-aux
  (:section ("structured-aux")
    (:guide "make a key at https://openrouter.ai/settings/keys, then set OPENROUTER_API_KEY or save it with /connect (api_keys.openrouter, where the openrouter cell's sign-in writes it); base_url, path and model are optional; every call needs https; redact cleans credential-shaped text out of what is sent")
    ("base_url" :string :default "https://openrouter.ai"
     :doc "the decision API root; https only")
    ("path" :string :default "/api/alpha/decisions"
     :doc "the decision route under it")
    ("model" :string :default "~typesafe/jev-latest"
     :doc "the model that answers; pin an exact id instead of the moving alias")
    ("timeout_seconds" :integer :default 15 :min 1
     :doc "one decision's ceiling")
    ("max_blocks" :integer :default 48 :min 2
     :doc "how many blocks a digest may ask about")
    ("blocks_per_call" :integer :default 16 :min 2
     :doc "how many KEEP/DROP questions ride one call")
    ("digest_budget_chars" :integer :default 6000 :min 400
     :doc "the digest's ceiling in characters")
    ("app_referer" :string :default "https://nodecode.ai"
     :doc "the attribution header OpenRouter shows for this app")
    ("app_title" :string :default "nodecode-structured-aux"
     :doc "the app name that header carries")
    ("redact" :boolean :default t
     :doc "strip credential-shaped text before it is sent"))
  (:help :structured-aux
   "structured-aux:approve judges a command, pick-tool chooses among tools, compress digests text, decide asks any bounded choice"
   +manual+)
  (:command "structured-aux" 'run-slash
   :description "OpenRouter decisions: where the key comes from; check it"
   :session nil))
