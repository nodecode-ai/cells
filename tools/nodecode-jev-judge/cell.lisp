;;;; cell.lisp --- the gate, the settings, /jev-judge and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A pre-tool gate, and the catalogue example of judging a call rather than
;;;; matching it: before EVAL runs, one Jev request says how destructive, how
;;;; exfiltrading and how damaging the call would be, the thresholds in code
;;;; decide, and the section's mode says what a triggered call earns.
;;;;
;;;; What it does with that answer, in the original's two modes:
;;;;
;;;;   shadow  (the default) the call runs; the decision is one JSON line
;;;;   enforce a triggered call is offered to the operator (Allow once / Deny),
;;;;           and runs only on Allow once; nobody there to ask is the refusal
;;;;
;;;; Two promises carried over from the original, both load-bearing:
;;;;
;;;; FAIL-OPEN, ALWAYS. No key, a timeout, a 4xx, a body that is not the
;;;; shape, an unanswered question, a bug in here: the call runs exactly as
;;;; it would have, and one line in the log says which it was. A gate that
;;;; can stop a turn when it is broken is worse than no gate.
;;;;
;;;; ONE LINE PER DECISION. Every judgment -- clear, flagged, escalated,
;;;; allowed, refused, failed open -- lands in a JSONL log with its reason,
;;;; so the operator can read what the judge thought instead of guessing.

(in-package #:nodecode-jev-judge)

;;; --- where the key comes from -------------------------------------------------------

(defun nonblank (value)
  "VALUE trimmed when it is text with something in it, else NIL."
  (and (stringp value)
       (plusp (length (nlk:trimmed value)))
       (nlk:trimmed value)))

(defun credential (provider env)
  "(values KEY SOURCE): the key auth.json saves under api_keys.PROVIDER, else
the one ENV names -- a saved key outranks a variable, as a provider's does."
  (let ((saved (ignore-errors
                 (nle::auth-api-key (nle::read-auth-file nle::*auth-file-path*) provider))))
    (cond ((nonblank saved) (values (nonblank saved) :auth))
          ((nle::credential-env env) (values (nle::credential-env env) :env))
          (t (values nil nil)))))

(defparameter +backends+
  '(("typesafe" "typesafe" "TYPESAFE_API_KEY")
    ("openrouter" "openrouter" "OPENROUTER_API_KEY"))
  "Each backend's name, its provider id in auth.json, and the variable a key
may come from instead.")

(defun resolve-backend (wanted)
  "The backend this configuration judges with, or NIL. autos: TypeSafe first,
then OpenRouter, as the original's auto picked by the key present."
  (let ((want (or (nonblank wanted) "auto")))
    (cond ((member want '("typesafe" "openrouter") :test #'equal)
           (when (credential (second (assoc want +backends+ :test #'equal))
                              (third (assoc want +backends+ :test #'equal)))
             want))
          (t (loop for (name provider env) in +backends+
                   when (credential provider env) return name)))))

(defun no-key-refusal (wanted)
  (format nil "backend ~a has no key: make one at ~a and set TYPESAFE_API_KEY (or save api_keys.typesafe in auth.json), or set OPENROUTER_API_KEY, or set backend to auto"
          wanted +key-page+))

;;; --- the settings the gate runs on --------------------------------------------------

(defun cell-settings (values table)
  "VALUES as the declaration derived them, plus the one resolved backend, its
URL, its key and the log file: the shape the section declaration cannot carry
because the endpoint depends on which key exists."
  (declare (ignore table))
  (let* ((wanted (or (getf values :backend) "auto"))
         (backend (resolve-backend wanted)))
    (unless backend (error 'nlk:config-refusal :detail (no-key-refusal wanted)))
    (let* ((provider (second (assoc backend +backends+ :test #'equal)))
           (env (third (assoc backend +backends+ :test #'equal))))
      (multiple-value-bind (key source) (credential provider env)
        (let* ((typesafe-base (or (nonblank (getf values :typesafe-base-url))
                                  (nle::credential-env "TYPESAFE_BASE_URL")
                                  +typesafe-base+))
               (typesafe-model (or (nonblank (getf values :typesafe-model))
                                   (nle::credential-env "TYPESAFE_DEFAULT_MODEL")
                                   +model+))
               (openrouter-base (or (nonblank (getf values :openrouter-base-url))
                                    +openrouter-base+))
               (openrouter-model (or (nonblank (getf values :openrouter-model))
                                     +openrouter-model+))
               (typesafe-p (equal backend "typesafe")))
          (append (list :backend backend
                        :key key
                        :key-source source
                        :url (if typesafe-p
                                 (concatenate 'string (string-right-trim "/" typesafe-base)
                                              +typesafe-route+)
                                 (concatenate 'string (string-right-trim "/" openrouter-base)
                                              +openrouter-route+))
                        :model (and typesafe-p typesafe-model)
                        :model-in-body (and (not typesafe-p) openrouter-model))
                  values))))))

(defun headers (key)
  "What the judgment request carries: the bearer KEY and JSON both ways."
  `(("authorization" . ,(format nil "Bearer ~a" key))
    ("accept" . "application/json")
    ("content-type" . "application/json")))

(defun timeout-seconds ()
  (min (or (setting :timeout-seconds) 3) +timeout-cap+))

;;; --- one judgment of one call -------------------------------------------------------

(defun judged (tool-name arguments)
  "(values JUDGMENT REASON): this call's judgment, from the cache or the wire,
or nothing and why the judge could not answer."
  (let* ((key (cache-key tool-name arguments (setting :backend)))
         (now (get-universal-time)))
    (multiple-value-bind (hit present) (cached key now)
      (if present
          (values hit nil)
          (let ((judgment (judge-once (build-state tool-name arguments) +gate-questions+
                                      (setting :url) (headers (setting :key))
                                      (setting :model-in-body) (setting :model)
                                      (timeout-seconds))))
            (cond ((judgment-reason judgment) (values nil (judgment-reason judgment)))
                  ((or (judgment-missing judgment) (unanswered (judgment-signals judgment)))
                   ;; Any asked question the judge did not answer is fail-open, never
                   ;; a clear verdict and never 0.0.
                   (values nil "answer_missing"))
                  (t (remember key judgment (or (setting :cache-seconds) 300) now)
                     (values judgment nil))))))))

(defparameter +asked-signals+ '("destructive" "exfiltration" "impact")
  "Every signal the gate asks for: one without an answer fails the judgment open.")

(defun unanswered (signals)
  "The asked signals SIGNALS does not carry."
  (loop for id in +asked-signals+
        unless (assoc id signals :test #'equal) collect id))

(defun signal-value (signals id)
  "The number SIGNALS carries for ID: a yes/no's probability, a score's level."
  (let ((plist (cdr (assoc id signals :test #'equal))))
    (if (equal id "impact") (getf plist :score) (getf plist :probability))))

(defun triggered (signals)
  "The signals that passed their threshold, in the gate's own order."
  (loop for (id value) on +thresholds+ by #'cddr
        for key = (string-downcase (string id))
        for measured = (signal-value signals key)
        when (and measured (>= measured value)) collect key))

(defun signal-object (signals)
  "SIGNALS as the log and the operator read them: two decimals, named."
  (apply #'nlk:make-json-object
         (loop for (id value) on +thresholds+ by #'cddr
               for key = (string-downcase (string id))
               for measured = (signal-value signals key)
               when measured append (list key (format nil "~,2f" measured)))))

(defun signal-text (signals)
  "SIGNALS in the words a refusal and a question use."
  (format nil "~{~a~^, ~}"
          (loop for (id value) on +thresholds+ by #'cddr
                for key = (string-downcase (string id))
                for measured = (signal-value signals key)
                when measured collect (format nil "~a ~,2f" key measured))))

(defun note (tool-name outcome &rest members)
  "One decision in the log: which call, which outcome, and the rest."
  (log-decision (list* (cons "source" "gate")
                       (cons "tool" tool-name)
                       (cons "mode" (setting :mode))
                       (cons "backend" (setting :backend))
                       (cons "outcome" outcome)
                       (loop for (name value) on members by #'cddr
                             collect (cons (string-downcase (string name)) value)))))

(defun gate-decision (tool-name arguments)
  "Judge the call and log it. => (:run) or (:refuse SIGNALS TRIGGERED)."
  (multiple-value-bind (judgment reason) (judged tool-name arguments)
    (if (null judgment)
        (progn (note tool-name "fail_open" :reason reason) '(:run))
        (let ((signals (judgment-signals judgment))
              (fired (triggered (judgment-signals judgment))))
          (cond ((null fired)
                 (note tool-name "clear" :signals (signal-object signals)
                                         :latency_ms (judgment-latency-ms judgment))
                 '(:run))
                ((not (equal (setting :mode) "enforce"))
                 (note tool-name "flagged_shadow" :signals (signal-object signals)
                                                 :triggered (coerce fired 'vector)
                                                 :latency_ms (judgment-latency-ms judgment))
                 '(:run))
                (t
                 (note tool-name "escalated" :signals (signal-object signals)
                                            :triggered (coerce fired 'vector))
                 (list :refuse signals fired)))))))

(defparameter +allow-once+ "Allow once")
(defparameter +deny+ "Deny")

(defun ask-operator-function ()
  "The core's ASK-OPERATOR, or NIL in a core that has none: read by name, so
this cell also loads where the question board is absent."
  (let ((symbol (find-symbol "ASK-OPERATOR" '#:nodecode.evolved)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun ask-before-run (signals fired)
  "With `ask' on, ask whether this call may run once.
=> (values ANSWER WHY), WHY :off, :nobody, :timeout or :denied."
  (let ((ask (and (setting :ask) (ask-operator-function))))
    (if (null ask)
        (values nil :off)
        (let ((answer (funcall ask (format nil "Jev flagged this call: ~{~a~^, ~}" fired)
                               :detail (format nil "~a, as Jev judged this call"
                                               (signal-text signals))
                               :choices (list +allow-once+ +deny+)
                               :seconds (max 5 (or (setting :ask-seconds) 120)))))
          (cond ((equal answer +allow-once+) (values answer nil))
                ((equal answer +deny+) (values answer :denied))
                (t (values answer :timeout)))))))

(defun refusal-text (fired signals why)
  "The model's words for a call that did not run."
  (format nil
          "ERROR: refused by nodecode-jev-judge: Jev judged this call ~{~a~^, ~} (~a)~a; the call did not run -- ~a"
          fired (signal-text signals)
          (case why
            (:denied "; the operator was asked and chose Deny")
            (:timeout "; the operator was asked and did not answer in time")
            (t ""))
          (if (eq why :off)
              "give the operator the command to run themselves, or restate it more plainly"
              "do not retry it or work around it: ask them what they want instead")))

(defun judge-or-run (op next)
  "The :TOOL point: a triggered call under `enforce' is a refusal (or a
question), everything else runs unchanged. Nothing here may raise."
  (let ((tool-name (getf op :name)))
    (if (not (and (setting :tools) (member tool-name (setting :tools) :test #'equal)))
        (funcall next op)
        (let ((decision (handler-case (gate-decision tool-name (getf op :arguments))
                          (error (condition)
                            (ignore-errors (note tool-name "fail_open"
                                                 :reason (princ-to-string condition)))
                            '(:run)))))
          (if (eq (first decision) :run)
              (funcall next op)
              (multiple-value-bind (answer why)
                  (ask-before-run (second decision) (third decision))
                (if (equal answer +allow-once+)
                    (progn (note tool-name "allowed_after_ask") (funcall next op))
                    (progn (note tool-name "refused" :why (string-downcase (string why))
                                                :triggered (coerce (third decision) 'vector))
                           (nle:failure (refusal-text (third decision) (second decision) why))))))))))

;;; --- the model's own judgments: (jev:ask state questions) --------------------------

(define-verb ask (state questions)
  "Ask the judge typed questions about one state, as the original's jev_ask
tool did. QUESTIONS is a JSON object of id -> {type, instructions, criteria},
or a plist of the same; a type of counter-intuitively bool is sent as noul.
=> (values ANSWERS LATENCY-MS), ANSWERS a plist of id -> (:probability N) for
a yes/no, (:score N :confidence C) or (:choice LABEL :confidence C)."
  (let* ((object (if (hash-table-p questions)
                      questions
                      (handler-case (nlk:decode-json (or questions "{}"))
                        (error () nil))))
         (answers (judge-once state (wire-questions object) (setting :url)
                              (headers (setting :key)) (setting :model-in-body)
                              (setting :model) (timeout-seconds))))
    (when (judgment-reason answers)
      (fail "Jev did not answer (~a); set backend, or check the key with /jev-judge check"
            (judgment-reason answers)))
    (values (loop for (id . plist) in (judgment-signals answers)
                  append (list (intern (string-upcase id) :keyword) plist))
            (judgment-latency-ms answers))))

;;; --- /jev-judge --------------------------------------------------------------------

(defun status ()
  "Where the judge is and what it judges, in a line; asks no one."
  (format nil "jev-judge: ~a mode, ~a backend (~a), judging ~{~a~^, ~}, log ~a"
          (setting :mode)
          (setting :backend)
          (case (setting :key-source)
            (:auth "key from auth.json")
            (:env "key from the environment")
            (t "no key"))
          (setting :tools)
          (log-file (setting :log-path))))

(defun check-answer ()
  "One judgment asked of a sample call: how the backend answers, in a line."
  (handler-case
      (let ((judgment (judge-once
                       (build-state "eval" (nlk:json-object "form" "(sh \"rm -rf ~/work\")"))
                       +gate-questions+ (setting :url) (headers (setting :key))
                       (setting :model-in-body) (setting :model) (timeout-seconds))))
        (if (judgment-reason judgment)
            (format nil "jev-judge: ~a did not answer (~a)" (setting :url) (judgment-reason judgment))
            (format nil "jev-judge: ~a answered in ~d ms: ~a"
                    (setting :url) (judgment-latency-ms judgment)
                    (signal-text (judgment-signals judgment)))))
    (error (condition) (format nil "jev-judge: ~a" condition))))

(defun panel (summary)
  "The dialog /jev-judge answers: the status, over the check it can run."
  (nle:list-dialog
   :title "Jev judge" :summary summary
   :rows (list (list :label "Check the judge"
                     :run (list :enter "/jev-judge check")
                     :detail "ask one judgment of a sample call"))))

(defun run-slash (args session-id)
  "/jev-judge typed alone: where it points and what it gates, and the panel;
`check' asks one judgment."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((equal verb "") (let ((text (status))) (values text (panel text))))
          ((equal verb "status") (status))
          ((equal verb "check") (check-answer))
          (t (values (format nil "jev-judge: ~a is not one of its choices (status, check)" verb)
                     (panel (status)))))))

;;; --- the manual --------------------------------------------------------------------

(defparameter +manual+
  "The nodecode-jev-judge cell gates the calls you make: before `eval' runs, one Jev judgment
says how destructive, how exfiltrating and how damaging the call would be, and the section's
mode decides what that earns. In shadow mode (the default) it only writes one JSON line per
decision; in enforce mode a triggered call is put to the operator as Allow once / Deny and
runs only on Allow once. Thresholds live in code: destructive 0.90, exfiltration 0.70,
impact 2.5 of 3. Any failure -- no key, a timeout, a strange body, an unanswered question --
is fail-open: the call runs and the log says why.
  (jev:ask STATE QUESTIONS)
      Ask the judge yourself, in the same vocabulary, for a closed decision: classify, route,
      score against a rubric, check a yes/no property. STATE is the text to judge; QUESTIONS a
      JSON object of id -> {\"type\":\"bool\"|\"choice\"|\"score\", \"instructions\":\"...\",
      \"criteria\":...}, as a JSON text or a hash. => (values ANSWERS LATENCY-MS).
      (jev:ask \"a diff\" \"{\\\"risky\\\":{\\\"type\\\":\\\"bool\\\",\\\"instructions\\\":\\\"Risky?\\\"}}\")
      => (:RISKY (:PROBABILITY 0.82d0))
/jev-judge says where the judge points and runs one check judgment. The decisions land in a
JSONL log: one line each with the outcome (clear, flagged_shadow, escalated, refused,
fail_open) and the signals behind it."
  "What (help :jev-judge) answers while the cell runs.")

(nle:define-cell jev-judge
  (:section ("jev-judge")
    (:guide "a key is required: TYPESAFE_API_KEY, or api_keys.typesafe in auth.json (OPENROUTER_API_KEY works too, on backend auto, with its own key); mode shadow only logs, enforce asks Allow once before a flagged call runs; tools names the calls it judges")
    ("mode" :choice :options '("shadow" "enforce") :default "shadow"
     :doc "shadow logs every decision and never interferes; enforce puts a triggered call to the operator, and refuses when nobody answers")
    ("tools" :list :default (vector "eval")
     :doc "the tool names the gate reads; Nodecode has one tool, eval")
    ("backend" :choice :options '("auto" "typesafe" "openrouter") :default "auto"
     :doc "auto uses the TypeSafe key when auth.json or the environment holds one, else OpenRouter; a forced backend with no key is refused at start")
    ("timeout_seconds" :integer :default 3 :min 1
     :doc "one judgment's ceiling; clamped to 10 s in code so a slow judge never stalls a call")
    ("cache_seconds" :integer :default 300 :min 0
     :doc "an identical call is judged once per this window")
    ("typesafe_base_url" :string :default ""
     :doc "the TypeSafe root; empty is TYPESAFE_BASE_URL, else https://api.typesafe.ai")
    ("typesafe_model" :string :default ""
     :doc "the judgment model; empty is TYPESAFE_DEFAULT_MODEL, else jev-latest")
    ("openrouter_base_url" :string :default "https://openrouter.ai/api/alpha"
     :doc "OpenRouter's alpha root; decisions post to <base>/decisions")
    ("openrouter_model" :string :default "~typesafe/jev-latest"
     :doc "the model slug the OpenRouter backend names in its body")
    ("ask" :boolean :default t
     :doc "in enforce mode, ask Allow once or Deny in the page before a flagged call runs; when nobody is there to ask, the call is refused")
    ("ask_seconds" :integer :default 120 :min 5 :doc "how long a question waits for an answer")
    ("log_path" :path :default ""
     :doc "the JSONL decision log; empty is logs/jev-judge.log under the Nodecode home"))
  (:settings #'cell-settings)
  (:help :jev-judge
   "jev-judge gates eval calls with one Jev judgment before they run (shadow logs, enforce asks); jev:ask asks for a typed judgment yourself"
   +manual+)
  (:hook :tool #'judge-or-run)
  (:command "jev-judge" 'run-slash
   :description "Jev judge: where it points, what it gates; check the judge"
   :session nil))
