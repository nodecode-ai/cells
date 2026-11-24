;;;; cell.lisp --- the manual, the two model tools, the gate, /nerve, the cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nerve is a supervisory layer: it asks a decision model one typed
;;;; question and reads the answer as a label with probabilities. A model
;;;; calls it for a verdict, a ranking or a check that work is done; the
;;;; cell also hooks the :TOOL point and asks the same provider whether a
;;;; call that would change something should run at all.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "nerve": {"provider": "auto", "api_key": "", "base_url": "",
;;;;             "model": "", "timeout_seconds": 10,
;;;;             "gate_mode": "off", "gate_tools": [], "gate_skip": [],
;;;;             "min_allow_percent": 90}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-nerve)

(defparameter +manual+
  "Nerve is available through the nodecode-nerve cell: a supervisory decision layer that asks a
decision model one typed question and reads the answer as a label with probabilities. Call it
through eval.
  (nerve:decide STATE INSTRUCTIONS CHOICES &key CRITERIA)
      CHOICES is two or more labels; CRITERIA an alist of (label . rubric) saying when each holds.
      => (:value \"ALLOW\" :confidence 0.93 :probabilities ((\"ALLOW\" . 0.93) (\"BLOCK\" . 0.07))
          :model \"jev-1.14\" :provider \"typesafe\" :latency-ms 412 ...)
  (nerve:rank STATE INSTRUCTIONS ITEMS)
      ITEMS an alist of (label . rubric); => (:value BEST :ranking ((label . p) ...) ...),
      highest probability first.
  (nerve:verify STATE INSTRUCTIONS)
      whether the work is done: PASS, RETRY, REPLAN or ESCALATE, with the evidence in STATE.
  (nerve:assess STATE QUESTIONS)
      one or more raw questions in one call: a plist of (id (:type \"bool\"|\"choice\"|\"score\"
      :instructions \"...\" :criteria ...)), a JSON object, or its text. => (values ANSWERS MODEL
      USAGE).
  (nerve:stats)
      what this start counted: calls, answers by value, gate reviews and refusals.
ERROR: NERVE-ERROR names the problem: no key, a base that did not answer, or an out-of-contract
answer.
The gate. With gate_mode \"advisory\" or \"enforce\" the cell also reviews tool calls that would
change something: it asks the model ALLOW, APPROVAL or BLOCK, with the call's name and arguments
(redacted: a member named like a secret, and a token inside any string, never leaves). Advisory
says the verdict as a notice and lets the call run; enforce refuses it unless the verdict is ALLOW
with p(ALLOW) at or above min_allow_percent. A read-only call is not asked about at all, and a
provider that cannot be reached lets the call run, saying so: the gate is a speed bump, not a
security boundary. gate_tools names calls to always review; gate_skip names calls never to.
/nerve says where it asks and what it has counted; /nerve enforce, /nerve advisory and /nerve off
switch the gate for this run."
  "What (help :nerve) answers while the cell runs.")

;;; --- the gate ------------------------------------------------------------------

(defparameter +gate-instructions+
  "Choose how the agent should handle this proposed tool call. ALLOW means routine and low-risk. APPROVAL means a person should explicitly approve before execution. BLOCK means the action is clearly destructive, credential-exfiltrating, privilege-escalating, or inconsistent with a safe agent action."
  "The question the gate asks, as the original asks it.")

(defparameter +gate-choices+
  '(("ALLOW" . "Routine, bounded, reversible, and consistent with normal agent work.")
    ("APPROVAL" . "Consequential, ambiguous, externally visible, hard to reverse, or confidence is insufficient.")
    ("BLOCK" . "Clearly dangerous/destructive, credential theft/exfiltration, or unacceptable blast radius."))
  "The three verdicts, and what each one holds.")

(defparameter +mutating-hint+
  (format nil "\\b(?:write|edit|update|delete|remove|create|mkdir|touch|commit|push|merge|deploy|restart|stop|send|post|publish|install|uninstall|purchase|apply|sudo|rm|mv|cp|chmod|chown|kill|truncate|drop|reset|clean|format)\\b")
  "The words that make a call worth asking about, as the original's router
reads them: whole words, so `form' is not `rm'.")

(defvar *gate-mode* nil
  "The mode /nerve set for this run; NIL reads the section's gate_mode.")

(defun gate-mode ()
  "The gate in force: what /nerve set, else the section's own."
  (or *gate-mode* (nlk:trimmed (or (setting :gate-mode) "off"))))

(defun arguments-text (value)
  "VALUE, a call's arguments, as the words it carries: a JSON object's
members and their text, everything else as it prints."
  (cond ((hash-table-p value)
         (let ((parts '()))
           (maphash (lambda (key item)
                      (push (format nil "~a ~a" key (arguments-text item)) parts))
                    value)
           (format nil "~{~a~^ ~}" (nreverse parts))))
        ((stringp value) value)
        ((vectorp value) (format nil "~{~a~^ ~}" (map 'list #'arguments-text value)))
        ((consp value) (format nil "~{~a~^ ~}" (mapcar #'arguments-text value)))
        (t (format nil "~a" value))))

(defun call-text (op)
  "Everything the call says, for the local read: its tool and its arguments."
  (format nil "~a ~a" (or (getf op :name) "") (arguments-text (getf op :arguments))))

(defun name-list (value)
  "VALUE, a list or the JSON array a section reads, as a list of strings."
  (cond ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t '())))

(defun worth-review-p (op)
  "Whether the gate asks the provider about OP: never for a call gate_skip
names, always for one gate_tools names, else when its text says it changes
something."
  (let ((name (string-downcase (or (getf op :name) "")))
        (text (string-downcase (call-text op))))
    (cond ((member name (name-list (setting :gate-skip)) :test #'string-equal) nil)
          ((member name (name-list (setting :gate-tools)) :test #'string-equal) t)
          (t (and (cl-ppcre:scan +mutating-hint+ text) t)))))

(defun min-allow ()
  "The p(ALLOW) an ALLOW verdict needs to run unattended, as a probability."
  (coerce (/ (or (setting :min-allow-percent) 90) 100) 'double-float))

(defun percent (probability)
  "PROBABILITY as whole percent, for a sentence."
  (round (* 100 probability)))

(defun refused (answer note)
  "The tool result a refused call answers: the verdict, why, and that the
call did not run, so the operator can run it themselves."
  (incf *refusals*)
  (nle:failure (format nil "ERROR: refused by nodecode-nerve: the decision model answered ~a (p=~a%)~@[ ~a~]. The call did not run -- say what it would have done, and give the operator the call to run themselves."
                         (getf answer :value)
                         (percent (probability-of (getf answer :probabilities)
                                                  (getf answer :value)))
                         note)))

(defun review-call (op next)
  "The :TOOL point: a call that changes something is put to the decision
model first. Enforce refuses anything but a confident ALLOW; advisory says the
verdict and runs the call; off, a read-only call, and an unreachable provider
all run it."
  (let ((mode (gate-mode)))
    (if (or (equal mode "off") (not (worth-review-p op)))
        (funcall next op)
        (let ((answer (handler-case
                          (progn
                            (incf *reviews*)
                            (choice-of (list :tool (getf op :name)
                                             :arguments (getf op :arguments))
                                       +gate-instructions+
                                       (mapcar #'car +gate-choices+)
                                       +gate-choices+
                                       "gate"))
                        (error (condition)
                          (nle:notice (format nil "nerve: the gate could not ask (~a); the call ran"
                                              condition)
                                      :level :warning :key "nodecode-nerve")
                          nil))))
          (cond ((null answer) (funcall next op))
                ((equal mode "advisory")
                 (nle:notice (format nil "nerve: ~a on ~a (p=~a%)"
                                     (getf answer :value) (getf op :name)
                                     (percent (probability-of (getf answer :probabilities)
                                                              (getf answer :value))))
                             :level :info :key "nodecode-nerve")
                 (funcall next op))
                ((and (equal (getf answer :value) "ALLOW")
                      (>= (probability-of (getf answer :probabilities) "ALLOW") (min-allow)))
                 (funcall next op))
                ((equal (getf answer :value) "ALLOW")
                 (refused answer "- weaker than min_allow_percent"))
                (t (refused answer nil)))))))

;;; --- the tools the model calls ------------------------------------------------------

(nlk:define-contract nerve-decide
  (:doc "Ask a decision model how to act: two or more labels, each with the rubric that says when it holds. Use it when the choice is judgement over evidence you already hold, not a task to run.")
  ("state" :text :required t
   :doc "the situation, as text: the evidence, the options, what is at stake")
  ("instructions" :text :required t
   :doc "the one question to answer, such as: which of these should the agent do next?")
  ("choices" :array :of :string :required t
   :doc "two or more labels to choose between, such as ALLOW, APPROVAL, BLOCK")
  ("criteria" :object
   :doc "one member per label, saying when that label holds; a label without one is judged on its name"))

(nlk:define-contract nerve-verify
  (:doc "Ask whether work is done: PASS, RETRY, REPLAN or ESCALATE, judged only on the evidence in the state. Use it before claiming a job finished.")
  ("state" :text :required t
   :doc "the evidence: what was asked, what was done, what the checks answered")
  ("instructions" :text :required t
   :doc "what the outcome had to satisfy, in one sentence"))

(defun criteria-alist (object)
  "OBJECT, the contract's criteria member, as the (LABEL . RUBRIC) alist
CHOICE-OF reads; a member with no rubric keeps NIL."
  (let ((pairs '()))
    (when (hash-table-p object)
      (maphash (lambda (label rubric) (push (cons label rubric) pairs)) object))
    (nreverse pairs)))

(defun choices-list (value)
  "VALUE, an array of labels the contract decoded, as a list of strings."
  (cond ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t '())))

(defun report (result)
  "RESULT, a decision plist, as the one line a tool answers."
  (let ((probabilities (getf result :probabilities)))
    (format nil "~a (confidence ~,2f; ~{~a ~,2f~^, ~}) via ~a; ~a"
            (getf result :value)
            (or (getf result :confidence) 0d0)
            (loop for (label . probability) in probabilities
                  append (list label (coerce probability 'double-float)))
            (or (getf result :model) "")
            (if (getf result :request-id)
                (format nil "request ~a" (getf result :request-id))
                "no request id"))))

(defun decide-tool (call)
  "The nerve_decide tool: the contract's verdict, as text."
  (handler-case
      (report (decide (nerve-decide-state call)
                      (nerve-decide-instructions call)
                      (choices-list (nerve-decide-choices call))
                      :criteria (criteria-alist (nerve-decide-criteria call))))
    (nerve-error (condition) (format nil "ERROR: ~a" condition))))

(defun verify-tool (call)
  "The nerve_verify tool: one rung of the ladder, as text."
  (handler-case
      (report (verify (nerve-verify-state call) (nerve-verify-instructions call)))
    (nerve-error (condition) (format nil "ERROR: ~a" condition))))

;;; --- the operator's command ----------------------------------------------------------

(defun status ()
  "Where nerve asks, the gate in force, and what this start counted; no network."
  (let ((key (api-key)))
    (format nil "nerve: ~a via ~a; ~a, gate ~a; ~d calls, ~d reviews, ~d refusals"
            (model) (base-url)
            (if key "key set"
                (if (own-base-p) "no key (own base)" "NO KEY"))
            (gate-mode) *calls* *reviews* *refusals*)))

(defun stats-line ()
  "The counters, one per line."
  (let ((stats (stats)))
    (format nil "calls ~d; reviews ~d; refusals ~d~%verdicts ~{[~a ~d]~^, ~}~%provider ~a, model ~a, base ~a"
            (getf stats :calls) (getf stats :reviews) (getf stats :refusals)
            (loop for (value . count) in (getf stats :verdicts) append (list value count))
            (getf stats :provider) (getf stats :model) (getf stats :base-url))))

(defun run-slash (args session-id)
  "/nerve: where it asks and what it counted; /nerve off, advisory or enforce
switches the gate for this run; /nerve stats prints the counters."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((equal verb "") (status))
          ((equal verb "stats") (stats-line))
          ((member verb '("off" "advisory" "enforce") :test #'equal)
           (setf *gate-mode* verb)
           (format nil "nerve: gate ~a for this run; ~a" verb (status)))
          ((equal verb "clear")
           (setf *gate-mode* nil)
           (format nil "nerve: gate reads the section again; ~a" (status)))
          (t (format nil "nerve: ~a is not one of its choices (off, advisory, enforce, stats, clear)"
                     verb)))))

(nlk:define-peripheral nerve :not-running t)

(nle:define-cell nerve
  (:section ("nerve")
    (:guide "pick a provider and put its key in the environment (TYPESAFE_API_KEY, OPENROUTER_API_KEY or OPENCODE_API_KEY), or name a base_url of your own; gate_mode off, advisory or enforce decides whether risky tool calls are put to the model before they run")
    ("provider" :choice :options '("auto" "typesafe" "openrouter" "opencode") :default "auto"
     :doc "which decisions wire answers; auto picks by the key in the environment (TypeSafe when only its key is set, else OpenRouter)")
    ("api_key" :secret :doc "the key itself, for a provider whose environment variable is not set")
    ("base_url" :string :default ""
     :doc "a root of your own, such as a System One server on this machine; empty is the provider's published one")
    ("model" :string :default ""
     :doc "the model label to ask for; empty is the provider's own default")
    ("timeout_seconds" :integer :default 10 :min 1 :doc "one decision's ceiling, in seconds")
    ("gate_mode" :choice :options '("off" "advisory" "enforce") :default "off"
     :doc "off: never ask; advisory: ask, say the verdict, run the call; enforce: refuse anything but a confident ALLOW")
    ("gate_tools" :list :doc "tool names always put to the model, whatever they say")
    ("gate_skip" :list :doc "tool names never put to the model")
    ("min_allow_percent" :integer :default 90 :min 0
     :doc "the p(ALLOW) an ALLOW verdict needs in enforce mode; below it the call is refused"))
  (:tool "nerve_decide" (nlk:contract 'nerve-decide) 'decide-tool)
  (:tool "nerve_verify" (nlk:contract 'nerve-verify) 'verify-tool)
  (:help :nerve "nerve: the supervisory decision layer - nerve:decide, nerve:rank, nerve:verify, nerve:assess, nerve:stats; the gate reviews risky tool calls" +manual+)
  (:hook :tool #'review-call)
  (:command "nerve" 'run-slash
   :description "Nerve: where it asks, what it counted, the gate's mode"
   :argument-hint "off|advisory|enforce|stats|clear")
  (:start (lambda ()
            (reset-stats)
            (setf *gate-mode* nil)
            (unless (api-key)
              (nle:notice (format nil "nerve: no key yet -- ~a runs decisions with /nerve off; set ~a or name one in the nerve section"
                                  (gate-mode) (field :env))
                          :level :info :key "nodecode-nerve"))
            (when (equal (gate-mode) "enforce")
              (nle:notice "nerve: gate enforce is on -- a tool call that changes something is put to the decision model first"
                          :level :info :key "nodecode-nerve-gate"))
            nil)))
