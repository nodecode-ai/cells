;;;; cell.lisp --- the four decision tools, /ms-decision, and the safety gate.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of ms-decision-provider's __init__.py and provider.py (see
;;;; NOTICE). The plugin registered four tools into the Hermes registry and an
;;;; optional pre_tool_call hook; here the tools are Nodecode tools of the same
;;;; names, and the hook is the :TOOL point.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "ms-decision": {"base_url": "https://<resource>.openai.azure.com",
;;;;                   "model": "", "gate": false, "gate_mode": "approve"}
;;;; The key is auth.json's api_keys.ms-decision, else AZURE_FOUNDRY_API_KEY.

(in-package #:nodecode-ms-decision)

;;; --- the manual -----------------------------------------------------------------

(defparameter +manual+
  "Microsoft-Decision-1 (Azure AI Foundry System One) answers typed questions about one state
with a probability distribution instead of prose. Four tools, all answering the endpoint's JSON
with the whole distribution kept, so you choose the threshold:
  decision_noul    {state, question, true_label?, false_label?} -> {\"noul\": P}. The probability
      a yes/no condition holds: is this urgent, is this safe to run.
  decision_choice  {state, question, options, criteria?} -> the most probable option, every option's
      probability and a confidence: routing to a model, a team or a queue.
  decision_score   {state, question, levels} -> a probability-weighted score on ordered levels,
      lowest first, with the distribution: severity, priority.
  decide           {state, questions} -> several typed questions (noul, choice or score) about
      one state in one call, answers keyed by question id.
The same four are Lisp verbs through eval: (ms-decision:noul STATE QUESTION),
(ms-decision:choice STATE QUESTION OPTIONS), (ms-decision:score STATE QUESTION LEVELS) and
(ms-decision:decide STATE QUESTIONS), each answering that same JSON text; a state is a string.
Use them to decide, rank and classify from evidence you already hold, then apply your own
threshold; a low probability is an abstention, not a failure.
ERROR: MS-DECISION-ERROR names a refused argument; an endpoint that is unset, keyless or
unreachable answers {\"error\": ...} and sends nothing.
With the gate on, a watched tool call (eval, write_file, patch and any you list) is projected to
a tool label and an allowlisted operation word -- never its arguments -- and the endpoint scores
it; an approval, or a hard denial in block mode, follows."
  "What (help :ms-decision) answers while the cell runs.")

;;; --- the tools ------------------------------------------------------------------

(nlk:define-contract decision-noul
  (:doc "Return the probability (0-1) that a yes/no condition holds for a state. Use for binary classification and gating: is this urgent, is this safe to run? The distribution is kept; you choose the threshold.")
  ("state" :text :required t :doc "the text or state to judge")
  ("question" :text :required t :doc "the yes/no question")
  ("true_label" :string :default "true" :doc "label for the true outcome")
  ("false_label" :string :default "false" :doc "label for the false outcome"))

(defun noul-tool (call)
  "The decision_noul tool: one probability."
  (noul (decision-noul-state call) (decision-noul-question call)
        :true-label (decision-noul-true-label call)
        :false-label (decision-noul-false-label call)))

(nlk:define-contract decision-choice
  (:doc "Pick one option from a fixed set for a state, with per-option probabilities and a confidence. Use for routing: which model, team or queue. The distribution is kept; you choose the threshold.")
  ("state" :text :required t :doc "the text or state to judge")
  ("question" :text :required t :doc "the routing or classification question")
  ("options" :array :of :string :required t :doc "the candidate labels")
  ("criteria" :object :doc "optional label -> description map that steers the classifier"))

(defun choice-tool (call)
  "The decision_choice tool: one option and every option's probability."
  (choice (decision-choice-state call) (decision-choice-question call)
          (decision-choice-options call)
          :criteria (decision-choice-criteria call)))

(nlk:define-contract decision-score
  (:doc "Score a state on an ordered scale (levels low to high) with a continuous score and the distribution. Use for prioritisation and severity.")
  ("state" :text :required t :doc "the text or state to judge")
  ("question" :text :required t :doc "the scoring question")
  ("levels" :array :of :string :required t :doc "ordered levels, lowest first"))

(defun score-tool (call)
  "The decision_score tool: one score and the distribution behind it."
  (score (decision-score-state call) (decision-score-question call)
         (decision-score-levels call)))

(nlk:define-contract decide-questions
  (:doc "Ask several typed decision questions about a state in one call. questions maps an id to {type: noul, choice or score; instructions; criteria}. Answers come back keyed by id, each with its full distribution.")
  ("state" :text :required t :doc "the text or state to judge")
  ("questions" :object :required t :doc "map of question id to typed question spec"))

(defun decide-tool (call)
  "The decide tool: one call, several typed answers."
  (decide (decide-questions-state call) (decide-questions-questions call)))

;;; --- the gate ---------------------------------------------------------------------
;; The plugin's pre_tool_call hook (provider.py:267-398), on the :TOOL point. It
;; is off unless the section says `gate'; on, every watched call is projected to
;; a fixed tool label and an allowlisted operation word -- no argument, path or
;; message leaves the machine -- and the endpoint scores it. The score is
;; advisory: `approve' asks the operator, and `block' denies a call the endpoint
;; scores below block_threshold or cannot score at all.

(defparameter +watched-tools+ '("eval" "write_file" "patch")
  "The tool names the gate reads by default: Nodecode's own, where the plugin
watched Hermes's terminal, execute_code, write_file, patch and the rest
(provider.py:42-47).")

(defparameter +operations+
  '("sh" "run-program" "launch-program" "run-shell-command" "uiop:run-program"
    "write-file" "delete-file" "rename-file" "ensure-directories-exist"
    "submit" "nle:submit" "restart-cells" "load-cell" "uninstall-cell")
  "The words an eval form's head may be for the gate to name it: a fixed set the
projection is matched against, so no text of the form itself enters egress
(provider.py:280-304).")

(defparameter *allow-once* "Allow once")
(defparameter *deny* "Deny")

(defvar *gate* nil
  "Whether the gate runs: the section's `gate' to begin with, and /ms-decision
gate on or off turns it either way without a restart.")

(defun watched-p (tool-name)
  "Whether the gate reads TOOL-NAME."
  (or (member tool-name +watched-tools+ :test #'equal)
      (member tool-name (ignore-errors (setting :watch)) :test #'equal)))

(defun form-operation (arguments)
  "The allowlisted head of an eval form, lowercased, or NIL."
  (let ((form (and (hash-table-p arguments) (gethash "form" arguments))))
    (when (stringp form)
      (let* ((start (position-if-not (lambda (c) (find c " (#'")) form))
             (end (and start (position-if (lambda (c) (find c " ()\"")) form :start start)))
             (head (and start (string-downcase (subseq form start (or end (length form)))))))
        (and head (find head +operations+ :test #'equal) head)))))

(defun projection (tool-name arguments)
  "The coarse state the endpoint sees: a fixed label and an allowlisted
operation word, never an argument (provider.py:269-304)."
  (format nil "Tool call: ~a; operation: ~a"
          (if (member tool-name +watched-tools+ :test #'equal) tool-name "other_tool")
          (or (form-operation arguments) "unspecified")))

(defun safety-score (tool-name arguments)
  "The probability the call is safe to run unattended, or NIL when the endpoint
scored nothing: an unscored call is ambiguous, never permission."
  (handler-case
      (probability (noul (projection tool-name arguments)
                         "Is this operation safe to run automatically without user approval?"
                         :true-label "read-only or clearly reversible, no data loss or shared-state change"
                         :false-label "deletes or overwrites data, touches system files, sends messages, spends money, or is hard to reverse")
                   "q")
    (error () nil)))

(defun block-threshold ()
  "The safety probability under which block mode denies: the section's
block_threshold, 0.5 when it is not a number from 0 to 1."
  (let ((value (let ((*read-eval* nil))
                 (ignore-errors (read-from-string (or (setting :block-threshold) ""))))))
    (if (and (realp value) (<= 0 value 1)) (coerce value 'double-float) 0.5d0)))

(defun ask-operator-function ()
  "The core's ASK-OPERATOR, or NIL in a core that has none."
  (let ((symbol (find-symbol "ASK-OPERATOR" '#:nodecode.evolved)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun call-shown (arguments)
  "The call's arguments as one line for the operator's own screen, clipped."
  (let ((text (handler-case (nlk:encode-json-object arguments) (error () ""))))
    (if (> (length text) 1200) (concatenate 'string (subseq text 0 1200) " ...") text)))

(defun ask-before-run (tool-name arguments)
  "Ask the operator whether the call may run. => the answer, or NIL and why."
  (let ((ask (ask-operator-function)))
    (if (null ask)
        (values nil :nobody)
        (funcall ask (format nil "ms-decision: may this ~a call run?" tool-name)
                 :detail (call-shown arguments)
                 :choices (list *allow-once* *deny*)
                 :seconds (max 5 (or (ignore-errors (setting :ask-seconds)) 120))))))

(defun approval-reason (score answer why)
  "Why a watched call did not run, in the model's words."
  (cond ((equal answer *deny*) "the operator was asked and chose Deny")
        ((eq why :timeout) "the operator was asked and did not answer in time")
        ((eq why :nobody)
         (format nil "the endpoint's signal was ~a and nobody was there to approve the call"
                 (if score (format nil "~,2f" score) "unavailable")))
        (t "no approval was given")))

(defun gate-refusal (tool-name detail)
  "The model's words for a watched call the gate stopped."
  (format nil "ERROR: refused by nodecode-ms-decision: ~a did not run -- ~a; ask the operator whether they want it, and do not work around it"
          tool-name detail))

(defun gate-tool (op next)
  "The :TOOL point: OP is (:name TOOL-NAME :arguments HASH :call-id ID). A
watched call the gate stops answers a refusal and NEXT never runs."
  (let ((name (getf op :name)))
    (if (and *gate* (stringp name) (watched-p name))
        (let* ((arguments (getf op :arguments))
               (score (safety-score name arguments))
               (threshold (block-threshold)))
          (if (and (equal (setting :gate-mode) "block")
                   (or (null score) (< score threshold)))
              (nle:failure
               (gate-refusal name
                             (if score
                                 (format nil "the endpoint scored it ~,2f, under the ~,2f threshold"
                                         score threshold)
                                 "the endpoint gave no score")))
              (multiple-value-bind (answer why) (ask-before-run name arguments)
                (if (equal answer *allow-once*)
                    (funcall next op)
                    (nle:failure (gate-refusal name (approval-reason score answer why)))))))
        (funcall next op))))

;;; --- /ms-decision ------------------------------------------------------------------

(defun status ()
  "Where the key comes from, the endpoint, the model and the gate, in a line;
no network."
  (multiple-value-bind (key source) (api-key)
    (format nil "ms-decision: ~a; ~a, model ~a, gate ~a"
            (cond ((null key)
                   (format nil "no key (set ~a, or save api_keys.~a in auth.json)"
                           +key-env+ +provider+))
                  ((eq source :auth) "key from auth.json")
                  (t (format nil "key from ~a" +key-env+)))
            (or (base-url) (format nil "no endpoint (set base_url or ~a)" +base-env+))
            (model)
            (if *gate* (format nil "on (~a)" (setting :gate-mode)) "off"))))

(defun answer-error (text)
  "The error TEXT, a response body, names, or NIL when it names none."
  (nlk:json-value (ignore-errors (nlk:decode-json text)) :string "error"))

(defun check-answer ()
  "One yes/no decision asked of the endpoint: how it answered, in a line."
  (let* ((text (noul "A health check of the decision endpoint."
                      "Is this a health check?"))
         (p (probability text "q")))
    (if p
        (format nil "ms-decision: ~a answered P(yes) ~,2f" (base-url) p)
        (format nil "ms-decision: no decision: ~a" (or (answer-error text) (nlk:clip text 200))))))

(defun panel-run (label line &rest keys)
  "A row of the panel whose Enter runs LINE."
  (list* :label label :run (list :enter line) keys))

(defun panel (summary)
  "The dialog bare /ms-decision answers."
  (nle:list-dialog
   :title "Microsoft-Decision-1" :summary summary
   :rows (list (panel-run "Check the endpoint" "/ms-decision check"
                          :detail "ask one yes/no question")
               (panel-run (if *gate* "Turn the gate off" "Turn the gate on")
                          (if *gate* "/ms-decision gate off" "/ms-decision gate on")
                          :detail "watch risky tool calls and ask before they run"))))

(defun run-slash (args session-id)
  "/ms-decision typed alone: where the key comes from and the panel; check asks
one decision, gate on or off turns the safety gate."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words (or args "")))
         (verb (string-downcase (or (first words) "")))
         (rest (string-downcase (or (second words) ""))))
    (cond
      ((equal verb "") (let ((text (status))) (values text (panel text))))
      ((equal verb "status") (status))
      ((equal verb "check") (check-answer))
      ((equal verb "gate")
       (cond ((equal rest "on") (setf *gate* t) (format nil "ms-decision: gate on (~a)" (setting :gate-mode)))
             ((equal rest "off") (setf *gate* nil) "ms-decision: gate off")
             (t (format nil "ms-decision: gate is ~a; say on or off" (if *gate* "on" "off")))))
      (t (values (format nil "ms-decision: ~a is not one of its choices" verb) (panel (status)))))))

(defun install ()
  "START-CELL's work the clauses do not cover: the gate switch, and its inverse."
  (setf *gate* (not (null (setting :gate))))
  (nle:on-stop (lambda () (setf *gate* nil))))

(nle:define-cell ms-decision
  (:section ("ms-decision")
    (:guide "set base_url to your Azure OpenAI resource host and AZURE_FOUNDRY_API_KEY (or save api_keys.ms-decision in auth.json); model is the deployment name; gate on watches risky tool calls, gate_mode approve asks you and block denies a low score; ask_seconds bounds the question")
    ("base_url" :string :default ""
     :doc "https://<resource>.openai.azure.com; empty reads AZURE_FOUNDRY_BASE_URL")
    ("model" :string :default ""
     :doc "the deployment name; empty reads MS_DECISION_MODEL, else ms-decision-1")
    ("timeout_seconds" :integer :default 20 :min 1 :doc "one decision's ceiling")
    ("gate" :boolean :default nil
     :doc "watch known-risk tool calls and require approval before they run")
    ("gate_mode" :choice :options '("approve" "block") :default "approve"
     :doc "approve asks the operator; block denies a call scored under block_threshold")
    ("block_threshold" :string :default "0.5"
     :doc "the safety probability under which block mode denies, 0 to 1")
    ("watch" :list :doc "tool names the gate watches, beside eval, write_file and patch")
    ("ask_seconds" :integer :default 120 :min 5 :doc "how long a gate question waits"))
  (:help :ms-decision
   "ms-decision: Microsoft-Decision-1 typed decisions (noul, choice, score) as tools over Azure AI Foundry"
   +manual+)
  (:command "ms-decision" #'run-slash
            :description "Microsoft-Decision-1 decisions: where the key comes from, check, gate"
            :argument-hint "status|check|gate on|off")
  (:tool "decision_noul" (nlk:contract 'decision-noul) #'noul-tool)
  (:tool "decision_choice" (nlk:contract 'decision-choice) #'choice-tool)
  (:tool "decision_score" (nlk:contract 'decision-score) #'score-tool)
  (:tool "decide" (nlk:contract 'decide-questions) #'decide-tool)
  (:hook :tool #'gate-tool)
  (:start #'install))
