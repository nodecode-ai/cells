;;;; cell.lisp --- the primer, the section, the tool, the /ux3 command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin is a skill plus two scripts. The skill is +PRIMER+,
;;;; which the model reads with (help :ux3) and whose one line rides every
;;;; request; the scripts' gate and its validator are kernel.lisp. What the
;;;; model calls is the ux3_gate tool, /ux3, and the three verbs through
;;;; eval -- no model is served here, so nothing touches the provider path.

(in-package #:nodecode-ux3)

(defparameter +primer+
  "UX3 reviews a product decision before it is built and again at its next iteration: three flows,
reviewed one at a time and never averaged, then one verdict that bounds what may happen next.
  user_flow      who it is for, who else is affected, what they are trying to accomplish, what it
                 costs them or takes from their control now
  evidence_flow  what was observed, where it came from, what interpretation is being made, which
                 counter-signal weakens it, and what changes if it is wrong
  business_flow  who creates value, who receives it, who pays or decides, who carries the cost and
                 the risk, and whether the exchange stays viable and legitimate
Then the intersections: situated understanding, viable learning, sustainable value exchange.
Evidence tiers, weakest first: t0 internal belief with no traceable source; t1 a stated opinion or
an isolated signal; t2 an interview, an observed workaround or a small test; t3 repeated behaviour,
adoption, payment or logs; t4 durable retention, workflow dependency or governance proof. The
tier bounds the action, and confidence never upgrades it: t0 forces stop_reframe and t1 can never
return continue.
One verdict, from the weakest lane: continue (all three continue) means write a context pack inside
the execution boundary, and that is the one branch that authorizes implementation; verify means run
the one proof step and nothing else; stop_reframe means stop and return one better product question.
The weakest flow is chosen deterministically: the most severe verdict, then the lowest evidence
tier, then evidence before user before business.
The verbs, in eval:
  (ux3:route \"prompt\" :files-changed 3)          where a task routes: routine, discovery or commitment
  (ux3:evaluate \"prompt\" :evidence-tier \"t2\")   the three lane verdicts, one combined verdict, the next step
  (ux3:decide '((:user \"verify\" \"t2\") (:evidence \"stop_reframe\" \"t1\") (:business \"continue\" \"t3\"))
              :may-do '(...) :must-not-do '(...))      a review you wrote lane by lane, reduced to the
                                                 canonical verdict, weakest flow and headline tier,
                                                 with every problem the original's validator raises
All three answer a plist; (ux3:gate-text ...) and (ux3:review-text ...) render one as prose. The
ux3_gate tool runs the baseline on a prompt, and /ux3 runs it on one line you type.
How to work: review before implementing, and do not implement during review. Each lane names its
strongest signal, its weakest assumption, the evidence it is missing, the disconfirming evidence it
looked for (first, before the support), and the condition that would stop it. Every result states
may_do, must_not_do and must_ask. Reversibility lowers the mode, never the review requirement. A
human-owned call (designer taste, a trade-off the operator owns) is recorded and stays human-owned;
it is not scored and does not become evidence. Post-build feedback is classified before it changes
direction."
  "What (help :ux3) answers while the cell runs: the original's skill, in the cell's voice.")

;;; --- the tool ----------------------------------------------------------------

(nlk:define-contract ux3-gate
  (:doc "Judge a product direction, feature, workflow or experiment before it is built, and again at its next iteration: the route, the three lane verdicts (user, evidence, business), one combined verdict, the weakest flow and the next step. Advisory: it says what to verify next, not what to build. For a review you wrote lane by lane, call (ux3:decide ...) in eval instead.")
  ("prompt" :text :required t
   :doc "the direction, feature or experiment to judge, in one or two sentences")
  ("evidence_tier" :string :options '("t0" "t1" "t2" "t3" "t4") :slot evidence-tier
   :doc "the strongest evidence behind it: t0 internal belief, t1 stated opinion, t2 an interview or a small test, t3 repeated behaviour or payment, t4 durable proof. Absent reads the section's default")
  ("files_changed" :integer :slot files-changed
   :doc "how many files the change touches; past five the baseline routes it as a commitment"))

(defun run-gate (call)
  "ux3_gate: the baseline gate for one prompt, as prose."
  (gate-text (evaluate (ux3-gate-prompt call)
                        :evidence-tier (or (ux3-gate-evidence-tier call) (setting :evidence-tier))
                        :files-changed (ux3-gate-files-changed call))))

;;; --- the operator's command --------------------------------------------------

(defun run-command (args session-id)
  "One /ux3 invocation: the gate over the line the operator typed."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (plusp (length text))
        (gate-text (evaluate text :evidence-tier (setting :evidence-tier)))
        (format nil "/ux3 takes the decision to judge on one line, e.g. /ux3 let every user share one board; ~
the same thing as a tool is ux3_gate, and (help :ux3) has the method."))))

(nle:define-cell ux3
  (:section ("ux3")
    (:guide "nothing is required: evidence_tier is the floor a gate that names none is read at (t0 is the honest default, t1-t4 lift it as evidence arrives); working_language is the language the review's prose is written in")
    ("evidence_tier" :choice :options '("t0" "t1" "t2" "t3" "t4") :default "t0"
     :doc "the evidence tier a gate that names none reads at; t0 forces stop_reframe, t1 can never continue")
    ("working_language" :string :default "en"
     :doc "the language the review's prose is written in; flow, verdict and tier names stay canonical"))
  (:help :ux3
         "ux3: product judgment across user, evidence and business flows before execution - (help :ux3)"
         +primer+)
  (:tool "ux3_gate" (nlk:contract 'ux3-gate) #'run-gate)
  (:command "ux3" 'run-command
            :description "UX3: judge a product decision before building it"
            :argument-hint "the decision to judge"))
