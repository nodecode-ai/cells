;;;; cell.lisp --- each turn takes the cheapest tier that can do the job.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of aean0x's hermes-model-picker as a Nodecode cell. Three
;;;; named tiers -- low, default, high -- each a model id on one provider. The
;;;; cell classifies each prompt as it is submitted, records the tier's model
;;;; as the session's own selection, and the turn that follows runs on it;
;;;; nothing is patched, because a session's model is a fact the turn reads.
;;;;
;;;; The seams are Nodecode's own. NLE:SUBMIT is the one in-process ingress;
;;;; advising it puts the classification ahead of the turn's model binding.
;;;; The :TOOL point counts failed tool calls, which is what allows
;;;; escalate_model to climb. NLK:RECORD-SESSION-MODEL-SELECTION writes the
;;;; one fact the next turn reads.

(in-package #:nodecode-model-picker)

;;; --- the tiers -----------------------------------------------------------

(defparameter *tier-order* '("low" "default" "high")
  "The tiers, cheapest first. A tier's model is the <TIER>_model setting
beside it; its policy is the <TIER>_best_for list.")

(defparameter *tier-aliases* '(("quick" . "low") ("standard" . "default")
                                ("expert" . "high") ("medium" . "default"))
  "The display names a prompt or a slash argument may use for a tier. The
upstream plugin's `medium' is the alias for `default', as it is there.")

(defparameter +primer+ (format nil "model-picker: each prompt is classified into one of three tiers and the session runs the tier's model for that turn -- low (lookups, small edits), default (ordinary work), high (money, irreversible or security-sensitive work). /model-picker low|default|high pins a tier for the session, /model-picker auto resumes classifying, and bare /model-picker says where the session stands. The classifier keeps the previous tier unless the work changes shape, high never sticks to the next turn, and a failed classification falls back to low: a routing failure never fails the turn. Repeated failed tool calls unlock escalate_model, which climbs one tier, at most to high. The tier's model is recorded as the session's own selection, so a pinned session is never classified. ~a"
                                      "Settings: low_model, default_model, high_model (required), provider (empty means the session's own), classifier_model, escalate_after, keep_previous and a best_for list per tier."))

(defun canonical-tier (word)
  "The tier WORD names, or NIL: a tier's own name, its display name, or a
word the tier appears in (`Low.', `HIGH')."
  (let ((word (string-downcase (string-trim " .:;*_\"'()[]" (or word "")))))
    (or (find word *tier-order* :test #'string=)
        (cdr (assoc word *tier-aliases* :test #'string=))
        (loop for tier in *tier-order*
              when (and (plusp (length word)) (search tier word)) return tier)
        (loop for (alias . tier) in *tier-aliases*
              when (search alias word) return tier))))

(defun tier-setting (tier field)
  "TIER's FIELD in the running section: :low-model reads low_model."
  (setting (intern (string-upcase (format nil "~a-~a" tier field)) :keyword)))

(defun tier-model (tier)
  "The model id TIER names, or NIL when the operator named none."
  (let ((model (tier-setting tier "model")))
    (and (stringp model) (plusp (length model)) model)))

(defun tier-policy (tier)
  "TIER's best_for list: what the classifier is told the tier is for."
  (remove-if-not #'stringp (tier-setting tier "best-for")))

(defun routing-provider ()
  "The provider every tier runs on, or NIL for the session's own: a
classification moves a model id, never the API host."
  (let ((provider (setting :provider)))
    (and (stringp provider) (plusp (length provider)) provider)))

(defun rank-of (tier)
  "How far up TIER stands from the floor."
  (or (position tier *tier-order* :test #'string=) 0))

;;; --- where each session stands -------------------------------------------

(defvar *sessions* (make-hash-table :test 'equal :synchronized t)
  "Session id -> plist (:tier TIER :pin TIER-OR-NIL :failures N). Live
only: a pin is a state of this process, and what a turn runs is the durable
selection fact the pin wrote.")

(defun session-state (session-id)
  "SESSION-ID's plist, made empty on first use."
  (or (gethash session-id *sessions*)
      (setf (gethash session-id *sessions*) (list :tier nil :pin nil :failures 0))))

(defun (setf session-state) (state session-id)
  (setf (gethash session-id *sessions*) state))

(defun note-failure (session-id result)
  "Count RESULT as a failed tool call for SESSION-ID when it reads as one,
and reset the count on a call that worked. A call outside a turn counts
under the NIL session: nothing is lost and nothing signals."
  (let* ((state (session-state session-id))
         (failed (and (stringp result) (eql 0 (search "ERROR" result)))))
    (setf (getf state :failures) (if failed (1+ (getf state :failures)) 0))
    (setf (session-state session-id) state)))

;;; --- classifying ---------------------------------------------------------

(defparameter +classifier-context-chars+ 12000
  "How much of a prompt the classifier reads: a long pasted file is not a
better routing signal past its opening.")

(defun classifier-prompt (&optional previous)
  "The classifier's whole instruction, generated from the best_for lists so
the section is the single source of the routing policy."
  (with-output-to-string (out)
    (format out "You route one turn of a coding session to the cheapest model tier that can do it well. Answer with one word: low, default or high.~%")
    (dolist (tier *tier-order*)
      (format out "~%~a -- for:~{\n- ~a~}" tier (tier-policy tier)))
    (format out (if (null previous)
                    "~%No tier has run yet this session.~%"
                    (format nil "~%The previous turn ran on ~a. Keep it unless the work has changed shape; high never carries to the next turn on its own.~%" previous)))
    (format out "~%Answer low for a lookup, a direct question or a small edit; high only for money, an irreversible or published action, or a security decision.")))

(defun parse-tier (answer)
  "The tier ANSWER names, or NIL when it names none: the first word of it
that is a tier, a display name or one of their aliases."
  (let ((answer (or answer "")))
    (loop for start = 0 then (1+ end)
          for end = (position-if (lambda (char) (member char '(#\Space #\Newline #\Tab #\. #\, #\: #\; #\* #\` #\" #\'))) answer :start start)
          while (< start (length answer))
          for word = (subseq answer start (min (length answer) (or end (length answer))))
          for tier = (canonical-tier word)
          when tier return tier
          while end)))

(defun classifier-target ()
  "The binding that sends this call to the classifier's own model, or NIL:
the host refuses an override it was not asked for, so a provider must be
named beside the model."
  (let ((model (setting :classifier-model))
        (provider (routing-provider)))
    (and (stringp model) (plusp (length model)) provider
         (list :provider provider :model model))))

(defun classify (prompt &optional previous)
  "The tier the classifier names for PROMPT, or NIL when the call fails:
the caller decides what a missing answer means."
  (let* ((system (classifier-prompt previous))
         (user (subseq prompt 0 (min (length prompt) +classifier-context-chars+)))
         (target (classifier-target)))
    (parse-tier (if target
                    (let ((nle::*auxiliary-model* target))
                      (nle:complete system user :max-tokens 8))
                    (nle:complete system user :max-tokens 8)))))

;;; --- applying a tier -----------------------------------------------------

(defun selection-of (session-id)
  "(values PROVIDER MODEL): what SESSION-ID's next turn would run."
  (if (nlk:store-open-p) (nlk:session-model-selection session-id) (values nil nil)))

(defun apply-tier (session-id tier &optional pin)
  "Make TIER the model SESSION-ID's next turn runs, and PIN it there when
PIN. => the model id the selection names, or NIL when there was nothing to
write."
  (let ((model (tier-model tier)))
    (when model
      (let ((state (session-state session-id)))
        (setf (getf state :tier) tier (getf state :pin) pin)
        (setf (session-state session-id) state))
      (when (nlk:store-open-p)
        (multiple-value-bind (provider current) (selection-of session-id)
          (let ((wanted (or (routing-provider) provider)))
            (unless (and (equal current model) (equal provider wanted))
              (nlk:record-session-model-selection
               session-id :provider wanted :model model
               :effort (nlk:session-model-effort session-id))))))
      model)))

(defun pinned-tier (session-id)
  (getf (session-state session-id) :pin))

(defun route-turn (session-id prompt &key steer)
  "Classify PROMPT and put SESSION-ID's next turn on the tier it names. A
steer continues the turn already running, a slash line is a command, and a
pinned session keeps its pin: none of the three is classified. => the tier
it runs, or NIL when it was left alone. Never signals: the turn is not the
classifier's to fail."
  (handler-case
      (when (and (stringp session-id) (stringp prompt) (not steer)
                 (plusp (length (nlk:trimmed prompt)))
                 (not (char= #\/ (char prompt 0)))
                 (not (pinned-tier session-id)))
        (let ((tier (or (handler-case (classify prompt (getf (session-state session-id) :tier))
                          (error () nil))
                        (and (setting :keep-previous) (getf (session-state session-id) :tier))
                        "low")))
          (apply-tier session-id tier)
          tier))
    (error () nil)))

;;; --- escalating ----------------------------------------------------------

(defun next-tier (tier)
  "The tier one above TIER, or TIER at the top of the order."
  (nth (min (1+ (rank-of tier)) (1- (length *tier-order*))) *tier-order*))

(defun escalate-model (session-id &optional reason)
  "Climb one tier for SESSION-ID, at most to the top, once its tool failures
reach escalate_after. => one line saying what happened."
  (unless session-id (fail "escalate_model needs a session: call it inside a turn"))
  (let* ((state (session-state session-id))
         (tier (or (getf state :tier) (pinned-tier session-id) "default"))
         (failures (getf state :failures))
         (needed (setting :escalate-after)))
    (cond ((< failures needed)
           (format nil "not yet: ~d of ~d failed tool calls. Fix what failed, or call again once ~d calls have failed."
                   failures needed needed))
          (t
           (let ((up (next-tier tier)))
             (if (equal up tier)
                 (progn (setf (getf state :failures) 0) (setf (session-state session-id) state)
                        (format nil "already on ~a, the top tier: nothing above it to climb to" tier))
                 (let ((model (apply-tier session-id up (pinned-tier session-id))))
                   (setf (getf (session-state session-id) :failures) 0)
                   (format nil "escalated from ~a to ~a (~a) for this session's next turn~@[: ~a~]"
                           tier up model reason))))))))

(nlk:define-contract escalation
  (:doc "Climb one model tier for this session after tool calls keep failing: the next turn runs the stronger model. Use it when the work is stuck on something the current tier keeps getting wrong, never to make a turn feel better.")
  ("reason" :text :required t :doc "what kept failing, one sentence"))

(defun escalate-record (report)
  "The escalate_model tool's body: REPORT is the boundary contract's record."
  (escalate-model (nle::eval-session-id) (escalation-reason report)))

;;; --- the hooks -----------------------------------------------------------

(defun submit-advice (next &rest args)
  "Advice on NLE:SUBMIT -- the one ingress a prompt enters by: classify it
before the turn's model is bound, then let the submit run."
  (destructuring-bind (session-id prompt &rest keys) args
    (route-turn session-id prompt :steer (getf keys :steer))
    (apply next session-id prompt keys)))

(defun tool-advice (op next)
  "The :TOOL point: count a failed call and reset the count on one that
worked. OP is (:name TOOL-NAME :arguments HASH :call-id ID)."
  (multiple-value-bind (text settle) (funcall next op)
    (note-failure (nle::eval-session-id) text)
    (values text settle)))

;;; --- the command ---------------------------------------------------------

(defun tier-line (tier)
  (format nil "~a -> ~a" tier (or (tier-model tier) "(no model named)")))

(defun standing (session-id)
  "Where SESSION-ID stands, one paragraph."
  (let* ((state (session-state session-id))
         (pin (getf state :pin))
         (tier (getf state :tier)))
    (format nil "model-picker: ~a~%~{~a~^~%~}~%failures since the last working call: ~d, escalation after ~d~@[~%~a~]"
            (cond (pin (format nil "pinned to ~a" pin))
                  (tier (format nil "classifying; the last turn ran ~a" tier))
                  (t "classifying; no turn has run yet"))
            (loop for each in *tier-order* collect (tier-line each))
            (getf state :failures) (setting :escalate-after)
            (let ((provider (routing-provider))) (and provider (format nil "every tier runs on provider ~a" provider))))))

(defun run-slash (args session-id)
  "/model-picker typed alone: where the session stands and its panel;
/model-picker low|default|high pins a tier; auto resumes classifying. =>
the answer's text, and the panel showcasing the picks when typed alone."
  (let ((text (string-downcase (nlk:trimmed (or args "")))))
    (unless session-id (fail "this shell has no session"))
    (cond ((member text '("" "status" "show") :test #'string=)
           (values (standing session-id)
                   (nle:list-dialog
                    :title "Model picker" :summary (standing session-id)
                    :rows (append
                           (loop for tier in *tier-order*
                                 collect (nle:ask-row (format nil "Pin ~a" tier)
                                                      (format nil "~a: ~a" tier (or (tier-model tier) "no model"))
                                                      "/model-picker" :verb tier))
                           (list (nle:ask-row "Classify again" "auto" "/model-picker" :verb "auto"))))))
          ((member text '("auto" "off" "resume") :test #'string=)
           (let ((state (session-state session-id)))
             (setf (getf state :pin) nil)
             (setf (getf state :failures) 0)
             (setf (session-state session-id) state)
             "model-picker: classifying every prompt again; the next turn takes the tier it names"))
          (t
           (let ((tier (canonical-tier text)))
             (unless tier
               (fail "/model-picker takes low, default, high, auto or nothing; it was given ~s" (or args "")))
             (let ((model (apply-tier session-id tier tier)))
               (unless model (fail "the ~a tier names no model: set ~a_model in the model-picker section" tier tier))
               (format nil "model-picker: pinned to ~a (~a); /model-picker auto resumes classifying" tier model)))))))

;;; --- the declaration -----------------------------------------------------

(nle:define-cell model-picker
  (:section ("model-picker")
    (:guide "Name a model for each of the three tiers, all on one provider -- a tier moves the model a turn runs, never the API host. provider is the id they share, empty for the session's own. classifier_model sends the classification to a cheaper model. escalate_after is how many failed tool calls unlock escalate_model")
    ("low_model" :string :required t
                 :doc "model id for lookups, direct questions and small edits")
    ("default_model" :string :required t
                     :doc "model id for ordinary development and research")
    ("high_model" :string :required t
                  :doc "model id for money, irreversible or security-sensitive work")
    ("provider" :string :default ""
                :doc "the provider every tier runs on; empty means the session's own")
    ("classifier_model" :string :default ""
                        :doc "model the classification call runs on, empty for the auxiliary model")
    ("escalate_after" :integer :default 3 :min 1
                       :doc "failed tool calls that unlock escalate_model")
    ("keep_previous" :boolean :default t
                     :doc "keep the last tier when the classifier cannot answer")
    ("low_best_for" :list
                    :default (list "short single-file edits" "a direct question answered from context"
                              "small research" "renaming and mechanical refactors"
                              "reading and quoting a document or codebase" "status checks and look-ups")
                    :doc "what the classifier is told the low tier is for")
    ("default_best_for" :list
                        :default (list "multi-step reasoning and multi-file changes"
                                  "conceptual or deep research across subjects"
                                  "root-cause debugging with no clear error"
                                  "writing or analysis from scratch" "planning before coding"
                                  "outbound voice: email, a published post")
                        :doc "what the classifier is told the default tier is for")
    ("high_best_for" :list
                     :default (list "money-moving transactions" "irreversible, destructive or publish actions"
                               "security-sensitive decisions")
                     :doc "what the classifier is told the high tier is for; high is meant to be rare"))
  (:tool "escalate_model" (nlk:contract 'escalation) 'escalate-record)
  (:help :model-picker "model-picker: each turn runs the cheapest model tier that can do the job, and escalate_model climbs one tier when the work keeps failing" +primer+)
  (:hook 'nle:submit #'submit-advice)
  (:hook :tool #'tool-advice)
  (:command "model-picker" 'run-slash
            :catches 'model-picker-error
            :description "Route each turn to a cheap, standard or expert model: pin a tier or resume classifying"
            :argument-hint "low|default|high|auto")
  (:start (lambda ()
            ;; A tier without a model cannot route, and a session left on a
            ;; silent pin is worse than a refusal that names the key.
            (dolist (tier *tier-order*)
              (unless (tier-model tier)
                (fail "the ~a tier names no model: set ~a_model in the model-picker section of the config" tier tier)))
            (setf *sessions* (make-hash-table :test 'equal :synchronized t))
            (nle:on-stop (lambda () (setf *sessions* (make-hash-table :test 'equal :synchronized t)))))))
