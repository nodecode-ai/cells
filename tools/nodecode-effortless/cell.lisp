;;;; cell.lisp --- the section, the judges, pick_effort, /effortless.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of ivcained/effortless-hermes for Hermes (see NOTICE). The
;;;; plugin was an llm_request middleware: every turn it asked a judge how
;;;; hard the next message was and rewrote the request's reasoning effort.
;;;; Nodecode has the same knob as a session setting, (nle:select-effort
;;;; "high"), so here the same judge runs on the model's own call, as the
;;;; pick_effort tool, and on the operator's, as /effortless: the verdict is
;;;; applied to this session, from its next turn on.

(in-package #:nodecode-effortless)

(defparameter +manual+
  "Effortless picks the reasoning effort a message deserves and sets it on this session.
  (effortless:pick-effort MESSAGE &key context session-id applyp)
      A fast judge reads the next user message once - TypeSafe's Jev, a local OpenAI-compatible
      server, or any custom API - and answers one rung of the ladder low, medium, high, xhigh,
      max. The verdict is written to this session with nle:select-effort, so the turns from the
      next one on run at it; :apply nil asks without setting anything. A level named in the
      message (\"max effort\", \"effort: high\") wins with no judge, and a short follow-up
      (\"go\", \"ok\") keeps the effort already in force. => the line the pick_effort tool
      answers, and the verdict.
  The pick_effort tool is that call, with this session's id already filled in: use it on a
  message that is plainly a big multi-step job or plainly a quick answer, not on every turn.
ERROR: EFFORTLESS-ERROR says why no judge answered; the session keeps its effort when one did
  not, and the plugin never blocked a prompt either."
  "What (help :effortless) answers while the cell runs.")

(defparameter *history* '()
  "The verdicts this run took, newest first: what /effortless and /effortless stats read.")

;;; --- the section as the cell reads it ---------------------------------------------

(defun jev-key ()
  "The TypeSafe key: the section's own, else TYPESAFE_API_KEY, else NIL."
  (let ((key (or (ignore-errors (setting :typesafe-key))
                 (uiop:getenv "TYPESAFE_API_KEY")
                 "")))
    (and (stringp key) (plusp (length (nlk:trimmed key))) (nlk:trimmed key))))

(defun openai-base (which)
  "The base url the LOCAL or CUSTOM judge is asked at."
  (let* ((raw (if (string= which "local") (setting :local-base-url) (setting :custom-base-url)))
         (base (string-right-trim "/" (or raw ""))))
    (if (plusp (length base))
        base
        (fail "the ~a judge has no base_url" which))))

(defun openai-url (which)
  (format nil "~a/chat/completions" (openai-base which)))

(defun openai-model (which)
  "The model id the LOCAL or CUSTOM judge is asked for; a local one may go unnamed."
  (let ((model (nlk:trimmed (or (if (string= which "local")
                                    (setting :local-model)
                                    (setting :custom-model))
                                ""))))
    (cond ((plusp (length model)) model)
          ((string= which "local") "local-model")
          (t (fail "the ~a judge has no model" which)))))

(defun openai-headers (which)
  "The authorization header a CUSTOM judge is sent, when it has a key."
  (let ((key (and (string= which "custom") (setting :custom-api-key))))
    (if (and (stringp key) (plusp (length (nlk:trimmed key))))
        (list (cons "authorization" (format nil "Bearer ~a" (nlk:trimmed key))))
        '())))

;;; --- asking the judges ------------------------------------------------------------

(defun judge-once (backend message current context)
  "BACKEND's verdict on MESSAGE, or NIL, and in the second value why not."
  (handler-case
      (cond ((string= backend "jev")
             (let ((key (jev-key)))
               (if (null key)
                   (values nil "jev: no key")
                   (jev-verdict (post-json (setting :jev-url)
                                            (jev-payload (or (setting :jev-model) +jev-model+)
                                                          current context message)
                                            (list (cons "authorization"
                                                        (format nil "Bearer ~a" key)))
                                            (setting :judge-timeout-seconds))
                                current))))
            ((member backend '("local" "custom") :test #'string=)
             (openai-verdict
              (post-json (openai-url backend)
                         (openai-payload (openai-model backend)
                                         (openai-question current context message))
                         (openai-headers backend)
                         (max 8 (or (setting :judge-timeout-seconds) 3)))))
            (t (values nil (format nil "~a is no judge" backend))))
    (error (condition) (values nil (princ-to-string condition)))))

(defun choose (message current context)
  "The primary judge's verdict, the fallback's when the first is unsure or
down, and the notes of what went wrong. => (values VERDICT NOTE)."
  (let ((notes '())
        (primary (setting :judge-backend))
        (fallback (setting :fallback-backend)))
    (labels ((ask (backend)
               (multiple-value-bind (verdict note) (judge-once backend message current context)
                 (when (and note (plusp (length note))) (push note notes))
                 verdict)))
      (let ((verdict (ask primary)))
        (when (and (or (null verdict) (verdict-unsure verdict))
                   (not (string= fallback "none")))
          (let ((second (ask fallback)))
            (when (and second (not (verdict-unsure second)))
              (setf verdict second))))
        (when (and verdict (verdict-unsure verdict))
          (setf verdict (and current (make-verdict :effort current :by "kept"
                                                    :why "the judge was unsure, so the effort was kept"))))
        (values verdict (format nil "~{~a~^; ~}" (reverse notes)))))))

;;; --- setting the session's effort ---------------------------------------------------

(defun set-session-rung (effort session-id)
  "Set SESSION-ID's effort to EFFORT, walking down the ladder when the model
refuses a rung (the plugin's clamp to the provider's own vocabulary).
=> (values RUNG DETAIL); RUNG is NIL when nothing landed."
  (let ((i (or (rung-index effort) 1)))
    (loop for j downfrom i to 0
          do (handler-case
                 (progn (nle:select-effort (nth j +efforts+) :session-id session-id)
                        (return-from set-session-rung (values (nth j +efforts+) nil)))
               (error (condition)
                 (when (zerop j)
                   (return-from set-session-rung
                     (values nil (princ-to-string condition)))))))))

(defun remember (verdict applied)
  "Keep VERDICT in the window /effortless reads."
  (let ((limit (or (ignore-errors (setting :history)) 20)))
    (when verdict
      (push (list :effort (verdict-effort verdict) :by (verdict-by verdict)
                  :why (verdict-why verdict) :applied applied)
            *history*))
    (when (and (integerp limit) (> (length *history*) limit))
      (setf *history* (subseq *history* 0 limit)))))

(defun run (message context session-id do-apply)
  "Judge MESSAGE and, when DO-APPLY, set SESSION-ID's effort.
=> (values VERDICT APPLIED NOTE)."
  (let* ((current (and session-id (ignore-errors (nlk:session-model-effort session-id))))
         (named (named-effort message)))
    (multiple-value-bind (verdict note)
        (cond (named
               (values (make-verdict :effort named :by "named"
                                      :why "named in the message") nil))
              ((and current (follow-up-p message))
               (values (make-verdict :effort current :by "kept"
                                      :why "follow-up, kept effort") nil))
              (t (choose message current context)))
      (when verdict
        (setf (verdict-effort verdict)
              (apply-bounds (verdict-effort verdict)
                            (setting :effort-floor) (setting :effort-ceiling)
                            (setting :effort-bias))))
      (let ((applied nil))
        (when (and verdict do-apply session-id)
          (setf applied (set-session-rung (verdict-effort verdict) session-id)))
        (remember verdict applied)
        (values verdict applied note)))))

(defun answer-line (verdict applied note)
  "The one line the tool and the command answer for VERDICT."
  (if (null verdict)
      (format nil "effort: no verdict~@[ (~a)~]; the session keeps its effort" note)
      (with-output-to-string (out)
        (format out "effort: ~a" (verdict-effort verdict))
        (unless (member (verdict-by verdict) '("kept" "named") :test #'string=)
          (format out " (~a~@[, ~a~])" (verdict-by verdict) (verdict-sure-text verdict)))
        (when (plusp (length (verdict-why verdict)))
          (format out " - ~a" (verdict-why verdict)))
        (cond ((null applied) (format out "; not applied to a session"))
              ((string= applied (verdict-effort verdict))
               (format out "; this session runs at ~a from its next turn" applied))
              (t (format out "; the model's ladder takes ~a, so that is what landed" applied))))))

(defun pick-effort (message &key context session-id (applyp t))
  "Judge MESSAGE and, unless APPLYP is NIL, set SESSION-ID's effort to the
verdict; the plugin's per-turn decision, called by hand.
=> (values the line pick_effort answers, the verdict)."
  (multiple-value-bind (verdict applied note)
      (run (or message "") (or context "") session-id applyp)
    (values (answer-line verdict applied note) verdict)))

(defun pick-tool (args)
  "The pick_effort tool: one verdict for the message the model passes, set on
the session the turn runs for."
  (multiple-value-bind (verdict applied note)
      (run (effort-pick-message args) (or (effort-pick-context args) "")
           (getf (nle:turn) :session-id) (effort-pick-apply-p args))
    (answer-line verdict applied note)))

;;; --- /effortless -------------------------------------------------------------------

(defun status-line ()
  "Where the judge comes from, in a line; no network."
  (let* ((backend (setting :judge-backend))
         (fallback (setting :fallback-backend))
         (bias (setting :effort-bias)))
    (format nil "effortless: judge ~a~a, ~a~a~a"
            backend
            (if (string= fallback "none") "" (format nil " (fallback ~a)" fallback))
            (if (setting :apply)
                "the verdict sets this session's effort"
                "the verdict is reported, not applied")
            (if (string= backend "jev")
                (format nil "; TypeSafe key ~a" (if (jev-key) "set" "missing"))
                "")
            (if (and (integerp bias) (not (zerop bias)))
                (format nil "; bias ~@d" bias)
                ""))))

(defun stats-text ()
  "The verdicts this run took, counted; no network."
  (if (null *history*)
      "effortless: no verdicts yet; /effortless test asks the judges once"
      (let ((by-effort (make-hash-table :test #'equal))
            (by-judge (make-hash-table :test #'equal)))
        (dolist (entry *history*)
          (incf (gethash (getf entry :effort) by-effort 0))
          (incf (gethash (getf entry :by) by-judge 0)))
        (format nil "effortless: ~d verdict~:p~%by effort: ~{~a~^, ~}~%by judge: ~{~a~^, ~}"
                (length *history*)
                (loop for key being the hash-keys of by-effort using (hash-value count)
                      collect (format nil "~a x~d" key count))
                (loop for key being the hash-keys of by-judge using (hash-value count)
                      collect (format nil "~a x~d" key count))))))

(defun test-text (session-id)
  "One sample prompt through the judge chain, without moving the session."
  (let ((sample "rename one variable in a single file"))
    (multiple-value-bind (verdict applied note) (run sample "" session-id nil)
      (declare (ignore applied))
      (format nil "effortless: test prompt ~s~%~a" sample (answer-line verdict nil note)))))

(defun run-slash (args session-id)
  "/effortless alone: the judge in force; test: one sample through it;
stats: the verdicts this run took; anything else is a message to judge."
  (let ((argument (nlk:trimmed (or args ""))))
    (cond ((string= argument "") (values (status-line) nil))
          ((string-equal argument "test") (values (test-text session-id) nil))
          ((string-equal argument "stats") (values (stats-text) nil))
          (t (pick-effort argument :session-id session-id)))))

;;; --- the declaration ----------------------------------------------------------------

(nlk:define-contract effort-pick
  (:doc "Pick the reasoning effort the next message deserves and set it on this session: easy messages run cheap, big multi-step jobs get more thinking. A fast judge reads the message once and answers one rung of low, medium, high, xhigh, max. Use it when a message is plainly a big job (review the whole repo, migrate everything, go through my drive) or plainly a quick answer; a level named in the message wins with no judge. It judges; it writes nothing but this session's effort.")
  ("message" :text :required t
   :doc "the next user message, verbatim, and nothing else")
  ("context" :string
   :doc "a few lines of the conversation before it, when the message only makes sense with them")
  ("apply" :boolean :default t :slot apply-p
   :doc "set this session's effort to the verdict; false asks without setting anything"))

(nle:define-cell effortless
  (:section ("effortless")
    (:guide "judge_backend picks the judge: jev asks TypeSafe's system one and needs a key; local asks an OpenAI-compatible server on this machine; custom asks an API you name. apply is whether a verdict is written to the session; effort_floor, effort_ceiling and effort_bias bound what a judge may answer")
    ("judge_backend" :choice :options '("jev" "local" "custom") :default "jev"
     :doc "the judge that answers: jev (TypeSafe), local (an OpenAI-compatible server), custom (any OpenAI-compatible API)")
    ("fallback_backend" :choice :options '("local" "custom" "jev" "none") :default "local"
     :doc "the second judge, asked when the first is unsure or down")
    ("typesafe_key" :secret
     :doc "the TypeSafe key for the jev judge; empty reads TYPESAFE_API_KEY")
    ("jev_url" :string :default "https://api.typesafe.ai/v1/systemone"
     :doc "TypeSafe's judgment route")
    ("jev_model" :string :default "jev-latest"
     :doc "the judgment model the jev judge asks for")
    ("local_base_url" :string :default "http://127.0.0.1:8080/v1"
     :doc "the base url of an OpenAI-compatible server on this machine")
    ("local_model" :string
     :doc "the local judge's model id; empty asks for local-model")
    ("custom_base_url" :string
     :doc "the base url of a custom OpenAI-compatible API")
    ("custom_model" :string
     :doc "the custom judge's model id")
    ("custom_api_key" :secret
     :doc "the custom judge's key; empty sends no authorization header")
    ("judge_timeout_seconds" :integer :default 3 :min 1
     :doc "one judge's ceiling in seconds; a slow one falls through")
    ("effort_floor" :choice :options '("low" "medium" "high" "xhigh" "max") :default "low"
     :doc "never pick below this rung")
    ("effort_ceiling" :choice :options '("low" "medium" "high" "xhigh" "max") :default "max"
     :doc "never pick above this rung")
    ("effort_bias" :integer :default 0
     :doc "-2 cheaper .. +2 smarter; tips the judge's close calls")
    ("apply" :boolean :default t
     :doc "write the verdict to this session's effort")
    ("history" :integer :default 20 :min 1
     :doc "how many verdicts /effortless keeps"))
  (:start (lambda ()
            (setf *history* '())
            (nle:on-stop (lambda () (setf *history* '())))))
  (:help :effortless
   "effortless: pick_effort judges a message's reasoning effort and sets it on this session; /effortless test checks the judge"
   +manual+)
  (:tool "pick_effort" (nlk:contract 'effort-pick) 'pick-tool)
  (:command "effortless" #'run-slash
            :description "Judge a message's reasoning effort, and set this session's"
            :argument-hint "test|stats|MESSAGE"))
