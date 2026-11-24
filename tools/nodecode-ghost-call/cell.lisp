;;;; cell.lisp --- the primer, the section, the /ghost command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was seven Hermes tools, a /ghost command, a skill and a
;;;; FastAPI bridge that carried the audio. The tools are tools.lisp, the
;;;; command is below, and the skill is +PRIMER+, which the model reads with
;;;; (help :ghost) and whose one line rides every request. The bridge is what
;;;; this rewrite replaces: the lines arrive as text instead of a microphone.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "ghost-call": {"api_key": "...", "api_key_env": "OPENAI_API_KEY",
;;;;                  "base_url": "https://api.openai.com/v1",
;;;;                  "model": "gpt-4.1-mini", "min_hint_interval_s": 4,
;;;;                  "max_hints": 40, "max_duration_s": 900,
;;;;                  "timeout_seconds": 20, "user_name": "..."}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-ghost-call)

(defparameter +primer+
  "Ghost Call coaches the operator, silently, through one phone call of their own. The operator
does the talking; hints go to the operator only and nothing is said to the other party.
  ghost_start      open a call on a goal: what the operator wants, with the facts (date, party
                   size, budget, name). Add target, their private walk-away figure, which no
                   hint ever suggests saying. One call is open at a time.
  ghost_line       one line the other party just said, as heard, with speaker \"them\". Answers
                   the next hint or SILENT and why. Feed it as the call goes; most lines
                   (fillers, greetings, repeats, the operator's own words) get SILENT free.
  ghost_hint_latest  the hints already given, for a question asked after the call.
  ghost_intervene  say one short phrase to the other party, once the operator has typed
                   /ghost allow: the answer carries the AI disclosure that must ride the first
                   spoken line. Refused until then, and always refused after the call ends.
  ghost_hangup     end the call and answer the one summary: what was agreed, the date, the
                   amount, the open questions and a draft follow-up message.
  ghost_summary    the same summary again, or after a call that ended on its own.
  ghost_status     state, duration and how many hints are left.
How to work: open with ghost_start, relay the goal back in one line, then feed each of the
other party's utterances with ghost_line and read the hint to yourself or say it quietly; do not
ask the operator questions mid-call and do not invent hints of your own. Hints are at most one
per four seconds and forty a call. When the call is over, ghost_hangup gives the one summary;
post it as it is, with the draft message for the operator to send.
The operator runs the same verbs from the keyboard: /ghost start <goal>, /ghost hint <what they
said>, /ghost status, /ghost allow, /ghost hangup."
  "What (help :ghost) answers while the cell runs: the plugin's skill, in one topic.")

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's values, with the key
resolved out of its variable and the model's endpoint defaulted."
  (declare (ignore table))
  (list :api-key (nonempty (getf values :api-key))
        :api-key-env (nonempty (getf values :api-key-env))
        :base-url (or (nonempty (getf values :base-url)) +base-url+)
        :model (or (nonempty (getf values :model)) +default-model+)
        :min-hint-interval-s (getf values :min-hint-interval-s)
        :max-hints (getf values :max-hints)
        :max-duration-s (getf values :max-duration-s)
        :timeout-seconds (getf values :timeout-seconds)
        :user-name (nonempty (getf values :user-name))))

;;; --- the operator's command --------------------------------------------------

(defparameter +usage+
  "usage: /ghost start <goal> | status | hint <what they said> | allow | hangup")

(defun command-verdict (text)
  "TEXT — what a command answered — with its own refusal caught: a command
answers the operator, it never signals into the shell."
  (handler-case text
    (ghost-call-error (condition)
      (format nil "ghost: ~a" (ghost-call-error-detail condition)))))

(defun run-command (args session-id)
  "One /ghost invocation: the original's slash command, over typed lines.
No argument answers the call's state."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (verb (if space (subseq text 0 space) text))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) "")))
    (command-verdict
     (cond
       ((zerop (length verb)) (status-text))
       ((member verb '("start" "START") :test #'string-equal)
        (start-call rest))
       ((string-equal verb "status") (status-text))
       ((string-equal verb "hint")
        (if (zerop (length rest))
            "say what they said: /ghost hint <what the other party just said>"
            (feed-line "them" rest)))
       ((member verb '("allow" "intervene") :test #'string-equal)
        (let ((session (active-session)))
          (setf (ghost-session-intervene-allowed session) t)
          (if (plusp (length rest))
              (intervene-line rest)
              (progn
                (format nil "intervene is allowed for ~a; ghost_intervene <text> says one line \
to the other party, with the AI disclosure in front"
                        (ghost-session-id session))))))
       ((string-equal verb "hangup") (end-call))
       ((string-equal verb "summary") (summary-answer (active-session)))
       (t +usage+)))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell ghost-call
  (:section ("ghost-call")
    (:guide "a key from platform.openai.com is all the cell needs: export OPENAI_API_KEY, or paste it in api_key, or name the variable in api_key_env; min_hint_interval_s, max_hints and max_duration_s bound one call exactly as the original's did; user_name is used only in the intervention disclosure; next step: /ghost start <goal>")
    ("api_key" :secret
     :doc "the OpenAI key the hints and the summary are made with")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (OPENAI_API_KEY is read either way)")
    ("base_url" :string
     :doc "the API address: https://api.openai.com/v1 unless you go through a proxy of your own")
    ("model" :string :default "gpt-4.1-mini"
     :doc "the fast text model for a hint and for the summary")
    ("min_hint_interval_s" :integer :default 4 :min 4
     :doc "the least time between two hints: four seconds is the original's floor")
    ("max_hints" :integer :default 40 :min 1
     :doc "the hard cap on hints in one call, at most forty")
    ("max_duration_s" :integer :default 900 :min 60
     :doc "the hard cap on one call in seconds")
    ("timeout_seconds" :integer :default 20 :min 1
     :doc "one summary request's ceiling; a hint has its own five seconds")
    ("user_name" :string
     :doc "the operator's name, used only in the intervention disclosure and the draft message"))
  (:settings 'read-settings)
  (:help :ghost
         "ghost-call: coach the operator through their own phone call with one-line hints - (help :ghost)"
         +primer+)
  (:command "ghost" 'run-command
            :description "Ghost Call: start, hint, status, allow, hangup"
            :argument-hint "start <goal> | hint <what they said> | status | allow | hangup"
            :session nil)
  (:tool "ghost_start" (nlk:contract 'ghost-start-call) #'tool-start)
  (:tool "ghost_line" (nlk:contract 'ghost-line-call) #'tool-line)
  (:tool "ghost_status" (nlk:contract 'ghost-side-call) #'tool-status)
  (:tool "ghost_hint_latest" (nlk:contract 'ghost-hints-call) #'tool-hints)
  (:tool "ghost_intervene" (nlk:contract 'ghost-intervene-call) #'tool-intervene)
  (:tool "ghost_hangup" (nlk:contract 'ghost-side-call) #'tool-hangup)
  (:tool "ghost_summary" (nlk:contract 'ghost-side-call) #'tool-summary)
  (:start (lambda ()
            (check-config)
            (nle:notice (format nil "ghost-call: ready — /ghost start <goal> opens a call (~a)"
                                (or (setting :model) +default-model+))
                        :key "nodecode-ghost-call")
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-ghost-call"))))))
