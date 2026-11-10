;;;; cell.lisp --- the primer, the section, the /phone-call command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was four Hermes tools, a /phone command, a skill, three hooks
;;;; and a FastAPI bridge that carried a realtime voice session over Twilio
;;;; Media Streams. The tools are tools.lisp, the command is below, and the
;;;; skill is +PRIMER+, which the model reads with (help :phone-call) and whose
;;;; one line rides every request. The bridge is what this rewrite replaces:
;;;; the call speaks a TwiML document instead of a live model.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "phone-call": {"account_sid": "AC...", "auth_token": "...",
;;;;                  "from_number": "+14155550100",
;;;;                  "allowed_numbers": ["+14155550123"],
;;;;                  "allow_all": false, "user_name": "Ada",
;;;;                  "callback_number": "+14155550100",
;;;;                  "daily_call_limit": 10, "max_duration_s": 300,
;;;;                  "enforce_quiet_hours": false, "callee_utc_offset": null,
;;;;                  "voice": "Polly.Joanna", "language": "en",
;;;;                  "timeout_seconds": 20}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-phone-call)

(defparameter +primer+
  "Phone Call places one outbound phone call for the operator, after they approve it, and tells them
what was said. Four tools:
  phone_prepare    write the brief and the approval card: callee_name, number (E.164), goal, and
                   message (exactly what the callee is told after the disclosure). Put EVERY
                   non-sensitive detail the callee may need in facts_to_share (person_to_see,
                   service, date, time_window, party_size, name); list any field that needs
                   consent in sensitive_fields (card, address, id_number, bank, email, phone).
                   Answers a call id and the card text. It never dials.
  phone_call       dial a call the operator approved. Takes only call_id. If the operator
                   approved with /phone-call approve, the call is already placed: read
                   phone_status instead of dialing again.
  phone_status     one call's state and result, or the recent log. A live call is refreshed
                   from Twilio first.
  phone_transcript what the cell said, and a link to the recording of the answer.
How to work: find the number (never guess it), prepare, show the card, and wait: only the operator
approves, with /phone-call approve <id>, and the approval is theirs to give, not yours. Keep one
short line about a call; do not invent a second card for the same goal unless they ask again.
At most one call is live, the daily limit and the gap to one number are enforced, emergency and
premium numbers are refused, and dialing is limited to the numbers in allowed_numbers.
The callee always hears an AI disclosure first. Sensitive facts are shared only when the operator
grants that field for that call (/phone-call consent <id> <field>); cvv, passwords and one-time
codes are never shared. Never redial on your own: no answer, busy or failed is reported plainly,
and a retry is a new prepare and a new approval.
The operator runs the same verbs from the keyboard: /phone-call status [<id>], /phone-call log,
/phone-call transcript <id>, /phone-call approve <id>, /phone-call cancel <id>,
/phone-call consent <id> <field>, /phone-call check."
  "What (help :phone-call) answers while the cell runs: the plugin's skill, in one topic.")

(defparameter +usage+
  "usage: /phone-call status [<id>] | log [<n>] | approve <id> | cancel <id> | consent <id> <field> | transcript <id> | check"
  "What /phone-call answers when it does not know the verb.")

(defun split-first (text)
  "TEXT as (values FIRST-WORD THE-REST)."
  (let* ((text (nlk:trimmed (or text "")))
         (space (position #\Space text)))
    (if space
        (values (subseq text 0 space) (nlk:trimmed (subseq text (1+ space))))
        (values text ""))))

(defun check-text ()
  "Which settings are set — names only, never a value — and how the cell will dial."
  (with-output-to-string (out)
    (format out "phone-call · ~a~%"
            (if (twilio-configured-p)
                "Twilio is configured: calls are real"
                "no Twilio account: calls are dry runs, nothing is dialed"))
    (format out "account_sid: ~a · auth_token: ~a · from_number: ~a~%"
            (if (account-sid) "set" "missing")
            (if (auth-token) "set" "missing")
            (if (from-number) "set" "missing"))
    (format out "allowlist: ~@[~d number~:p~]~@[~a~]~%"
            (let ((n (length (allowlist)))) (and (plusp n) n))
            (cond ((ignore-errors (setting :allow-all)) " (allow_all: every number may be dialed)")
                  ((null (allowlist)) " (empty: nothing may be dialed)")
                  (t " (allowed_numbers in the section)")))
    (format out "daily_call_limit: ~d · max_duration_s: ~d · quiet hours: ~a~%"
            (or (ignore-errors (setting :daily-call-limit)) +daily-limit-default+)
            (or (ignore-errors (setting :max-duration-s)) +default-duration-s+)
            (if (ignore-errors (setting :enforce-quiet-hours))
                (format nil "08:00-21:00 at UTC~@[~d~]" (ignore-errors (setting :callee-utc-offset)))
                "off"))
    (format out "user_name: ~a · callback_number: ~a~%"
            (user-name)
            (or (nonempty (ignore-errors (setting :callback-number))) "none"))))

(defun approve-text (id)
  "The operator's one gesture: approve this call, then place it."
  (unless (plusp (length id))
    (return-from approve-text "which call? /phone-call approve <id>, the id phone_prepare answered"))
  (approve-call id)
  (let ((placed (handler-case (place-call id)
                  (phone-call-error (condition)
                    (format nil "approved, but not dialed: ~a"
                            (phone-call-error-detail condition))))))
    (format nil "~a~%~a" placed
            (if (terminal-p (call-state (get-call id)))
                (format nil "/phone-call transcript ~a reads what was said." id)
                (format nil "/phone-call status ~a follows the call." id)))))

(defun cancel-text (id)
  "Cancel an unapproved card, or a call that has not been dialed."
  (unless (plusp (length id))
    (return-from cancel-text "which call? /phone-call cancel <id>"))
  (let ((call (get-call id)))
    (unless call (return-from cancel-text (format nil "there is no call ~a" id)))
    (transition id "canceled" :error "canceled by the operator")
    (format nil "call ~a to ~a is canceled; nothing is dialed." id (getf call :callee))))

(defun consent-text (rest)
  "Grant one sensitive field for one call: /phone-call consent <id> <field>."
  (multiple-value-bind (id field) (split-first rest)
    (cond ((zerop (length id)) "usage: /phone-call consent <id> card|address|id_number|bank|email|phone")
          ((zerop (length field))
           (format nil "which field? one of ~{~a~^, ~}" +sensitive-fields+))
          (t (let* ((call (get-call id))
                    (name (string-downcase (nlk:trimmed field))))
               (unless call (return-from consent-text (format nil "there is no call ~a" id)))
               (unless (member name +sensitive-fields+ :test #'equal)
                 (return-from consent-text
                   (format nil "~a is not a field a call may share: ~{~a~^, ~}"
                           name +sensitive-fields+)))
               (unless (or (member name (getf call :sensitive-requested) :test #'equal)
                           (let ((found nil))
                             (when (hash-table-p (getf call :facts))
                               (maphash (lambda (key value)
                                          (when (equal (fact-field key value) name)
                                            (setf found t)))
                                        (getf call :facts)))
                             found))
                 (return-from consent-text
                   (format nil "call ~a did not ask to share ~a; nothing to grant" id name)))
               (update-call id :granted (adjoin name (call-granted call) :test #'equal))
               (format nil "consent given: ~a may be shared on call ~a.~%~a"
                       name id (format-card (get-call id))))))))

(defun command-verdict (thunk)
  "Run THUNK — one command's verb — with its refusal caught: a command
answers the operator, it never signals into the shell."
  (handler-case (funcall thunk)
    (phone-call-error (condition)
      (format nil "phone-call: ~a" (phone-call-error-detail condition)))))

(defun run-command (args session-id)
  "One /phone-call invocation: the original's slash command, over the same verbs."
  (declare (ignore session-id))
  (multiple-value-bind (verb rest) (split-first args)
    (command-verdict
     (lambda ()
     (cond
       ((zerop (length verb)) (log-text 5))
       ((string-equal verb "status") (status-answer (first (multiple-value-list (split-first rest))) nil))
       ((string-equal verb "log") (log-text (let ((n (split-first rest)))
                                              (and (plusp (length n)) (ignore-errors (parse-integer n))))))
       ((string-equal verb "approve") (approve-text rest))
       ((string-equal verb "cancel") (cancel-text rest))
       ((string-equal verb "consent") (consent-text rest))
       ((string-equal verb "transcript") (transcript-answer (first (multiple-value-list (split-first rest)))))
       ((string-equal verb "check") (check-text))
       (t +usage+))))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell phone-call
  (:section ("phone-call")
    (:guide "a Twilio account is all a real call needs: account_sid, auth_token and from_number from console.twilio.com, and the numbers the operator may call in allowed_numbers — with that list empty nothing is dialed; then /phone-call check, phone_prepare, and /phone-call approve <id>")
    ("account_sid" :string :doc "the Twilio Account SID (console.twilio.com)")
    ("auth_token" :secret :doc "the Twilio auth token; kept in the config, never logged")
    ("from_number" :string :doc "the Twilio number the call is placed from, in E.164")
    ("allowed_numbers" :list
     :doc "the numbers this cell may dial, in E.164; empty refuses every number")
    ("allow_all" :boolean :default nil
     :doc "the developer's opt-out of the allowlist: every number may be dialed")
    ("user_name" :string :doc "who the agent calls on behalf of, said in the disclosure")
    ("callback_number" :string :doc "a callback number the call may give, in E.164")
    ("daily_call_limit" :integer :default 10 :min 1
     :doc "calls per rolling 24 hours")
    ("max_duration_s" :integer :default 300 :min 30
     :doc "the default hard cap on one call, in seconds")
    ("enforce_quiet_hours" :boolean :default nil
     :doc "refuse calls outside 08:00 to 21:00 callee time; off by default, as in the original")
    ("callee_utc_offset" :integer
     :doc "the callee's offset from UTC in hours, needed when quiet hours are on")
    ("voice" :string :default "Polly.Joanna"
     :doc "the Twilio text voice the message is spoken in")
    ("language" :string :default "en"
     :doc "the spoken language of a call, when the model names none")
    ("timeout_seconds" :integer :default 20 :min 1
     :doc "one Twilio request's ceiling"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (list :account-sid (nonempty (getf values :account-sid))
                     :auth-token (nonempty (getf values :auth-token))
                     :from-number (nonempty (getf values :from-number))
                     :allowed-numbers (getf values :allowed-numbers)
                     :allow-all (getf values :allow-all)
                     :user-name (nonempty (getf values :user-name))
                     :callback-number (nonempty (getf values :callback-number))
                     :daily-call-limit (getf values :daily-call-limit)
                     :max-duration-s (getf values :max-duration-s)
                     :enforce-quiet-hours (getf values :enforce-quiet-hours)
                     :callee-utc-offset (getf values :callee-utc-offset)
                     :voice (nonempty (getf values :voice))
                     :language (nonempty (getf values :language))
                     :timeout-seconds (getf values :timeout-seconds))))
  (:help :phone-call
         "phone-call: approved outbound phone calls with an AI disclosure - (help :phone-call)"
         +primer+)
  (:command "phone-call" 'run-command
            :description "Phone Call: prepare, approve, status, transcript"
            :argument-hint "status [<id>] | approve <id> | cancel <id> | consent <id> <field> | transcript <id> | log | check"
            :session nil)
  (:tool "phone_prepare" (nlk:contract 'phone-prepare-call) #'tool-prepare)
  (:tool "phone_call" (nlk:contract 'phone-call-call) #'tool-call)
  (:tool "phone_status" (nlk:contract 'phone-status-call) #'tool-status)
  (:tool "phone_transcript" (nlk:contract 'phone-transcript-call) #'tool-transcript)
  (:start (lambda ()
            (reset-store)
            (nle:notice
             (format nil "phone-call: ready — ~a; /phone-call check says why"
                     (if (twilio-configured-p)
                         "Twilio is set, calls are real"
                         "no Twilio account, so calls are dry runs"))
             :key "nodecode-phone-call")
            (nle:on-stop (lambda ()
                           (reset-store)
                           (nle:notice nil :key "nodecode-phone-call"))))))
