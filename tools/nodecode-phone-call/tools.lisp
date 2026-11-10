;;;; tools.lisp --- the original's four tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; phone_prepare never dials, phone_call dials only a call the operator
;;;; approved, phone_status reads the state and refreshes it from Twilio, and
;;;; phone_transcript answers what was said. The names and the jobs are the
;;;; original's; what the callee hears is a scripted TwiML message where the
;;;; original ran a realtime voice session, so the cell writes down what it
;;;; said and the recording link is where the other side's words are.

(in-package #:nodecode-phone-call)

;;; --- what the model calls ---------------------------------------------------------

(nlk:define-contract phone-prepare-call
  (:doc "Prepare an outbound phone call and answer the approval card. Never places a call.
Use before phone_call. Nothing is dialed until the operator approves with /phone-call approve.")

  ("callee_name" :text :required t
   :doc "who is called, including any person the user named, e.g. \"Dr. Smith's dental office\"")
  ("number" :text :required t :slot phone-number
   :doc "the number to dial in E.164, e.g. +14155550123 (international format, country code first)")
  ("number_source" :string
   :doc "'user', or the URL where the number was found; the card shows it")
  ("goal" :text :required t
   :doc "one full sentence: what to do, with whom, when")
  ("message" :string
   :doc "what the callee is told after the AI disclosure, in the words to say; the goal when absent")
  ("facts_to_share" :object
   :doc "EVERY non-sensitive detail the callee may need: person_to_see, service, date, time_window,
party_size, booking name. The call knows nothing else. A cvv, password or one-time code here is refused.")
  ("sensitive_fields" :array :of :string
   :doc "fields that need the operator's per-call consent: card, address, id_number, bank, email, phone")
  ("language" :string :doc "the spoken language of the call, e.g. en; the section's default otherwise")
  ("max_duration_s" :integer
   :doc "the hard cap for this call in seconds, 30 to 900; the section's default otherwise")
  ("record" :boolean :doc "record the answer, so the operator can hear what the other side said")
  ("leave_voicemail" :boolean
   :doc "the call may reach a machine: keep the message short and give the callback number")
  ("override_quiet_hours" :boolean
   :doc "the operator asked for this call outside 08:00 to 21:00 callee time"))

(nlk:define-contract phone-call-call
  (:doc "Place a call the operator has already approved. Answers the call's state and Twilio's
sid, or refuses with why. One approval dials once, and nothing is redialed on its own.")
  ("call_id" :text :required t
   :doc "the id phone_prepare answered; the operator approves it with /phone-call approve <id>"))

(nlk:define-contract phone-status-call
  (:doc "One call's state and result, or the recent call log when no id is given. A call that is
still live is refreshed from Twilio first.")
  ("call_id" :string :doc "the call to read; every recent call when absent")
  ("limit" :integer :doc "how many of the recent calls, at most 50; ten when absent"))

(nlk:define-contract phone-transcript-call
  (:doc "What was said on a call: the lines the cell spoke, and a link to Twilio's recording of
the answer when the call was recorded. Ask only when the operator wants it.")
  ("call_id" :text :required t :doc "the call to read"))

;;; --- preparing --------------------------------------------------------------------

(defun forbidden-fact-refusal (facts)
  "Why FACTS may never be shared, or NIL."
  (let ((why nil))
    (when (hash-table-p facts)
      (maphash (lambda (key value)
                 (let ((field (fact-field key value)))
                   (when (and (not why) (member field +forbidden-fields+ :test #'equal))
                     (setf why (format nil "the fact ~a is a ~a value, which is never shared"
                                        key field)))))
               facts))
    why))

(defun supersede-awaiting ()
  "One card at a time: a new prepare replaces the unapproved card before it."
  (dolist (call (recent-calls 50))
    (when (pending-approval-p call)
      (ignore-errors (transition (getf call :id) "canceled" :error "superseded")))))

(defun prepare-call (call)
  "PHONE_PREPARE: check, write the card, install nothing that can dial."
  (multiple-value-bind (number why) (validate-number (phone-prepare-call-phone-number call))
    (when (null number) (fail "~a" why))
    (let ((allow (allowlist-refusal number)))
      (when allow (fail "~a" allow)))
    (let ((quiet (quiet-hours-refusal (now) (phone-prepare-call-override-quiet-hours call))))
      (when quiet (fail "~a" quiet)))
    (let ((facts (phone-prepare-call-facts-to-share call)))
      (let ((bad (forbidden-fact-refusal facts)))
        (when bad (fail "~a" bad)))
      (let ((requested (sensitive-requested (phone-prepare-call-sensitive-fields call)))
            (duration (clamp-duration (phone-prepare-call-max-duration-s call))))
        (supersede-awaiting)
        (let* ((id (format nil "c~d" (incf *counter*)))
               (call (remember
                      (list :id id
                            :state "awaiting_approval"
                            :callee (phone-prepare-call-callee-name call)
                            :number number
                            :number-source (or (phone-prepare-call-number-source call) "")
                            :goal (phone-prepare-call-goal call)
                            :message (or (phone-prepare-call-message call) "")
                            :facts facts
                            :sensitive-requested requested
                            :granted '()
                            :language (or (nonempty (phone-prepare-call-language call))
                                          (or (nonempty (ignore-errors (setting :language))) "en"))
                            :max-duration-s duration
                            :record (phone-prepare-call-record call)
                            :leave-voicemail (phone-prepare-call-leave-voicemail call)
                            :override-quiet-hours (phone-prepare-call-override-quiet-hours call)
                            :created-at (now)
                            :lines '()))))
          (format nil "~a~%Nothing is dialed yet. The operator approves with /phone-call approve ~a \
(one use, fifteen minutes), or cancels with /phone-call cancel ~a."
                  (format-card call) id id))))))

;;; --- dialing ----------------------------------------------------------------------

(defun place-call (call-id)
  "PHONE_CALL: the approval is spent here, and only here is anything dialed."
  (let ((call (get-call call-id)))
    (unless call (fail "there is no call ~a" call-id))
    (let ((why (approval-refusal call-id)))
      (when why (fail "not approved: ~a" why)))
    (multiple-value-bind (number why) (validate-number (getf call :number))
      (when (or (null number) (not (equal number (getf call :number))))
        (fail "~a" (or why "the number failed validation")))
      (let ((allow (allowlist-refusal number)))
        (when allow (fail "~a" allow)))
      (let ((quiet (quiet-hours-refusal (now) (getf call :override-quiet-hours))))
        (when quiet (fail "~a" quiet)))
      (let ((rate (rate-limit-refusal (getf call :number) call-id)))
        (when rate (fail "~a" rate)))
      (unless (consume-approval call-id)
        (fail "the approval for ~a was already used" call-id))
      (setf call (transition call-id "dialing" :dialed-at (now)))
      (add-line call-id "agent" (opening-line (getf call :record)))
      (add-line call-id "agent" (call-message call))
      (if (not (twilio-configured-p))
          (progn
            (transition call-id "completed" :outcome "dry_run" :duration-s 0)
            (format nil "Dry run: ~a was not dialed, because no Twilio account is set (account_sid, \
auth_token and from_number in the phone-call section). The call said: \"~a ~a\". \
Give the operator the sid, approve, and set the three settings to make it real."
                    (getf call :number) (opening-line (getf call :record)) (call-message call)))
          (multiple-value-bind (sid error) (create-call call (twiml-for call))
            (cond
              (error (transition call-id "failed" :error error :outcome "failed")
                     (fail "dialing failed: ~a" error))
              (t (update-call call-id :provider-sid sid :outcome "placed")
                 (format nil "Ringing ~a (~a) · call ~a is dialing, Twilio sid ~a. The callee hears \
the AI disclosure first: \"~a\" · /phone-call status ~a follows it; /phone-call transcript ~a reads \
what was said."
                         (getf call :callee) (getf call :number) call-id sid
                         (opening-line (getf call :record)) call-id call-id))))))))

;;; --- reading ----------------------------------------------------------------------

(defparameter +twilio-states+
  '(("queued" . "dialing") ("initiated" . "dialing") ("ringing" . "ringing")
    ("in-progress" . "in_progress") ("completed" . "completed") ("busy" . "busy")
    ("no-answer" . "no_answer") ("failed" . "failed") ("canceled" . "canceled"))
  "Twilio's call status as this cell's state.")

(defun refresh-call (call)
  "Ask Twilio where CALL stands, when it is live and this cell may ask.
Answers a note, or NIL. Best effort: a refused refresh is not an error."
  (let ((sid (getf call :provider-sid)))
    (when (and sid (twilio-configured-p) (not (terminal-p (call-state call))))
      (multiple-value-bind (info error) (fetch-call sid)
        (declare (ignore error))
        (when info
          (let* ((status (getf info :status))
                 (state (cdr (assoc status +twilio-states+ :test #'equal))))
            (when (and state (not (equal state (call-state call))))
              (ignore-errors
                (transition (getf call :id) state
                            :outcome (if (equal state "completed") "answered" state)
                            :duration-s (let ((d (getf info :duration)))
                                          (and (stringp d) (plusp (length d))
                                               (ignore-errors (parse-integer d)))))))
            (getf call :id)))))))

(defun call-line (call)
  "CALL as one line of the log."
  (format nil "~a · ~a · ~a (~a)~@[ · ~a~]"
          (getf call :id) (call-state call) (getf call :callee) (getf call :number)
          (nonempty (getf call :outcome))))

(defun status-text (call)
  "One call, whole: state, brief, outcome and where to read more."
  (let* ((id (getf call :id))
         (granted (call-granted call)))
    (multiple-value-bind (safe withheld) (build-safe-facts (getf call :facts) granted)
      (with-output-to-string (out)
        (format out "~a~%" (call-line call))
        (format out "Goal: ~a~%" (redact-text (getf call :goal) granted))
        (format out "Opening line: \"~a\"~%" (opening-line (getf call :record)))
        (when (getf call :provider-sid)
          (format out "Twilio sid: ~a~%" (getf call :provider-sid)))
        (when safe
          (format out "Will share: ~{~a~^; ~}~%"
                  (mapcar (lambda (pair) (format nil "~a: ~a" (car pair) (value-text (cdr pair))))
                          safe)))
        (when withheld
          (format out "Withheld: ~{~a~^; ~}~%" withheld))
        (when (getf call :duration-s)
          (format out "Duration: ~d s (est. cost $~,2f)~%" (getf call :duration-s)
                  (estimate-cost-usd (getf call :duration-s))))
        (when (nonempty (getf call :error))
          (format out "Why: ~a~%" (getf call :error)))
        (if (terminal-p (call-state call))
            (format out "/phone-call transcript ~a reads what was said.~%" id)
            (format out "Waiting for the operator: /phone-call approve ~a · /phone-call cancel ~a~%"
                    id id))))))

(defun log-text (limit)
  "The recent calls, newest first."
  (let ((calls (recent-calls (or limit 10))))
    (if (null calls)
        "no call prepared yet: phone_prepare writes the card, /phone-call approve dials it"
        (with-output-to-string (out)
          (format out "~d call~:p, newest first:~%" (length calls))
          (dolist (call calls) (format out "  ~a~%" (call-line call)))))))

(defun status-answer (call-id limit)
  "PHONE_STATUS: one call, or the log."
  (if (nonempty call-id)
      (let ((call (expire-if-stale call-id)))
        (unless call (fail "there is no call ~a" call-id))
        (refresh-call call)
        (status-text (get-call (getf call :id))))
      (log-text (and limit (max 1 (min 50 limit))))))

(defun transcript-answer (call-id)
  "PHONE_TRANSCRIPT: what the cell said, and the recording when there is one."
  (let ((call (get-call call-id)))
    (unless call (fail "there is no call ~a" call-id))
    (let ((lines (getf call :lines))
          (recording (and (getf call :record) (terminal-p (call-state call))
                          (getf call :provider-sid) (twilio-configured-p)
                          (fetch-recording (getf call :provider-sid)))))
      (when recording (update-call call-id :recording-url recording))
      (with-output-to-string (out)
        (format out "Call ~a · ~a · ~a~%~%" call-id (call-state call) (getf call :callee))
        (if (null lines)
            (format out "Nothing was said yet: the call has not been dialed.~%")
            (dolist (line lines)
              (format out "  ~a: ~a~%" (if (equal (first line) "agent") "agent" "operator")
                      (second line))))
        (when (getf call :recording-url)
          (format out "~%Recording of the answer: ~a~%" (getf call :recording-url)))
        (unless (getf call :record)
          (format out "~%Nothing was recorded (record was off on this call).~%"))))))

;;; --- the tool bodies --------------------------------------------------------------

(defun tool-prepare (call) (prepare-call call))
(defun tool-call (call) (place-call (phone-call-call-call-id call)))
(defun tool-status (call) (status-answer (phone-status-call-call-id call)
                                         (phone-status-call-limit call)))
(defun tool-transcript (call) (transcript-answer (phone-transcript-call-call-id call)))
