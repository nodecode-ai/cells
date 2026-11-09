;;;; cell.lisp --- idle check-ins: the rule, the tool, /checkin, the two hooks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One rule, one state file, one tool, carried over from the original
;;;; plugin: after the chat has been quiet for base_minutes, the cell puts
;;;; one prompt into the bound session and the answer is a normal turn, so it
;;;; reaches the operator wherever that session's chats go. The wait doubles
;;;; for each check-in they leave unanswered, up to a week, and nothing goes
;;;; out outside their waking hours. The morning brief, when it is on, is one
;;;; short brief a day just after the hours open.
;;;;
;;;; The model's tool can only make check-ins quieter; reaching further --
;;;; checking in more, resuming, wider hours, the brief on -- is the
;;;; operator's own, and the tool answers the exact command to send.

(in-package #:nodecode-checkin)

(nlk:define-peripheral checkin :not-running t)

(nlk:access (admission nlk::active-input-admission))

;;; --- the boundary ---------------------------------------------------------------

(nlk:define-contract checkin-request
  (:doc "Read or change the operator's proactive check-ins: after their chat has been quiet for
base_minutes you put one prompt into the bound session and its answer reaches them, doubling
the wait for each check-in they leave unanswered and only inside their waking hours. This tool
can only make check-ins quieter; anything that reaches further is refused with the exact
command to tell the operator to send.")
  ("action" :text :required t :options '("status" "set")
   :doc "status reads the rule and the next check-in; set changes it (quieter only)")
  ("level" :text :options '("less" "normal" "more")
   :doc "how often: less is every 4 hours of quiet, normal 2, more 1")
  ("base_minutes" :integer
   :doc "quiet minutes before a check-in, 15 to 1440; a stored lower value is kept until changed")
  ("active_start" :integer :doc "the first local hour a check-in may fire, 0-23")
  ("active_end" :integer :doc "the hour their quiet day begins again, 0-23")
  ("utc_offset_minutes" :integer
   :doc "their offset from UTC in minutes, east positive; -300 is New York in winter")
  ("paused" :text :options '("on" "off") :doc "on pauses every check-in; off resumes")
  ("morning_brief" :text :options '("on" "off")
   :doc "one short brief a day just after the hours open; only the operator turns it on"))

(defun answer (&rest members)
  "MEMBERS as the JSON text a tool answers with."
  (nlk:encode-json-object (apply #'nlk:make-json-object members)))

;;; --- the rule ---------------------------------------------------------------------

(defparameter *levels* '(("less" . 240) ("normal" . 120) ("more" . 60))
  "The three named waits, in minutes, and their order of reach.")

(defun level-name (minutes)
  "The level MINUTES is, or custom."
  (or (car (rassoc minutes *levels* :test #'=)) "custom"))

(defun busy-p (session)
  "Whether a turn of SESSION is running now."
  (and (nlk:store-open-p)
       (let ((busy (ignore-errors (nlk:active-turn-p session))))
         (and busy t))))

(defun due-at (now offset)
  "When the next check-in is due: the wait from the last exchange, last
check-in or binding, doubled per unanswered one, moved to their next active
hour."
  (let* ((anchor (max (or (state :last-activity) 0)
                      (or (state :last-nudge) 0)
                      (or (state :bound-at) 0)))
         (wait (wait-seconds (live :base) (or (state :nudges) 0))))
    (next-active (+ anchor wait) offset (live :start) (live :end))))

(defun briefed-p (now offset)
  "Whether the local day NOW falls in has had its brief."
  (= (or (state :last-brief) -1) (local-midnight now offset)))

(defun clock-sentence (utc offset)
  "UT, said the way a person says it: Tuesday 08:14 in their own hours."
  (let* ((midnight (local-midnight utc offset))
         (minutes (mod (floor (- utc midnight) 60) 1440))
         (day (mod (floor midnight 86400) 7)))
    (format nil "~a ~2,'0d:~2,'0d"
            (nth day '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
            (floor minutes 60) (mod minutes 60))))

(defun clock-words (utc offset)
  "Only the hour and minute, as the operator reads a clock."
  (let* ((minutes (mod (+ (* (floor utc 3600) 60) offset) 1440))
         (hour (floor minutes 60)))
    (format nil "~d:~2,'0d ~a" (if (zerop (mod hour 12)) 12 (mod hour 12)) (mod minutes 60)
            (if (< hour 12) "AM" "PM"))))

(defun day-words (utc offset)
  "Today, tomorrow or a weekday, for the day UTC falls on."
  (let ((days (- (floor (local-midnight utc offset) 86400)
                 (floor (local-midnight (funcall *clock*) offset) 86400))))
    (cond ((<= days 0) "today")
          ((= days 1) "tomorrow")
          ((<= days 6) (nth (mod (floor (local-midnight utc offset) 86400) 7)
                            '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")))
          (t (format nil "in ~d days" days)))))

;;; --- putting a prompt in ----------------------------------------------------------

(defun submitted (admission)
  "The disposition and turn id of what SUBMIT answered."
  (if (typep admission 'nlk::active-input-admission)
      (values (admission.disposition admission) (admission.turn-id admission))
      (values admission nil)))

(defun submit-prompt (text &key (source-id "check-in"))
  "Put TEXT into the bound session through the one in-process ingress.
=> the disposition."
  (let ((session (live :session)))
    (when (nlk:store-open-p)
      (ignore-errors
        (unless (nlk:session-exists-p session)
          (nlk:standby-session :id session))))
    (multiple-value-bind (disposition turn)
        (submitted (nle:submit session text
                                :command-id (format nil "checkins-~a-~a" source-id (funcall *clock*))
                                :source "checkins" :source-id source-id))
      (setf *our-turn* turn)
      disposition)))

(defun checked-in (now disposition &key asked)
  "Record what a check-in cost the back-off, and answer DISPOSITION."
  (unless asked
    (setf (state :last-nudge) now
          (state :nudges) (1+ (or (state :nudges) 0))))
  (setf (state :last-result)
        (if (and disposition (member disposition '(:started :queued :steered)))
            (string-downcase (symbol-name disposition))
            "refused"))
  (save-state)
  disposition)

(defun send-check-in (now offset &key asked)
  "The one check-in NOW calls for: the prompt, the ingress, the back-off."
  (let ((elapsed (- now (or (state :last-activity) (state :bound-at) now))))
    (checked-in now
                (submit-prompt
                 (format nil *check-in-prompt* *marker* (ago-text elapsed) (clock-sentence now offset)
                         *voice* *safety* (if asked *asked* *silent*))
                 :source-id "check-in")
                :asked asked)))

(defun send-brief (now offset)
  "The day's one morning brief, and the day marked spent before it goes out."
  (setf (state :last-brief) (local-midnight now offset)
        (state :last-activity) now)
  (save-state)
  (checked-in now
              (submit-prompt (format nil *brief-prompt* *brief-marker*
                                     (clock-sentence now offset) *voice* *safety* *silent*)
                             :source-id "brief")
              :asked t))

(defun tick (&key (now (funcall *clock*)))
  "One pass of the idle rule => the disposition a check-in went out with, or NIL."
  (when (bound-p)
    (let* ((offset (live :offset))
           (start (live :start))
           (end (live :end))
           (hour (local-hour now offset)))
      (with-state
        (cond
          ((live :paused) nil)
          ((busy-p (live :session)) nil)
          ((not (in-window-p start end hour)) nil)
          ;; the brief first: the morning's check-in waits for it to go out
          ((and (live :brief) (not (briefed-p now offset)))
           (let ((at (brief-at now offset start)))
             (cond ((< now at) nil)
                   ((< (- now (or (state :last-user) 0)) (* 2 3600))
                    (setf (state :last-brief) (local-midnight now offset)
                          (state :last-result) "brief skipped: operator active")
                    (save-state)
                    nil)
                   (t (send-brief now offset)))))
          ((< now (due-at now offset)) nil)
          (t (send-check-in now offset)))))))

;;; --- the two hooks ----------------------------------------------------------------

(defun frame-turn-id (op)
  "The turn OP belongs to, when its payload says."
  (let ((payload (getf op :payload)))
    (and payload (nlk:json-value payload :string "turn_id"))))

(defvar *seen-turn* nil
  "The operator's last turn read off the frames, so one turn writes once.")

(defun observe-frame (op)
  "A live frame left the gateway: the operator's own turn is the exchange
the wait counts from, and one turn is noted once."
  (let ((session (getf op :session-id)))
    (when (and (bound-p) (equal session (live :session)))
      (let ((turn (frame-turn-id op)))
        (when turn
          (cond ((and *our-turn* (equal turn *our-turn*)) nil)
                ((equal turn *seen-turn*) nil)
                (t (with-state
                     (setf *seen-turn* turn *our-turn* nil)
                     (note-activity (funcall *clock*) :user t)))))))))

(defun voice-at-round (op)
  "At a turn's first round, the texting note rides its request."
  (when (and (equal "texting" (live :voice))
             (getf op :turn)
             (eql 0 (or (getf op :round) 0)))
    (ignore-errors
      (nlk:record-turn-context-prompt (getf op :turn) :text *voice-note*))))

;;; --- the tool ----------------------------------------------------------------------

(defparameter *tool-actions* '("status" "set")
  "The tool's two actions.")

(defun status-object ()
  "The rule as data: what the model reads and what /checkin prints."
  (let* ((now (funcall *clock*))
         (offset (live :offset))
         (due (and (bound-p) (not (live :paused)) (due-at now offset))))
    (apply #'nlk:make-json-object
           (append
            (list "bound" (bound-p)
                  "session" (live :session)
                  "level" (level-name (live :base))
                  "base_minutes" (live :base)
                  "active_start" (live :start)
                  "active_end" (live :end)
                  "utc_offset_minutes" offset
                  "paused" (and (live :paused) t)
                  "morning_brief" (and (live :brief) t)
                  "voice" (live :voice)
                  "nudges_since_reply" (or (state :nudges) 0)
                  "quiet_minutes_since_operator"
                  (and (state :last-activity) (round (- now (state :last-activity)) 60))
                  "last_result" (state :last-result))
            (if due
                (list "next_check_in" (format nil "~a ~a" (clock-words due offset) (day-words due offset)))
                (list "next_check_in" nil))))))

(defun offset-text (minutes)
  "MINUTES east of UTC, as a person writes it."
  (format nil "UTC~a~2,'0d:~2,'0d" (if (minusp minutes) #\- #\+) (floor (abs minutes) 60) (mod (abs minutes) 60)))

(defun wait-text (minutes)
  "MINUTES of quiet, in words."
  (if (zerop (mod minutes 60))
      (format nil "~d hour~p" (floor minutes 60) (floor minutes 60))
      (format nil "~d minute~p" minutes minutes)))

(defun status-text ()
  "The rule in one plain sentence."
  (let* ((now (funcall *clock*))
         (offset (live :offset))
         (due (and (bound-p) (not (live :paused)) (due-at now offset)))
         (head (format nil "Check-ins after ~a of quiet (level ~a), ~d:00-~d:00 ~a~a"
                       (wait-text (live :base)) (level-name (live :base))
                       (live :start) (live :end) (offset-text offset)
                       (if (live :paused) ", paused" ""))))
    (if (not (bound-p))
        (format nil "~a. No chat is bound yet: send /checkin on in the chat you want it in." head)
        (format nil "~a.~@[ Next ~a ~a.~]~@[ Brief next ~a.~]"
                head
                (and due (clock-words due offset)) (and due (day-words due offset))
                (and (live :brief) (not (briefed-p now offset))
                     (clock-words (brief-at now offset (live :start)) offset))))))

(defparameter +primer+
  "The check-in tool reads and quiets the operator's proactive check-ins. After their chat has
been quiet for base_minutes, the cell puts one prompt into the bound session; its answer is a
normal turn and reaches them wherever that session's chats go. The wait doubles for each
check-in they leave unanswered, up to a week, and nothing fires outside the hours
active_start..active_end in their own UTC offset. With morning_brief on, one short brief goes
out a day just after the hours open.

  action status   the rule, the bound chat and the next check-in
  action set      quieter only: level less, a higher base_minutes, paused on, an earlier
                  active_end or a later active_start, morning_brief off, or their
                  utc_offset_minutes. A change that would reach further is refused with
                  ask_user_to_send: tell the operator to send exactly that command.

Map what they say: \"check in less\" is level less; \"quiet for now\" is paused on; \"I'm in
New York\" is utc_offset_minutes -300 (east of UTC is positive); \"not before 9am\" is
active_start 9; \"stop the morning brief\" is morning_brief off. Only they turn the brief on,
with /checkin brief on, and only they bind the chat, with /checkin on typed in it."
  "What (help :checkin) answers while the cell runs.")

(defparameter +usage+
  "usage: /checkin [status | on [SESSION] | off | now | less | normal | more | pause | resume |
hours START-END | utc OFFSET-MINUTES | brief on|off | voice texting|default]"
  "What a word the command does not know answers.")

;;; --- settings, as the tool and the command move them --------------------------------

(defun proposed (changes &key tool)
  "The running values CHANGES would leave. Signals a plain error naming the
member a value does not fit."
  (let ((out (copy-list *live*)))
    (loop for (key value) on changes by #'cddr
          do (setf (getf out key) value))
    (let ((base (getf out :base))
          (start (getf out :start))
          (end (getf out :end))
          (offset (getf out :offset))
          (voice (getf out :voice)))
      (unless (and (integerp base) (<= (if tool 15 1) base 1440))
        (error "base_minutes must be a whole number from ~d to 1440" (if tool 15 1)))
      (unless (and (integerp start) (<= 0 start 23))
        (error "active_start must be an hour from 0 to 23"))
      (unless (and (integerp end) (<= 0 end 23))
        (error "active_end must be an hour from 0 to 23"))
      (when (= start end)
        (error "active_start and active_end must differ"))
      (unless (and (integerp offset) (<= -840 offset 840))
        (error "utc_offset_minutes must be from -840 to 840"))
      (unless (member voice '("texting" "default") :test #'equal)
        (error "voice must be texting or default")))
    out))

(defun reaches-further (current new)
  "The operator's command a change needs, when it makes check-ins reach
further; NIL when it only quiets them."
  (cond
    ((< (getf new :base) (getf current :base)) "/checkin less")
    ((and (getf current :paused) (not (getf new :paused))) "/checkin resume")
    ((not (subsetp (window-hours (getf new :start) (getf new :end))
                   (window-hours (getf current :start) (getf current :end))))
     (format nil "/checkin hours ~d-~d" (getf new :start) (getf new :end)))
    ((and (getf new :brief) (not (getf current :brief))) "/checkin brief on")
    (t nil)))

(defun commit (new)
  "Move every value NEW differs in, into the running cell and the section."
  (loop for (key value) on new by #'cddr
        unless (equal (getf *live* key) value)
          do (set-live key value))
  (values))

(defun change (changes &key tool)
  "Apply CHANGES: => (values OK TEXT-or-STATUS). With TOOL, a change that
reaches further is refused with the command to send."
  (handler-case
      (let* ((new (proposed changes :tool tool))
             (hint (and tool (reaches-further *live* new))))
        (if hint
            (values nil
                    (list :error "that change would make check-ins reach further; the tool can only make them quieter."
                          :ask_user_to_send hint))
            (progn (commit new) (values t nil))))
    (error (condition) (values nil (list :error (princ-to-string condition))))))

;;; --- the tool's body ---------------------------------------------------------------

(defun tool-set (arguments)
  "The changes a set action names, as a plist; the first error it names is
the answer."
  (let ((changes '()))
    (flet ((want (key value) (when value (setf (getf changes key) value))))
      (want :base (and (checkin-request-level arguments)
                       (cdr (assoc (checkin-request-level arguments) *levels* :test #'equal))))
      (want :base (checkin-request-base-minutes arguments))
      (want :start (checkin-request-active-start arguments))
      (want :end (checkin-request-active-end arguments))
      (want :offset (checkin-request-utc-offset-minutes arguments))
      (want :paused (and (checkin-request-paused arguments)
                         (equal "on" (checkin-request-paused arguments))))
      (want :brief (and (checkin-request-morning-brief arguments)
                        (equal "on" (checkin-request-morning-brief arguments)))))
    changes))

(defun run-checkin (arguments)
  "The checkin tool: status, or a change that only quiets the rule."
  (let ((action (checkin-request-action arguments)))
    (cond
      ((equal action "status") (answer "ok" t "rule" (status-object)))
      ((equal action "set")
       (let ((changes (tool-set arguments)))
         (if (null changes)
             (answer "ok" nil "error" "set needs at least one setting to change")
             (multiple-value-bind (ok problem) (change changes :tool t)
               (if ok
                   (answer "ok" t "rule" (status-object))
                   (answer "ok" nil "error" (getf problem :error)
                           "ask_user_to_send" (getf problem :ask_user_to_send)))))))
      (t (answer "ok" nil "error" (format nil "unknown action ~s" action))))))

;;; --- /checkin -----------------------------------------------------------------------

(defun on-text (session-id words)
  "Bind SESSION: the one the command was typed in, or the one they named."
  (let ((session (or (first words) session-id)))
    (if (or (null session) (zerop (length session)))
        "Name the chat to check in on: type /checkin on in it, or /checkin on SESSION."
        (progn (bind session)
               (format nil "Check-ins are on for ~a: one after ~a of quiet, ~d:00-~d:00 local time."
                       session (level-name (live :base)) (live :start) (live :end))))))

(defun hour-pair (word)
  "The (START END) a \"8-22\" is, or NIL."
  (let ((dash (position #\- word)))
    (when dash
      (let ((start (ignore-errors (parse-integer word :end dash)))
            (end (ignore-errors (parse-integer word :start (1+ dash)))))
        (and start end (list start end))))))

(defun change-text (changes)
  "Apply CHANGES from the command line, answering the rule or the error."
  (multiple-value-bind (ok problem) (change changes)
    (if ok (status-text) (format nil "~a" (getf problem :error)))))

(defun run-command (args session-id)
  "One /checkin invocation, the operator's own path: it may reach further.
=> (values TEXT DIALOG)."
  (let* ((words (nlk:split-words args))
         (verb (string-downcase (or (first words) "")))
         (rest (rest words)))
    (cond
      ((member verb '("" "status") :test #'equal) (values (status-text)))
      ((member verb '("on" "here" "bind") :test #'equal) (values (on-text session-id rest)))
      ((member verb '("off" "unbind") :test #'equal)
       (unbind) (values "Check-ins are off: nothing will text you."))
      ((equal verb "now")
       (values (if (not (bound-p))
                   "No chat is bound yet: send /checkin on in the chat you want it in."
                   (if (send-check-in (funcall *clock*) (live :offset) :asked t)
                       "Sent a check-in now; its answer lands in this chat."
                       "Couldn't start a check-in: the session refused the prompt."))))
      ((assoc verb *levels* :test #'equal)
       (values (change-text (list :base (cdr (assoc verb *levels* :test #'equal))))))
      ((equal verb "pause") (values (change-text (list :paused t))))
      ((equal verb "resume") (values (change-text (list :paused nil))))
      ((and (equal verb "hours") rest (hour-pair (first rest)))
       (destructuring-bind (start end) (hour-pair (first rest))
         (values (change-text (list :start start :end end)))))
      ((and (equal verb "utc") rest (ignore-errors (parse-integer (first rest))))
       (values (change-text (list :offset (parse-integer (first rest))))))
      ((and (equal verb "brief") rest (member (string-downcase (first rest)) '("on" "off") :test #'equal))
       (values (change-text (list :brief (equal "on" (string-downcase (first rest)))))))
      ((and (member verb '("voice" "tone") :test #'equal) rest
            (member (string-downcase (first rest)) '("texting" "default") :test #'equal))
       (values (change-text (list :voice (string-downcase (first rest))))))
      (t (values +usage+)))))

;;; --- start and stop -------------------------------------------------------------------

(defun start ()
  "Read the section, load the state, and start the ticker."
  (live-from-settings)
  (proposed '())
  (setf *state* (or (load-state) '())
        *our-turn* nil
        *seen-turn* nil)
  (when (and (bound-p) (null (state :bound-at)))
    (setf (state :bound-at) (funcall *clock*))
    (save-state))
  (setf *worker* (nlk:worker-start "checkins" (lambda () (tick) nil)
                                    :wake (lambda () (live :tick 60))))
  (nle:on-stop (lambda ()
                 (when *worker* (nlk:worker-stop *worker*) (setf *worker* nil))))
  (values))

(nle:define-cell checkin
  (:section ("checkin")
    (:guide "type /checkin on in the chat you want it in; utc_offset_minutes is your offset from UTC, east positive; base_minutes is the quiet before the first check-in")
    ("session" :string :required t
     :doc "the session to check in on; /checkin on in a chat sets it to that chat")
    ("base_minutes" :integer :default 120 :min 1
     :doc "quiet minutes before the first check-in; the wait doubles for each one left unanswered")
    ("active_start" :integer :default 8 :min 0
     :doc "the first local hour a check-in may fire, 0-23")
    ("active_end" :integer :default 22 :min 0
     :doc "the hour the quiet day begins again, 0-23")
    ("utc_offset_minutes" :integer :default 0 :min -840
     :doc "your offset from UTC in minutes, east positive: -300 is New York in winter")
    ("paused" :boolean :default nil :doc "pause every check-in without losing the rule")
    ("morning_brief" :boolean :default nil
     :doc "one short brief a day just after the hours open")
    ("voice" :choice :options '("texting" "default") :default "texting"
     :doc "how replies in a chat read: texting, or the model's own voice")
    ("tick_seconds" :integer :default 60 :min 15
     :doc "how often the rule looks at the clock"))
  (:tool "checkin" (nlk:contract 'checkin-request) 'run-checkin)
  (:help :checkin "checkin: after the chat goes quiet, one check-in prompt to the bound session; base_minutes, hours and the brief" +primer+)
  (:command "checkin" 'run-command
            :description "Idle check-ins: on, off, now, less, normal, more, pause, resume, hours 8-22, utc -300, brief on, voice texting"
            :argument-hint "[status|on|off|now|less|normal|more|pause|resume|hours 8-22|utc -300|brief on|off|voice texting]")
  (:hook :frame (nlk:observer #'observe-frame "checkin"))
  (:hook :round (nlk:observer #'voice-at-round "checkin"))
  (:start #'start))
