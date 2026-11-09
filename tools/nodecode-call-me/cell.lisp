;;;; cell.lisp --- the two tools, the command and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :call-me) answers the text below, and every request's help section
;;;; carries one line naming it.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "call-me": {"allowed_users": ["5551234567"], "names": "5551234567=Sam",
;;;;               "label": "Nodecode", "timeout": 30, "call_wait": 120}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from radres's Hermes Agent plugin (see NOTICE): the two verbs the
;;;; plugin gave its agent -- a text to the phone and a call that speaks a
;;;; sentence and hands the spoken answer back as a transcript -- the allowlist
;;;; both are fenced by, the session file, and the name-or-number recipient.

(in-package #:nodecode-call-me)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "The /call-me app on an iPhone gives a person a private phone number for their agents.
This cell gives the agent two things to do with that number, both fenced by the section's
allowed_users list: nobody else can be texted or rung, however the conversation goes.
  callme_text  {message, to}     Send a text to the operator's phone. It arrives in the
      /call-me thread for this agent, plain text, at most 2000 characters, and is read
      on a lock screen: lead with the fact, leave the narration to the transcript.
  callme_call  {text, to, wait_seconds}   Ring the phone, say TEXT out loud, and wait for
      the person's spoken answer, which comes back in the result as a transcript. Use it
      when a text is not enough: something urgent, or an answer you need to hear now. The
      text is spoken verbatim, so keep it to one or two sentences ending in a question that
      can be answered in one sentence. The call ends when they hang up or the wait runs
      out (30 to 300 seconds; the section's call_wait when unset), and the answer says
      missed when nobody picked up.
to names who: a 10-digit number from allowed_users, or a name listed in the section's
names; empty means the only allowed number. Everything else -- an unknown name, a number
off the list -- is refused before any request leaves, so a stranger is never reached.
The agent's own number, the one the person texts, is in the session file the service
minted; deleting it gets a fresh number. /callme says what is set and texts someone.
Prefer callme_text: a call interrupts a person, so ring only for what cannot wait."
  "What (help :call-me) answers while the cell runs.")

;;; --- answers and refusals -------------------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a verb cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (call-me-error (condition)
       (nle:failure (format nil "ERROR: /call-me: ~a" (call-me-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: /call-me: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun started-p ()
  "Whether the section is read and the verbs may run."
  (and *call-me* t))

;;; --- reading the section --------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, plus the numbers and names the verbs resolve against."
  (declare (ignore table))
  (let* ((numbers (parse-numbers (getf values :allowed-users)))
         (named (parse-names (or (getf values :names) "")))
         (plist (copy-list values)))
    (unless numbers
      (nlk:config-error "allowed_users holds no 10-digit /call-me number: open the /call-me app on the iPhone and copy the number it shows"))
    (setf (getf plist :numbers) numbers
          (getf plist :names) (loop for number in numbers
                                     for pair = (assoc number named :test #'string=)
                                     when pair collect pair)
          (getf plist :label) (or (nonblank (getf values :label)) "Nodecode")
          (getf plist :timeout) (or (getf values :timeout) 30)
          (getf plist :call-wait) (max +min-wait+ (min +max-wait+ (or (getf values :call-wait) 120))))
    plist))

(defun recipient (who)
  "WHO -- a number or a name -- as an allowed number, or NIL."
  (resolve-recipient who (setting :numbers) (setting :names)))

(defun unknown-recipient-answer (who)
  "The refusal for a WHO that resolves to nobody allowed: it says who is allowed."
  (let ((allowed (format nil "~{~a~^, ~}" (loop for number in (setting :numbers)
                                                  collect (name-of number)))))
    (if (nonblank (princ-to-string (or who "")))
        (refuse "~a is not an allowed /call-me number or name; allowed: ~a" who allowed)
        (refuse "say who to reach: allowed: ~a" allowed))))

;;; --- callme_text ------------------------------------------------------------------

(defun text-message (call)
  "The callme_text tool: one text, to one allowed number."
  (answering
    (let ((message (nonblank (text-message-message call)))
          (to (recipient (text-message-to call))))
      (unless message
        (return-from text-message (refuse "the message is empty: say the one fact to send")))
      (unless to
        (return-from text-message (unknown-recipient-answer (text-message-to call))))
      (let ((object (send-message to message)))
        (nlk:encode-json-object
         (nlk:json-object "sent" t
                           "to" (name-of to)
                           "message_id" (string-field object "message_id")))))))

;;; --- callme_call --------------------------------------------------------------------

(defun clamp-wait (seconds)
  "SECONDS inside the wait the service accepts, 30 to 300, defaulting to the
section's own call_wait."
  (let ((wait (or seconds (setting :call-wait) 120)))
    (max +min-wait+ (min +max-wait+ wait))))

(defun make-call (call)
  "The callme_call tool: ring the phone, speak, and answer the transcript."
  (answering
    (let ((text (nonblank (spoken-call-text call)))
          (to (recipient (spoken-call-to call))))
      (unless text
        (return-from make-call (refuse "the text is empty: say the one sentence to speak")))
      (unless to
        (return-from make-call (unknown-recipient-answer (spoken-call-to call))))
      (let* ((wait (clamp-wait (spoken-call-wait-seconds call)))
             (object (place-call to text wait))
             (status (or (nonblank (string-field object "status")) "unknown"))
             (answer (nlk:json-object
                      "status" status
                      "transcript" (string-field object "transcript")
                      "to" (name-of to))))
        (when (and (not (string= status "completed"))
                   (zerop (length (string-field object "transcript"))))
          (setf answer (nlk:json-object
                        "status" status
                        "transcript" ""
                        "to" (name-of to)
                        "hint" "Nobody answered. Send a text instead of calling again right away.")))
        (nlk:encode-json-object answer)))))

;;; --- ring and text from eval ---------------------------------------------------------

(defun ring (text &rest pairs)
  "The tool's own body, from eval:
(call-me:ring \"The build is green. Deploy now?\" :to \"Sam\")
=> the answer the model would read: the transcript, or the refusal in words."
  (make-call
   (decode-spoken-call
    (apply #'nlk:make-json-object
           "text" text
           (loop for (key value) on pairs by #'cddr
                 append (list (string-downcase (substitute #\_ #\- (symbol-name key))) value))))))

(defun text (message &rest pairs)
  "The callme_text tool's own body, from eval:
(call-me:text \"The deploy is live.\" :to \"5551234567\")."
  (text-message
   (decode-text-message
    (apply #'nlk:make-json-object
           "message" message
           (loop for (key value) on pairs by #'cddr
                 append (list (string-downcase (substitute #\_ #\- (symbol-name key))) value))))))

;;; --- what the operator reads ---------------------------------------------------------

(defun saved-number ()
  "The agent's own /call-me number, as saved, or NIL before a session is minted."
  (let ((state (and *call-me* (load-state))))
    (and state (nonblank (string-field state "number")))))

(defun status ()
  "What /callme answers: the numbers it may reach, and the agent's own."
  (if (not (started-p))
      (format nil "call-me is not running: turn the nodecode-call-me cell on in /cells, ~
or (restart-cells \"nodecode-call-me\").")
      (format nil "~{~a~^~%~}"
              (list "/call-me (nodecode-call-me):"
                    (format nil "  reachable  ~{~a~^, ~}"
                            (loop for number in (setting :numbers) collect (name-of number)))
                    (format nil "  thread     ~a" (setting :label))
                    (format nil "  this agent ~a"
                            (or (saved-number)
                                "no number yet: the first text or call mints one"))
                    (format nil "  call wait  ~a seconds" (setting :call-wait))
                    "  The model texts with callme_text and rings with callme_call."))))

(defun status-dialog ()
  "The rows the bare /callme offers under its answer."
  (nle:list-dialog
   :title "/call-me"
   :summary "A phone number for the agent: text it, or let it ring you."
   :rows (list (nle:ask-row "Text someone" "Who, and what to say" "/callme text "
                             :placeholder "5551234567 the build is green" :verb "send"))))

(defun command-text (words)
  "`/callme text WHO MESSAGE' => what went out, or why it did not."
  (let ((to (first words))
        (message (format nil "~{~a~^ ~}" (rest words))))
    (cond ((null words) "Usage: /callme text WHO MESSAGE")
          ((zerop (length (nlk:trimmed message))) "Usage: /callme text WHO MESSAGE")
          (t (let ((number (recipient to)))
               (if (null number)
                   (format nil "~a is not on allowed_users; text one of ~{~a~^, ~}."
                           to (loop for n in (setting :numbers) collect (name-of n)))
                   (handler-case
                       (let ((object (send-message number message)))
                         (format nil "Texted ~a through /call-me (message ~a)."
                                 (name-of number)
                                 (or (nonblank (string-field object "message_id")) "sent")))
                     (call-me-error (condition)
                       (format nil "/call-me did not take the text: ~a"
                               (call-me-error-detail condition))))))))))

(defun command-call (words)
  "`/callme call WHO TEXT' => the spoken answer, or why there was none."
  (let ((to (first words))
        (text (format nil "~{~a~^ ~}" (rest words))))
    (cond ((null words) "Usage: /callme call WHO TEXT")
          ((zerop (length (nlk:trimmed text))) "Usage: /callme call WHO TEXT")
          (t (let ((number (recipient to)))
               (if (null number)
                   (format nil "~a is not on allowed_users; ring one of ~{~a~^, ~}."
                           to (loop for n in (setting :numbers) collect (name-of n)))
                   (handler-case
                       (let* ((object (place-call number text (setting :call-wait)))
                              (spoken (string-field object "transcript")))
                         (if (plusp (length spoken))
                             (format nil "~a said: ~a" (name-of number) spoken)
                             (format nil "~a did not answer (call ~a)."
                                     (name-of number) (or (nonblank (string-field object "status")) "missed"))))
                     (call-me-error (condition)
                       (format nil "/call-me did not take the call: ~a"
                               (call-me-error-detail condition))))))))))

(defun run-command (args session-id)
  "The /callme command: status by default, a text or a call on a verb."
  (declare (ignore session-id))
  (let* ((words (uiop:split-string (nlk:trimmed (or args "")) :separator '(#\space)))
         (verb (string-downcase (or (first words) ""))))
    (cond ((or (string= verb "") (string= verb "status"))
           (values (status) (and (started-p) (status-dialog))))
          ((string= verb "text")
           (values (if (started-p) (command-text (rest words)) (status))
                   (and (started-p) (status-dialog))))
          ((string= verb "call")
           (values (if (started-p) (command-call (rest words)) (status))))
          (t (values "Usage: /callme [status | text WHO MESSAGE | call WHO TEXT]"
                      (and (started-p) (status-dialog)))))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell call-me
  (:section ("call-me")
    (:guide "install the /call-me app on the iPhone and copy the 10-digit number it shows; put that number in allowed_users; next step: /callme status")
    ("allowed_users" :list
     :doc "the 10-digit /call-me numbers the agent may text and ring; nobody else is reachable")
    ("names" :string :default ""
     :doc "number=name pairs, e.g. 5551234567=Sam,5559876543=Alex: the model sees the name and may use it in place of the number")
    ("label" :string :default "Nodecode"
     :doc "the thread name the phone shows for this agent")
    ("timeout" :integer :default 30 :min 1
     :doc "how long one request to the /call-me service may take, in seconds")
    ("call_wait" :integer :default 120 :min 30
     :doc "how long a call waits for the person to answer and finish, in seconds; 300 at most"))
  (:settings #'read-settings)
  (:help :call-me
         "call-me: callme_call rings the operator's phone and returns the spoken answer"
         *manual*)
  (:tool "callme_text" (nlk:contract 'text-message) #'text-message)
  (:tool "callme_call" (nlk:contract 'spoken-call) #'make-call)
  (:command "callme" 'run-command
            :description "/call-me: what the phone number reaches, or a text or call now"
            :argument-hint "status | text WHO MESSAGE | call WHO TEXT"))
