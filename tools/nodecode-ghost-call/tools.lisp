;;;; tools.lisp --- the seven tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original exposed seven tools to Hermes; six of the names and all of
;;;; their jobs are kept. What changed is where the lines come from: the
;;;; original's bridge transcribed both legs of a real call and pushed hints
;;;; by itself, so its tools only read state. Here the model (or the operator)
;;;; types what was said with ghost_line, and the same gate, the same prompt
;;;; and the same rate limits decide whether that line earns a hint.

(in-package #:nodecode-ghost-call)

;;; --- what the call is ------------------------------------------------------------

(nlk:define-contract ghost-start-call
  (:doc "Open a Ghost Call: coach the operator, silently, through one phone call of their own.
Hints go to the operator only; nothing is said to the other party. Answers the session id and
what to do next. One call is open at a time.")
  ("goal" :text :required t
   :doc "what the operator wants out of the call, in their words, with the facts: \"book a
table Friday 8pm for 4, budget $80, name on the booking\"")
  ("callee_label" :string :doc "who is on the other side, e.g. the restaurant's name")
  ("target" :string
   :doc "the operator's private target or walk-away figure: the hint model is told it is
private, so no hint ever suggests saying that number")
  ("user_name" :string :doc "the operator's name, used only in the intervention disclosure"))

(nlk:define-contract ghost-line-call
  (:doc "Hand the coach one line that was just said on the call. Answers the next hint, or
SILENT and why: most lines (fillers, greetings, repeats, the operator's own words, a line
inside the interval) never reach the model. Feed the other party's lines as they speak.")
  ("text" :text :required t :doc "exactly what was just said, as heard")
  ("speaker" :string :options '("them" "you" "call")
   :doc "them (the other party, the default), you (the operator), or call (one microphone
next to a phone on speaker, the speaker unknown)"))

(nlk:define-contract ghost-intervene-call
  (:doc "Say one short phrase to the OTHER party, once the operator has allowed it with
/ghost allow. Answers the exact words to speak, with the AI disclosure in front of them.")
  ("text" :text :required t :doc "what to say, at most 240 characters")
  ("session_id" :string :doc "accepted for the original's shape; one call is open at a time"))

(nlk:define-contract ghost-hints-call
  (:doc "The hints already given on this call, newest first, for a question asked afterwards.
Do not poll it during the call: ghost_line answers as the call goes.")
  ("n" :integer :doc "how many of the latest hints, at most 40; five when the call names none")
  ("session_id" :string :doc "accepted for the original's shape; one call is open at a time"))

(nlk:define-contract ghost-side-call
  (:doc "One of the call's side verbs: its state, its summary, or its end.")
  ("session_id" :string :doc "accepted for the original's shape; one call is open at a time"))

;;; --- the verbs the tools and /ghost share --------------------------------------------

(defun start-call (goal &key callee-label target user-name)
  "Open a call and answer what the operator and the model need to know."
  (let ((session (open-session :goal goal :callee-label callee-label
                                :target target :user-name user-name)))
    (format nil "ghost ~a is listening: ~a~@[ (for ~a)~]~%~a~%\
Feed each line the other party says with ghost_line, speaker \"them\". ~
Hints are for the operator's eyes (or their earpiece) only; the other party never hears them. ~
Nothing is said to the other party unless the operator types /ghost allow, and then ghost_intervene. ~
/ghost hint <what they said> does one line from the keyboard; ghost_hangup ends the call and ~
answers the summary. Duration cap ~d s, at most ~d hints, one every ~d s."
            (ghost-session-id session)
            (ghost-session-goal session)
            (let ((label (ghost-session-callee-label session)))
              (and (plusp (length label)) label))
            (if (plusp (length (ghost-session-target session)))
                (format nil "Private target kept out of every hint: ~a."
                        (ghost-session-target session))
                "No private target was given; use ghost_start's target to keep one out of the hints.")
            (setting :max-duration-s) (setting :max-hints) (interval-s))))

(defun feed-line (speaker text)
  "One line of the call in; the next hint, or SILENT and why, out."
  (let* ((session (active-session))
         (speaker (string-downcase (nlk:trimmed (or speaker "them"))))
         (text (nlk:trimmed (or text ""))))
    (when (ended-p session)
      (fail "this call has ended; ghost_summary reads it, ghost_start opens the next one"))
    (record-turn session speaker text)
    (multiple-value-bind (worth-while reason)
        (gate speaker text :recent (rest (ghost-session-turns session)))
      (cond
        ((not worth-while)
         (format nil "SILENT (~a) · ~a" reason (status-line session)))
        ((zerop (hints-left session))
         (format nil "SILENT (the cap of ~d hints is spent) · ~a"
                 (min +max-hints-ceiling+ (setting :max-hints)) (status-line session)))
        ((plusp (wait-s session))
         (format nil "SILENT (~d s until the next hint may go out) · ~a"
                 (ceiling (wait-s session)) (status-line session)))
        (t (let* ((raw (ghost-chat (hint-messages session) :max-tokens 60
                                    :timeout +hint-timeout-s+))
                  (line (clean-hint raw (hint-lines session))))
             (if (null line)
                 (format nil "SILENT (nothing new worth saying) · ~a" (status-line session))
                 (let ((session (record-hint session line)))
                   (format nil "HINT #~d · ~a · ~a~%~a~%\
Operator's eyes only: this is never spoken to the other party."
                           (ghost-session-hint-count session)
                           (stamp (elapsed session))
                           (status-line session)
                           line)))))))))

(defun hint-messages (session)
  "The messages one hint is built from: the goal, the private target, the
earlier hints and the last lines of the call."
  (let ((prev (hint-lines session))
        (lines (last (transcript-turns session) (min +context-turns+
                                                     (length (transcript-turns session))))))
    (list (message "system" +hint-system-prompt+)
          (message "user"
                   (with-output-to-string (out)
                     (format out "USER GOAL: ~a~%" (ghost-session-goal session))
                     (let ((label (ghost-session-callee-label session))
                           (target (ghost-session-target session)))
                       (when (plusp (length label))
                         (format out "OTHER PARTY: ~a~%" label))
                       (when (plusp (length target))
                         (format out "PRIVATE TARGET (never say it, never suggest that \
number): ~a~%" target)))
                     (format out "HINTS ALREADY GIVEN:~%~{  - ~a~%~}"
                             (or (subseq prev 0 (min 8 (length prev))) (list "(none)")))
                     (format out "TRANSCRIPT (latest last):~%~:{  ~a: ~a~%~}"
                             (mapcar (lambda (turn)
                                       (list (if (member (getf turn :speaker) +other-speakers+
                                                          :test #'equal)
                                                 "them" "you")
                                             (getf turn :text)))
                                     lines))
                     (format out "Write the hint line or SILENT."))))))

(defun intervene-line (text)
  "What to say to the other party, or a refusal naming the one thing to type."
  (let* ((session (active-session))
         (text (nlk:trimmed (or text ""))))
    (when (ended-p session)
      (fail "this call has ended; intervene is for a live call"))
    (unless (ghost-session-intervene-allowed session)
      (fail "intervene is off for ~a: the operator turns it on by typing /ghost allow"
            (ghost-session-id session)))
    (when (zerop (length text))
      (fail "text is required: what to say to the other party"))
    (let ((spoken (if (ghost-session-disclosed session)
                      (subseq text 0 (min 240 (length text)))
                      (format nil "This is an AI assistant speaking on behalf of ~a. ~a"
                              (let ((name (ghost-session-user-name session)))
                                (if (plusp (length name)) name "the caller"))
                              (subseq text 0 (min 240 (length text)))))))
      (setf (ghost-session-disclosed session) t)
      (format nil "Say this to the other party:~%~a~%~%\
The disclosure rides the first line only; a later intervention is the words alone."
              spoken))))

(defun end-call ()
  "End the call and answer its one summary."
  (let ((session (or (current-session) *session*)))
    (unless session
      (fail "no ghost call to end: ghost_start opens one"))
    (end-session session "the operator hung up")
    (summary-answer session)))

;;; --- the tool bodies -------------------------------------------------------------

(defun tool-start (call)
  "ghost_start"
  (start-call (ghost-start-call-goal call)
              :callee-label (ghost-start-call-callee-label call)
              :target (ghost-start-call-target call)
              :user-name (ghost-start-call-user-name call)))

(defun tool-line (call)
  "ghost_line"
  (feed-line (ghost-line-call-speaker call) (ghost-line-call-text call)))

(defun tool-status (call)
  "ghost_status"
  (declare (ignore call))
  (let ((session *session*))
    (if (null session)
        "no ghost call yet: ghost_start opens one"
        (status-text session))))

(defun tool-hints (call)
  "ghost_hint_latest"
  (let ((session (active-session))
        (wanted (or (ghost-hints-call-n call) 5)))
    (let ((hints (subseq (ghost-session-hints session) 0
                          (min (length (ghost-session-hints session)) (max 1 (min 40 wanted))))))
      (if (null hints)
          (format nil "no hint yet on ~a · ~a" (ghost-session-id session)
                  (status-line session))
          (with-output-to-string (out)
            (format out "~d hint~:p on ~a, newest first:~%"
                    (length hints) (ghost-session-id session))
            (dolist (hint hints)
              (format out "  #~d ~a · ~a~%" (getf hint :n) (stamp (getf hint :at))
                      (getf hint :text))))))))

(defun tool-intervene (call)
  "ghost_intervene"
  (intervene-line (ghost-intervene-call-text call)))

(defun tool-hangup (call)
  "ghost_hangup"
  (declare (ignore call))
  (end-call))

(defun tool-summary (call)
  "ghost_summary"
  (declare (ignore call))
  (let ((session (or (current-session) *session*)))
    (unless session
      (fail "no ghost call yet: ghost_start opens one, ghost_hangup ends it"))
    (summary-answer session)))
