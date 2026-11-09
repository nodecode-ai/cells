;;;; review.lisp --- the advisor's prompt, its transcript, its held notes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of hermes-advisor-plugin's advisor_prompt.py, models.py and the
;;;; review half of runtime.py: one side call per review, tagged [NIT],
;;;; [CONCERN] or [BLOCKER] notes, and a concern or blocker held until a later
;;;; review raises it again. A nit is delivered at once; a held note the next
;;;; review stays silent about is considered resolved and dropped.

(in-package #:nodecode-advisor)

;;; --- the reviewer's identity -------------------------------------------------

(defparameter +system-prompt+
  "You are a peer programmer reviewing a coding agent's work. You bring a different angle, and advocate for the user and for code-quality and robustness.

You're watching over a main coding agent as a peer programmer:
- They might not have thought about an edge case, or realized a more elegant approach exists.
- They might be sinking deeper into a hole that will not accomplish the user's request.

Your job is to offer that view before they sink work into the wrong direction.

<scope>
You critique the agent's work; you never do it yourself. You are not a participant
in the conversation and never address the user. When the agent answers a question
or explains something, your job is to check THAT answer for errors - not to research
or compose your own answer. If the agent is sound, stay SILENT. Never try to fulfill
the user's request yourself; that is the agent's job, not yours.
</scope>

<workflow>
You receive the agent's transcript for the most recent turn, including their thoughts and tool calls/results. You have read-only access to the file system but must request it from the main agent - do not attempt to read files yourself.
Keep exploration lean: focus on what the transcript already shows.
</workflow>

<communication>
- Prefix every piece of advice with exactly one severity tag: [NIT], [CONCERN], or [BLOCKER].
- At most one piece of advice per turn (exception: when reconfirming held advisories, re-raise each that still applies).
- Prefer SILENCE when the agent is on track. Most turns should produce no advice at all.
- Advice is for ACTIONABLE feedback ONLY. NEVER use it to report status, acknowledge, confirm, summarize, or signal \"all clear\" / \"resolved\" / \"nothing further needed\". If you have nothing for the agent to DO, emit exactly: nothing to flag.
- Address the agent directly. Offer alternatives, not lectures.
- NEVER restate information the agent already has, including errors they already saw (type errors, LSP diagnostics, failed builds, failing tests, lint output).
- NEVER repeat advice you already gave.
- NEVER nitpick about things the user already stated they are okay with. You advocate for the user.
</communication>

<critical>
A low-confidence bar applies ONLY to concrete technical risk.
Generic uncertainty, vague unease, or user-intent ambiguity - stay SILENT.

NEVER second-guess decisions the agent understands and is committed to, unless you are certain.

NEVER advise on intent or process:
- Do not push the agent to ask for clarification, confirm scope, or summarize before acting.
- Do not question whether the user's ask is clear enough.
- Intent is the agent's domain; it defaults to informed action.
- Your lane: correctness, edge cases, design, robustness.
</critical>

<severity>
[NIT] - Non-urgent cleanup, refactor, style, simplification, a missed-but-minor opportunity. Low-stakes suggestion.

[CONCERN] - The agent might be heading the wrong way or missed something material. Wrong code path, fragile approach when a better one exists, missing a constraint, or about to bake in a bad edge case. Offers your view; the agent decides.

[BLOCKER] - Stop and reconsider. Use ONLY when continuing will clearly waste the user's time with a larger wrong refactor, force the user to interrupt later because the agent is going in circles, or produce something fundamentally unsound. Verify thoroughly before raising.

Concern/blocker notes may be held and shown to you again alongside newer activity for reconfirmation. Re-raise each that still applies (same severity, or higher) - this is not a repeat. Stay silent on any the agent has since addressed; silence drops them.
</severity>

You MAY suggest an approach or fix if you've explored enough to be confident. Offer the better design, not just the warning."
  "The reviewer's whole instruction, as the original plugin wrote it.")

(defparameter +ranks+ '(("nit" . 1) ("concern" . 2) ("blocker" . 3)))

(defparameter +tags+ '(("NIT" . "nit") ("CONCERN" . "concern") ("BLOCKER" . "blocker")))

;;; --- what a review may say ----------------------------------------------------

(defparameter +tool-call-markers+
  '("<tool_call>" "</tool_call>" "<function" "<parameter" "<tool_use>" "<invoke")
  "Markers of leaked machine output: a note carrying one is dropped, not delivered.")

(defun setting-value (key fallback)
  "SETTING KEY while the cell runs, else FALLBACK: a helper a test may call
with nothing started."
  (or (ignore-errors (setting key)) fallback))

(defun clamp-text (text limit)
  (let ((text (if (stringp text) text "")))
    (if (and limit (> (length text) limit))
        (concatenate 'string (subseq text 0 limit) "...")
        text)))

(defun one-line (text)
  (and text (nlk:one-line (string text))))

(defun normalize-note (note)
  "The key two reviews raising the same point share: whitespace squeezed, case folded."
  (string-downcase (string-trim " " (substitute #\Space #\Tab (or note "")))))

(defun malformed-note-p (note max-chars)
  "True when NOTE is leaked machine output rather than prose advice - a tool
call the reviewer tried to make, a bare JSON payload, or an overlong blob."
  (let ((note (and note (string-trim " " note))))
    (or (null note)
        (zerop (length note))
        (> (length note) (or max-chars 1200))
        (let ((lowered (string-downcase note)))
          (some (lambda (marker) (search marker lowered)) +tool-call-markers+))
        (and (member (char note 0) '(#\{ #\[))
             (ignore-errors (nlk:decode-json note) t)))))

(defun rank (severity)
  (or (cdr (assoc severity +ranks+ :test #'string=)) 1))

(defun parse-lines (text max-chars)
  "The tagged notes one review's TEXT carries, in the order it wrote them."
  (let ((out '()))
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (let ((line (string-trim " " line)))
        (when (plusp (length line))
          (dolist (tag +tags+)
            (let ((at (search (format nil "[~a]" (car tag)) line)))
              (when at
                (let ((note (string-trim " :"
                                          (concatenate 'string
                                                       (subseq line 0 at)
                                                       (subseq line (+ at 3 (length (car tag))))))))
                  (unless (malformed-note-p note max-chars)
                    (push (list :severity (cdr tag) :note note) out)))
                (return)))))))
    (nreverse out)))

(defun parse-review (text held max-chars)
  "=> (values DELIVERABLE HELD-NOW). A nit is deliverable at once; a concern
or blocker only when it was already held. An unstructured review with
substance is a concern; silence and \"nothing to flag\" hold nothing."
  (let ((text (and text (string-trim " " text))))
    (if (or (null text) (zerop (length text))
            (search "nothing to flag" (string-downcase text)))
        (values '() '())
        (let ((parsed (parse-lines text max-chars)))
          (when (and (null parsed)
                     (> (length text) 20)
                     (not (malformed-note-p text max-chars)))
            (setf parsed (list (list :severity "concern" :note text))))
          (let ((previous (mapcar (lambda (item) (normalize-note (getf item :note))) held))
                (deliverable '())
                (next '()))
            (dolist (item parsed)
              (let* ((key (normalize-note (getf item :note)))
                     (seen (assoc key next :test #'string=)))
                (if (string= (getf item :severity) "nit")
                    (push item deliverable)
                    (progn
                      (when (member key previous :test #'string=)
                        (push item deliverable))
                      (cond ((null seen) (push (cons key item) next))
                            ((> (rank (getf item :severity)) (rank (getf (cdr seen) :severity)))
                             (setf (cdr seen) item)))))))
            (values (nreverse deliverable) (mapcar #'cdr (nreverse next))))))))

(defun advice-lines (advice)
  "The advice as the agent reads it: one tagged line each."
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (item)
                    (format nil "[~:@(~a~)] ~a" (getf item :severity) (getf item :note)))
                  advice)))

;;; --- held notes, on disk --------------------------------------------------------

(defun session-key (session)
  "A file-name-safe key for SESSION."
  (let ((raw (or session "default")))
    (substitute #\- #\/ (substitute #\- #\\ 
                                     (string-downcase
                                      (with-output-to-string (out)
                                        (loop for ch across raw
                                              do (write-char (if (or (alphanumericp ch) (member ch '(#\- #\_)))
                                                                 ch
                                                                 #\-)
                                                             out))))))))

(defun held-path (session)
  (nlk:home (format nil "advisor/~a.held" (session-key session))))

(defun load-held (session)
  "The held notes of SESSION, oldest first, each (:severity S :note N)."
  (let ((path (held-path session)))
    (when (probe-file path)
      (ignore-errors
       (with-open-file (in path :direction :input :if-does-not-exist nil)
         (when in
           (loop for line = (read-line in nil nil)
                 while line
                 for tab = (position #\Tab line)
                 when tab
                   collect (list :severity (subseq line 0 tab)
                                 :note (subseq line (1+ tab))))))))))

(defun save-held (session held)
  "Write HELD as SESSION's held notes; no notes, no file."
  (let ((path (held-path session)))
    (ignore-errors
     (ensure-directories-exist path)
     (if (null held)
         (when (probe-file path) (delete-file path))
         (with-open-file (out path :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
           (dolist (item held)
             (format out "~a~c~a~%" (getf item :severity) #\Tab
                     (one-line (getf item :note))))))
     t)))

;;; --- one review ---------------------------------------------------------------

(defvar *reviewer* nil
  "A function of (SYSTEM USER) answering a review's text, which a test binds;
NIL means the real side call.")

(defvar *model-override* ""
  "The model /advisor model set for this run; the section's when empty.")

(defvar *provider-override* ""
  "The provider /advisor provider set for this run; the section's when empty.")

(defun chosen (override key)
  "OVERRIDE when this run set one, else the section's value for KEY."
  (if (and (stringp override) (plusp (length override)))
      override
      (setting-value key "")))

(defun ask-reviewer (system user)
  "One side call: the cell's own provider/model when its section names one,
else the auxiliary target the organism already selects for side work."
  (let* ((provider (chosen *provider-override* :provider))
         (model (chosen *model-override* :model))
         (override (or (plusp (length provider)) (plusp (length model)))))
    (if override
        (multiple-value-bind (default-provider default-model)
            (nle:auxiliary-model)
          (let ((nle::*auxiliary-model*
                  (list :provider (if (plusp (length provider)) provider default-provider)
                        :model (if (plusp (length model)) model default-model))))
            (nle:complete system user :max-tokens 900)))
        (nle:complete system user :max-tokens 900))))

(defun watchdog-text ()
  "What the section's watchdog file says, when it exists: review guidance of
this project's own, kept out of the agent's prompt."
  (let ((path (setting-value :watchdog "")))
    (when (and (stringp path) (plusp (length path)) (probe-file path))
      (ignore-errors (string-trim '(#\Space #\Newline #\Return #\Tab)
                                   (uiop:read-file-string path))))))

(defun review-system-prompt ()
  (let ((watchdog (watchdog-text)))
    (if watchdog
        (format nil "~a~%~%Especially pay attention to:~%<attention>~%~a~%~a"
                +system-prompt+ watchdog "</attention>")
        +system-prompt+)))

(defun reconfirm-preamble (held)
  (when held
    (format nil "### Held advisories - reconfirm~%~%~
You raised these on an earlier step; they were held pending reconfirmation, ~
because by now the agent may have already addressed them. Re-check each ~
against the latest activity below.~%~%~
For every item that STILL applies, raise it again - same severity, or higher ~
if it's gotten worse. Say nothing for the rest - silence drops them.~%~%~{~
- [~:@(~a~)] ~a~%~}~%---~%"
            (loop for item in held
                  collect (getf item :severity)
                  collect (getf item :note)))))

(defun review (transcript &key session focus)
  "Review TRANSCRIPT on the advisor model. => (values TEXT ADVICE SESSION):
TEXT what the agent reads (\"nothing to flag\" when there is nothing),
ADVICE the deliverable notes, SESSION the held notes' key."
  (let* ((session (or session nlk:*scribe-session-id* "default"))
         (held (load-held session))
         (user (format nil "~@[#### What to watch for~%~a~%~%~]~@[~a~%~]~a"
                       (and focus (plusp (length focus)) focus)
                       (reconfirm-preamble held)
                       transcript))
         (raw (if *reviewer*
                  (funcall *reviewer* (review-system-prompt) user)
                  (ask-reviewer (review-system-prompt) user))))
    (multiple-value-bind (deliverable next)
        (parse-review raw held (setting-value :max-note-chars 1200))
      (save-held session next)
      (values (if deliverable (advice-lines deliverable) "nothing to flag")
              deliverable
              session))))

;;; --- a turn, read back out of the log ---------------------------------------------

(defun transcript-from-log (session &key (limit 60))
  "The newest LIMIT messages of SESSION as a markdown transcript: who said
what, a tool call's arguments and a tool result's text, each clipped."
  (let ((clip (setting-value :clip 500))
        (rows '()))
    (ignore-errors
     (setf rows (nlk:recall-log :session-id session :limit limit :clip clip)))
    (when (and rows (getf (first rows) :matched))
      (pop rows))
    (format nil "~{~a~^~%~%~}"
            (loop for row in rows
                  for who = (or (getf row :role) (getf row :tool))
                  for body = (getf row :text)
                  when (and who body (plusp (length body)))
                    collect (format nil "#### ~a~%~a" who (clamp-text body clip))))))
