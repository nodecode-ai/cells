;;;; cell.lisp --- Enhance Prompt: one rewrite, as a tool and as a command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin has two halves over one prompt: a sparkle beside Send
;;;; that rewrites the composer draft in place (desktop/plugin.js), and an
;;;; agent-side `enhance_prompt' tool plus `/enhance' command (__init__.py).
;;;; A cell has no sparkle to draw, so the rewrite is one verb here, reached
;;;; three ways: the tool, the command, and eval - and the command writes its
;;;; answer back into the composer as a draft effect, which is what the sparkle
;;;; did. Nothing is ever sent: the command answers text and an effect, and a
;;;; shell puts the text in its input box.
;;;;
;;;; The model call is one side call, NLK:COMPLETE: SYSTEM the whole
;;;; instruction, USER the one message, no tools, no history. It runs on the
;;;; auxiliary model (general.auxiliary_model) or, with :session-id, on that
;;;; session's own - the original's "the chat's own model". Each rewrite is
;;;; one call; the original's second call, which scored the result, is left
;;;; out (see NOTICE).
;;;;
;;;; Config, a sibling top-level key:
;;;;   "enhance-prompt": {"default_stage": "1", "max_tokens": 16384}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-enhance-prompt)

;;; --- the manual -------------------------------------------------------------

(defparameter *manual*
  "Enhance Prompt rewrites a draft into a structured agent brief - one goal line and
numbered requirements - without answering it and without sending anything.
  enhance_prompt  {input, stage}
      input is the draft to rewrite: at least 8 characters, in any language (the answer
      keeps the draft's language). stage 1 (default) clarifies it, 2 breaks each
      requirement into the action, the inputs and the deliverable, 3 reframes the task
      as phases with checkpoints. The answer is the rewritten brief; hand it to the
      person as the prompt to run, or call it again at the next stage to deepen it.
      Nothing is sent and no session is started: the call spends one model turn on the
      auxiliary model and answers text.
A person reaches the same rewrite with /enhance: type the rough draft in the composer
(or give it as the argument), run /enhance [1|2|3], and the rewritten brief is put back
in the composer as the draft, ready to send. Running /enhance again on that brief
deepens it one stage; /enhance undo puts the draft the first rewrite started from back."
  "What (help :enhance-prompt) answers while the cell runs.")

;;; --- one rewrite ------------------------------------------------------------

(defvar *complete* 'nle:complete
  "The one side call a rewrite runs, as a function designator: NLK:COMPLETE -
SYSTEM the whole instruction, USER the one message, no tools, no history. A
symbol rather than a function so a test's stub of the fdefinition is the call
this cell makes.")

(defun rewrite (draft &key (stage 1) max-tokens session-id)
  "DRAFT rewritten as a structured agent brief at STAGE, the model's answer
with the wrappers taken off. Signals ENHANCE-PROMPT-ERROR when the model
answers nothing."
  (let ((raw (funcall *complete*
                      (system-prompt stage)
                      (user-message draft)
                      :max-tokens max-tokens
                      :session-id session-id)))
    (let ((text (strip-wrappers raw)))
      (when (string= "" text)
        (fail "the model answered nothing (~d characters, all wrapper)" (length (or raw ""))))
      text)))

;;; --- the chain of drafts one chat is on ---------------------------------------
;;; A sparkle clicked again deepens the brief it produced last, and one
;;; discard puts back the draft the chain started from. A session's chain is
;;; the same three facts, kept by session id.

(defstruct (chain (:constructor make-chain ()))
  (backup "" :type string)
  (output "" :type string)
  (stage 0 :type integer))

(defvar *chains* (make-hash-table :test #'equal)
  "SESSION-ID -> the CHAIN of drafts its /enhance runs are on. Cleared at
start and stop: a chain is a click's memory, not a durable fact.")

(defun chain-for (session-id)
  "SESSION-ID's chain, made on first use. Call under the cell's lock."
  (or (gethash (or session-id "") *chains*)
      (setf (gethash (or session-id "") *chains*) (make-chain))))

(defun next-stage (session-id source explicit)
  "STAGE and BACKUP a run on SOURCE gets: EXPLICIT, when given, wins; a run on
the brief the last one answered deepens one stage and keeps that chain's
backup; any other draft starts a new chain at the default stage."
  (with-enhance-prompt-lock
    (let* ((chain (chain-for session-id))
           (deepen (and (null explicit) (plusp (chain-stage chain))
                        (string= source (chain-output chain))))
           (default (default-stage)))
      (values (cond (explicit explicit)
                    (deepen (min 3 (1+ (chain-stage chain))))
                    (t default))
              (if deepen (chain-backup chain) source)))))

(defun note-output (session-id text stage backup)
  "Remember TEXT as the brief SESSION-ID's chain is on at STAGE, with BACKUP
the draft it started from."
  (with-enhance-prompt-lock
    (let ((chain (chain-for session-id)))
      (setf (chain-backup chain) backup
            (chain-output chain) text
            (chain-stage chain) stage)
      text)))

(defun default-stage ()
  "The stage a fresh draft starts at, from the section (1 unless set)."
  (or (ignore-errors (parse-integer (string (setting :default-stage)) :junk-allowed t)) 1))

(defun max-tokens ()
  "One rewrite's completion ceiling, from the section."
  (ignore-errors (setting :max-tokens)))

;;; --- the command -------------------------------------------------------------

(defparameter +undo-words+ '("undo" "revert" "discard")
  "What /enhance takes instead of a draft, for the one-click discard the
sparkle's second button was.")

(defparameter +stages+ '("1" "2" "3")
  "The three stages: what the section offers, and what /enhance takes.")

(defparameter +stage-words+ +stages+)

(defun trim (text)
  (string-trim +space-chars+ (or text "")))

(defun stage-word (text)
  "TEXT as a stage number, or NIL when it is not one."
  (let ((word (string-downcase (trim text))))
    (when (member word +stage-words+ :test #'string=)
      (parse-integer word))))

(defun split-stage (args)
  "ARGS as (values STAGE REST): a leading 1, 2 or 3 is the stage the rest of
the argument asks to start at."
  (let* ((trimmed (trim args))
         (space (position #\Space trimmed)))
    (if (stage-word (if space (subseq trimmed 0 space) trimmed))
        (values (stage-word (if space (subseq trimmed 0 space) trimmed))
                (trim (if space (subseq trimmed (1+ space)) "")))
        (values nil trimmed))))

(defun usage-line ()
  (format nil "Usage: type the draft in the composer and run /enhance [1|2|3], ~
               or /enhance [STAGE] <draft of 8+ characters>. /enhance undo puts ~
               back the draft a rewrite started from."))

(defun undo-line (session-id)
  "Put back the draft SESSION-ID's chain started from, as a draft effect."
  (with-enhance-prompt-lock
    (let ((chain (gethash (or session-id "") *chains*)))
      (cond
        ((or (null chain) (string= "" (chain-backup chain)))
         "/enhance has not rewritten this chat's draft: nothing to undo")
        (t
         (let ((backup (chain-backup chain)))
           (setf (chain-backup chain) "" (chain-output chain) "" (chain-stage chain) 0)
           (values (format nil "Restored the draft /enhance started from (~d characters)."
                           (length backup))
                   nil
                   (list (list :draft backup :how :set)))))))))

(defun enhance-and-set (source stage session-id)
  "Rewrite SOURCE at STAGE, and answer its text with the draft effect that
puts it in the composer - never a send."
  (multiple-value-bind (stage backup) (next-stage session-id source stage)
    (handler-case
        (let ((text (rewrite source :stage stage :max-tokens (max-tokens))))
          (note-output session-id text stage backup)
          (values text nil (list (list :draft text :how :set))))
      (enhance-prompt-error (condition)
        (format nil "Enhance failed: ~a" condition))
      (error (condition)
        (format nil "Enhance failed: ~a" condition)))))

(defun run-slash (args session-id &key draft)
  "ARGS and the composer's DRAFT, as /enhance reads them; answers the text a
shell shows, and the effect that sets its composer."
  (let* ((draft (trim draft))
         (args (trim args))
         (word (string-downcase args)))
    (multiple-value-bind (explicit body) (split-stage args)
      (cond
        ((member word +undo-words+ :test #'string=) (undo-line session-id))
        ((>= (length draft) +min-draft-chars+) (enhance-and-set draft explicit session-id))
        ((>= (length body) +min-draft-chars+) (enhance-and-set body explicit session-id))
        (t (usage-line))))))

;;; --- the tool ----------------------------------------------------------------

(nlk:define-contract enhance-request
  (:doc "Rewrite a draft into a clearer, more specific prompt: one goal line
followed by numbered requirements. Matches the draft's own language. Does not
answer the request, does not send anything, and does not run any of the work it
describes - it only reformulates. Use it when a person's request is rough,
ambiguous or too thin to execute on the first attempt; hand the rewritten
brief back to them (or run it) instead of the original draft.")
  ("input" :text :required t
   :doc "the draft to rewrite: the person's own words, at least 8 characters, in any language")
  ("stage" :integer :default 1
   :doc "how deep the rewrite goes: 1 clarifies the draft into a short brief (default), 2 breaks each requirement into the action, the inputs it needs and the deliverable, 3 reframes it as a project with phases, dependencies and checkpoints"))

(defun enhance-tool (request)
  "The enhance_prompt tool's body: REQUEST's draft, rewritten."
  (let* ((draft (enhance-request-input request))
         (stage (max 1 (min 3 (or (enhance-request-stage request) 1)))))
    (cond
      ((< (length (trim draft)) +min-draft-chars+)
       (format nil "enhance_prompt: the draft is under ~d characters - there is nothing ~
                    to rewrite yet; give me the person's words as they are." +min-draft-chars+))
      (t
       (handler-case
           (rewrite draft :stage stage :max-tokens (max-tokens)
                           :session-id (and (boundp 'nlk:*scribe-session-id*)
                                            nlk:*scribe-session-id*))
         (enhance-prompt-error (condition)
           (format nil "enhance_prompt failed: ~a" condition))
         (error (condition)
           (format nil "enhance_prompt failed: ~a" condition)))))))

;;; --- the cell ---------------------------------------------------------------

(nle:define-cell enhance-prompt
  (:section ("enhance-prompt")
    (:guide "nothing to fetch or paste: /enhance and enhance_prompt run on the session's own model; default_stage starts a fresh draft deeper, max_tokens caps one rewrite; next step: type a rough draft and run /enhance")
    ("default_stage" :choice :options +stages+ :default "1"
     :doc "the stage a fresh draft starts at: 1 clarifies, 2 details each requirement, 3 phases the work")
    ("max_tokens" :integer :default 16384 :min 256
     :doc "one rewrite's completion ceiling; a brief is long, so this is generous where the model allows it"))
  (:help :enhance-prompt
         "enhance-prompt: enhance_prompt and /enhance rewrite a draft into a numbered agent brief, in place, without sending it"
         *manual*)
  (:tool "enhance_prompt" (nlk:contract 'enhance-request) #'enhance-tool)
  (:start #'install))

;;; --- what the cell installs ---------------------------------------------------
;;;
;;; The command is registered here rather than by a (:command ...) clause: a
;;; shell that can hand a command the composer's draft says so by accepting
;;; :reads, which an older Nodecode's REGISTER-COMMAND does not. A rewrite in
;;; place is what the original sparkle was, so it is asked for where it can be
;;; had, and /enhance <draft> is what everywhere else runs.

(defparameter +command-owner+ "nodecode-enhance-prompt"
  "The key /enhance is filed under: the folder's own name, as a declaration's
(:command ...) clause would file it.")

(defparameter +command-description+
  "Rewrite a draft as a numbered agent brief, without sending it"
  "What the catalog says /enhance is for.")

(defun install-command ()
  "Put /enhance in the one slash catalog, reading the composer's draft where
the runtime offers it and taking a draft as its argument everywhere."
  (handler-case
      (nle:register-command +command-owner+ "enhance" #'run-slash
                             :description +command-description+
                             :argument-hint "[1|2|3|undo]"
                             :reads '(:draft))
    (error ()
      (nle:register-command +command-owner+ "enhance" #'run-slash
                             :description +command-description+
                             :argument-hint "[1|2|3|undo]"))))

(defun install ()
  "START-CELL's own start: the command in the catalog and the chain table
empty, and both taken back when the cell stops."
  (install-command)
  (nle:on-stop (lambda () (nle:unregister-commands +command-owner+)))
  (with-enhance-prompt-lock (clrhash *chains*))
  (nle:on-stop (lambda () (with-enhance-prompt-lock (clrhash *chains*)))))

;;; --- what eval reaches -------------------------------------------------------

(define-verb enhance (draft &key (stage 1))
  "DRAFT rewritten as a structured agent brief at STAGE 1-3, the way
/enhance and the enhance_prompt tool do it: one side call, nothing sent.
(enhance (a draft string)) is the plain rewrite; stage 2 and 3 deepen it."
  (rewrite draft :stage (max 1 (min 3 stage)) :max-tokens (max-tokens)))

