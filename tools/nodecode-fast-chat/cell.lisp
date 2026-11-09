;;;; cell.lisp --- one fresh session on the model you name.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the original gives a user: a new frontend session on an explicit
;;;; model and provider, a speed tier above the default, and the profile's own
;;;; defaults left alone. The thing a Nodecode session can name is its
;;;; provider, model and reasoning rung, and the fact that records that pick
;;;; per session is SESSION.MODEL_SELECTED -- what /models writes through
;;;; NLK:RECORD-SESSION-MODEL-SELECTION and what NLK:GATEWAY-TARGET reads
;;;; before a turn. So /fast-chat opens a session, records the named target
;;;; on it, and moves nothing else: not the gateway's default, not the session
;;;; the command was typed in. With --prompt the new session starts working
;;;; at once; without it, it waits for its first turn and its id is answered
;;;; for /sessions.
;;;;
;;;; The original's Fast/ultrafast service tier is a Hermes-provider billing
;;;; feature; Nodecode has no service tier, so the nearest setting it does
;;;; have -- the model's reasoning rung -- is what the cell names instead.

(in-package #:nodecode-fast-chat)

(nlk:define-peripheral fast-chat :not-running t)

(defparameter +efforts+ '("off" "minimal" "low" "medium" "high" "xhigh" "max")
  "The reasoning rungs this organism's models ladder through, cheapest first.
A rung a model does not offer is refused by the session's first turn, not
here, so the list is what a line may say, not what any one model admits.")

(defparameter *usage*
  "usage: /fast-chat [PROVIDER/]MODEL[:EFFORT] [--provider P] [--effort RUNG] [--in DIR] [--prompt TEXT]"
  "The one line a malformed command answers with.")

(defparameter *manual*
  (format nil "fast-chat: one fresh session on a model you name, without moving the default.~%
/fast-chat [PROVIDER/]MODEL[:EFFORT] [--provider P] [--effort RUNG] [--in DIR] [--prompt TEXT]~%
MODEL is a model id as /models lists it, behind PROVIDER/ for another provider and before :EFFORT for a rung; --provider and --effort say either part outright, and the flags win. A bare /fast-chat takes the model, provider and effort of the fast-chat section. Without --in the new session works in this one's directory. Without --prompt it waits, and its id is answered so /sessions can open it; with one it starts its first turn at once. Only that session's own pick is recorded: the gateway's default model and every other session stay on what they run on.~%The rungs are ~{~a~^, ~}." ; ~
  (list "off" "minimal" "low" "medium" "high" "xhigh" "max"))
  "What (help :fast-chat) answers, and the one line every request carries.")

;;; --- the argument line -----------------------------------------------------

(defun split-words (text)
  "TEXT split on whitespace, empty words gone."
  (remove "" (uiop:split-string (or text "") :separator '(#\Space #\Tab #\Newline))
          :test #'string=))

(defparameter +flags+ '(("--provider" . :provider) ("--effort" . :effort) ("--in" . :directory))
  "The flags a line may carry, and the member each sets.")

(defun parse-arguments (text)
  "TEXT, the line typed after /fast-chat, as a plist.
=> (:model SPEC :provider P :effort RUNG :directory DIR :prompt TEXT :help T),
each member present when the line said it (:prompt is always there). --prompt takes the rest of
the line, so a prompt may hold spaces without quotes."
  (let* ((words (split-words text))
         (prompt-at (position "--prompt" words :test #'string=))
         (head (if prompt-at (subseq words 0 prompt-at) words))
         (prompt (and prompt-at (nlk:trimmed (format nil "~{~a~^ ~}" (nthcdr (1+ prompt-at) words)))))
         (parsed (list :prompt (and prompt (plusp (length prompt)) prompt))))
    (loop while head
          for word = (pop head)
          do (cond
               ((member word '("--help" "-h") :test #'string=) (setf (getf parsed :help) t))
               ((assoc word +flags+ :test #'string=)
                (let ((value (pop head)))
                  (unless value (fail "~a needs a value. ~a" word *usage*))
                  (setf (getf parsed (cdr (assoc word +flags+ :test #'string=))) value)))
               ((and (plusp (length word)) (not (char= #\- (char word 0))))
                (when (getf parsed :model)
                  (fail "one model at a time. ~a" *usage*))
                (setf (getf parsed :model) word))
               (t (fail "~s is not a fast-chat option. ~a" word *usage*))))

    parsed))

(defun split-model-spec (spec)
  "SPEC as three values: MODEL, PROVIDER and EFFORT.
A trailing :RUNG is an effort only when it names one of +EFFORTS+, so an id
like deepseek-r1:free stays whole; a PROVIDER/ prefix is a provider only
when the spec holds exactly one slash, so openrouter/deepseek/deepseek-r1
stays whole -- the rule Nodecode's own --model follows."
  (let ((model (or spec "")) (provider nil) (effort nil))
    (let ((colon (position #\: model :from-end t)))
      (when (and colon (< (1+ colon) (length model))
                 (member (subseq model (1+ colon)) +efforts+ :test #'string=))
        (setf effort (subseq model (1+ colon))
              model (subseq model 0 colon))))
    (let ((slash (position #\/ model)))
      (when (and slash (plusp slash) (= slash (position #\/ model :from-end t))
                 (< (1+ slash) (length model)))
        (setf provider (subseq model 0 slash)
              model (subseq model (1+ slash)))))
    (values model provider effort)))

(defun named (value)
  "VALUE, or NIL when it is absent or blank."
  (let ((trimmed (and (stringp value) (nlk:trimmed value))))
    (and trimmed (plusp (length trimmed)) trimmed)))

;;; --- the command -----------------------------------------------------------

(defun launch (args session-id)
  "The /fast-chat command: a new session on the target ARGS names, and the
defaults where they were. Answers the text the shell shows."
  ;; The refusal while the folder is stopped is the peripheral's own: a verb
  ;; of a cell nobody started says so instead of reading an absent section.
  (running-settings)
  (let ((parsed (parse-arguments args)))
    (if (getf parsed :help)
        *usage*
        (multiple-value-bind (spec-model spec-provider spec-effort)
            (split-model-spec (or (getf parsed :model) (setting :model)))
          (let* ((model spec-model)
                 (provider (or (named (getf parsed :provider)) spec-provider
                               (named (setting :provider))))
                 (effort (or (named (getf parsed :effort)) spec-effort
                             (named (setting :effort))))
                 (prompt (getf parsed :prompt))
                 (directory (or (named (getf parsed :directory))
                                (and session-id (ignore-errors (nlk:find-session-cwd session-id))))))
            ;; Both checks land before anything durable exists: a line that
            ;; names no model, or a rung no model ladders, leaves no empty
            ;; session behind it.
            (unless (and model (plusp (length model)))
              (fail "which model? ~a" *usage*))
            (unless (member effort +efforts+ :test #'string=)
              (fail "~s is not a reasoning rung. ~{~a~^, ~}" effort +efforts+))
            (let ((id (handler-case (nlk:durable-session-id (nlk:create-session :cwd directory))
                        (error (condition)
                          (fail "no session was opened in ~a: ~a"
                                (or directory "the default directory") condition)))))
              (nlk:record-session-model-selection id :provider provider :model model
                                                     :effort effort)
              (when prompt
                (nle:submit id prompt :source "fast-chat" :source-id session-id))
              (format nil "fast-chat: session ~a opened on ~@[~a/~]~a, effort ~a; the default model is untouched.~@[ It is already working on your prompt.~] Open it from /sessions."
                      id provider model effort prompt)))))))

;;; --- the cell --------------------------------------------------------------

(nle:define-cell fast-chat
  ;; A model is the one answer a session cannot do without, so the folder is
  ;; unconfigured until it has one: /fast-chat still takes a model on its line,
  ;; but the folder's own start gate is this, and the setup panel is where a
  ;; person answers it.
  (:section ("fast-chat")
    (:guide "model is the model a bare /fast-chat opens on, as /models lists it; provider says who serves it; effort is the rung it reasons at")
    ("model" :string :required t :doc "the model a bare /fast-chat opens on")
    ("provider" :string :default "" :doc "the provider serving that model; empty follows the gateway default")
    ("effort" :choice :options '("off" "minimal" "low" "medium" "high" "xhigh" "max")
              :default "low" :doc "the reasoning rung a bare /fast-chat opens on"))
  (:command "fast-chat" 'launch
            :description "A fresh session on the model you name"
            :argument-hint "MODEL [--provider P] [--effort E] [--prompt TEXT]")
  (:help :fast-chat "fast-chat: a fresh session on a model you name, defaults untouched" *manual*))
