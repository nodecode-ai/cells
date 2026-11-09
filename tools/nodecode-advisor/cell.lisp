;;;; cell.lisp --- the advisor's verbs, /advisor, its tool, and START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of hermes-advisor-plugin's __init__.py and the rest of
;;;; runtime.py. What the Hermes plugin did through post_llm_call hooks, a
;;;; Nodecode cell does through the :FRAME point: a turn that reaches a
;;;; terminal fact is queued, one worker thread reviews it on a second model,
;;;; and deliverable advice stands as a notice the agent reads in its next
;;;; request.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "advisor": {"auto": true, "model": "", "provider": "", "clip": 500}

(in-package #:nodecode-advisor)

(nlk:define-peripheral advisor :not-running t)

(defparameter +primer+
  "The advisor is a second model that reviews this session's work. Set
`advisor: {\"auto\": true}' and every completed turn is reviewed in the
background: deliverable advice stands as a notice under this cell's key and
rides your next request. Or ask for one by hand, with (help :advisor).

  /advisor                on or off, the model, how many notes are held
  /advisor on | off       review every completed turn, or stop
  /advisor review [focus] review the newest turn now
  /advisor model NAME     the reviewer's model (default: the auxiliary model)
  /advisor provider NAME  the reviewer's provider, beside the model
  /advisor clear          drop the held notes and start over
  /advisor test SEVERITY NOTE   deliver a test note: nit, concern or blocker

Advice carries one tag. [NIT] is delivered at once. [CONCERN] and [BLOCKER]
are held until a later review raises the same point again, so a one-off
remark does not interrupt the work; a held note the next review is silent
about is dropped as resolved. The advisor reviews, it never acts: it reads
the turn's transcript and answers prose, no tools and no session state.
A tool, advisor_review, reviews a transcript the model passes it."
  "What (help :advisor) answers while the cell runs.")

;;; --- the cell's own state ----------------------------------------------------

(defvar *enabled* nil
  "Whether completed turns are reviewed: the section's `auto' to begin with,
and /advisor on or off turns it either way without a restart.")

(defvar *pending* '()
  "Terminal turns waiting to be reviewed, newest first: (SESSION TURN).")

(defvar *worker* nil
  "The reviewing thread while it runs, or NIL.")

(defvar *workers* t
  "Whether START-CELL runs the reviewing thread; a test calls TICK by hand.")

;;; --- what a review does with its answer ----------------------------------------

(defun deliver (text)
  "Stand the advice under this cell's key: every shell shows it, and the agent
reads it in the notices block of its next request."
  (nle:notice text :level :info :key "nodecode-advisor"))

(defun review-turn (session &key focus)
  "Review the newest turn of SESSION and deliver what it found.
=> a line saying what happened."
  (let ((transcript (transcript-from-log session)))
    (if (or (null transcript) (zerop (length transcript)))
        "no turn to review yet"
        (multiple-value-bind (text advice reviewer-session)
            (review transcript :session session :focus focus)
          (declare (ignore reviewer-session))
          (cond ((null advice)
                 (deliver nil)
                 (format nil "reviewed ~a: ~a" session text))
                (t
                 (deliver (format nil "Advisor review~%~%~a" text))
                 (format nil "reviewed ~a: ~a note~:p~%~a" session (length advice) text)))))))

(defun tick ()
  "Drain the queue: one review per terminal turn, newest first, and never an
error out of the worker."
  (loop while *pending*
        do (let ((entry (pop *pending*)))
             (ignore-errors (review-turn (first entry))))))

(defun queue-turn (session turn)
  (setf *pending* (cons (list session turn)
                        (remove session *pending* :key #'first :test #'string=)))
  (when *worker* (ignore-errors (nlk:worker-poke *worker*))))

(defun observe-frame (op)
  "A :FRAME observer: a turn that reached a terminal fact is queued for review
when the cell is on. Publishing thread: a list and a poke, no I/O."
  (let ((session (getf op :session-id)))
    (when (and *enabled* (stringp session))
      (multiple-value-bind (type payload metadata turn-id) (nlk:frame-fact op)
        (declare (ignore payload metadata))
        (when (and (stringp type)
                   (member type '("turn.completed" "turn.failed" "turn.cancelled")
                           :test #'string=))
          (queue-turn session (or turn-id "")))))))

;;; --- the tool ------------------------------------------------------------------

(nlk:define-contract advisor-review-arguments
  (:doc "Have a second model review work before you commit to it. Pass the transcript: the user's ask, what you did, every tool call and result, your answer. It answers tagged advice - [NIT] worth doing, [CONCERN] a material risk, [BLOCKER] unsound - or \"nothing to flag\". It reviews; it never edits files or runs anything. Use it when the work is substantial, a refactor is spreading, or you are unsure the answer actually does what was asked.")
  ("transcript" :text :required t
                :doc "the work to review, as text: the request, the steps, the tool calls and results, the answer")
  ("focus" :string :doc "one line on what to watch for, when it is narrower than the transcript"))

(defun review-tool (args)
  "The advisor_review tool: one review of what the model passed, its advice
as the answer. The note is held per session, so a concern raised twice is
delivered the second time."
  (multiple-value-bind (text advice session)
      (review (advisor-review-arguments-transcript args)
              :focus (advisor-review-arguments-focus args))
    (declare (ignore session))
    (if advice
        (format nil "~a~%[~d note~:p]" text (length advice))
        text)))

;;; --- /advisor ------------------------------------------------------------------

(defun named-or (value fallback)
  (if (and (stringp value) (plusp (length value))) value fallback))

(defun status-line (session)
  (format nil "advisor ~a - model ~a, provider ~a, ~d held~%~
Usage: /advisor [on|off|review|model|provider|clear|test]"
          (if *enabled* "on (every turn)" "off")
          (named-or (chosen *model-override* :model) "(auxiliary model)")
          (named-or (chosen *provider-override* :provider) "(auxiliary)")
          (length (load-held (or session "default")))))

(defun run-slash (args session-id)
  "The answer to one /advisor invocation."
  (let* ((raw (nlk:trimmed (or args "")))
         (space (position #\Space raw))
         (head (string-downcase (if space (subseq raw 0 space) raw)))
         (value (if space (string-trim " " (subseq raw (1+ space))) "")))
    (nlk:dispatch head string=
      ("" (status-line session-id))
      ("status" (status-line session-id))
      ("on" (setf *enabled* t)
       "advisor on: every completed turn is reviewed, and its advice stands in the notices")
      ("off" (setf *enabled* nil)
       "advisor off: only /advisor review and the advisor_review tool review anything")
      ("review" (review-turn session-id :focus value))
      ("clear" (save-held (or session-id "default") '())
       "held notes dropped")
      ("model" (if (plusp (length value))
                    (progn (setf *model-override* value)
                           (format nil "advisor model ~a (this run; set advisor.model to keep it)" value))
                    (format nil "advisor model ~a" (chosen *model-override* :model))))
      ("provider" (if (plusp (length value))
                      (progn (setf *provider-override* value)
                             (format nil "advisor provider ~a (this run; set advisor.provider to keep it)" value))
                      (format nil "advisor provider ~a" (chosen *provider-override* :provider))))
      ("test" (let* ((parts (nlk:split-words value))
                     (severity (string-downcase (or (first parts) "")))
                     (note (string-trim " " (subseq value (min (length value)
                                                                (length (first parts)))))))
                (if (and (member severity '("nit" "concern" "blocker") :test #'string=)
                         (plusp (length note)))
                    (progn (deliver (format nil "Advisor review~%~%~a"
                                            (advice-lines (list (list :severity severity :note note)))))
                           (format nil "delivered a test ~a" severity))
                    "usage: /advisor test <nit|concern|blocker> <note>")))
      (t (format nil "~a is not one of its choices; /advisor alone shows them" head)))))

;;; --- the entry ------------------------------------------------------------------

(defun install ()
  (setf *enabled* (not (null (setting-value :auto nil)))
        *pending* '())
  (nle:on-stop (lambda () (setf *enabled* nil *pending* '()
                               *model-override* "" *provider-override* "")))
  (when *workers*
    (setf *worker* (nlk:worker-start "nodecode-advisor" #'tick :wake 30))
    (nle:on-stop (lambda () (setf *worker* (nlk:worker-stop *worker*))))))

(nle:define-cell advisor
  (:section ("advisor")
    (:guide "auto is the whole switch: on, every completed turn is reviewed; model and provider pick a second model of its own, else the auxiliary one")
    ("auto" :boolean :default nil :doc "review every completed turn")
    ("model" :string :doc "the reviewer's model; empty uses the auxiliary model")
    ("provider" :string :doc "the reviewer's provider, beside model")
    ("clip" :integer :default 500 :min 100 :doc "characters kept per tool call or result")
    ("max_note_chars" :integer :default 1200 :min 100 :doc "the longest advice note kept")
    ("watchdog" :path :doc "a file of review guidance for this project, appended to the reviewer's instruction"))
  (:start #'install)
  (:help :advisor "a second model reviews each turn and posts tagged advice; /advisor on, and advisor_review for one by hand" +primer+)
  (:hook :frame (nlk:observer #'observe-frame "nodecode-advisor"))
  (:tool "advisor_review" (nlk:contract 'advisor-review-arguments) 'review-tool)
  (:command "advisor" #'run-slash
            :description "A second model reviews the work; /advisor on to review every turn"
            :argument-hint "on|off|review|model|provider|clear|test"))
