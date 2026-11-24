;;;; cell.lisp --- the section, the proposals, the revisions, the tools, /enchant.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One rewrite is a side call: the enhancer's instruction as the system
;;;; message, the draft as the user message, and only the rewritten prompt
;;;; back. It is never applied on the spot. A proposal stands for one draft
;;;; until it is taken or discarded, and taking it is the review: the whole
;;;; rewrite, or only the groups the person names, composed back into the draft
;;;; they wrote. What is taken is a revision; undo and redo move through the
;;;; revisions kept for that draft without spending another model call.


(in-package #:nodecode-enchant)

;;; --- the manual ----------------------------------------------------------------
;;; (help :enchant) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter *manual*
  "Enchant rewrites a draft prompt so it says what the person meant, then shows
them the change instead of sending it, through the nodecode-enchant cell.
  /enchant [ENHANCER] [draft]
      Rewrite the composer's draft, or the draft typed after the enhancer, with
      one of the five enhancers: clarify (the default), tighten, grammar, expand,
      summarize. The rewrite is a proposal: the draft in the composer is left as
      it was, and the answer lists the groups it comes in (change-1, change-2,
      ...), each the text taken out and the text put in its place.
  /enchant apply [all|1,2|change-2]
      Take the standing proposal whole, or only the groups named, and put the
      result in the composer as the draft, ready to send: nothing is sent.
  /enchant instant [ENHANCER]
      Propose and take it whole in one step.
  /enchant undo | /enchant redo
      Move the draft one revision back or forward: the revisions are kept per
      chat, and a redo spends no model call.
  /enchant discard   drop the standing proposal.
  /enchant list      the five enhancers, each a row that runs it.
The model reaches the same review with three tools: enchant_propose returns the
groups, enchant_apply takes the ones it names, enchant_history undoes and redoes.
A draft the composer changed since the last enchant refuses to undo: the review
fails closed rather than overwrite what the person typed."
  "What (help :enchant) answers while the cell runs.")

;;; --- one rewrite ---------------------------------------------------------------

(defvar *complete* 'nle:complete
  "The one side call a rewrite runs, as a function designator: NLE:COMPLETE -
SYSTEM the whole instruction, USER the one message, no tools, no history. A
symbol rather than a function so a test's stub of the fdefinition is the call
this cell makes.")

(defun rewrite (draft enhancer session-id)
  "DRAFT rewritten by ENHANCER, the model's answer trimmed and capped, or a
refusal when it answered nothing."
  (let ((raw (funcall *complete* (instruction enhancer) draft
                       :max-tokens (or (setting :max-tokens) +default-max-tokens+)
                       :session-id session-id)))
    (let ((text (nlk:trimmed (or raw ""))))
      (when (string= "" text)
        (fail "the model answered nothing"))
      (subseq text 0 (min (length text) +max-output-chars+)))))

;;; --- the proposals and the revisions, one of each per draft --------------------
;;; A draft is a chat: a proposal and the revisions of a draft are keyed by
;;; the session the enchant ran in, so a second chat never moves the first
;;; one's history. Both tables are guarded by the cell's lock.

(defparameter *proposals* (make-hash-table :test #'equal)
  "Draft key -> the one proposal standing for it, or absent.")

(defparameter *revisions* (make-hash-table :test #'equal)
  "Draft key -> its REVISIONS: the drafts it has been, and where it stands.")

(defvar *enchantments* 0
  "How many proposals this image made: the number a proposal's id carries.")

(defstruct (revisions (:constructor make-revisions (values index)))
  "One draft's history: the texts it has been, oldest first, and the one it
stands on. A take appends and drops whatever was ahead of it; undo and redo
only move INDEX, so a redo costs nothing."
  (values '() :type list)
  (index 0 :type fixnum))

(defun draft-key (session-id)
  "The key a draft's proposal and revisions are filed under."
  (or session-id ""))

(defun current-session ()
  "The session the running tool call belongs to, or NIL."
  (and (boundp 'nlk:*scribe-session-id*) nlk:*scribe-session-id*))

(defun history-limit ()
  "How many revisions one draft keeps: the section's `history'."
  (max 1 (or (setting :history) 30)))

(defun resolvable-enhancer (name)
  "NAME as the enhancer a rewrite runs with: the section's default when it is
empty."
  (let ((name (string-downcase (nlk:trimmed (or name "")))))
    (if (string= "" name) (setting :enhancer) name)))

(defun propose (draft &key enhancer session-id)
  "Rewrite DRAFT with ENHANCER (the section's when empty) and keep it as the
proposal standing for that draft. => the proposal plist."
  (let* ((draft (draft-text draft))
         (enhancer (resolvable-enhancer enhancer))
         (text (rewrite draft enhancer session-id))
         (key (draft-key session-id)))
    (multiple-value-bind (groups whole) (change-groups draft text)
      (let ((proposal (list :id (format nil "p~d" (incf *enchantments*))
                            :enhancer enhancer
                            :draft draft
                            :proposed text
                            :whole whole
                            :groups groups)))
        (with-enchant-lock (setf (gethash key *proposals*) proposal))
        proposal))))

(defun standing-proposal (key)
  "The proposal standing for the draft KEY, or NIL."
  (with-enchant-lock (gethash key *proposals*)))

(defun note-revision (key before after)
  "Make AFTER the draft KEY stands on, with BEFORE the draft it was before it.
A history that does not stand on BEFORE starts again from BEFORE; one already
standing on AFTER does not move. Past `history' revisions the oldest goes."
  (with-enchant-lock
    (let* ((history (gethash key *revisions*))
           (limit (history-limit)))
      (when (or (null history)
                (not (equal before (nth (revisions-index history) (revisions-values history)))))
        (setf history (make-revisions (list before) 0)))
      (unless (equal after (nth (revisions-index history) (revisions-values history)))
        (let* ((values (revisions-values history))
               (kept (append (subseq values 0 (1+ (revisions-index history)))
                             (list after)))
               (kept (last kept (min (length kept) (1+ limit)))))
          (setf (revisions-values history) kept
                (revisions-index history) (1- (length kept)))))
      (setf (gethash key *revisions*) history)
      history)))

(defun accept (key &key groups)
  "Take the proposal standing for the draft KEY - whole, or GROUPS of its
changes - and record the result as a revision. => (values TEXT REVISIONS)."
  (let ((proposal (standing-proposal key)))
    (unless proposal
      (fail "no proposal stands for this draft: enchant it first"))
    (let* ((original (getf proposal :draft))
           (accepted (if (and groups (not (getf proposal :whole)))
                         (compose original (getf proposal :groups) groups)
                         (getf proposal :proposed)))
           (history (note-revision key original accepted)))
      (with-enchant-lock (remhash key *proposals*))
      (values accepted history))))

(defun revise (key direction &key draft)
  "Move the draft KEY one revision back (DIRECTION -1) or forward (1), after
checking it still stands where the history left it when DRAFT is given: the
composer's own text. => (values TEXT STATE), STATE :MOVED, :START or :END."
  (with-enchant-lock
    (let ((history (gethash key *revisions*)))
      (unless history
        (fail "this chat has no enchanted revisions yet"))
      (let* ((values (revisions-values history))
             (index (revisions-index history))
             (draft (nlk:trimmed (or draft ""))))
        (when (and (plusp (length draft))
                   (not (equal draft (nth index values))))
          (fail "the draft changed since the last enchant, so nothing was moved: run /enchant to review it again"))
        (let ((next (+ index direction)))
          (cond ((minusp next) (values nil :start))
                ((>= next (length values)) (values nil :end))
                (t (setf (revisions-index history) next)
                   (values (nth next values) :moved))))))))

;;; --- reading a proposal out loud ------------------------------------------------

(defun proposal-text (proposal)
  "PROPOSAL as the text a person reads: the enhancer, how many changes, each
one a group of its own, and the rewrite whole."
  (let ((groups (getf proposal :groups)))
    (with-output-to-string (out)
      (format out "Enchant (~a) on a ~d-character draft: ~a.~%"
              (getf proposal :enhancer)
              (length (getf proposal :draft))
              (cond ((getf proposal :whole)
                     "too long to review in groups, so it is offered whole")
                    ((null groups) "nothing changed")
                    (t (format nil "~d change~:p" (length groups)))))
      (when (and groups (not (getf proposal :whole)))
        (format out "~{~a~%~}" (mapcar #'group-line groups)))
      (format out "~%Proposed:~%~a" (getf proposal :proposed))
      (format out "~2%Take it whole with /enchant apply all, or the groups you name with /enchant apply 1,3."))))

;;; --- the three tools ------------------------------------------------------------

(nlk:define-contract enchant-propose
  (:doc "Rewrite a draft prompt so it says what the person meant, and hand it
back for review - not sent, not answered, no tools run. The rewrite arrives
as numbered groups (change-1, change-2, ...), each the text taken out of the
draft and the text put in its place, so the person can take only the ones
they want with enchant_apply. Use it when a draft is rough, ambiguous, wordy
or in the wrong shape; never rewrite someone's words without showing them.")
  ("draft" :text :required t
   :doc "the draft to rewrite, exactly as the person wrote it")
  ("enhancer" :text :default "" :options '("clarify" "tighten" "grammar" "expand" "summarize")
   :doc "which rewrite: clarify (make the request unambiguous), tighten (cut filler), grammar (fix style only), expand (into concrete ordered work), summarize (keep the decisions); empty takes the section's default"))

(defun tool-propose (request)
  "The enchant_propose tool's body: REQUEST's draft, rewritten and kept as the
proposal standing for this chat."
  (handler-case
      (let ((proposal (propose (enchant-propose-draft request)
                                :enhancer (enchant-propose-enhancer request)
                                :session-id (current-session))))
        (format nil "~a~2%Apply with enchant_apply ({\"proposal\": \"~a\"}, and optionally the groups you name)."
                (proposal-text proposal) (getf proposal :id)))
    (enchant-error (condition) (format nil "enchant_propose: ~a" condition))
    (error (condition) (format nil "enchant_propose failed: ~a" condition))))

(nlk:define-contract enchant-apply
  (:doc "Take a standing enchantment proposal - the rewrite of this chat's
draft - and answer the accepted text, ready to paste back as the draft. Take
it whole (groups empty) or only the changes the person picked, by id: the
rest of their draft stays exactly as they wrote it. Answer the text and say
what was taken; undo and redo move through the revisions afterwards.")
  ("proposal" :text :default ""
   :doc "the id enchant_propose answered (p1, p2, ...); empty takes the one standing for this chat")
  ("groups" :array :of :string
   :doc "the change ids to take, as enchant_propose listed them (change-1, change-2, ...); empty takes the whole rewrite"))

(defun tool-apply (request)
  "The enchant_apply tool's body: the proposal taken, whole or in part."
  (handler-case
      (let* ((key (draft-key (current-session)))
             (proposal (standing-proposal key))
             (named (nlk:trimmed (or (enchant-apply-proposal request) ""))))
        (when (and (plusp (length named))
                   (not (string-equal named (getf proposal :id))))
          (fail "no proposal ~a stands for this chat: the proposal standing is ~a"
                named (if proposal (getf proposal :id) "none")))
        (multiple-value-bind (accepted history) (accept key :groups (named-groups request))
          (declare (ignore history))
          (format nil "Enchantment applied (~d characters).~2%~a~2%Undo it with enchant_history {\"action\": \"undo\"}, or /enchant undo."
                  (length accepted) accepted)))
    (enchant-error (condition) (format nil "enchant_apply: ~a" condition))
    (error (condition) (format nil "enchant_apply failed: ~a" condition))))

(defun named-groups (request)
  "The group ids REQUEST names, or NIL for every one of them."
  (let ((ids (loop for id across (coerce (or (enchant-apply-groups request) #()) 'vector)
                    for text = (nlk:trimmed (princ-to-string id))
                    unless (string= "" text)
                      collect (canonical-group text))))
    (and ids ids)))

(defun canonical-group (id)
  "ID as the group id it names: 2 and change-2 are the same group."
  (let ((id (string-downcase (nlk:trimmed id))))
    (cond ((every #'digit-char-p id) (format nil "change-~d" (parse-integer id)))
          ((and (> (length id) 7) (string= "change-" id :end2 7)) id)
          (t (format nil "change-~a" id)))))

(nlk:define-contract enchant-history
  (:doc "Move through the drafts this chat's enchantment has been - undo and
redo - without spending another model call. A move answers the draft to put
back in the composer. It refuses when the composer no longer holds the
draft the history is on, so nothing the person typed is overwritten.")
  ("action" :text :required t :options '("undo" "redo" "status")
   :doc "undo one revision, redo one, or say where the draft stands")
  ("draft" :text :default ""
   :doc "what the composer holds now, so a move that would overwrite an edit is refused; empty skips the check"))

(defun tool-history (request)
  "The enchant_history tool's body: undo, redo or status."
  (handler-case
      (let ((action (string-downcase (enchant-history-action request)))
            (draft (enchant-history-draft request)))
        (cond ((string= action "status") (history-status (draft-key (current-session))))
              (t (move-answer action (draft-key (current-session)) draft))))
    (enchant-error (condition) (format nil "enchant_history: ~a" condition))
    (error (condition) (format nil "enchant_history failed: ~a" condition))))

(defun history-status (key)
  "Where the draft KEY stands: how many revisions it has, which one it is on,
and what stands for it."
  (let ((history (standing-revisions key)))
    (if (null history)
        "No enchanted revisions for this chat yet."
        (format nil "This draft has ~d revision~:p; it stands on ~d of ~d.~@[ A proposal is waiting to be applied.~]"
                (length (revisions-values history))
                (1+ (revisions-index history))
                (length (revisions-values history))
                (standing-proposal key)))))

(defun standing-revisions (key)
  "The revisions KEY keeps, or NIL."
  (with-enchant-lock (gethash key *revisions*)))

(defun move-answer (action key draft)
  "The text and the draft effect a move ACTION on the draft KEY answers."
  (multiple-value-bind (text state) (revise key (if (string= action "undo") -1 1) :draft draft)
    (ecase state
      (:start (values "/enchant: this is the draft as it started - nothing older to undo" nil nil))
      (:end (values "/enchant: this is the newest revision - nothing to redo" nil nil))
      (:moved (values (format nil "~a~2%~a" (if (string= action "undo") "Undone." "Redone.") text)
                      nil
                      (list (list :draft text :how :set)))))))

;;; --- the command ----------------------------------------------------------------
(defun split-words (text)
  "TEXT's words: the pieces between whitespace."
  (let ((words '()) (start nil))
    (loop for index from 0 below (length text)
          for space = (whitespace-p (char text index))
          do (cond ((and space start)
                    (push (subseq text start index) words)
                    (setf start nil))
                   ((not space) (unless start (setf start index)))))
    (when start (push (subseq text start) words))
    (nreverse words)))

(defun rest-words (text)
  "TEXT past its first word, trimmed: what a /enchant line says after the verb."
  (let ((start (position-if-not #'whitespace-p text)))
    (if (null start)
        ""
        (let ((end (position-if #'whitespace-p text :start start)))
          (nlk:trimmed (if end (subseq text end) ""))))))

(defun run-slash (args session-id &key draft)
  "ARGS and the composer's DRAFT, as /enchant reads them: the text a shell
shows, the dialog it opens, and the effect that sets its composer."
  (handler-case
      (slash-answer (nlk:trimmed (or args "")) (draft-key session-id) (nlk:trimmed (or draft "")))
    (enchant-error (condition) (format nil "/enchant: ~a" condition))
    (error (condition) (format nil "/enchant failed: ~a" condition))))

(defun slash-answer (args key draft)
  "ARGS against the draft KEY and the composer's DRAFT."
  (let* ((words (split-words args))
         (head (string-downcase (or (first words) "")))
         (rest (rest-words args)))
    (cond
      ((string= head "") (if (string= "" draft) (enhancer-dialog) (propose-answer nil draft key)))
      ((string= head "list") (enhancer-dialog))
      ((string= head "apply") (apply-answer rest key))
      ((string= head "instant") (instant-answer rest draft key))
      ((string= head "undo") (move-answer "undo" key draft))
      ((string= head "redo") (move-answer "redo" key draft))
      ((string= head "status") (values (history-status key) nil nil))
      ((string= head "discard") (discard-answer key))
      ((member head (enhancer-names) :test #'string=)
       (propose-answer head (if (string= "" rest) draft rest) key))
      (t (propose-answer nil args key)))))

(defun propose-answer (enhancer draft key)
  "The review a proposal is shown with: its text, and a dialog whose rows take
it whole, take one group, or drop it."
  (when (string= "" draft)
    (fail "there is no draft: type it in the composer, or after the enhancer"))
  (let ((proposal (propose draft :enhancer enhancer :session-id key)))
    (values (proposal-text proposal) (proposal-dialog proposal) nil)))

(defun apply-answer (rest key)
  "The proposal standing for KEY applied whole or by the groups REST names,
and put in the composer."
  (let ((ids (group-ids rest)))
    (multiple-value-bind (accepted history) (accept key :groups ids)
      (values (format nil "Enchantment applied (~d characters, ~d revision~:p kept). /enchant undo puts the draft back.~2%~a"
                      (length accepted) (length (revisions-values history)) accepted)
              nil
              (list (list :draft accepted :how :set))))))

(defun instant-answer (rest draft key)
  "A proposal on DRAFT taken whole at once, with the enhancer REST names."
  (when (string= "" draft) (fail "there is no draft: type it in the composer"))
  (let ((proposal (propose draft :enhancer (if (string= "" rest) nil rest) :session-id key)))
    (multiple-value-bind (accepted history) (accept key)
      (values (format nil "Enchant (~a) applied whole: ~d change~:p, ~d revision~:p kept. /enchant undo puts the draft back.~2%~a"
                      (getf proposal :enhancer) (length (getf proposal :groups))
                      (length (revisions-values history)) accepted)
              nil
              (list (list :draft accepted :how :set))))))

(defun discard-answer (key)
  "The standing proposal for KEY dropped."
  (let ((proposal (standing-proposal key)))
    (unless proposal (fail "no proposal stands for this draft"))
    (with-enchant-lock (remhash key *proposals*))
    (values (format nil "Discarded ~a (~a): the composer's draft was never touched.~2%~a"
                    (getf proposal :id) (getf proposal :enhancer) (getf proposal :draft))
            nil nil)))

(defun group-ids (rest)
  "REST as the group ids it names: NIL for all of them, else each number or
change-N, comma or space separated."
  (let ((rest (string-downcase (nlk:trimmed rest))))
    (cond ((or (string= "" rest) (string= "all" rest)) nil)
          (t (loop for piece in (split-on rest '(#\, #\Space))
                   for trimmed = (nlk:trimmed piece)
                   unless (string= "" trimmed)
                     collect (canonical-group trimmed))))))

(defun split-on (text delimiters)
  "TEXT cut at every character in DELIMITERS."
  (let ((pieces '()) (start 0))
    (loop for index from 0 below (length text)
          when (member (char text index) delimiters :test #'char=)
            do (push (subseq text start index) pieces)
               (setf start (1+ index)))
    (push (subseq text start) pieces)
    (nreverse pieces)))

(defun enhancer-dialog ()
  "The five enhancers as the rows that run them."
  (values "Enchant: pick an enhancer below, or type /enchant <enhancer> <draft>. The composer's draft is rewritten when one is run."
          (nle:list-dialog
           :title "Enchant"
           :context "the composer's draft"
           :summary "Rewrite the draft, review the changes, take the ones you want"
           :rows (loop for (name . body) in +enhancers+
                       collect (list :key name
                                     :label (string-capitalize name)
                                     :detail body
                                     :value name
                                     :verb "Rewrite"
                                     :run (list :enter (format nil "/enchant ~a" name)))))
          nil))

(defun proposal-dialog (proposal)
  "PROPOSAL as the rows a person picks from: take it whole, take one group,
or drop it."
  (let* ((groups (getf proposal :groups))
         (rows (list (list :key "all"
                           :label "Take the whole rewrite"
                           :detail (format nil "~a, ~a" (getf proposal :enhancer)
                                           (if (getf proposal :whole) "offered whole"
                                               (format nil "~d change~:p" (length groups))))
                           :verb "Apply"
                           :run (list :enter "/enchant apply all")))))
    (values ""
            (nle:list-dialog
             :title "Enchant"
             :context (format nil "~a, ~a" (getf proposal :enhancer)
                              (if (getf proposal :whole) "offered whole"
                                  (format nil "~d change~:p" (length groups))))
             :summary "Take the whole rewrite, or only the changes you pick"
             :rows (append rows
                           (when (and groups (not (getf proposal :whole)))
                             (loop for group in (subseq groups 0 (min 8 (length groups)))
                                   collect (list :key (getf group :id)
                                                 :label (format nil "Take only ~a" (getf group :id))
                                                 :detail (group-line group)
                                                 :verb "Apply"
                                                 :run (list :enter (format nil "/enchant apply ~a"
                                                                           (getf group :id))))))
                           (list (list :key "discard" :label "Discard the proposal"
                                       :verb "Discard" :run (list :enter "/enchant discard"))))))))

;;; --- the cell -------------------------------------------------------------------

(defparameter +command-owner+ "nodecode-enchant"
  "The key /enchant is filed under: the folder's own name, as a declaration's
(:command ...) clause would file it.")

(defun install-command ()
  "Put /enchant in the one slash catalog, reading the composer's draft where
the runtime offers it and taking a draft as its argument everywhere."
  (handler-case
      (nle:register-command +command-owner+ "enchant" #'run-slash
                             :description "Rewrite a draft, review the changes, take the ones you want"
                             :argument-hint "[ENHANCER|apply|instant|undo|redo|discard|list] [draft]"
                             :reads '(:draft))
    (error ()
      (nle:register-command +command-owner+ "enchant" #'run-slash
                             :description "Rewrite a draft, review the changes, take the ones you want"
                             :argument-hint "[ENHANCER|apply|instant|undo|redo|discard|list] [draft]"))))

(defun install ()
  "START-CELL's own start: /enchant in the catalog, and both tables empty;
all of it taken back when the cell stops."
  (install-command)
  (nle:on-stop (lambda () (nle:unregister-commands +command-owner+)))
  (with-enchant-lock
    (clrhash *proposals*)
    (clrhash *revisions*)))
(nle:define-cell enchant
  (:section ("enchant")
    (:guide "nothing to fetch or paste: the rewrite runs on the session's own model (the auxiliary target); enhancer picks the instruction a bare /enchant uses, history how many revisions undo and redo keep, max_tokens one rewrite's ceiling; next step: type a draft in the composer and run /enchant")
    ("enhancer" :choice :options '("clarify" "tighten" "grammar" "expand" "summarize") :default "clarify"
     :doc "/enchant with no enhancer named rewrites with this one")
    ("history" :integer :default 30 :min 1
     :doc "how many revisions one draft keeps: undo and redo move through them, and a redo spends no model call")
    ("max_tokens" :integer :default 4096 :min 256
     :doc "one rewrite's completion ceiling, as the original asked its model for"))
  (:help :enchant
         "enchant: /enchant and enchant_propose rewrite a draft, review the changes group by group, undo and redo"
         *manual*)
  (:tool "enchant_propose" (nlk:contract 'enchant-propose) #'tool-propose)
  (:tool "enchant_apply" (nlk:contract 'enchant-apply) #'tool-apply)
  (:tool "enchant_history" (nlk:contract 'enchant-history) #'tool-history)
  (:start #'install))
