;;;; cell.lisp --- the provenance gate: what the agent keeps, in the user's words.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode rewrite of simin-yuan's hermes-provenance-gate (see NOTICE).
;;;; The original gated Hermes's memory and skill_manage tools. Nodecode keeps
;;;; a memory with (define-memory ...) or (define-skill ...) through eval, so
;;;; the :tool hook reads the eval call's text and checks only a call that
;;;; keeps one. What the user typed this session is captured from the request
;;;; the context is built for, the same seam a router reads it from.

(in-package #:nodecode-provenance)

(nlk:define-peripheral provenance :not-running t)

(defparameter +default-mode+ "observe")
(defparameter +default-scope+ "decisions")
(defparameter +retry-limit+ 3)
(defparameter +claim-preview+ 160)

(defun mode-setting () (if *provenance* (setting :mode) +default-mode+))
(defun scope-setting () (if *provenance* (setting :scope) +default-scope+))

;;; --- what the user typed this session --------------------------------------------

(defparameter *said* (make-hash-table :test #'equal)
  "Session id -> the texts the user typed, oldest first, bounded.")
(defparameter *said-limit* 200)
(defparameter *blocks* (make-hash-table :test #'equal)
  "A count of blocks per (session . call target), for the stop line.")

(defun remember-said (session-id texts)
  (when (and (stringp session-id) texts)
    (setf (gethash session-id *said*)
          (last (append (gethash session-id *said*)
                        (remove-if-not (lambda (text) (and (stringp text) (plusp (length text)))) texts))
                *said-limit*))))

(defun session-said (session-id)
  (and (stringp session-id) (gethash session-id *said*)))

(defun user-texts-of (messages)
  "The texts of the user's own messages among MESSAGES, in order."
  (let ((texts '()))
    (dolist (message (cond ((listp messages) messages)
                           ((vectorp messages) (coerce messages 'list))))
      (when (and (hash-table-p message) (nle::user-role-message-p message))
        (let ((text (nle:message-content message)))
          (when (and (stringp text) (plusp (length text))) (push text texts)))))
    (nreverse texts)))

(defun advice-argument (arguments key)
  "KEY's value among the arguments a context request is made with."
  (loop for (name value) on (rest arguments) by #'cddr when (eq name key) return value))

(defun capture-around (next &rest arguments)
  "Advice on the context build: the user's messages of this session are kept, so a
gate run later in the turn has them. A failure here never fails the request."
  (handler-case
      (let ((messages (advice-argument arguments :messages))
            (session (first arguments)))
        (when (and (stringp session) messages)
          (remember-said session (user-texts-of messages))))
    (error (condition) (declare (ignore condition)) nil))
  (apply next arguments))

;;; --- the evidence log -------------------------------------------------------------

(defun evidence-path () (nlk:home "provenance/evidence.jsonl"))

(defun evidence-rows ()
  "Every evidence row on file, oldest first; a row that cannot be read is skipped."
  (let ((path (evidence-path)))
    (when (probe-file path)
      (with-open-file (in path :if-does-not-exist nil)
        (loop for line = (read-line in nil nil)
              while line
              for row = (handler-case (nlk:decode-json line) (error () nil))
              when (hash-table-p row) collect row)))))

(defun write-rows (rows)
  (let ((path (evidence-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (row rows) (write-line (nlk:encode-json-object row) out)))))

(defun days () (if *provenance* (setting :retention-days) 30))

(defun prune-evidence ()
  "Drop rows older than the retention the section names. A file that cannot be read is left."
  (handler-case
      (let* ((cutoff (- (get-universal-time) (* 86400 (days))))
             (rows (evidence-rows)))
        (when rows (write-rows (remove-if (lambda (row)
                                            (let ((ts (gethash "ts" row)))
                                              (and (integerp ts) (< ts cutoff))))
                                          rows))))
    (error (condition) (declare (ignore condition)) nil)))

(defun record-check (check tool-name session-id verdict)
  (handler-case
      (let ((path (evidence-path))
            (row (nlk:json-object
                  "ts" (get-universal-time)
                  "tool" (or tool-name "check")
                  "session" (or session-id "")
                  "claim" (or (getf check :claim) "")
                  "quote" (or (getf check :quote) "")
                  "verdict" verdict
                  "reason" (or (getf check :reason) ""))))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :append :if-does-not-exist :create)
          (write-line (nlk:encode-json-object row) out)))
    (error (condition) (declare (ignore condition)) nil)))

;;; --- one answer -------------------------------------------------------------------

(defun preview (text limit)
  (let ((one (nlk:one-line (or text ""))))
    (if (> (length one) limit) (concatenate 'string (subseq one 0 (1- limit)) "…") one)))

(defun failed-check (checks)
  (find-if-not (lambda (check) (getf check :ok)) checks))

(defun check-summary (checks mode)
  "CHECKS read back as what the model or the operator is told."
  (let ((failed (failed-check checks)))
    (cond ((null checks)
           "provenance: pass - no sentence here states a user decision, so nothing was checked.")
          ((null failed)
           (format nil "provenance: pass - ~d claim~p, each backed by the user's own words~@[ (\"~a\")~]."
                   (length checks) (length checks)
                   (getf (find-if (lambda (c) (getf c :ok)) checks) :quote)))
          ((string= mode "block")
           (format nil "provenance: blocked (~a) - ~a"
                   (getf failed :reason) (block-message failed 1)))
          (t (format nil "provenance: would_block (~a) - the write states a user decision (\"~a\")~@[ and the quote (\"~a\") does not back it~]; mode observe lets it through."
                      (getf failed :reason) (preview (getf failed :claim) +claim-preview+)
                      (getf failed :quote))))))

(defun block-message (check attempts)
  (let ((claim (preview (getf check :claim) +claim-preview+))
        (quote (preview (getf check :quote) 80))
        (reason (getf check :reason)))
    (format nil "provenance: this write states a user decision (\"~a\") without the user's words.~@[ ~a~] Quote the user's exact words in \"...\" if they said it, or write it as your own suggestion or as an open question. Nothing was written.~@[ Stop retrying this write; ask the user instead.~]"
            claim
            (cond ((string= reason "quote_not_found")
                   (format nil "The quoted words (\"~a\") are not in anything the user typed in this session." quote))
                  ((string= reason "quote_too_short")
                   (format nil "The quote (\"~a\") is shorter than ~d words and is not one of the user's last few messages in full."
                           quote +short-quote-limit+))
                  ((string= reason "quote_negated")
                   (format nil "The quoted words (\"~a\") follow a negation in what the user typed (for example \"never ...\" or \"don't ...\"), so they do not back the decision." quote))
                  ((string= reason "quote_from_question")
                   (format nil "The quoted words (\"~a\") come from a question the user asked." quote))
                  ((string= reason "quote_unrelated")
                   (format nil "The quoted words (\"~a\") do not share key words with the decision." quote))
                  ((string= reason "no_quote") "No quote of the user is in it.")
                  (t nil))
            (and (>= attempts +retry-limit+)))))

(defun check-text (text &key user-text (scope nil) (mode nil))
  "The cell's one verb: check TEXT (a memory or a skill line) against the user's own words.
USER-TEXT is what the user typed; empty asks this session's captured messages.
=> (VALUES SUMMARY CHECKS), the summary a sentence and the checks a list of plists."
  (let* ((texts (cond ((and (stringp user-text) (plusp (length (string-trim '(#\Space) user-text))))
                        (list user-text))
                       (t (session-said (getf (nle:turn) :session-id)))))
         (checks (evaluate-text text texts :scope (or scope (scope-setting))))
         (mode (or mode (mode-setting))))
    (values (check-summary checks mode) checks)))

;;; --- the tools, the command, the manual ------------------------------------------

(nlk:define-contract check-ask
  (:doc "Check a text the agent is about to keep (a line of memory or of a skill) against the user's own words: does every sentence that states a user decision carry a quote that appears verbatim in what the user typed this session? Use it before keeping a fact about what the user decided.")
  ("text" :text :required t :doc "the memory or skill line about to be written")
  ("user_text" :string :default "" :doc "the user's own words to check against, when they are not this session's captured messages"))

(nlk:define-contract report-ask
  (:doc "What the provenance gate has seen: how many kept claims carried the user's words, how many stated a decision without them, and the newest one that did.")
  ("limit" :integer :default 5 :doc "how many newest flagged claims to list"))

(defun latest-rows (verdict &optional (limit 5))
  (let ((rows (remove-if-not (lambda (row) (equal (gethash "verdict" row) verdict)) (evidence-rows))))
    (last rows (min limit (length rows)))))

(defun report-text (&optional (limit 5))
  (let* ((rows (evidence-rows))
         (counted (lambda (verdict)
                    (count-if (lambda (row) (equal (gethash "verdict" row) verdict)) rows)))
         (flagged (remove-if (lambda (row) (equal (gethash "verdict" row) "pass")) rows))
         (latest (last flagged (min limit (length flagged))))
         (newest (car (last flagged)))
         (lines (format nil "~{  ~a~%~}"
                        (mapcar (lambda (row)
                                  (format nil "~a ~a: \"~a\""
                                          (or (gethash "verdict" row) "?")
                                          (or (gethash "reason" row) "-")
                                          (preview (gethash "claim" row) 100)))
                                latest))))
    (format nil "provenance: ~a, ~a, ~d evidence row~:p (pass ~d, would_block ~d, blocked ~d)~%~@[~a~]~@[  newest flagged: \"~a\" (~a)~]"
            (if *provenance* "running" "not running")
            (mode-setting) (length rows)
            (funcall counted "pass") (funcall counted "would_block") (funcall counted "blocked")
            (and (plusp (length lines)) lines)
            (and newest (gethash "claim" newest))
            (and newest (gethash "reason" newest)))))
(defun status-text ()
  (format nil "provenance: ~a~%  mode: ~a~%  scope: ~a~%  retention: ~d day~:p~%  evidence: ~a~%  sessions captured this boot: ~d"
          (if *provenance* "running" "not running")
          (mode-setting) (scope-setting) (days) (evidence-path) (hash-table-count *said*)))

(defun tool-check (ask)
  (multiple-value-bind (summary checks)
      (check-text (check-ask-text ask) :user-text (check-ask-user-text ask))
    (dolist (check checks) (record-check check "provenance_check" (getf (nle:turn) :session-id)
                                          (if (getf check :ok) "pass" (if (string= (mode-setting) "block") "blocked" "would_block"))))
    summary))

(defun tool-report (ask)
  (report-text (report-ask-limit ask)))

(defun run-command (args session-id)
  (declare (ignore session-id))
  (let ((text (and args (string-trim '(#\Space #\Tab) args))))
    (cond ((or (null text) (zerop (length text)) (string-equal text "report"))
           (values (report-text)
                   (nle:list-dialog
                    :title "Provenance gate"
                    :summary "What the agent kept, and whether the user's own words back it."
                    :rows (list (nle:ask-row "Check a text" "Text" "/provenance"
                                              :placeholder "User decided to ship on Friday." :verb "check")))))
          ((string-equal text "purge")
           (handler-case (progn (when (probe-file (evidence-path)) (delete-file (evidence-path)))
                                "provenance: the evidence log is empty now.")
             (error (condition) (format nil "provenance: could not purge the log (~a)." condition))))
          ((cl-ppcre:scan "^check\\s" text)
           (multiple-value-bind (summary checks)
               (check-text (string-trim '(#\Space) (subseq text 6)))
             (values summary (and checks nil))))
          (t (multiple-value-bind (summary checks) (check-text text)
               (values summary (and checks nil)))))))

(defparameter +primer+
  "Provenance gate is on: a write that keeps a memory or a skill is checked against the words the
user typed this session. A sentence that states a user decision (\"User decided ...\") needs a quote
in \"...\" that appears verbatim in what the user typed, shares key words with the decision and does
not drop a negation. In observe mode (the default) the write goes through and is recorded; in block
mode it is refused with an instruction. One verb:
  (nodecode-provenance:check-text \"User decided to ship on Friday.\")   the summary, and the checks"
  "What (help :provenance) answers while the cell runs.")

(defun memory-write-text (op)
  "The text OP writes when the call keeps a memory or a skill, or NIL for any other call."
  (let* ((name (getf op :name))
         (arguments (getf op :arguments)))
    (when (and (stringp name) (hash-table-p arguments))
      (cond
        ((string-equal name "eval")
         (let ((text (with-output-to-string (out)
                       (let ((form (gethash "form" arguments))
                             (strings (gethash "strings" arguments)))
                         (when form (format out "~@[~a~]~%" (if (stringp form) form (format nil "~s" form))))
                         (when (hash-table-p strings)
                           (maphash (lambda (key value)
                                      (declare (ignore key))
                                      (when (stringp value) (format out "~a~%" value)))
                                    strings))))))
           (when (cl-ppcre:scan "(?i)\\(\\s*define-(?:memory|skill)" text) text)))
        ((member name '("memory" "remember" "skill_manage") :test #'string-equal)
         (let ((content (gethash "content" arguments)))
           (when (stringp content) content)))
        (t nil)))))

(defun gate-call (op next)
  "The :TOOL point: a call that keeps a decision the user's words do not back is
recorded in observe mode and refused in block mode; every other call runs."
  (let ((text (memory-write-text op)))
    (if (null text)
        (funcall next op)
        (let* ((session (getf (nle:turn) :session-id))
               (checks (evaluate-text text (session-said session) :scope (scope-setting)))
               (failed (failed-check checks)))
          (dolist (check checks)
            (record-check check (getf op :name) session
                          (cond ((getf check :ok) "pass")
                                ((string= (mode-setting) "block") "blocked")
                                (t "would_block"))))
          (cond ((null failed) (funcall next op))
                ((string= (mode-setting) "block")
                 (let* ((key (format nil "~a|~a" (or session "") (getf op :name)))
                        (attempts (incf (gethash key *blocks* 0))))
                   (nle:failure (block-message failed attempts))))
                (t (funcall next op)))))))

(nle:define-cell provenance
  (:section ("provenance")
    (:guide "mode observe never refuses a write and records what it would have refused; mode block refuses it; scope decisions+preferences also asks a quote of a stated preference; next step: /provenance shows what it has seen")
    ("mode" :choice :options '("observe" "block") :default "observe"
            :doc "observe records a decision claim with no quote and lets the write through; block refuses it")
    ("scope" :choice :options '("decisions" "decisions+preferences") :default "decisions"
             :doc "decisions+preferences also asks for a quote when the write states what the user prefers")
    ("retention_days" :integer :default 30 :min 1
                      :doc "days of evidence rows kept under the cell's home")
    ("max_claims" :integer :default 100 :min 1
                  :doc "how many claims one write may state before it is refused"))
  (:tool "provenance_check" (nlk:contract 'check-ask) 'tool-check)
  (:tool "provenance_report" (nlk:contract 'report-ask) 'tool-report)
  (:command "provenance" 'run-command
            :description "What was kept, and whether the user's words back it"
            :argument-hint "check TEXT | report | purge")
  (:help :provenance "provenance:check-text asks whether a memory write states a user decision the user's words do not back" +primer+)
  (:hook 'nle::snapshot-context-request #'capture-around)
  (:hook :tool #'gate-call)
  (:start (lambda ()
            (clrhash *said*)
            (clrhash *blocks*)
            (prune-evidence)
            (nle:on-stop (lambda () (clrhash *said*) (clrhash *blocks*))))))
