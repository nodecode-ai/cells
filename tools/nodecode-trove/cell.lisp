;;;; cell.lisp --- Trove: recall over the durable log, cited by seq.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of misterdas's hermes-trove (see NOTICE). The original keeps
;;;; every message, tool call and result in a SQLite DAG and answers from it,
;;;; so compaction never loses one. Nodecode keeps the same facts in its
;;;; durable event log already, and eviction only moves a visibility floor:
;;;; the window forgets, the log does not. This cell is the original's
;;;; surface over that log -- grep every session at once, read one session in
;;;; order, fetch one cited entry whole, the session directory, an evidence
;;;; pack under a character budget, and a status line -- plus /trove for the
;;;; operator.
;;;;
;;;; Every row cites [session seq]. The seq is the log position, so a row
;;;; read now names the same entry after the window has moved on: that is the
;;;; original's one promise, and the reason a row is worth citing at all.

(defpackage #:nodecode-trove
  (:use #:cl)
  (:export #:trove-error #:trove-error-detail))

(in-package #:nodecode-trove)

(nlk:define-peripheral trove :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter *primer*
  "Trove reads the durable log every session writes -- each input, answer, tool
call and result, whether or not it is still in the window. Every row cites
[session seq]; the seq is a log position, so a row read now names the same
entry after the window has moved on, and trove_entry returns it whole.
  trove_search QUERY             grep every session at once, newest first
  trove_read [session_id, match, before, limit, clip]
                                  one session's rows, oldest first; page older
                                  with before=<the oldest seq returned>
  trove_entry SEQ                one entry, body whole
  trove_sessions                 the session directory, newest activity first
  trove_evidence QUERY [budget]  the rows one query matched as a cited digest
  trove_status                   what the log holds and what this cell reads
/trove typed alone opens a panel; /trove QUERY searches; /trove read SESSION
reads one session. A row read with nothing matching is an answer, not an error."
  "What (help :trove) answers while the cell runs.")

;;; --- small readers -------------------------------------------------------------

(defun trove-clip (text limit)
  "TEXT on one line, at most LIMIT characters."
  (let* ((limit (max 1 (or limit 1)))
         (one (substitute #\Space #\Return
                          (substitute #\Space #\Tab
                                      (substitute #\Space #\Newline (or text ""))))))
    (if (> (length one) limit)
        (concatenate 'string (subseq one 0 limit) "...")
        one)))

(defun short-time (at)
  "AT as minutes: an ISO stamp cut to its minute, or as given."
  (cond ((and (stringp at) (>= (length at) 16)) (subseq at 0 16))
        ((stringp at) at)
        (t "?")))

(defun who (row)
  "The speaker of a row: a message's role, a tool call's tool, or `entry'."
  (or (getf row :role) (getf row :tool) "entry"))

(defun take (count list)
  (if (and count (< count (length list))) (subseq list 0 count) list))

(defun recall-rows (rows)
  "ROWS with RECALL-LOG's leading (:matched N :showing M) marker split off.
=> (values ROWS MARKER), MARKER NIL when every match fitted."
  (let ((marker (and rows (getf (first rows) :matched) (first rows))))
    (values (if marker (rest rows) rows) marker)))

(defun current-session ()
  "The session this tool body runs for, when the turn has one."
  (or nlk:*scribe-session-id*
      (and (boundp 'nle::*current-durable-turn*)
           nle::*current-durable-turn*
           (ignore-errors
             (nlk:durable-turn-session-id nle::*current-durable-turn*)))))

(defun newest-session-id ()
  (getf (first (ignore-errors (nlk:list-sessions))) :id))

(defun session-entry (session)
  (find session (ignore-errors (nlk:list-sessions))
        :key (lambda (entry) (getf entry :id)) :test #'equal))

(defun session-name (session)
  (let* ((entry (session-entry session))
         (title (and entry (getf entry :title))))
    (if (and (stringp title) (plusp (length title))) title session)))

(defun store-text ()
  "Where the log lives, as far as this image will say."
  (cond ((not (nlk:store-open-p)) "not open")
        ((and (boundp 'nlk::*store*) nlk::*store*
              (ignore-errors (nlk:store-path nlk::*store*)))
         (namestring (nlk:store-path nlk::*store*)))
        (t "open")))

(defun row-line (row session &key title (clip nil clip-given))
  "One row of an answer: its citation first, then who and what."
  (format nil "[~a ~d ~a ~a]~@[ ~a:~] ~a"
          session (or (getf row :seq) 0) (short-time (getf row :at)) (who row)
          (and title (plusp (length title)) title)
          (trove-clip (or (getf row :text) "(no body)")
                      (if clip-given clip (setting :clip)))))

(defun truncate-text (text limit)
  "TEXT as it is, cut to LIMIT characters with an ellipsis when it is longer."
  (let ((text (or text "")))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit) "...")
        text)))

;;; --- the six reads ------------------------------------------------------------

(defun search-text (query limit window)
  "Every session's rows matching QUERY, newest first, each cited."
  (multiple-value-bind (rows marker)
      (recall-rows (nlk:recall-search query
                                      :limit (max 1 (or limit (setting :limit)))
                                      :window (max 40 (or window (setting :clip)))
                                      :lead 40))
    (if (null rows)
        (format nil "Trove: no entry in the log matches ~s." query)
        (with-output-to-string (out)
          (format out "Trove: ~d row~:p~@[ of ~d matched~] for ~s, newest first. A row cites [session seq]; trove_entry on the seq reads that entry whole, trove_read session_id=<session> before=<seq> continues around it."
                  (length rows) (getf marker :matched) query)
          (dolist (row rows)
            (format out "~%~a" (row-line row (getf row :session-id)
                                         :title (trove-clip (getf row :title) 60))))))))

(defun read-text (session &key match before limit clip)
  "One session's rows in order, oldest first, paged by BEFORE."
  (let* ((session (or session (current-session) (newest-session-id)))
         (limit (max 1 (or limit (setting :limit))))
         (clip (max 40 (or clip (setting :clip)))))
    (unless session (fail "the log holds no session to read yet"))
    (unless (session-entry session)
      (fail "no session ~a in the log: trove_sessions lists them" session))
    (multiple-value-bind (rows marker)
        (recall-rows (nlk:recall-log :session-id session
                                     :match (and match (plusp (length match)) match)
                                     :before before :limit limit :clip clip))
      (if (null rows)
          (format nil "Trove: ~a~@[ before seq ~d~]~@[ matching ~s~] has no conversational row."
                  (session-name session) before match)
          (with-output-to-string (out)
            (format out "Trove: ~a [~a]~@[, before seq ~d~]~@[ matching ~s~]: ~d row~:p~@[ of ~d matched~], oldest first."
                    (session-name session) session before match
                    (length rows) (getf marker :matched))
            (dolist (row rows)
              (format out "~%~a" (row-line row session :clip clip)))
            (when (getf marker :matched)
              (format out "~%older: trove_read session_id=~a before=~d" session (getf (first rows) :seq))))))))

(defun entry-text (seq)
  "One entry, wherever it sits, body whole (to a generous ceiling)."
  (let ((row (nlk:recall-entry seq)))
    (unless row (fail "no entry at seq ~d" seq))
    (let ((body (or (getf row :text) (getf row :payload) "(no body)")))
      (format nil "[seq ~d ~a ~a]~%~a~@[~%... cut at ~d characters~]"
              seq (short-time (getf row :at)) (who row)
              (truncate-text body 8000)
              (and (> (length body) 8000) 8000)))))

(defun sessions-text (limit)
  "The session directory, newest activity first."
  (let ((sessions (take (max 1 (or limit (setting :sessions)))
                         (ignore-errors (nlk:list-sessions)))))
    (if (null sessions)
        "Trove: the log holds no session yet."
        (with-output-to-string (out)
          (format out "Trove: ~d session~:p, newest activity first. trove_read session_id=<id> reads one." (length sessions))
          (dolist (entry sessions)
            (format out "~%~a  ~d message~:p  ~a  ~a~@[  ~a~]"
                    (getf entry :id) (or (getf entry :message-count) 0)
                    (short-time (getf entry :updated-at))
                    (trove-clip (or (getf entry :title) "(untitled)") 70)
                    (let ((dir (getf entry :working-directory)))
                      (and (stringp dir) (plusp (length dir)) dir))))))))

(defun evidence-text (query budget limit)
  "The rows QUERY matched as one digest, deduplicated and cited, in BUDGET."
  (let* ((budget (max 200 (or budget (setting :budget))))
         (rows nil) (marker nil))
    (multiple-value-setq (rows marker)
      (recall-rows (nlk:recall-search query
                                       :limit (max 1 (or limit (setting :limit)))
                                       :window 400 :lead 40)))
    (if (null rows)
        (format nil "Trove: no entry in the log matches ~s." query)
        (let ((seen (make-hash-table :test #'equal))
              (kept '()))
          (dolist (row rows)
            (let ((text (or (getf row :text) "")))
              (unless (gethash text seen)
                (setf (gethash text seen) t)
                (push row kept))))
          (setf kept (nreverse kept))
          (with-output-to-string (out)
            (let ((left budget))
              (format out "Trove evidence for ~s: ~d row~:p after duplicates~@[, of ~d matched~]. Cite as [session seq]; trove_entry reads one whole."
                      query (length kept) (getf marker :matched))
              (loop for row in kept
                    for block = (format nil "~%~a~%    ~a"
                                        (format nil "[~a ~d ~a ~a]"
                                                (getf row :session-id) (getf row :seq)
                                                (short-time (getf row :at)) (who row))
                                        (trove-clip (or (getf row :text) "") (max 40 (min 800 (- left 60)))))
                    while (plusp left)
                    do (write-string block out)
                       (decf left (length block)))
              (format out "~%budget ~d characters." budget)))))))

(defun status-text ()
  "What the log holds and what this cell reads it with."
  (let* ((sessions (ignore-errors (length (nlk:list-sessions))))
         (events (ignore-errors (nlk:event-count))))
    (format nil "Trove: ~@[~d session~:p~]~@[ over ~d durably logged event~:p~]; store ~a. Rows cite [session seq] and trove_entry reads one whole; limit=~d clip=~d budget=~d sessions=~d."
            sessions events (store-text)
            (setting :limit) (setting :clip) (setting :budget) (setting :sessions))))

;;; --- the tools ------------------------------------------------------------------

(defun tool-search (call)
  (search-text (search-call-query call) (search-call-limit call) (search-call-window call)))

(defun tool-read (call)
  (read-text (read-call-session-id call) :match (read-call-match call)
             :before (read-call-before call) :limit (read-call-limit call)
             :clip (read-call-clip call)))

(defun tool-entry (call)
  (entry-text (entry-call-seq call)))

(defun tool-sessions (call)
  (sessions-text (sessions-call-limit call)))

(defun tool-evidence (call)
  (evidence-text (evidence-call-query call) (evidence-call-budget call) (evidence-call-limit call)))

(defun tool-status (call)
  (declare (ignore call))
  (status-text))

;;; --- /trove ----------------------------------------------------------------------

(defun panel ()
  (let ((sessions (ignore-errors (nlk:list-sessions))))
    (nle:list-dialog
     :title "Trove"
     :summary (if sessions
                  (format nil "~d session~:p in the log" (length sessions))
                  "the log is empty")
     :rows (append
            (list (nle:ask-row "Search every session" "Query" "/trove"
                               :placeholder "words to find" :verb "search"
                               :detail "every session's log at once, cited by seq"))
            (loop for entry in (take 8 sessions)
                  collect (nle:run-row
                           (format nil "Read ~a" (trove-clip (or (getf entry :title)
                                                                 (getf entry :id)) 50))
                           (format nil "/trove read ~a" (getf entry :id))
                           :detail (format nil "~d message~:p, last ~a"
                                           (or (getf entry :message-count) 0)
                                           (short-time (getf entry :updated-at)))))))))

(defun run-slash (args session-id)
  "Bare /trove: what the log holds and a panel. /trove QUERY searches every
session; /trove read SESSION reads one, this one unless named; /trove status
is the status line."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (head (if space (subseq text 0 space) text))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) "")))
    (cond ((zerop (length text)) (values (status-text) (panel)))
          ((equal head "read")
           (values (read-text (if (plusp (length rest)) rest nil)) nil))
          ((equal head "status") (values (status-text) nil))
          (t (values (search-text text nil nil) nil)))))

;;; --- the declaration ------------------------------------------------------------

(nlk:define-contract search-call
  (:doc "Grep every session's durable log at once: every token of the query must appear in an entry's conversational body, case-insensitively. Use it to find where something was said, then trove_read that session, or trove_entry on a row's seq for the body whole. Rows are newest first and each cites [session seq].")
  ("query" :text :required t :doc "the words to look for; every one must appear in the entry")
  ("limit" :integer :doc "how many rows to answer, newest first (the section's limit unless given)")
  ("window" :integer :doc "characters of the matching body one row shows (the section's clip unless given)"))

(nlk:define-contract read-call
  (:doc "Read one session's durable log in order, oldest first: what was asked, what was answered, which tools ran and what they returned, whether or not it is still in the model's window. Page older with before set to the oldest seq the last answer returned. The session is the one the turn runs for unless named.")
  ("session_id" :string :doc "the session to read; trove_sessions lists them")
  ("match" :string :doc "only rows whose body contains this fragment; % matches anything and _ one character")
  ("before" :integer :doc "page older: only rows before this seq")
  ("limit" :integer :doc "how many rows (the section's limit unless given)")
  ("clip" :integer :doc "characters of body one row shows (the section's clip unless given)"))

(nlk:define-contract entry-call
  (:doc "One entry of the log, body whole, by the seq a trove_search or trove_read row cited. The entry stays readable whatever has since left the window.")
  ("seq" :integer :required t :doc "the seq a row cited"))

(nlk:define-contract sessions-call
  (:doc "The session directory: id, message count, last activity, title and folder of each session, newest activity first. Use an id with trove_read.")
  ("limit" :integer :doc "how many sessions (the section's sessions unless given)"))

(nlk:define-contract evidence-call
  (:doc "Compile the rows one query matched into a single cited digest: duplicates dropped, newest first, cut to a character budget so the answer fits a context window. Every block cites [session seq] for trove_entry.")
  ("query" :text :required t :doc "the words to look for")
  ("budget" :integer :doc "characters the digest keeps (the section's budget unless given)")
  ("limit" :integer :doc "how many rows to consider (the section's limit unless given)"))

(nlk:define-contract status-call
  (:doc "What Trove reads: how many sessions and durable events the local log holds, where it is, and the limits this cell answers under.")
  ("detail" :boolean :default nil :doc "reserved; the status line is the same in either case"))

(nle:define-cell trove
  (:section ("trove")
    (:guide "nothing needs a key: every answer reads the durable log this image already keeps; limit is how many rows one answer shows, clip how much of each, budget how much an evidence pack keeps, sessions how many the directory lists; next step: ask for trove_status")
    ("limit" :integer :default 20 :min 1 :doc "rows one search or read answers")
    ("clip" :integer :default 200 :min 40 :doc "characters of one row's body")
    ("budget" :integer :default 4000 :min 200 :doc "characters one evidence pack keeps")
    ("sessions" :integer :default 12 :min 1 :doc "sessions the directory lists"))
  (:tool "trove_search" (nlk:contract 'search-call) 'tool-search)
  (:tool "trove_read" (nlk:contract 'read-call) 'tool-read)
  (:tool "trove_entry" (nlk:contract 'entry-call) 'tool-entry)
  (:tool "trove_sessions" (nlk:contract 'sessions-call) 'tool-sessions)
  (:tool "trove_evidence" (nlk:contract 'evidence-call) 'tool-evidence)
  (:tool "trove_status" (nlk:contract 'status-call) 'tool-status)
  (:help :trove "trove: never lose a message - search, read and cite every session's log" *primer*)
  (:command "trove" 'run-slash
            :catches 'trove-error
            :description "Search every session's log, or read one"
            :argument-hint "QUERY | read SESSION"))
