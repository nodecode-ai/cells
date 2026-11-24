;;;; cell.lisp --- a session's house files, fingerprinted, and the receipt when one moves.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of noprijbi99-dotcom/moment (see NOTICE). A session's standing
;;;; material is read at its start and quoted back in the conversation; when a
;;;; file behind it changes under the running session -- an edit from another
;;;; shell, a memory written from elsewhere, the operator rewriting the rules --
;;;; the model keeps answering from the copy it read. This cell fingerprints the
;;;; files at a session's first round, re-checks them every round after, and
;;;; when one moved it puts a receipt in that very round's request: the file,
;;;; both timestamps, the lines that changed, and the text as it stands now.
;;;;
;;;; One change is reported once: the baseline advances to the new bytes, so a
;;;; second change is a second receipt. The moment_check tool answers the same
;;;; question on demand, and /moment does for the operator.
;;;;
;;;; State (session baselines and each receipt's time) lives in the JSON file
;;;; the section names, and nothing else is written anywhere.

(defpackage #:nodecode-moment
  (:use #:cl)
  (:nicknames #:moment))

(in-package #:nodecode-moment)

(nlk:define-peripheral moment :not-running t)

;;; --- the words ---------------------------------------------------------------

(defparameter *marker*
  "[moment] DRIFT RECEIPT — ~a changed mid-session."
  "The line a receipt opens on: the model reads what follows as the state of a
file it has already met, not as a new instruction from the operator.")

(defparameter *instruction*
  "Instruction: prefer the state above over the copy you read earlier in this session for anything it covers. If it is relevant to your answer, say plainly that this file changed while the session was running and what changed; never quote the earlier copy as current. If you read the file again, use exactly the path above."
  "The closing line of every receipt. The original carried one too, in the same
spirit; it is the whole point of the receipt that the model act on it.")

(defparameter *house-files* '("SOUL.md" "AGENTS.md" "SYSTEM.md")
  "The files every session stands on, watched when the section names no others:
the soul a channel reads, the rules every request reads, and the prompt a turn
opens under. Each is the original's counterpart of a Hermes house file.")

(defparameter +primer+
  "The files a session stands on can change while it runs: SOUL.md, AGENTS.md,
SYSTEM.md under the home -- and whatever moment.watch adds -- are fingerprinted
at the session's first round. When one moves, the next round of the session
carries a drift receipt: which file, when the baseline was taken, when it was
detected, the lines that changed, and the text as it stands now, so answer from
that and say plainly that the file drifted. Nothing changed means nothing is
said; an unreadable or missing file is drift like any other, and one reported
change is not reported again until the next change. moment_check answers the
same question on demand -- status per file against this session's start, the
receipts recorded this session, and each file's current fingerprint. The
operator types /moment for the report and /moment reset to take the baseline
again from the files as they are now. state_file must be set before either
answers."
  "What (help :moment) answers while the cell runs.")

;;; --- where the state and the files are ----------------------------------------

(defun state-configured-p ()
  "T when the section says where moment's state lives."
  (let ((name (ignore-errors (setting :state-file))))
    (and (stringp name) (plusp (length (nlk:trimmed name))))))

(defun state-path ()
  "The state file the section names, or a refusal saying which setting to fill."
  (let ((name (and (state-configured-p) (nlk:trimmed (setting :state-file)))))
    (unless name
      (fail "moment.state_file is not set: name the file moment keeps its session baselines in, e.g. moment/state.json under the home, in /settings (or the moment section of config.jsonc)"))
    (if (or (uiop:string-prefix-p "/" name) (uiop:string-prefix-p "~/" name))
        (nlk:expand-home name)
        (nlk:home name))))

(defun watched-path (text)
  "TEXT, one watch entry, as a pathname: `~/x' and `/x' as written, anything
else relative to the home."
  (if (or (uiop:string-prefix-p "/" text) (uiop:string-prefix-p "~/" text))
      (nlk:expand-home text)
      (nlk:home text)))

(defun expand-watch (text)
  "The files one watch entry names, as pathnames: a folder answers the .lisp
definitions in it, so a knowledge cell of memories is watched as one entry."
  (let ((path (watched-path text)))
    (if (uiop:directory-exists-p path)
        (sort (directory (merge-pathnames "*.lisp" (uiop:ensure-directory-pathname path))) #'string<)
        (list path))))

(defun watched-files ()
  "Every file this session's drift is judged on, as native namestrings, sorted:
the section's watch entries when it has any, the three house files otherwise."
  (let ((entries (loop for entry in (or (setting :watch) '())
                        for text = (nlk:trimmed (princ-to-string entry))
                        when (plusp (length text))
                          collect text)))
    (sort (remove-duplicates
           (mapcar #'sb-ext:native-namestring
                   (if entries
                       (loop for entry in entries append (expand-watch entry))
                       (mapcar (lambda (name) (nlk:home name)) *house-files*)))
           :test #'string=)
          #'string<)))

(defun short-name (path)
  "PATH as a receipt and a report name it: the file's own name."
  (file-namestring path))

;;; --- fingerprinting -------------------------------------------------------------

(defun cap-text (text)
  "TEXT cut to the section's max_chars: what a receipt and the state file carry."
  (let ((room (setting :max-chars)))
    (if (> (length text) room) (subseq text 0 room) text)))

(defun fingerprint (path)
  "=> (values PRESENT SHA TEXT AT): the file at PATH as it is now. SHA is taken
over the whole text even when TEXT is cut for storage, so a change past the cap
is still a change."
  (let ((present (probe-file path)))
    (cond ((null present) (values nil nil nil (nlk:unix-now)))
          (t (let ((text (nlk:read-text path)))
               (if (null text)
                   (values t nil nil (nlk:unix-now))
                   (values t (subseq (nlk:sha256-text text) 0 16) (cap-text text)
                           (nlk:unix-now))))))))

(defun snapshot ()
  "Every watched file fingerprinted now, as a table of path -> its record."
  (let ((files (make-hash-table :test #'equal)))
    (dolist (path (watched-files) files)
      (multiple-value-bind (present sha text at) (fingerprint path)
        (setf (gethash path files)
              (list :exists present :sha sha :text text :at at))))))

(defun same-p (base current)
  "Whether BASE and CURRENT describe the same bytes, or the same absence."
  (and base
       (eq (and (getf base :exists) t) (and (getf current :exists) t))
       (equal (getf base :sha) (getf current :sha))))

;;; --- the diff a receipt quotes ----------------------------------------------------

(defun common-prefix (older newer &aux (limit (min (length older) (length newer))))
  "How many lines OLDER and NEWER open with, in common."
  (let ((count 0))
    (loop while (and (< count limit) (string= (nth count older) (nth count newer)))
          do (incf count))
    count))

(defun common-suffix (older newer &aux (limit (- (min (length older) (length newer))
                                                (common-prefix older newer))))
  "How many lines OLDER and NEWER end with, in common, past their shared head."
  (let ((count 0))
    (loop while (and (< count limit)
                     (string= (nth (- (length older) 1 count) older)
                              (nth (- (length newer) 1 count) newer)))
          do (incf count))
    count))

(defun changed-lines (older newer)
  "OLD= and NEW= text as the lines that differ, oldest first, each capped:
removals as -line and additions as +line. The shared head and tail are cut
away first, so an edit in the middle of a file reads as that edit."
  (let* ((older (and older (uiop:split-string older :separator '(#\Newline))))
         (newer (and newer (uiop:split-string newer :separator '(#\Newline))))
         (head (common-prefix older newer))
         (tail (common-suffix older newer))
         (gone (subseq older head (max head (- (length older) tail))))
         (came (subseq newer head (max head (- (length newer) tail)))))
    (append (mapcar (lambda (line) (concatenate 'string "-" (cap-line line))) gone)
            (mapcar (lambda (line) (concatenate 'string "+" (cap-line line))) came))))

(defparameter *line-chars* 160
  "The most of one changed line a receipt quotes.")

(defun cap-line (line)
  "LINE cut to *line-chars*."
  (if (> (length line) *line-chars*) (subseq line 0 *line-chars*) line))

(defparameter *shown-lines* 10
  "How many changed lines one receipt quotes; the rest is summed in a line.")

;;; --- the receipt -----------------------------------------------------------------

(defun receipt (path base current baseline-at)
  "The text one changed file is reported as: what it is, when it moved, the
lines that changed, and the text as it stands now."
  (let* ((name (short-name path))
         (created (or (null base) (not (getf base :exists))))
         (gone (not (getf current :exists)))
         (lines (list (format nil *marker* name)
                      (format nil "Path: ~a" path)
                      (format nil "Session baseline: ~a · Detected: ~a"
                              (nlk:iso-time (or baseline-at 0) :millis nil)
                              (nlk:iso-time (getf current :at) :millis nil)))))
    (cond (created (push "The file did not exist when this session started; it exists now." lines))
          (gone (push "The file existed when this session started; it is gone now." lines))
          (t (let ((changed (changed-lines (getf base :text) (getf current :text))))
               (if (null changed)
                   (push "Content changed beyond the text kept for a line diff." lines)
                   (progn
                     (push "Changed lines (session start -> now):" lines)
                     (dolist (line (subseq changed 0 (min *shown-lines* (length changed))))
                       (push (concatenate 'string "  " line) lines))
                     (when (> (length changed) *shown-lines*)
                       (push (format nil "  ... and ~d more changed line(s)."
                                     (- (length changed) *shown-lines*))
                             lines)))))))
    (when (and (getf current :text) (plusp (length (getf current :text))))
      (push (format nil "Current content of ~a:" name) lines)
      (push (getf current :text) lines))
    (push *instruction* lines)
    (format nil "~{~a~^~%~}" (nreverse lines))))

;;; --- the state file --------------------------------------------------------------

(defun blank-state ()
  "No session has a baseline yet."
  (make-hash-table :test #'equal))

(defun decode-files (object)
  "One JSON object of path -> record as the table this file keeps it in."
  (let ((files (make-hash-table :test #'equal)))
    (loop for path being the hash-keys of (nlk:json-value object :object)
            using (hash-value value)
          do (setf (gethash path files)
                   (list :exists (nlk:json-value value :boolean "exists")
                         :sha (nlk:json-value value :string "sha")
                         :text (nlk:json-value value :string "text")
                         :at (or (nlk:json-value value :integer "at") 0))))
    files))

(defun decode-session (entry)
  "One session's ENTRY as the plist this file keeps it in."
  (let* ((files (decode-files (nlk:json-value entry :object "files")))
         (start (nlk:json-value entry :object "start")))
    (list :at (or (nlk:json-value entry :integer "at") 0)
          :files files
          :start (if (hash-table-p start) (decode-files start) (copy-files files))
          :receipts (loop for row across (nlk:json-array entry "receipts")
                          collect (list :file (or (nlk:json-value row :string "file") "")
                                        :at (or (nlk:json-value row :integer "at") 0))))))

(defun decode-state (object)
  "The whole decoded state OBJECT as a table of session id -> its record."
  (let ((sessions (make-hash-table :test #'equal))
        (table (nlk:json-value object :object "sessions")))
    (when (hash-table-p table)
      (loop for id being the hash-keys of table
              using (hash-value entry)
            do (setf (gethash id sessions) (decode-session entry))))
    sessions))

(defun prune-state (sessions)
  "SESSIONS with the baselines older than keep_days dropped, then only the
keep_sessions most recent kept. A baseline whose age cannot be read is kept
while there is room, so pruning never loses one it cannot date while a fresher
one exists."
  (let ((cutoff (- (nlk:unix-now) (* 86400 (setting :keep-days))))
        (room (setting :keep-sessions))
        (dated '())
        (undated '()))
    (maphash (lambda (id session)
               (let ((at (getf session :at)))
                 (if (or (not (integerp at)) (<= at 0))
                     (push (cons id session) undated)
                     (when (>= at cutoff)
                       (push (list at id session) dated)))))
             sessions)
    (let ((kept (blank-state)))
      (dolist (row (sort dated #'> :key #'first))
        (when (plusp room)
          (setf (gethash (second row) kept) (third row))
          (decf room)))
      (dolist (row undated)
        (when (plusp room)
          (setf (gethash (car row) kept) (cdr row))
          (decf room)))
      kept)))

(defun load-state ()
  "Every session's baseline, pruned. An absent, unreadable or foreign state
file is no baselines -- never a refusal on the turn path."
  (let ((path (ignore-errors (state-path))))
    (if (null path)
        (blank-state)
        (let ((object (and (probe-file path) (ignore-errors (nlk:decode-json (nlk:read-text path))))))
          (if (hash-table-p object)
              (prune-state (decode-state object))
              (blank-state))))))

(defun encode-files (files)
  "FILES, a table of path -> record, as the JSON object the state file keeps."
  (let ((object (make-hash-table :test #'equal)))
    (maphash (lambda (path record)
               (setf (gethash path object)
                     (nlk:make-json-object "exists" (and (getf record :exists) t)
                                            "sha" (getf record :sha)
                                            "text" (getf record :text)
                                            "at" (or (getf record :at) 0))))
             files)
    object))

(defun encode-state (sessions)
  "Every session's baseline as the JSON object the state file keeps."
  (let ((object (make-hash-table :test #'equal)))
    (maphash (lambda (id session)
               (setf (gethash id object)
                     (nlk:make-json-object
                      "at" (or (getf session :at) 0)
                      "files" (encode-files (or (getf session :files) (blank-state)))
                      "start" (encode-files (or (getf session :start) (blank-state)))
                      "receipts" (apply #'vector
                                        (loop for row in (getf session :receipts)
                                              collect (nlk:make-json-object
                                                       "file" (getf row :file)
                                                       "at" (or (getf row :at) 0)))))))
             sessions)
    (nlk:make-json-object "format" 1 "sessions" object)))

(defun save-state (sessions)
  "SESSIONS to the state file, atomically: tmp, then rename. Best effort -- a
read-only folder must never break the turn it was called on."
  (handler-case
      (let* ((path (state-path))
             (tmp (uiop:parse-native-namestring (concatenate 'string (namestring path) ".tmp"))))
        (ensure-directories-exist path)
        (with-open-file (out tmp :direction :output :if-exists :supersede
                                 :external-format :utf-8)
          (write-string (nlk:encode-json-object (encode-state sessions)) out))
        (rename-file tmp path)
        path)
    (error (condition)
      (declare (ignore condition))
      nil)))

(defun newest-session (sessions)
  "The id of the session whose baseline is the most recent, or NIL."
  (let ((rows '()))
    (maphash (lambda (id session) (push (cons (or (getf session :at) 0) id) rows)) sessions)
    (when rows (cdr (first (sort rows #'> :key #'car))))))

;;; --- one round's check ------------------------------------------------------------

(defun observe (session)
  "Fingerprint every watched file for SESSION and answer the receipt text when
one moved since that session's baseline, NIL when none did. The first call for
a session takes the baseline and stays silent."
  (let* ((sessions (load-state))
         (entry (gethash session sessions))
         (current (snapshot)))
    (if (null entry)
        (progn
          (setf (gethash session sessions)
                (list :at (nlk:unix-now) :files current :start (copy-files current) :receipts '()))
          (save-state sessions)
          nil)
        (let ((baseline (getf entry :files))
              (moved '()))
          (maphash (lambda (path now)
                     (let ((base (gethash path baseline)))
                       (unless (same-p base now)
                         (push (list path base now) moved)
                         (setf (gethash path baseline) (copy-list now)))))
                   current)
          (when moved
            (let ((texts '()))
              (dolist (row (sort moved #'string< :key #'first))
                (destructuring-bind (path base now) row
                  (push (receipt path base now (getf entry :at)) texts)
                  (setf (getf entry :receipts)
                        (append (getf entry :receipts)
                                (list (list :file (short-name path) :at (getf now :at)))))))
              (setf (gethash session sessions) entry)
              (save-state sessions)
              (format nil "~{~a~^~%~%~}" (nreverse texts))))))))

(defun copy-files (files)
  "A fresh table holding FILES' records, so the advancing baseline and the
session-start baseline are never the same conses."
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (path record) (setf (gethash path copy) (copy-list record))) files)
    copy))

;;; --- the report ------------------------------------------------------------------

(defun entry-status (base current)
  "The one line a report gives one file against its session-start baseline."
  (cond ((null base) "not in baseline")
        ((same-p base current) "unchanged since session start")
        (t "CHANGED since session start")))

(defun check-entries (session only)
  "=> (values ENTRIES BASELINE-AT SESSION): one plist per watched file, each
with its status against SESSION's start, and the id whose baseline was read."
  (let* ((sessions (load-state))
         (session (or session (newest-session sessions)))
         (entry (and session (gethash session sessions)))
         (start (and entry (or (getf entry :start) (getf entry :files))))
         (entries '()))
    (maphash (lambda (path now)
               (when (or (null only)
                         (search (string-downcase only) (string-downcase path)))
                 (let ((base (and start (gethash path start))))
                   (push (list :path path
                               :name (short-name path)
                               :exists (and (getf now :exists) t)
                               :sha (getf now :sha)
                               :status (if entry (entry-status base now) "no baseline for this session yet")
                               :changed (and base (not (same-p base now))
                                             (changed-lines (getf base :text) (getf now :text))))
                         entries))))
             (snapshot))
    (values (sort entries #'string< :key (lambda (row) (getf row :path)))
            (and entry (getf entry :at))
            session)))

(defun report-json (entries baseline-at session)
  "The report as the JSON the model reads: per-file status and fingerprint,
plus the baseline time and the receipts this session recorded."
  (let ((files (make-hash-table :test #'equal)))
    (dolist (row entries)
      (setf (gethash (getf row :path) files)
            (nlk:make-json-object
             "exists" (and (getf row :exists) t)
             "sha256_16" (getf row :sha)
             "path" (getf row :path)
             "status" (getf row :status)
             "changed_lines" (apply #'vector (subseq (or (getf row :changed) '())
                                                      0 (min *shown-lines*
                                                             (length (or (getf row :changed) '()))))))))
    (let* ((sessions (load-state))
           (entry (and session (gethash session sessions))))
      (nlk:encode-json-object
       (nlk:make-json-object
        "tool" "moment_check"
        "checked_at" (nlk:iso-time (nlk:unix-now))
        "session" (or session "unknown")
        "session_baseline_at" (and baseline-at (nlk:iso-time baseline-at :millis nil))
        "files" files
        "drift_receipts_this_session" (apply #'vector
                                              (loop for row in (and entry (getf entry :receipts))
                                                    collect (nlk:make-json-object
                                                             "file" (getf row :file)
                                                             "at" (nlk:iso-time (getf row :at) :millis nil)))))))))

(defun report-text (entries baseline-at session)
  "The report as the operator reads it: one line a file."
  (with-output-to-string (out)
    (format out "moment: the files this session stands on, against ~a~%"
            (if baseline-at
                (format nil "the baseline taken ~a" (nlk:iso-time baseline-at :millis nil))
                "no baseline yet"))
    (format out "session: ~a~%" (or session "unknown"))
    (if (null entries)
        (format out "No watched file. moment.watch is empty and the home's ~{~a~^, ~} are not there.~%"
                *house-files*)
        (dolist (row entries)
          (format out "  ~a  ~a~%" (getf row :status) (getf row :name))
          (dolist (line (subseq (or (getf row :changed) '())
                                 0 (min *shown-lines* (length (or (getf row :changed) '())))))
            (format out "      ~a~%" line))))
    (format out "~a" +primer+)))

;;; --- the model's way in ------------------------------------------------------------

(nlk:define-contract check-call
  (:doc "Check whether the files this session stands on moved under it: each one's status against the session's starting state (unchanged, CHANGED, created, deleted or no baseline), the lines that changed, and its current fingerprint. Use it when the operator asks whether your standing material is still what you read, or before relying on a remembered fact in a long session.")
  ("file" :text
          :doc "limit the report to the watched file whose path contains this text, e.g. SOUL.md; omit it for every watched file"))

(defun check-tool (call)
  "The moment_check tool: the report for the calling session, or for the most
recent baseline when the call comes from outside a turn."
  (running-settings)
  (state-path)
  (multiple-value-bind (entries baseline-at session) (check-entries (calling-session) (check-call-file call))
    (report-json entries baseline-at session)))

(defun calling-session ()
  "The session whose turn is calling, or NIL outside a turn."
  (or nlk:*scribe-session-id*
      (ignore-errors (nle::eval-session-id))))

;;; --- the operator's way in ------------------------------------------------------------

(defun run-slash (args session-id)
  "One /moment invocation: no argument reports, 'reset' takes the baseline
again from the files as they are now."
  (running-settings)
  (state-path)
  (let ((args (nlk:trimmed (or args ""))))
    (cond ((string-equal args "reset")
           (let ((session (or session-id (calling-session)
                               (fail "resetting a baseline needs a session: run /moment reset inside a session's turn"))))
             (let ((sessions (load-state))
                   (current (snapshot)))
               (setf (gethash session sessions)
                     (list :at (nlk:unix-now) :files current :start (copy-files current) :receipts '()))
               (save-state sessions)
               (format nil "moment: baseline taken again from ~d watched file~:p as they are now; drift is measured from here."
                       (hash-table-count current)))))
          (t (multiple-value-bind (entries baseline-at session) (check-entries (or session-id (calling-session)) nil)
               (report-text entries baseline-at session))))))

;;; --- every round ------------------------------------------------------------------------

(defun deliver (op next)
  "The :round point: every watched file is checked against the session's
baseline, and a changed one is put into the request about to be built."
  (prog1 (funcall next op)
    (handler-case
        (let ((session (getf op :session-id))
              (turn (getf op :turn)))
          (when (and session turn (nlk:store-open-p) (state-configured-p))
            (let ((text (observe session)))
              (when text
                (nlk:record-turn-context-prompt turn :text text)))))
      (error (condition) (warn "moment: ~a" condition)))))

;;; --- the declaration ---------------------------------------------------------------------

(nle:define-cell moment
  (:section ("moment")
    (:guide "state_file is where moment keeps its per-session fingerprints (moment/state.json under the home is the usual answer); watch adds the files, or folders of .lisp definitions, whose drift is reported on top of the home's SOUL.md, AGENTS.md and SYSTEM.md; max_chars caps how much of a changed file one receipt carries; keep_sessions and keep_days bound the state file; - /cells shows whether the folder started")
    ("state_file" :path :required t
     :doc "the file moment keeps its session baselines in, e.g. moment/state.json under the home; the tool and /moment refuse until it is set")
    ("watch" :list
     :doc "files, or folders of .lisp definitions, whose drift is reported besides the home's SOUL.md, AGENTS.md and SYSTEM.md")
    ("max_chars" :integer :default 2000 :min 200
     :doc "the most of a changed file one receipt carries, in characters")
    ("keep_sessions" :integer :default 50 :min 1
     :doc "how many sessions' baselines the state file keeps")
    ("keep_days" :integer :default 30 :min 1
     :doc "how old a session's baseline may be before it is dropped"))
  (:tool "moment_check" (nlk:contract 'check-call) 'check-tool)
  (:hook :round #'deliver)
  (:command "moment" 'run-slash
            :catches 'moment-error
            :description "Whether the files this session stands on moved under it; reset takes the baseline again"
            :argument-hint "reset")
  (:help :moment "moment: reports when a house file changed under the running session and hands the model the new text; /moment" +primer+))
