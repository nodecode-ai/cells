;;;; backlog.lisp --- what went wrong lately, read at $0 from records every install keeps.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The backlog is a cycle's raw material: things that failed, read from
;;;; records every install keeps, grouped so one fault seen ten times is one
;;;; item counted ten, and sorted so the most frequent comes first --
;;;; recurrence, not age, because an old one-off rarely reproduces.
;;;;
;;;;   :TURN-FAILED   turn.failed facts in the event log, by the condition's
;;;;                  type and its rendering, with numbers, ids, paths and
;;;;                  object printouts stripped so one fault in two sessions
;;;;                  groups as one
;;;;   :HARM          experience sightings of kind harm -- a definition that
;;;;                  was followed and did damage -- by the definition's name
;;;;   :VERIFICATION  experience verifications that failed or were blocked,
;;;;                  by their target
;;;;   :QA            report_issue notes, by tool and symptom, when the qa
;;;;                  cell is loaded
;;;;
;;;; Nothing here names a machine, a repository or a channel. A source that
;;;; does not apply answers NIL. A source that cannot read signals, and
;;;; BACKLOG returns it as unread beside the items, never as an empty source:
;;;; an unknown is not a none.
;;;;
;;;; Each source is a reader over the records and a pure ITEMS function over
;;;; what the reader returns; the tests drive the pure halves with data of
;;;; those shapes. What becomes of an item over cycles -- attempts counted,
;;;; dropped items left out -- is the ledger's (ledger.lisp): this file only
;;;; reads, groups and counts.

(in-package #:nodecode-selffix)

(defparameter *backlog-days* 14
  "How far back the backlog reads, in days.")

(defparameter *evidence-cap* 3
  "How many of an item's newest occurrences it carries as evidence.")

(defvar *excluded-session-p* 'nodecode-cron::wake-session-p
  "A predicate on a session id: true for a session whose failures are not
backlog. The cron wake's own, by default: the cycle runs on that wake, so a
cycle that fails never feeds the next one its own failure. Forks are left
out besides.")

;;; --- normalizing ------------------------------------------------------------

(defparameter +normalizations+
  '(("#<[^>]*>" . "#<..>")
    ("(?<![\\w.])(?:~|\\.\\.?)?/(?:[\\w.@%+-]+/?)+" . "PATH")
    ("\\b[a-z][a-z0-9]*(?:-[0-9A-Z]{6,})+\\b" . "ID")
    ("(?i)\\b[0-9a-f]{8,}\\b" . "N")
    ("\\d+(?:\\.\\d+)?" . "N")
    ("\\s+" . " "))
  "What differs between two sightings of one fault, in the order it is
replaced: object printouts, paths, durable ids, hex runs, numbers, spacing.")

(defun normalize-text (text &key (cap 160))
  "TEXT on one line with +NORMALIZATIONS+ applied, at most CAP characters."
  (let ((s (or text "")))
    (loop for (pattern . replacement) in +normalizations+
          do (setf s (ppcre:regex-replace-all pattern s replacement)))
    (setf s (string-trim " " s))
    (if (> (length s) cap) (subseq s 0 cap) s)))

;;; --- occurrences and items --------------------------------------------------

(defun occurrence (source key title &key at session turn text)
  "One sighting of the fault KEY names, from SOURCE."
  (list :source source :key key :title title
        :at (or at "") :session session :turn turn :text text))

(defun group-occurrences (occurrences)
  "OCCURRENCES grouped by key into items, most frequent first, the newest
first among equals. An item is (:SOURCE :KEY :TITLE :COUNT :FIRST :LAST
:EVIDENCE), its evidence the newest *EVIDENCE-CAP* occurrences as
(:AT :SESSION :TURN :TEXT), newest first."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (o occurrences)
      (push o (gethash (getf o :key) groups)))
    (let ((items
            (loop for key being the hash-keys of groups using (hash-value seen)
                  for newest-first = (sort (copy-list seen) #'string> :key (lambda (o) (getf o :at)))
                  for newest = (first newest-first)
                  collect (list :source (getf newest :source) :key key
                                :title (getf newest :title)
                                :count (length seen)
                                :first (getf (car (last newest-first)) :at)
                                :last (getf newest :at)
                                :evidence (loop for o in newest-first
                                                repeat *evidence-cap*
                                                collect (list :at (getf o :at) :session (getf o :session)
                                                              :turn (getf o :turn) :text (getf o :text)))))))
      (sort items (lambda (a b)
                    (or (> (getf a :count) (getf b :count))
                        (and (= (getf a :count) (getf b :count))
                             (string> (getf a :last) (getf b :last)))))))))

(defun in-window-p (at since)
  "Whether the ISO stamp AT is at or after the ISO stamp SINCE (NIL: no bound)."
  (or (null since) (and (stringp at) (string>= at since))))

;;; --- the pure halves --------------------------------------------------------

(defun turn-failed-items (rows &key since)
  "Occurrences from ROWS -- (SESSION-ID TURN-ID OCCURRED-AT PAYLOAD-JSON),
the reader's shape -- left out a fork's session and one *EXCLUDED-SESSION-P*
names."
  (loop for (session turn at payload) in rows
        for object = (ignore-errors (nlk:decode-json payload))
        for type = (or (and object (nlk:json-value object :string "condition-type")) "ERROR")
        for detail = (or (and object (nlk:json-value object :string "detail")) "")
        for normal = (normalize-text detail)
        when (and (in-window-p at since)
                  (not (nlk:fork-session-p session))
                  (not (funcall *excluded-session-p* session)))
          collect (occurrence :turn-failed (format nil "turn-failed ~a: ~a" type normal)
                              (format nil "turns failed with ~a: ~a" type normal)
                              :at at :session session :turn turn
                              :text (nlk:one-line detail :cap 300))))

(defun ledger-field (line key)
  (let ((value (gethash key line)))
    (and (stringp value) (plusp (length value)) value)))

(defun harm-items (lines &key since)
  "Occurrences from the experience ledger's LINES (hash tables) of kind harm."
  (loop for line in lines
        for name = (ledger-field line "name")
        when (and name (equal (ledger-field line "kind") "harm")
                  (in-window-p (ledger-field line "at") since))
          collect (occurrence :harm (format nil "harm ~a" name)
                              (format nil "following ~a did harm" name)
                              :at (ledger-field line "at") :session (ledger-field line "session")
                              :turn (ledger-field line "turn")
                              :text (or (ledger-field line "quote") (ledger-field line "note")))))

(defparameter +unmet-statuses+ '("failed" "blocked")
  "Verification statuses that are backlog; passed and skipped are not.")

(defun verification-items (lines &key since)
  "Occurrences from the experience ledger's LINES of kind verification whose
status is in +UNMET-STATUSES+."
  (loop for line in lines
        for name = (ledger-field line "name")
        for status = (ledger-field line "status")
        when (and name (equal (ledger-field line "kind") "verification")
                  (member status +unmet-statuses+ :test #'equal)
                  (in-window-p (ledger-field line "at") since))
          collect (occurrence :verification (format nil "verification ~a" (normalize-text name))
                              (format nil "verification ~a: ~a" status name)
                              :at (ledger-field line "at") :session (ledger-field line "session")
                              :turn (ledger-field line "turn")
                              :text (or (ledger-field line "note") (ledger-field line "quote")))))

(defun qa-items (notes &key since)
  "Occurrences from report_issue NOTES (hash tables), by tool and symptom."
  (loop for note in notes
        for tool = (or (ledger-field note "tool") "?")
        for symptom = (or (ledger-field note "symptom") "other")
        when (in-window-p (ledger-field note "at") since)
          collect (occurrence :qa (format nil "qa ~a ~a" tool symptom)
                              (format nil "~a reported as ~a" tool symptom)
                              :at (ledger-field note "at") :session (ledger-field note "session")
                              :turn (ledger-field note "turn")
                              :text (ledger-field note "note"))))

;;; --- the readers ------------------------------------------------------------

(defun read-turn-failed (since)
  (turn-failed-items
   (nlk:events :kind "turn.failed" :where (list "occurred_at >= ?" since)
               :as :rows :columns '("session_id" "turn_id" "occurred_at" "payload"))
   :since since))

(defun read-harm (since)
  (harm-items (nlk:knowledge-ledger-lines) :since since))

(defun read-verification (since)
  (verification-items (nlk:knowledge-ledger-lines) :since since))

(defun read-qa (since)
  "The qa cell's notes, or NIL when that cell is not loaded."
  (let* ((package (find-package "NODECODE-QA"))
         (reader (and package (find-symbol "READ-NOTES" package))))
    (and reader (fboundp reader) (qa-items (funcall reader) :since since))))

(defparameter *backlog-sources*
  '((:turn-failed . read-turn-failed)
    (:harm . read-harm)
    (:verification . read-verification)
    (:qa . read-qa))
  "Each source and its reader, a function of the window's ISO start that
answers occurrences.")

(defun days-ago-iso (days &optional (now (get-universal-time)))
  (nlk:iso-time (- now (* days 24 60 60) nlk:+unix-epoch+)))

(defun backlog (&key limit (days *backlog-days*) (sources (mapcar #'car *backlog-sources*)))
  "What failed in the last DAYS, as items most frequent first (see
GROUP-OCCURRENCES), at most LIMIT. The second value lists each source that
could not be read as (SOURCE . WHY): unread, not empty."
  (let ((since (days-ago-iso days)) (occurrences '()) (unread '()))
    (loop for (source . reader) in *backlog-sources*
          when (member source sources)
            do (handler-case (setf occurrences (append occurrences (funcall reader since)))
                 (error (condition)
                   (push (cons source (nlk:one-line (princ-to-string condition) :cap 200)) unread))))
    (let ((items (group-occurrences occurrences)))
      (values (if (and limit (< limit (length items))) (subseq items 0 limit) items)
              (nreverse unread)))))
