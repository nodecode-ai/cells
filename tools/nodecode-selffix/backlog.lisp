;;;; backlog.lisp --- what went wrong lately, read at $0 from records every install keeps.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The backlog is a cycle's raw material: things that failed, grouped so one
;;;; fault seen ten times is one item counted ten, and sorted so the most
;;;; frequent comes first -- recurrence, not age, because an old one-off
;;;; rarely reproduces.
;;;;
;;;; A failure in this image is a condition, so the backlog's first source is
;;;; the condition itself, caught live and kept as a readable form:
;;;;
;;;;   :CASE          <selffix>/cases/<fp>.lisp, one plist per fault, written
;;;;                  by the capture (capture.lisp): (:TYPE :FUNCTION :ARGS
;;;;                  :COUNT :FIRST :LAST :GENERATION :TURN). Grouped by the
;;;;                  exact pair (TYPE . FUNCTION), symbols compared as
;;;;                  symbols -- no message text is parsed. A case whose ARGS
;;;;                  read back as a list is FIXABLE: a repro form can call
;;;;                  FUNCTION with them. One whose ARGS are :UNREADABLE is a
;;;;                  note.
;;;;
;;;; Everything else is a note -- counted, reported, never a fix candidate,
;;;; because there is no call to replay:
;;;;
;;;;   :TURN-FAILED   turn.failed facts no case covers (a provider refusal, a
;;;;                  failure before the capture was on), by condition type
;;;;   :HARM          experience sightings of kind harm, by definition name
;;;;   :VERIFICATION  experience verifications that failed or were blocked
;;;;   :QA            report_issue notes, by tool and symptom
;;;;
;;;; Nothing here names a machine, a repository or a channel. A source that
;;;; does not apply answers NIL. A source that cannot read signals, and
;;;; BACKLOG returns it as unread beside the items, never as an empty source:
;;;; an unknown is not a none. A case file that does not read back -- its
;;;; package is gone after an update -- is named the same way, as unread.
;;;;
;;;; Each source is a reader over the records and a pure ITEMS function over
;;;; what the reader returns; the tests drive the pure halves with data of
;;;; those shapes. What becomes of an item over cycles is the ledger's
;;;; (ledger.lisp): this file only reads, groups and counts.

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

(defun excluded-session-p (session)
  (and session
       (or (nlk:fork-session-p session)
           (funcall *excluded-session-p* session))))

;;; --- normalizing (the ledger's fingerprint of a note) ----------------------

(defparameter +normalizations+
  '(("#<[^>]*>" . "#<..>")
    ("(?<![\\w.])(?:~|\\.\\.?)?/(?:[\\w.@%+-]+/?)+" . "PATH")
    ("\\b[a-z][a-z0-9]*(?:-[0-9A-Z]{6,})+\\b" . "ID")
    ("(?i)\\b[0-9a-f]{8,}\\b" . "N")
    ("\\d+(?:\\.\\d+)?" . "N")
    ("\\s+" . " "))
  "What differs between two sightings of one note, in the order it is
replaced. The backlog itself no longer keys on message text; the ledger's
fingerprint still uses it.")

(defun normalize-text (text &key (cap 160))
  "TEXT on one line with +NORMALIZATIONS+ applied, at most CAP characters."
  (let ((s (or text "")))
    (loop for (pattern . replacement) in +normalizations+
          do (setf s (ppcre:regex-replace-all pattern s replacement)))
    (setf s (string-trim " " s))
    (if (> (length s) cap) (subseq s 0 cap) s)))

;;; --- occurrences and items --------------------------------------------------

(defun occurrence (source key title &key at first session turn text (weight 1) fields)
  "One sighting of the fault KEY names, from SOURCE. WEIGHT is how many
sightings it stands for (a case counts its own), FIRST the oldest of them,
FIELDS a plist the item carries from its newest occurrence."
  (list :source source :key key :title title
        :at (or at "") :first (or first at "") :session session :turn turn :text text
        :weight weight :fields fields))

(defun group-occurrences (occurrences)
  "OCCURRENCES grouped by key into items, most frequent first, the newest
first among equals. An item is (:SOURCE :KEY :TITLE :COUNT :FIRST :LAST
:EVIDENCE :FIXABLE ...), its count the sum of the weights, its evidence the
newest *EVIDENCE-CAP* occurrences as (:AT :SESSION :TURN :TEXT), newest
first, and the newest occurrence's FIELDS after."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (o occurrences)
      (push o (gethash (getf o :key) groups)))
    (let ((items
            (loop for key being the hash-keys of groups using (hash-value seen)
                  for newest-first = (sort (copy-list seen) #'string> :key (lambda (o) (getf o :at)))
                  for newest = (first newest-first)
                  collect (list* :source (getf newest :source) :key key
                                 :title (getf newest :title)
                                 :count (reduce #'+ seen :key (lambda (o) (getf o :weight)))
                                 :first (reduce (lambda (a b) (if (string< b a) b a))
                                                (mapcar (lambda (o) (getf o :first)) seen))
                                 :last (getf newest :at)
                                 :evidence (loop for o in newest-first
                                                 repeat *evidence-cap*
                                                 collect (list :at (getf o :at) :session (getf o :session)
                                                               :turn (getf o :turn) :text (getf o :text)))
                                 (or (getf newest :fields) (list :fixable nil))))))
      (sort items (lambda (a b)
                    (or (> (getf a :count) (getf b :count))
                        (and (= (getf a :count) (getf b :count))
                             (string> (getf a :last) (getf b :last)))))))))

(defun in-window-p (at since)
  "Whether the ISO stamp AT is at or after the ISO stamp SINCE (NIL: no bound)."
  (or (null since) (and (stringp at) (string>= at since))))

;;; --- cases: the condition itself --------------------------------------------

(defun qualified (thing)
  "THING as text that names it whatever *PACKAGE* is: a symbol as PKG::NAME."
  (typecase thing
    (null "-")
    (symbol (let ((package (symbol-package thing)))
              (if package
                  (format nil "~a::~a" (package-name package) (symbol-name thing))
                  (format nil "#:~a" (symbol-name thing)))))
    (t (princ-to-string thing))))

(defun case-condition (case)
  "CASE's grouping key: the exact pair (TYPE . FUNCTION)."
  (cons (getf case :type) (getf case :function)))

(defun case-turns (case)
  "The turns CASE was seen in, newest first: its :TURNS, else its :TURN."
  (or (getf case :turns) (let ((turn (getf case :turn))) (and turn (list turn)))))

(defun case-items (cases &key since)
  "Occurrences from CASES -- (PATH . PLIST), the reader's shape -- one per
case, weighted by its count, keyed by the exact (TYPE . FUNCTION); a case
last seen before SINCE, or in an excluded session, left out."
  (loop for (path . case) in cases
        for (type . function) = (case-condition case)
        for args = (getf case :args)
        for fixable = (and function (listp args))
        when (and type
                  (in-window-p (getf case :last) since)
                  (not (excluded-session-p (getf case :session))))
          collect (occurrence :case (format nil "case ~a ~a" (qualified type) (qualified function))
                              (format nil "~a signalled in ~a~:[ (its arguments did not print readably)~;~]"
                                      (qualified type) (qualified function) fixable)
                              :at (getf case :last) :first (getf case :first)
                              :session (getf case :session) :turn (first (case-turns case))
                              :text (format nil "~a in ~a" (qualified type) (qualified function))
                              :weight (max 1 (or (getf case :count) 1))
                              :fields (list :fixable fixable
                                            :condition (case-condition case)
                                            :args args
                                            :generation (getf case :generation)
                                            :turns (case-turns case)
                                            :file path))))

(defun read-case-file (path)
  "The plist PATH holds, read with *READ-EVAL* off in standard syntax. A
symbol whose package is gone signals, as any reader error does."
  (with-open-file (in path :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (*package* (find-package '#:keyword)))
        (let ((form (read in)))
          (unless (and (listp form) (keywordp (first form)))
            (error "not a case plist: ~a" (nlk:one-line (prin1-to-string form) :cap 80)))
          form)))))

(defun read-case-files ()
  "Every case under cases/ as (PATH . PLIST); the second value, each file
that did not read back as (PATH . WHY)."
  (let ((cases '()) (unread '()))
    (dolist (path (directory (selffix-path "cases/*.lisp")))
      (handler-case (push (cons path (read-case-file path)) cases)
        (error (condition)
          (push (cons path (nlk:one-line (princ-to-string condition) :cap 200)) unread))))
    (values (nreverse cases) (nreverse unread))))

(defun read-cases (since)
  (multiple-value-bind (cases unread) (read-case-files)
    (values (case-items cases :since since)
            (loop for (path . why) in unread
                  collect (format nil "~a: ~a" (file-namestring path) why)))))

;;; --- notes: the pure halves -------------------------------------------------

(defun turn-failed-items (rows &key since)
  "Occurrences from ROWS -- (SESSION-ID TURN-ID OCCURRED-AT PAYLOAD-JSON),
the reader's shape -- keyed by the condition's type, left out a fork's
session and one *EXCLUDED-SESSION-P* names."
  (loop for (session turn at payload) in rows
        for object = (ignore-errors (nlk:decode-json payload))
        for type = (or (and object (nlk:json-value object :string "condition-type")) "ERROR")
        for detail = (or (and object (nlk:json-value object :string "detail")) "")
        when (and (in-window-p at since) (not (excluded-session-p session)))
          collect (occurrence :turn-failed (format nil "turn-failed ~a" type)
                              (format nil "turns failed with ~a" type)
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
          collect (occurrence :verification (format nil "verification ~a" name)
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
  '((:case . read-cases)
    (:turn-failed . read-turn-failed)
    (:harm . read-harm)
    (:verification . read-verification)
    (:qa . read-qa))
  "Each source and its reader, a function of the window's ISO start that
answers occurrences, and as a second value what of it could not be read.")

(defun days-ago-iso (days &optional (now (get-universal-time)))
  (nlk:iso-time (- now (* days 24 60 60) nlk:+unix-epoch+)))

(defun covered-by-a-case (occurrences)
  "OCCURRENCES less a turn-failed one whose turn a case was seen in: one
failure is counted once, as its case."
  (let ((turns (make-hash-table :test #'equal)))
    (dolist (o occurrences)
      (when (eq (getf o :source) :case)
        (dolist (turn (getf (getf o :fields) :turns))
          (setf (gethash turn turns) t))))
    (remove-if (lambda (o) (and (eq (getf o :source) :turn-failed)
                                (gethash (getf o :turn) turns)))
               occurrences)))

(defun backlog (&key limit (days *backlog-days*) (sources (mapcar #'car *backlog-sources*)) fixable)
  "What failed in the last DAYS, as items most frequent first (see
GROUP-OCCURRENCES), at most LIMIT; with FIXABLE, only the items a repro form
can call (cases with readable arguments). The second value lists each source
that could not be read, wholly or in part, as (SOURCE . WHY): unread, not
empty."
  (let ((since (days-ago-iso days)) (occurrences '()) (unread '()))
    (loop for (source . reader) in *backlog-sources*
          when (member source sources)
            do (handler-case
                   (multiple-value-bind (found why) (funcall reader since)
                     (setf occurrences (append occurrences found))
                     (dolist (w why) (push (cons source w) unread)))
                 (error (condition)
                   (push (cons source (nlk:one-line (princ-to-string condition) :cap 200)) unread))))
    (let ((items (group-occurrences (covered-by-a-case occurrences))))
      (when fixable
        (setf items (remove-if-not (lambda (i) (getf i :fixable)) items)))
      (values (if (and limit (< limit (length items))) (subseq items 0 limit) items)
              (nreverse unread)))))
