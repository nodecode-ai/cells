;;;; cell.lisp --- pending memory review, the durable store, /magi.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Magi keeps two things a user of the original Hermes plugin keeps: a
;;;; queue of memory writes waiting for a person's yes, and the durable
;;;; entries those writes land in. The files are the original's: a pending
;;;; record is one JSON object under pending/memory/ (id, created_at,
;;;; subsystem, payload.target, payload.action, payload.content,
;;;; payload.old_text), and the stores are MEMORY.md and USER.md, one entry
;;;; a block, entries separated by a line holding a single section sign.
;;;; Point `store' at a Hermes profile and the queue and stores a Hermes
;;;; install already has are the queue this cell reviews.
;;;;
;;;; Nothing here is automatic: a proposal is staged, read, then approved or
;;;; rejected by name. An approval applies the operations to the store and
;;;; removes the record; a rejection only removes it.

(in-package #:nodecode-magi)

(defparameter +delimiter+ (format nil "~c~c~c" #\Newline (code-char 167) #\Newline)
  "What separates one stored entry from the next, as Hermes' own stores do.")

(defvar *serial* 0 "Bumped so two proposals in one second never share an id.")

;; The condition, FAIL, *MAGI*, RUNNING-SETTINGS and DEFINE-VERB, the way
;; every peripheral declares them.
(nlk:define-peripheral magi :not-running t)

(defun guarded (body)
  "Run BODY; a refusal answers ERROR: MAGI-ERROR: ..., so a tool call reads as
failed and the model sees what to do about it."
  (handler-case (funcall body)
    (magi-error (condition) (nle:failure (format nil "ERROR: MAGI-ERROR: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: MAGI-ERROR: ~a" condition)))))

;;; --- where the store is -------------------------------------------------------------

(defun store-path ()
  "The store folder the section names, or NIL when it names none."
  (let ((configured (ignore-errors (setting :store))))
    (when (and (stringp configured) (plusp (length (nlk:trimmed configured))))
      (uiop:ensure-directory-pathname (nlk:expand-home (nlk:trimmed configured))))))

(defun require-store ()
  "The store folder, or a refusal naming the setting that names it."
  (or (store-path)
      (fail "magi.store is not set: name the folder that holds MEMORY.md, USER.md and pending/memory in /settings")))

(defun target-name (target)
  "The file an entry of TARGET lives in, or NIL when TARGET is not one of the two."
  (cond ((string-equal (or target "") "memory") "MEMORY.md")
        ((string-equal (or target "") "user") "USER.md")
        (t nil)))

(defun target-file (target)
  (or (target-name target) (fail "target must be memory or user, not ~s" target))
  (merge-pathnames (target-name target) (require-store)))

(defun pending-directory ()
  (merge-pathnames "pending/memory/" (require-store)))

;;; --- the stores ---------------------------------------------------------------------

(defun split-entries (text)
  "TEXT as entries: split on the delimiter, trimmed, empties dropped."
  (loop with start = 0
        with out = '()
        for at = (search +delimiter+ text :start2 start)
        do (let ((entry (string-trim '(#\Space #\Newline #\Tab #\Return)
                                     (subseq text start (or at (length text))))))
             (when (plusp (length entry)) (push entry out)))
        while at
        do (setf start (+ at (length +delimiter+)))
        finally (return (nreverse out))))

(defun entries (target)
  "The durable entries of TARGET, in the order the file holds them."
  (let ((path (target-file target)))
    (if (probe-file path) (split-entries (or (nlk:read-text path) "")) '())))

(defun join-strings (strings separator)
  "STRINGS as one string, SEPARATOR between them."
  (with-output-to-string (out)
    (loop for (string . rest) on strings
          do (write-string string out)
             (when rest (write-string separator out)))))

(defun write-entries (target entries)
  "Store ENTRIES as TARGET's whole store, in one atomic write."
  (nlk:write-file-atomically (target-file target)
                              (if entries
                                  (format nil "~a~c" (join-strings entries +delimiter+) #\Newline)
                                  "")))

(defun entries-text (target entries &key (limit 50))
  "ENTRIES numbered, with the store's budget line above them."
  (if (null entries)
      (format nil "~a: no stored entries yet.~@[~%~a~]"
              (string-downcase target) (budget-line target entries))
      (with-output-to-string (out)
        (format out "~a: ~d entries~%~@[~a~%~]"
                (string-downcase target) (length entries) (budget-line target entries))
        (loop for entry in entries for index from 1
              while (<= index limit)
              do (format out "~d. ~a~%" index entry))
        (when (> (length entries) limit)
          (format out "(+~d more)~%" (- (length entries) limit))))))

(defun budget-line (target entries)
  "The store's size against the section's ceiling, or NIL when there is none."
  (let ((used (loop for entry in entries sum (length entry)))
        (limit (ignore-errors (setting :char-limit))))
    (when (and (integerp limit) (plusp limit))
      (format nil "~d/~d characters used" used limit))))

;;; --- operations on the store ---------------------------------------------------------

(defun entry-index (text entries)
  "The index of the one entry TEXT names: the exact entry, else the one entry
carrying TEXT; a refusal when none or more than one does."
  (let ((exact (position text entries :test #'equal)))
    (cond (exact exact)
          (t (let ((hits (loop for entry in entries for index from 0
                               when (search text entry) collect index)))
               (cond ((= 1 (length hits)) (first hits))
                     ((null hits) (fail "no stored entry matches ~s" (gist text 60)))
                     (t (fail "~d stored entries match ~s; give the whole entry"
                               (length hits) (gist text 60)))))))))

(defun apply-operation (entries operation)
  "ENTRIES with OPERATION applied, a plist of :action :content :old-text."
  (let ((action (getf operation :action))
        (content (getf operation :content))
        (old (getf operation :old-text)))
    (cond
      ((equal action "add")
       (when (member content entries :test #'equal)
         (fail "that entry is already stored"))
       (append entries (list content)))
      ((equal action "replace")
       (let ((index (entry-index old entries)))
         (append (subseq entries 0 index) (list content) (subseq entries (1+ index)))))
      ((equal action "remove")
       (let ((index (entry-index old entries)))
         (append (subseq entries 0 index) (subseq entries (1+ index)))))
      (t (fail "action must be add, replace or remove, not ~s" action)))))

(defun operations-problems (operation &key (index 0))
  "What makes OPERATION unapplyable, as a list of sentences."
  (let ((action (getf operation :action))
        (content (getf operation :content))
        (old (getf operation :old-text)))
    (cond
      ((not (member action '("add" "replace" "remove") :test #'equal))
       (list (format nil "operation ~d: action must be add, replace or remove" index)))
      ((equal action "add")
       (if (and (stringp content) (plusp (length (nlk:trimmed content)))) '()
           (list (format nil "operation ~d: add needs the text to store" index))))
      (t (let ((problems '()))
           (unless (and (stringp old) (plusp (length (nlk:trimmed old))))
             (push (format nil "operation ~d: ~a needs the text of the entry it names" index action)
                   problems))
           (when (and (equal action "replace")
                      (not (and (stringp content) (plusp (length (nlk:trimmed content))))))
             (push (format nil "operation ~d: replace needs the text to store" index) problems))
           (nreverse problems))))))

;;; --- the pending queue ----------------------------------------------------------------

(defun field (object key)
  "KEY's string in OBJECT, or NIL."
  (when (hash-table-p object)
    (let ((value (gethash key object)))
      (when (stringp value) value))))

(defun operation-of (table)
  (list :action (field table "action")
        :content (field table "content")
        :old-text (field table "old_text")))

(defun record-files ()
  (let ((directory (pending-directory)))
    (if (uiop:directory-exists-p directory)
        (sort (directory (merge-pathnames "*.json" directory))
              #'string< :key #'namestring)
        '())))

(defun record-of (path)
  "PATH as a proposal, or NIL when it is not one."
  (let ((data (ignore-errors (nlk:decode-json (nlk:read-text path)))))
    (when (hash-table-p data)
      (let* ((payload (gethash "payload" data))
             (action (field payload "action"))
             (target (field payload "target"))
             (operations (cond ((equal action "batch")
                                (let ((vector (gethash "operations" payload)))
                                  (when (vectorp vector)
                                    (loop for item across vector collect (operation-of item)))))
                               ((stringp action) (list (operation-of payload)))))
             (problems (unless (target-name target)
                         (list (format nil "target must be memory or user, not ~s" target)))))
        (dolist (operation operations)
          (setf problems (append problems (operations-problems operation))))
        (list :path path
              :id (or (field data "id") (pathname-name path))
              :created (field data "created_at")
              :target target
              :action action
              :operations operations
              :problems problems)))))

(defun pending-records ()
  (remove nil (mapcar #'record-of (record-files))))

(defun gist (text &optional (width 72))
  "TEXT on one line, cut to WIDTH characters."
  (let ((one (substitute #\Space #\Newline (or text ""))))
    (if (> (length one) width) (concatenate 'string (subseq one 0 (1- width)) "…") one)))

(defun pending-line (record &key (index 0))
  (format nil "~@[~d. ~]~a  ~a  ~a~@[  (~{~a~^; ~})~]~%     ~a"
          (and (plusp index) index)
          (getf record :id)
          (string-downcase (or (getf record :target) "?"))
          (or (getf record :action) "?")
          (getf record :problems)
          (gist (or (getf (first (getf record :operations)) :content)
                    (getf (first (getf record :operations)) :old-text)))))

(defun record-text (record)
  "One proposal in full: what it would do, and the diff it makes."
  (with-output-to-string (out)
    (format out "~a  ~a  ~a~@[  staged ~a~]~%"
            (getf record :id) (string-downcase (or (getf record :target) "?"))
            (or (getf record :action) "?") (getf record :created))
    (when (getf record :problems)
      (format out "cannot apply: ~{~a~^; ~}~%" (getf record :problems)))
    (dolist (operation (getf record :operations))
      (case (intern (string-upcase (or (getf operation :action) "")) :keyword)
        (:ADD (format out "+ ~a~%" (getf operation :content)))
        (:REPLACE (format out "- ~a~%+ ~a~%" (getf operation :old-text) (getf operation :content)))
        (:REMOVE (format out "- ~a~%" (getf operation :old-text)))
        (t (format out "? ~a~%" (getf operation :action)))))))

(defun new-id (target action content old)
  (nlk:short-digest (format nil "~a|~a|~a|~a|~a|~a"
                             (nlk:iso-now) target action content old (incf *serial*))
                     12))

(defun stage (target action content &optional old-text)
  "Propose one write: TARGET's store gains CONTENT, or the entry OLD-TEXT
becomes CONTENT or goes. Answers the proposal's id."
  (let ((clean-target (string-downcase (or target "")))
        (clean-action (string-downcase (or action ""))))
    (target-file clean-target)
    (let ((operation (list :action clean-action
                           :content (and content (nlk:trimmed content))
                           :old-text (and old-text (nlk:trimmed old-text)))))
      (let ((problems (operations-problems operation)))
        (when problems (fail "~{~a~^; ~}" problems)))
      ;; a proposal that cannot apply is refused now, not at approval
      (apply-operation (entries clean-target) operation)
      (let* ((id (new-id clean-target clean-action content old-text))
             (path (merge-pathnames (format nil "~a.json" id) (pending-directory)))
             (record (nlk:encode-json-object
                      (nlk:json-object
                       "id" id
                       "created_at" (nlk:iso-now)
                       "subsystem" "memory"
                       "payload" (nlk:json-object
                                  "target" clean-target
                                  "action" clean-action
                                  "content" (or content "")
                                  "old_text" (or old-text ""))))))
        (ensure-directories-exist path)
        (nlk:write-file-atomically path (format nil "~a~c" record #\Newline))
        id))))

(defun resolve-record (selector)
  "The one proposal SELECTOR names, by id or a unique prefix of it."
  (let* ((key (nlk:trimmed (or selector "")))
         (records (pending-records))
         (candidates (remove-if-not (lambda (record) (uiop:string-prefix-p key (getf record :id)))
                                    records)))
    (cond ((zerop (length key)) (fail "name a pending proposal: /magi pending lists them"))
          ((= 1 (length candidates)) (first candidates))
          ((null candidates) (fail "no pending proposal matches ~s" key))
          (t (fail "~s is more than one proposal (~{~a~^, ~}); give more of the id"
                   key (mapcar (lambda (record) (getf record :id)) candidates))))))

(defun decide (selector decision)
  "Approve or reject the proposal SELECTOR names. An approval applies its
operations to the store and takes the record away; a rejection only takes it
away."
  (let* ((record (resolve-record selector))
         (approved (string-equal (or decision "") "approve")))
    (unless (or approved (string-equal (or decision "") "reject"))
      (fail "decision must be approve or reject, not ~s" decision))
    (when (and approved (getf record :problems))
      (fail "~a cannot apply: ~{~a~^; ~}" (getf record :id) (getf record :problems)))
    (when approved
      (let ((entries (entries (getf record :target))))
        (dolist (operation (getf record :operations))
          (setf entries (apply-operation entries operation)))
        (write-entries (getf record :target) entries)))
    (ignore-errors (delete-file (getf record :path)))
    (format nil "~a ~a (~a, ~a)"
            (if approved "applied" "rejected")
            (getf record :id)
            (string-downcase (getf record :target))
            (getf record :action))))

(defun list-pending (&optional target limit)
  "The proposals waiting for a person, one numbered block each; TARGET
narrows them to one store and LIMIT to the first few."
  (let ((records (remove-if-not (lambda (record)
                                  (or (null target)
                                      (equal (string-downcase target)
                                             (string-downcase (getf record :target)))))
                                (pending-records)))
        (limit (or limit (ignore-errors (setting :page-size)) 20)))
    (if records
        (with-output-to-string (out)
          (loop for record in records for index from 1
                while (<= index limit)
                do (format out "~a~%" (pending-line record :index index)))
          (when (> (length records) limit)
            (format out "(+~d more)~%" (- (length records) limit))))
        "no pending memory writes")))

(defun show-record (selector)
  "One proposal, in full, with the diff it would make."
  (record-text (resolve-record selector)))

(defun add-entry (target text)
  "Store TEXT in TARGET's store now, with no proposal in between."
  (target-file target)
  (write-entries target (apply-operation (entries target) (list :action "add" :content text)))
  (format nil "stored in ~a (~d entries)" (string-downcase target) (length (entries target))))

;;; --- the manual --------------------------------------------------------------------

(defparameter +primer+
  "Magi reviews memory before it is durable, and keeps the durable store tidy.

A pending proposal is one JSON record under <store>/pending/memory/, and the
stores are MEMORY.md and USER.md under <store>, one entry a block. Point
magi.store at a Hermes profile and its queue and its stores are what Magi
reads.

; a capital word is a string argument, quoted where you write it:
(magi:list-pending)                     the proposals waiting for a person
(magi:show-record ID)                    one proposal, with its diff
(magi:stage MEMORY ADD TEXT)             propose a write; answers the id
(magi:stage USER REPLACE NEW OLD)        propose replacing the entry OLD
(magi:decide ID approve)                 apply it, or reject to drop it
(magi:entries MEMORY)                    the durable entries and the budget
(magi:add-entry MEMORY TEXT)             store an entry with no proposal

/magi alone lists the queue; /magi pending, /magi show ID, /magi diff ID,
/magi approve ID, /magi reject ID, /magi memory [QUERY], /magi user [QUERY],
/magi add memory TEXT, /magi stats. A refusal is ERROR: MAGI-ERROR and says
which setting or which entry it needs."
  "What (help :magi) answers while the cell runs.")

(defun summary-line ()
  "One line: the store, the queue, the entries, the last refusal."
  (let ((store (store-path)))
    (if (null store)
        "magi: no store set; name the folder holding MEMORY.md and USER.md in /settings"
        (handler-case
            (format nil "magi: ~a; ~d pending, ~d stored entries (~d user), budget ~a"
                    (namestring store)
                    (length (pending-records))
                    (length (entries "memory"))
                    (length (entries "user"))
                    (or (budget-line "memory" (entries "memory")) "unset"))
          (magi-error (condition) (format nil "magi: ~a" condition))))))

(defun panel (line)
  "The dialog bare /magi answers: what to do next, and the line as it stands."
  (nle:list-dialog
   :title "Magi"
   :summary line
   :rows (list (nle:ask-row "Review the queue" "Proposal" "/magi show"
                             :placeholder "magi" :verb "show")
               (nle:ask-row "Approve a proposal" "Proposal" "/magi approve"
                             :placeholder "magi" :verb "approve")
               (nle:ask-row "Remember something" "Text" "/magi add memory"
                             :placeholder "what to keep" :verb "add"))))

(defun split-verb (args)
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text)))
    (values (if space (subseq text 0 space) text)
            (if space (nlk:trimmed (subseq text (1+ space))) ""))))

(defun filtered (target query)
  (let ((all (entries target)))
    (if (zerop (length query))
        all
        (remove-if-not (lambda (entry) (search query entry :test #'char-equal)) all))))

(defun run-slash (args session-id)
  "One /magi invocation => the answer's text, and typed alone the dialog."
  (declare (ignore session-id))
  (multiple-value-bind (head rest) (split-verb args)
    (labels ((body (text) (values text (panel (summary-line))))
             (needs (form) (if (zerop (length rest))
                               (body (format nil "magi: ~a what? (see /magi)" form))
                               nil)))
      (cond
        ((zerop (length head)) (values (summary-line) (panel (summary-line))))
        ((string-equal head "stats") (body (summary-line)))
        ((string-equal head "pending")
         (body (handler-case
                   (let ((records (pending-records)))
                     (if records
                         (with-output-to-string (out)
                           (loop for record in records for index from 1
                                 do (format out "~a~%" (pending-line record :index index))))
                         "magi: no proposals are waiting"))
                 (magi-error (condition) (format nil "magi: ~a" condition)))))
        ((or (string-equal head "show") (string-equal head "diff"))
         (or (needs "show")
             (body (handler-case (record-text (resolve-record rest))
                     (magi-error (condition) (format nil "magi: ~a" condition))))))
        ((or (string-equal head "approve") (string-equal head "reject"))
         (or (needs head)
             (body (handler-case (decide rest head)
                     (magi-error (condition) (format nil "magi: ~a" condition))))))
        ((or (string-equal head "memory") (string-equal head "user"))
         (body (handler-case (entries-text (string-downcase head) (filtered head rest))
                 (magi-error (condition) (format nil "magi: ~a" condition)))))
        ((string-equal head "add")
         (multiple-value-bind (target text) (split-verb rest)
           (if (zerop (length text))
               (body "magi: add what? /magi add memory TEXT")
               (body (handler-case (add-entry target text)
                       (magi-error (condition) (format nil "magi: ~a" condition)))))))
        (t (body (format nil "magi: ~a is not one of its choices (pending, show, approve, reject, memory, user, add, stats)"
                         head)))))))

;;; --- the tools ----------------------------------------------------------------------

(nlk:define-contract pending-call
  (:doc "List the memory writes waiting for a person's approval, in the store Magi reviews. Each line is an id, the store it writes, the action and the text at stake; a record that cannot apply says why.")
  ("target" :text :options '("memory" "user")
            :doc "only proposals for this store; leave it out for both")
  ("limit" :integer :doc "the most lines to answer; 20 by default"))

(nlk:define-contract show-call
  (:doc "Show one pending memory write in full, with the diff it would make: the entries it adds with +, the entries it takes away with -.")
  ("id" :text :required t :doc "the proposal's id, or a unique prefix of it"))

(nlk:define-contract stage-call
  (:doc "Propose a change to durable memory. It lands in the pending queue and changes nothing until a person approves it: add stores new text, replace rewrites one stored entry, remove takes one away.")
  ("target" :text :required t :options '("memory" "user")
            :doc "memory for facts about the work, user for facts about the person")
  ("action" :text :required t :options '("add" "replace" "remove")
            :doc "what the proposal does to the store")
  ("content" :text :doc "the text to store, for add and replace")
  ("old_text" :text :doc "the entry the proposal rewrites or removes, quoted from the store"))

(nlk:define-contract decide-call
  (:doc "Approve or reject one pending memory write. Approving applies it to the store and takes it out of the queue; rejecting only takes it out.")
  ("id" :text :required t :doc "the proposal's id, or a unique prefix of it")
  ("decision" :text :required t :options '("approve" "reject")
              :doc "approve applies it, reject drops it"))

(nlk:define-contract memory-call
  (:doc "Read the durable memory store: every entry, numbered, with how much of the store's character budget it uses. Magi never writes here; a change goes through the queue.")
  ("target" :text :required t :options '("memory" "user") :doc "which store to read")
  ("query" :text :doc "only entries carrying this text, case-insensitive")
  ("limit" :integer :doc "the most entries to answer; 50 by default"))

(defun pending-tool (call)
  (guarded (lambda () (list-pending (pending-call-target call) (pending-call-limit call)))))

(defun show-tool (call)
  (guarded (lambda () (show-record (show-call-id call)))))

(defun stage-tool (call)
  (guarded
   (lambda ()
     (format nil "staged ~a (nothing changes until a person approves it)"
             (stage (stage-call-target call) (stage-call-action call)
                    (stage-call-content call) (stage-call-old-text call))))))

(defun decide-tool (call)
  (guarded (lambda () (decide (decide-call-id call) (decide-call-decision call)))))

(defun memory-tool (call)
  (guarded
   (lambda ()
     (let* ((target (string-downcase (memory-call-target call)))
            (query (or (memory-call-query call) "")))
       (entries-text target (filtered target query)
                     :limit (or (memory-call-limit call) 50))))))

;;; --- the one declaration ---------------------------------------------------------------

(nle:define-cell magi
  (:section ("magi")
    (:guide "name the folder that holds MEMORY.md and USER.md and pending/memory in store; a Hermes profile works as it stands; char_limit is the store's ceiling in characters, shown beside what it holds")
    ("store" :path :required t
             :doc "the folder holding MEMORY.md, USER.md and the pending/memory queue")
    ("char_limit" :integer :default 4000 :min 1
                  :doc "how much text MEMORY.md holds before a person should compact it")
    ("page_size" :integer :default 20 :min 1
                 :doc "how many pending proposals a listing shows when a call names no limit"))
  (:tool "magi_pending" (nlk:contract 'pending-call) 'pending-tool)
  (:tool "magi_show" (nlk:contract 'show-call) 'show-tool)
  (:tool "magi_stage" (nlk:contract 'stage-call) 'stage-tool)
  (:tool "magi_decide" (nlk:contract 'decide-call) 'decide-tool)
  (:tool "magi_memory" (nlk:contract 'memory-call) 'memory-tool)
  (:command "magi" 'run-slash
            :description "Memory: review pending writes, approve, reject, read the store"
            :argument-hint "[pending|show|approve|reject|memory|user|add|stats] ...")
  (:help :magi "magi: pending memory review and the durable store - magi:list-pending, magi:decide, magi:entries" +primer+)
  (:start (lambda () nil)))
