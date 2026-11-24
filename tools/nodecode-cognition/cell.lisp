;;;; cell.lisp --- cognition: durable memory kept on this machine.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode rewrite of aska-digital/cognition-memory's Hermes memory
;;;; provider (see NOTICE). The original keeps everything local: a session's
;;;; operator messages are scanned at its end for lasting statements, the
;;;; matches go onto a durable FIFO queue, and a drain pushes them through a
;;;; fetch-first merge into one commit log that is the single truth. Every
;;;; entry carries mandatory provenance, a conflict keeps both versions under
;;;; deterministic refs, an evicted entry is archived first, and recall injects
;;;; committed entries into the prompt under a token budget with a marker.
;;;;
;;;; This cell keeps all of that in this image: two JSON files under one
;;;; directory instead of SQLite plus JSON, the same refs, the same merge, the
;;;; same provenance, the same truncation order. What it cannot carry is the
;;;; original's cross-process flock: a Nodecode home is one process, and the
;;;; folder lock below is what stands in for it.

(defpackage #:nodecode-cognition
  (:use #:cl)
  (:nicknames #:cognition)
  (:export #:cognition-error #:cognition-error-detail
           #:store-dir #:committed-path #:queue-path #:archive-path
           #:extract-facts #:capture #:enqueue #:drain-due #:recall-block
           #:search-committed #:status-text #:dream-digest #:push-entry
           #:committed-table #:run-command))

(in-package #:nodecode-cognition)

(nlk:define-peripheral cognition :not-running t :lock "cognition")

;;; --- what the cell runs on ------------------------------------------------

(defparameter +chars-per-token+ 4
  "What a recall budgets per token. English prose runs near four characters;
the budget is a guardrail rather than a bill, so the estimate may be rough.")

(defparameter +max-content+ 400
  "The longest one memory keeps, in characters: a lasting statement, not a
transcript. The original truncated at 400 the same way.")

(defparameter +retry-delays+ '(60 300 1800)
  "Seconds before a failed push is tried again: one minute, five, then every
thirty. The original's schedule, unchanged.")

(defparameter +retry-jitter+ 0.1
  "The fraction of a retry delay that is jitter, so retries do not herd.")

(defparameter +max-entry-age+ (* 24 3600)
  "An entry older than this is dead-lettered with a tombstone rather than
retried: the original's 24h cap.")

(defparameter +provenance-fields+
  '("source_session" "author" "created_at" "origin_repo" "trace_id")
  "The provenance every entry carries. An entry missing one is refused at the
gate, so a recalled fact can always be traced to the session that said it.")

(defparameter +signals+
  '(" i prefer" " i like" " i love" " i use" " i want" " i need" " i always"
    " i never" " i usually" " i tend to" " my favorite" " my favourite"
    " my preferred" " my default" " we decided" " we agreed" " we chose"
    " we will" " remember that" " the project uses" " the project needs"
    " the project requires" " the project depends on")
  "The phrases that mark a lasting statement, space-padded so a match is a
word. Deliberately narrow, as the original's patterns are: the capture is
automatic, so it must prefer missing a fact to storing chatter.")

;;; --- where the store is ---------------------------------------------------

(defun store-dir ()
  "The directory the store lives in: the section's, else <home>/cognition/."
  (let* ((given (setting :dir))
         (text (and given (nlk:trimmed (princ-to-string given)))))
    (if (and text (plusp (length text)))
        (uiop:ensure-directory-pathname (nlk:expand-home text))
        (nlk:home "cognition/"))))

(defun committed-path () "The commit log: the store's single truth." (merge-pathnames "committed.json" (store-dir)))
(defun queue-path () "The durable push queue." (merge-pathnames "queue.json" (store-dir)))
(defun archive-path () "Where an evicted entry is written before it goes." (merge-pathnames "archive.jsonl" (store-dir)))

(defmacro with-store-lock (&body body)
  "BODY holding the store directory's lock, made first when it is absent."
  `(progn
     (ensure-directories-exist (committed-path))
     (nlk:with-folder-lock ((store-dir)) ,@body)))

;;; --- reading and writing one JSON file ------------------------------------

(defun read-json-file (path)
  "PATH decoded, or NIL: an absent, blank or torn file reads as nothing."
  (when (probe-file path)
    (let ((object (ignore-errors (nlk:decode-json (nlk:read-text path)))))
      (and (hash-table-p object) object))))

(defun write-json-file (path object)
  "OBJECT written to PATH in one atomic replace; the file is one JSON body."
  (let* ((dir (store-dir))
         (tmp (merge-pathnames (format nil ".~a-~d.tmp" (pathname-name path) (random 1000000)) dir)))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string (nlk:encode-json-object object) out)
      (terpri out))
    (nlk:replace-file tmp path)))

;;; --- the commit log -------------------------------------------------------

(defun committed-table ()
  "The committed entries as a table ref => entry, read from disk."
  (let ((object (read-json-file (committed-path))))
    (or object (make-hash-table :test #'equal))))

(defun field (entry key)
  "ENTRY's string KEY, or NIL."
  (and (hash-table-p entry) (nlk:json-value entry :string key)))

(defun provenance-of (entry)
  "ENTRY's provenance object, or NIL."
  (and (hash-table-p entry) (nlk:json-value entry :object "provenance")))

(defun provenance-field (entry key)
  "The value of provenance KEY on ENTRY, or NIL."
  (let ((provenance (provenance-of entry)))
    (and provenance (nlk:json-value provenance :string key))))

(defun provenance-score (entry)
  "How many provenance fields ENTRY actually carries: more traceable ranks
higher in a budget cut."
  (count-if (lambda (key) (let ((value (provenance-field entry key)))
                             (and value (plusp (length value)))))
            +provenance-fields+))

(defun entry-created (entry)
  "ENTRY's creation moment, as a number; 0 when it names none."
  (let ((value (provenance-of entry)))
    (or (and value (nlk:json-value value :number "created_at")) 0)))

(defun entry-salience (entry)
  (or (and (hash-table-p entry) (nlk:json-value entry :number "salience")) 0))

(defun entry-pinned-p (entry)
  (let ((value (and (hash-table-p entry) (nlk:json-value entry :any "pinned"))))
    (and value (not (eq value :null)) value)))

(defun entry-before-p (a b)
  "The recall order, highest priority first: a pinned entry survives the cut
before anything else, then the one with more provenance, then the newer, and
salience last, as the original orders it."
  (labels ((key (entry) (list (if (entry-pinned-p entry) 1 0)
                               (provenance-score entry)
                               (entry-created entry)
                               (entry-salience entry))))
    (let ((ka (key a)) (kb (key b)))
      (loop for x in ka for y in kb
            when (/= x y) return (> x y)
            finally (return nil)))))

(defun validate-provenance (entry)
  "Signal unless ENTRY carries every provenance field with a value: an
empty value proves nothing, as the original's gate says."
  (let ((provenance (provenance-of entry)))
    (unless provenance (fail "an entry has no provenance record"))
    (let ((missing (remove-if (lambda (key)
                                (let ((value (nlk:json-value provenance :any key)))
                                  (and value (not (eq value :null)) (not (equal value "")))))
                              +provenance-fields+)))
      (when missing
        (fail "an entry's provenance is missing ~{~a~^, ~}" missing)))))

(defun conflict-ref (ref seconds)
  "The deterministic ref a keep-both conflict lands under."
  (format nil "~a-conflict-~d" ref seconds))

(defun push-entry (table entry base &key (now (nlk:unix-now)))
  "Push ENTRY into TABLE against the BASE the writer saw. => (values STATUS
REF): applied when the base still matches or the same entry is already there,
conflict when something else committed in between, in which case ENTRY is
kept under a conflict ref and the committed one is untouched."
  (validate-provenance entry)
  (let ((ref (field entry "ref")))
    (unless (and ref (plusp (length ref))) (fail "an entry has no ref"))
    (let ((committed (gethash ref table)))
      (cond
        ((or (null committed) (equalp committed base))
         (setf (gethash ref table) entry)
         (values "applied" ref))
        ((equalp committed entry) (values "applied" ref))
        (t (let* ((head (conflict-ref ref now))
                  (target head)
                  (suffix 1))
             (loop while (gethash target table)
                   do (incf suffix)
                      (setf target (format nil "~a-~d" head suffix)))
             (setf (gethash "ref" entry) target
                   (gethash "conflict_with" entry) ref)
             (setf (gethash target table) entry)
             (values "conflict" target)))))))

;;; --- provenance ------------------------------------------------------------

(defun git-author ()
  "The [user] name in the home's .gitconfig, when it names one: the
original's fallback when no author is configured, and the only thing this
cell reads outside its own store."
  (let ((path (merge-pathnames ".gitconfig" (user-homedir-pathname))))
    (when (probe-file path)
      (ignore-errors
        (let ((section nil) (found nil))
          (dolist (line (uiop:read-file-lines path) found)
            (let ((text (nlk:trimmed line)))
              (cond
                ((and (plusp (length text)) (char= #\[ (char text 0)))
                 (let* ((close (position #\] text))
                        (name (and close (string-downcase (subseq text 1 close)))))
                   (setf section (and name (first (nlk:split-words name))))))
                (found nil)
                ((and (equal section "user")
                      (let ((equals (position #\= text)))
                        (and equals (string-equal (nlk:trimmed (subseq text 0 equals)) "name"))))
                 (setf found (nlk:trimmed (subseq text (1+ (position #\= text))))))))))))))

(defun author-name ()
  "Who a memory is stamped with: the section's author, else the home's git
name, else unknown. Disclosed in the README."
  (or (let ((configured (and (setting :author) (nlk:trimmed (princ-to-string (setting :author))))))
        (and configured (plusp (length configured)) configured))
      (let ((git (git-author))) (and git (plusp (length git)) git))
      "unknown"))

(defun trace-id ()
  (let ((hash (nlk:sha256-text (format nil "~a|~d|~d" (nlk:iso-now) (get-universal-time) (random 1000000000)))))
    (subseq hash 0 32)))

(defun make-provenance (session author repo)
  (nlk:json-object "source_session" (or session "session")
                   "author" author
                   "created_at" (nlk:unix-now)
                   "origin_repo" repo
                   "trace_id" (trace-id)))

;;; --- what a message says that is worth keeping -------------------------------

(defun statement-p (text)
  "Whether TEXT is word-marked by one of the signals."
  (let ((padded (format nil " ~a " (string-downcase text))))
    (some (lambda (signal) (search signal padded)) +signals+)))

(defun extract-facts (texts)
  "The lasting statements among TEXTS, in order, each at most 400 characters:
the original's heuristic, applied to the operator's own messages."
  (loop for text in texts
        for trimmed = (and (stringp text) (nlk:trimmed text))
        when (and trimmed (<= 10 (length trimmed) 2000) (statement-p trimmed))
          collect (subseq trimmed 0 (min (length trimmed) +max-content+))))

;;; --- the durable push queue -------------------------------------------------

(defun queue-state ()
  "The queue's pending entries and its tombstones, oldest first."
  (let ((object (read-json-file (queue-path))))
    (values (coerce (or (and object (nlk:json-value object :array "entries")) #()) 'list)
            (coerce (or (and object (nlk:json-value object :array "tombstones")) #()) 'list))))

(defun save-queue (entries tombstones)
  (write-json-file (queue-path)
                   (nlk:json-object "entries" (coerce entries 'vector)
                                    "tombstones" (coerce tombstones 'vector))))

(defun ref-session (session)
  "SESSION as ref material: lowercase, alphanumerics and dashes, at most 32."
  (let* ((text (string-downcase (princ-to-string (or session "session"))))
         (clean (make-string-output-stream)))
    (loop for char across text
          for count from 0
          while (< count 32)
          do (write-char (if (or (alphanumericp char) (char= char #\-)) char #\-) clean))
    (let ((out (get-output-stream-string clean)))
      (if (plusp (length out)) out "session"))))

(defun next-ref (session table entries)
  "The next free ref for SESSION: the original's cog-<session>-<n>, numbered
past everything the store and the queue already hold."
  (let* ((prefix (format nil "cog-~a-" (ref-session session)))
         (used (loop for key being the hash-keys of table
                     when (eql 0 (search prefix key)) maximize (ref-number key prefix)))
         (queued (loop for entry in entries
                       for ref = (field entry "ref")
                       when (and ref (eql 0 (search prefix ref))) maximize (ref-number ref prefix))))
    (format nil "~a~d" prefix (1+ (max (or used 0) (or queued 0))))))

(defun ref-number (ref prefix)
  "The integer a ref's tail names, or 0."
  (or (ignore-errors (parse-integer ref :start (length prefix) :junk-allowed t)) 0))

(defun tombstone (tombstones ref reason payload)
  (append tombstones
          (list (nlk:json-object "ref" ref "reason" reason
                                  "dropped_at" (nlk:unix-now) "payload" payload))))

(defun enqueue (content session)
  "Keep CONTENT on the durable queue for SESSION; => its ref. The queue is
bounded, and an overflow drops the oldest pending entry with a tombstone so
the loss is visible."
  (with-store-lock
    (multiple-value-bind (entries tombstones) (queue-state)
      (let* ((table (committed-table))
             (ref (next-ref session table entries)))
        (when (>= (length entries) (setting :queue-max-entries))
          (let* ((oldest (first entries))
                 (payload (and oldest (nlk:json-value oldest :object "payload"))))
            (setf entries (rest entries))
            (setf tombstones (tombstone tombstones (field oldest "ref")
                                         "evicted-drop-oldest" payload))))
        (let ((entry (nlk:json-object
                      "ref" ref "session" (or session "session")
                      "status" "pending" "attempts" 0
                      "created_at" (nlk:unix-now) "next_due_at" (nlk:unix-now)
                      "payload" (nlk:json-object
                                 "content" content "base" :null "salience" 0.5
                                 "provenance" (make-provenance session (author-name) (first-repo))))))
          (save-queue (append entries (list entry)) tombstones)
          ref)))))

(defun first-repo ()
  "The repo label a memory is filed under: the first the section names."
  (let ((repos (and (setting :repos) (remove-if-not #'stringp (setting :repos)))))
    (or (first repos) "default")))

(defun retry-delay (attempts)
  "Seconds before the next attempt: the original's 1m / 5m / 30m schedule,
10% jittered, never under a second."
  (let* ((base (nth (min attempts (1- (length +retry-delays+))) +retry-delays+))
         (jitter (* +retry-jitter+ base (- (random 2000) 1000) 1/1000)))
    (max 1 (+ base jitter))))

(defun entry-due-p (entry now)
  (<= (or (nlk:json-value entry :number "next_due_at") 0) now))

(defun ack (entries tombstones ref)
  (declare (ignore tombstones))
  (remove-if (lambda (entry) (equal (field entry "ref") ref)) entries))

(defun fail-entry (entries tombstones ref now)
  "Count one failed attempt: retry on the schedule, or dead-letter the entry
once it is older than the cap."
  (loop for entry in entries
        if (equal (field entry "ref") ref)
          do (let ((created (or (nlk:json-value entry :number "created_at") now))
                   (attempts (1+ (or (nlk:json-value entry :integer "attempts") 0))))
               (if (>= (- now created) +max-entry-age+)
                   (return (values (remove entry entries)
                                   (tombstone tombstones ref "expired-max-24h"
                                              (nlk:json-value entry :object "payload"))))
                   (progn
                     (setf (gethash "attempts" entry) attempts
                           (gethash "next_due_at" entry) (+ now (retry-delay (1- attempts))))
                     (return (values entries tombstones)))))
        else collect entry into kept
        finally (return (values kept tombstones))))

(defun drain-due ()
  "Push every due queue entry through the fetch-first merge; => (values
PUSHED CONFLICTS FAILED). One failed push never breaks the drain: it is
retried on the schedule instead."
  (with-store-lock
    (multiple-value-bind (entries tombstones) (queue-state)
      (let ((table (committed-table))
            (now (nlk:unix-now))
            (pushed 0) (conflicts 0) (failed 0))
        (dolist (entry entries)
          (when (entry-due-p entry now)
            (let* ((ref (field entry "ref"))
                   (payload (nlk:json-value entry :object "payload"))
                   (base (and payload (nlk:json-value payload :object "base")))
                   (content (and payload (nlk:json-value payload :string "content")))
                   (record (and payload (nlk:json-object
                                         "ref" ref
                                         "content" (or content "")
                                         "salience" (or (and payload (nlk:json-value payload :number "salience")) 0.5)
                                         "provenance" (nlk:json-value payload :object "provenance")))))
              (handler-case
                  (multiple-value-bind (status landed) (push-entry table record base :now now)
                    (declare (ignore landed))
                    (if (string= status "conflict") (incf conflicts) (incf pushed))
                    (setf entries (ack entries tombstones ref)))
                (error (condition)
                  (declare (ignore condition))
                  (incf failed)
                  (multiple-value-setq (entries tombstones)
                    (fail-entry entries tombstones ref now)))))))
        (write-json-file (committed-path) table)
        (save-queue entries tombstones)
        (values pushed conflicts failed)))))

(defun capture (text session)
  "Extract the lasting statements in TEXT and queue each for SESSION; => the
refs. This is the automatic half: nothing here is a save the model asked for."
  (let ((refs (loop for fact in (extract-facts (list text))
                     collect (enqueue fact session))))
    (values refs)))

;;; --- recall under a budget ----------------------------------------------------

(defun entry-line (entry)
  "One recall line, with its ref, its content and where it came from."
  (format nil "- [~a] ~a~@[ (~a)~]"
          (or (field entry "ref") "?")
          (or (field entry "content") "")
          (provenance-field entry "origin_repo")))

(defun estimate-tokens (text)
  (if (and text (plusp (length text))) (max 1 (floor (length text) +chars-per-token+)) 1))

(defun truncation-marker (dropped budget)
  (format nil "[cognition: ~d omitted — prefetch budget ~d tokens]"
          dropped budget))

(defun apply-budget (entries budget)
  "Fit ENTRIES into BUDGET tokens: highest priority first, a marker whenever
anything was dropped, and the top entry kept even under a budget of nothing.
=> (values TEXT KEPT DROPPED)."
  (let* ((ranked (sort (copy-list entries) #'entry-before-p))
         (lines (mapcar #'entry-line ranked))
         (costs (mapcar #'estimate-tokens lines))
         (kept '()) (kept-costs '()) (used 0))
    (loop for line in lines for cost in costs
          when (<= (+ used cost) budget)
            do (push line kept) (push cost kept-costs) (incf used cost))
    (setf kept (nreverse kept) kept-costs (nreverse kept-costs))
    (let ((dropped (- (length lines) (length kept))))
      (when (and (null kept) lines)
        (setf kept (list (first lines)) kept-costs (list (first costs))
              used (first costs) dropped (1- (length lines))))
      (when (plusp dropped)
        (let ((marker (truncation-marker dropped budget)))
          (loop while (and (> (length kept) 1)
                           (> (+ used (estimate-tokens marker)) budget))
                do (decf used (car (last kept-costs)))
                   (setf kept (butlast kept) kept-costs (butlast kept-costs))
                   (incf dropped)
                   (setf marker (truncation-marker dropped budget)))
          (setf kept (append kept (list marker)))))
      (values (format nil "~{~a~^~%~}" kept) (length kept) dropped))))

(defun pending-count ()
  (length (queue-state)))

(defun recall-block ()
  "What rides the prompt: committed memories under the budget, plus a line
saying how many are still unpushed. NIL when there is nothing to say."
  (when (setting :recall)
    (ignore-errors (drain-due))
    (let* ((table (committed-table))
           (entries (loop for entry being the hash-values of table collect entry))
           (budget (setting :budget-tokens))
           (pending (pending-count)))
      (multiple-value-bind (text kept dropped) (apply-budget entries budget)
        (declare (ignore dropped))
        (let ((blocks '()))
          (when (and (plusp (length text)) (plusp kept))
            (push (format nil "## Cognition Memory~%~a" text) blocks))
          (when (plusp pending)
            (push (format nil "[cognition: ~d pending (unpushed) in the session-end queue]"
                          pending)
                  blocks))
          (when blocks (format nil "~{~a~^~%~}" (nreverse blocks))))))))

(defun standing-section (next session-id)
  "The recall block as one harness section of its own, the chain's sections
untouched: what every request carries."
  (let ((text (ignore-errors (recall-block))))
    (if text
        (acons "cognition" text
               (remove "cognition" (funcall next session-id) :key #'car :test #'equal))
        (funcall next session-id))))

;;; --- the three tools -----------------------------------------------------------

(defun search-committed (query &key (top-k 10) repo)
  "Committed memories whose content holds QUERY, at most TOP-K, each with
its ref and where it came from. Pending entries are not searched."
  (let* ((needle (string-downcase (or query "")))
         (rows (loop for entry being the hash-values of (committed-table)
                     for content = (string-downcase (or (field entry "content") ""))
                     when (and (or (zerop (length needle)) (search needle content))
                               (or (null repo) (zerop (length repo))
                                   (equal repo (provenance-field entry "origin_repo"))))
                       collect entry)))
    (if rows
        (format nil "~d memor~:p for ~s~%~{~a~^~%~}"
                (min (length rows) top-k) (or query "")
                (mapcar #'entry-line (subseq rows 0 (min (length rows) top-k))))
        (format nil "no committed memory holds ~s" (or query "")))))

(defun pending-oldest-age ()
  (let ((now (nlk:unix-now)))
    (or (loop for entry in (queue-state)
              for created = (nlk:json-value entry :number "created_at")
              when created maximize (- now created))
        0)))

(defun next-retry-in ()
  (let ((now (nlk:unix-now)))
    (or (loop for entry in (queue-state)
              for due = (nlk:json-value entry :number "next_due_at")
              when due minimize (max 0 (- due now)))
        0)))

(defun status-text ()
  "The queue's backlog and the log's size, as one answer."
  (multiple-value-bind (entries tombstones) (queue-state)
    (format nil "cognition: ~d committed, ~d pending (unpushed), ~d tombstone~:p; oldest pending ~ds, next retry in ~ds; repos ~{~a~^, ~}; store ~a"
            (hash-table-count (committed-table))
            (length entries) (length tombstones)
            (pending-oldest-age) (next-retry-in)
            (or (setting :repos) '("default"))
            (namestring (store-dir)))))

(defun dream-digest (&key repo)
  "A read-only digest of committed state: the highest-salience entries and
the keep-both conflicts a person still has to choose between. Dream never
writes."
  (let* ((rows (loop for entry being the hash-values of (committed-table)
                     when (or (null repo) (zerop (length repo))
                              (equal repo (provenance-field entry "origin_repo")))
                       collect entry))
         (ranked (sort (copy-list rows) #'> :key #'entry-salience))
         (conflicts (remove-if-not (lambda (entry) (search "-conflict-" (or (field entry "ref") ""))) rows)))
    (with-output-to-string (out)
      (format out "# Dream digest (read-only — committed state only)~%")
      (if ranked
          (format out "~{- ~a~^~%~}~%" (mapcar #'entry-line (subseq ranked 0 (min 20 (length ranked)))))
          (format out "no committed memory yet~%"))
      (when conflicts
        (format out "## Unresolved conflicts (keep-both — needs a human choice)~%")
        (dolist (entry conflicts)
          (format out "- ~a keeps both ~a and ~a~%"
                  (field entry "ref") (field entry "conflict_with") (field entry "ref"))))
      (let ((omitted (- (length rows) 20)))
        (when (plusp omitted)
          (format out "[dream: ~d further committed entries omitted]~%" omitted))))))

;;; --- the boundary, framed once ----------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the model's one-line repair."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: cognition: ~a" condition)))))

(nlk:define-contract cognition-search
  (:doc "Search the memories committed at past session ends: preferences, decisions and project facts this machine kept from the operator's own words. Search here before answering anything that may depend on an earlier session. Committed state only; entries still pending are not searched.")
  ("query" :text :required t :doc "what to find. An empty query answers the newest committed memories.")
  ("top_k" :integer :default 10 :doc "how many memories to answer at most")
  ("repo" :string :default "" :doc "only memories filed under this label; empty for every label"))

(nlk:define-contract cognition-status
  (:doc "Show the cognition push-queue backlog: pending means unpushed, a fact extracted from a past message that has not committed yet, plus the tombstones of entries dropped by the bound or the 24h cap.")
  ("note" :string :default "" :doc "ignored; the answer is the backlog itself"))

(nlk:define-contract cognition-dream
  (:doc "Read-only synthesis of committed memories: a digest of what is known and the unresolved keep-both conflicts a person should choose between. Dream never writes.")
  ("repo" :string :default "" :doc "restrict the digest to one label; empty for every label"))

(defun tool-search (request)
  (answering
    (search-committed (cognition-search-query request)
                      :top-k (cognition-search-top-k request)
                      :repo (cognition-search-repo request))))

(defun tool-status (request)
  (declare (ignore request))
  (answering (status-text)))

(defun tool-dream (request)
  (answering (dream-digest :repo (cognition-dream-repo request))))

;;; --- the command -------------------------------------------------------------

(defun summary-line ()
  (format nil "cognition: ~d committed memor~:p under ~a; ~d pending."
          (hash-table-count (committed-table)) (namestring (store-dir)) (pending-count)))

(defun panel ()
  (nle:list-dialog
   :title "Cognition" :summary (summary-line)
   :rows (list (nle:ask-row "Search memory" "Query" "/cognition search"
                             :placeholder "what to find" :verb "search"
                             :detail "committed memories, by their words")
               (nle:run-row "Dream digest" "/cognition dream"
                             :detail "what is known, and the conflicts still open"))))

(defun run-command (args session-id)
  "One /cognition invocation: bare, the store's line and a panel; with a
verb, the answer alone."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (princ-to-string (or args ""))))
         (space (position #\Space text))
         (head (string-downcase (if space (subseq text 0 space) text)))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond ((zerop (length text)) (values (summary-line) (panel)))
          ((string= head "search") (values (search-committed rest) nil))
          ((string= head "status") (values (status-text) nil))
          ((string= head "dream") (values (dream-digest :repo rest) nil))
          ((string= head "remember")
           (values (if (zerop (length rest))
                       "cognition: say what to remember, as /cognition remember TEXT"
                       (format nil "kept ~a" (enqueue (subseq rest 0 (min (length rest) +max-content+)) session-id)))
                   nil))
          (t (values (search-committed text) nil)))))

;;; --- capture on the way in -------------------------------------------------

(defun capture-hook (next &rest args)
  "Advise the in-process ingress: every prompt that enters a session is read
once for a lasting statement, and the prompt goes on untouched."
  (let ((answer (apply next args)))
    (handler-case
        (let ((session (first args)) (prompt (second args)))
          (when (and (setting :capture) (stringp prompt) (plusp (length prompt)))
            (capture prompt session)))
      (error (condition) (warn "cognition: capture failed: ~a" condition)))
    answer))

;;; --- the cell -------------------------------------------------------------

(defvar +primer+
  "The nodecode-cognition cell keeps durable memory on this machine. When the operator's message states a lasting preference, a decision or a project fact, it is queued at the moment the prompt arrives and committed by the next drain; every committed entry carries provenance - the session it came from, the author, when, and under which label - and a conflict keeps both versions under a ref of its own. The committed state rides every request under a token budget, with a marker when something was dropped. Verbs: cognition_search finds a committed memory by its words; cognition_status shows the queue's backlog; cognition_dream is a read-only digest, and surfaces the keep-both conflicts a person still has to choose between."
  "What (help :cognition) answers while the cell runs.")

(nle:define-cell cognition
  (:section ("cognition")
    (:guide "nothing here is required: the store lives under the home; capture and recall are the two automatic halves; budget_tokens caps what rides a prompt")
    ("dir" :path
     :doc "where the store lives: <home>/cognition/ unless set")
    ("repos" :list
     :doc "labels a memory may be filed under; the first is used for new ones")
    ("capture" :boolean :default t
     :doc "read each incoming prompt for a lasting statement and queue it")
    ("recall" :boolean :default t
     :doc "put committed memories in every request, under budget_tokens")
    ("budget_tokens" :integer :default 8000 :min 64
     :doc "the prompt budget recall fits into, in tokens")
    ("queue-max-entries" :integer :default 1000 :min 1
     :doc "pending entries kept before the oldest is dropped with a tombstone")
    ("author" :string
     :doc "who memories are stamped with; empty reads the home's git user.name"))
  (:help :cognition "cognition: durable memory on this machine — cognition_search finds it, cognition_status shows the queue, cognition_dream reads it" +primer+)
  (:command "cognition" #'run-command
            :description "This machine's durable memory: search it, its backlog, a digest"
            :argument-hint "search QUERY | status | dream | remember TEXT")
  (:tool "cognition_search" (nlk:contract 'cognition-search) #'tool-search)
  (:tool "cognition_status" (nlk:contract 'cognition-status) #'tool-status)
  (:tool "cognition_dream" (nlk:contract 'cognition-dream) #'tool-dream)
  (:hook 'nle:submit #'capture-hook)
  (:hook 'nle::read-harness-sections #'standing-section)
  (:start (lambda ()
            ;; The directory is made now, so a first failure names it.
            (ensure-directories-exist (committed-path)))))
