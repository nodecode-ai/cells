;;;; cell.lisp --- OptMem as a Nodecode cell: the tools, the digest, the store.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of rarf/optmem-hermes-plugin's memory provider (see NOTICE).
;;;; The original's modes, tool names, tool answers and warn-words are kept;
;;;; the harness seams are Nodecode's: the `optmem' section is the config,
;;;; the tools are the cell declaration's registrations, the wake digest
;;;; rides every request as a harness section instead of a provider prefetch,
;;;; and a finished turn arrives on the :frame point instead of on_turn_start.
;;;; No key, no service, no network: the store is a folder on this disk.

(in-package #:nodecode-optmem)

;;; --- a tool's answers ---------------------------------------------------------

(defun answer-json (value)
  "VALUE — a JSON object — as the text a tool answers with."
  (nlk:encode-json-object value))

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model
repairs from."
  `(handler-case (progn ,@body)
     (error (condition)
       (nle:failure (format nil "ERROR: optmem: ~a" condition)))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed, non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (cond ((and (stringp value) (plusp (length (nlk:trimmed value)))) (nlk:trimmed value))
          ((integerp value) (princ-to-string value))
          (t (fail "~a is required" (substitute #\_ #\- name))))))

(defun argument-string (args name)
  (let ((value (gethash name args)))
    (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))

(defun argument-integer (args name)
  "ARGS' NAME as an integer, or NIL: a call that meant a number and sent a
string is told so rather than silently coerced."
  (let ((value (gethash name args)))
    (if (integerp value) (max 0 value) nil)))

(defun argument-boolean (args name)
  (let ((value (gethash name args)))
    (and value t)))

(defun tool-schema (description &rest properties)
  "DESCRIPTION and PROPERTIES — (NAME SCHEMA REQUIRED-P) ... — as a tool schema."
  (list :description description
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name schema required) in properties
                                   nconc (list name schema)))
         "required" (coerce (loop for (name schema required) in properties
                                   when required collect name)
                            'vector))))

(defun string-schema (doc &optional options)
  (apply #'nlk:make-json-object
         (append (list "type" "string" "description" doc)
                 (when options (list "enum" (coerce options 'vector))))))

;;; --- the tools -----------------------------------------------------------------

(defun result-object (hit)
  (destructuring-bind (score id date text) hit
    (nlk:make-json-object "score" (float score 1.0) "id" id "date" date "text" text)))

(defun object-array (objects)
  (coerce objects 'vector))

(defun wake-answer (result)
  "RESULT — WAKE's plist — as the answer the model reads. An incomplete
digest says so, names the blocks to compress, and carries the next nap's
prompt, as the original's `memo wake' refusal does."
  (let ((lines (getf result :lines)))
    (cond
      ((getf result :complete)
       (if (null lines)
           (nlk:make-json-object "context" (object-array '()) "note" "OptMem empty." "complete" t)
           (nlk:make-json-object "context" (object-array lines)
                                  "count" (length lines) "complete" t)))
      (t (let* ((label (lambda (block) (format nil "~a-~a" (first block) (1- (second block)))))
                (missing (mapcar label (getf result :missing)))
                (rebuild (mapcar label (getf result :rebuild)))
                (notes (append (when (getf result :nap)
                                 (list "the context needs the missing blocks compressed first. Do the compression below, then run optmem_wake again."))
                               (when rebuild
                                 (list (format nil "the summary for ~{~a~^, ~} is missing or corrupt while its records remain in the log, and no compression is pending for it. Run optmem_forget with that block id to drop the stale summary so a nap can rebuild it, or optmem_zoom to inspect. Nothing runs automatically."
                                                rebuild))))))
           (apply #'nlk:make-json-object
                  (append (list "context" (object-array lines)
                                "count" (length lines)
                                "complete" (and nil t)
                                "needs_compression" t
                                "missing_blocks" (object-array missing)
                                "note" (format nil "Cannot wake: ~{~a~^ ~}" notes))
                          (when (getf result :nap) (list "nap_prompt" (getf result :nap)))
                          (when rebuild
                            (list "rebuild_blocks" (object-array rebuild)
                                  "action" "forget_then_nap")))))))))

(defun tool-note (args)
  "Record one durable memory line; a pending compression is reported with it."
  (answering
    (let* ((text (argument-text args "text"))
           (id (with-optmem-lock (append-memory text)))
           (next (first (pending-naps (log-records) 1))))
      (answer-json
       (apply #'nlk:make-json-object
              (append (list "saved_as" (format nil "#~a" id) "status" "added")
                      (when next
                        (list "nap_due" (nlk:make-json-object "lo" (first next) "hi" (second next))
                              "note" "Run optmem_nap for this block before your next action; hi is EXCLUSIVE."))))))))

(defun tool-recall (args)
  "Search the whole log: the original's regex, BM25, or auto."
  (answering
    (let* ((query (argument-text args "query"))
           (requested (or (argument-string args "mode")
                           (and (argument-boolean args "bm25") "bm25")
                           (setting :recall-mode)))
           (resolved (plan-recall query requested))
           (topk (let ((given (argument-integer args "topk")))
                   (cond (given given)
                         ((string= resolved "regex") 0)
                         (t 5)))))
      (multiple-value-bind (hits total truncated mode-used)
          (recall-meta query :mode requested :topk topk)
        (answer-json
         (apply #'nlk:make-json-object
                (append (list "results" (object-array (mapcar #'result-object hits))
                              "count" (length hits)
                              "mode_used" mode-used
                              "total_matches" total
                              "truncated" (and truncated t))
                        (when truncated
                          (list "note" (format nil "Showing the newest ~d of ~d matches (reading budget). Narrow the query or pass topk."
                                                (length hits) total))))))))))

(defun tool-wake (_args)
  "The digest for this store."
  (declare (ignore _args))
  (answering (answer-json (wake-answer (wake (log-records) (wake-budget))))))

(defun tool-nap (args)
  "Store one block's compression."
  (answering
    (let* ((lo (argument-integer args "lo"))
           (hi (argument-integer args "hi"))
           (summary (argument-text args "summary")))
      (unless (and lo hi) (fail "lo and hi are required: integers, hi EXCLUSIVE"))
      ;; The public contract is [lo, hi), but older prompts described the
      ;; displayed range (#8-9) as if hi were inclusive; accept that shape.
      (when (block-problem lo hi)
        (cond ((null (block-problem lo (1+ hi))) (setf hi (1+ hi)))
              (t (fail "~a" (block-problem lo hi)))))
      (let ((status (apply-nap lo hi summary)))
        (answer-json
         (apply #'nlk:make-json-object
                (append (list "status" status
                              "block" (format nil "~a-~a" lo (1- hi))
                              "hi_exclusive" hi)
                        (cond ((string= status "compressed") '())
                              ((string= status "already-settled")
                               (list "note" "This block already has a summary; nothing was written."))
                              (t (list "note" "No writable summary slot for this block; refresh optmem_wake because another nap may have settled or forgotten it."))))))))))

(defun tool-zoom (args)
  "Open a block into its two halves."
  (answering
    (let ((lo (argument-integer args "lo"))
          (hi (argument-integer args "hi")))
      (unless (and lo hi) (fail "lo and hi are required: integers, hi EXCLUSIVE"))
      (answer-json (nlk:make-json-object "block" (format nil "~a-~a" lo (1- hi))
                                          "halves" (object-array (zoom lo hi)))))))

(defun tool-forget (args)
  "Drop a summary (and every block built on it) so a nap rebuilds it."
  (answering
    (let ((lo (argument-integer args "lo"))
          (hi (argument-integer args "hi")))
      (unless (and lo hi) (fail "lo and hi are required: integers, hi EXCLUSIVE"))
      (let ((gone (forget lo hi)))
        (if (null gone)
            (fail "No summary at ~a-~a; nothing to forget." lo (1- hi))
            (answer-json
             (nlk:make-json-object
              "status" "forgotten"
              "block" (format nil "~a-~a" lo (1- hi))
              "dropped" (object-array (mapcar (lambda (block)
                                                 (format nil "~a-~a" (first block) (1- (second block))))
                                               gone))
              "count" (length gone))))))))

(defun tool-config (args)
  "Show the store's size knobs, or apply NAME=VALUE changes."
  (answering
    (let ((changes (gethash "changes" args))
          (over (read-config))
          (planned '()))
      (when (and changes (not (vectorp changes))) (fail "changes must be an array of NAME=VALUE strings"))
      (loop for change across (or changes #())
            do (let* ((text (princ-to-string change))
                      (equals (position #\= text))
                      (name (and equals (string-upcase (nlk:trimmed (subseq text 0 equals)))))
                      (allowed (format nil "~{~a~^, ~}" (mapcar #'car +knobs+))))
                 (unless (and equals name (assoc name +knobs+ :test #'string=))
                   (fail "invalid knob ~s; allowed: ~a" text allowed))
                 (unless (member name +settable-knobs+ :test #'string=)
                   (fail "~a is an unsupported setting: it is a read-only memo-parity display value with no runtime effect, so it cannot be changed. Nothing was written. Settable knobs: ~{~a~^, ~}."
                         name +settable-knobs+))
                 (let ((value (ignore-errors (parse-integer text :start (1+ equals) :junk-allowed t))))
                   (unless value (fail "invalid value for ~a: ~s is not an integer" name (subseq text (1+ equals))))
                   (unless (plusp value) (fail "invalid value for ~a: must be a positive integer" name))
                   (push (cons name value) planned))))
      (when planned
        (dolist (change planned)
          (let ((entry (assoc (car change) over :test #'string=)))
            (if entry
                (setf (cdr entry) (cdr change))
                (setf over (acons (car change) (cdr change) over)))))
        (write-config over)
        (let ((persisted (read-config)))
          (dolist (change planned)
            (unless (equal (cdr (assoc (car change) persisted :test #'string=)) (cdr change))
              (fail "~a was not persisted (found ~s); the config file is unchanged."
                    (car change) (cdr (assoc (car change) persisted :test #'string=)))))))
      (answer-json
       (nlk:make-json-object
        "config" (object-array
                  (loop for knob in +knobs+
                        for name = (car knob)
                        collect (nlk:make-json-object
                                 "name" name
                                 "value" (or (cdr (assoc name over :test #'string=)) (cdr knob))
                                 "default" (cdr knob)
                                 "what" (or (cdr (assoc name +knob-docs+ :test #'string=)) "")
                                 "settable" (and (member name +settable-knobs+ :test #'string=) t))))
        "changed" (and planned t)
        "effective_wake_budget" (wake-budget))))))

;;; --- import: a file of dated memories, inside the store --------------------------

(defun import-path (name)
  "The file NAME inside <store>/imports/, refused unless it is one bare name:
the original's import_security reads one bare filename and no path."
  (let ((name (nlk:trimmed (or name ""))))
    (when (zerop (length name)) (fail "file is required"))
    (when (or (find #\/ name :test #'char=) (find #\\ name :test #'char=)
              (string= name ".") (string= name ".."))
      (fail "~s is not a bare file name: pass the name of a file in <store>/imports/" name))
    (let ((path (merge-pathnames name (store-path "imports/"))))
      (unless (probe-file path) (fail "no such file in <store>/imports/: ~a" name))
      path)))

(defun date-problem (date)
  "Why DATE is not a real YYYY-MM-DD date, or NIL."
  (unless (and (= (length date) 10)
                (cl-ppcre:scan "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" date))
    (return-from date-problem "expected 'YYYY-MM-DD <text>'"))
  (unless (ignore-errors
            (encode-universal-time 0 0 0
                                   (parse-integer date :start 8 :end 10)
                                   (parse-integer date :start 5 :end 7)
                                   (parse-integer date :start 0 :end 4)))
    (return-from date-problem "date is not a real calendar date"))
  nil)

(defun parse-import-lines (text)
  "TEXT's lines as (DATE . MEMORY) pairs, all validated before anything is
written, the way the original's atomic import does."
  (let ((records (loop for record across (log-records) when record collect record))
        (last "0000-00-00")
        (parsed '()))
    (when records (setf last (second (car (last records)))))
    (loop for line in (lines-of text)
          for index from 1
          for trimmed = (string-trim '(#\Space #\Tab) line)
          unless (zerop (length trimmed))
            do (let* ((space (position #\Space trimmed))
                      (date (if space (subseq trimmed 0 space) trimmed))
                      (memory (if space (string-trim '(#\Space #\Tab) (subseq trimmed (1+ space))) "")))
                 (when (zerop (length memory))
                   (fail "line ~d: expected 'YYYY-MM-DD <text>'" index))
                 (let ((problem (date-problem date)))
                   (when problem (fail "line ~d: ~a" index problem)))
                 (when (string< date last)
                   (fail "line ~d: date precedes the previous memory" index))
                 (when (> (nlk:utf-8-size memory) +entry-chars+)
                   (fail "line ~d: text exceeds the ~d-byte limit" index +entry-chars+))
                 (setf last date)
                 (push (cons date memory) parsed)))
    (nreverse parsed)))

(defun tool-import (args)
  "Bulk-load historical memories from <store>/imports/<file>."
  (answering
    (let* ((path (import-path (argument-string args "file")))
           (text (nlk:read-text path (* 4 1024 1024)))
           (parsed (parse-import-lines (or text (fail "~a is not a UTF-8 file under 4 MiB" path)))))
      (with-optmem-lock
        (dolist (pair parsed) (append-memory (cdr pair) (car pair))))
      (answer-json (nlk:make-json-object "status" "imported" "count" (length parsed))))))

;;; --- the digest every request carries -------------------------------------------

(defun digest-text ()
  "The wake digest as the block the model reads, or NIL when the store is
empty: recent memories verbatim, older ones compressed, bounded by the
store's reading budget."
  (let ((result (wake (log-records) (wake-budget))))
    (when (getf result :lines)
      (with-output-to-string (out)
        (format out "Your OptMem memory, newest first; older memories are compressed into #lo-hi lines.~%")
        (dolist (line (getf result :lines)) (format out "~a~%" line))
        (when (getf result :nap)
          (format out "A compression is due: run optmem_nap on the block above before your next action (hi is EXCLUSIVE).~%"))))))

(defun wake-section (next session-id)
  "Advice on NLE::READ-HARNESS-SECTIONS: the digest rides every request as
the `optmem' block. A store that cannot be read is a turn that goes on
without it, never a failed turn."
  (let ((sections (remove "optmem" (funcall next session-id) :key #'car :test #'string=)))
    (let ((text (ignore-errors (digest-text))))
      (if (and text (plusp (length text)))
          (acons "optmem" text sections)
          sections))))

;;; --- automatic naps: the original's opt-in background compaction ---------------

(defvar *turns* 0
  "Finished turns seen since this cell started, for the auto-nap cadence.")

(defparameter +durable-words+
  '("aprov" "decid" "budget" "deploy" "launch" "start" "complet" "done" "shipped"
    "client" "contract" "meet" "scheduled" "monitor" "churn" "kpi" "objective"
    "goal" "paywall" "growth" "onboard" "staging" "prod" "release" "fix" "bug"
    "feature" "approved" "decision" "replaced" "superseded" "preference" "policy")
  "Words that signal a durable fact: the original's local extractor's list,
which the model-free path scores memories by.")

(defun durable-score (line)
  (let ((low (string-downcase line))
        (score 0))
    (dolist (word +durable-words+ score)
      (when (search word low) (incf score)))
    (when (and (>= (length line) 10)
               (every #'digit-char-p (subseq line 0 4))
               (char= (char line 4) #\-))
      (incf score 2))
    score))

(defun clip-bytes (text limit)
  "TEXT cut to at most LIMIT UTF-8 bytes, on a character boundary."
  (loop with used = 0
        with end = 0
        for index from 0 below (length text)
        for size = (nlk:utf-8-size (string (char text index)))
        while (<= (+ used size) limit)
        do (incf used size)
           (setf end (1+ index))
        finally (return (subseq text 0 end))))

(defun local-summary (lines)
  "A deterministic, model-free extraction of LINES: the newest memory, then
the lines that read as durable facts, fitted into the byte limit. Nothing
durable reads as \"\", and the caller skips the nap rather than lose a block."
  (let ((scored (mapcar (lambda (line) (cons (durable-score line) line)) lines)))
    (when (find-if (lambda (pair) (plusp (car pair))) scored)
      (let* ((latest (string-right-trim '(#\Space) (clip-bytes (cdr (car (last scored))) +entry-chars+)))
             (parts (if (plusp (length latest)) (list latest) '()))
             (used (nlk:utf-8-size latest))
             (ranked (sort (loop for pair in (butlast scored)
                                 for index from 0
                                 collect (list (car pair) index (cdr pair)))
                           (lambda (a b)
                             (if (= (first a) (first b))
                                 (> (second a) (second b))
                                 (> (first a) (first b)))))))
        (dolist (entry ranked)
          (let ((line (string-right-trim '(#\Space) (third entry))))
            (when (and (plusp (first entry)) (plusp (length line))
                       (not (member line parts :test #'string=)))
              (let ((cost (+ (nlk:utf-8-size line) (if parts 3 0))))
                (when (<= (+ used cost) +entry-chars+)
                  (push line parts)
                  (incf used cost))))))
        (format nil "~{~a~^ | ~}" parts)))))

(defun block-lines (lo hi)
  "The texts a block's summary is made from: raw memories for a small block,
the two halves' summaries for a larger one."
  (let ((records (log-records)))
    (if (<= (- hi lo) +raw-max+)
        (mapcar #'third (log-slice records lo hi))
        (let ((mid (floor (+ lo hi) 2)))
          (remove nil (list (tree-get lo mid) (tree-get mid hi)))))))

(defun auto-nap ()
  "Compress the next pending block from what it holds, with no model call."
  (let ((next (first (pending-naps (log-records) 1))))
    (when next
      (let ((summary (local-summary (block-lines (first next) (second next)))))
        (when (and summary (plusp (length summary)))
          (apply-nap (first next) (second next) summary))))))

(defun observe-frame (op)
  "Fold one published frame: every tenth finished turn is the moment a
pending compression is taken, when the section asks for automatic naps."
  (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
    (declare (ignore payload metadata turn))
    (when (and (equal type "turn.completed") (setting :auto-nap))
      (when (zerop (mod (incf *turns*) 10))
        ;; Never crash a turn on a failed compression.
        (ignore-errors (auto-nap))))))

;;; --- the manual and /optmem ----------------------------------------------------

(defparameter +primer+
  "OptMem is permanent memory on this disk: an append-only log that nothing overwrites
and no model call is made to keep. ONE atomic fact per note, not a paragraph. (help :optmem) is this page.
  optmem_note {text}                    record one durable fact, one line, <= 280 UTF-8 bytes
  optmem_recall {query, topk?, mode?}   search all of it: regex, BM25, or auto
  optmem_wake {}                         the digest: recent memories verbatim, older ones compressed
  optmem_nap {lo, hi, summary}          compress block #lo-hi into one line (hi is EXCLUSIVE)
  optmem_zoom {lo, hi}                  open a block into its two halves, down to raw memories
  optmem_forget {lo, hi}                drop a summary so a nap rebuilds it; raw memories stay
  optmem_config {changes?}              the store's size knobs, or NAME=VALUE changes to WAKE_LINES / PART_CHARS
  optmem_import {file}                  bulk-load YYYY-MM-DD lines from <store>/imports/<file>
When a nap is due, do it before your next action. Recall reaches every memory ever written,
so ask it for an old decision instead of guessing. Never edit the store's files by hand."
  "What (help :optmem) answers while the cell runs.")

(defun status-line ()
  (let* ((records (log-records))
         (pending (pending-naps records))
         (next (first pending)))
    (format nil "OptMem: ~d memories in ~a, ~d pending compression~:p, wake budget ~d lines.~%~@[Next block: #~a-~a (optmem_nap, hi exclusive).~%~]/optmem QUERY searches the log; the tools optmem_note, optmem_recall, optmem_wake, optmem_nap, optmem_zoom, optmem_forget, optmem_config and optmem_import keep it."
            (length records) (namestring (store-directory)) (length pending)
            (wake-budget) next (if next (1- (second next)) 0))))

(defun format-results (query)
  "QUERY's matches as the lines a shell reads."
  (multiple-value-bind (hits total truncated mode-used)
      (recall-meta query :mode (or (setting :recall-mode) "auto") :topk 0)
    (if (null hits)
        (format nil "No memory matched ~s (~a)." query mode-used)
        (with-output-to-string (out)
          (format out "~d of ~d memories for ~s (~a):"
                  (min 20 (length hits)) total query mode-used)
          (dolist (hit (subseq hits 0 (min 20 (length hits))))
            (format out "~% #~a ~a ~a" (second hit) (third hit) (fourth hit)))
          (when truncated
            (format out "~% (truncated: narrow the query)"))))))

(defun run-command (args session-id)
  "One /optmem invocation: a query searches the log, no argument the store."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (format-results query)
        (status-line))))

;;; --- the cell -------------------------------------------------------------------

(defparameter *note-schema*
  (tool-schema
   (format nil "Record one durable memory line to the OptMem append-only log (family facts, decisions, events of lasting effect). One line, max ~d UTF-8 bytes (not characters). If a compression is due, do it (optmem_nap) before your next action. Use for things worth remembering forever — not ephemeral chat."
           +entry-chars+)
   (list "text" (string-schema (format nil "The memory, one line, at most ~d UTF-8 bytes." +entry-chars+)) t)))

(defparameter *recall-schema*
  (tool-schema
   "Search the entire OptMem history. mode='auto' (default) keeps the original memo recall regex for pattern-like queries and routes a natural-language sentence to accent-normalized token search; mode='regex' forces exact memo parity (an invalid pattern is reported as an error); mode='bm25' forces ranked fuzzy search (e.g. 'cacula' matches 'caçula'). The answer reports mode_used. Use when you need an old fact, decision or event."
   (list "query" (string-schema "Search query (a sentence or a regex pattern).") t)
   (list "topk" (nlk:make-json-object "type" "integer"
                                       "description" "Max results. In regex mode every match within the reading budget is returned unless you set topk; the semantic modes default to 5. The answer reports total_matches and truncated.")
         nil)
   (list "mode" (string-schema "Retrieval mode. Defaults to the section's recall_mode ('auto')."
                                '("auto" "regex" "bm25"))
         nil)
   (list "bm25" (nlk:make-json-object "type" "boolean"
                                       "description" "Legacy alias for mode='bm25' (default false).")
         nil)))

(defparameter *nap-schema*
  (tool-schema
   (format nil "Apply a compression the digest asked for. Call optmem_nap with the block id and a one-line summary (at most ~d UTF-8 bytes, not characters) that keeps what has lasting effect and drops the rest. Invent nothing. Aim well below the byte limit; after a size error, rewrite substantially shorter rather than retrying small edits."
           +entry-chars+)
   (list "lo" (nlk:make-json-object "type" "integer" "description" "Block start id (inclusive).") t)
   (list "hi" (nlk:make-json-object "type" "integer"
                                     "description" "Block end id (EXCLUSIVE). A displayed #8-9 block uses hi=10.")
         t)
   (list "summary" (string-schema (format nil "One-line compression, at most ~d UTF-8 bytes." +entry-chars+)) t)))

(defparameter *wake-schema*
  (list :description
        "Print the current OptMem context: recent memories verbatim, old ones decayed into summaries. Run at session start or when you need the full picture."
        :parameters (nlk:make-json-object "type" "object"
                                           "properties" (nlk:make-json-object)
                                           "required" (vector))))

(defparameter *zoom-schema*
  (tool-schema
   "Open a decay-tree node (block lo-hi, e.g. 0-15) into its two halves, down to the raw memories. Use it to recover detail compressed away by a nap. hi is EXCLUSIVE."
   (list "lo" (nlk:make-json-object "type" "integer" "description" "Block start id (inclusive).") t)
   (list "hi" (nlk:make-json-object "type" "integer" "description" "Block end id (EXCLUSIVE).") t)))

(defparameter *forget-schema*
  (tool-schema
   "Drop a bad summary at block lo-hi so the next nap rebuilds it. The raw memories are never touched. hi is EXCLUSIVE."
   (list "lo" (nlk:make-json-object "type" "integer" "description" "Block start id (inclusive).") t)
   (list "hi" (nlk:make-json-object "type" "integer" "description" "Block end id (EXCLUSIVE).") t)))

(defparameter *config-schema*
  (tool-schema
   "Show or change OptMem size knobs for this store (mirrors memo config). Pass NAME=VALUE pairs to change (e.g. WAKE_LINES=48), or no args to show current values. Settable: WAKE_LINES, PART_CHARS; ENTRY_CHARS, RAW_MAX and PART_LINES are displayed, read-only memo-parity values."
   (list "changes" (nlk:make-json-object "type" "array"
                                          "items" (nlk:make-json-object "type" "string")
                                          "description" "Optional NAME=VALUE pairs to apply.")
         nil)))

(defparameter *import-schema*
  (tool-schema
   "Bulk-load historical memories from a file in <store>/imports/. Pass its bare file name; each line is 'YYYY-MM-DD <text>', oldest first, at most 280 bytes of text."
   (list "file" (string-schema "File name inside <store>/imports/.") t)))

(nle:define-cell optmem
  (:section ("optmem")
    (:guide "no key and no service: the store is a folder on this disk; store names another folder; recall_mode is how optmem_recall searches by default; wake_lines bounds the digest every request carries; auto_nap compresses one block every ten turns")
    ("store" :path
     :doc "the folder holding LOG.txt and TREE/; empty means <home>/optmem/")
    ("recall_mode" :choice :options '("auto" "regex" "bm25") :default "auto"
     :doc "how optmem_recall searches: auto keeps the regex for patterns and routes prose to token search")
    ("wake_lines" :integer :default 96 :min 1
     :doc "how many blocks the wake digest prints; a smaller budget compresses older memories sooner")
    ("auto_nap" :boolean :default nil
     :doc "compress one pending block every ten finished turns, with no model call"))
  (:help :optmem
   "optmem: note, recall and compress a permanent local memory - optmem_note \"fact\""
   +primer+)
  (:command "optmem" 'run-command
            :description "Show the OptMem store, or search it"
            :argument-hint "QUERY")
  (:tool "optmem_note" *note-schema* 'tool-note)
  (:tool "optmem_recall" *recall-schema* 'tool-recall)
  (:tool "optmem_wake" *wake-schema* 'tool-wake)
  (:tool "optmem_nap" *nap-schema* 'tool-nap)
  (:tool "optmem_zoom" *zoom-schema* 'tool-zoom)
  (:tool "optmem_forget" *forget-schema* 'tool-forget)
  (:tool "optmem_config" *config-schema* 'tool-config)
  (:tool "optmem_import" *import-schema* 'tool-import)
  (:hook 'nle::read-harness-sections #'wake-section)
  (:hook :frame (nlk:observer #'observe-frame "nodecode-optmem"))
  (:start (lambda ()
            (handler-case (ensure-store)
              (error (condition)
                (error 'nlk:config-refusal
                       :detail (format nil "optmem: cannot use the store at ~a: ~a"
                                       (store-directory) condition))))
            (setf *turns* 0)
            (nle:on-stop (lambda () (setf *turns* 0))))))
