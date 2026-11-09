;;;; cell.lisp --- tameru: big tool results, kept small, nothing lost.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a context engine for another agent: it subclasses that
;;;; agent's compressor, shortens the older tool results in place -- the head,
;;;; the tail, every line that reads as an error, one exemplar per distinct
;;;; error template -- and keeps the exact original in a bounded store that
;;;; one tool, tameru_expand, reads back by ref. A shortened result starts
;;;; with a one-line summary ending in [tameru: ... ref=<16 hex>].
;;;;
;;;; Nodecode has no compressor to subclass, so the same reduction runs at
;;;; the seam a tool result crosses on its way to the model: the core's
;;;; :TOOL point. A result longer than min_chars, from a tool that is not on
;;;; the keep list, comes back shortened, its exact text kept in this cell's
;;;; own bounded store under the ref the summary names; tameru_expand hands
;;;; the original lines back by range or by regex. Deterministic and local:
;;;; no model call, no network, no child process, nothing written to disk.

(defpackage #:nodecode-tameru
  (:use #:cl)
  ;; The command and the tool body, for the tests and for anyone reading the
  ;; manual; START-CELL the loader finds for itself.
  (:export #:report #:expand-tool))

(in-package #:nodecode-tameru)

(nlk:define-peripheral tameru :not-running t)

;;; --- what the cell remembers ------------------------------------------------

(defparameter *store* '()
  "Newest-first originals: (:ref :text :tool :lines). Live-only: a restart
forgets it, and store_limit bounds it. A probable credential is never kept.")

(defparameter *seen-results* 0
  "Tool results the hook has looked at since the start.")
(defparameter *shortened-results* 0
  "How many of those it shortened.")
(defparameter *saved-chars* 0
  "Characters kept out of the model's context since the start.")

(defparameter +header+
  "[tameru: kept ~d of ~d lines, ~d distinct error~:p, ~d of ~d characters · ref=~a]"
  "The first line of a shortened result: the counts, then the ref
tameru_expand takes. The original's every row ends in the same
[tameru: ... ref=] header (render.py).")

(defparameter +unkept-header+
  "[tameru: kept ~d of ~d lines, ~d distinct error~:p, ~d of ~d characters · the original was not kept (credential-looking text)]"
  "The same line for a result whose original holds a probable credential:
the original's store refuses to keep one (recovery.py).")

(defparameter +keep-tools+ '("tameru" "tameru_expand" "session_search" "delegate_task"
                            "cronjob_manage" "process_manage")
  "Tools whose result is never shortened: the original's own keep list
(engine.py:104), so its answers and another cell's long-form results arrive
whole.")

;;; --- reading text -----------------------------------------------------------

(defun split-lines (text)
  "TEXT as a list of lines, a trailing carriage return taken off each."
  (let ((lines '()) (start 0))
    (dotimes (i (length text))
      (when (char= (char text i) #\Newline)
        (push (string-right-trim '(#\Return) (subseq text start i)) lines)
        (setf start (1+ i))))
    (push (string-right-trim '(#\Return) (subseq text start)) lines)
    (nreverse lines)))

(defun join-lines (lines)
  (with-output-to-string (out)
    (loop for (line . rest) on lines
          do (write-string line out)
             (when rest (terpri out)))))

(defun blank-line-p (line)
  (zerop (length (string-trim '(#\Space #\Tab #\Return) line))))

(defun clamp-line (line)
  "LINE within max_line_chars characters, keeping both of its ends."
  (let ((max (setting :max-line-chars)))
    (if (or (null max) (<= (length line) max))
        line
        (let ((head (ceiling (* max 0.6)))
              (tail (max 0 (- max (ceiling (* max 0.6)) 5))))
          (concatenate 'string (subseq line 0 head) " ... "
                       (subseq line (- (length line) tail)))))))

;;; --- which lines matter -----------------------------------------------------

(defparameter +error-pattern+
  (ppcre:create-scanner
   "(?i)\\berror|exception|traceback|panic:|\\bfatal|\\bfailed|failure|denied|refused|caused by|err!|\\bwarn|\\bE[0-9]{4}:|exit code [1-9]")
  "A line that reads as an error, after the original's _ERROR_RE
(tameru/compress_context.py:625) and its critical set.")

(defparameter +progress-pattern+
  (ppcre:create-scanner
   "^(\\[?[=#_>\\-*\\s]{3,}[0-9]{1,3}%|Compiling [A-Za-z0-9_-]+ v|Downloading\\b|Pulling fs layer|Step [0-9]+/[0-9]+|Collecting [A-Za-z0-9_.-]+)")
  "A progress bar or an installer's chatter: worth one count, never a line.")

(defparameter +secret-patterns+
  (mapcar #'ppcre:create-scanner
          '("-----BEGIN [A-Z ]*PRIVATE KEY-----"
            "\\bsk-[A-Za-z0-9_-]{16,}"
            "\\bgh[pousr]_[A-Za-z0-9]{20,}"
            "\\bAKIA[0-9A-Z]{16}\\b"
            "\\bxox[baprs]-[A-Za-z0-9-]{10,}"
            "(?i)(api[_-]?key|access[_-]?token|client[_-]?secret|password)\\s*[:=]\\s*\\S{8,}"
            "(?i)\\bBearer\\s+[A-Za-z0-9._-]{20,}"))
  "The shapes a probable credential takes, as the original's secret scan
(tameru/compress_context.py:101) covers them. A text carrying one is
shortened but not kept, and tameru_expand says it has no original.")

(defun error-line-p (line)
  (and (plusp (length line))
       (not (blank-line-p line))
       (ppcre:scan +error-pattern+ line)
       t))

(defun progress-line-p (line)
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (and (plusp (length trimmed)) (ppcre:scan +progress-pattern+ trimmed) t)))

(defun fingerprint (line)
  "LINE as a template: its volatile parts, digits and hex ids, masked, so two
copies of one failure land on one string and one exemplar stands for both."
  (let* ((s (string-downcase (subseq line 0 (min 300 (length line)))))
         (s (ppcre:regex-replace-all "[0-9a-f]{8,}" s "<id>"))
         (s (ppcre:regex-replace-all "[0-9]+" s "#"))
         (s (ppcre:regex-replace-all "\\s+" s " ")))
    (subseq s 0 (min 140 (length s)))))

(defun contains-secret-p (text)
  (some (lambda (scanner) (ppcre:scan scanner text)) +secret-patterns+))

;;; --- the store ---------------------------------------------------------------

(defun fnv1a-64 (text)
  "TEXT's 64-bit FNV-1a hash: the ref is derived from the text, so the same
result twice is one entry."
  (let ((hash #xcbf29ce484222325))
    (loop for ch across text
          do (setf hash (logand #xffffffffffffffff
                                 (* (logxor hash (char-code ch)) #x100000001b3))))
    hash))

(defparameter +zero-ref+ "0000000000000000")

(defun free-ref (text)
  "A ref for TEXT: its own hash, or the next free one when that is taken by
a different text."
  (let ((base (string-downcase (format nil "~16,'0x" (fnv1a-64 text)))))
    (loop for n from 0
          for candidate = (if (zerop n) base
                              (string-downcase (format nil "~16,'0x" (+ (fnv1a-64 text) n))))
          for taken = (find candidate *store* :key (lambda (entry) (getf entry :ref)) :test #'string=)
          when (or (null taken) (equal text (getf taken :text)))
            return candidate)))

(defun store-put (text tool lines)
  "Keep TEXT, newest first, bounded by store_limit. => its ref."
  (let ((ref (free-ref text)))
    (setf *store* (cons (list :ref ref :text text :tool tool :lines lines)
                        (remove ref *store* :key (lambda (entry) (getf entry :ref)) :test #'string=)))
    (let ((limit (or (setting :store-limit) 16)))
      (when (> (length *store*) limit)
        (setf *store* (subseq *store* 0 limit))))
    ref))

(defun store-get (ref)
  (and (stringp ref)
       (find ref *store* :key (lambda (entry) (getf entry :ref)) :test #'string=)))

;;; --- the reduction -----------------------------------------------------------

(defun kept-flags (lines)
  "A simple vector, one flag a line: the head, the tail, and the first line
of every distinct error template. => (values FLAGS DISTINCT)."
  (let* ((total (length lines))
         (head (min (or (setting :head-lines) 8) total))
         (tail (min (or (setting :tail-lines) 8) (- total head)))
         (budget (or (setting :error-lines) 40))
         (flags (make-array total :initial-element nil))
         (distinct 0)
         (seen (make-hash-table :test #'equal)))
    (dotimes (i head) (setf (aref flags i) t))
    (dotimes (i tail) (setf (aref flags (- total 1 i)) t))
    ;; A progress bar or an installer's chatter is worth a count, never a line.
    (loop for line in lines
          for i from 0
          when (progress-line-p line) do (setf (aref flags i) nil))
    (loop for line in lines
          for i from 0
          when (and (< distinct budget) (error-line-p line))
            do (let ((fp (fingerprint line)))
                 (unless (gethash fp seen)
                   (setf (gethash fp seen) t (aref flags i) t)
                   (incf distinct))))
    (values flags distinct)))

(defun body-lines (lines flags)
  "LINES reduced to the kept ones, with a marker line where a run went: one
exemplar and its count when the run repeats one template, else the count."
  (let ((out '()) (total (length lines)) (i 0))
    (loop while (< i total)
          do (if (aref flags i)
                 (progn (push (clamp-line (nth i lines)) out) (incf i))
                 (let ((start i)
                       (counts (make-hash-table :test #'equal))
                       (order '()))
                   (loop while (and (< i total) (not (aref flags i)))
                         do (let* ((line (nth i lines))
                                   (fp (if (blank-line-p line) "" (fingerprint line))))
                              (unless (gethash fp counts) (push fp order))
                              (incf (gethash fp counts 0))
                              (incf i)))
                   (let* ((run (- i start))
                          (top (loop for fp in order maximize (gethash fp counts)))
                          (winner (loop for fp in order when (= (gethash fp counts) top) return fp))
                          (exemplar (and winner (plusp (length winner))
                                          (loop for j from start below i
                                                for line = (nth j lines)
                                                when (equal winner (if (blank-line-p line)
                                                                       "" (fingerprint line)))
                                                  return line))))
                     (if (and exemplar (>= top 3))
                         (progn (push (format nil "[×~d] ~a" top (clamp-line exemplar)) out)
                                (when (> run top)
                                  (push (format nil "[... ~:d lines ...]" (- run top)) out)))
                         (push (format nil "[... ~:d line~:p ...]" run) out))))))
    (nreverse out)))

(defun shorten (text tool)
  "TEXT reduced. => (values NEW-TEXT REF TOTAL KEPT DISTINCT), or NIL when
the reduction would save less than a fifth of the characters."
  (let* ((lines (split-lines text))
         (total (length lines)))
    (multiple-value-bind (flags distinct) (kept-flags lines)
      (let* ((kept (count t flags))
             (body (join-lines (body-lines lines flags)))
             (secret (contains-secret-p text))
             (probe (format nil (if secret +unkept-header+ +header+) kept total distinct 0 0
                            (if secret "" +zero-ref+)))
             ;; Two passes: the header states the length of the whole answer,
             ;; and the answer is the header on top of the body.
             (header-length (length probe))
             (new-length (+ header-length 1 (length body)))
             (ref (unless secret (store-put text tool total)))
             (header (format nil (if secret +unkept-header+ +header+)
                             kept total distinct new-length (length text)
                             (or ref ""))))
        (when (< new-length (floor (* (length text) 0.8)))
          (values (format nil "~a~%~a" header body) ref total kept distinct))))))

;;; --- the hook ---------------------------------------------------------------

(defun keep-result-p (tool)
  (or (member tool +keep-tools+ :test #'string=)
      (member tool (ignore-errors (setting :skip-tools)) :test #'string=)))

(defun shorten-tool-result (op next)
  "A :TOOL point hook: run the call, then shorten the result the model is
about to read. Fail open: any error here answers the result as it came."
  (multiple-value-bind (text thunk) (funcall next op)
    (let ((tool (getf op :name)))
      (when (stringp text) (incf *seen-results*))
      (values
       (if (and (stringp text)
                (not (keep-result-p tool))
                (>= (length text) (or (setting :min-chars) 4000))
                (find #\Newline text))
           (handler-case
               (multiple-value-bind (new ref) (shorten text tool)
                 (if new
                     (progn (incf *shortened-results*)
                            (incf *saved-chars* (- (length text) (length new)))
                            new)
                     text))
             (error () text))
           text)
       thunk))))

;;; --- tameru_expand -------------------------------------------------------------

(nlk:define-contract tameru-expand
  (:doc "Tameru Expand: read back the exact text of a tool result that tameru shortened. A shortened result starts with one line ending in [tameru: ... ref=xxxxxxxxxxxxxxxx]; pass those 16 hex characters as ref. Answer the whole original, or only lines start_line..end_line (1-based, inclusive), or only the lines matching a case-insensitive regex given as grep. The answer holds one original line (blank lines included), at most max_chars of them: when it was cut, the last line says from which line to continue.")
  ("ref" :text :required t
         :doc "the 16 hex characters after ref= in a [tameru: ...] header")
  ("start_line" :integer :default 1
                :doc "the first original line to answer, counting from 1")
  ("end_line" :integer :default 0
              :doc "the last original line to answer; 0 means the end of the original")
  ("grep" :string :default ""
          :doc "only the lines matching this case-insensitive regex")
  ("max_chars" :integer :default 12000
               :doc "the most characters one answer carries"))

(defun grep-lines (pattern lines)
  "The 1-based numbers of the LINES matching PATTERN, a case-insensitive
regex. => (values NUMBERS PROBLEM)."
  (let ((scanner (handler-case (ppcre:create-scanner pattern :case-insensitive-mode t)
                    (error () nil))))
    (if (null scanner)
        (values '() (format nil "grep is not a valid regex: ~a" pattern))
        (values (loop for line in lines
                      for i from 1
                      when (ppcre:scan scanner line) collect i)
                nil))))

(defun expand-tool (args)
  "The tameru_expand tool: exact original lines by range or by regex. Never
raises; a miss says which ref was not found."
  (handler-case
      (let* ((raw (string-trim '(#\Space) (tameru-expand-ref args)))
             (ref (string-downcase (if (and (>= (length raw) 4)
                                            (string= "ref=" (subseq raw 0 4)))
                                       (subseq raw 4) raw)))
             (entry (store-get ref)))
        (if (null entry)
            (format nil "tameru_expand ref=~a not found: this cell keeps the last ~d shortened results, and a restart forgets them.~@[~%A shortened result's first line names its ref after ref=.~]"
                    ref (or (setting :store-limit) 16) (plusp (length ref)))
            (let* ((lines (split-lines (getf entry :text)))
                   (total (length lines))
                   (first (min (max 1 (or (tameru-expand-start-line args) 1)) total))
                   (last (let ((end (or (tameru-expand-end-line args) 0)))
                           (if (or (null end) (zerop end)) total (min end total))))
                   (pattern (or (tameru-expand-grep args) "")))
              (multiple-value-bind (matched problem) (grep-lines pattern lines)
                (cond
                  (problem (format nil "tameru_expand ref=~a · ~a" ref problem))
                  (t
                   (let* ((chosen (if (plusp (length pattern))
                                      (remove-if (lambda (n) (or (< n first) (> n last))) matched)
                                      (loop for n from first to last collect n)))
                          (capped '())
                          (used 0)
                          (limit (max 1 (or (tameru-expand-max-chars args) 12000)))
                          (cut nil))
                     (dolist (n chosen)
                       (let ((line (nth (1- n) lines)))
                         (if (and (plusp (length capped)) (> (+ used 1 (length line)) limit))
                             (setf cut t)
                             (progn (push line capped) (incf used (1+ (length line)))))))
                     (format nil "~a~%~%~a~@[~%~%[... truncated at max_chars: continue with start_line ~d ...]~]"
                             (if (plusp (length pattern))
                                 (format nil "tameru_expand ref=~a · ~d of ~d lines match /~a/ · lines ~{~d~^, ~}~@[ …~]"
                                         ref (length matched) total pattern
                                         (subseq matched 0 (min 40 (length matched)))
                                         (> (length matched) 40))
                                 (format nil "tameru_expand ref=~a · lines ~d-~d of ~d~@[, cut at ~d~]"
                                         ref first (if capped (first (last chosen)) first) total
                                         (and cut (length chosen))))
                             (if capped (join-lines (nreverse capped)) "(no line in that range)")
                             (and cut (1+ (first (last chosen))))))))))))
    (error (condition)
      (format nil "tameru_expand failed: ~a" condition))))

;;; --- /tameru -------------------------------------------------------------------

(defun report-text ()
  (format nil "tameru: shortened ~:d of ~:d tool results, keeping ~:d characters out of the context; ~:d original~:p held for tameru_expand. It runs at ~:d characters or more, keeps the first ~d and last ~d lines, a line clamped at ~d characters and at most ~d distinct error lines~@[; skip_tools adds ~{~a~^, ~}~]"
          *shortened-results* *seen-results* *saved-chars* (length *store*)
          (or (setting :min-chars) 4000) (or (setting :head-lines) 8)
          (or (setting :tail-lines) 8) (or (setting :max-line-chars) 300)
          (or (setting :error-lines) 40)
          (setting :skip-tools)))

(defun store-text ()
  (if (null *store*)
      "no original is held"
      (format nil "~{~a~^~%~}"
              (loop for entry in *store*
                    collect (format nil "  ref=~a · ~a · ~:d lines · ~:d characters"
                                    (getf entry :ref) (getf entry :tool)
                                    (getf entry :lines) (length (getf entry :text)))))))

(defun report (args session-id)
  "One /tameru: what it has shortened and what it holds, `reset' to forget
the originals and the counts, `expand REF' to read one back here."
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (or args "") :separator '(#\Space #\Tab))))
         (head (string-downcase (or (first words) ""))))
    (cond
      ((string= head "") (format nil "~a~%~%holding:~%~a" (report-text) (store-text)))
      ((string= head "reset")
       (setf *store* '() *seen-results* 0 *shortened-results* 0 *saved-chars* 0)
       "tameru: originals and counts cleared")
      ((string= head "expand")
       (let* ((ref (string-downcase (or (second words) "")))
              (entry (store-get (if (and (>= (length ref) 4) (string= "ref=" (subseq ref 0 4)))
                                    (subseq ref 4) ref))))
         (if (null entry)
             (format nil "tameru: no original under ~s (try /tameru to list them)" (or (second words) ""))
             (let ((text (getf entry :text)))
               (format nil "tameru: ref=~a · ~:d characters~%~%~a~@[~%[... cut at 6000 characters ...]~]"
                       (getf entry :ref) (length text)
                       (subseq text 0 (min 6000 (length text)))
                       (> (length text) 6000))))))
      (t (format nil "tameru: unknown argument ~s (try /tameru, /tameru reset, /tameru expand REF)"
                 (first words))))))

;;; --- the declaration ----------------------------------------------------------

(nle:define-cell tameru
  (:section ("tameru")
    (:guide "nothing here is required: the defaults shorten any tool result over 4000 characters to its first and last eight lines and every distinct error, and keep the exact original for tameru_expand; /tameru says what it saved")
    ("min_chars"     :integer :default 4000 :min 200
                     :doc "the smallest tool result worth shortening, in characters")
    ("head_lines"    :integer :default 8 :min 0
                     :doc "lines kept from the start of a result")
    ("tail_lines"    :integer :default 8 :min 0
                     :doc "lines kept from the end of a result")
    ("max_line_chars" :integer :default 300 :min 40
                      :doc "the longest one kept line may be, folded in its middle")
    ("error_lines"   :integer :default 40 :min 0
                     :doc "how many distinct error lines are kept")
    ("store_limit"   :integer :default 16 :min 1
                     :doc "how many originals tameru_expand can read back")
    ("skip_tools"    :list
                     :doc "more tool names whose results are never shortened"))
  (:tool "tameru_expand" (nlk:contract 'tameru-expand) 'expand-tool)
  (:command "tameru" 'report
            :description "Tameru: what shortening saved, and the originals it holds"
            :argument-hint "[expand REF | reset]")
  (:help :tameru
         "tameru: big tool results, kept small, nothing lost - /tameru [expand REF | reset]"
         "A tool result longer than min_chars comes back shortened: its first head_lines lines, its last tail_lines lines, and the first line of every distinct error template, with one marker line where a run went. Its first line ends in a [tameru: ... ref=xxxxxxxxxxxxxxxx] header, and tameru_expand reads the exact original back by that ref, whole, by line range, or by a case-insensitive regex; a result holding a probable credential is shortened but not kept. Nothing leaves this machine: no model call, no network, no child process, and the store is memory only, so a restart forgets it. /tameru says what it saved and what it holds, /tameru reset clears both, /tameru expand REF prints an original.")
  (:hook :tool #'shorten-tool-result)
  (:start (lambda ()
            (setf *store* '() *seen-results* 0 *shortened-results* 0 *saved-chars* 0)
            (nle:on-stop (lambda ()
                           (setf *store* '() *seen-results* 0
                                 *shortened-results* 0 *saved-chars* 0))))))
