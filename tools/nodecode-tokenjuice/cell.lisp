;;;; cell.lisp --- the whole cell: one :TOOL hook that compacts terminal output.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin registers ONE hook, transform_terminal_output, and
;;;; hands the captured text to a local `tokenjuice compact-shell-result'
;;;; process; the CLI owns the rules. A cell may not run the CLI, so the rules
;;;; are here, in Lisp: strip the control codes, drop adjacent repeats, clamp
;;;; a line that is too long, keep HEAD lines and TAIL lines and say how many
;;;; went, and answer the result only when it is strictly shorter by enough to
;;;; matter. Nothing else about a call is touched -- the command still runs
;;;; as it ran, and a failure's output is left whole, as the original leaves
;;;; it (bridge.py:11).
;;;;
;;;; Where the original sees Hermes's terminal results, this sees the core's
;;;; :TOOL point: a call to a shell tool, or an eval whose one form is
;;;; (sh "...") -- the two ways a command's output reaches a model here.

(defpackage #:nodecode-tokenjuice
  (:use #:cl)
  ;; The slash command's entry point, for the tests and for anyone reading
  ;; the manual; START-CELL the loader finds for itself.
  (:export #:report))

(in-package #:nodecode-tokenjuice)

(nlk:define-peripheral tokenjuice :not-running t)

(defparameter *shell-tools* '("sh" "bash" "shell" "exec_command" "evolved_shell" "run")
  "Tool names whose result is a command's own output, as the original's host
adapters name them. A tool of another name is compacted only when the config
lists it in extra_tools.")

(defparameter *extra-tools* nil
  "The configured extra_tools, materialized at start.")

(defparameter *seen-calls* 0
  "Terminal results the hook has looked at since the start.")
(defparameter *compacted-calls* 0
  "How many of those it shortened.")
(defparameter *saved-chars* 0
  "Characters kept out of the model's context since the start.")
(defparameter *omitted-lines* 0
  "Lines folded away since the start.")

(defparameter +omitted-marker+ "... ~:d lines omitted ..."
  "The line that stands where the middle of an output was folded, as the
original's headTail writes it (text.ts:202).")

(defparameter +middle-marker+ " ... "
  "What stands inside one line clamped in the middle.")

;;; --- reading text ---------------------------------------------------------

(defun strip-ansi (text)
  "TEXT without ANSI control sequences: colour and cursor codes, which cost
tokens and say nothing to a model reading prose."
  (ppcre:regex-replace-all
   (concatenate 'string (string #\Escape) "\\[[0-9;?]*[A-Za-z]") text ""))

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

(defun trim-empty-edges (lines)
  "LINES without the blank ones at either end."
  (reverse (member-if-not #'blank-line-p (reverse (member-if-not #'blank-line-p lines)))))

(defun collapse-blank-runs (lines)
  "LINES with every run of blank lines cut to one."
  (let ((out '()) (previous-blank nil))
    (dolist (line lines (nreverse out))
      (let ((blank (blank-line-p line)))
        (unless (and blank previous-blank) (push line out))
        (setf previous-blank blank)))))

(defun dedupe-adjacent (lines)
  "LINES without a line equal to the one right before it -- the original's
dedupeAdjacent (text.ts:135): a build that repeats one warning forty times
costs one line."
  (let ((out '()))
    (dolist (line lines (nreverse out))
      (unless (equal line (first out)) (push line out)))))

(defun clamp-line (line)
  "LINE within max_line_chars characters, its middle folded: a minified
JSON or a long path list reads the same at both ends."
  (let ((max (setting :max-line-chars)))
    (if (<= (length line) max)
        line
        (let* ((body (- max (length +middle-marker+)))
               (head (ceiling (* body 0.7)))
               (tail (max 0 (- body head))))
          (concatenate 'string (subseq line 0 head) +middle-marker+
                       (subseq line (- (length line) tail)))))))

(defun head-tail (lines head tail)
  "LINES cut to HEAD at the front and TAIL at the back with the count of
what went between them, as the original's headTail does. => (values LINES
OMITTED)."
  (let ((head (max 0 head)) (tail (max 0 tail)))
    (if (<= (length lines) (+ head tail))
        (values lines 0)
        (let ((omitted (- (length lines) head tail)))
          (values (append (subseq lines 0 head)
                          (list (format nil +omitted-marker+ omitted))
                          (subseq lines (- (length lines) tail)))
                  omitted)))))

(defun failing-output-p (text)
  "Whether TEXT is a command that did not succeed, by the two lines the
shell's own output carries: `[exit N]' for N other than 0, `[timed out ...]'.
The original rewrites successful output only (bridge.py:11)."
  (let* ((lines (remove-if #'blank-line-p (split-lines text)))
         (last (car (last lines))))
    (and last
         (or (ppcre:scan "^\\[exit (?!0\\])" last)
             (ppcre:scan "^\\[timed out" last)))))

(defun split-text-header (text)
  "TEXT as (values HEADER BODY): HEADER is the [text: N lines, M characters]
line a multi-line string result arrives under (turn.lisp TEXT-BLOCK), so the
counts can be put right after folding; it is NIL for raw output."
  (if (and (ppcre:scan "^\\[text: [0-9,]+ line" text)
           (let ((end (position #\Newline text)))
             (and end (char= #\] (char text (1- end))) end)))
      (let ((end (position #\Newline text)))
        (values (subseq text 0 end) (subseq text (1+ end))))
      (values nil text)))

(defun text-header (body)
  "The header a multi-line string result carries, recounting BODY."
  (format nil "[text: ~:D line~:p, ~:D characters]"
          (+ (count #\Newline body)
             (if (and (plusp (length body)) (char/= #\Newline (char body (1- (length body))))) 1 0))
          (length body)))

;;; --- the one reduction ----------------------------------------------------

(defun compact-text (text)
  "TEXT with its middle folded away. => (values NEW-TEXT OMITTED-LINES), or
NIL when folding would not save min_saved_chars characters."
  (multiple-value-bind (header body) (split-text-header text)
    (when (and (setting :keep-failures) (failing-output-p body))
      (return-from compact-text nil))
    (when (setting :strip-ansi) (setf body (strip-ansi body)))
    (let* ((lines (trim-empty-edges (split-lines body)))
           (source (length lines)))
      (unless (>= source (setting :min-lines))
        (return-from compact-text nil))
      (when (setting :dedupe) (setf lines (dedupe-adjacent lines)))
      (setf lines (collapse-blank-runs (mapcar #'clamp-line lines)))
      (multiple-value-bind (folded omitted) (head-tail lines (setting :head-lines) (setting :tail-lines))
        (let* ((new-body (join-lines folded))
               (new-text (if header
                             (format nil "~a~%~a" (text-header new-body) new-body)
                             new-body)))
          (when (and (plusp omitted)
                     (>= (- (length text) (length new-text)) (setting :min-saved-chars)))
            (values new-text omitted)))))))

;;; --- which calls are terminal output --------------------------------------

(defun shell-form-p (form)
  "Whether FORM is one shell call: (sh \"...\"), or its strings-object shape."
  (and (ppcre:scan "^[ \t]*\\((?:sh|shell|run|nle:sh|nodecode:sh)[ \t)]" form) t))

(defun terminal-call-p (name arguments)
  "Whether the call NAME with ARGUMENTS is a command whose output is a
terminal's: a shell tool's own call, or an eval that is one shell form."
  (when (and (stringp name) (hash-table-p arguments))
    (cond
      ((or (member name *shell-tools* :test #'string=)
           (member name *extra-tools* :test #'string=))
       (let ((command (gethash "command" arguments)))
         (and (stringp command) (plusp (length command)) t)))
      ((string= name "eval")
       (let ((form (gethash "form" arguments)))
         (and (stringp form) (shell-form-p form))))
      (t nil))))

;;; --- the hook -------------------------------------------------------------

(defun compact-result (text)
  "TEXT compacted and counted, or TEXT when compacting would not pay."
  (incf *seen-calls*)
  (handler-case
      (multiple-value-bind (new omitted) (compact-text text)
        (if new
            (progn (incf *compacted-calls*)
                   (incf *saved-chars* (- (length text) (length new)))
                   (incf *omitted-lines* omitted)
                   new)
            text))
    (error () text)))

(defun compact-tool-call (op next)
  "A :TOOL point hook: run the call, then fold the output the model is about
to read. The call itself is untouched -- NEXT runs it with the same OP, and
the shell's status still comes back beside the text."
  (multiple-value-bind (text thunk) (funcall next op)
    (values (if (and (stringp text)
                     (terminal-call-p (getf op :name) (getf op :arguments)))
                (compact-result text)
                text)
            thunk)))

;;; --- /tokenjuice -----------------------------------------------------------

(defun report (args session-id)
  "One /tokenjuice: what the hook has saved since the start, and what it runs
with. ARGS `reset' puts the counts back to zero."
  (declare (ignore session-id))
  (running-settings)
  (when (and (stringp args) (string-equal (string-trim " " args) "reset"))
    (setf *seen-calls* 0 *compacted-calls* 0 *saved-chars* 0 *omitted-lines* 0)
    (return-from report "tokenjuice: counts reset"))
  (format nil "tokenjuice: compacted ~:d of ~:d terminal results, keeping ~:d characters and ~:d lines ~
               out of the context; on for ~{~a~^, ~}~@[, plus ~{~a~^, ~}~]; ~:[a failure is compacted too~;~
               a failure is left whole~]; head/tail ~d/~d lines, a line clamped at ~d characters, rewriting ~
               only when at least ~d characters go"
          *compacted-calls* *seen-calls* *saved-chars* *omitted-lines*
          *shell-tools* *extra-tools*
          (setting :keep-failures)
          (setting :head-lines) (setting :tail-lines)
          (setting :max-line-chars) (setting :min-saved-chars)))

(nle:define-cell tokenjuice
  (:section ("tokenjuice")
    (:guide "nothing here is required: the defaults fold noise and leave the first and last ten lines; ~
             raise head_lines while reading a long log, lower min_saved_chars to compact smaller results")
    ("min_lines"        :integer :default 12  :min 2  :doc "the fewest lines worth compacting")
    ("head_lines"       :integer :default 10  :min 0  :doc "lines kept from the start of the output")
    ("tail_lines"       :integer :default 10  :min 0  :doc "lines kept from the end of the output")
    ("max_line_chars"   :integer :default 240 :min 40 :doc "the longest one line may be kept, folded in its middle")
    ("min_saved_chars"  :integer :default 200 :min 0  :doc "leave the result alone when folding saves fewer characters")
    ("dedupe"           :boolean :default t   :doc "drop a line that repeats the one right before it")
    ("strip_ansi"       :boolean :default t   :doc "drop colour and cursor codes")
    ("keep_failures"    :boolean :default t   :doc "leave a failing command's output whole")
    ("extra_tools"      :list                 :doc "more tool names whose results are terminal output"))
  (:hook :tool #'compact-tool-call)
  (:command "tokenjuice" 'report
            :description "What terminal-output compaction has saved"
            :argument-hint "[reset]")
  (:help :tokenjuice
         "tokenjuice: terminal output, compacted before the model reads it"
         "A tool call that runs a shell command answers through the tokenjuice hook: control codes go, a line repeating the one before it goes, a line longer than max_line_chars keeps its two ends, and past head_lines the middle is folded into one `... N lines omitted ...' line, keeping tail_lines at the end. Only a call that ran (no `[exit N]' other than 0) and only when the fold saves at least min_saved_chars characters is rewritten; a failure, a short result and a result that would not pay come back as they were. The section `tokenjuice' in the config sets the counts; /tokenjuice says what it has saved and `reset' zeroes that.")
  (:start (lambda ()
            (setf *extra-tools* (setting :extra-tools)
                  *seen-calls* 0
                  *compacted-calls* 0
                  *saved-chars* 0
                  *omitted-lines* 0)
            (nle:on-stop (lambda () (setf *extra-tools* nil
                                          *seen-calls* 0
                                          *compacted-calls* 0
                                          *saved-chars* 0
                                          *omitted-lines* 0))))))
