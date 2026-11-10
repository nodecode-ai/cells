;;;; report.lisp --- the compact report a large output is replaced by.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The shape is the original's: a header naming the tool and how much text
;;;; was kept, the extractor's summary, the first and last few lines carrying
;;;; the original's own line numbers, every error and warning line outside
;;;; those with its number, and how to ask for more. A number the model reads
;;;; in a report is a number it can hand to detox_inspect unchanged.
;;;;
;;;; What the rewrite drops of the original's six extractors is their depth:
;;;; one light classifier names the shape of the text (tests, build, listing,
;;;; json, text) and each shape gets a summary of a line or two. A line over
;;;; 300 characters is read only that far, as in the original, so a minified
;;;; payload cannot make the scanner slow.

(in-package #:nodecode-detox)

(defparameter +error-pattern+
  "error|fatal|exception|traceback|failed|failure|panic|cannot find|not found|syntaxerror|typeerror|valueerror|runtimeerror|assert|permission denied|undefined|referenceerror|corrupted|timed out|timeout|critical|segfault|segmentation fault|abort|aborted|oom|out of memory|killed|connection refused|connection reset|refused|nullpointer|unhandled"
  "What a line says to be read as an error; the original's own list.")

(defparameter +warning-pattern+ "warning|warn|deprecated|deprecation|caution"
  "What a line says to be read as a warning.")

(defparameter +error-scanner+
  (cl-ppcre:create-scanner +error-pattern+ :case-insensitive-mode t)
  "+ERROR-PATTERN+ compiled once, its matching ignoring case.")

(defparameter +warning-scanner+
  (cl-ppcre:create-scanner +warning-pattern+ :case-insensitive-mode t)
  "+WARNING-PATTERN+ compiled once, its matching ignoring case.")

(defparameter +tests-pattern+
  "(?:[0-9]+ (?:passed|failed|skipped))|(?:^|[ :])(?:FAILED|PASSED)|^not ok [0-9]+|^ok [0-9]+ "
  "What a test run's own lines look like.")

(defparameter +build-pattern+
  "^[^ :]+\\.[a-z]+:[0-9]+(?::[0-9]+)?: (?:error|warning)|error\\[E[0-9]+\\]|^ld: |undefined reference"
  "What a compiler's own diagnostic lines look like.")

(defparameter +failure-pattern+ "(?:FAILED|not ok|AssertionError|panic:)"
  "What a failing test's line says.")

(defparameter +tally-pattern+ "[0-9]+ (?:passed|failed|skipped|errors?)"
  "A line that counts a run.")

(defparameter +line-clip+ 300
  "A line longer than this is read this far only, as the original did.")

(defparameter +report-head+ "=== [DETOX: "
  "How a report opens; an output already carrying it is never wrapped twice.")

(defun clip (text limit)
  "TEXT cut to LIMIT characters."
  (if (> (length text) limit) (subseq text 0 limit) text))

(defun blank-p (line)
  "Whether LINE holds no visible character."
  (zerop (length (string-trim '(#\Space #\Tab) line))))

(defun report-text-p (text)
  "Whether TEXT is one of this cell's reports already."
  (not (null (search +report-head+ text :end2 (min 300 (length text))))))

;;; --- what the text is ------------------------------------------------------

(defun diagnostic-lines (lines &key (max 40))
  "The lines of LINES naming an error or a warning, as (NUMBER SEVERITY TEXT), most MAX of them."
  (let ((found '()))
    (loop for line in lines
          for n from 1
          while (< (length found) max)
          do (let ((trimmed (string-trim '(#\Space #\Tab) (clip line +line-clip+))))
               (when (plusp (length trimmed))
                 (cond ((cl-ppcre:scan +error-scanner+ trimmed)
                        (push (list n "ERROR" line) found))
                       ((cl-ppcre:scan +warning-scanner+ trimmed)
                        (push (list n "WARN" line) found))))))
    (nreverse found)))

(defun json-looking-p (text)
  "Whether TEXT opens and closes as a JSON object or array holding a field."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (length (length trimmed)))
    (and (> length 2)
         (member (char trimmed 0) '(#\{ #\[))
         (member (char trimmed (1- length)) '(#\} #\]))
         (not (null (cl-ppcre:scan "\"[^\"]+\"[ ]*:" trimmed))))))

(defun listing-line-p (line)
  "Whether LINE is one entry of a listing: an ls row, a tree row, or a bare path."
  (or (cl-ppcre:scan "^[dl-][rwx-]{9}[.@+ ]" line)
      (cl-ppcre:scan "^[|`+]-+ " line)
      (cl-ppcre:scan "^/[^ ]*$" line)))

(defun listing-looking-p (text)
  "Whether TEXT reads as a listing of files rather than prose or a log."
  (let ((lines (remove-if #'blank-p (text-lines text))))
    (and (>= (length lines) 8)
         (>= (count-if #'listing-line-p lines) (ceiling (* 3 (length lines)) 5)))))

(defun content-type (text)
  "One word for the shape of TEXT: tests, build, listing, json or text.
A run's tally is read off its last lines as its shape is off its first, so
what is looked at is both ends rather than the opening alone."
  (let* ((length (length text))
         (tail (if (> length 2000) (subseq text (- length 2000)) text))
         (head (concatenate 'string (clip text 4000) " " tail)))
    (cond ((json-looking-p head) "json")
          ((cl-ppcre:scan +tests-pattern+ head) "tests")
          ((cl-ppcre:scan +build-pattern+ head) "build")
          ((listing-looking-p head) "listing")
          (t "text"))))

;;; --- the extractor's line or two -------------------------------------------

(defun json-keys (lines)
  "The field names LINES open, in order, at most eight distinct ones."
  (let ((keys '()))
    (dolist (line lines)
      (multiple-value-bind (whole groups)
          (cl-ppcre:scan-to-strings "^ {1,4}\"([A-Za-z0-9_.-]{1,40})\"[ ]*:" line)
        (declare (ignore whole))
        (when (and groups (plusp (length (aref groups 0))))
          (pushnew (aref groups 0) keys :test #'equal))))
    (subseq (nreverse keys) 0 (min 8 (length keys)))))

(defun summary-lines (type lines)
  "A line or two saying what LINES holds, for TYPE; NIL when there is nothing to add."
  (cond
    ((string= type "tests")
     (let ((tally (car (last (remove nil
                                     (mapcar (lambda (line)
                                               (and (cl-ppcre:scan +tally-pattern+ line)
                                                    (clip (string-trim '(#\Space #\Tab) line) 160)))
                                             lines)))))
           (failures (count-if (lambda (line) (cl-ppcre:scan +failure-pattern+ line)) lines)))
       (list (format nil "tests: ~a" (or tally "no counting line"))
             (format nil "failing lines: ~d" failures))))
    ((string= type "build")
     (let ((found (diagnostic-lines lines :max 40)))
       (list (format nil "diagnostics: ~d, of them errors: ~d"
                     (length found) (count "ERROR" found :key #'second :test #'equal)))))
    ((string= type "listing")
     (let ((entries (remove-if #'blank-p lines)))
       (list (format nil "entries: ~d, of them folders: ~d"
                     (length entries)
                     (count-if (lambda (line) (cl-ppcre:scan "^(?:d|[|`+]-+ \\S+$)" line))
                               entries)))))
    ((string= type "json")
     (let ((keys (json-keys lines)))
       (when keys (list (format nil "fields: ~{~a~^, ~}" keys)))))
    (t (let ((headings (count-if (lambda (line)
                                   (cl-ppcre:scan "^(?:#|=+ |[A-Z][A-Z0-9 _-]{6,}$)" line))
                                 lines)))
         (when (plusp headings)
           (list (format nil "headings: ~d of ~d lines" headings (length lines))))))))

;;; --- the report -------------------------------------------------------------

(defun numbered (lines start &key (limit 80))
  "LINES as `  12 | text', numbered from START, at most LIMIT of them."
  (let ((out '()))
    (loop for line in lines
          for n from start
          while (< (length out) limit)
          do (push (format nil "~4d | ~a" n (clip line +line-clip+)) out))
    (nreverse out)))

(defun compact-report (tool text id type &key (head 5) (tail 5) (diagnostics 40))
  "TEXT's compact report, naming evidence ID and the line numbers of the original."
  (let* ((lines (text-lines text))
         (total (length lines))
         (head-lines (subseq lines 0 (min head total)))
         (tail-lines (if (> total head) (subseq lines (max head (- total tail))) '()))
         (tail-start (if (> total head) (1+ (max head (- total (length tail-lines)))) 1))
         (previewed (append (loop for n from 1 to (length head-lines) collect n)
                            (loop for n from tail-start to total collect n)))
         (seen (remove-if (lambda (entry) (member (first entry) previewed)) 
                           (diagnostic-lines lines :max diagnostics)))
         (summary (summary-lines type lines))
         (out '()))
    (flet ((say (control &rest arguments)
             (push (apply #'format nil control arguments) out))
           (say-all (lines)
             (dolist (line lines) (push line out))))
      (say "~a~a] (~a) ===" +report-head+ tool type)
      (say "Original output (~:d chars, ~:d lines) kept as evidence ~a." (length text) total id)
      (say "")
      (when summary
        (say "--- Extracted summary ---")
        (say-all summary)
        (say ""))
      (say "--- Head (lines 1-~d) ---" (length head-lines))
      (say-all (numbered head-lines 1))
      (when seen
        (say "")
        (say "--- Diagnostic findings (~d) ---" (length seen))
        (say-all (loop for (number severity line) in seen
                        collect (format nil "~4d [~a] | ~a" number severity (clip line +line-clip+)))))
      (when tail-lines
        (say "")
        (say "--- Tail (lines ~d-~d) ---" tail-start total)
        (say-all (numbered tail-lines tail-start)))
      (say "")
      (say "--- Retrieval ---")
      (say "~a(evidence_id=\"~a\", start_line=<N>, end_line=<M>) answers exact lines."
           "detox_inspect" id)
      (say "~a(evidence_id=\"~a\", query=\"<phrase>\") finds a phrase with its line number."
           "detox_search" id)
      (say "=== [END DETOX REPORT ~a] ===" id))
    (format nil "~{~a~^~%~}" (nreverse out))))

(defun fit-report (tool text id type &key (head 5) (tail 5) (max-chars 3000))
  "The fullest report of TEXT that stays under MAX-CHARS; NIL when even the barest does not."
  (loop for diagnostics in '(40 20 10 4 0)
        for report = (compact-report tool text id type
                                     :head head :tail tail :diagnostics diagnostics)
        when (<= (length report) max-chars)
          return report))
