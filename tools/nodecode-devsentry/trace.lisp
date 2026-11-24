;;;; trace.lisp --- a pasted traceback read as frames, names and leads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; PARSE-TRACE reads the text it is handed and nothing else. When a call
;;;; also names a project, ATTACH-SOURCE-CONTEXT adds a few lines for a cited
;;;; file that stays inside that project and is not a symbolic link. Nothing
;;;; here executes the project, and every answer says the leads are leads.

(in-package #:nodecode-devsentry)

(defparameter +disclaimer+
  (concatenate 'string
               "These are explainable debugging leads for a person to verify. "
               "They are not a confirmed root cause, and this tool does not claim one.")
  "What every diagnosis says about itself.")

(defparameter *max-trace-chars* 200000 "How much of a pasted trace is read.")
(defparameter *max-frames* 50 "How many frames one answer carries.")
(defparameter *max-exceptions* 20 "How many exception names one answer carries.")
(defparameter *max-context-frames* 5 "How many frames get source lines.")
(defparameter *max-line-chars* 200 "How long one quoted source line may be.")
(defparameter *max-source-bytes* (* 256 1024)
  "How large a cited source file may be before it is not read.")

(defun split-lines (text)
  "TEXT split at its newlines, the line breaks themselves left out."
  (let ((lines '()) (start 0))
    (loop
      (let ((at (position #\Newline text :start start)))
        (push (subseq text start at) lines)
        (if at (setf start (1+ at)) (return (nreverse lines)))))))

;;; --- the leads, by exception name ------------------------------------------------

(defparameter +leads+
  '(;; Python
    ("ZeroDivisionError" . "Check the divisor at the cited line, including a value returned by an earlier call. Confirm it cannot be zero for this input.")
    ("KeyError" . "The mapping did not contain the reported key. Compare that key with the keys actually present, including spelling and nested structure.")
    ("IndexError" . "An index was outside the sequence. Check the length before the access and whether the sequence was empty.")
    ("TypeError" . "An operation received an unexpected type. In Python this is often None. In JavaScript the message often names undefined or null. Inspect the value at the cited call.")
    ("AttributeError" . "The receiver may be None or a different type than the code expects. Check the object at the cited line before the attribute is used.")
    ("NameError" . "A name is not defined on this path. Check spelling, imports, and assignments that may have been skipped.")
    ("UnboundLocalError" . "A local name was read before assignment. An assignment later in the function makes that name local for the whole function.")
    ("ImportError" . "An import failed. Check that the module is installed in this environment and that the import path matches the package layout.")
    ("ModuleNotFoundError" . "Python could not find the module. Check the active environment, the package name, and whether the project root is on the import path.")
    ("FileNotFoundError" . "The path was not found. Check whether it is relative to the process working directory and whether the file exists there.")
    ("PermissionError" . "The process was denied access. Check filesystem permissions and whether a directory was used as a file.")
    ("IsADirectoryError" . "A directory was used where a file was required.")
    ("NotADirectoryError" . "A file was used where a directory was required.")
    ("SyntaxError" . "The cited line may not be valid syntax. Use the message, and the caret when one is present, to locate the token.")
    ("IndentationError" . "The cited line may mix indentation or not match the surrounding block.")
    ("TabError" . "Tabs and spaces may be mixed in the indentation around the cited line.")
    ("AssertionError" . "An assertion failed. Read the expression at the cited line and the inputs that reached it.")
    ("ValueError" . "A value had an acceptable type but was not acceptable to the operation. Read the message together with the cited call.")
    ("JSONDecodeError" . "The input was not valid JSON. The message usually includes a position to inspect.")
    ("UnicodeDecodeError" . "Bytes could not be decoded with the chosen encoding. Check whether the data is binary or uses another encoding.")
    ("RecursionError" . "Recursion exceeded the limit. Look for a call that never reaches its base case.")
    ("StopIteration" . "Iteration ended unexpectedly. Check a next call or a generator that does not yield.")
    ("RuntimeError" . "A runtime check failed. Treat the message as the specific lead and inspect the cited frame.")
    ("OSError" . "An operating-system call failed. Use the message at the cited frame as the lead.")
    ("ConnectionError" . "A network operation failed. This tool did not retry it. Check the endpoint and whether the service was running.")
    ("TimeoutError" . "An operation timed out. This tool did not retry it. Check the timeout and whether the peer responded.")
    ("MemoryError" . "The process ran out of memory. Look for an unbounded read or a structure that grows with the input.")
    ("OverflowError" . "A numeric result was too large for the operation. Check the operands at the cited line.")
    ("NotImplementedError" . "Execution reached a method that is not implemented. Check which subclass or branch was used.")
    ("KeyboardInterrupt" . "Execution was interrupted. That is a signal rather than a fault in the cited expression by itself.")
    ("SystemExit" . "The process requested an exit. Check the exit code in the message and the call that raised it.")
    ;; JavaScript
    ("ReferenceError" . "JavaScript evaluated a name that is not defined in this scope. Check spelling and imports.")
    ("RangeError" . "A number was outside the range JavaScript allows. Check the value at the cited call.")
    ;; Java
    ("NullPointerException" . "A null reference was used. Check the receiver at the cited frame and where it should have been assigned.")
    ("IllegalArgumentException" . "A method rejected an argument. Compare the argument at the cited frame with the method requirements.")
    ("IllegalStateException" . "An object was used in a state the method does not allow. Check the calls that led to the cited frame.")
    ("IndexOutOfBoundsException" . "An index was outside the collection. Check the size before the access.")
    ("ClassCastException" . "An object was cast to a type it does not have. Check the value runtime type at the cited frame.")
    ("FileNotFoundException" . "The path was not found. Check the working directory and the path string at the cited frame.")
    ;; .NET
    ("NullReferenceException" . "A null reference was used. Check the object at the cited line and the assignment that should have set it.")
    ;; a panic, and a compiler's own error
    ("panic" . "A panic stopped the goroutine or Rust thread. Read the panic message against the cited file and line.")
    ("error" . "The compiler reported an error at the cited location. Read the error code and message against that line."))
  "What a person can check for the exception names this cell knows.")

(defun short-name (name)
  "NAME without what precedes its last dot."
  (let ((dot (position #\. name :from-end t)))
    (if dot (subseq name (1+ dot)) name)))

(defun lead-for (name message)
  "The lead for the exception NAME carrying MESSAGE."
  (let* ((short (short-name name))
         (specific (or (cdr (assoc short +leads+ :test #'equal))
                       (cdr (assoc name +leads+ :test #'equal))))
         (lowered (string-downcase (or message "")))
         (extra (if (and (equal short "TypeError")
                          (some (lambda (token) (search token lowered))
                                '("undefined" "null" "nonetype")))
                    (concatenate 'string
                                 " The message mentions undefined, null, or None, so find where "
                                 "that empty value is produced before the cited call.")
                    "")))
    (cond (specific (concatenate 'string specific extra))
          ((plusp (length lowered))
           (format nil "The trace names ~a. Compare its message with the last cited frame and the inputs there." name))
          (t (format nil "The trace names ~a. Inspect the last cited frame and the inputs that reached it." name)))))

;;; --- frames and exceptions -------------------------------------------------------

(defun make-frame (style path number function column)
  (list :style style
        :path (string-trim '(#\Space #\Tab #\") path)
        :line (parse-integer number)
        :function (and function (string-trim '(#\Space #\Tab) function))
        :column column
        :context nil))

(defun make-exception (style name message)
  (list :style style
        :name (string-trim '(#\Space #\Tab #\") name)
        :message (and message (string-trim '(#\Space #\Tab) message))))

(defun strip-thread-prefix (line)
  "LINE without the thread name a Java exception is announced with."
  (if (uiop:string-prefix-p "Exception in thread " line)
      (let ((close (position #\" line :start (length "Exception in thread "))))
        (if close (string-left-trim " " (subseq line (1+ close))) line))
      line))

(defun parse-trace (text)
  "TEXT parsed => a plist of frames, exceptions, leads and the bounds it hit."
  (let ((truncated-input nil)
        (frames '()) (exceptions '())
        (frames-truncated nil) (exceptions-truncated nil)
        (previous-code ""))
    (when (> (length text) *max-trace-chars*)
      (setf text (subseq text (- (length text) *max-trace-chars*))
            truncated-input t))
    (labels ((add-frame (frame)
               (if (>= (length frames) *max-frames*)
                   (setf frames-truncated t)
                   (push frame frames)))
             (add-exception (exception)
               (if (>= (length exceptions) *max-exceptions*)
                   (setf exceptions-truncated t)
                   (push exception exceptions))))
      (dolist (raw (split-lines text))
        (let* ((line (string-right-trim '(#\Space #\Tab #\Return) raw))
               (stripped (string-trim '(#\Space #\Tab #\Return) line)))
          (unless (or (zerop (length stripped))
                      (uiop:string-prefix-p "Traceback (most recent call last):" stripped)
                      (uiop:string-prefix-p "During handling of the above exception" stripped)
                      (uiop:string-prefix-p "The above exception was the direct cause" stripped))
            (block line-body
              ;; a Rust panic, old form (with a message) then new form
              (cl-ppcre:register-groups-bind (message path number column)
                  ("panicked at '(.*)', ([^:]+):(\\d+):(\\d+)" stripped)
                (add-exception (make-exception "rust" "panic" message))
                (add-frame (make-frame "rust" path number nil (parse-integer column)))
                (setf previous-code "")
                (return-from line-body nil))
              (cl-ppcre:register-groups-bind (path number column)
                  ("panicked at ([^:']+):(\\d+):(\\d+)" stripped)
                (add-exception (make-exception "rust" "panic" nil))
                (add-frame (make-frame "rust" path number nil (parse-integer column)))
                (setf previous-code "")
                (return-from line-body nil))
              ;; a Python frame
              (cl-ppcre:register-groups-bind (path number function)
                  ("^\\s*File (.*), line (\\d+)(?:, in (.*))?$" line)
                (add-frame (make-frame "python" path number function nil))
                (setf previous-code "")
                (return-from line-body nil))
              ;; a .NET frame
              (cl-ppcre:register-groups-bind (function path number)
                  ("^\\s*at (.+?) in (.+):line (\\d+)\\s*$" line)
                (add-frame (make-frame "dotnet" path number function nil))
                (setf previous-code "")
                (return-from line-body nil))
              ;; a Java frame
              (when (not (search ":line " stripped))
                (cl-ppcre:register-groups-bind (function path number)
                    ("^\\s*at ([\\w.$/<>]+)\\(([^():]+):(\\d+)\\)\\s*$" line)
                  (add-frame (make-frame "java" path number function nil))
                  (setf previous-code "")
                  (return-from line-body nil)))
              ;; a JavaScript frame
              (when (uiop:string-prefix-p "at " stripped)
                (cl-ppcre:register-groups-bind (function path number column)
                    ("^\\s*at (?:(.+?) \\()?(.+):(\\d+):(\\d+)\\)?\\s*$" line)
                  (add-frame (make-frame "javascript" path number function
                                          (parse-integer column)))
                  (setf previous-code "")
                  (return-from line-body nil)))
              ;; a Go frame, the function above it when there is one
              (cl-ppcre:register-groups-bind (path number)
                  ("^(.+\\.go):(\\d+)(?: \\+0x[0-9a-fA-F]+)?$" stripped)
                (add-frame (make-frame "go" path number
                                        (and (uiop:string-suffix-p "()" previous-code)
                                             previous-code)
                                        nil))
                (setf previous-code "")
                (return-from line-body nil))
              ;; a compiler's own pointer at a location
              (cl-ppcre:register-groups-bind (path number column)
                  ("^\\s*--> ([^:]+):(\\d+):(\\d+)\\s*$" line)
                (add-frame (make-frame "rustc" path number nil (parse-integer column)))
                (setf previous-code "")
                (return-from line-body nil))
              ;; a Go panic line
              (cl-ppcre:register-groups-bind (message)
                  ("^panic: (.*)$" stripped)
                (add-exception (make-exception "go" "panic" message))
                (setf previous-code "")
                (return-from line-body nil))
              ;; a compiler error or warning, with its code when it has one
              (cl-ppcre:register-groups-bind (name code message)
                  ("^(error|warning)(?:\\[(E\\d+)\\])?: (.*)$" stripped)
                (add-exception (make-exception "rustc" name
                                                (if code (format nil "~a: ~a" code message) message)))
                (setf previous-code "")
                (return-from line-body nil))
              (let ((candidate (strip-thread-prefix stripped)))
                ;; a Java (or .NET) exception name and its message
                (cl-ppcre:register-groups-bind (name message)
                    ("^(?:Caused by: )?((?:[\\w$]+\\.)+[\\w$]+(?:Exception|Error))(?:: (.*))?$" candidate)
                  (add-exception (make-exception (if (uiop:string-prefix-p "System." name) "dotnet" "java")
                                                  name message))
                  (setf previous-code "")
                  (return-from line-body nil))
                ;; a bare exception name in any language
                (when (not (or (uiop:string-prefix-p "File " candidate)
                               (uiop:string-prefix-p "at " candidate)))
                  (cl-ppcre:register-groups-bind (name message)
                      ("^((?:\\w+\\.)*[A-Za-z_][\\w]*(?:Error|Exception|Interrupt|Exit|Warning))(?:: ?(.*))?$" candidate)
                    (add-exception (make-exception (if (uiop:string-prefix-p "System." name) "dotnet" "generic")
                                                    name message))
                    (setf previous-code "")
                    (return-from line-body nil))))
              (setf previous-code (if (and (uiop:string-suffix-p "()" stripped)
                                            (not (find #\Space stripped)))
                                      stripped
                                      ""))))))
      (let* ((exceptions (nreverse exceptions))
             (frames (nreverse frames))
             ;; a Rust panic without a message beside one with a message is the
             ;; same panic read twice
             (cleaned (remove-if (lambda (item)
                                   (and (equal (getf item :name) "panic")
                                        (null (getf item :message))
                                        (some (lambda (other)
                                                (and (equal (getf other :name) "panic")
                                                     (getf other :message)))
                                              exceptions)))
                                 (remove-duplicates exceptions :test #'equal)))
             (leads '()))
        (dolist (item cleaned)
          (let ((lead (lead-for (getf item :name) (getf item :message))))
            (unless (member lead leads :test #'equal) (push lead leads))))
        (when (and (null cleaned) (null frames))
          (push (concatenate 'string
                             "No exception name or stack frame matched the Python, JavaScript, Java, Go, "
                             "Rust, or .NET patterns this tool knows. The text may be a log line or another format.")
                leads))
        (list :parsed (and (or cleaned frames) t)
              :input-truncated truncated-input
              :exceptions cleaned
              :frames frames
              :frames-truncated frames-truncated
              :exceptions-truncated exceptions-truncated
              :leads (nreverse leads))))))

;;; --- a few lines of a cited file, when it stays inside the project -------------------

(defun inside-project-file (project raw)
  "RAW as a regular file inside PROJECT => (values NAMESTRING NIL), or a reason."
  (cond
    ((or (null raw) (not (stringp raw)) (zerop (length raw))) (values nil "invalid_path"))
    ((some (lambda (character) (< (char-code character) 32)) raw) (values nil "invalid_path"))
    ((member ".." (uiop:split-string raw :separator "/\\") :test #'equal)
     (values nil "outside_project"))
    (t
     (let* ((project-real (string-right-trim "/\\\\" (uiop:native-namestring (truename project))))
            (pathname (uiop:parse-native-namestring raw))
            (candidate (if (uiop:absolute-pathname-p pathname)
                           pathname
                           (merge-pathnames pathname (uiop:ensure-directory-pathname project-real)))))
       (unless (uiop:string-prefix-p (format nil "~a/" project-real)
                                      (uiop:native-namestring candidate))
         (return-from inside-project-file (values nil "outside_project")))
       (cond
         ((symlink-p candidate) (values nil "symlink"))
         ((null (probe-file candidate)) (values nil "not_in_project"))
         ((not (plain-file-p candidate)) (values nil "not_a_file"))
         ((sensitive-name-p (file-namestring candidate)) (values nil "sensitive_path"))
         (t
          (let ((real (string-right-trim "/\\\\" (uiop:native-namestring (truename candidate)))))
            (if (uiop:string-prefix-p (format nil "~a/" project-real) real)
                (values real nil)
                (values nil "outside_project")))))))))

(defun omitted (reason)
  (nlk:json-object "omitted" t "reason" reason))

(defun source-snippet (path line context)
  "PATH's lines around LINE, CONTEXT each side, as the answer carries them."
  (let ((size (file-size path)))
    (when (or (null size) (> size *max-source-bytes*))
      (return-from source-snippet (omitted "file_too_large")))
    (let ((text (handler-case (uiop:read-file-string path) (error () nil))))
      (when (null text) (return-from source-snippet (omitted "unreadable")))
      (let* ((lines (split-lines text))
             (count (length lines)))
        (when (> line count)
          (return-from source-snippet
            (nlk:json-object "omitted" t "reason" "line_not_in_file" "file_lines" count)))
        (let ((start (max 1 (- line context)))
              (end (min count (+ line context)))
              (rows '()))
          (loop for number from start to end
                for text-line = (subseq (nth (1- number) lines)
                                         0 (min *max-line-chars* (length (nth (1- number) lines))))
                do (let ((row (nlk:json-object "line" number "text" text-line)))
                     (when (= number line) (setf (gethash "cited" row) t))
                     (push row rows)))
          (nlk:json-object "omitted" nil "snippet" (coerce (nreverse rows) 'vector)))))))

(defun attach-source-context (parsed project context-lines)
  "PARSED with source lines for up to five frames that stay inside PROJECT."
  (let ((attached 0))
    (dolist (frame (getf parsed :frames))
      (cond
        ((>= attached *max-context-frames*)
         (setf (getf frame :context) (omitted "context_limit")))
        (t
         (multiple-value-bind (path reason) (inside-project-file project (getf frame :path))
           (if reason
               (setf (getf frame :context) (omitted reason))
               (let ((snippet (source-snippet path (getf frame :line) context-lines)))
                 (setf (getf frame :context) snippet)
                 (unless (gethash "omitted" snippet) (incf attached))))))))
    parsed))

;;; --- the answer ------------------------------------------------------------------

(defun frames-json (frames)
  (coerce (mapcar (lambda (frame)
                    (nlk:json-object "style" (getf frame :style)
                                      "path" (getf frame :path)
                                      "line" (getf frame :line)
                                      :opt "column" (getf frame :column)
                                      :opt "function" (getf frame :function)
                                      :opt "context" (getf frame :context)))
                  frames)
          'vector))

(defun exceptions-json (exceptions)
  (coerce (mapcar (lambda (item)
                    (nlk:json-object "style" (getf item :style)
                                      "name" (getf item :name)
                                      :opt "message" (getf item :message)))
                  exceptions)
          'vector))

(defun trace-answer (parsed &key context-lines clamped project-info excerpt)
  "PARSED as the JSON object the diagnose_error tool answers."
  (let ((answer (nlk:json-object
                 "ok" t
                 "tool" "diagnose_error"
                 "parsed" (getf parsed :parsed)
                 "input_truncated" (getf parsed :input-truncated)
                 "exceptions" (exceptions-json (getf parsed :exceptions))
                 "frames" (frames-json (getf parsed :frames))
                 "frames_truncated" (getf parsed :frames-truncated)
                 "exceptions_truncated" (getf parsed :exceptions-truncated)
                 "leads" (coerce (getf parsed :leads) 'vector)
                 "disclaimer" +disclaimer+
                 "certainty" "leads_only"
                 "limits" (nlk:json-object "context_lines" (or context-lines 0)
                                             "clamped" (and clamped t)))))
    (when project-info (setf (gethash "project_path" answer) project-info))
    (when excerpt (setf (gethash "excerpt" answer) excerpt))
    answer))
