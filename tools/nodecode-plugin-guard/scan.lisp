;;;; scan.lisp --- the walk, the rules, and the report.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A scan reads a tree as text: no file of it is imported, compiled or
;;;; run, no process starts, nothing is written, nothing leaves the machine.
;;;; The tree is bounded -- so many files, so many bytes of each -- and the
;;;; findings are a review aid: what a person reads next, not a verdict.

(in-package #:nodecode-plugin-guard)

(defparameter *file-bytes* (* 512 1024)
  "The most of one file a scan reads; a larger one counts as unread.")

(defparameter *depth-limit* 8
  "How deep below the folder a scan follows directories.")

(defparameter *skip-directories*
  '(".git" ".hg" ".svn" "node_modules" "__pycache__" ".venv" "venv" ".eggs"
    "build" "dist" "target" ".cache" ".mypy_cache" ".pytest_cache" ".ruff_cache"
    ".tox" "site-packages")
  "Directories a scan does not enter: version control, generated output, and
the place a built package lands.")

;;; --- findings -------------------------------------------------------------------

(defun finding (rule path line message)
  "One finding: a plist of :RULE, :SEVERITY, :PATH, :LINE and :MESSAGE."
  (list :rule rule :severity (rule-severity rule) :path path :line line :message message))

(defun finding-rank (finding)
  "FINDING's severity as a number."
  (severity-rank (getf finding :severity)))

;;; --- reading --------------------------------------------------------------------

(defun read-text (path)
  "PATH's bytes as a string, or NIL when it is binary or over *FILE-BYTES*.
The decode is one character a byte: the rules are ASCII, and a byte is
never a line break that was not one."
  (handler-case
      (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
        (let ((size (file-length in)))
          (when (<= size *file-bytes*)
            (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
              (read-sequence bytes in)
              (unless (find 0 bytes)
                (map 'string #'code-char bytes))))))
    (error () nil)))

(defun line-of (text position)
  "The 1-based line of POSITION in TEXT."
  (1+ (loop for index from 0 below (min position (length text))
             count (char= (char text index) #\Newline))))

(defun window (text position size)
  "TEXT from POSITION, at most SIZE characters."
  (subseq text position (min (length text) (+ position size))))

;;; --- the walk --------------------------------------------------------------------

(defun directory-name (path)
  "PATH's own name, the last part of its directory list."
  (let ((parts (pathname-directory path)))
    (if (and parts (stringp (car (last parts))))
        (car (last parts))
        (namestring path))))

(defun skipped-directory-p (path)
  (member (string-downcase (directory-name path)) *skip-directories* :test #'string=))

(defun walk (directory &key (files '()) (depth 0) (limit 500))
  "Every regular file below DIRECTORY, at most LIMIT of them, skipping what
*SKIP-DIRECTORIES* names. The list is reversed at the end, so it reads in
directory order rather than the walk's."
  (when (< depth *depth-limit*)
    (dolist (sub (ignore-errors (uiop:subdirectories directory)))
      (unless (or (skipped-directory-p sub) (>= (length files) limit))
        (setf files (walk sub :files files :depth (1+ depth) :limit limit))))
    (dolist (file (ignore-errors (uiop:directory-files directory)))
      (when (< (length files) limit)
        (push file files))))
  files)

;;; --- one file -------------------------------------------------------------------

(defun secret-env-hit (lower)
  "Where an environment read names a credential: (POSITION . WORD), or NIL."
  (let ((positions '()))
    (dolist (needle '("os.environ" "os.getenv" "getenv" "environ["))
      (loop with start = 0
            for at = (search needle lower :start2 start)
            while at
            do (push at positions)
               (setf start (+ at (length needle)))))
    (dolist (at (sort positions #'<))
      (let ((best nil))
        (dolist (word *secret-env-words*)
          (let ((word-at (search word (window lower at 160))))
            (when (and word-at (not (member word *standard-env* :test #'string=))
                       (or (null best) (< word-at (cdr best))))
              (setf best (cons word word-at)))))
        (when best (return (cons at (car best))))))))

(defun scan-file (path root)
  "Every finding one file yields, its path relative to ROOT, and whether the
file was read at all -- a second value, because a binary or an oversized
file counts as unread rather than as a finding."
  (let ((relative (enough-namestring path root))
        (name (string-downcase (file-namestring path)))
        (type (string-downcase (or (pathname-type path) "")))
        (findings '()))
    (when (or (member name *sensitive-names* :test #'string=)
              (member type *sensitive-suffixes* :test #'string=))
      (push (finding "PG201" relative 1
                     (format nil "~a usually holds credentials" name))
            findings))
    (let ((text (read-text path)))
      (unless text (return-from scan-file (values '() nil)))
      (when text
        (let ((lower (string-downcase text)))
          (dolist (pair *secret-needles*)
            (let ((at (search (car pair) lower)))
              (when at
                (push (finding "PG201" relative (line-of text at)
                               (format nil "~a: ~a" (cdr pair) (car pair)))
                      findings))))
          (when (member type *code-types* :test #'string=)
            (dolist (rule *code-rules*)
              (let ((hit (loop for needle in (second rule)
                               for at = (search needle lower)
                               when at return (cons needle at))))
                (when hit
                  (push (finding (first rule) relative (line-of text (cdr hit))
                                 (format nil "~a: ~a"
                                         (rule-description (first rule)) (car hit)))
                        findings))))
            (let ((hit (secret-env-hit lower)))
              (when hit
                (push (finding "PG107" relative (line-of text (car hit))
                               (format nil "~a: an environment read names a credential"
                                       (rule-description "PG107")))
                      findings)))))))
    (values (nreverse findings) t)))

;;; --- the folder itself -----------------------------------------------------------

(defun file-present-p (root name)
  (and (probe-file (merge-pathnames name root)) t))

(defun declaration-p (root)
  "Whether ROOT declares a plugin or a cell at all."
  (or (file-present-p root "plugin.yaml")
      (file-present-p root "plugin.yml")
      (file-present-p root "pyproject.toml")
      (file-present-p root "package.json")
      (some (lambda (path) path)
            (ignore-errors (uiop:directory-files root "*.asd")))))

(defun test-file-p (path)
  (let ((name (string-downcase (file-namestring path))))
    (or (uiop:string-prefix-p "test_" name)
        (uiop:string-prefix-p "test-" name)
        (search "-test." name)
        (search "_test." name))))

(defun tests-present-p (root files)
  "Whether ROOT carries a test folder or a test-named file of its own."
  (or (and (ignore-errors
             (some (lambda (sub)
                     (member (string-downcase (directory-name sub)) '("test" "tests")
                             :test #'string=))
                   (uiop:subdirectories root)))
           t)
      (some #'test-file-p files)))

(defun hygiene-findings (root files)
  "What the folder lacks, as findings against it."
  (let ((findings '()))
    (unless (declaration-p root)
      (push (finding "PG001" "." 1 (rule-description "PG001")) findings))
    (unless (or (file-present-p root "LICENSE") (file-present-p root "LICENSE.md")
                (file-present-p root "LICENSE.txt") (file-present-p root "COPYING"))
      (push (finding "PG002" "." 1 (rule-description "PG002")) findings))
    (unless (file-present-p root "SECURITY.md")
      (push (finding "PG003" "." 1 (rule-description "PG003")) findings))
    (unless (tests-present-p root files)
      (push (finding "PG004" "." 1 (rule-description "PG004")) findings))
    findings))

;;; --- dependencies ----------------------------------------------------------------

(defun hex-string-p (text)
  (and (plusp (length text)) (every (lambda (c) (digit-char-p c 16)) text)))

(defun pinned-p (line)
  "Whether LINE carries a 40-hex commit id of its own."
  (loop with length = (length line)
        for index from 0 to (- length 40)
        thereis (and (hex-string-p (subseq line index (+ index 40)))
                     (or (zerop index)
                         (not (alphanumericp (char line (1- index))))))))

(defun bounded-p (line)
  "Whether a dependency LINE carries an upper bound."
  (or (search "==" line) (search "~=" line) (search "<" line) (search "!=" line)))

(defun requirement-findings (path relative)
  "PG202 and PG203 over one requirements file."
  (let ((findings '())
        (line-number 0))
    (dolist (line (ignore-errors (uiop:read-file-lines path)))
      (incf line-number)
      (let* ((trimmed (string-trim '(#\Space #\Tab #\Return) line))
             (lower (string-downcase trimmed)))
        (unless (or (zerop (length trimmed)) (char= (char trimmed 0) #\#)
                    (uiop:string-prefix-p "-r" trimmed)
                    (uiop:string-prefix-p "--requirement" trimmed))
          (cond
            ((or (search "git+" lower) (search "http://" lower) (search "https://" lower))
             (unless (pinned-p lower)
               (push (finding "PG202" relative line-number
                              (format nil "~a: ~a" (rule-description "PG202")
                                      (window trimmed 0 80)))
                     findings)))
            ((not (bounded-p lower))
             (push (finding "PG203" relative line-number
                            (format nil "~a: ~a" (rule-description "PG203")
                                    (window trimmed 0 80)))
                   findings))))))
    (nreverse findings)))

(defun install-findings (path relative)
  "PG204 and PG202 over the remote installers a manifest or a build file names."
  (let ((findings '())
        (line-number 0))
    (dolist (line (ignore-errors (uiop:read-file-lines path)))
      (incf line-number)
      (let ((lower (string-downcase line)))
        (when (and (or (search "curl " lower) (search "wget " lower))
                   (search "http" lower)
                   (search "|" lower)
                   (or (search "sh" lower) (search "iex" lower)))
          (push (finding "PG204" relative line-number
                         (format nil "~a: ~a" (rule-description "PG204")
                                 (window (string-trim '(#\Space #\Tab) line) 0 80)))
                findings))
        (when (and (search "git+" lower) (not (pinned-p lower)))
          (push (finding "PG202" relative line-number
                         (format nil "~a: ~a" (rule-description "PG202")
                                 (window (string-trim '(#\Space #\Tab) line) 0 80)))
                findings))))
    findings))

(defun dependency-findings (root)
  "The dependency findings of ROOT's own declaration files."
  (let ((findings '()))
    (dolist (path (ignore-errors (uiop:directory-files root)))
      (let* ((name (string-downcase (file-namestring path)))
             (relative (file-namestring path))
             (type (string-downcase (or (pathname-type path) ""))))
        (cond
          ((and (uiop:string-prefix-p "requirements" name) (string= type "txt"))
           (setf findings (append (requirement-findings path relative) findings)))
          ((member name '("plugin.yaml" "plugin.yml" "pyproject.toml" "action.yml"
                          "action.yaml")
                   :test #'string=)
           (setf findings (append (install-findings path relative) findings))))))
    findings))

;;; --- the scan --------------------------------------------------------------------

(defun resolve-directory (path)
  "PATH as an absolute directory pathname, or a refusal."
  (let* ((named (uiop:parse-native-namestring (or path "")))
         (full (if (uiop:absolute-pathname-p named)
                   named
                   (merge-pathnames named (uiop:getcwd)))))
    (unless (uiop:directory-exists-p full)
      (fail "~a is not a directory: name the folder to review" path))
    (uiop:ensure-directory-pathname (truename full))))

(defun sorted-findings (findings)
  "FINDINGS loudest first, then by path, line and rule."
  (sort findings
        (lambda (left right)
          (let ((rank-left (finding-rank left))
                (rank-right (finding-rank right)))
            (cond ((/= rank-left rank-right) (> rank-left rank-right))
                  ((string/= (getf left :path) (getf right :path))
                   (string< (getf left :path) (getf right :path)))
                  ((/= (getf left :line) (getf right :line))
                   (< (getf left :line) (getf right :line)))
                  (t (string< (getf left :rule) (getf right :rule))))))))

(defun scan-directory (path &key (min-severity "high") (max-findings 20)
                                 (max-files 500) exclude)
  "Review the tree at PATH, reading its files and never running them. =>
a plist: :LABEL the folder's name, :ROOT its pathname, :FILES read, :UNREAD
skipped, :TOTAL findings, :FINDINGS at most MAX-FINDINGS of them loudest
first, :FAILING how many reach MIN-SEVERITY, :THRESHOLD MIN-SEVERITY.
EXCLUDE names rule ids to leave out."
  (let* ((root (resolve-directory path))
         (files (walk root :limit max-files))
         (threshold (string-downcase (or min-severity "high")))
         (excluded (mapcar (lambda (id) (string-upcase (string id))) exclude))
         (findings '())
         (unread 0))
    (dolist (file files)
      (multiple-value-bind (found read) (scan-file file root)
        (if read
            (setf findings (append found findings))
            (incf unread))))
    (setf findings (append (dependency-findings root) findings))
    (setf findings (append (hygiene-findings root files) findings))
    (setf findings (remove-if (lambda (one) (member (getf one :rule) excluded
                                                    :test #'string=))
                              findings))
    (let* ((sorted (sorted-findings findings))
           (reach (count-if (lambda (one)
                              (>= (finding-rank one) (severity-rank threshold)))
                            sorted)))
      (list :label (directory-name root)
            :root root
            :files (- (length files) unread)
            :unread unread
            :total (length sorted)
            :findings (if (and (integerp max-findings) (plusp max-findings))
                          (subseq sorted 0 (min max-findings (length sorted)))
                          sorted)
            :counts (severity-counts sorted)
            :failing reach
            :threshold threshold))))

;;; --- the report -------------------------------------------------------------------

(defun severity-counts (findings)
  "FINDINGS counted by severity, loudest first, as \"1 critical, 2 high\"; \"\" when none."
  (let ((parts '()))
    (dolist (name (reverse *severities*))
      (let ((count (count name findings
                           :key (lambda (one) (getf one :severity)) :test #'string=)))
        (when (plusp count)
          (push (format nil "~d ~a" count name) parts))))
    (if parts (format nil ": ~{~a~^, ~}" (nreverse parts)) "")))

(defun report-text (result)
  "RESULT, a SCAN-DIRECTORY plist, as the text a person and the model read."
  (with-output-to-string (out)
    (format out "plugin-guard: ~a -- ~d file~:p read~@[, ~d unread~], ~d finding~:p~a~%"
            (getf result :label) (getf result :files) (getf result :unread)
            (getf result :total) (or (getf result :counts) ""))
    (if (plusp (getf result :failing))
        (format out "FAIL at ~a: ~d finding~:p at or above it~%"
                (getf result :threshold) (getf result :failing))
        (format out "ok: nothing at or above ~a~%" (getf result :threshold)))
    (dolist (one (getf result :findings))
      (format out "  ~10a ~5a ~a:~d - ~a~%"
              (string-upcase (getf one :severity)) (getf one :rule) (getf one :path)
              (getf one :line) (getf one :message)))
    (when (> (getf result :total) (length (getf result :findings)))
      (format out "  ... ~d more finding~:p; raise max-findings to read them all~%"
              (- (getf result :total) (length (getf result :findings)))))
    (format out "read as text, never run: a review aid, not a verdict.~%")))
