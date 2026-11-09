;;;; inspect.lisp --- bounded, read-only frontend inspection.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's hardlight/inspection.py, in Lisp: an allowlisted walk of
;;;; one directory, hidden/vendor/build/secret paths and symbolic links
;;;; skipped before any read, size and count budgets that truncate rather than
;;;; fail, the token declarations it finds, and three advisory checks. No
;;;; project code is executed or imported, and nothing is written.

(in-package #:nodecode-hardlight)

(defparameter +inspect-extensions+
  '(".html" ".htm" ".css" ".scss" ".sass" ".less" ".js" ".jsx" ".ts" ".tsx" ".vue" ".svelte")
  "The file types the walk reads.")

(defparameter +inspect-excluded+
  '("node_modules" "vendor" "dist" "build" "coverage" "out" "target" "bower_components"
    "__pycache__" "venv" "env" "secrets" "credentials")
  "Directories and names that are never walked into.")

(defparameter +inspect-file-limit+ (* 512 1024) "One file's ceiling.")
(defparameter +inspect-total-limit+ (* 8 1024 1024) "One call's ceiling, over every file.")
(defparameter +inspect-entry-limit+ 5000 "Entries one call may look at.")
(defparameter +inspect-directory-limit+ 128 "Directories one call may enter.")

(defparameter +inspect-secret-scanner+
  (cl-ppcre:create-scanner
   "(?:^|[._-])(?:secrets?|credentials?|tokens?|passwords?|private[-_]?keys?)(?:[._-]|$)"
   :case-insensitive-mode t)
  "A name that looks like it holds a secret is skipped before any read.")

(defparameter +inspect-token-scanner+
  (cl-ppcre:create-scanner "(?<![\\w-])(--[\\w-]+)\\s*:\\s*([^;{}\\n]+)")
  "The custom-property declarations the walk reports, with their value.")

(defparameter +inspect-image-scanner+ (cl-ppcre:create-scanner "<img\\b[^>]*>" :case-insensitive-mode t))
(defparameter +inspect-anchor-scanner+ (cl-ppcre:create-scanner "<a\\b[^>]*>" :case-insensitive-mode t))
(defparameter +inspect-alt-scanner+ (cl-ppcre:create-scanner "\\balt\\s*=" :case-insensitive-mode t))
(defparameter +inspect-href-scanner+ (cl-ppcre:create-scanner "\\bhref\\s*=\\s*[\"'](?:|#)[\"']" :case-insensitive-mode t))
(defparameter +inspect-outline-scanner+ (cl-ppcre:create-scanner "\\boutline\\s*:\\s*(?:none|0)\\s*[;}]" :case-insensitive-mode t))

(defparameter +inspect-limitations+
  '("Static advisory checks only; no project code, routes or styles are executed."
    "JSX/TSX, framework markup and CSS tokens are heuristic; comments and strings may match."
    "No cascade, visual quality, computed contrast, task success or WCAG conformance is established."
    "Only allowlisted UTF-8 frontend files are inspected; exclusions and budget truncation are not a clean pass."
    "Symbolic links inside the tree are skipped, so a linked folder is reported as skipped rather than followed.")
  "What one walk does not establish, said in the answer.")

;;; --- the boundary -----------------------------------------------------------

(nlk:define-contract inspect-call
  (:doc "Read-only bounded inspection of a frontend project: the files read, the CSS custom properties found, and advisory findings for missing image alt text, placeholder links and removed focus outlines. Never executes project code.")
  ("root" :string :default "" :doc "an existing local directory to walk; empty takes the section's root")
  ("max_files" :integer :default 0 :doc "1..500 files to read; 0 takes the section's default"))

;;; --- one file's text --------------------------------------------------------

(defun inspect-line-at (text position)
  "The 1-based line POSITION sits on in TEXT."
  (1+ (count #\Newline text :end (min position (length text)))))

(defun inspect-finding (path line rule evidence recovery method)
  (nlk:json-object "path" path "line" line "rule" rule "severity" "advisory"
                    "evidence" (subseq evidence 0 (min 240 (length evidence)))
                    "recovery" recovery "method" method))

(defun inspect-image-findings (rel text)
  "Every image tag with no alt attribute, and every anchor with an empty or
placeholder href, as findings in the order they appear."
  (let ((findings '()))
    (cl-ppcre:do-scans (start end reg-starts reg-ends +inspect-image-scanner+ text)
      (declare (ignore end reg-starts reg-ends))
      (let ((tag (subseq text start end)))
        (unless (cl-ppcre:scan +inspect-alt-scanner+ tag)
          (push (inspect-finding rel (inspect-line-at text start) "image-alt" tag
                                 "Supply meaningful alt text, or alt=\"\" for a confirmed decorative image."
                                 "tag-scan")
                findings))))
    (cl-ppcre:do-scans (start end reg-starts reg-ends +inspect-anchor-scanner+ text)
      (declare (ignore end reg-starts reg-ends))
      (let ((tag (subseq text start end)))
        (when (cl-ppcre:scan +inspect-href-scanner+ tag)
          (push (inspect-finding rel (inspect-line-at text start) "placeholder-link" tag
                                 "Use a real destination, or a button for a local action; test keyboard and Back behavior."
                                 "tag-scan")
                findings))))
    (nreverse findings)))

(defun inspect-scan-text (rel text)
  "The custom properties and the advisory findings of one file's TEXT, as
(values TOKENS FINDINGS)."
  (let ((tokens '()) (findings '()))
    (cl-ppcre:do-scans (start end reg-starts reg-ends +inspect-token-scanner+ text)
      (declare (ignore end))
      (let ((name (subseq text (aref reg-starts 0) (aref reg-ends 0)))
            (value (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (subseq text (aref reg-starts 1) (aref reg-ends 1)))))
        (push (nlk:json-object "name" name "value" value "path" rel
                                "line" (inspect-line-at text start) "method" "heuristic")
              tokens)))
    (when (member (string-downcase (or (pathname-type rel) "")) '("html" "htm") :test #'equal)
      (setf findings (append (inspect-image-findings rel text) findings)))
    (cl-ppcre:do-scans (start end reg-starts reg-ends +inspect-outline-scanner+ text)
      (declare (ignore end reg-starts reg-ends))
      (push (inspect-finding rel (inspect-line-at text start) "focus-outline" (subseq text start end)
                             "Verify an equally visible replacement focus indicator; do not remove focus without one."
                             "heuristic")
            findings))
    (values (nreverse tokens) findings)))

;;; --- the walk ---------------------------------------------------------------

(defun inspect-namestring (path)
  (string-right-trim "/" (sb-ext:native-namestring path)))

(defun inspect-link-p (path)
  "PATH is a symbolic link, judged by where it resolves and where it is."
  (let ((true (ignore-errors (truename path))))
    (and true (not (equal (inspect-namestring true) (inspect-namestring path))))))

(defun inspect-entry-name (entry)
  "ENTRY's own name, with its extension, read off where it is."
  (let* ((string (string-right-trim "/" (sb-ext:native-namestring entry)))
         (slash (position #\/ string :from-end t)))
    (subseq string (if slash (1+ slash) 0))))

(defun inspect-entries (directory)
  "Every entry of DIRECTORY, sorted by name."
  (sort (copy-list (directory (merge-pathnames (make-pathname :name :wild :type :wild) directory)))
        #'string< :key (lambda (entry) (or (pathname-name entry) ""))))

(defun inspect-read-file (path)
  "PATH's octets when it is at most the file ceiling, else (values NIL WHY)."
  (handler-case
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let ((size (file-length in)))
          (cond ((> size +inspect-file-limit+) (values nil "file exceeds 512 KiB"))
                (t (let ((data (make-array size :element-type '(unsigned-byte 8))))
                     (read-sequence data in)
                     (values data nil))))))
    (error () (values nil "unreadable or changed"))))

(defun inspect-project (root max-files)
  "hardlight_inspect: ROOT walked, at most MAX-FILES read, as JSON."
  (let ((start (and (stringp root) (plusp (length (string-trim " " root))) root)))
    (cond
      ((null start) (nlk:encode-json-object (nlk:json-object "ok" nil "error" "root must be an existing local directory path")))
      (t
       (let ((resolved (ignore-errors (truename (uiop:ensure-directory-pathname start)))))
         (cond
           ((or (null resolved) (not (uiop:directory-exists-p resolved)))
            (nlk:encode-json-object
             (nlk:json-object "ok" nil "error" "root must be an existing local directory path")))
           (t
            (let ((entries-seen 0) (directories-seen 0) (bytes-read 0) (budget-hit nil)
                  (inventory '()) (tokens '()) (findings '()) (skipped '()))
              (labels ((skip (rel reason) (push (nlk:json-object "path" rel "reason" reason) skipped))
                       (walk (directory relative)
                         (when (>= directories-seen +inspect-directory-limit+)
                           (setf budget-hit t)
                           (skip (or relative ".") "directory budget")
                           (return-from walk nil))
                         (incf directories-seen)
                         (let ((batch (inspect-entries directory)))
                           (when (> (+ entries-seen (length batch)) +inspect-entry-limit+)
                             (setf batch (subseq batch 0 (max 0 (- +inspect-entry-limit+ entries-seen))))
                             (setf budget-hit t)
                             (skip (or relative ".") "entry budget"))
                           (incf entries-seen (length batch))
                           (dolist (entry batch)
                             (let* ((name (inspect-entry-name entry))
                                    (lower (string-downcase name))
                                    (rel (if relative (format nil "~a/~a" relative name) name))
                                    (extension (let ((dot (position #\. name :from-end t)))
                                                 (if dot (string-downcase (subseq name dot)) ""))))
                               (cond
                                 ((or (and (plusp (length lower)) (char= #\. (char lower 0)))
                                      (member lower +inspect-excluded+ :test #'equal)
                                      (cl-ppcre:scan +inspect-secret-scanner+ name))
                                  (skip rel "hidden, secret, vendor or build path"))
                                 ((inspect-link-p entry) (skip rel "symlink"))
                                 ((uiop:directory-exists-p entry)
                                  (walk entry rel))
                                 ((not (uiop:file-exists-p entry)) (skip rel "not a regular file"))
                                 ((not (member extension +inspect-extensions+ :test #'equal))
                                  (skip rel "unsupported extension"))
                                 ((>= (length inventory) max-files)
                                  (setf budget-hit t)
                                  (skip rel "file count budget"))
                                 (t
                                  (multiple-value-bind (data why) (inspect-read-file entry)
                                    (cond
                                      (why (skip rel why))
                                      ((> (+ bytes-read (length data)) +inspect-total-limit+)
                                       (setf budget-hit t) (skip rel "total byte budget"))
                                      ((find 0 data) (skip rel "binary content"))
                                      (t
                                       (let ((text (ignore-errors
                                                     (sb-ext:octets-to-string data :external-format :utf-8))))
                                         (if (null text)
                                             (skip rel "unsupported encoding; requires UTF-8")
                                             (progn
                                               (incf bytes-read (length data))
                                               (push (nlk:json-object "path" rel "bytes" (length data)
                                                                       "extension" extension)
                                                     inventory)
                                               (multiple-value-bind (found-tokens found-findings)
                                                   (inspect-scan-text rel text)
                                                 (dolist (token found-tokens) (push token tokens))
                                                 (dolist (finding found-findings) (push finding findings))))))))))))))))
                (walk resolved nil)
                (nlk:encode-json-object
                 (nlk:json-object
                  "ok" t
                  "root" (sb-ext:native-namestring resolved)
                  "inventory" (coerce (nreverse inventory) 'vector)
                  "tokens" (coerce (nreverse tokens) 'vector)
                  "findings" (coerce (nreverse findings) 'vector)
                  "skipped" (coerce (nreverse skipped) 'vector)
                  "coverage" (nlk:json-object
                              "partial" (or (and skipped t) budget-hit)
                              "budget_hit" (and budget-hit t)
                              "entries_seen" entries-seen
                              "directories_seen" directories-seen
                              "bytes_read" bytes-read
                              "files_read" (length inventory)
                              "max_files" max-files
                              "limits" (nlk:json-object "file_bytes" +inspect-file-limit+
                                                         "total_bytes" +inspect-total-limit+
                                                         "entries" +inspect-entry-limit+
                                                         "directories" +inspect-directory-limit+)
                              "limitations" (coerce (copy-list +inspect-limitations+) 'vector)))))))))))))
