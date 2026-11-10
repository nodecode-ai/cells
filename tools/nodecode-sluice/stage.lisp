;;;; stage.lisp --- a copy of the workspace, and the baseline it is compared to.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing runs the agent's work here; this is the store. A stage is two
;;;; trees under the nodecode home -- STAGE, what the agent works in, and
;;;; BASE, the copy as it was taken -- plus a manifest of digests that says
;;;; whether the real workspace has moved underneath the review. The plugin
;;;; kept the same two trees under the Hermes home and hashed with SHA-256;
;;;; this keeps them under <NODECODE_HOME>/plugin-data/sluice/ and hashes
;;;; with the MD5 SBCL carries, which is a change detector and not a signature.

(in-package #:nodecode-sluice)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-md5))

;; Bumped when a manifest's shape changes; an older stage is simply retaken.
(defparameter *manifest-version* 1)

(defparameter *skip-dirs*
  '("node_modules" "__pycache__" ".venv" "venv" ".pytest_cache" ".mypy_cache"
    ".ruff_cache" ".tox" ".gradle")
  "Dependency and cache directories never copied, as the plugin skipped them.")

(defparameter *max-file-bytes* (* 25 1024 1024))
(defparameter *max-total-bytes* (* 1024 1024 1024))
(defparameter *stale-days* 14)

;;; --- paths -----------------------------------------------------------------

(defun native (path)
  "PATH as a native namestring."
  (uiop:native-namestring path))

(defun strip-slash (text)
  (string-right-trim "/" text))

(defun path-join (root relative)
  "ROOT and RELATIVE joined with one separator, as a namestring."
  (format nil "~a/~a" (strip-slash root) relative))

(defun directory-of (relative)
  "The directory part of RELATIVE, or NIL."
  (let ((slash (position #\/ relative :from-end t)))
    (and slash (subseq relative 0 slash))))

(defun normalize-path (base relative)
  "RELATIVE read against the absolute directory BASE, .. folded in."
  (let ((stack '()))
    (dolist (part (uiop:split-string relative :separator "/"))
      (cond ((or (zerop (length part)) (equal part ".")) nil)
            ((equal part "..") (when stack (pop stack)))
            (t (push part stack))))
    (let ((tail (format nil "~{~a~^/~}" (reverse stack))))
      (if (zerop (length tail))
          (strip-slash base)
          (format nil "~a/~a" (strip-slash base) tail)))))

(defun path-under-p (path root)
  (let ((path (strip-slash path)) (root (strip-slash root)))
    (or (equal path root)
        (uiop:string-prefix-p (format nil "~a/" root) path))))

(defun real-directory (path)
  "PATH as an absolute directory namestring with no trailing separator, symlinks
resolved, or NIL when it names no directory."
  (let* ((text (string-trim '(#\Space #\Tab) (if (stringp path) path (native path))))
         (text (if (uiop:string-prefix-p "~/" text)
                   (native (nlk:expand-home text))
                   text)))
    (when (and (plusp (length text))
               (uiop:directory-exists-p (uiop:parse-native-namestring text)))
      (strip-slash (native (truename (uiop:ensure-directory-pathname
                                       (uiop:parse-native-namestring text))))))))

(defun hex-of (bytes)
  (format nil "~{~2,'0x~}" (coerce bytes 'list)))

(defun digest-string (text)
  "A digest of TEXT's UTF-8 bytes, as hex: change detection, not a signature."
  (hex-of (sb-md5:md5sum-sequence (sb-ext:string-to-octets text :external-format :utf-8))))

(defun digest-file (path)
  (ignore-errors (hex-of (sb-md5:md5sum-file path))))

;;; --- entries ---------------------------------------------------------------

(defstruct (entry (:constructor make-entry (kind &key mode digest target)))
  kind    ; :file or :link
  mode    ; the permission bits of a regular file
  digest  ; the hex digest of its bytes, NIL for a link
  target) ; a link's target

(defun symlink-p (path)
  (let ((st (ignore-errors (sb-posix:lstat path))))
    (and st (sb-posix:s-islnk (sb-posix:stat-mode st)))))

(defun entry-for (path)
  "What PATH is, as an ENTRY, or NIL when it is missing or neither file nor link."
  (let ((st (ignore-errors (sb-posix:lstat path))))
    (when st
      (let ((mode (sb-posix:stat-mode st)))
        (cond ((sb-posix:s-islnk mode)
               (make-entry :link :target (ignore-errors (sb-posix:readlink path))))
              ((sb-posix:s-isreg mode)
               (make-entry :file :mode (logand mode #o7777) :digest (digest-file path)))
              (t nil))))))

(defun same-entry-p (a b)
  (and a b
       (eq (entry-kind a) (entry-kind b))
       (equal (entry-digest a) (entry-digest b))
       (equal (entry-target a) (entry-target b))))

(defun link-stays-inside-p (root relative target)
  "Whether a link at RELATIVE pointing at TARGET stays inside ROOT."
  (and target
       (plusp (length target))
       (not (absolute-path-string-p target))
       (let ((dir (directory-of relative)))
         (path-under-p (normalize-path (if dir (path-join root dir) root) target) root))))

(defun absolute-path-string-p (target)
  "Whether TARGET is written as an absolute path: what a link may not be, here."
  (and (plusp (length target)) (char= #\/ (char target 0))))

;;; --- what belongs to the workspace -----------------------------------------

(defun glob-p (pattern text)
  "PATTERN with * and ? matched against TEXT, as a .gitignore line means them."
  (cond ((zerop (length pattern)) (zerop (length text)))
        ((char= #\* (char pattern 0))
         (or (glob-p (subseq pattern 1) text)
             (and (plusp (length text)) (glob-p pattern (subseq text 1)))))
        ((zerop (length text)) nil)
        ((or (char= #\? (char pattern 0))
             (char-equal (char pattern 0) (char text 0)))
         (glob-p (subseq pattern 1) (subseq text 1)))
        (t nil)))

(defun read-gitignore (root)
  "The patterns of ROOT's .gitignore, as written, or NIL."
  (let ((file (path-join root ".gitignore")))
    (when (uiop:file-exists-p file)
      (with-open-file (in file :external-format :utf-8 :if-does-not-exist nil)
        (when in
          (loop for line = (read-line in nil nil)
                while line
                for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
                unless (or (zerop (length trimmed))
                           (char= #\# (char trimmed 0))
                           (char= #\! (char trimmed 0)))
                  collect trimmed))))))

(defun pattern-matches-p (pattern relative)
  "Whether one .gitignore PATTERN covers RELATIVE."
  (let* ((anchored (and (plusp (length pattern)) (char= #\/ (char pattern 0))))
         (body (strip-slash pattern)))
    (when (plusp (length body))
      (cond ((or anchored (find #\/ body))
             (or (glob-p body relative)
                 (uiop:string-prefix-p (format nil "~a/" body) relative)))
            (t (loop for part in (uiop:split-string relative :separator "/")
                     thereis (glob-p body part)))))))

(defun directory-entries (dir)
  "DIR's children as (NAME . PATH) pairs, a symlinked directory once."
  (let ((base (uiop:ensure-directory-pathname (format nil "~a/" (strip-slash dir))))
        (seen (make-hash-table :test #'equal))
        (pairs '()))
    (dolist (file (ignore-errors (uiop:directory-files base)))
      (push (cons (file-namestring file) (strip-slash (native file))) pairs))
    (dolist (sub (ignore-errors (uiop:subdirectories base)))
      (let ((name (car (last (pathname-directory sub)))))
        (when (stringp name)
          (pushnew (cons name (strip-slash (native sub))) pairs :key #'car :test #'equal))))
    (loop for pair in pairs
          unless (gethash (car pair) seen)
            do (setf (gethash (car pair) seen) t)
            and collect pair)))

(defun list-files (root)
  "The relative paths of the files and links that belong to ROOT, sorted.
.git is never listed; a .gitignore line and a dependency directory keep their
files out, as the plugin's listing did."
  (let ((out '())
        (patterns (read-gitignore root)))
    (labels ((walk (dir relative)
               (dolist (pair (sort (directory-entries dir) #'string< :key #'car))
                 (let* ((name (car pair))
                        (path (cdr pair))
                        (child (if (zerop (length relative))
                                   name
                                   (format nil "~a/~a" relative name))))
                   (cond ((string= name ".git") nil)
                         ((symlink-p path) (push child out))
                         ((uiop:directory-exists-p path)
                          (unless (member name *skip-dirs* :test #'string=)
                            (walk path child)))
                         ((uiop:file-exists-p path)
                          (unless (loop for pattern in patterns
                                        thereis (pattern-matches-p pattern child))
                            (push child out))))))))
      (walk root ""))
    (sort out #'string<)))

(defun scan (root)
  "ROOT's files as a table of relative path to ENTRY."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (relative (list-files root) table)
      (let ((entry (entry-for (path-join root relative))))
        (when entry (setf (gethash relative table) entry))))))

;;; --- the stage ---------------------------------------------------------------

(defstruct (stage (:constructor make-stage (root dir)))
  root      ; the real workspace, a namestring with no trailing separator
  dir       ; this stage's own directory, ending in a separator
  (manifest (make-hash-table :test #'equal)) ; relative path -> the ENTRY as taken
  (skipped '())
  (created 0)
  (last-rev ""))

(defun stage-path (stage) (format nil "~astage" (stage-dir stage)))
(defun base-path (stage) (format nil "~abase" (stage-dir stage)))
(defun stage-name (stage) (car (last (pathname-directory (stage-dir stage)))))

(defun state-root ()
  "Where stages live: <NODECODE_HOME>/plugin-data/sluice/."
  (strip-slash (native (nlk:home "plugin-data/sluice/"))))

(defun stage-id (root)
  (subseq (digest-string root) 0 12))

(defun manifest-lines (stage)
  (sort (loop for relative being the hash-keys of (stage-manifest stage)
                using (hash-value entry)
              collect (list relative (entry-kind entry) (or (entry-mode entry) 0)
                            (entry-digest entry) (entry-target entry)))
        #'string< :key #'car))

(defun save-stage (stage)
  (let ((data (list :version *manifest-version* :root (stage-root stage)
                    :created (stage-created stage) :skipped (stage-skipped stage)
                    :last-rev (stage-last-rev stage)
                    :manifest (manifest-lines stage))))
    (with-open-file (out (format nil "~ameta.sexp" (stage-dir stage))
                         :direction :output :if-exists :supersede :external-format :utf-8)
      (let ((*print-length* nil) (*print-level* nil))
        (with-standard-io-syntax (prin1 data out))))
    t))

(defun load-stage-dir (dir)
  "The stage kept in DIR, or NIL when there is none to load."
  (let ((file (format nil "~ameta.sexp" dir)))
    (when (uiop:file-exists-p file)
      (let ((data (with-open-file (in file :external-format :utf-8 :if-does-not-exist nil)
                    (when in
                      (let ((*read-eval* nil))
                        (with-standard-io-syntax (read in nil nil)))))))
        (when (and (listp data) (eql *manifest-version* (getf data :version)))
          (let ((stage (make-stage (getf data :root) dir)))
            (setf (stage-created stage) (getf data :created)
                  (stage-skipped stage) (getf data :skipped)
                  (stage-last-rev stage) (or (getf data :last-rev) ""))
            (loop for (relative kind mode digest target) in (getf data :manifest)
                  do (setf (gethash relative (stage-manifest stage))
                           (make-entry kind :mode mode :digest digest :target target)))
            (when (and (uiop:directory-exists-p (format nil "~astage" dir))
                       (uiop:directory-exists-p (format nil "~abase" dir)))
              stage)))))))

(defun current-name ()
  (let ((file (format nil "~a/current" (state-root))))
    (when (uiop:file-exists-p file)
      (string-trim '(#\Space #\Newline #\Return)
                   (with-open-file (in file :external-format :utf-8 :if-does-not-exist nil)
                     (or (and in (read-line in nil "")) ""))))))

(defun current-stage ()
  (let ((name (current-name)))
    (and name (plusp (length name))
         (load-stage-dir (format nil "~a/~a/" (state-root) name)))))

(defun mark-current (stage)
  (ensure-directories-exist (format nil "~a/" (state-root)))
  (with-open-file (out (format nil "~a/current" (state-root))
                       :direction :output :if-exists :supersede :external-format :utf-8)
    (format out "~a" (stage-name stage)))
  stage)

(defun discard-stage (stage)
  (ignore-errors (uiop:delete-directory-tree
                  (uiop:ensure-directory-pathname (stage-dir stage)) :validate t))
  (when (equal (current-name) (stage-name stage))
    (ignore-errors (delete-file (format nil "~a/current" (state-root)))))
  t)

(defun prune-stale (&key keep (days *stale-days*))
  "Remove stages nobody has touched in DAYS days, except the one named KEEP."
  (let ((cutoff (- (get-universal-time) (* days 86400)))
        (removed 0))
    (dolist (dir (ignore-errors (uiop:subdirectories
                                  (uiop:ensure-directory-pathname (state-root)))))
      (let ((name (car (last (pathname-directory dir)))))
        (unless (equal name keep)
          (let ((touched (or (ignore-errors (file-write-date (format nil "~ameta.sexp" (native dir)))) 0)))
            (when (< touched cutoff)
              (ignore-errors (uiop:delete-directory-tree dir :validate t))
              (incf removed))))))
    removed))

;;; --- taking one ---------------------------------------------------------------

(defun copy-file-entry (source destination entry)
  (ensure-directories-exist destination)
  (if (eq (entry-kind entry) :link)
      (sb-posix:symlink (entry-target entry) destination)
      (progn (uiop:copy-file source destination)
             (ignore-errors (sb-posix:chmod destination (entry-mode entry))))))

(defun copy-tree-into (source destination)
  "SOURCE copied under DESTINATION, link for link."
  (ensure-directories-exist (format nil "~a/" (strip-slash destination)))
  (dolist (pair (directory-entries source))
    (let ((from (cdr pair)) (to (path-join destination (car pair))))
      (cond ((symlink-p from) (ignore-errors (sb-posix:symlink (sb-posix:readlink from) to)))
            ((uiop:directory-exists-p from) (copy-tree-into from to))
            (t (ignore-errors (uiop:copy-file from to)))))))

(defun create-stage (root &key (copy-git t)
                          (max-file-bytes *max-file-bytes*)
                          (max-total-bytes *max-total-bytes*))
  "Take a fresh stage of ROOT. => a STAGE."
  (let ((root (or (real-directory root) (fail "~a is not a directory" root))))
    (let* ((dir (format nil "~a/~a/" (state-root) (stage-id root)))
           (stage (make-stage root dir))
           (total 0))
      (ignore-errors (uiop:delete-directory-tree
                      (uiop:ensure-directory-pathname dir) :validate t))
      (ensure-directories-exist (format nil "~astage/" dir))
      (ensure-directories-exist (format nil "~abase/" dir))
      (setf (stage-created stage) (get-universal-time))
      (handler-case
          (progn
            (dolist (relative (list-files root))
              (let* ((source (path-join root relative))
                     (entry (entry-for source)))
                (cond
                  ((null entry) nil)
                  ((eq (entry-kind entry) :link)
                   (if (link-stays-inside-p root relative (entry-target entry))
                       (progn (copy-file-entry source (path-join (stage-path stage) relative) entry)
                              (copy-file-entry source (path-join (base-path stage) relative) entry))
                       (push (format nil "~a: symlink points outside the workspace" relative)
                             (stage-skipped stage))))
                  (t
                   (let ((size (or (ignore-errors
                                    (with-open-file (in source :element-type '(unsigned-byte 8))
                                      (file-length in)))
                                   0)))
                     (if (> size max-file-bytes)
                         (push (format nil "~a: larger than ~d MB" relative
                                       (floor max-file-bytes (* 1024 1024)))
                               (stage-skipped stage))
                         (progn
                           (incf total size)
                           (when (> total max-total-bytes)
                             (fail "the workspace is larger than ~d MB; point the sluice section's workspace at a smaller folder or raise max_workspace_mb"
                                   (floor max-total-bytes (* 1024 1024))))
                           (copy-file-entry source (path-join (base-path stage) relative) entry)
                           (copy-file-entry source (path-join (stage-path stage) relative) entry)
                           (setf (gethash relative (stage-manifest stage)) entry))))))))
            (when (and copy-git (uiop:directory-exists-p (path-join root ".git")))
              (copy-tree-into (path-join root ".git") (path-join (stage-path stage) ".git")))
            (save-stage stage)
            (mark-current stage)
            (prune-stale :keep (stage-name stage))
            stage)
        (error (condition)
          (ignore-errors (uiop:delete-directory-tree
                          (uiop:ensure-directory-pathname dir) :validate t))
          (error condition))))))

(defun open-or-create (root &rest options)
  "The pending stage for ROOT when there is one, else a fresh copy."
  (let ((stage (current-stage)))
    (if (and stage (equal (stage-root stage) (real-directory root)))
        (progn (mark-current stage) stage)
        (apply #'create-stage (real-directory root) options))))

(defun drift (stage)
  "Files whose real copy no longer matches the baseline."
  (loop for relative being the hash-keys of (stage-manifest stage)
          using (hash-value entry)
        unless (same-entry-p entry (entry-for (path-join (stage-root stage) relative)))
          collect relative))
