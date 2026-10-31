;;;; detect.lisp --- what is installed here, and whether it is a fixed build.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One check is one JSON object of the feed, its KIND naming how it reads
;;;; the machine. Every kind answers the same plist,
;;;;   (:status :vulnerable|:ok|:absent|:skipped|:error :found TEXT :why TEXT)
;;;; and every kind reads through *RUN* or the file system, so a test stubs
;;;; one name and never touches the machine.
;;;;
;;;;   version    run COMMAND, read the first version in its output
;;;;   npm        a package's version in DIR's package-lock.json / node_modules
;;;;   composer   a package's version in DIR's composer.lock
;;;;   wp-plugin  a WordPress plugin's Version: header under DIR/wp-content/plugins
;;;;   command    run COMMAND; output (or output matching MATCH) is a hit
;;;;   files      any path matching one of PATHS is a hit
;;;;
;;;; A version is judged against the check's own lists: BAD (exact builds,
;;;; "*" for any), MIN (older is unaffected) and FIXED (one entry per
;;;; release line). A version on a line FIXED names is vulnerable below that
;;;; entry; a version on a line it does not name is vulnerable when older
;;;; than the newest fix (an end-of-life line) and fine when newer. A line is
;;;; the first LINE parts of a version: 2 by default (PHP 8.3, GitLab 19.4),
;;;; 1 where a project numbers its lines by major alone (Node 24).
;;;;
;;;; What a check runs never comes from the fetch. The live feed is read from
;;;; `main', which an unattended job writes from web news (UPDATING.md), so it
;;;; is data: it decides which advisories there are and what their versions
;;;; mean, never which program runs. A `version' or `command' check runs a
;;;; program only when this installed copy vouches for it, one of two ways:
;;;;
;;;;   a probe   the Python-package version check, its package name the one
;;;;             thing read from the feed: checked as a package name, and
;;;;             handed to a script of ours as an argument, never as code
;;;;   pinned    a command the advisories.json beside this file carries word
;;;;             for word -- the copy git pinned with the cell, which only an
;;;;             update moves, so a command reached it through a release
;;;;
;;;; Any other command is :SKIPPED and says why; the next nodecode update
;;;; brings it. A site folder or host name filled into a pinned command is
;;;; checked first, so neither can close the quotes it sits in.

(in-package #:nodecode-cyber-security)

(defun run-command (argv)
  "Run ARGV with no shell of ours => (VALUES OUTPUT EXIT). A program that
is not installed answers exit 127, as a shell would."
  (handler-case
      (multiple-value-bind (out err code)
          (uiop:run-program argv :output :string :error-output nil
                                 :ignore-error-status t :input nil)
        (declare (ignore err))
        (values (or out "") code))
    (error () (values "" 127))))

(defparameter *run* 'run-command
  "How a check runs a command: (funcall *RUN* ARGV) => (VALUES OUTPUT EXIT).")

;;; --- versions ------------------------------------------------------------------

(defun parse-version (text)
  "The first dotted version in TEXT as a list of integers, or NIL."
  (let ((m (and text (cl-ppcre:scan-to-strings "\\d+(?:\\.\\d+)+" text))))
    (when m (mapcar #'parse-integer (cl-ppcre:split "\\." m)))))

(defun version< (a b)
  "A is an older version than B (lists of integers, missing parts are 0)."
  (loop for i from 0 below (max (length a) (length b))
        for x = (or (nth i a) 0) and y = (or (nth i b) 0)
        when (< x y) return t
        when (> x y) return nil))

(defun version-string (v) (format nil "~{~d~^.~}" v))

(defun same-line-p (a b &optional (depth 2))
  "A and B share a release line: their first DEPTH parts agree (2 for PHP's
8.3, 1 for Node's 24)."
  (loop for i below depth always (eql (or (nth i a) 0) (or (nth i b) 0))))

(defun judge (version check)
  "VERSION (a list) against CHECK's bad/min/fixed => (VALUES STATUS WHY)."
  (let ((bad (field check "bad"))
        (min (parse-version (field check "min")))
        (fixed (remove nil (mapcar #'parse-version (field check "fixed"))))
        (shown (version-string version)))
    (cond ((member "*" bad :test #'equal)
           (values :vulnerable (format nil "~a is installed" shown)))
          ((some (lambda (b) (equal (parse-version b) version)) bad)
           (values :vulnerable (format nil "~a is the bad build" shown)))
          ((and min (version< version min))
           (values :ok (format nil "~a predates the affected range" shown)))
          ((null fixed)
           (values :ok (format nil "~a is not a bad build" shown)))
          (t
           (let ((line (let ((depth (field check "line" 2)))
                         (find-if (lambda (f) (same-line-p f version depth)) fixed)))
                 (newest (reduce (lambda (a b) (if (version< a b) b a)) fixed)))
             (cond (line
                    (if (version< version line)
                        (values :vulnerable (format nil "~a < ~a" shown (version-string line)))
                        (values :ok (format nil "~a >= ~a" shown (version-string line)))))
                   ((version< version newest)
                    (values :vulnerable (format nil "~a is on a line with no fix; move to ~{~a~^ / ~}"
                                                shown (mapcar #'version-string fixed))))
                   (t (values :ok (format nil "~a is newer than every fix" shown)))))))))

;;; --- placeholders -------------------------------------------------------------

(defun fill-in (string dir host)
  "STRING with {dir} and {host} replaced."
  (let ((s string))
    (when dir (setf s (cl-ppcre:regex-replace-all "\\{dir\\}" s (string-right-trim "/" dir))))
    (when host (setf s (cl-ppcre:regex-replace-all "\\{host\\}" s host)))
    s))

(defparameter +host-pattern+
  "^[A-Za-z0-9](?:[A-Za-z0-9-]{0,62})(?:\\.[A-Za-z0-9](?:[A-Za-z0-9-]{0,62}))*$"
  "A host name or IPv4 address: the only text filled in for {host}.")

(defun placeholder-problem (argv dir host)
  "Why DIR or HOST may not be filled into ARGV, or NIL. Commands quote
{dir} and {host} for a shell, so a value carrying a quote, a dollar sign,
a backquote, a backslash or a line break could end the quotes and run
what follows."
  (flet ((uses (name) (some (lambda (a) (search name a)) argv)))
    (cond ((and host (uses "{host}") (not (cl-ppcre:scan +host-pattern+ host)))
           (format nil "~s is not a host name" host))
          ((and dir (uses "{dir}") (find-if (lambda (c) (find c "'\"`$\\")) dir))
           (format nil "the folder ~s holds a quote or shell character" dir))
          ((and dir (uses "{dir}") (find-if (lambda (c) (member c '(#\Newline #\Return #\Nul))) dir))
           "the folder name holds a line break"))))

;;; --- what a check may run ---------------------------------------------------------

(defparameter +python-version-script+
  "^import importlib\\.metadata as m; print\\(m\\.version\\('([A-Za-z0-9](?:[A-Za-z0-9._-]*[A-Za-z0-9])?)'\\)\\)$"
  "The feed's Python-package version check: python3 -c and this script, the
package name (PEP 508's form) the one part that varies.")

(defun python-probe (argv)
  "The words this copy runs for the feed's Python-package version check ARGV:
our own script, the package name handed to it as an argument and never read
as code. NIL when ARGV is not that check."
  (when (and (= (length argv) 3) (equal (first argv) "python3") (equal (second argv) "-c"))
    (cl-ppcre:register-groups-bind (name) (+python-version-script+ (third argv))
      (list "python3" "-c" "import importlib.metadata as m, sys; print(m.version(sys.argv[1]))" name))))

(defvar *pinned-cache* nil
  "(WRITE-DATE . SET): PINNED-COMMANDS read once per write of the bundled file.")

(defun command-set (entries)
  "Every command ENTRIES' checks run -- a check's command or backport, its
words as written -- as an EQUAL hash set."
  (let ((set (make-hash-table :test #'equal)))
    (dolist (a entries set)
      (dolist (c (field a "checks"))
        (when (hash-table-p c)
          (dolist (key '("command" "backport"))
            (let ((words (field c key)))
              (when (and (consp words) (every #'stringp words))
                (setf (gethash words set) t)))))))))

(defun pinned-commands ()
  "Every command the advisories.json beside this file carries, as COMMAND-SET."
  (let* ((path (packaged-path))
         (date (ignore-errors (file-write-date path))))
    (unless (and *pinned-cache* (eql (car *pinned-cache*) date))
      (setf *pinned-cache*
            (cons date (command-set (handler-case (read-feed (uiop:read-file-string path))
                                      (error () nil))))))
    (cdr *pinned-cache*)))

(defparameter *pinned* 'pinned-commands
  "The commands this installed copy vouches for: (funcall *PINNED*) => an EQUAL
hash set of word lists. Tests stub it; nothing else should.")

(defun runnable (argv dir host)
  "ARGV, a check's command from the feed, as the words this copy runs =>
(VALUES WORDS NIL), or (VALUES NIL WHY) when it may not run."
  (cond ((not (and (consp argv) (every #'stringp argv)))
         (values nil "its command is not a list of words"))
        ((python-probe argv))
        ((not (gethash argv (funcall *pinned*)))
         (values nil "not run: this command reached the live feed after this copy was installed (a nodecode update brings it)"))
        ((placeholder-problem argv dir host)
         (values nil (format nil "not run: ~a" (placeholder-problem argv dir host))))
        (t (values (mapcar (lambda (a) (fill-in a dir host)) argv) nil))))

(defun needs (check)
  "What CHECK cannot run without: :dir, :host or NIL."
  (let ((n (field check "needs"))
        (kind (field check "kind")))
    (cond ((equal n "dir") :dir)
          ((equal n "host") :host)
          ((member kind '("npm" "composer" "wp-plugin") :test #'equal) :dir))))

;;; --- the kinds ----------------------------------------------------------------

(defun read-json-file (path)
  (when (probe-file path)
    (handler-case (shasht:read-json (uiop:read-file-string path)) (error () nil))))

(defun npm-version (dir package)
  "PACKAGE's installed version under DIR, from the lockfile or node_modules."
  (let* ((lock (read-json-file (merge-pathnames "package-lock.json" (uiop:ensure-directory-pathname dir))))
         (packages (and (hash-table-p lock) (gethash "packages" lock)))
         (entry (and (hash-table-p packages)
                     (gethash (concatenate 'string "node_modules/" package) packages))))
    (or (and (hash-table-p entry) (gethash "version" entry))
        (let ((pj (read-json-file (merge-pathnames (concatenate 'string "node_modules/" package "/package.json")
                                                   (uiop:ensure-directory-pathname dir)))))
          (and (hash-table-p pj) (gethash "version" pj))))))

(defun composer-version (dir packages)
  "The first of PACKAGES found in DIR's composer.lock, as its version string."
  (let ((lock (read-json-file (merge-pathnames "composer.lock" (uiop:ensure-directory-pathname dir)))))
    (when (hash-table-p lock)
      (loop for p across (or (gethash "packages" lock) #())
            when (and (hash-table-p p) (member (gethash "name" p) packages :test #'equal))
              return (string-left-trim "vV" (gethash "version" p))))))

(defun wp-plugin-version (dir slugs)
  "The Version: header of the first plugin folder in SLUGS under DIR."
  (dolist (slug slugs)
    (let ((folder (merge-pathnames (format nil "wp-content/plugins/~a/" slug)
                                   (uiop:ensure-directory-pathname dir))))
      (dolist (file (directory (merge-pathnames "*.php" folder)))
        (let ((v (cl-ppcre:register-groups-bind (v)
                     ("(?im)^[ \\t/*#@]*Version:\\s*([0-9][0-9.]*)"
                      (handler-case (uiop:read-file-string file) (error () "")))
                   v)))
          (when v (return-from wp-plugin-version v)))))))

(defun run-check (check &key dir host)
  "One CHECK of the feed against this machine => (:status :found :why)."
  (let ((kind (field check "kind"))
        (need (needs check)))
    (flet ((versioned (text)
             (let ((v (parse-version text)))
               (if (null v)
                   (list :status :absent :why "not installed here")
                   (multiple-value-bind (status why) (judge v check)
                     (list :status status :found (version-string v) :why why))))))
      (cond
        ((and (eq need :dir) (null dir))
         (list :status :skipped :why "needs the site's folder (:dir)"))
        ((and (eq need :host) (null host))
         (list :status :skipped :why "needs the site's host name (:host)"))
        ((equal kind "version")
         (multiple-value-bind (words refused) (runnable (field check "command") dir host)
           (if refused
               (list :status :skipped :why refused)
               (multiple-value-bind (out code) (funcall *run* words)
                 (if (or (/= code 0) (null (parse-version out)))
                     (list :status :absent :why "not installed here")
                     (let ((result (versioned out)))
                       ;; A distro build backports the fix under the old number.
                       (if (and (eq (getf result :status) :vulnerable) (field check "backport"))
                           (multiple-value-bind (proof-words proof-refused)
                               (runnable (field check "backport") dir host)
                             (let ((proof (if proof-refused ""
                                              (string-trim '(#\Space #\Newline #\Tab)
                                                           (funcall *run* proof-words)))))
                               (cond ((plusp (length proof))
                                      (list :status :ok :found (getf result :found)
                                            :why (format nil "distro build carries the fix (~a)"
                                                         (subseq proof 0 (min 80 (length proof))))))
                                     (proof-refused
                                      (list :status :vulnerable :found (getf result :found)
                                            :why (format nil "~a; its distro-backport check was ~a"
                                                         (getf result :why) proof-refused)))
                                     (t result))))
                           result)))))))
        ((equal kind "npm")
         (let ((v (npm-version dir (field check "package"))))
           (if v (versioned v) (list :status :absent :why "not in this project"))))
        ((equal kind "composer")
         (let ((v (composer-version dir (field check "packages"))))
           (if v (versioned v) (list :status :absent :why "not in this project"))))
        ((equal kind "wp-plugin")
         (let ((v (wp-plugin-version dir (field check "slugs"))))
           (if v (versioned v) (list :status :absent :why "plugin not installed"))))
        ((equal kind "command")
         (multiple-value-bind (words refused) (runnable (field check "command") dir host)
           (if refused
               (list :status :skipped :why refused)
               (multiple-value-bind (out code) (funcall *run* words)
                 (let ((text (string-trim '(#\Space #\Newline #\Tab #\Return) out))
                       (match (field check "match")))
                   (cond ((= code 127) (list :status :absent :why "its tool is not installed here"))
                         ((zerop (length text)) (list :status :ok :why "nothing found"))
                         ((and match (not (cl-ppcre:scan match text)))
                          (list :status :ok :found (first-line text) :why "no match"))
                         (t (list :status :vulnerable :found (first-line text) :why "found"))))))))
        ((equal kind "files")
         (let ((hits (loop for p in (field check "paths")
                           append (directory (fill-in p dir host)))))
           (if hits
               (list :status :vulnerable :found (namestring (first hits)) :why "found")
               (list :status :ok :why "nothing found"))))
        (t (list :status :error :why (format nil "unknown check kind ~a" kind)))))))

(defun first-line (text)
  (let ((nl (position #\Newline text)))
    (subseq text 0 (min 160 (or nl (length text))))))
