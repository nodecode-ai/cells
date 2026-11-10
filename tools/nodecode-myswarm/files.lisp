;;;; files.lisp --- where an agents.md is, and the four verbs that move one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Reading is free: LIST and TOPOLOGY read the workspace's agents.md and
;;;; change nothing. EXPORT writes one scrubbed copy, IMPORT takes a stranger's
;;;; file in (keeping the one it replaced), REVERT puts the one it replaced
;;;; back. Every write lands in two places only -- the folder the cell was told
;;;; to work in, and its own data folder for backups and default exports -- and
;;;; nothing else on this machine is touched.
;;;;
;;;; An import is a copy with a backup, never a merge: agents.md is either the
;;;; one that was there, the one a person imported, or (after a revert) the one
;;;; that was there again.

(in-package #:nodecode-myswarm)

(defparameter +agents-names+ '("AGENTS.override.md" "agents.md" "AGENTS.md")
  "The filenames an agents.md goes by, in the order one is preferred.")

(defparameter +reserved-export-names+
  '("agents.md" "agents.override.md" "claude.md" "soul.md" ".cursorrules" ".cursorrules.md")
  "Names an export may never take: a copy that overwrote the file it copies is no copy.")

(defparameter *import-display-chars* 6000
  "How much of an imported file one answer shows before it says how much it left.")

(defparameter *import-max-chars* 400000
  "The largest file an import reads.")

(defparameter +sharing-hint+
  (format nil "Sharing tip: the copy-paste block is a quick preview and a lightweight starter config, ~
while the exported .md keeps every agent instruction verbatim. PII pruning is best effort -- read ~
anything you share before you share it.")
  "What every sharing answer ends with.")

;;; --- where things are -------------------------------------------------------------

(defun base-directory ()
  "The folder this call works in: the section's workspace when it names one,
else the session's own working directory, else the process's."
  (let ((configured (ignore-errors (setting :workspace))))
    (uiop:ensure-directory-pathname
     (if (and configured (stringp configured) (plusp (length (trimmed configured))))
         (trimmed configured)
         (or (ignore-errors
               (let ((session (ignore-errors (symbol-value 'nlk:*scribe-session-id*))))
                 (and session (nlk:find-session-cwd session))))
             (uiop:getcwd))))))

(defun data-directory ()
  "The cell's own folder: backups, and exports that name no place.
Signals when the section names none, because a backup nowhere is no backup."
  (let ((configured (ignore-errors (setting :data-dir))))
    (unless (and configured (stringp configured) (plusp (length (trimmed configured))))
      (fail "data_dir is not set: name a folder in the myswarm section of the config -- it is where ~
an import keeps the agents.md it replaces -- then (restart-cells \"nodecode-myswarm\")"))
    (ensure-directories-exist (uiop:ensure-directory-pathname (trimmed configured)))))

(defun backup-directory ()
  (ensure-directories-exist (merge-pathnames "backup/" (data-directory))))

(defun inside-p (path directory)
  "Is PATH under DIRECTORY?"
  (let ((p (pathname-directory (pathname (uiop:native-namestring path))))
        (d (pathname-directory (pathname (uiop:native-namestring directory)))))
    (and (eq (first p) :absolute) (>= (length p) (length d))
         (equal d (subseq p 0 (length d))))))

(defun local-path (path directory)
  "PATH read against DIRECTORY when it is relative."
  (merge-pathnames (uiop:parse-native-namestring (trimmed path)) directory))

(defun agents-file (&optional (directory (base-directory)))
  "The agents.md DIRECTORY holds, preferring AGENTS.override.md, or NIL."
  (or (loop for name in +agents-names+
            for path = (merge-pathnames name directory)
            when (probe-file path) return (probe-file path))
      (loop for path in (ignore-errors (uiop:directory-files directory))
            for name = (string-downcase (or (pathname-name path) ""))
            when (member name (mapcar #'string-downcase +agents-names+) :test #'string=)
              return path)))

(defun agents-file! (&optional (directory (base-directory)))
  "AGENTS-FILE, or a refusal naming the folder that holds none."
  (or (agents-file directory)
      (fail "no agents.md in ~a: put one there, or name the folder it is in"
            (uiop:native-namestring directory))))

(defun read-text (path)
  "PATH's text, or NIL when there is no file there."
  (when (probe-file path)
    (uiop:read-file-string path :external-format :utf-8)))

(defun write-text (text path)
  "TEXT written to PATH, its folder made first when it is missing."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
    (write-string (or text "") out))
  path)

;;; --- reading ---------------------------------------------------------------------

(defun swarm-of (path)
  "PATH as (VALUES TITLE AGENTS METHOD), or a refusal."
  (multiple-value-bind (title agents method) (parse-agents (read-text path))
    (when (and (null agents) (eq method :empty))
      (fail "~a declares no agents" (uiop:native-namestring path)))
    (values title agents method)))

(defun list-agents (&optional path)
  "The agents agents.md declares, as the table a person reads."
  (let* ((directory (base-directory))
         (file (if path (local-path path directory) (agents-file! directory))))
    (multiple-value-bind (title agents) (swarm-of file)
      (declare (ignore title))
      (list-text title agents :source (file-namestring file)))))

(defparameter +frame-translation+
  '((#\─ . #\-) (#\│ . #\|) (#\┌ . #\+) (#\┐ . #\+) (#\└ . #\+) (#\┘ . #\+)
    (#\├ . #\+) (#\┤ . #\+) (#\┬ . #\+) (#\┴ . #\+) (#\┼ . #\+) (#\▼ . #\v))
  "The frames a diagram may be drawn in when a terminal is no place for box characters.")

(defun ascii-frame (text)
  "TEXT with every box character read as the 7-bit shape it stands for."
  (map 'string (lambda (character) (or (cdr (assoc character +frame-translation+)) character)) text))

(defun topology (&optional path)
  "The swarm as the box-and-arrow diagram, then the copy-paste block."
  (let* ((directory (base-directory))
         (file (if path (local-path path directory) (agents-file! directory))))
    (multiple-value-bind (title agents) (swarm-of file)
      (let ((diagram (topology-text title agents)))
        (when (string-equal (or (ignore-errors (setting :default-style)) "unicode") "ascii")
          (setf diagram (ascii-frame diagram)))
        (format nil "~a~%~%~a~%~%Sharing tip: the block above is a preview; /myswarm export ~
writes the whole file.~%"
                diagram (copy-paste-text title agents))))))

;;; --- exporting --------------------------------------------------------------------

(defun check-export-target (path directory)
  "PATH as the file an export may write inside DIRECTORY, or a refusal."
  (let* ((path (if (pathname-type path) path (make-pathname :type "md" :defaults path)))
         (name (string-downcase (or (file-namestring path) ""))))
    (when (member name +reserved-export-names+ :test #'string=)
      (fail "~a is a configuration filename: an export may not take it" name))
    (unless (string= (or (pathname-type path) "") "md")
      (setf path (make-pathname :type "md" :defaults path)))
    path))

(defun export-copy (&optional output)
  "The workspace's agents.md, scrubbed, written as one .md a person can share."
  (let* ((directory (base-directory))
         (file (agents-file! directory))
         (data (data-directory))
         (target (if output
                     (local-path output directory)
                     (merge-pathnames (or (ignore-errors (setting :default-export-filename))
                                          "myswarm-export.md")
                                      directory)))
         (target (check-export-target target directory)))
    (unless (or (inside-p target directory) (inside-p target data))
      (fail "an export goes inside ~a or ~a, not ~a"
            (uiop:native-namestring directory) (uiop:native-namestring data)
            (uiop:native-namestring target)))
    (when (equal (uiop:native-namestring (probe-file target))
                 (uiop:native-namestring file))
      (fail "~a is the file being exported: write the copy somewhere else" (file-namestring target)))
    (multiple-value-bind (title agents method) (parse-agents (read-text file))
      (declare (ignore method))
      (multiple-value-bind (clean counts) (scrub-pii (read-text file))
        (write-text (format nil "~a~%~a" (export-header (length agents)) clean) target)
        (format nil "myswarm: wrote ~a (~d agent~:p)~%PII pruning: best effort -- ~a~%~a"
                (uiop:native-namestring target) (length agents)
                (counts-text counts) +sharing-hint+)))))

;;; --- importing and reverting -------------------------------------------------------

(defun backup-state ()
  "What the backup folder holds: :ORIGINAL when it keeps an agents.md, :ABSENT
when the import found none, NIL when no import is live."
  (let* ((directory (backup-directory))
         (active (merge-pathnames "active" directory)))
    (when (probe-file active)
      (cond ((probe-file (merge-pathnames "agents.md" directory)) :original)
            ((probe-file (merge-pathnames "absent" directory)) :absent)
            (t :unknown)))))

(defun clear-backup ()
  (let ((directory (backup-directory)))
    (dolist (name '("agents.md" "absent" "active"))
      (let ((path (merge-pathnames name directory)))
        (when (probe-file path) (ignore-errors (delete-file path)))))))

(defun import-file (source &key preview allow-suspicious)
  "A stranger's agents.md, shown, read for what it could do, and -- when PREVIEW
is NIL and nothing high-severity stopped it -- made the workspace's own, the
one it replaced kept in the data folder."
  (unless (and source (plusp (length (trimmed source))))
    (fail "name the file to import: /myswarm import FILE.md"))
  (let* ((directory (base-directory))
         (path (local-path source directory))
         (text (or (read-text path) (fail "no file at ~a" (uiop:native-namestring path)))))
    (when (> (length text) *import-max-chars*)
      (fail "~a is longer than ~d characters: an agents.md this cell imports is a configuration, ~
not a corpus" (file-namestring path) *import-max-chars*))
    (multiple-value-bind (title agents method) (parse-agents text)
      (let* ((findings (scan-text text))
             (blocked (and (scan-high-p findings) (not allow-suspicious)))
             (markdown (if (eq method :ascii-block) (swarm-markdown title agents) text))
             (shown (if (> (length text) *import-display-chars*)
                        (format nil "~a~%... (~d more characters in the file)"
                                (subseq text 0 *import-display-chars*)
                                (- (length text) *import-display-chars*))
                        text)))
        (cond
          (preview
           (format nil "myswarm: preview of ~a -- nothing written~%~%~a~%~%What it declares: ~
~d agent~:p, read by ~(~a~).~%Security: ~a~%~%Nothing was written.~%"
                   (file-namestring path) shown (length agents) method
                   (findings-text findings)))
          (blocked
           (format nil "myswarm: ~a was not imported -- the warnings below are high severity, and an ~
imported agents.md is instructions your model will follow. Read it, then import it again with ~
/myswarm import --allow ~a if you trust it.~%~%~a~%Security: ~a~%~%Its contents: ~%~a~%"
                   (file-namestring path) (file-namestring path) shown
                   (findings-text findings) shown))
          (t (let ((existing (agents-file directory)))
               (clear-backup)
               (let ((backup (backup-directory)))
                 (if existing
                     (progn (write-text (read-text existing) (merge-pathnames "agents.md" backup))
                            (write-text (uiop:native-namestring existing) (merge-pathnames "where" backup)))
                     (write-text "" (merge-pathnames "absent" backup)))
                 (write-text "" (merge-pathnames "active" backup)))
               (let ((target (or existing (merge-pathnames "agents.md" directory))))
                 (write-text markdown target)
                 (format nil "myswarm: imported ~a as ~a -- ~d agent~:p. Your own agents.md ~
is kept: /myswarm revert puts it back.~%~%Security (best effort): ~a~%~%Its contents: ~%~a~%"
                         (file-namestring path) (file-namestring target) (length agents)
                         (findings-text findings) shown)))))))))

(defun revert ()
  "The agents.md an import replaced, put back; or the one an import created, taken away."
  (let* ((directory (base-directory))
         (state (backup-state))
         (target (agents-file directory)))
    (ecase (or state (fail "no import to revert: nothing is live, so agents.md is the file that was always there"))
      (:original
       (let ((backup (merge-pathnames "agents.md" (backup-directory)))
             (target (or target (merge-pathnames "agents.md" directory))))
         (write-text (read-text backup) target)
         (clear-backup)
         (format nil "myswarm: reverted ~a to the agents.md you had before the import"
                 (file-namestring target))))
      (:absent
       (when target (ignore-errors (delete-file target)))
       (clear-backup)
       (format nil "myswarm: reverted -- the agents.md the import created is gone, and the folder ~
holds none again"))
      (:unknown
       (clear-backup)
       "myswarm: the backup folder held no agents.md and no marker; nothing was restored"))))

;;; --- where things stand -----------------------------------------------------------

(defun status ()
  "What the cell would read, write and keep, in one answer."
  (let* ((directory (base-directory))
         (file (agents-file directory))
         (data (ignore-errors (data-directory)))
         (state (ignore-errors (backup-state))))
    (with-output-to-string (out)
      (format out "myswarm: workspace ~a~%" (uiop:native-namestring directory))
      (if (null file)
          (format out "  agents.md: none~%")
          (multiple-value-bind (title agents method) (parse-agents (read-text file))
            (format out "  agents.md: ~a~%  agents: ~d, read by ~(~a~), ~a~%"
                    (file-namestring file) (length agents) method (abbreviate title 60))))
      (format out "  data folder: ~a~%"
              (if data (uiop:native-namestring data)
                  "data_dir is not set -- an import has nowhere to keep a backup"))
      (format out "  import: ~a~%"
              (case state
                (:original "live, the agents.md before it is kept")
                (:absent "live, the import created agents.md")
                (:unknown "live, but the backup is unreadable")
                (t "none")))
      (format out "  style: ~(~a~), export name: ~a~%"
              (or (ignore-errors (setting :default-style)) "unicode")
              (or (ignore-errors (setting :default-export-filename)) "myswarm-export.md")))))
