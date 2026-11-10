;;;; scope.lisp --- what a bundle holds, and what restoring it does.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original kept its scope as an allowlist with two slices: a
;;;; shared one (config minus the host's own keys, notes, memories,
;;;; skills, profiles) and a local one (.env, auth state, the gateway) --
;;;; roam/scope.py. A Nodecode home has fewer moving parts, so the scope
;;;; here is one list the operator owns: INCLUDE, a few paths under the
;;;; home, and EXCLUDE, the prefixes left out of them. The defaults are
;;;; the two things a second machine wants -- config.jsonc and the cells
;;;; you keep -- and nothing cached.
;;;;
;;;; Restoring is the original's plan/backup/write: every file the
;;;; bundle would replace is copied under .roam/backups/<stamp>/ first,
;;;; with a journal, and a path that would leave the home is refused
;;;; before a single file moves (roam/restore.py).

(in-package #:nodecode-roam)

(defparameter *default-include* '("config.jsonc" "cells/")
  "What a bundle holds unless the section says otherwise.")

(defparameter *default-exclude* '(".cache/" ".git/" "store/" ".roam/" "node_modules/")
  "Prefixes left out of the include, relative to the home.")

(defconstant +max-file+ (* 4 1024 1024) "One file larger than this is named and left out.")
(defconstant +max-plan-rows+ 200 "How many rows one answer spells out.")
(defconstant +max-depth+ 16 "How deep the walk follows folders.")

;;; --- the home, and a path inside it ----------------------------------------

(defun home-name ()
  "The Nodecode home as a namestring with a trailing slash."
  (let ((name (uiop:native-namestring (uiop:ensure-directory-pathname (nlk:home)))))
    (substitute #\/ #\\ name)))

(defun in-home (relative)
  "RELATIVE under the home, as a pathname."
  (nlk:home relative))

(defun relative-name (path)
  "PATH's name relative to the home, with forward slashes."
  (let* ((name (substitute #\/ #\\ (uiop:native-namestring path)))
         (root (home-name)))
    (if (uiop:string-prefix-p root name)
        (subseq name (length root))
        (file-namestring path))))

(defun slashless (text)
  "TEXT with \\ spelled / and no leading ./"
  (let ((text (substitute #\/ #\\ (or text ""))))
    (if (uiop:string-prefix-p "./" text) (subseq text 2) text)))

(defun safe-relative-p (relative)
  "Whether RELATIVE is a plain path inside the home: no drive, no root, no .."
  (and (stringp relative)
       (plusp (length relative))
       (<= (length relative) 512)
       (not (member (char relative 0) '(#\/ #\\ #\~) :test #'char=))
       (not (find #\: relative :test #'char=))
       (not (member ".." (uiop:split-string (slashless relative) :separator "/") :test #'equal))))

;;; --- the scope ---------------------------------------------------------------

(defun scope-include ()
  "The include paths the section names, or the defaults."
  (let ((include (setting :include)))
    (if include include *default-include*)))

(defun scope-exclude ()
  "The exclude prefixes the section names, or the defaults."
  (let ((exclude (setting :exclude)))
    (if exclude exclude *default-exclude*)))

(defun excluded-p (relative excludes)
  "Whether RELATIVE starts with one of EXCLUDES, or is a fasl or a temp file."
  (or (some (lambda (prefix) (and (plusp (length prefix))
                                  (uiop:string-prefix-p (slashless prefix) relative)))
            excludes)
      (let ((name (file-namestring relative)))
        (or (uiop:string-suffix-p name ".fasl")
            (uiop:string-suffix-p name "~")
            (uiop:string-suffix-p name ".nroam")))))

(defun walk (folder prefix depth out excludes)
  "Collect FOLDER's files into OUT as (RELATIVE . PATH), bounded by DEPTH."
  (when (< depth +max-depth+)
    (dolist (file (ignore-errors (uiop:directory-files folder)))
      (let ((relative (concatenate 'string prefix (file-namestring file))))
        (unless (excluded-p relative excludes)
          (push (cons relative file) out))))
    (dolist (sub (ignore-errors (uiop:subdirectories folder)))
      (let* ((name (car (last (pathname-directory (uiop:ensure-directory-pathname sub)))))
             (relative (format nil "~a~a/" prefix name)))
        (unless (excluded-p relative excludes)
          (setf out (walk sub relative (1+ depth) out excludes))))))
  out)

(defun scope-files ()
  "Every file the scope holds: (RELATIVE . PATH), sorted by RELATIVE."
  (let ((files '())
        (excludes (scope-exclude)))
    (dolist (include (scope-include))
      (let* ((relative (slashless include))
             (path (in-home relative)))
        (cond ((uiop:directory-pathname-p path)
               (setf files
                     (walk path (if (uiop:string-suffix-p relative "/") relative (concatenate 'string relative "/"))
                           0 files excludes)))
              ((probe-file path)
               (unless (or (excluded-p relative excludes) (probe-file-directory-p path))
                 (push (cons relative path) files))))))
    (sort (remove-duplicates files :key #'car :test #'equal) #'string< :key #'car)))

(defun probe-file-directory-p (path)
  "Whether PATH is a directory that exists."
  (and (probe-file path) (uiop:directory-exists-p path)))

;;; --- the manifest --------------------------------------------------------------

(defun text-file-p (octets)
  "Whether OCTETS read as UTF-8 text: no NUL, and it decodes."
  (and (not (find 0 octets))
       (ignore-errors (progn (octets-text octets) t))))

(defun file-entry (relative path)
  "RELATIVE and PATH as one manifest entry: content, size and digest."
  (let* ((octets (read-octets path))
         (size (length octets)))
    (list :path relative
          :size size
          :sha256 (hex (sha256 octets))
          :written-at (or (ignore-errors (file-write-date path)) 0)
          :text (and (text-file-p octets) (octets-text octets))
          :octets octets)))

(defun manifest-entries (&key (files (scope-files)))
  "=> (values ENTRIES SKIPPED): every file of FILES that fits, and the
names of those too large to carry."
  (let ((entries '()) (skipped '()))
    (dolist (file files)
      (let ((size (or (with-open-file (in (cdr file) :element-type '(unsigned-byte 8)
                                             :if-does-not-exist nil)
                        (and in (file-length in)))
                      0)))
        (if (> size +max-file+)
            (push (car file) skipped)
            (push (file-entry (car file) (cdr file)) entries))))
    (values (sort entries #'string< :key (lambda (entry) (getf entry :path)))
            (nreverse skipped))))

(defun content-digest (entries)
  "One digest of ENTRIES' paths and content, so two machines can compare
what they hold without exchanging it."
  (hex (sha256 (text-octets
                (with-output-to-string (out)
                  (dolist (entry entries)
                    (format out "~a~c~a~c" (getf entry :path) #\Nul (getf entry :sha256) #\Newline)))))))

(defun entry-json (entry)
  "ENTRY as the JSON object the bundle's manifest carries."
  (apply #'nlk:make-json-object
         (append (list "path" (getf entry :path)
                       "size" (getf entry :size)
                       "sha256" (getf entry :sha256)
                       "writtenAt" (getf entry :written-at))
                 (if (getf entry :text)
                     (list "text" (getf entry :text))
                     (list "base64" (base64 (getf entry :octets)))))))

(defun manifest-json (entries &key saved-at profile digest)
  "The manifest a bundle carries: ENTRIES, when it was taken, from which
profile, and one digest of its contents."
  (nlk:json-object
   "format" +format+
   "savedAt" saved-at
   "profile" profile
   "contentSha256" (or digest (content-digest entries))
   "files" (coerce (loop for entry in entries collect (entry-json entry)) 'vector)))

(defun manifest-files (manifest)
  "The manifest's file entries as plists of :path :sha256 :text :base64."
  (let ((files (nlk:json-value manifest :array "files")))
    (if (vectorp files)
        (loop for file across files
              when (hash-table-p file)
                collect (list :path (slashless (nlk:json-value file :string "path"))
                              :sha256 (nlk:json-value file :string "sha256")
                              :size (nlk:json-value file :integer "size")
                              :text (nlk:json-value file :string "text")
                              :base64 (nlk:json-value file :string "base64")))
        '())))

(defun entry-content (entry)
  "ENTRY's content as octets."
  (or (and (getf entry :text) (text-octets (getf entry :text)))
      (and (getf entry :base64) (base64-decode (getf entry :base64)))
      (fail "roam: the bundle carries no content for ~a" (getf entry :path))))

;;; --- the plan -------------------------------------------------------------------

(defun current-digest (relative)
  "The digest of the file at RELATIVE under the home, or NIL when it is absent."
  (let ((path (in-home relative)))
    (when (and (safe-relative-p relative) (probe-file path) (not (probe-file-directory-p path)))
      (ignore-errors (hex (sha256 (read-octets path)))))))

(defun plan-restore (manifest)
  "What restoring MANIFEST would do, one row a file: (:path :action . :entry),
ACTION one of :add, :update, :same. A path outside the home is a refusal."
  (loop for entry in (manifest-files manifest)
        for path = (getf entry :path)
        do (unless (safe-relative-p path)
             (fail "roam: the bundle names ~s, which is not a path inside the home: refusing it" path))
        collect (list :path path
                      :action (let ((here (current-digest path)))
                                (cond ((null here) :add)
                                      ((equal here (getf entry :sha256)) :same)
                                      (t :update)))
                      :entry entry)))

(defun plan-counts (plan)
  "PLAN counted: (values ADDED UPDATED SAME)."
  (values (count :add plan :key (lambda (row) (getf row :action)))
          (count :update plan :key (lambda (row) (getf row :action)))
          (count :same plan :key (lambda (row) (getf row :action)))))

;;; --- applying it ------------------------------------------------------------------

(defun backup-root ()
  "The folder this home's pre-restore backups live in."
  (nlk:home ".roam/backups/"))

(defun backup-stamp ()
  "One backup's name: the moment, with no character a path cannot carry."
  (substitute #\- #\: (remove #\Space (string-downcase (nlk:iso-time (nlk:unix-now) :millis t)))))

(defun backup-directories ()
  "This home's backups, newest first: their names."
  (sort (loop for folder in (ignore-errors (uiop:subdirectories (backup-root)))
               for name = (car (last (pathname-directory (uiop:ensure-directory-pathname folder))))
               when (stringp name) collect name)
        #'string>))

(defun prune-backups (keep)
  "Leave the newest KEEP backups; => how many went."
  (let ((pruned 0))
    (dolist (name (nthcdr (max 1 keep) (backup-directories)))
      (when (ignore-errors (uiop:delete-directory-tree (merge-pathnames (format nil "~a/" name) (backup-root))
                                                       :validate t :if-does-not-exist :ignore))
        (incf pruned)))
    pruned))

(defun copy-octets (from to)
  "FROM's bytes to TO, through a temp beside it and one rename."
  (nlk:write-file-atomically to (read-octets from))
  to)

(defun apply-plan (plan &key (keep (setting :backup-keep)) (reason "pull"))
  "Write every :add and :update row of PLAN under the home, copying each
replaced file into one backup first, and a journal beside it.
=> a plist: :added, :updated, :kept, :backup, :rows."
  (let* ((stamp (backup-stamp))
         (added 0) (updated 0)
         (journal '()))
    (dolist (row plan)
      (let* ((relative (getf row :path))
             (action (getf row :action))
             (path (in-home relative))
             (entry (getf row :entry)))
        (case action
          (:same nil)
          ((:add :update)
           (when (eq action :update)
             (let ((backup (merge-pathnames relative (merge-pathnames (format nil "~a/" stamp) (backup-root)))))
               (ensure-directories-exist backup)
               (copy-octets path backup)
               (push (list :path relative :backup (namestring (uiop:enough-pathname backup (backup-root))))
                     journal)))
           (let ((octets (entry-content entry)))
             (ensure-directories-exist path)
             (nlk:write-file-atomically path octets)
             (if (eq action :add) (incf added) (incf updated)))))))
    (when journal
      (let ((folder (merge-pathnames (format nil "~a/" stamp) (backup-root))))
        (nlk:write-file-atomically
         (merge-pathnames "journal.json" folder)
         (format nil "~a~%"
                 (nlk:encode-json-object
                  (nlk:json-object "format" +format+
                                   "at" (nlk:iso-time (nlk:unix-now) :millis nil)
                                   "reason" reason
                                   "files" (coerce (loop for row in (reverse journal)
                                                          collect (nlk:json-object "path" (getf row :path)
                                                                                   "backup" (getf row :backup)))
                                                  'vector)))))))
    (when (and journal keep (>= keep 1)) (ignore-errors (prune-backups keep)))
    (list :added added :updated updated
          :kept (count :same plan :key (lambda (row) (getf row :action)))
          :backup (and journal stamp))))

(defun backup-journal (name)
  "Backup NAME's journal as (PATH BACKUP) rows, or NIL when it has none."
  (let ((file (merge-pathnames (format nil "~a/journal.json" name) (backup-root))))
    (when (probe-file file)
      (let ((object (ignore-errors (nlk:decode-json (uiop:read-file-string file)))))
        (when object
          (loop for row across (or (nlk:json-value object :array "files") #())
                when (hash-table-p row)
                  collect (list (nlk:json-value row :string "path")
                                (nlk:json-value row :string "backup"))))))))

(defun restore-backup (name)
  "Put back every file journaled in backup NAME. => how many were written."
  (unless (member name (backup-directories) :test #'equal)
    (fail "roam: no backup named ~a: /roam backups lists them" name))
  (let ((rows (backup-journal name))
        (written 0))
    (unless rows (fail "roam: backup ~a has no journal: nothing to put back" name))
    (dolist (row rows)
      (let ((relative (first row)) (backup (second row)))
        (unless (safe-relative-p relative)
          (fail "roam: backup ~a names ~s, which is not a path inside the home" name relative))
        (let ((source (merge-pathnames (or backup relative) (backup-root))))
          (when (probe-file source)
            (let ((target (in-home relative)))
              (ensure-directories-exist target)
              (copy-octets source target)
              (incf written))))))
    written))
