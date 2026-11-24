;;;; cell.lisp --- a file manager over the folders the operator exposes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :cloud-files) answers the primer below, and every request's help
;;;; section carries one line naming it. Nothing hooks the provider path: this
;;;; cell reads and writes the filesystem, and no seam but the disk is used.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "cloud-files": {"roots": ["/srv/notes", "/srv/projects"],
;;;;                   "max_file_mb": 100}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from electricsheephq's Hermes Agent plugin (see NOTICE): the
;;;; folder model, the Path guard (a relative path only, segment by segment,
;;;; with symbolic links resolved and the root's own home kept out), the
;;;; listing with its sort and cap, the name search with its skipped heavy
;;;; folders, the sha256 whole-file read and the conflict-checked atomic save,
;;;; and folder creation.

(in-package #:nodecode-cloud-files)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Cloud Files is available through the nodecode-cloud-files cell: five tools over the folders
the operator exposed, which a person can also call from eval in the cloud-files package.
  files_list  {root, path, sort, offset, limit}
      One folder of an exposed folder: its entries with name, path, is_dir, size, mtime,
      and whether the file is editable. Directories first, then names. With no root and no
      path it answers the exposed folders themselves and their ids (r0, r1, ...), which the
      other four take as root. A folder too large answers a page at a time (offset, limit)
      and says when it has more; sort by mtime to see what changed recently.
  files_search  {root, path, query}
      Files and folders by name, anywhere below a folder. The query is a substring or a glob
      with * and ?. Contents are not searched. .git, node_modules, __pycache__, virtualenvs
      and caches are not descended, and hidden names are skipped unless the query starts
      with a dot. An answer cut short says whether visits, time or the count stopped it.
  files_read  {root, path}
      A file as text, with the sha256 of the bytes read. That sha256 is what files_save
      needs. A binary file, a file over the cell's size cap, or a path that leaves the
      root is refused, and says which.
  files_save  {root, path, text, base_sha256}
      Save a whole Markdown or plain-text file (.md, .markdown, .txt), atomically. Read it
      with files_read first and pass the sha256 it answered: if the file changed since,
      the save is refused as a conflict and carries the file as it is now, so a person's
      or another agent's edit is never overwritten blind. A new file (no base_sha256) is
      created; an existing one without a base_sha256 is refused.
  files_mkdir  {root, path}
      Create a folder and any missing folder above it.
Every path is relative to its root: \"docs/notes.md\", not \"/srv/notes/docs/notes.md\".
Parent traversal (..), absolute paths, a symbolic link that leaves the root, and the folder
Nodecode itself keeps its config and database in are refused before anything is touched.
Only the file manager's own staging names (.cfm-<hex>.part) are ever written and renamed away.")

;;; --- what the original fixes, and the rewrite keeps ------------------------------

(defparameter +editable-exts+ '("md" "markdown" "txt")
  "The extensions the editor writes. The original's EDIT_EXTS.")

(defparameter +edit-max-bytes+ (* 1024 1024)
  "The largest file the editor reads and writes: 1 MiB, the original's EDIT_MAX_BYTES.")

(defparameter +text-max-bytes+ (* 1024 1024)
  "The largest file files_read answers as text, the original's TEXT_PREVIEW_MAX_BYTES.")

(defparameter +scan-limit+ 20000
  "The most entries one listing scans, the original's SCAN_LIMIT.")

(defparameter +search-visits+ 200000
  "The most entries one search looks at before it says it stopped on visits.")

(defparameter +search-skip+ '(".git" "node_modules" "__pycache__" ".venv" "venv" ".tox"
                              ".mypy_cache" ".cache")
  "Folders a search does not descend into, the original's SEARCH_SKIP.")

(defparameter +list-cap+ 1000 "The most entries one listing answers, whatever limit asks.")

;;; --- answers, and the one condition -----------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (cloud-files-error (condition)
       (nle:failure (format nil "ERROR: ~a" (cloud-files-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Cloud Files: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun answer-json (object)
  (nlk:encode-json-object object))

;;; --- reading the section --------------------------------------------------------

(defun folder-name (path)
  "PATH's own name, without its trailing separator."
  (let* ((directory (pathname-directory path))
         (name (and directory (car (last directory)))))
    (if (stringp name) name "root")))

(defun read-settings (values table)
  "The declared VALUES, with each root turned into a checked ROOT once, here."
  (declare (ignore table))
  (let ((configured (getf values :roots))
        (roots '())
        (index -1))
    (loop for value in (cond ((listp configured) configured)
                                      ((vectorp configured) (coerce configured 'list))
                                      (t '()))
          do (let ((text (nonblank value)))
               (when text
                 (let* ((directory (uiop:ensure-directory-pathname (nlk:expand-home text)))
                        (real (ignore-errors (truename directory))))
                   (unless (uiop:absolute-pathname-p directory)
                     (nlk:config-error "root ~s is not an absolute folder: give the full path, e.g. \"/srv/notes\"" text))
                   (unless (and real (uiop:directory-pathname-p (probe-file real)))
                     (nlk:config-error "root ~s is not a folder this machine has" text))
                   (incf index)
                   (push (list :id (format nil "r~d" index)
                               :label (folder-name real)
                               :path real
                               :real (uiop:native-namestring (uiop:ensure-directory-pathname real)))
                         roots)))))
    (unless roots
      (nlk:config-error "no roots: set roots to the absolute folders Cloud Files may show, e.g. [\"/srv/notes\"]"))
    (let ((plist (copy-list values)))
      (setf (getf plist :roots) (nreverse roots)
            (getf plist :max-file-mb) (max 1 (or (getf values :max-file-mb) 100)))
      plist)))

(defun roots () (setting :roots))

(defun root-max-bytes ()
  (* 1024 1024 (setting :max-file-mb)))

(defun pick-root (id)
  "The root ID names, or the first one when ID is blank; an unknown id is a refusal
naming the ones there are."
  (let* ((id (nonblank id))
         (all (roots)))
    (if (null id)
        (first all)
        (or (find id all :key (lambda (root) (getf root :id)) :test #'string=)
            (fail "~s is not an exposed folder: they are ~{~a~^, ~}"
                  id (mapcar (lambda (root) (getf root :id)) all))))))

(defun all-roots-answer ()
  (answer-json
   (nlk:json-object
    "ok" t
    "roots" (map 'vector (lambda (root)
                           (nlk:json-object "id" (getf root :id)
                                            "label" (getf root :label)
                                            "path" (namestring (getf root :path))))
                 (roots))
    "note" "pass a root id to files_list, files_search, files_read, files_save or files_mkdir")))

;;; --- the path guard ---------------------------------------------------------------

(defun split-rel (rel)
  "REL's segments, or a refusal: a path is relative, with no NUL and no .."
  (let ((rel (or rel "")))
    (when (find (code-char 0) rel)
      (fail "the path carries a NUL character"))
    (when (or (uiop:string-prefix-p "/" rel) (uiop:string-prefix-p "~" rel)
              (uiop:absolute-pathname-p (pathname rel)))
      (fail "~s is absolute: use a path relative to the root, e.g. \"docs/notes.md\"" rel))
    (let ((parts '()))
      (dolist (part (uiop:split-string rel :separator "/") (nreverse parts))
        (cond ((member part '("" ".") :test #'equal))
              ((string= part "..")
               (fail "parent traversal is not allowed: ~s" rel))
              (t (push part parts)))))))

(defun inside-p (namestring root-namestring)
  "Whether NAMESTRING is ROOT-NAMESTRING or below it, both native and
directory-form."
  (let ((path (uiop:native-namestring (uiop:ensure-directory-pathname namestring))))
    (uiop:string-prefix-p root-namestring path)))

(defun symlink-p (path)
  "Whether PATH is a link: it exists, and it resolves somewhere else than its
own name says. Read from the pathname system rather than lstat, so no foreign
call has to be held across a binding."
  (let ((resolved (ignore-errors (probe-file path))))
    (and resolved
         (not (equal (uiop:native-namestring (uiop:ensure-directory-pathname resolved))
                     (uiop:native-namestring (uiop:ensure-directory-pathname path)))))))

(defun protected-p (root resolved)
  "Whether RESOLVED is inside the folder Nodecode keeps its config and database in,
unless the root itself is inside it: the original's protected homes."
  (let ((home (uiop:ensure-directory-pathname (nlk:home))))
    (and (inside-p (uiop:native-namestring (uiop:ensure-directory-pathname resolved))
                   (uiop:native-namestring home))
         (not (inside-p (getf root :real) (uiop:native-namestring home))))))

(defun resolve-under (root rel &key for-write)
  "The path REL names under ROOT, checked segment by segment: every segment that
exists must resolve inside the root, the final one must not be a symbolic link
when writing, and the whole path must sit outside Nodecode's own home. A refusal
names REL, so the model can see what it asked for."
  (let* ((rel (or rel ""))
         (parts (split-rel rel))
         (path (uiop:ensure-directory-pathname (getf root :path)))
         (real (getf root :real)))
    (when (and for-write (null parts))
      (fail "a path is required: ~s names the root itself" rel))
    (loop for part in parts
          for tail on parts
          for last = (null (rest tail))
          do (let* ((candidate (merge-pathnames part path))
                    (existing (probe-file candidate)))
               (when existing
                 (let ((resolved (ignore-errors (truename candidate))))
                   (unless (and resolved
                                (inside-p (uiop:native-namestring resolved) real))
                     (fail "~s leaves ~a: it resolves to ~a"
                           rel (getf root :label)
                           (or (and resolved (namestring resolved)) "nothing the root holds")))
                   (when (protected-p root resolved)
                     (fail "~s is inside Nodecode's own home folder, which Cloud Files keeps out of reach"
                           rel))
                   (when (and last for-write (symlink-p candidate))
                     (fail "~s is a symbolic link: writing through one is not allowed" rel))))
               (setf path (if last candidate (uiop:ensure-directory-pathname candidate)))))
    ;; The join itself, for a path that exists only through a link's tail.
    (let ((final (probe-file path)))
      (when final
        (let ((resolved (ignore-errors (truename final))))
          (unless (and resolved (inside-p (uiop:native-namestring resolved) real))
            (fail "~s leaves ~a" rel (getf root :label))))))
    path))

;;; --- reading a folder ---------------------------------------------------------------

(defun directory-names (folder)
  "Every entry name in FOLDER, dot files included and . and .. left out."
  #+unix
  (let ((handle (sb-posix:opendir (uiop:native-namestring folder)))
        (names '()))
    (unwind-protect
         (loop for entry = (sb-posix:readdir handle)
               while (and entry (not (sb-alien:null-alien entry)))
               for name = (sb-posix:dirent-name entry)
               unless (member name '("." "..") :test #'equal)
                 do (push name names))
      (sb-posix:closedir handle))
    (nreverse names))
  #-unix
  (append (mapcar #'file-namestring (uiop:directory-files folder))
          (mapcar #'folder-name (uiop:subdirectories folder))))

(defun temp-name-p (name)
  "Whether NAME is one of the file manager's own staging names."
  (and (ppcre:scan "^\\.cfm-[0-9a-f]{32}\\.part$" (string-downcase name)) t))

(defun editable-name-p (name)
  (member (string-downcase (or (pathname-type (pathname name)) "")) +editable-exts+
          :test #'string=))

(defun path-size (path)
  (handler-case
      (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
        (and in (file-length in)))
    (error () nil)))

(defun iso-time (universal)
  "UNIVERSAL as a UTC ISO-8601 stamp, or NIL."
  (and universal
       (multiple-value-bind (second minute hour day month year)
           (decode-universal-time universal 0)
         (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
                 year month day hour minute second))))

(defun join-rel (prefix name)
  (if (zerop (length prefix)) name (concatenate 'string prefix "/" name)))

(defun entry-object (root folder prefix name)
  "NAME's entry in FOLDER, as the answer carries it, or NIL when it cannot be read."
  (let ((target (merge-pathnames name (uiop:ensure-directory-pathname folder)))
        (rel (join-rel prefix name)))
    (handler-case
        (let* ((link (symlink-p target))
               (resolved (ignore-errors (truename target)))
               (outside (and link (or (null resolved)
                                      (not (inside-p (uiop:native-namestring resolved)
                                                     (getf root :real)))))))
          (if outside
              (nlk:json-object "name" name "path" rel "is_dir" nil "size" nil
                                "mtime" nil "editable" nil "link_outside" t)
              (let ((directory (and resolved (uiop:directory-pathname-p resolved))))
                (nlk:json-object "name" name
                                 "path" rel
                                 "is_dir" (and directory t)
                                 "size" (and (not directory) (path-size target))
                                 "mtime" (iso-time (ignore-errors (file-write-date target)))
                                 "editable" (and (not directory) (editable-name-p name) t)
                                 "link_outside" nil))))
      (error () nil))))

(defun entry-dir-p (entry) (and (nlk:json-value entry :boolean "is_dir") t))
(defun entry-name (entry) (or (nlk:json-value entry :string "name") ""))
(defun entry-mtime (entry) (nlk:json-value entry :string "mtime"))

(defun sort-entries (entries sort)
  (if (string= sort "mtime")
      (sort entries (lambda (a b)
                      (let ((ta (entry-mtime a)) (tb (entry-mtime b)))
                        (cond ((and ta tb (not (string= ta tb))) (string> ta tb))
                              (ta (not tb))
                              (tb nil)
                              (t (string-lessp (string-downcase (entry-name a))
                                               (string-downcase (entry-name b))))))))
      (sort entries (lambda (a b)
                      (let ((da (entry-dir-p a)) (db (entry-dir-p b)))
                        (cond ((and da (not db)) t)
                              ((and db (not da)) nil)
                              ((string= (string-downcase (entry-name a))
                                        (string-downcase (entry-name b)))
                               (string-lessp (entry-name a) (entry-name b)))
                              (t (string-lessp (string-downcase (entry-name a))
                                                (string-downcase (entry-name b))))))))))

(defun scan-folder (root folder rel)
  "FOLDER's entries, sorted, and whether the scan cap cut them."
  (let ((entries '())
        (truncated nil)
        (seen 0))
    (dolist (name (directory-names folder))
      (when (>= seen +scan-limit+)
        (setf truncated t)
        (return))
      (incf seen)
      (unless (temp-name-p name)
        (let ((entry (entry-object root folder rel name)))
          (when entry (push entry entries)))))
    (values (nreverse entries) truncated)))

;;; --- files_list -------------------------------------------------------------------


(defun list-files (call)
  "The files_list tool: one folder, or the exposed folders themselves."
  (answering
    (let* ((root-id (nonblank (file-listing-root call)))
           (rel (or (file-listing-path call) ""))
           (sort (or (nonblank (file-listing-sort-by call)) "name"))
           (offset (max 0 (or (file-listing-offset call) 0)))
           (limit (min +list-cap+ (max 1 (or (file-listing-limit call) 200)))))
      (unless (member sort '("name" "mtime") :test #'string=)
        (fail "sort ~s is neither name nor mtime" sort))
      (when (and (null root-id) (zerop (length (nlk:trimmed rel))))
        (return-from list-files (all-roots-answer)))
      (let* ((root (pick-root root-id))
             (target (resolve-under root rel :for-write nil)))
        (unless (uiop:directory-pathname-p (or (probe-file target) (pathname (namestring target))))
          (fail "~s is not a folder" rel))
        (unless (probe-file target)
          (fail "~s is not a folder ~a has" rel (getf root :label)))
        (let ((rel-folder (format nil "~{~a~^/~}" (split-rel rel))))
          (multiple-value-bind (entries truncated) (scan-folder root target rel-folder)
            (let* ((sorted (sort-entries entries sort))
                   (total (length sorted))
                   (page (loop for entry in sorted
                               for index from 0
                               when (and (>= index offset) (< index (+ offset limit)))
                                 collect entry)))
              (answer-json
               (nlk:json-object
                "ok" t
                "root" (getf root :id)
                "label" (getf root :label)
                "path" rel-folder
                "entries" (coerce page 'vector)
                "total" total
                "truncated" (or truncated (> total (+ offset limit)))
                "note" (if (> total (+ offset limit))
                           (format nil "~d more entries: ask again with offset ~d"
                                   (- total (+ offset limit)) (+ offset limit))
                           ""))))))))))

;;; --- files_search -----------------------------------------------------------------

(defun glob-pattern (query)
  "QUERY as an anchored regular expression: * is any run, ? one character."
  (with-output-to-string (out)
    (write-string "\\A" out)
    (loop for character across query
          do (case character
               (#\* (write-string ".*" out))
               (#\? (write-char #\. out))
               (t (when (find character ".+()[]{}^$|\\") (write-char #\\ out))
                  (write-char character out))))
    (write-string "\\z" out)))

(defun search-files (call)
  "The files_search tool: names below a folder, with the original's caps."
  (answering
    (let* ((query (nlk:trimmed (or (file-search-query call) "")))
           (limit (min +list-cap+ (max 1 (or (file-search-limit call) 50))))
           (root (pick-root (file-search-root call)))
           (rel (or (file-search-path call) ""))
           (folder (resolve-under root rel :for-write nil)))
      (when (or (zerop (length query)) (> (length query) 200))
        (fail "use a search query between 1 and 200 characters"))
      (multiple-value-bind (results truncated reason visited)
          (walk-for root folder (format nil "~{~a~^/~}" (split-rel rel)) query limit)
        (declare (ignore truncated))
        (answer-json
         (nlk:json-object
          "ok" t
          "root" (getf root :id)
          "path" (format nil "~{~a~^/~}" (split-rel rel))
          "query" query
          "results" (coerce (nreverse results) 'vector)
          "truncated" (and reason t)
          "reason" (or reason "")
          "visited" visited))))))

(defun name-matches-p (query lower name)
  (let ((name-lower (string-downcase name)))
    (if (or (find #\* query) (find #\? query))
        (and (ignore-errors (ppcre:scan (glob-pattern query) name-lower)) t)
        (and (search lower name-lower) t))))

(defun walk-for (root start prefix query limit)
  "RESULTS and why the walk stopped: the original's breadth-first search. Each
folder in the queue carries its own relative prefix, so an entry found three
folders down answers the whole path from the root."
  (let ((queue (list (cons start prefix)))
        (results '())
        (visited 0)
        (lower (string-downcase query))
        (dotted (uiop:string-prefix-p "." query))
        (reason nil))
    (loop while (and queue (null reason))
          do (let* ((item (pop queue))
                    (folder (car item))
                    (folder-prefix (cdr item)))
               (dolist (name (directory-names folder))
                 (incf visited)
                 (when (>= visited +search-visits+)
                   (return-from walk-for (values results t "visits" visited)))
                 (let* ((target (merge-pathnames name (uiop:ensure-directory-pathname folder)))
                        (hidden (uiop:string-prefix-p "." name))
                        (excluded (or (member name +search-skip+ :test #'string=)
                                      (and hidden (not dotted)))))
                   (unless (temp-name-p name)
                     (when (name-matches-p query lower name)
                       (let ((entry (entry-object root folder folder-prefix name)))
                         (when (and entry (not (and excluded (entry-dir-p entry))))
                           (push entry results)
                           (when (>= (length results) limit)
                             (return-from walk-for (values results t "results" visited)))))))
                   (unless excluded
                     (when (and (not (symlink-p target))
                                (ignore-errors (uiop:directory-pathname-p (probe-file target))))
                       (push (cons target (join-rel folder-prefix name)) queue)))))))
    (values results nil reason visited)))


;;; --- files_read -------------------------------------------------------------------

(defun text-of (path limit)
  "PATH's bytes as text, or a refusal: not UTF-8, or over LIMIT."
  (let ((size (path-size path)))
    (when (null size)
      (fail "~a cannot be read" (file-namestring path)))
    (when (> size limit)
      (fail "~a is ~:d bytes, over the ~:d-byte limit for reading" (file-namestring path) size limit))
    (let ((text (ignore-errors
                 (uiop:read-file-string path :external-format :utf-8))))
      (cond ((null text) (fail "~a is not UTF-8 text" (file-namestring path)))
            ((find (code-char 0) text) (fail "~a is binary, not text" (file-namestring path)))
            (t text)))))

(defun read-file (call)
  "The files_read tool: a file as text, with the sha256 files_save checks against."
  (answering
    (let* ((root (pick-root (file-read-root call)))
           (rel (or (file-read-path call) ""))
           (target (resolve-under root rel :for-write nil)))
      (when (zerop (length (split-rel rel)))
        (fail "~s names the root itself: give a file" rel))
      (unless (probe-file target)
        (fail "~s is not a file ~a has (looked for ~a)" rel (getf root :label) (namestring target)))
      (when (uiop:directory-pathname-p (probe-file target))
        (fail "~s is a folder: use files_list" rel))
      (let* ((text (text-of target +text-max-bytes+))
             (size (path-size target)))
        (answer-json
         (nlk:json-object
          "ok" t
          "root" (getf root :id)
          "path" (format nil "~{~a~^/~}" (split-rel rel))
          "text" text
          "sha256" (nlk:sha256-text text)
          "size" size
          "mtime" (iso-time (ignore-errors (file-write-date target)))
          "editable" (and (editable-name-p (file-namestring target)) t)
          "note" (if (editable-name-p (file-namestring target))
                     ""
                     "only .md, .markdown and .txt files can be saved back")))))))

;;; --- files_save -------------------------------------------------------------------

(defun octet-length (text)
  #+sbcl (length (sb-ext:string-to-octets text :external-format :utf-8))
  #-sbcl (length text))

(defun save-file (call)
  "The files_save tool: the original's conflict-checked atomic save."
  (answering
    (let* ((root (pick-root (file-save-root call)))
           (parts (split-rel (or (file-save-path call) "")))
           (rel (format nil "~{~a~^/~}" parts))
           (text (or (file-save-text call) ""))
           (base (or (nonblank (file-save-base-sha256 call)) ""))
           (target (resolve-under root (or (file-save-path call) "") :for-write t)))
      (when (null parts)
        (fail "a path is required: ~s names the root itself" rel))
      (unless (editable-name-p (car (last parts)))
        (fail "~s is not a Markdown or plain-text file: only .md, .markdown and .txt are saved here"
              rel))
      (when (> (octet-length text) +edit-max-bytes+)
        (fail "the text is ~:d bytes, over the ~:d-byte limit for saving"
              (octet-length text) +edit-max-bytes+))
      (let ((existing (probe-file target)))
        (when existing
          (unless (uiop:directory-pathname-p existing)
            (let* ((current (text-of target +edit-max-bytes+))
                   (digest (nlk:sha256-text current)))
              (cond ((zerop (length base))
                     (fail "~s already exists: read it with files_read and pass the sha256 it answered as base_sha256" rel))
                    ((not (string-equal base digest))
                     (return-from save-file
                       (answer-json
                        (nlk:json-object
                         "ok" nil
                         "conflict" t
                         "root" (getf root :id)
                         "path" rel
                         "sha256" digest
                         "text" (nlk:clip current 20000)
                         "note" "the file changed since it was read: merge into this text and save again with this sha256"))))))))
        (let ((temp (make-pathname :name (format nil ".cfm-~(~32,'0x~).part"
                                                 (random (expt 2 128)))
                                    :type nil :defaults target)))
          (unwind-protect
               (progn
                 (with-open-file (out temp :direction :output :if-exists nil
                                            :if-does-not-exist :create
                                            :external-format :utf-8)
                   (write-string text out))
                 (rename-file temp target)
                 (answer-json
                  (nlk:json-object
                   "ok" t
                   "root" (getf root :id)
                   "path" rel
                   "sha256" (nlk:sha256-text text)
                   "size" (path-size target)
                   "mtime" (iso-time (ignore-errors (file-write-date target)))
                   "created" (and (null existing) t))))
            (when (probe-file temp)
              (ignore-errors (delete-file temp)))))))))

;;; --- files_mkdir --------------------------------------------------------------------

(defun make-folder (call)
  "The files_mkdir tool: a folder and whatever is missing above it."
  (answering
    (let* ((root (pick-root (folder-create-root call)))
           (parts (split-rel (or (folder-create-path call) "")))
           (path (uiop:ensure-directory-pathname (getf root :path)))
           (created '()))
      (when (null parts)
        (fail "a path is required: ~s names the root itself"
              (or (folder-create-path call) "")))
      (resolve-under root (format nil "~{~a~^/~}" parts) :for-write t)
      (dolist (part parts)
        (setf path (uiop:ensure-directory-pathname
                    (merge-pathnames (concatenate 'string part "/") path)))
        (unless (probe-file path)
          (ensure-directories-exist path)
          (push part created)))
      (answer-json
       (nlk:json-object "ok" t
                        "root" (getf root :id)
                        "path" (format nil "~{~a~^/~}" parts)
                        "created" (coerce (nreverse created) 'vector)
                        "exists" (and (null created) t))))))

;;; --- the /files command ----------------------------------------------------------------

(defun files (args session-id)
  "One /files invocation: the exposed folders, or what one of them holds."
  (declare (ignore session-id))
  (answering
    (let* ((words (remove "" (uiop:split-string (nlk:trimmed (or args "")) :separator " ")
                          :test #'equal))
           (root-id (first words))
           (rel (format nil "~{~a~^/~}" (rest words))))
      (if (null root-id)
          (with-output-to-string (out)
            (format out "~d exposed folder~:p:" (length (roots)))
            (dolist (root (roots))
              (format out "~&  ~a  ~a  ~a" (getf root :id) (getf root :label)
                      (namestring (getf root :path))))
            (format out "~&/files r0 [folder] lists one of them."))
          (let* ((root (pick-root root-id))
                 (target (resolve-under root rel :for-write nil))
                 (rel-folder (format nil "~{~a~^/~}" (split-rel rel))))
            (multiple-value-bind (entries truncated) (scan-folder root target rel-folder)
              (let ((sorted (sort-entries entries "name")))
                (with-output-to-string (out)
                  (format out "~a (~a) ~a~@[ (capped)~]"
                          (getf root :id) (getf root :label)
                          (if (zerop (length rel-folder)) "/" rel-folder)
                          truncated)
                  (dolist (entry sorted)
                    (format out "~&  ~:[~;- ~]~a~@[  ~:d B~]"
                            (entry-dir-p entry) (entry-name entry)
                            (and (not (entry-dir-p entry))
                                 (nlk:json-value entry :integer "size"))))))))))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell cloud-files
  (:section ("cloud-files")
    (:guide "name the absolute folders Cloud Files may show in roots; that is all it needs; next step: ask what is in them")
    ("roots" :list
     :doc "the absolute folders the five tools may see and change, e.g. [\"/srv/notes\", \"/srv/projects\"]; nothing outside them is reachable")
    ("max_file_mb" :integer :default 100 :min 1
     :doc "the largest file files_read answers, in whole MiB"))
  (:settings #'read-settings)
  (:help :cloud-files
         "cloud-files: files_list, files_search, files_read, files_save and files_mkdir work in the folders the operator exposed"
         *manual*)
  (:command "files" 'files
            :description "Cloud Files: the exposed folders, and what one of them holds")
  (:tool "files_list" (nlk:contract 'file-listing) #'list-files)
  (:tool "files_search" (nlk:contract 'file-search) #'search-files)
  (:tool "files_read" (nlk:contract 'file-read) #'read-file)
  (:tool "files_save" (nlk:contract 'file-save) #'save-file)
  (:tool "files_mkdir" (nlk:contract 'folder-create) #'make-folder))
