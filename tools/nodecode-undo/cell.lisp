;;;; cell.lisp --- snapshots of the files a write, an edit or a shell removes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One cell, three hooks and one command. A hook around WRITE-FILE, EDIT and
;;;; SH reads the paths the call is about to touch, copies what is there into a
;;;; store under the home, lets the call run untouched, and reads the paths
;;;; again: an entry that changed nothing is dropped, so the timeline carries
;;;; only calls that did something. /undo lists the entries, shows one, and
;;;; puts the paths back -- but only where they still look the way the call
;;;; left them, unless --force; and a restore snapshots what it is about to
;;;; overwrite first, so a restore can itself be undone.
;;;;
;;;; Nothing here parses a whole command language: WRITE-FILE and EDIT name
;;;; their path, and SH is read only when it is one simple rm or mv. Anything
;;;; else (a redirect, a pipeline, a program that writes files) is not covered,
;;;; which is what /undo show says of an entry whose paths were skipped.
;;;;
;;;; The store is the cell's own: <home>/undo-effects/entries/<id>.sexp, one
;;;; entry a file, and <home>/undo-effects/objects/<xx>/<digest>, the bytes,
;;;; content-addressed so two snapshots of one file cost one copy.

(defpackage #:nodecode-undo
  (:use #:cl))

(in-package #:nodecode-undo)

(nlk:define-peripheral undo :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Every (write-file ...), (edit ...) and simple (sh \"rm ...\") the organism
runs is snapshotted by the nodecode-undo cell: what the file was, and what the
call left. /undo lists those calls, newest first, each with an id and the paths
it touched; nothing in the list means every write so far was a no-op.
  /undo show ID|last          what one entry covers, and what it skipped.
  /undo restore ID|last        put the paths back as they were; a path changed
      since the call, by anything else, is a conflict and is left alone --
      --force puts it back anyway, --dry-run only says what it would do.
  /undo prune                  drop entries past the retention, and the bytes
      nothing points at any more.
A restore is undoable: it snapshots what it overwrites before it writes.
Covered: a file's bytes. Not covered: directories, modes, anything a shell
command writes other than the simple rm and mv that name their targets -- say
what an entry skipped rather than promise more."
  "What (help :undo) answers while the cell runs.")

;;; --- paths and protected names ------------------------------------------------

(defparameter +protected-names+
  '(".env" ".netrc" ".git-credentials" "auth.json" "credentials" "credentials.json"
    ".env.local" "secrets.json")
  "Files holding credentials are not copied into a snapshot store.")

(defparameter +protected-suffixes+
  '(".pem" ".key" ".p12" ".pfx" ".kdbx")
  "The suffixes a secret usually wears.")

(defparameter +protected-prefixes+ '("id_rsa" "id_ed25519" "id_ecdsa")
  "Key files named for their algorithm.")

(defun protected-name-p (name)
  "Is NAME a file the store keeps out of its snapshots?"
  (let ((name (string-downcase (or name ""))))
    (or (member name +protected-names+ :test #'string=)
        (some (lambda (suffix) (and (> (length name) (length suffix))
                                    (string= suffix name :start2 (- (length name) (length suffix)))))
              +protected-suffixes+)
        (some (lambda (prefix) (and (>= (length name) (length prefix))
                                    (string= prefix name :start2 0)))
              +protected-prefixes+))))

(defun resolve-path (path)
  "PATH as an absolute namestring: a relative one is against the session's
directory, which the engine's own reader already knows how to find."
  (when path
    (let* ((text (string path))
           (reader (find-symbol "EVAL-SNIPPET-PATH" '#:nodecode.evolved)))
      (or (ignore-errors (namestring (if (and reader (fboundp reader))
                                         (funcall reader text)
                                         (pathname text))))
          (ignore-errors (namestring (uiop:parse-native-namestring text)))))))

;;; --- the store ------------------------------------------------------------------

(defun store-root () (nlk:home "undo-effects/"))

(defun entries-dir () (nlk:home "undo-effects/entries/"))

(defun manifest-path (id)
  (nlk:home (format nil "undo-effects/entries/~a.sexp" id)))

(defun object-path (sha)
  "Where the bytes whose digest is SHA live: two characters of fan-out, so a
long-lived store does not put every file in one directory."
  (let ((hex (if (and (> (length sha) 7) (string= "sha256:" sha :end2 7))
                 (subseq sha 7)
                 sha)))
    (nlk:home (format nil "undo-effects/objects/~a/~a" (subseq hex 0 (min 2 (length hex))) hex))))

(defun id-p (text)
  "An entry id as this store writes it: digits, letters, a dash."
  (and (stringp text)
       (plusp (length text))
       (<= (length text) 40)
       (every (lambda (c) (or (alphanumericp c) (char= c #\-))) text)))

(defun id-now ()
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time))
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d-~2,'0d"
            year month day hour minute second (random 100))))

(defun octets-of (path)
  "PATH's bytes."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(defun write-octets (path bytes)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create :element-type '(unsigned-byte 8))
    (write-sequence bytes out)))

(defun store-object (sha bytes)
  (let ((path (object-path sha)))
    (unless (probe-file path)
      (write-octets path bytes))))

(defun describe-path (path &key copy)
  "The state of PATH as a plist: (:kind :absent), (:kind :file :sha ... :size
...) when it is a file this store covers, else a kind that says why not.
With COPY the bytes are kept in the object store."
  (handler-case
      (cond
        ((not (probe-file path)) (list :kind :absent))
        ((uiop:directory-pathname-p (pathname path)) (list :kind :directory))
        ((protected-name-p (file-namestring path)) (list :kind :protected))
        (t (let* ((bytes (octets-of path))
                  (size (length bytes)))
             (if (> size (* 1024 1024 (setting :max-file-mb)))
                 (list :kind :too-large :size size)
                 (let ((sha (nlk:sha256-text bytes)))
                   (when copy (store-object sha bytes))
                   (list :kind :file :sha sha :size size))))))
    (error () (list :kind :unreadable))))

(defun save-entry (entry)
  (let ((path (manifest-path (getf entry :id)))
        (*package* (find-package :nodecode-undo))
        (*print-readably* t) (*print-pretty* t) (*print-length* nil) (*print-level* nil))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write entry :stream out :readably t)
      (terpri out))))

(defun load-entry (id)
  (when (id-p id)
    (let ((path (manifest-path id)))
      (when (probe-file path)
        (with-open-file (in path)
          (let ((*read-eval* nil) (*package* (find-package :nodecode-undo)))
            (ignore-errors (read in nil nil))))))))

(defun entries ()
  "Every readable entry, newest first."
  (let ((found '()))
    (when (probe-file (entries-dir))
      (dolist (path (directory (nlk:home "undo-effects/entries/*.sexp")))
        (let ((entry (ignore-errors
                       (with-open-file (in path)
                         (let ((*read-eval* nil) (*package* (find-package :nodecode-undo)))
                           (read in nil nil))))))
          (when (and (listp entry) (getf entry :id)) (push entry found)))))
    (sort found #'> :key (lambda (entry) (or (getf entry :ts) 0)))))

(defun entry-line (entry)
  "One entry as the listing shows it: id, time, what the call was, the paths."
  (format nil "~a  ~a  ~a  ~{~a~^, ~}~@[  (~a)~]"
          (getf entry :id)
          (if (getf entry :ts)
              (multiple-value-bind (s m h d mo y) (decode-universal-time (getf entry :ts))
                (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m))
              "?")
          (or (getf entry :summary) (getf entry :tool) "?")
          (mapcar (lambda (item) (getf item :path)) (getf entry :items))
          (getf entry :restored)))

;;; --- the watch ------------------------------------------------------------------

(defun begin-entry (paths tool summary)
  "Snapshot every covered PATH now, and answer the pending entry, or NIL when
nothing there is worth keeping."
  (let ((items '()))
    (dolist (path (remove-duplicates paths :test #'equal))
      (let ((pre (describe-path path :copy t)))
        (when (member (getf pre :kind) '(:file :absent))
          (push (list :path path :pre pre) items))))
    (when items
      (let ((entry (list :id (id-now) :ts (get-universal-time) :tool tool
                         :summary summary :items (nreverse items))))
        (save-entry entry)
        entry))))

(defun note-post (item post)
  "Record POST in ITEM in place: the entry's own list must see it, and a
(setf (getf item :post) ...) on a local would only move the local."
  (let ((tail (member :post item)))
    (if tail (setf (cadr tail) post) (nconc item (list :post post))))
  item)

(defun finish-entry (entry ok)
  "Read the paths again; an entry that changed nothing is dropped, so the list
carries only calls that did something."
  (let ((items (getf entry :items)))
    (dolist (item items)
      (note-post item (describe-path (getf item :path))))
    (if (and ok (every (lambda (item) (equal (getf item :pre) (getf item :post))) items))
        (ignore-errors (delete-file (manifest-path (getf entry :id))))
        (save-entry entry))))

(defun watching (paths tool summary thunk)
  "Run THUNK with PATHS snapshotted first and read again after; the answer and
the values pass through untouched, and a failure out of THUNK still records
what it left."
  (let ((entry (ignore-errors (begin-entry paths tool summary)))
        (result nil) (ok nil))
    (unwind-protect
         (progn (setf result (multiple-value-list (funcall thunk)) ok t))
      (when entry (ignore-errors (finish-entry entry ok))))
    (values-list result)))

;;; --- what a shell command removes -------------------------------------------------

(defparameter +simple-targets+ '("rm" "mv")
  "The commands whose arguments name the files they remove or overwrite.")

(defun shell-targets (command)
  "The paths a simple COMMAND removes or overwrites, or NIL when the command
is anything but one rm or mv: a pipeline, a redirect, a subshell hides its
targets, and a snapshot of a guess is worse than none."
  (when (and (stringp command)
             (not (some (lambda (c) (find c "&;|<>`$()")) command)))
    (let* ((words (remove "" (uiop:split-string command :separator '(#\Space #\Tab #\Newline)) :test #'equal))
           (head (and words (file-namestring (pathname (first words)))))
           (rest (rest words)))
      (when (member head +simple-targets+ :test #'string=)
        (let ((targets (remove-if (lambda (word) (or (zerop (length word)) (char= #\- (char word 0)))) rest)))
          (when targets (mapcar #'resolve-path targets)))))))

;;; --- the hooks ----------------------------------------------------------------------

(defun around-write (next path text)
  "WRITE-FILE: the file as it is now is the snapshot."
  (let ((resolved (resolve-path path)))
    (if resolved
        (watching (list resolved) "write-file" (format nil "write-file ~a" (file-namestring resolved))
                  (lambda () (funcall next path text)))
        (funcall next path text))))

(defun around-edit (next path old new)
  (let ((resolved (resolve-path path)))
    (if resolved
        (watching (list resolved) "edit" (format nil "edit ~a" (file-namestring resolved))
                  (lambda () (funcall next path old new)))
        (funcall next path old new))))

(defun around-sh (next command &rest args)
  (let ((targets (ignore-errors (shell-targets command))))
    (if targets
        (watching targets "sh" (format nil "sh ~a" (subseq command 0 (min 80 (length command))))
                  (lambda () (apply next command args)))
        (apply next command args))))

;;; --- putting it back -------------------------------------------------------------

(defun put-back (path pre)
  "Make PATH what PRE says it was."
  (case (getf pre :kind)
    (:absent (when (probe-file path) (delete-file path)))
    (:file (let ((source (object-path (getf pre :sha))))
             (unless (probe-file source)
               (error "the snapshot of ~a is missing from the store" path))
             (write-octets path (octets-of source))))))

(defun restore-plan (entry force)
  "Each item with the decision taken: :restore, :unchanged or :conflict."
  (loop for item in (getf entry :items)
        for pre = (getf item :pre)
        for post = (getf item :post)
        for current = (describe-path (getf item :path))
        collect (list :item item
                      :decision (cond ((equal current pre) :unchanged)
                                      ((or force (null post) (equal current post)) :restore)
                                      (t :conflict)))))

(defun restore-entry (entry force dry-run)
  "Put ENTRY back. A restore snapshots what it overwrites first, so it can
itself be undone. => a plist report."
  (let* ((plan (restore-plan entry force))
         (todo (remove-if-not (lambda (row) (eq :restore (getf row :decision))) plan))
         (report (list :id (getf entry :id) :changed (mapcar (lambda (row) (getf (getf row :item) :path)) todo)
                       :conflict (loop for row in plan when (eq :conflict (getf row :decision))
                                       collect (getf (getf row :item) :path))
                       :dry-run dry-run))
         (safety nil))
    (unless (or dry-run (null todo))
      (setf safety (ignore-errors
                     (begin-entry (mapcar (lambda (row) (getf (getf row :item) :path)) todo)
                                  "undo-effects.restore"
                                  (format nil "before restoring ~a" (getf entry :id)))))
      (loop for row in todo
            for path = (getf (getf row :item) :path)
            for pre = (getf (getf row :item) :pre)
            do (handler-case (put-back path pre)
                 (error (e) (push (format nil "~a (~a)" path e) (getf report :conflict)))))
      (when safety (ignore-errors (finish-entry safety t)))
      (setf (getf report :safety) (and safety (getf safety :id))))
    (when (and (getf report :changed) (not dry-run) (null (getf report :conflict)))
      (setf (getf entry :restored) t)
      (ignore-errors (save-entry entry)))
    report))

;;; --- housekeeping -------------------------------------------------------------------

(defun prune-store ()
  "Drop entries past the retention, then the bytes nothing points at."
  (let* ((all (entries))
         (cutoff (- (get-universal-time) (* 86400 (setting :retention-days))))
         (keep (setting :keep-last))
         (removed 0))
    (loop for entry in all
          for index from 0
          when (or (> cutoff (or (getf entry :ts) 0)) (>= index keep))
            do (when (ignore-errors (delete-file (manifest-path (getf entry :id))) (incf removed))))
    (let ((live (make-hash-table :test #'equal)))
      (dolist (entry (entries))
        (dolist (item (getf entry :items))
          (dolist (state (list (getf item :pre) (getf item :post)))
            (when (equal (getf state :kind) :file)
              (setf (gethash (getf state :sha) live) t)))))
      (when (probe-file (nlk:home "undo-effects/objects/"))
        (dolist (path (directory (nlk:home "undo-effects/objects/*/*")))
          (unless (gethash (file-namestring path) live)
            (ignore-errors (delete-file path))))))
    removed))

;;; --- /undo ---------------------------------------------------------------------------

(defun show-text (entry)
  (with-output-to-string (out)
    (format out "~a  ~a~%~@[~a~%~]"
            (getf entry :id) (or (getf entry :tool) (getf entry :tool) "?") (getf entry :summary))
    (dolist (item (getf entry :items))
      (let ((pre (getf item :pre)) (post (getf item :post)))
        (format out "  ~a~%    before: ~a   after: ~a~%"
                (getf item :path)
                (state-text pre) (state-text post))))
    (when (getf entry :restored) (format out "  (restored once already)~%"))))

(defun state-text (state)
  (case (getf state :kind)
    (:absent "absent")
    (:file (format nil "a file, ~d byte~:p" (or (getf state :size) 0)))
    (:directory "a directory (not covered)")
    (:protected "a credential file (not covered)")
    (:too-large "too large to snapshot")
    (:unreadable "unreadable (not covered)")
    (nil "not recorded yet")
    (t (format nil "~(~a~)" (getf state :kind)))))

(defun select-entry (selector)
  (cond ((null selector) (first (entries)))
        ((member selector '("last" "newest") :test #'string-equal) (first (entries)))
        (t (load-entry selector))))

(defun list-text ()
  (let ((all (entries)))
    (if all
        (with-output-to-string (out)
          (format out "~d snapshot~:p, newest first~%  /undo show ID | /undo restore ID~%" (length all))
          (dolist (entry all) (format out "  ~a~%" (entry-line entry))))
        (format nil "no snapshot yet: nothing a write, an edit or a simple rm did has changed a covered file~%"))))

(defun restore-text (selector flags)
  (let ((entry (select-entry selector)))
    (if (null entry)
        (format nil "no entry ~a~%" (or selector "yet"))
        (let ((report (restore-entry entry (member "--force" flags :test #'string-equal)
                                     (member "--dry-run" flags :test #'string-equal))))
          (with-output-to-string (out)
            (format out "~a~:[  (dry run: nothing written)~;~]: ~d path~:p ~:[would come~;came~] back, ~d left alone~%"
                    (getf report :id) (getf report :dry-run) (length (getf report :changed))
                    (not (getf report :dry-run)) (length (getf report :conflict)))
            (dolist (path (getf report :changed)) (format out "  ~a~%" path))
            (dolist (path (getf report :conflict)) (format out "  ~a (changed since; --force puts it back anyway)~%" path))
            (when (getf report :safety) (format out "  this restore is undoable: /undo restore ~a~%" (getf report :safety))))))))

(defun run-slash (args session-id)
  "The /undo command: no argument lists, as the original's /undo-effects does."
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (or args "") :separator '(#\Space #\Tab)) :test #'equal))
         (verb (string-downcase (or (first words) "list")))
         (rest (rest words)))
    (handler-case
        (cond ((member verb '("list" "ls" "") :test #'string=) (list-text))
              ((member verb '("show" "inspect") :test #'string=)
               (let ((entry (select-entry (first rest))))
                 (if entry (show-text entry) (format nil "no entry ~a~%" (or (first rest) "yet")))))
              ((string= verb "restore")
               (restore-text (first rest) (rest rest)))
              ((string= verb "prune")
               (format nil "~d entr~:p dropped~%" (prune-store)))
              (t (format nil "unknown /undo verb ~s: list | show ID | restore ID|last [--dry-run] [--force] | prune~%" verb)))
      (error (e) (format nil "undo-effect: ~a~%" e)))))

;;; --- the cell -----------------------------------------------------------------------

(nle:define-cell undo
  (:section ("undo")
    (:guide "the store lives under the home; max_file_mb caps one file, retention_days and keep_last bound the store")
    ("max_file_mb" :integer :default 20 :min 1
     :doc "largest file a snapshot keeps; a bigger one is listed as not covered")
    ("retention_days" :integer :default 14 :min 1
     :doc "how long an entry is kept")
    ("keep_last" :integer :default 200 :min 1
     :doc "how many entries survive whatever their age"))
  (:hook 'nodecode.evolved::write-file #'around-write)
  (:hook 'nodecode.evolved::edit #'around-edit)
  (:hook 'nodecode.evolved::sh #'around-sh)
  (:command "undo" 'run-slash
            :description "Put back files an agent deleted or overwrote"
            :argument-hint "[list|show ID|restore ID|last [--dry-run] [--force]|prune]")
  (:help :undo "undo: put back a file a write, an edit or an rm overwrote - /undo restore last" +primer+)
  (:start (lambda ()
            (ignore-errors (prune-store)))))
