;;;; cell.lisp --- restore points for a folder, a rewind that previews, and the
;;;; effects a rewind cannot undo.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One cell, five tools, one command and one hook. A snapshot copies what a
;;;; folder holds into a store under the home, content-addressed, so two
;;;; checkpoints of one unchanged file cost one copy. `checkpoint' takes one on
;;;; demand; the hook on the :TOOL point takes one before the first call of a
;;;; turn that changes files, so the work always has something to fall back to.
;;;; `rewind' previews what going back would revert, restore and remove, and only
;;;; writes when the model confirms. `fork' copies a checkpoint into a sibling
;;;; folder. `effects' is the ledger of what already left the machine -- a push,
;;;; a publish, an apply -- because those are exactly what a rewind cannot undo.
;;;;
;;;; Nothing here reads or writes anything outside its own folder, the folder it
;;;; protects, one sibling folder on a fork, and its own store: a rewind copies
;;;; what it replaces into <home>/time-machine/trash/ before it writes.

(defpackage #:nodecode-time-machine
  (:use #:cl)
  (:nicknames #:time-machine))

(in-package #:nodecode-time-machine)

(nlk:define-peripheral time-machine :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Every rewind restores files; the time-machine cell is the one that knows what
a rewind cannot undo. A checkpoint is a picture of the protected folder's files,
held content-addressed under the home, so the same file at the same text costs
one copy however many checkpoints hold it.
  checkpoint LABEL            take a restore point now; answers its id.
  list [N]                    the restore points, newest first.
  diff ID                     what a rewind to ID would revert, restore, remove.
  rewind ID [--yes] [PATH...]  preview; --yes applies. Only the named paths
      when some are given. What is replaced is kept under the store's trash.
  fork ID [NAME]              copy the folder as of ID into a sibling folder.
  effects [ID]                what left the machine since ID (a git push, a
      publish, an apply), each with the command that compensates it.
  status                      the protected folder, the newest checkpoint, and
      the effects a rewind to it would leave standing.
A call that changes files earns an automatic restore point before it runs, and a
shell command that pushed or published is written into the effect ledger with a
note on the result. A checkpoint covers a file's bytes, never directories, modes
or what a shell wrote beyond the files themselves."
  "What (help :time-machine) answers while the cell runs.")

;;; --- the store ----------------------------------------------------------------

(defun home-string (&optional (relative ""))
  "RELATIVE as a namestring under the organism's home, where this cell keeps
everything of its own."
  (namestring (pathname (nlk:home relative))))

(defun ensure-dir (path)
  (ensure-directories-exist (namestring (pathname path)))
  path)

(defun store-root () (home-string "time-machine/"))
(defun checkpoint-dir () (home-string "time-machine/checkpoints/"))
(defun object-dir () (home-string "time-machine/objects/"))
(defun trash-root () (home-string "time-machine/trash/"))
(defun effects-path () (home-string "time-machine/effects.sexp"))
(defun manifest-path (id) (home-string (format nil "time-machine/checkpoints/~a.sexp" id)))

(defun object-path (sha)
  (let ((hex (if (and (> (length sha) 7) (string= "sha256:" sha :end2 7)) (subseq sha 7) sha)))
    (home-string (format nil "time-machine/objects/~a/~a"
                          (subseq hex 0 (min 2 (length hex))) hex))))

(defvar *checkpointed-at* nil
  "The universal time of the last automatic restore point this process took, so
a run of calls inside the cooldown costs one snapshot.")

(defvar *taken* '()
  "The checkpoints this process took, newest first, so a listing never
  depends on the folder read alone.")

(defvar *rewound-to* nil
  "The checkpoint id the last rewind went back to, for the next status line.")

(defun now-id (prefix)
  (format nil "~a_~d_~3,'0d" prefix (get-universal-time) (random 1000)))

(defun when-text (universal)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (or universal 0) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun mb-text (bytes)
  (format nil "~,1f MB" (/ (or bytes 0) 1048576.0)))

;;; --- the protected folder -----------------------------------------------------

(defparameter +system-folders+
  '("/bin" "/boot" "/dev" "/etc" "/lib" "/lib32" "/lib64" "/opt" "/proc" "/root"
    "/run" "/sbin" "/snap" "/srv" "/sys" "/usr" "/var")
  "Folders a checkpoint refuses to protect: a rewind of one rewrites the system.")

(defun folder-namestring (path)
  (string-right-trim "/" (namestring path)))

(defun under-p (path folder)
  "Does PATH name FOLDER, or something inside it?"
  (let ((prefix (concatenate 'string folder "/")))
    (or (string= path folder)
        (and (<= (length prefix) (length path))
             (string= prefix path :end2 (length prefix))))))

(defun workspace-root ()
  "The protected folder as one absolute namestring, and NIL; or NIL and the
reason there is none."
  (let* ((configured (setting :workspace))
         (given (and (stringp configured) (string-trim " " configured)))
         (raw (if (and given (plusp (length given)))
                  given
                  (ignore-errors (namestring (uiop:getcwd))))))
    (if (null raw)
        (values nil "the folder the session runs in could not be named")
        (let* ((true (ignore-errors (probe-file raw)))
               (name (and true (folder-namestring true))))
          (cond ((null name)
                 (values nil (format nil "~a is not there" raw)))
                ((string= name "") (values nil "the root of the filesystem"))
                ((string= name (folder-namestring (nlk:home "")))
                 (values nil "the home folder itself"))
                ((some (lambda (folder) (under-p name folder)) +system-folders+)
                 (values nil (format nil "~a is a system folder" name)))
                (t (values name nil)))))))

(defun workspace-or-error ()
  (multiple-value-bind (root refusal) (workspace-root)
    (or root (error "no folder to protect: ~a" refusal))))

(defun relative-name (path root)
  (let ((prefix (concatenate 'string root "/"))
        (name (namestring path)))
    (when (and (<= (length prefix) (length name))
               (string= prefix name :end2 (length prefix)))
      (subseq name (length prefix)))))

(defun workspace-files (root)
  "Every regular file under ROOT as (RELATIVE . NAMESTRING), in name order.
.git is left out when the section says so, as is the store when it sits inside."
  (let ((files '())
        (skips (append (when (setting :exclude-git) (list ".git"))
                        (let ((store (folder-namestring (nlk:home ""))))
                          (when (and (under-p store root) (not (string= store root)))
                            (list (subseq store (1+ (length root)))))))))
    (labels ((walk (dir)
               (dolist (sub (ignore-errors (uiop:subdirectories dir)))
                 (unless (member (file-namestring sub) skips :test #'string=)
                   (walk sub)))
               (dolist (file (ignore-errors (uiop:directory-files dir)))
                 (let ((relative (relative-name file root)))
                   (when relative (push (cons relative (namestring file)) files))))))
      (walk (concatenate 'string root "/")))
    (sort files #'string< :key #'car)))

;;; --- files, digests, the object store -----------------------------------------

(defun read-file-bytes (path)
  "PATH's bytes, or NIL when it cannot be read."
  (ignore-errors
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
        (read-sequence bytes in)
        bytes))))

(defun file-digest (path)
  "SHA-256 of PATH's bytes as `sha256:...', or NIL when it cannot be read."
  (let ((bytes (read-file-bytes path)))
    (and bytes (nlk:sha256-text bytes))))

(defun file-size (path)
  (or (ignore-errors (with-open-file (in path :element-type '(unsigned-byte 8))
                       (file-length in)))
      0))

(defun store-object (sha bytes)
  (let ((path (object-path sha)))
    (unless (probe-file path)
      (ensure-dir path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
        (write-sequence bytes out)))
    path))

(defun object-bytes (sha)
  (read-file-bytes (object-path sha)))

(defun write-bytes (path bytes)
  (ensure-dir path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
    (write-sequence bytes out))
  path)

(defun write-text (path text)
  (ensure-dir path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
    (write-string text out))
  path)

(defun read-sexp (path)
  (and (probe-file path)
       (let ((*read-eval* nil) (*package* (find-package :cl)))
         (ignore-errors (with-open-file (in path) (read in nil nil))))))

;;; --- checkpoints --------------------------------------------------------------

(defun take-checkpoint (&key label (auto nil))
  "Copy the protected folder's files into the store and record the manifest.
LABEL is the line a human reads in the list; AUTO marks one this cell took by
itself, which is the only kind pruning is allowed to drop. Answers the manifest."
  (let* ((root (workspace-or-error))
         (limit (* 1024 1024 (setting :max-file-mb)))
         (cap (setting :max-files))
         (files '()) (bytes 0) (skipped 0))
    (ensure-dir (checkpoint-dir))
    (dolist (entry (workspace-files root))
      (destructuring-bind (relative . name) entry
        (let ((size (file-size name)))
          (cond ((> (length files) cap) (incf skipped))
                ((> size limit) (incf skipped))
                (t (let ((data (read-file-bytes name)))
                     (if (null data)
                         (incf skipped)
                         (let ((sha (nlk:sha256-text data)))
                           (store-object sha data)
                           (push (list :path relative :sha sha :size size) files)
                           (incf bytes size)))))))))
    (let* ((id (now-id "ckpt"))
           (manifest (list :id id :label (or label "") :auto (and auto t)
                           :created (get-universal-time) :workspace root
                           :bytes bytes :skipped skipped :files (nreverse files))))
      (with-standard-io-syntax
        (let ((*print-length* nil) (*print-level* nil) (*print-pretty* nil)
              (*print-readably* nil))
          (write-text (manifest-path id) (format nil "~s~%" manifest))))
      (setf *checkpointed-at* (get-universal-time)
            *taken* (cons manifest (remove id *taken* :test #'string= :key (lambda (item) (getf item :id)))))
      manifest)))

(defvar *taken* '()
  "The checkpoints this process took, newest first, so a listing never
  depends on the folder read alone.")

(defun manifests ()
  "Every checkpoint the store holds, newest first."
  (let ((disk (loop for path in (ignore-errors (uiop:directory-files (checkpoint-dir)))
                    for manifest = (read-sexp path)
                    when (and (consp manifest) (getf manifest :id)) collect manifest))
        (seen (make-hash-table :test #'equal))
        (all '()))
    (dolist (manifest (append *taken* disk))
      (let ((id (getf manifest :id)))
        (unless (gethash id seen)
          (setf (gethash id seen) t)
          (push manifest all))))
    (sort all #'> :key (lambda (manifest) (or (getf manifest :created) 0)))))

(defun newest-manifest ()
  (first (manifests)))

(defun find-manifest (id)
  "The checkpoint ID names, `last' and an empty id meaning the newest, or NIL."
  (cond ((or (null id) (string= id "") (string-equal id "last")) (newest-manifest))
        (t (or (find id (manifests) :test #'string= :key (lambda (manifest) (getf manifest :id)))
               (read-sexp (manifest-path id))))))

(defun gc-objects ()
  "Drop the stored bytes no checkpoint points at any more."
  (let ((live (make-hash-table :test #'equal)))
    (dolist (manifest (manifests))
      (dolist (file (getf manifest :files))
        (setf (gethash (getf file :sha) live) t)))
    (dolist (path (ignore-errors (directory (home-string "time-machine/objects/*/*"))))
      (unless (gethash (format nil "sha256:~a" (file-namestring path)) live)
        (ignore-errors (delete-file path))))))

(defun prune-checkpoints ()
  "Drop the automatic checkpoints past the retention, then their bytes. A
checkpoint a human asked for is never dropped."
  (let* ((automatic (remove-if-not (lambda (manifest) (getf manifest :auto)) (manifests)))
         (doomed (nthcdr (setting :retention) automatic))
         (dropped 0))
    (dolist (manifest doomed)
      (when (ignore-errors (delete-file (manifest-path (getf manifest :id))))
        (incf dropped)))
    (when doomed (gc-objects))
    dropped))

;;; --- what a rewind would change ------------------------------------------------

(defun paths-of (paths)
  "PATHS as a list of relative namestrings, `./' and leading slashes trimmed."
  (let ((items (cond ((null paths) nil)
                     ((stringp paths) (list paths))
                     ((vectorp paths) (coerce paths 'list))
                     ((listp paths) paths)
                     (t nil))))
    (remove "" (mapcar (lambda (item)
                         (string-left-trim "/" (string-right-trim "/" (string item))))
                       items)
            :test #'equal)))

(defun path-wanted-p (relative paths)
  (or (null paths)
      (some (lambda (prefix) (under-p relative prefix)) paths)))

(defun compare-workspace (manifest &key paths)
  "What a rewind to MANIFEST would do: MODIFIED (here, changed), DELETED (the
checkpoint holds it, it is gone) and ADDED (here, the checkpoint never saw it),
each a list of relative names, and the count of files that match already."
  (let* ((root (getf manifest :workspace))
         (recorded (make-hash-table :test #'equal))
         (modified '()) (deleted '()) (added '()) (unchanged 0))
    (dolist (file (getf manifest :files))
      (setf (gethash (getf file :path) recorded) file))
    (dolist (file (getf manifest :files))
      (let ((relative (getf file :path)))
        (when (path-wanted-p relative paths)
          (let ((sha (file-digest (concatenate 'string root "/" relative))))
            (cond ((null sha) (push relative deleted))
                  ((string= sha (getf file :sha)) (incf unchanged))
                  (t (push relative modified)))))))
    (dolist (entry (workspace-files root))
      (let ((relative (car entry)))
        (when (and (path-wanted-p relative paths) (not (gethash relative recorded)))
          (push relative added))))
    (list :modified (nreverse modified) :deleted (nreverse deleted)
          :added (nreverse added) :unchanged unchanged)))

(defun apply-rewind (manifest &key paths)
  "Put MANIFEST's files back: what changed is reverted, what the checkpoint
holds and the folder lost is restored, what the checkpoint never saw is removed.
Every file about to be replaced or removed is copied under the store's trash
first, so the rewind itself can be undone by hand."
  (let* ((root (getf manifest :workspace))
         (changes (compare-workspace manifest :paths paths))
         (stamp (now-id "trash"))
         (touched (append (getf changes :modified) (getf changes :deleted) (getf changes :added)))
         (trashed 0) (written 0) (removed 0) (errors '()))
    (dolist (relative touched)
      (let ((name (concatenate 'string root "/" relative)))
        (when (probe-file name)
          (let ((target (home-string (format nil "time-machine/trash/~a/~a" stamp relative))))
            (ensure-dir target)
            (when (ignore-errors (rename-file name target)) (incf trashed))))))
    (dolist (relative (append (getf changes :modified) (getf changes :deleted)))
      (let* ((file (find relative (getf manifest :files)
                          :test #'string= :key (lambda (item) (getf item :path))))
             (bytes (and file (object-bytes (getf file :sha)))))
        (if (null bytes)
            (push (format nil "~a: its bytes are no longer in the store" relative) errors)
            (handler-case (progn (write-bytes (concatenate 'string root "/" relative) bytes)
                                 (incf written))
              (error (condition) (push (format nil "~a: ~a" relative condition) errors))))))
    (dolist (relative (getf changes :added))
      (when (ignore-errors (delete-file (concatenate 'string root "/" relative))) (incf removed)))
    (setf *rewound-to* (getf manifest :id))
    (list :checkpoint (getf manifest :id) :written written :removed removed
          :trashed trashed :trash stamp :errors (nreverse errors) :changes changes)))

(defun fork-checkpoint (manifest &key name)
  "Write MANIFEST's files into a sibling of the protected folder and answer its
path. Nothing there is touched: the folder is new."
  (let* ((root (getf manifest :workspace))
         (suffix (let ((given (and name (string-trim " " (string name)))))
                   (if (and given (plusp (length given)))
                       (format nil "~a-~a" given (get-universal-time))
                       (format nil "fork-~d" (get-universal-time)))))
         (target (format nil "~a-~a" root suffix))
         (written 0) (missing 0))
    (when (probe-file target)
      (error "~a is already there" target))
    (ensure-dir (concatenate 'string target "/"))
    (dolist (file (getf manifest :files))
      (let ((bytes (object-bytes (getf file :sha))))
        (cond ((null bytes) (incf missing))
              (t (write-bytes (concatenate 'string target "/" (getf file :path)) bytes)
                 (incf written)))))
    (list :path target :written written :missing missing :checkpoint (getf manifest :id))))

;;; --- the effect ledger ---------------------------------------------------------

(defparameter +effects+
  '((" git push" "git push" "a remote branch"
     "git revert <sha> and push, or git push --force-with-lease <remote> <previous-sha>:<branch>")
    (" npm publish" "npm publish" "the npm registry"
     "npm unpublish <package>@<version>")
    (" pnpm publish" "npm publish" "the npm registry"
     "npm unpublish <package>@<version>")
    (" yarn publish" "yarn publish" "the npm registry"
     "npm unpublish <package>@<version>")
    (" docker push" "docker push" "a registry"
     "delete the tag on the registry; the layers stay until it collects them")
    (" gh release create" "release" "the forge"
     "gh release delete <tag>")
    (" kubectl apply" "kubectl apply" "a cluster"
     "kubectl rollout undo for a workload, or kubectl delete -f the same manifest")
    (" kubectl delete" "kubectl delete" "a cluster"
     "kubectl apply -f the manifest that held the object")
    (" terraform apply" "terraform apply" "the provider"
     "terraform destroy -target=<resource>, or apply the state that was there before")
    (" fly deploy" "deploy" "the platform"
     "fly deploy a previous image; the running release is already replaced")
    (" flyctl deploy" "deploy" "the platform"
     "fly deploy a previous image; the running release is already replaced")
    (" heroku" "deploy" "the platform"
     "roll back to the previous release")
    (" poetry publish" "publish" "the package index"
     "yank the release on the index")
    (" twine upload" "publish" "the package index"
     "yank the release on the index"))
  "Shell commands that change something outside the folder. Each is (NEEDLE KIND
TARGET COMPENSATION); NEEDLE carries its leading space so `confirm' is not a push.")

(defparameter +mutating-needles+
  '(" rm " " rm -" " mv " " cp " " mkdir " " touch " " sed -i" " truncate " " tee "
    " git checkout" " git restore" " git reset" " git clean" " git rebase"
    " git merge" " git stash" " git switch" " git apply"
    " npm install" " pnpm install" " yarn install" " pip install" " poetry add"
    " chmod " " chown " " > " " >> " " alembic " " prisma " " migrate")
  "Shell commands carrying one of these change files, so a call running one
earns a restore point before it runs.")

(defun command-of (arguments)
  "The shell command a tool call carries, when it carries one."
  (when (hash-table-p arguments)
    (let ((command (or (gethash "command" arguments)
                       (gethash "cmd" arguments)
                       (gethash "script" arguments))))
      (and (stringp command) (plusp (length (string-trim " " command))) command))))

(defun mutating-call-p (name command)
  "Can this call change a file in the folder?"
  (or (member name '("write-file" "edit" "patch") :test #'string=)
      (and command
           (let ((text (concatenate 'string " " (string-downcase command))))
             (and (some (lambda (needle) (search needle text)) +mutating-needles+) t)))))

(defun effect-of (command)
  "The first entry of +EFFECTS+ COMMAND matches, or NIL."
  (and command
       (let ((text (concatenate 'string " " (string-downcase command))))
         (find-if (lambda (rule) (search (first rule) text)) +effects+))))

(defun target-of (command)
  (let* ((words (uiop:split-string (string-trim " " (string command))
                                    :separator '(#\Space #\Tab)))
         (rest (remove-if (lambda (word) (or (string= word "") (char= #\- (char word 0))))
                          (rest words))))
    (and rest (format nil "~{~a~^ ~}" (subseq rest 0 (min 3 (length rest)))))))

(defun read-effects ()
  (let ((items '())
        (*read-eval* nil)
        (*package* (find-package :keyword)))
    (when (probe-file (effects-path))
      (ignore-errors
        (with-open-file (in (effects-path))
          (loop for form = (read in nil nil) while form do (push form items)))))
    (nreverse items)))

(defun record-effect (rule command)
  "Write the fact that COMMAND crossed RULE's point of no return."
  (let ((entry (list :ts (get-universal-time) :kind (second rule) :target (third rule)
                      :command (string-trim " " (string command))
                      :compensation (fourth rule) :reversible nil)))
    (ensure-dir (store-root))
    (with-standard-io-syntax
      (let ((*print-pretty* nil) (*print-length* nil))
        (with-open-file (out (effects-path) :direction :output :if-exists :append
                                               :if-does-not-exist :create)
          (write-string (format nil "~s~%" entry) out))))
    entry))

(defun effects-since (id)
  "The ledger entries at or after the checkpoint ID names; the whole ledger
when there is no such checkpoint."
  (let* ((manifest (find-manifest id))
         (since (and manifest (getf manifest :created)))
         (items (read-effects)))
    (values (if since (remove-if (lambda (entry) (< (getf entry :ts) since)) items) items)
            (and manifest (getf manifest :id)))))

(defun effect-line (entry)
  (format nil "~a  ~a~@[ -> ~a~]  (no rewind undoes this)~@[~%    undo: ~a~]"
          (when-text (getf entry :ts)) (getf entry :kind)
          (getf entry :target) (getf entry :compensation)))

;;; --- the texts a human and a model read ----------------------------------------

(defun changes-lines (changes &key (limit 8))
  (let ((lines (list (format nil "Will revert ~d, restore ~d, remove ~d; ~d unchanged."
                              (length (getf changes :modified))
                              (length (getf changes :deleted))
                              (length (getf changes :added))
                              (getf changes :unchanged)))))
    (dolist (pair (list (cons "revert " (getf changes :modified))
                        (cons "restore " (getf changes :deleted))
                        (cons "remove " (getf changes :added))))
      (dolist (relative (subseq (cdr pair) 0 (min limit (length (cdr pair)))))
        (push (format nil "  ~a~a" (car pair) relative) lines))
      (when (> (length (cdr pair)) limit)
        (push (format nil "  ... and ~d more" (- (length (cdr pair)) limit)) lines)))
    (nreverse lines)))

(defun preview-text (manifest &key paths)
  (with-output-to-string (out)
    (format out "rewind preview: ~a~@[ (~a)~]~%" (getf manifest :id) (getf manifest :label))
    (dolist (line (changes-lines (compare-workspace manifest :paths paths)))
      (format out "~a~%" line))
    (multiple-value-bind (effects since) (effects-since (getf manifest :id))
      (declare (ignore since))
      (when effects
        (format out "A rewind will NOT undo ~d effect~:p recorded since:~%" (length effects))
        (dolist (entry (last effects (min 5 (length effects))))
          (format out "  ~a~%" (effect-line entry)))))
    (format out "Apply with: rewind ~a --yes~%" (getf manifest :id))))

(defun rewind-text (report manifest)
  (with-output-to-string (out)
    (format out "rewound ~a: ~d file~:p written, ~d removed, ~d kept in the trash under ~a~%"
            (getf manifest :id) (getf report :written) (getf report :removed)
            (getf report :trashed) (getf report :trash))
    (dolist (error (getf report :errors)) (format out "  ~a~%" error))
    (let ((effects (effects-since (getf manifest :id))))
      (when effects
        (format out "Still true outside the folder (~d):~%" (length effects))
        (dolist (entry effects) (format out "  ~a~%" (effect-line entry)))))))

(defun list-text (&key (limit 20) (root nil))
  (let* ((all (if root
                   (remove-if-not (lambda (m) (string= root (getf m :workspace))) (manifests))
                   (manifests)))
         (shown (subseq all 0 (min limit (length all)))))
    (if all
        (with-output-to-string (out)
          (format out "~d checkpoint~:p~@[, newest ~d~]~%" (length all) (unless (= (length all) (length shown)) (length shown)))
          (dolist (manifest shown)
            (format out "  ~a  ~a  ~8a  ~d file~:p  ~a  ~a~%"
                    (getf manifest :id) (when-text (getf manifest :created))
                    (if (getf manifest :auto) "auto" "manual")
                    (length (getf manifest :files)) (mb-text (getf manifest :bytes))
                    (getf manifest :label))))
        (format nil "no checkpoint yet: nothing has taken a restore point of ~a~%"
                (or root (getf (first (manifests)) :workspace) "the folder")))))

(defun status-text ()
  (multiple-value-bind (root refusal) (workspace-root)
    (with-output-to-string (out)
      (format out "time-machine~%")
      (if root
          (format out "Folder: ~a~%" root)
          (format out "Folder: not protected (~a)~%" refusal))
      (let ((newest (and root (find-if (lambda (m) (string= root (getf m :workspace))) (manifests)))))
        (format out "Checkpoints: ~d~%" (if root
                                             (count root (manifests) :test #'string= :key (lambda (m) (getf m :workspace)))
                                             (length (manifests))))
        (when newest
          (format out "Newest: ~a  ~a  ~d file~:p  ~a  ~a~%"
                  (getf newest :id) (when-text (getf newest :created))
                  (length (getf newest :files)) (mb-text (getf newest :bytes))
                  (getf newest :label)))
        (when (and root newest)
          (multiple-value-bind (effects since) (effects-since (getf newest :id))
            (if effects
                (progn
                  (format out "Effects a rewind to ~a would NOT undo (~d):~%" since (length effects))
                  (dolist (entry (last effects (min 5 (length effects))))
                    (format out "  ~a~%" (effect-line entry))))
                (format out "No effect since ~a: a rewind is clean.~%" since)))))
      (format out "Store: ~a~%" (store-root)))))

;;; --- the tools the model calls -------------------------------------------------

(nlk:define-contract checkpoint-request
  (:doc "Take a restore point of the protected folder now. Use it before a large refactor, a migration, a dependency upgrade, or any change you may want to undo as a whole. Checkpoints are incremental, so repeating one is cheap. It answers the checkpoint id that rewind and fork take. An automatic restore point already happens before the first file change of a turn.")
  ("label" :string :default "" :doc "a short label a human reads in the list, e.g. 'before the schema migration'"))

(nlk:define-contract rewind-request
  (:doc "Preview or restore the protected folder to a checkpoint. Use it when the changes broke the project and fixing forward costs more than starting over. A preview is the default: it names the files it would revert, restore and remove, and the effects recorded outside the folder that a restore will not undo. confirm=true performs the restore; the file it replaces is kept in the store's trash first.")
  ("checkpoint_id" :string :default "" :doc "the checkpoint to go back to; empty means the newest")
  ("confirm" :boolean :default nil :doc "true performs the restore, false only previews it")
  ("paths" :array :of :string :doc "restore only these files or folders, as relative names"))

(nlk:define-contract fork-request
  (:doc "Copy the protected folder as it was at a checkpoint into a new sibling folder, so an alternative can be tried without touching the current work. Use it when two approaches must be compared instead of one overwriting the other. It answers the new path and never writes inside the current folder.")
  ("checkpoint_id" :string :default "" :doc "the checkpoint to copy; empty means the newest")
  ("name" :string :default "" :doc "a name for the new folder, to tell two forks apart"))

(nlk:define-contract list-request
  (:doc "List the restore points of the protected folder, newest first, with their size, file count and label. Use it to pick the id rewind or fork takes.")
  ("limit" :integer :default 20 :doc "how many to list"))

(nlk:define-contract effects-request
  (:doc "List what this session did outside the folder since a checkpoint -- a push, a publish, a deploy, an apply -- with the command that compensates each one. A rewind restores files and cannot undo these, so read this before a rewind, or when you must remember what already left the machine.")
  ("since" :string :default "" :doc "the checkpoint to look back to; empty means the newest"))

(defun checkpoint-tool (request)
  "The checkpoint tool: one restore point now, and its id."
  (let ((manifest (take-checkpoint :label (checkpoint-request-label request))))
    (format nil "checkpoint ~a ~d file~:p, ~a~@[  ~a~]. rewind ~a goes back to it."
            (getf manifest :id) (length (getf manifest :files)) (mb-text (getf manifest :bytes))
            (let ((label (getf manifest :label)))
              (and (plusp (length label)) label))
            (getf manifest :id))))

(defun rewind-tool (request)
  "The rewind tool: a preview, or the restore when confirm is true."
  (let ((manifest (find-manifest (rewind-request-checkpoint-id request)))
        (paths (paths-of (rewind-request-paths request))))
    (unless manifest
      (error "no checkpoint ~a: list shows the ones there are"
             (rewind-request-checkpoint-id request)))
    (if (rewind-request-confirm request)
        (rewind-text (apply-rewind manifest :paths paths) manifest)
        (let ((text (preview-text manifest :paths paths)))
          (if paths
              text
              (format nil "~aThe files it replaces are kept in the trash. Nothing was written." text))))))

(defun fork-tool (request)
  "The fork tool: the checkpoint's files, in a sibling folder."
  (let ((manifest (find-manifest (fork-request-checkpoint-id request))))
    (unless manifest
      (error "no checkpoint ~a to fork from" (fork-request-checkpoint-id request)))
    (let ((report (fork-checkpoint manifest :name (fork-request-name request))))
      (format nil "forked checkpoint ~a into ~a: ~d file~:p~@[, ~d missing from the store~]"
              (getf report :checkpoint) (getf report :path) (getf report :written)
              (let ((missing (getf report :missing)))
                (and (plusp missing) missing))))))

(defun list-tool (request)
  "The list tool: the restore points of this folder, newest first."
  (multiple-value-bind (root refusal) (workspace-root)
    (declare (ignore refusal))
    (list-text :limit (list-request-limit request) :root root)))

(defun effects-tool (request)
  "The effects tool: what a rewind cannot undo, with its compensation."
  (multiple-value-bind (effects since) (effects-since (effects-request-since request))
    (with-output-to-string (out)
      (format out "effects since ~a: ~d~%" (or since "the beginning") (length effects))
      (if effects
          (dolist (entry effects) (format out "~a~%" (effect-line entry)))
          (format out "none recorded: every rewind so far is clean~%"))
      (format out "~:[Files only: the effects above stay.~;Safe to rewind.~]~%" (null effects)))))

;;; --- the one hook: the restore point and the ledger ----------------------------

(defun effect-note (rule)
  (format nil "[time-machine] point of no return: ~a -> ~a. A rewind restores files and cannot undo this.~@[ To compensate: ~a~]~%"
          (second rule) (or (third rule) "outside the folder") (fourth rule)))

(defun observe-tool (op next)
  "A :TOOL point hook. OP is (:name TOOL-NAME :arguments HASH :call-id ID).
Before a call that changes files: the first one of a turn earns a restore point.
After it: a command that crossed a point of no return is written into the
ledger, and the result says so where the model reads it."
  (let* ((name (getf op :name))
         (command (command-of (getf op :arguments)))
         (checkpoint (when (and (setting :auto-checkpoint)
                                (mutating-call-p name command)
                                (or (null *checkpointed-at*)
                                    (>= (- (get-universal-time) *checkpointed-at*)
                                        (setting :mutation-cooldown-seconds))))
                       (ignore-errors (take-checkpoint :auto t)))))
    (multiple-value-bind (text thunk) (funcall next op)
      (let ((rule (effect-of command)))
        (when rule
          (record-effect rule command)
          (when (and (setting :annotate-results) (stringp text))
            (setf text (concatenate 'string text (effect-note rule))))))
      (when (and checkpoint (setting :annotate-results) (stringp text))
        (setf text (concatenate 'string text
                                (format nil "[time-machine] restore point ~a holds this folder before the call; rewind ~a --yes puts it back.~%"
                                        (getf checkpoint :id) (getf checkpoint :id)))))
      (values text thunk))))

;;; --- /time-machine ------------------------------------------------------------

(defun run-slash (args session-id)
  "The /time-machine command: status, list, diff, rewind, fork, effects, help."
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (or args "") :separator '(#\Space #\Tab))
                        :test #'equal))
         (verb (string-downcase (or (first words) "status")))
         (rest (rest words)))
    (handler-case
        (cond ((member verb '("status" "") :test #'string=) (status-text))
              ((member verb '("list" "ls") :test #'string=)
               (list-text :limit (let ((first (first rest)))
                                   (if (and first (every #'digit-char-p first))
                                       (max 1 (min 50 (parse-integer first)))
                                       20))))
              ((string= verb "diff")
               (let ((manifest (find-manifest (first rest))))
                 (if manifest
                     (with-output-to-string (out)
                       (format out "differences from ~a~%" (getf manifest :id))
                       (dolist (line (changes-lines (compare-workspace manifest)))
                         (format out "~a~%" line)))
                     (format nil "no checkpoint ~a~%" (or (first rest) "yet")))))
              ((member verb '("rewind" "restore") :test #'string=)
               (let* ((apply (member "--yes" rest :test #'string=))
                      (tokens (remove "--yes" rest :test #'string=))
                      (manifest (find-manifest (first tokens)))
                      (paths (paths-of (rest tokens))))
                 (cond ((null manifest)
                        (format nil "no checkpoint ~a~%" (or (first tokens) "yet")))
                       (apply (rewind-text (apply-rewind manifest :paths paths) manifest))
                       (t (preview-text manifest :paths paths)))))
              ((string= verb "fork")
               (let ((manifest (find-manifest (first rest))))
                 (if manifest
                     (let ((report (fork-checkpoint manifest :name (second rest))))
                       (format nil "forked ~a into ~a: ~d file~:p~%"
                               (getf report :checkpoint) (getf report :path) (getf report :written)))
                     (format nil "no checkpoint ~a~%" (or (first rest) "yet")))))
              ((string= verb "effects")
               (effects-tool (make-effects-request :since (or (first rest) ""))))
              ((member verb '("help" "-h" "--help") :test #'string=) +primer+)
              (t (format nil "unknown /time-machine verb ~s: status | list [N] | diff ID | rewind ID [--yes] [PATH...] | fork ID [NAME] | effects [ID] | help~%"
                          verb)))
      (error (condition) (format nil "time-machine: ~a~%" condition)))))

;;; --- the cell -----------------------------------------------------------------

(nle:define-cell time-machine
  (:section ("time-machine")
    (:guide "workspace is the folder to protect (empty: the one the session runs in); nothing else is required")
    ("workspace" :string :default ""
     :doc "absolute path of the folder to protect; empty means the folder the session runs in")
    ("auto_checkpoint" :boolean :default t
     :doc "take a restore point before a call that changes files")
    ("mutation_cooldown_seconds" :integer :default 120 :min 0
     :doc "least seconds between two automatic restore points")
    ("retention" :integer :default 20 :min 1
     :doc "automatic checkpoints kept; a checkpoint a human asked for is never pruned")
    ("max_file_mb" :integer :default 50 :min 1
     :doc "largest file a checkpoint copies; a bigger one is counted as skipped")
    ("max_files" :integer :default 5000 :min 1
     :doc "most files one checkpoint walks")
    ("exclude_git" :boolean :default t
     :doc "leave the .git folder out of checkpoints: faster, and a rewind keeps the history")
    ("annotate_results" :boolean :default t
     :doc "say in a tool result where its restore point is, and when it crossed a point of no return"))
  (:tool "checkpoint" (nlk:contract 'checkpoint-request) #'checkpoint-tool)
  (:tool "rewind" (nlk:contract 'rewind-request) #'rewind-tool)
  (:tool "fork_checkpoint" (nlk:contract 'fork-request) #'fork-tool)
  (:tool "list_checkpoints" (nlk:contract 'list-request) #'list-tool)
  (:tool "list_effects" (nlk:contract 'effects-request) #'effects-tool)
  (:hook :tool #'observe-tool)
  (:command "time-machine" #'run-slash
            :description "Restore points, a rewind preview, and what a rewind cannot undo"
            :argument-hint "[status|list [N]|diff ID|rewind ID [--yes] [PATH...]|fork ID [NAME]|effects [ID]]")
  (:help :time-machine
         "time-machine: restore points, and the effects a rewind cannot undo - /time-machine rewind ID --yes"
         +primer+
         )
  (:start (lambda ()
            (ensure-dir (store-root))
            (ensure-dir (checkpoint-dir))
            (ignore-errors (prune-checkpoints))
            (setf *checkpointed-at* nil))))
