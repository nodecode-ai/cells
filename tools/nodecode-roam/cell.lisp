;;;; cell.lisp --- the operator's own backup: an encrypted bundle, two machines.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of vcruz305/hermes-roam (see NOTICE). The original pushed
;;;; a sealed bundle of its home to a store the user owned and pulled it
;;;; back on the other machine, with three tools (roam_status, roam_push,
;;;; roam_pull), a /roam command and a bundled skill. The names, the
;;;; answers and the discipline are kept: the model may look and push,
;;;; but a pull that writes needs the operator's allow_agent_apply; every
;;;; restore copies what it replaces into a backup first and journals it;
;;;; a push over a store another machine moved past the one this home
;;;; last saw is refused unless it is forced (roam/engine.py, head.py).
;;;;
;;;; What the cell does not do: it never runs a command, never reads a
;;;; credential file, and reaches only the store the section names.

(in-package #:nodecode-roam)

(nlk:define-peripheral roam :not-running t :lock "roam")

;;; --- what this home remembers -------------------------------------------------

(defun state-file ()
  "What this home remembers about its store, kept with its backups."
  (nlk:home ".roam/state.json"))

(defun read-state ()
  "=> a plist (:last-version :last-digest :last-at :pulled-at); NIL when none."
  (let ((file (state-file)))
    (when (probe-file file)
      (let ((object (ignore-errors (nlk:decode-json (uiop:read-file-string file)))))
        (when (hash-table-p object)
          (list :last-version (nlk:json-value object :integer "lastVersion")
                :last-digest (nlk:json-value object :string "contentSha256")
                :last-at (nlk:json-value object :string "lastPushAt")
                :pulled-at (nlk:json-value object :string "lastPullAt")))))))

(defun write-state (&key last-version last-digest last-at pulled-at)
  "Remember LAST-VERSION of the store, the content digest that went with
it, and when this home last pushed or pulled."
  (let ((old (read-state)))
    (nlk:write-file-atomically
     (state-file)
     (format nil "~a~%"
             (nlk:encode-json-object
              (nlk:json-object "format" +format+
                               "profile" (bundle-profile)
                               "lastVersion" (or last-version (getf old :last-version) :null)
                               "contentSha256" (or last-digest (getf old :last-digest) :null)
                               "lastPushAt" (or last-at (getf old :last-at) :null)
                               "lastPullAt" (or pulled-at (getf old :pulled-at) :null))))
     :mode :private)))

;;; --- the passphrase -----------------------------------------------------------

(defun non-empty (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun passphrase ()
  "The passphrase the section points at -- an environment variable or a
file -- or a refusal naming both places it could be."
  (let* ((name (non-empty (setting :passphrase-env)))
         (file (setting :passphrase-file))
         (from-env (and name (non-empty (uiop:getenv name))))
         (from-file (and file (probe-file file) (non-empty (ignore-errors (uiop:read-file-string file))))))
    (or from-env from-file
        (fail "roam: the bundle is encrypted and no passphrase was found: set ~@[the variable ~a~]~@[ or write one in ~a~], then try again"
              name (and file (namestring file))))))

;;; --- this home, as a manifest ---------------------------------------------------

(defun local-scan ()
  "This home in scope. => (values ENTRIES SKIPPED DIGEST BYTES)."
  (multiple-value-bind (entries skipped) (manifest-entries)
    (values entries skipped (content-digest entries)
            (loop for entry in entries sum (getf entry :size)))))

(defun head-version (head)
  (and (hash-table-p head) (nlk:json-value head :integer "version")))

;;; --- the tools' answers -----------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: roam: ~a" condition)))))

(defun argument (args name)
  "ARGS' NAME, or NIL; ARGS is NIL when a call carries no arguments."
  (and (hash-table-p args) (gethash name args)))

(defun true-p (value)
  "VALUE as a switch: JSON true, the string \"true\"/\"yes\", or 1."
  (cond ((eq value t) t)
        ((eq value :true) t)
        ((and (integerp value) (= value 1)) t)
        ((and (stringp value) (member (string-downcase (nlk:trimmed value)) '("true" "yes" "1") :test #'string=)) t)
        (t nil)))

(defun true-json (value)
  (if value :true :false))

;;; --- roam_status --------------------------------------------------------------------

(defun remote-summary (head)
  "HEAD as the answer's remote object, or :null when the store holds none."
  (if head
      (nlk:json-object "version" (or (head-version head) 0)
                        "savedAt" (or (nlk:json-value head :string "savedAt") :null)
                        "profile" (or (nlk:json-value head :string "profile") :null)
                        "files" (or (nlk:json-value head :integer "files") 0)
                        "bytes" (or (nlk:json-value head :integer "bytes") 0)
                        "contentSha256" (or (nlk:json-value head :string "contentSha256") :null))
      :null))

(defun status-plist ()
  "What this home and its store hold now, as one plist."
  (multiple-value-bind (entries skipped digest bytes) (local-scan)
    (let* ((state (read-state))
           (head (store-head)))
      (list :entries entries :skipped skipped :digest digest :bytes bytes
            :state state :head head
            :changed (not (equal digest (getf state :last-digest)))
            :drift (and head (not (equal digest (nlk:json-value head :string "contentSha256"))))))))

(defun tool-status (args)
  "roam_status: what this home holds, what the store holds, and whether they differ."
  (declare (ignore args))
  (answering
   (running-settings)
   (let* ((status (status-plist))
          (entries (getf status :entries))
          (head (getf status :head)))
     (nlk:encode-json-object
      (nlk:json-object
       "success" :true
       "backend" (store-backend)
       "store" (store-what)
       "profile" (bundle-profile)
       "local" (nlk:json-object
                "files" (length entries)
                "bytes" (getf status :bytes)
                "contentSha256" (getf status :digest)
                "changedSinceLastPush" (true-json (getf status :changed))
                "skipped" (coerce (getf status :skipped) 'vector))
       "remote" (remote-summary head)
       "drift" (true-json (getf status :drift))
       "message" (cond ((null head)
                         (format nil "~d file~:p in scope; the store holds no bundle for profile ~a yet"
                                 (length entries) (bundle-profile)))
                        ((getf status :drift)
                         (format nil "~d file~:p in scope; the store's version ~a was taken from another state — roam_pull would show the difference"
                                 (length entries) (head-version head)))
                        (t (format nil "~d file~:p in scope and the store's version ~a is the same content"
                                   (length entries) (head-version head)))))))))

;;; --- roam_push ----------------------------------------------------------------------

(defun push-once (&key force note)
  "Seal this home and write it to the store. => a plist of what went."
  (let* ((status (status-plist))
         (head (getf status :head))
         (state (getf status :state))
         (remote (or (head-version head) 0))
         (seen (or (getf state :last-version) 0))
         (entries (getf status :entries)))
    (when (and head (> remote seen) (not force))
      (fail "roam: the store holds version ~d and this home last saw version ~d: pull it here first, or push with force to replace it"
            remote seen))
    (let* ((version (max (1+ remote) (1+ seen)))
           (profile (bundle-profile))
           (saved-at (nlk:iso-time (nlk:unix-now)))
           (bytes (getf status :bytes))
           (digest (getf status :digest))
           (head-object (nlk:json-object
                         "format" +format+
                         "version" version
                         "savedAt" saved-at
                         "profile" profile
                         "files" (length entries)
                         "bytes" bytes
                         "contentSha256" digest
                         "note" (or (non-empty note) :null)
                         "origin" (nlk:profile-name (nlk:home))))
           (manifest (manifest-json entries :saved-at saved-at :profile profile :digest digest))
           (bundle (seal-bundle (passphrase)
                                 (nlk:encode-json-octets head-object)
                                 (nlk:encode-json-octets manifest))))
      (store-write bundle)
      (write-state :last-version version :last-digest digest :last-at saved-at)
      (list :version version :files (length entries) :bytes bytes :digest digest
            :skipped (getf status :skipped) :at saved-at))))

(defun tool-push (args)
  "roam_push: seal this home and write it to the store."
  (answering
   (running-settings)
   (let ((result (push-once :force (true-p (argument args "force"))
                            :note (argument args "note"))))
     (nlk:encode-json-object
      (nlk:json-object
       "success" :true
       "version" (getf result :version)
       "files" (getf result :files)
       "bytes" (getf result :bytes)
       "contentSha256" (getf result :digest)
       "savedAt" (getf result :at)
       "skipped" (coerce (getf result :skipped) 'vector)
       "message" (format nil "Pushed version ~d: ~d file~:p, ~a~@[; ~d file~:p left out for size~]"
                          (getf result :version) (getf result :files) (bytes-text (getf result :bytes))
                          (let ((skipped (getf result :skipped))) (and skipped (length skipped)))))))))

;;; --- roam_pull ----------------------------------------------------------------------

(defun pull-once (&key apply)
  "Read the store, plan the restore, and with APPLY write it. => a plist."
  (let ((octets (or (store-read)
                    (fail "roam: the store holds no bundle for profile ~a yet: roam_push writes one"
                          (bundle-profile)))))
    (multiple-value-bind (manifest head) (open-bundle (passphrase) octets)
      (let* ((plan (plan-restore manifest))
             (version (or (head-version head) 0))
             (result (and apply (apply-plan plan :reason (format nil "roam_pull version ~a" version)))))
        (when apply
          (write-state :last-version version
                       :last-digest (or (nlk:json-value manifest :string "contentSha256") nil)
                       :pulled-at (nlk:iso-time (nlk:unix-now))))
        (list :plan plan :manifest manifest :head head :version version :result result)))))

(defun plan-rows (plan)
  "PLAN as the answer's rows, capped."
  (coerce (loop for row in (subseq plan 0 (min +max-plan-rows+ (length plan)))
                for action = (getf row :action)
                collect (nlk:json-object
                         "path" (getf row :path)
                         "action" (ecase action (:add "add") (:update "update") (:same "same"))))
          'vector))

(defun tool-pull (args)
  "roam_pull: what a restore would do, and -- with allow_agent_apply on -- the restore."
  (answering
   (running-settings)
   (let* ((apply (true-p (argument args "apply"))))
     (when (and apply (not (setting :allow-agent-apply)))
       (fail "roam: restoring writes files, and allow_agent_apply is off: turn it on in the roam section, or run /roam pull apply yourself"))
     (let ((answer (pull-once :apply apply)))
       (multiple-value-bind (added updated same) (plan-counts (getf answer :plan))
         (nlk:encode-json-object
          (nlk:json-object
           "success" :true
           "applied" (true-json apply)
           "version" (getf answer :version)
           "savedAt" (or (nlk:json-value (getf answer :head) :string "savedAt") :null)
           "added" added
           "updated" updated
           "same" same
           "backup" (or (getf (getf answer :result) :backup) :null)
           "rows" (plan-rows (getf answer :plan))
           "message" (cond ((and apply (getf (getf answer :result) :backup))
                             (format nil "Restored version ~d: ~d added, ~d replaced, ~d already the same; the replaced files are under .roam/backups/~a"
                                     (getf answer :version) added updated same
                                     (getf (getf answer :result) :backup)))
                            (apply
                             (format nil "Version ~d was already what this home holds: ~d file~:p, nothing written"
                                     (getf answer :version) same))
                            (t (format nil "Version ~d would add ~d file~:p and replace ~d; ~d~[~:; more~] already match. applied is false: nothing was written"
                                       (getf answer :version) added updated same same))))))))))

;;; --- the text the operator reads ------------------------------------------------------

(defun bytes-text (bytes)
  "BYTES as a person reads a size."
  (let ((bytes (or bytes 0)))
    (cond ((< bytes 1024) (format nil "~d B" bytes))
          ((< bytes (* 1024 1024)) (format nil "~,1f KB" (/ bytes 1024.0)))
          (t (format nil "~,1f MB" (/ bytes (* 1024.0 1024.0)))))))

(defun status-text ()
  "/roam and /roam status: this home and its store, side by side."
  (let* ((status (status-plist))
         (head (getf status :head))
         (state (getf status :state)))
    (format nil "roam: profile ~a, store ~a (~a)~%  local   ~d file~:p, ~a, ~a~%  store   ~a~%  scope   ~{~a~^, ~} (leaving out ~{~a~^, ~})"
            (bundle-profile) (store-what) (store-backend)
            (length (getf status :entries)) (bytes-text (getf status :bytes))
            (cond ((getf status :changed)
                   (format nil "changed since the last push~@[ (version ~a)~]" (getf state :last-version)))
                  (t (format nil "the same as version ~a" (getf state :last-version))))
            (if head
                (format nil "version ~a, ~a, ~d file~:p, ~a~@[ -- differs from what is here~]"
                        (head-version head)
                        (or (nlk:json-value head :string "savedAt") "no date")
                        (or (nlk:json-value head :integer "files") 0)
                        (bytes-text (nlk:json-value head :integer "bytes"))
                        (getf status :drift))
                (format nil "no bundle for profile ~a yet" (bundle-profile)))
            (scope-include) (scope-exclude))))

(defun push-text (&key force)
  "The line /roam push answers."
  (let ((result (push-once :force force)))
    (format nil "roam: pushed version ~d -- ~d file~:p, ~a to ~a~@[; ~d file~:p left out for size~]"
            (getf result :version) (getf result :files) (bytes-text (getf result :bytes))
            (store-what)
            (let ((skipped (getf result :skipped))) (and skipped (length skipped))))))

(defun diff-text ()
  "The line /roam diff answers: what a restore would do, without doing it."
  (let ((answer (pull-once :apply nil)))
    (multiple-value-bind (added updated same) (plan-counts (getf answer :plan))
      (format nil "roam: version ~a would add ~d file~:p, replace ~d and leave ~d alone~%~{  ~a~^~%~}"
              (getf answer :version) added updated same
              (loop for row in (subseq (getf answer :plan) 0 (min 20 (length (getf answer :plan))))
                    collect (format nil "~a  ~a"
                                    (ecase (getf row :action) (:add "add   ") (:update "replace") (:same "same  "))
                                    (getf row :path)))))))

(defun pull-apply-text ()
  "The line /roam pull apply answers: the restore itself, and its backup."
  (let* ((answer (pull-once :apply t))
         (result (getf answer :result)))
    (format nil "roam: restored version ~a -- ~d file~:p added, ~d replaced, ~d unchanged~@[; what was replaced is in .roam/backups/~a~]"
            (getf answer :version) (getf result :added) (getf result :updated) (getf result :kept)
            (getf result :backup))))

(defun backups-text ()
  "The line /roam backups answers."
  (let ((backups (backup-directories)))
    (if backups
        (format nil "roam: ~d backup~:p in .roam/backups -- ~{~a~^, ~}"
                (length backups) (subseq backups 0 (min 12 (length backups))))
        "roam: no backup yet; one is taken whenever a restore replaces a file")))

(defun restore-text (name)
  "The line /roam restore NAME answers."
  (format nil "roam: backup ~a put ~d file~:p back" name (restore-backup name)))

(defun roam-panel ()
  "The dialog bare /roam answers: the four things an operator does from here."
  (nle:list-dialog
   :title "Roam"
   :summary (format nil "profile ~a, store ~a" (bundle-profile) (store-what))
   :rows (list (nle:run-row "Push this home to the store" "/roam push"
                             :detail "one sealed bundle, this profile")
                (nle:run-row "Show what a restore would change" "/roam diff"
                             :detail "nothing is written")
                (nle:run-row "Restore the store onto this home" "/roam pull apply"
                             :detail "each replaced file is backed up first")
                (nle:run-row "Put an earlier backup back" "/roam backups"
                             :detail "a row for every backup"))))

(defun run-command (args session-id)
  "One /roam invocation: bare, `status`, `push [force]`, `diff`, `pull`
(a preview), `pull apply`, `backups`, `restore NAME`."
  (declare (ignore session-id))
  (running-settings)
  (let* ((typed (nlk:trimmed (or args "")))
         (space (position #\Space typed))
         (head (string-downcase (subseq typed 0 space)))
         (rest (nlk:trimmed (if space (subseq typed (1+ space)) ""))))
    (cond
      ((zerop (length typed)) (values (status-text) (roam-panel)))
      ((member head '("status" "st") :test #'string=) (status-text))
      ((string= head "push")
       (push-text :force (member (string-downcase rest) '("force" "--force" "now") :test #'string=)))
      ((member head '("diff" "preview" "plan") :test #'string=) (diff-text))
      ((string= head "pull")
       (if (member (string-downcase rest) '("apply" "now" "yes") :test #'string=)
           (pull-apply-text)
           (diff-text)))
      ((string= head "backups") (backups-text))
      ((string= head "restore")
       (if (plusp (length rest))
           (restore-text rest)
           (backups-text)))
      (t (fail "roam: ~a is not one of status, push [force], diff, pull, pull apply, backups, restore NAME" typed)))))

;;; --- the manual ------------------------------------------------------------------

(defparameter +primer+
  "Roam keeps a sealed copy of this home -- config.jsonc and the cells you keep, by default --
in a store the operator owns, so a second machine can be restored from it. Three tools and /roam.
  roam_status  what this home holds, what the store holds, and whether they differ.
  roam_push    seal the files in scope and write them to the store, as this profile's next version;
               force replaces a store another machine has moved past what this home last saw.
  roam_pull    with apply=false, the plan: which files would be added, replaced or left alone;
               with apply=true, the restore itself, and only when allow_agent_apply is on.
Every restore copies each file it replaces into .roam/backups/<stamp>/ first, and journals it, so
/roam backups and /roam restore NAME put an earlier state back. The bundle is AES-256-GCM sealed
under a passphrase the section only points at (an environment variable or a file), and its head is
HMAC-signed under a subkey, so a wrong passphrase or an edited store is refused before anything is
written. The store is a folder or one https URL the operator can PUT to. Pushing is the model's to
do; a restore that writes is the operator's, unless they turn allow_agent_apply on."
  "What (help :roam) answers while the cell runs.")

;;; --- the tool schemas ----------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "additionalProperties" :false
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type "description" doc))))
         "required" #())))

(defparameter *status-schema*
  (tool-schema
   "What this home holds in scope -- config.jsonc and the cells it keeps, by default -- what the store holds for this profile, and whether the two differ. The remote half is the bundle's signed head, so a status call needs no passphrase and reads nothing of the contents."
   '())
  "The roam_status tool.")

(defparameter *push-schema*
  (tool-schema
   "Seal the files in scope under the section's passphrase and write them to the store as this profile's next version. Answers the version, the file and byte counts and the content digest. A store another machine has moved past this home's last version is refused unless force is true: pull it here first."
   '(("force" "boolean" "Replace a store this home is behind, instead of refusing.")
     ("note" "string" "One line kept in the bundle's head, saying what this version is.")))
  "The roam_push tool.")

(defparameter *pull-schema*
  (tool-schema
   "Read the store's bundle under the passphrase and answer the plan: which files would be added, which replaced, which already match. With apply true and the section's allow_agent_apply on, it writes them too, copying every replaced file into a backup first. Without apply nothing is written."
   '(("apply" "boolean" "Write the files, not only the plan; needs allow_agent_apply.")))
  "The roam_pull tool.")

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell roam
  (:section ("roam")
    (:guide "store is the folder or https:// URL the bundle lives in; passphrase_env (or passphrase_file) names where the passphrase is, never the passphrase itself; include and exclude say which paths under the home are in scope")
    ("store" :string :required t
     :doc "where the bundle lives: a folder path, or an https:// URL the operator can PUT to")
    ("passphrase_env" :env :default "NODECODE_ROAM_PASSPHRASE"
     :doc "environment variable holding the passphrase the bundle is sealed under")
    ("passphrase_file" :path
     :doc "a file holding the passphrase, when it is not in the environment")
    ("profile" :string :default "default"
     :doc "which bundle in the store: one name per set of machines")
    ("include" :list
     :doc "paths under the home the bundle holds; default config.jsonc and cells/")
    ("exclude" :list
     :doc "prefixes left out of the include; default .cache/ .git/ store/ .roam/ node_modules/")
    ("authorization_env" :env
     :doc "environment variable holding an Authorization header, sent verbatim to an https store")
    ("operation_timeout" :integer :default 60 :min 1
     :doc "seconds one store read or write may take")
    ("allow_agent_apply" :boolean :default nil
     :doc "let roam_pull write files without the operator's own yes")
    ("backup_keep" :integer :default 5 :min 1
     :doc "how many pre-restore backups of this home are kept"))
  (:help :roam "roam: roam_status, roam_push, roam_pull - an encrypted bundle of this home in storage you own; /roam" +primer+)
  (:command "roam" 'run-command
            :catches 'roam-error
            :description "This home's sealed backup: status, push, diff, restore"
            :argument-hint "status · push [force] · diff · pull apply · backups · restore NAME")
  (:tool "roam_status" *status-schema* 'tool-status)
  (:tool "roam_push" *push-schema* 'tool-push)
  (:tool "roam_pull" *pull-schema* 'tool-pull)
  (:start (lambda ()
            ;; Nothing runs in the background: a bundle is written when
            ;; someone asks for one. The state folder is made now, so a
            ;; first push has somewhere to remember itself.
            (ensure-directories-exist (state-file)))))
