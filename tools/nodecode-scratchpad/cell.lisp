;;;; cell.lisp --- Scratchpad: per-session draft buffers, edited in place.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of Zek-Takai/hermes-devkit (see NOTICE). The shape of
;;;; the library is the original's: one folder per session under the cell's own
;;;; data root, each buffer a plain file whose extension is its highlighting;
;;;; write, patch in place, read, list, delete, and promote to a real file,
;;;; with the same 2 MB ceiling, the same refusal to patch text that matches
;;;; twice, and the same short placement rule that makes the scratchpad the
;;;; default home for drafts. The seams are Nodecode's: the section is the
;;;; config, the tool is one (:tool ...) clause of NLE:DEFINE-CELL, the
;;;; original's pre_llm_call hook is a :ROUND hook prompt, and its command is
;;;; /scratchpad.

(defpackage #:nodecode-scratchpad
  (:use #:cl)
  (:nicknames #:scratchpad)
  (:export #:scratchpad-error #:scratchpad-error-detail
           #:write-buffer #:patch-buffer #:read-buffer #:list-buffers
           #:delete-buffer #:promote-buffer #:prune
           #:*rule*))

(in-package #:nodecode-scratchpad)

(nlk:define-peripheral scratchpad :not-running t)

(defconstant +max-name+ 128 "A buffer name's length: the original's bound.")
(defconstant +prune-interval+ 3600 "How often a write checks for idle folders.")

(defparameter *rule*
  "[scratchpad] Drafts you show, discuss or iterate on with the user (code, HTML pages, notes) belong in the scratchpad: named buffers for this session, one per draft, edited in place with action=patch instead of saved again as a new file. action=list names them, action=read shows one. Throwaway machinery nobody reads (screenshots, downloads, logs, one-off scripts) can stay where you are working. Project work, or a file the user asks for, is a real file: action=promote writes the buffer there."
  "The per-turn placement rule: the original's rule, with its desktop viewer left out.")

;;; --- names and paths ---------------------------------------------------------

(defun octets (text)
  "How many bytes TEXT takes, as the original's ceiling counts them."
  (length (sb-ext:string-to-octets text :external-format :utf-8)))

(defun line-count (text)
  (+ (count #\Newline text)
     (if (and (plusp (length text)) (not (char= (char text (1- (length text))) #\Newline))) 1 0)))

(defun buffer-name-p (name)
  "Whether NAME is a buffer's: a plain file name, a letter or digit first, then
letters, digits, dot, underscore and dash."
  (and (stringp name)
       (<= 1 (length name) +max-name+)
       (alphanumericp (char name 0))
       (every (lambda (character)
                (or (alphanumericp character) (find character "._-")))
              name)))

(defun safe-folder-p (name)
  "Whether NAME is an id a folder may be named by, as the original's safe-name
check reads it."
  (and (stringp name)
       (<= 1 (length name) 96)
       (every (lambda (character)
                (or (and (< (char-code character) 128) (alphanumericp character))
                    (find character "-_")))
              name)))

(defun root ()
  "The scratchpad's folder under the home: one directory per session."
  (uiop:ensure-directory-pathname (nlk:home "scratchpad/")))

(defun folder-name (session)
  "SESSION's folder name: the id itself when a folder may wear it, else its
hash, so a path-shaped id can never leave the root."
  (if (safe-folder-p session)
      session
      (concatenate 'string "s-" (subseq (nlk:sha256-text session) 7 39))))

(defun session-folder (session &key (create t))
  "SESSION's scratchpad folder, made with its .session marker when CREATE."
  (unless (and (stringp session) (plusp (length session)))
    (fail "no session is bound to this call"))
  (let ((folder (merge-pathnames (format nil "~a/" (folder-name session)) (root))))
    (when (and create (not (uiop:directory-exists-p folder)))
      (ensure-directories-exist folder)
      (nlk:write-file-atomically (merge-pathnames ".session" folder) session :mode #o600))
    folder))

(defun buffer-file (session name &key must-exist)
  "SESSION's buffer NAME, refused when MUST-EXIST and there is none."
  (unless (buffer-name-p name)
    (fail "~s is no buffer name: a plain file name like 'main.py' (letters, digits, . _ -), at most ~d characters"
          name +max-name+))
  (let ((path (merge-pathnames name (session-folder session))))
    (when (and must-exist (not (probe-file path)))
      (fail "no buffer named ~s in this session; action=list names them" name))
    path))

(defun read-text (path)
  (uiop:read-file-string path :external-format :utf-8))

(defun file-bytes (path)
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (file-length in)))

(defun max-bytes ()
  (* 1024 (setting :max-buffer-kb)))

(defun checked-content (text)
  "TEXT as a buffer's content, refused past the ceiling the section sets."
  (unless (stringp text)
    (fail "content must be a string"))
  (when (> (octets text) (max-bytes))
    (fail "a buffer holds at most ~d KB (scratchpad.max_buffer_kb)" (setting :max-buffer-kb)))
  text)

(defun buffer-json (path &key content (created nil created-p))
  "PATH as every action answers it: its name, where it is, its size, and its
content when CONTENT."
  (let* ((text (read-text path))
         (object (nlk:json-object
                  "success" :true
                  "name" (file-namestring path)
                  "path" (namestring path)
                  "bytes" (octets text)
                  "lines" (line-count text)
                  "content" (if content text :null))))
    (when created-p
      (setf (gethash "created" object) (if created t :false)))
    (nlk:encode-json-object object)))

;;; --- who is calling ------------------------------------------------------------

(defun current-session ()
  "The session a call belongs to: the running turn's, or the one whose eval
this is."
  (or (getf (nle:turn) :session-id)
      nlk:*scribe-session-id*
      (fail "no session is bound to this call")))

(defun need-name (name action)
  "NAME, or the refusal an action that needs one answers."
  (or (and name (plusp (length name)) name)
      (fail "~a needs name: the buffer's file name, e.g. main.py" action)))

;;; --- the verbs ------------------------------------------------------------------

(defun write-buffer (name content &optional session)
  "Create or replace the buffer NAME with CONTENT in SESSION's scratchpad.
=> the buffer's JSON."
  (let* ((session (or session (current-session)))
         (path (buffer-file session (need-name name "write")))
         (content (checked-content (or content "")))
         (created (not (probe-file path))))
    (ensure-directories-exist path)
    (nlk:write-file-atomically path content :mode #o600)
    (prune-when-due)
    (buffer-json path :created created)))

(defun replaced (text old new count replace-all)
  "TEXT with OLD replaced by NEW: every match with REPLACE-ALL, else the first."
  (with-output-to-string (out)
    (loop with start = 0
          for at = (search old text :start2 start)
          for done = 0 then (1+ done)
          while (and at (or replace-all (< done 1)))
          do (write-string (subseq text start at) out)
             (write-string new out)
             (setf start (+ at (length old)))
          finally (write-string (subseq text start) out))))

(defun patch-buffer (name old new &key replace-all session)
  "Replace OLD with NEW once in the buffer NAME, or every time with
REPLACE-ALL; text that matches nowhere or twice changes nothing."
  (let* ((session (or session (current-session)))
         (path (buffer-file session (need-name name "patch") :must-exist t))
         (text (read-text path)))
    (unless (and (stringp old) (plusp (length old)))
      (fail "old_text must not be empty"))
    (let ((count (loop with count = 0 with start = 0
                       for at = (search old text :start2 start)
                       while at do (setf start (1+ at)) (incf count)
                       finally (return count))))
      (when (zerop count)
        (fail "old_text not found in ~a; action=read shows the buffer as it is" name))
      (when (and (> count 1) (not replace-all))
        (fail "old_text matches ~d places in ~a; add context or set replace_all" count name))
      (nlk:write-file-atomically path (replaced text old (or new "") count replace-all) :mode #o600)
      (prune-when-due)
      (buffer-json path))))

(defun buffer-files (folder)
  "FOLDER's buffers: its plain files, its own dot files left out."
  (if (uiop:directory-exists-p folder)
      (remove-if (lambda (path) (char= (char (file-namestring path) 0) #\.))
                 (uiop:directory-files folder))
      '()))

(defun read-buffer (name &optional session)
  "The buffer NAME of SESSION's scratchpad, its content and its size."
  (buffer-json (buffer-file (or session (current-session)) (need-name name "read") :must-exist t)
               :content t))

(defun list-buffers (&optional session)
  "This session's buffers, each with its name, size and lines."
  (let ((folder (session-folder (or session (current-session)) :create nil)))
    (nlk:encode-json-object
     (nlk:json-object
      "success" :true
      "path" (namestring folder)
      "buffers" (coerce (loop for path in (buffer-files folder)
                               collect (let ((text (read-text path)))
                                         (nlk:json-object "name" (file-namestring path)
                                                          "bytes" (octets text)
                                                          "lines" (line-count text))))
                         'vector)))))

(defun delete-buffer (name &optional session)
  "Remove the buffer NAME from SESSION's scratchpad."
  (let ((path (buffer-file (or session (current-session)) (need-name name "delete")
                            :must-exist t)))
    (delete-file path)
    (nlk:encode-json-object (nlk:json-object "success" :true "name" name "deleted" :true))))

(defun expanded (path)
  "PATH with a leading ~ read as the home, as the original's promote reads it."
  (if (and (plusp (length path)) (char= (char path 0) #\~))
      (let ((rest (subseq path (if (and (> (length path) 1) (char= (char path 1) #\/)) 2 1))))
        (namestring (merge-pathnames rest (user-homedir-pathname))))
      path))

(defun promote-buffer (name destination &key overwrite session)
  "Write the buffer NAME to the real file DESTINATION, overwriting one already
there only with OVERWRITE."
  (let* ((source (buffer-file (or session (current-session)) (need-name name "promote")
                               :must-exist t))
         (target (and (stringp destination) (plusp (length destination))
                      (expanded destination))))
    (unless target
      (fail "promote needs path: the absolute or ~~ destination of the real file"))
    (unless (uiop:absolute-pathname-p target)
      (fail "promote path must be absolute or start with ~~"))
    (when (uiop:directory-exists-p target)
      (fail "~a is a directory; name the file to write" target))
    (when (and (probe-file target) (not overwrite))
      (fail "~a already exists; set overwrite to replace it" target))
    (let ((text (read-text source)))
      (ensure-directories-exist target)
      (nlk:write-file-atomically target text :mode #o600)
      (nlk:encode-json-object
       (nlk:json-object "success" :true "promoted" name "path" (namestring target)
                         "bytes" (octets text))))))

;;; --- cleanup -------------------------------------------------------------------

(defun session-folders ()
  "The scratchpad folders that carry a .session marker: the ones cleanup owns."
  (let ((root (root)))
    (if (uiop:directory-exists-p root)
        (remove-if-not (lambda (folder) (probe-file (merge-pathnames ".session" folder)))
                       (uiop:subdirectories root))
        '())))

(defun last-active (folder)
  "FOLDER's newest write, as a universal time: its own stamp or any file in it."
  (reduce #'max
          (loop for path in (cons folder (uiop:directory-files folder))
                collect (or (file-write-date path) 0))
          :initial-value 0))

(defun prune (&key idle-days (now (get-universal-time)))
  "Remove the session folders idle longer than IDLE-DAYS, the section's 90 by
default. => the folder names removed."
  (let ((days (or idle-days (setting :idle-days)))
        (removed '()))
    (dolist (folder (session-folders))
      (when (> (- now (last-active folder)) (* days 86400))
        (push (car (last (pathname-directory folder))) removed)
        (uiop:delete-directory-tree folder :validate t :if-does-not-exist :ignore)))
    (nreverse removed)))

(defun prune-when-due ()
  "Prune at most once an hour, from a write: cleanup is best-effort and never
takes a call down with it."
  (handler-case
      (let ((stamp (merge-pathnames ".pruned" (root))))
        (when (or (not (probe-file stamp))
                  (> (- (get-universal-time) (file-write-date stamp)) +prune-interval+))
          (ensure-directories-exist stamp)
          (nlk:write-file-atomically stamp (format nil "~a~%" (get-universal-time)) :mode #o600)
          (prune)))
    (error () nil)))

;;; --- the per-turn rule ---------------------------------------------------------

(defun buffer-names (session)
  (mapcar #'file-namestring (buffer-files (session-folder session :create nil))))

(defun rule-text (session)
  "The placement rule, then the names this session already has."
  (let ((names (handler-case (buffer-names session) (error () '()))))
    (if names
        (format nil "~a Buffers this session: ~{~a~^, ~}." *rule*
                (subseq names 0 (min 20 (length names))))
        *rule*)))

(defun remind (op)
  "At each model request: the rule in the round about to be asked, so a
running turn hears it without stopping."
  (let ((session (getf op :session-id))
        (turn (getf op :turn)))
    (when (and session turn (nlk:store-open-p))
      (nlk:record-turn-context-prompt turn :text (rule-text session)))))

;;; --- the tool ------------------------------------------------------------------

(nlk:define-contract scratchpad-call
  (:doc "Drafts you show, discuss or iterate on with the user: code, HTML pages, notes. Each session has named buffers it edits in place (write once, then patch) instead of creating a new file per draft; they are removed with the session. Not for throwaway machinery nobody reads (screenshots, downloads, logs): that stays where you are working. A real file, only when the work moves into a project or the user asks for one: action=promote.")
  ("action" :text :required t :options '("write" "patch" "read" "list" "delete" "promote")
            :doc "what to do")
  ("name" :string :doc "buffer file name, e.g. main.py or page.html: the extension sets highlighting")
  ("content" :string :doc "write: the whole buffer")
  ("old_text" :string :doc "patch: the exact text to replace")
  ("new_text" :string :doc "patch: what replaces it")
  ("replace_all" :boolean :doc "patch: replace every match")
  ("path" :string :doc "promote: the absolute or ~ destination of the real file")
  ("overwrite" :boolean :doc "promote: replace a file already there"))

(defun tool-answer (function)
  "FUNCTION's answer, or its refusal as the ERROR line the model repairs from."
  (handler-case (funcall function)
    (scratchpad-error (condition) (nle:failure (format nil "ERROR: scratchpad: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: scratchpad: ~a" condition)))))

(defun run-tool (call)
  "The scratchpad tool: one action on this session's buffers."
  (tool-answer
   (lambda ()
     (let ((action (scratchpad-call-action call)))
       (cond ((string= action "write")
              (write-buffer (scratchpad-call-name call) (or (scratchpad-call-content call) "")))
             ((string= action "patch")
              (patch-buffer (scratchpad-call-name call)
                            (or (scratchpad-call-old-text call) "")
                            (or (scratchpad-call-new-text call) "")
                            :replace-all (and (scratchpad-call-replace-all call) t)))
             ((string= action "read")
              (read-buffer (scratchpad-call-name call)))
             ((string= action "list") (list-buffers))
             ((string= action "delete")
              (delete-buffer (scratchpad-call-name call)))
             ((string= action "promote")
              (promote-buffer (scratchpad-call-name call)
                              (scratchpad-call-path call)
                              :overwrite (and (scratchpad-call-overwrite call) t)))
             (t (fail "unknown action ~s: write, patch, read, list, delete or promote" action)))))))

;;; --- /scratchpad and the manual -------------------------------------------------

(defun sized (bytes)
  (cond ((< bytes 1024) (format nil "~d B" bytes))
        ((< bytes (* 1024 1024)) (format nil "~,1f KB" (/ bytes 1024.0)))
        (t (format nil "~,1f MB" (/ bytes (* 1024.0 1024.0))))))

(defun summary (session max-kb idle-days)
  "The session's scratchpad as /scratchpad typed alone answers it."
  (let* ((folder (session-folder session :create nil))
         (files (buffer-files folder))
         (total (reduce #'+ files :key #'file-bytes :initial-value 0)))
    (with-output-to-string (out)
      (format out "scratchpad: ~d buffer~:p, ~a, in ~a; at most ~d KB each, removed after ~d idle day~:p.~%"
              (length files) (sized total) (namestring folder) max-kb idle-days)
      (when files
        (dolist (path files)
          (format out "  ~a, ~a~%" (file-namestring path) (sized (file-bytes path))))
        (format out "/scratchpad read NAME shows one, /scratchpad delete NAME removes one.~%")))))

(defun panel (session)
  "The picker bare /scratchpad carries: this session's buffers, one to open."
  (nle:list-dialog
   :title "Scratchpad" :summary "Drafts of this session"
   :rows (append (loop for path in (buffer-files (session-folder session :create nil))
                       collect (nle:run-row (format nil "~a  ~a" (file-namestring path)
                                                     (sized (file-bytes path)))
                                            (format nil "/scratchpad read ~a" (file-namestring path))
                                            :detail "show it in this answer"))
                 (list (nle:run-row "Remove the idle ones" "/scratchpad prune"
                                    :detail "folders nothing touched in a while")))))

(defun run-command (args session-id)
  "/scratchpad typed alone: this session's buffers and a picker. `read NAME`
shows one, `delete NAME` removes one, `prune` removes idle folders."
  (let ((idle-days (setting :idle-days))
        (max-kb (setting :max-buffer-kb))
        (text (nlk:trimmed (or args ""))))
    (unless (and session-id (plusp (length session-id)))
      (fail "this shell has no session, so there is no scratchpad to show"))
    (cond ((zerop (length text))
           (values (summary session-id max-kb idle-days) (panel session-id)))
          ((string-equal text "prune")
           (let ((removed (prune :idle-days idle-days)))
             (format nil "scratchpad: removed ~d idle folder~:p~@[: ~{~a~^, ~}~]."
                     (length removed) removed)))
          (t
           (let* ((words (nlk:split-words text))
                  (verb (string-downcase (first words)))
                  (name (or (second words) (first words))))
             (cond ((member verb '("read" "show") :test #'string=)
                    (read-buffer name session-id))
                   ((member verb '("delete" "remove") :test #'string=)
                    (delete-buffer name session-id))
                   (t (read-buffer name session-id))))))))

(defparameter +primer+
  "Scratchpad: one managed home for drafts. Every session has named buffers under the home's scratchpad folder, edited in place and removed when the folder sits idle for idle_days. The model uses the scratchpad tool (write, patch, read, list, delete, promote); the operator uses /scratchpad, whose rows open a buffer in the answer. (scratchpad:write-buffer \"main.py\" \"...\") and its siblings are the same verbs from eval: patch-buffer, read-buffer, list-buffers, delete-buffer, promote-buffer, and (scratchpad:prune) removes the idle folders now. A patch whose old_text matches twice is refused rather than guessed, and a buffer is at most max_buffer_kb."
  "What (help :scratchpad) answers while the cell runs.")

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell scratchpad
  (:section ("scratchpad")
    (:guide "nothing here is required; a buffer is a draft this session keeps and idle folders are removed; next step: /scratchpad lists them")
    ("idle_days" :integer :default 90 :min 1
     :doc "remove a session's scratchpad after this many idle days")
    ("max_buffer_kb" :integer :default 2048 :min 1
     :doc "how large one buffer may be, in kilobytes"))
  (:help :scratchpad "scratchpad: per-session draft buffers, edited in place - scratchpad action=write|patch|read|list|delete|promote"
         +primer+)
  (:command "scratchpad" 'run-command
            :description "This session's draft buffers: show, read, delete or prune them"
            :argument-hint "read NAME · delete NAME · prune")
  (:tool "scratchpad" (nlk:contract 'scratchpad-call) 'run-tool)
  (:hook :round (nle:observer #'remind "nodecode-scratchpad")))
