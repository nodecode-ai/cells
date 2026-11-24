;;;; cell.lisp --- memory-shield: a policy in front of the memory writes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of churnast/hermes-memory-shield (MIT); see NOTICE.
;;;;
;;;; WHAT IT KEEPS. The agent keeps what it remembers as definitions --
;;;; (define-memory name "one sentence" :type :user) for who the operator is,
;;;; every other type and a prose define-skill for its own notes -- each one a
;;;; file in a knowledge cell. A message in a group chat can talk it into
;;;; rewriting or wiping them. This cell judges every write before it runs and
;;;; answers a refusal instead: the owner profile is added to anywhere, but
;;;; corrected only by the operator in a direct chat and never deleted; the
;;;; agent's own notes may be added to and corrected, never deleted; a channel
;;;; chat is read-only unless its session is listed in trusted_sessions.
;;;;
;;;; WHAT A CALL IS. Nodecode's agent works through one tool, eval, whose form
;;;; carries the definitions it keeps. The hook READS the form as data -- the
;;;; same reader the scribe uses -- and finds the memory writes in it: a
;;;; define-memory or define-skill (an add, or a replace when a definition of
;;;; that name already exists), an (unintern 'name) of one, an (edit path old
;;;; new) or (write-file path text) into a knowledge folder, and a shell
;;;; command that removes or moves one of its files. Only those count: a
;;;; comment naming define-memory, a test string, or a write to any other file
;;;; passes.
;;;;
;;;; UNDO. A knowledge cell is its own git repository and every keep is a
;;;; commit, so the original's snapshot copies are not repeated here: git is
;;;; the undo.
;;;;
;;;; Config, a sibling top-level key beside `channels':
;;;;   "memory-shield": {"profile": "owner_edits", "notes": "no_delete",
;;;;                      "shared": "read_only", "mode": "block",
;;;;                      "trusted_sessions": []}

(defpackage #:nodecode-memory-shield
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting #:policy-text #:knowledge-folders
           #:*folders* #:*source-kind* #:*session* #:*trusted-sessions*))

(in-package #:nodecode-memory-shield)

;;; --- the seams a test holds ------------------------------------------------

(defvar *folders* nil
  "The knowledge folders to judge, or NIL to ask the kernel for them.")

(defvar *source-kind* nil
  "A session's source kind, or NIL to ask the kernel. :CHANNEL is a room.")

(defvar *session* nil
  "The session a call is judged in, or NIL to read the running turn.")

(defvar *trusted-sessions* nil
  "Sessions judged like a direct chat, or NIL to read the setting.")

(defun trusted-sessions ()
  (or *trusted-sessions* (setting :trusted-sessions)))

(defvar *read-package*
  (or (find-package "NODECODE-MEMORY-SHIELD-READ")
      (make-package "NODECODE-MEMORY-SHIELD-READ" :use '()))
  "Where a call's form is read: a name is never interned in the cell's own package.")

;;; --- reading a call ---------------------------------------------------------

(defun read-forms (text)
  "The forms TEXT holds, read and never evaluated. NIL when it cannot be read."
  (let ((*read-eval* nil)
        (*package* *read-package*)
        (stream (make-string-input-stream text)))
    (handler-case
        (loop for form = (read stream nil :eof)
              until (eq form :eof)
              collect form)
      (error () nil))))

(defun head-name (form)
  "The name of the operator FORM starts with, downcased, or NIL."
  (and (consp form)
       (symbolp (car form))
       (string-downcase (symbol-name (car form)))))

(defun unquoted (x)
  (if (and (consp x) (eq (car x) 'quote) (null (cddr x))) (second x) x))

(defun name-of (x)
  "X as a definition name: a symbol or a string, downcased, or NIL."
  (let ((x (unquoted x)))
    (cond ((symbolp x) (string-downcase (symbol-name x)))
          ((stringp x) (string-downcase x)))))

(defun text-of (x strings)
  "The text X names: a literal string, or a ($ \"name\") the strings object
resolves. NIL when X is anything the reader cannot see through."
  (cond ((stringp x) x)
        ((and (consp x) (equal (head-name x) "$")
              (stringp (second x)) strings)
         (gethash (second x) strings))
        (t nil)))

(defun argument-text (arguments)
  "The text a tool call carries: EVAL's form, or every string of another tool."
  (if (hash-table-p arguments)
      (let ((form (gethash "form" arguments)))
        (cond ((stringp form) form)
              (form (format nil "~s" form))
              (t (with-output-to-string (out)
                   (maphash (lambda (key value)
                              (declare (ignore key))
                              (when (stringp value) (format out "~a~%" value)))
                            arguments)))))
      ""))

(defun argument-strings (arguments)
  (and (hash-table-p arguments)
       (let ((strings (gethash "strings" arguments)))
         (and (hash-table-p strings) strings))))

;;; --- the knowledge cells ----------------------------------------------------

(defun knowledge-folders ()
  "The folders the definitions are kept in: the ones configured, else the
kernel's knowledge cells (the layer and cells of code hold no memory)."
  (or *folders*
      (ignore-errors (remove-if-not #'nlk:knowledge-folder-p (nlk:definition-folders)))
      (ignore-errors (list (nlk:knowledge-directory)))))

(defun folder-files (folder)
  (let ((folder (uiop:ensure-directory-pathname folder)))
    (append (ignore-errors (uiop:directory-files folder "*.lisp"))
            (loop for sub in (ignore-errors (uiop:subdirectories folder))
                  append (ignore-errors (uiop:directory-files sub "*.lisp"))))))

(defun knowledge-files ()
  (loop for folder in (knowledge-folders) append (folder-files folder)))

(defun file-definition (path)
  "PATH's first memory or skill, as (:NAME .. :HEAD .. :TYPE ..), or NIL."
  (loop for form in (read-forms (or (ignore-errors (uiop:read-file-string path)) ""))
        for head = (head-name form)
        when (member head '("define-memory" "define-skill") :test #'equal)
          do (return (list :name (name-of (second form))
                           :head head
                           :type (let ((tail (member :type (cddr form))))
                                   (and tail (name-of (second tail))))))))

(defun definition-store (definition)
  (if (and definition (equal (getf definition :type) "user")) :profile :notes))

(defun definition-entry (name)
  "(:PATH .. :STORE ..) for the definition NAME the knowledge cells hold, or NIL."
  (and name
       (loop for path in (knowledge-files)
             for definition = (file-definition path)
             when (and definition (equal (getf definition :name) name))
               do (return (list :path path :store (definition-store definition))))))

(defun knowledge-path-p (text)
  "Whether TEXT names a file inside one of the knowledge folders."
  (let ((path (ignore-errors (uiop:parse-native-namestring text))))
    (and path
         (loop for folder in (knowledge-folders)
               thereis (ignore-errors
                        (uiop:subpathp path (uiop:ensure-directory-pathname folder)))))))

(defun knowledge-path-file (text)
  "The knowledge file TEXT names, or NIL."
  (let ((wanted (ignore-errors (uiop:native-namestring (uiop:parse-native-namestring text)))))
    (and wanted
         (loop for path in (knowledge-files)
               when (equal wanted (ignore-errors (uiop:native-namestring path)))
                 do (return path)))))

(defparameter +stubs+
  '("none" "null" "nil" "empty" "blank" "deleted" "removed" "cleared" "forgotten"
    "tbd" "todo" "obsolete" "outdated" "not relevant")
  "Text that, written over an entry, wipes it as surely as a removal.")

(defun stub-text-p (text)
  (let ((plain (string-downcase (remove-if-not #'alphanumericp (or text "")))))
    (or (< (length plain) 3) (member plain +stubs+ :test #'equal))))

;;; --- what a call writes -----------------------------------------------------

(defparameter +label+ '(:profile "the owner profile" :notes "the agent notes"))

(defun write-op (store action what)
  (list :store store :action action :of what))

(defun form-ops (form strings &optional (ops '()))
  "Every memory write FORM makes, oldest first."
  (cond
    ((not (consp form)) ops)
    (t
     (let ((head (head-name form)))
       (cond
         ;; A keep: an add, or a replace when the name is already kept.
         ((member head '("define-memory" "define-skill") :test #'equal)
          (let* ((name (name-of (second form)))
                 (declared (let ((tail (member :type (cddr form))))
                             (and tail (name-of (second tail)))))
                 (store (if (equal declared "user") :profile :notes))
                 (entry (definition-entry name)))
            (cons (write-op (or (and entry (getf entry :store)) store)
                            (if entry :replace :add)
                            (format nil "~a '~a'" (getf +label+ store) (or name "?")))
                  ops)))
         ;; Forgetting one.
         ((equal head "unintern")
          (let* ((name (name-of (second form)))
                 (entry (definition-entry name)))
            (if entry
                (cons (write-op (getf entry :store) :remove
                                (format nil "~a '~a'" (getf +label+ (getf entry :store)) name))
                      ops)
                ops)))
         ;; A file written or edited inside a knowledge folder.
         ((member head '("edit" "write-file") :test #'equal)
          (let ((path (text-of (second form) strings)))
            (if (and path (knowledge-path-p path))
                (let* ((file (knowledge-path-file path))
                       (store (or (definition-store (and file (file-definition file))) :notes))
                       (new (and (equal head "edit") (text-of (fourth form) strings)))
                       (action (cond (file (if (and new (stub-text-p new)) :remove :replace))
                                     (t :add))))
                  (cons (write-op store action
                                  (format nil "~a '~a'" (getf +label+ store)
                                          (or (and file (pathname-name file)) "?")))
                        ops))
                ops)))
         ;; A shell command that removes or moves one of those files.
         ((equal head "sh")
          (let ((command (text-of (second form) strings)))
            (if command
                (loop for file in (knowledge-files)
                      when (and (search (file-namestring file) command)
                                (some (lambda (word) (search word command))
                                      '("rm " "rm -" "mv " "shred" "truncate" ">" "rmdir")))
                        do (push (write-op (or (definition-store (file-definition file)) :notes)
                                           :remove (format nil "~a '~a'"
                                                           (getf +label+ (or (definition-store
                                                                              (file-definition file))
                                                                             :notes))
                                                           (pathname-name file)))
                                 ops)
                           and return (nreverse ops))
                ops)))
         ;; Anything else: the writes may be inside it.
         (t (loop for sub in (cdr form)
                  do (setf ops (form-ops sub strings ops))
                  finally (return ops))))))))

(defun call-ops (text strings)
  "The memory writes a call makes, deduplicated."
  (remove-duplicates (form-ops (cons 'progn (read-forms text)) strings)
                     :test #'equal :from-end t))

;;; --- the policy -------------------------------------------------------------

(defparameter +levels+ '("off" "no_delete" "owner_edits" "append_only" "read_only"))

(defun level-for (store)
  (let ((value (setting (if (eq store :profile) :profile :notes))))
    (if (member value +levels+ :test #'equal) value "no_delete")))

(defun allowed-actions (level direct)
  "The actions LEVEL allows. OWNER_EDITS is the one that reads the place too:
the operator may correct their own profile, a channel chat may not."
  (cond ((equal level "off") '(:add :replace :remove))
        ((equal level "no_delete") '(:add :replace))
        ((equal level "owner_edits") (if direct '(:add :replace) '(:add)))
        ((equal level "append_only") '(:add))
        (t '())))

(defun current-session ()
  (or *session* (getf (nle:turn) :session-id)))

(defun session-shared-p (session)
  "Whether SESSION is a room: a chat surface, and not one the operator trusts."
  (let ((kind (or *source-kind*
                  (and session (ignore-errors (nlk:session-source-kind session))))))
    (and (equal kind "channel")
         (not (and session (member session (trusted-sessions) :test #'equal))))))

(defun judge-ops (ops session)
  "The operations the policy refuses, each with :WHY."
  (let ((shared (session-shared-p session))
        (denied '()))
    (dolist (op ops (nreverse denied))
      (let ((allowed (allowed-actions (level-for (getf op :store)) (not shared))))
        (when shared
          (setf allowed (intersection allowed
                                       (allowed-actions (setting :shared) t)
                                       :test #'equal)))
        (unless (member (getf op :action) allowed :test #'equal)
          (push (append op (list :why (if shared "shared" "direct"))) denied))))))

(defun refusal-text (denied shared)
  (let ((actions (format nil "~{~a~^, ~}"
                         (remove-duplicates
                          (mapcar (lambda (op) (string-downcase (symbol-name (getf op :action))))
                                  denied)
                          :test #'equal)))
        (whats (format nil "~{~a~^, ~}" (mapcar (lambda (op) (getf op :of)) denied))))
    (format nil "ERROR: refused by memory-shield: '~a' is not allowed for ~a~a; the call did not run -- ~a"
            actions whats (if shared " (in a shared chat)" "")
            (if shared
                "a channel chat is read-only for memory unless its session is listed in trusted_sessions; ask the operator in a direct chat"
                "nobody deletes what the agent keeps, and only the operator corrects the owner profile in a direct chat -- add a new dated entry with the correction instead"))))

(defun judge-call (op next)
  "The :TOOL point: a refused write answers the model, NEXT never runs."
  (let* ((arguments (getf op :arguments))
         (session (current-session))
         (ops (call-ops (argument-text arguments) (argument-strings arguments)))
         (denied (and ops (judge-ops ops session))))
    (cond ((null denied) (funcall next op))
          ((equal (setting :mode) "observe")
           (nle:notice (format nil "memory-shield let this through: ~a"
                               (mapcar (lambda (o) (string-downcase (symbol-name (getf o :action))))
                                       denied))
                       :level :warning :key "nodecode-memory-shield")
           (funcall next op))
          (t (nle:failure (refusal-text denied (session-shared-p session)))))))

;;; --- the slash command ------------------------------------------------------

(defun policy-text (&optional (session (current-session)))
  (format nil "memory-shield policy:~%  owner profile: ~a~%  agent notes: ~a~%  shared chats: ~a~%  on a violation: ~a~%  knowledge folders: ~{~a~^, ~}~%This session: ~a (~a)"
          (setting :profile) (setting :notes) (setting :shared) (setting :mode)
          (or (mapcar #'uiop:native-namestring (knowledge-folders)) (list "none"))
          (or session "none")
          (cond ((session-shared-p session) "a shared chat: read-only for memory")
                ((member session (trusted-sessions) :test #'equal)
                 "trusted: judged like a direct chat")
                (t "a direct chat with the operator"))))

(defun run-command (arguments session)
  (declare (ignore arguments))
  (values (policy-text session) nil))

(defparameter +help+
  "memory-shield judges the agent's memory writes before they run: a define-memory or define-skill, an unintern of one, an edit or write-file into a knowledge folder, a shell command that removes one. The owner profile (memories of :type :user) may be added to anywhere, corrected only by the operator in a direct chat, and never deleted; the agent notes may be added to and corrected, never deleted; a channel chat is read-only unless its session is in trusted_sessions. /memory-shield shows the policy, the folders and this session.")

(nlk:define-peripheral memory-shield :not-running t)

(nle:define-cell memory-shield
  (:section ("memory-shield")
    (:guide "profile and notes are one of off, no_delete, owner_edits, append_only, read_only; shared applies on top in a channel chat; trusted_sessions names sessions judged like a direct chat")
    ("profile" :choice :options '("owner_edits" "append_only" "no_delete" "read_only" "off")
     :default "owner_edits"
     :doc "the owner profile: memories of :type :user")
    ("notes" :choice :options '("no_delete" "owner_edits" "append_only" "read_only" "off")
     :default "no_delete"
     :doc "the agent's own notes: every other memory type and prose skill")
    ("shared" :choice :options '("read_only" "append_only" "no_delete" "off")
     :default "read_only"
     :doc "applied on top of the two above inside a channel chat")
    ("mode" :choice :options '("block" "observe")
     :default "block"
     :doc "block refuses the call; observe lets it run and says so")
    ("trusted_sessions" :list
     :doc "session ids judged like a direct chat, your own channel with the agent")
    ("folders" :list
     :doc "knowledge folders to judge; empty asks the kernel for the knowledge cells"))
  (:hook :tool #'judge-call)
  (:command "memory-shield" #'run-command
            :description "The memory policy, the folders it reads, and this session"
            :argument-hint "")
  (:help "memory-shield"
         "memory-shield: what the agent may add, correct or delete in memory"
         +help+)
  (:start (lambda ()
            (let ((folders (setting :folders)))
              (setf *folders* (and folders (mapcar #'uiop:ensure-directory-pathname folders)))
              (nle:on-stop (lambda () (setf *folders* nil)))))))
