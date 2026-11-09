;;;; cell.lisp --- the section, the five tools, /vault, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes secret source: a local AES-256-SIV store whose
;;;; items are named hv:<item>[:<field>], mapped to environment variables by
;;;; name, with the key held outside the store. This cell is the same store
;;;; with the same spelling and the same refs, reached by a model and by hand
;;;; instead of by the harness at boot: five tools, /vault, (help :vault) and
;;;; the (vault:...) verbs from eval.
;;;;
;;;; The one thing Nodecode cannot do the way Hermes does is put a variable
;;;; into a process that already started, so the mapping is applied at the
;;;; one place it is still early enough: vault_run runs a shell command with
;;;; the section's mapped variables in *that* command's environment, and the
;;;; secret never rides the transcript.

(in-package #:nodecode-vault)

;;; --- the manual ------------------------------------------------------------------

(defparameter +primer+
  "Vault is the operator's local secret store: one file, sealed as a whole
with AES-256-GCM under a 32-byte key the file does not carry. Items are named
by refs, hv:<item>[:<field>], the spelling the original uses.

(vault:secret \"hv:openai:key\")    one value.
(vault:put \"openai\" \"key\" \"...\")  store one field.
(vault:items)                        every item, values included.
(vault:mapping)                      the section's env map as (NAME . REF).
(vault:run \"curl ...\")              a command with those variables in its environment.
The model has the same as tools: vault_list, vault_get, vault_put,
vault_remove and vault_run; vault_run is the one to reach for, because the
secret goes into the command's environment and never into the transcript.

/vault setup makes the key file and an empty store once; /vault list, show,
put, rm, env and doctor do the rest. Nothing is created by a read: a missing
key or store is a refusal that names the way back. A refusal is
ERROR: VAULT-ERROR and says what to set."
  "What (help :vault) answers while the cell runs.")

;;; --- the calls the model makes ---------------------------------------------------

(nlk:define-contract list-call
  (:doc "List the items in the operator's local encrypted vault: ids, labels and field names. Values are never shown; use vault_get for one.")
  ("item" :text :doc "only this item, by id"))

(nlk:define-contract get-call
  (:doc "Read one secret from the operator's local encrypted vault, by its hv:<item>[:<field>] reference. Prefer vault_run when a command needs it: that keeps the value out of the transcript.")
  ("ref" :text :required t :doc "the reference, as in hv:openai:key"))

(nlk:define-contract put-call
  (:doc "Store one secret field in the operator's local encrypted vault, creating the item if it is new.")
  ("item" :text :required t :doc "the item id: 1-128 letters, numbers, dots, underscores or hyphens")
  ("field" :text :required t :doc "the field name, e.g. key")
  ("value" :text :required t :doc "the secret itself")
  ("label" :text :doc "a human label for the item"))

(nlk:define-contract remove-call
  (:doc "Delete one item from the operator's local encrypted vault.")
  ("item" :text :required t :doc "the item id"))

(nlk:define-contract run-call
  (:doc "Run a shell command with the vault's mapped variables in its environment, so a secret reaches the command without being printed.")
  ("command" :text :required t :doc "the shell command, as sh would read it"))

;;; --- the five tools ----------------------------------------------------------------

(defun guarded (thunk)
  "THUNK's answer; any refusal becomes the tool answer ERROR: ..., which the
transcript draws as a failed call."
  (handler-case (prog1 (funcall thunk) (setf *last-error* nil))
    (vault-error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: VAULT-ERROR: ~a" condition)))
    (error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: VAULT-ERROR: ~a" condition)))))

(defun describe-item (one)
  "ONE as one line: id, label, and the names of its fields."
  (format nil "~a~@[ (~a)~]: ~{~a~^, ~}"
          (getf one :id)
          (getf one :label)
          (mapcar #'car (getf one :fields))))

(defun list-tool (call)
  (guarded
   (lambda ()
     (let* ((wanted (list-call-item call))
            (all (items)))
       (when (and wanted (plusp (length wanted)))
         (unless (item all wanted) (fail "no vault item ~a" wanted))
         (setf all (list (item all wanted))))
       (if all
           (format nil "~{~a~%~}" (mapcar #'describe-item all))
           "(empty vault)")))))

(defun get-tool (call) (guarded (lambda () (secret (get-call-ref call)))))

(defun put-tool (call)
  (guarded (lambda () (put (put-call-item call) (put-call-field call)
                            (put-call-value call) :label (put-call-label call)))))

(defun remove-tool (call) (guarded (lambda () (forget (remove-call-item call)))))

(defun run-tool (call) (guarded (lambda () (run (run-call-command call)))))

;;; --- /vault -----------------------------------------------------------------------

(defun words (text) (uiop:split-string text :separator '(#\Space #\Tab)))

(defun masked (one field)
  (if (and field (field-value one field)) "(set)" "(empty)"))

(defun vault-doctor ()
  "What /vault doctor prints: the paths, the custody, and the count of items."
  (let* ((path (store-path))
         (items (items)))
    (format nil "store: ~a~%key: ~a~@[~%environment variable; unset here~]~%~a~%items: ~d~@[~%last refusal: ~a~]"
            path
            (if (setting :key-env) (format nil "key_env ~a" (setting :key-env))
                (format nil "key_file ~a" (key-file-path)))
            (and (setting :key-env) (not (uiop:getenv (setting :key-env))))
            (if (probe-file path) "store present" "store not made yet: /vault setup")
            (length items)
            *last-error*)))

(defun run-vault (args session-id)
  "One /vault invocation => its text."
  (declare (ignore session-id))
  (let* ((all (words (nlk:trimmed (or args ""))))
         (verb (first all))
         (rest (rest all)))
    (handler-case
        (cond
          ((null verb) +primer+)
          ((equal verb "setup") (setup))
          ((equal verb "list")
           (let ((items (items)))
             (if items (format nil "~{~a~%~}" (mapcar #'describe-item items)) "(empty vault)")))
          ((equal verb "get") (secret (or (first rest) (fail "/vault get needs a ref"))))
          ((equal verb "show")
           (let* ((ref (or (first rest) (fail "/vault show needs a ref")))
                  (one (item (items) (parse-ref ref))))
             (unless one (fail "no vault item ~a" ref))
             (format nil "~a: ~{~a~^, ~}"
                     (getf one :id)
                     (loop for (field . value) in (getf one :fields)
                           collect (format nil "~a ~a" field (masked one field))))))
          ((equal verb "put")
           (unless (>= (length rest) 3) (fail "/vault put needs ITEM FIELD VALUE"))
           (put (first rest) (second rest) (format nil "~{~a~^ ~}" (cddr rest))))
          ((equal verb "rm") (forget (or (first rest) (fail "/vault rm needs an item"))))
          ((equal verb "env")
           (let ((id (or (first rest) (fail "/vault env needs ITEM ENV-NAME [FIELD]")))
                 (name (second rest)))
             (check-id id) (check-id name :what "environment variable name")
             (format nil "vault.env:~%  - ~s"
                     (format nil "~a=hv:~a~@[:~a~]" name id (third rest)))))
          ((equal verb "doctor") (vault-doctor))
          (t (fail "unknown /vault verb ~a: list, show, get, put, rm, env, run, setup, doctor" verb)))
      (vault-error (condition) (format nil "vault: ~a" condition)))))

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell vault
  (:section ("vault")
    (:guide "run /vault setup once to make the key and the store; then set key_file (or key_env) and map variables with env")
    ("store" :path :doc "the encrypted store file; default vault/agent-vault.sexp under the home")
    ("key_file" :path :doc "the file holding the key as 64 hex characters; default vault/master.key")
    ("key_env" :env :doc "an environment variable holding the key as 64 hex characters; outranks key_file")
    ("env" :list :doc "NAME=hv:<item>[:<field>] pairs vault_run resolves into a command's environment")
    ("timeout_seconds" :integer :default 60 :min 1 :doc "one vault_run's ceiling"))
  (:tool "vault_list" (nlk:contract 'list-call) #'list-tool)
  (:tool "vault_get" (nlk:contract 'get-call) #'get-tool)
  (:tool "vault_put" (nlk:contract 'put-call) #'put-tool)
  (:tool "vault_remove" (nlk:contract 'remove-call) #'remove-tool)
  (:tool "vault_run" (nlk:contract 'run-call) #'run-tool)
  (:help :vault "the local encrypted secret store: refs, mapping and vault_run" +primer+)
  (:command "vault" #'run-vault
            :description "The operator's local encrypted secret store"
            :argument-hint "list | show REF | get REF | put ITEM FIELD VALUE | rm ITEM | env ITEM NAME | run CMD | setup | doctor"
            :complete (lambda (text session-id)
                        (declare (ignore text session-id))
                        (mapcar (lambda (verb) (list :name verb :value verb))
                                '("list" "show" "get" "put" "rm" "env" "run" "setup" "doctor")))))
