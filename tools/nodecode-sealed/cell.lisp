;;;; cell.lisp --- the section, the four tools, /sealed, the manual, the load.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes secret source: one AES-256-GCM store of
;;;; environment variables, unlocked by a 32-byte key file at startup and
;;;; handed to the process as its secrets. This cell is the same store, in the
;;;; same format, reached three ways: its variables are put into this
;;;; process's environment at every start (so any lane, tool or subprocess
;;;; that reads a variable finds them), the model has four tools to keep it,
;;;; and a person has /sealed and (help :sealed).
;;;;
;;;; What the original does at boot in its runtime, a Nodecode cell does in
;;;; its :start clause: the store is read once, its variables exported, and
;;;; the stop clause puts every one of them back. A value never rides an
;;;; answer: /sealed list and sealed_list answer names and lengths only.

(in-package #:nodecode-sealed)

;;; --- the manual ----------------------------------------------------------------------

(defparameter +primer+
  "Sealed is an encrypted store of environment variables: one file of
AES-256-GCM ciphertext, opened by a 32-byte key file of its own, both 0600.
Its variables are put into this process's environment at every start, so a
provider, a cell or a command that names one finds it and the plaintext lives
in no .env.

/sealed init                 make the key file and an empty store, once.
/sealed set NAME VALUE      store one variable (the value is in this line).
/sealed unset NAME          remove one.
/sealed list                names and how many characters each holds, never a value.
/sealed value NAME          one value, in clear.
/sealed status              both paths, their modes, the count, what is loaded.
/sealed rotate              re-encrypt under a new key, the old one kept as .bak.
/sealed load | unload       read the store into this environment now, or put it back.

From eval: (sealed:names), (sealed:value \"NAME\"), (sealed:set-value \"NAME\" \"...\"),
(sealed:status), (sealed:load-secrets). The model has the same as tools:
sealed_status, sealed_list, sealed_set and sealed_unset.

The format is the original's: a store file opens here and one written here
opens there. A refusal is ERROR: SEALED-ERROR and names the way back."
  "What (help :sealed) answers while the cell runs.")

;;; --- the calls the model makes --------------------------------------------------------

(nlk:define-contract status-call
  (:doc "The sealed store's state: the store and key paths, their file modes, how many variables it holds and which are loaded into this process's environment. Never a value. Call it when a key a tool needs is missing, or when the user asks what is in the store.")
  ("open" :boolean :default t :slot store-open
          :doc "open the store and count its entries; false reports the paths and modes alone, without the key"))

(nlk:define-contract list-call
  (:doc "The names of the variables in the sealed store, with how many characters each value holds. Values are never shown: to use one, name the variable to the tool that needs it, because the store's variables are already in this process's environment.")
  ("prefix" :text :default ""
            :doc "only names starting with this string, e.g. OPENAI_ to see one provider's"))

(nlk:define-contract set-call
  (:doc "Store one variable in the operator's encrypted store. It is encrypted at rest and loaded into the environment at every start; the value is in this call, so say so rather than repeating it.")
  ("name" :text :required t
          :doc "the variable name: letters, numbers and underscores, not starting with a number")
  ("value" :text :required t
           :doc "the secret itself"))

(nlk:define-contract unset-call
  (:doc "Remove one variable from the operator's encrypted store.")
  ("name" :text :required t :doc "the variable name to remove"))

;;; --- the four tools -------------------------------------------------------------------

(defun guarded (thunk)
  "THUNK's answer; a refusal becomes the tool answer ERROR: SEALED-ERROR: ...,
which the transcript draws as a failed call."
  (handler-case (funcall thunk)
    (sealed-error (condition) (nle:failure (format nil "ERROR: SEALED-ERROR: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: SEALED-ERROR: ~a" condition)))))

(defun list-text (&key (prefix ""))
  "Each variable's name and the length of its value, one a line; never a value."
  (let* ((values (read-values))
         (wanted (if (plusp (length prefix))
                     (remove-if-not (lambda (pair) (uiop:string-prefix-p prefix (car pair))) values)
                     values)))
    (if wanted
        (with-output-to-string (out)
          (dolist (pair (sort (copy-list wanted) #'string< :key #'car))
            (format out "~a  (~d chars)~%" (car pair) (length (cdr pair)))))
        (if (plusp (length prefix))
            (format nil "no variable starts with ~a" prefix)
            "(the store is empty)"))))

(defun status-tool (call)
  (guarded (lambda () (status :open (status-call-store-open call)))))

(defun list-tool (call)
  (guarded (lambda () (list-text :prefix (or (list-call-prefix call) "")))))

(defun set-tool (call)
  (guarded (lambda () (set-value (set-call-name call) (set-call-value call)))))

(defun unset-tool (call)
  (guarded (lambda () (unset (unset-call-name call)))))

;;; --- /sealed --------------------------------------------------------------------------

(defun words (text)
  "TEXT split on whitespace, empty pieces dropped."
  (remove-if #'zerop (uiop:split-string text :separator '(#\Space #\Tab)) :key #'length))

(defun run-sealed (args session-id)
  "One /sealed invocation => its text."
  (declare (ignore session-id))
  (let* ((all (words (nlk:trimmed (or args ""))))
         (verb (first all))
         (rest (rest all)))
    (handler-case
        (cond
          ((null verb) +primer+)
          ((equal verb "init")
           (init :force (member (first rest) '("force" "--force" "-f") :test #'equal)))
          ((equal verb "set")
           (unless (>= (length rest) 2) (fail "/sealed set needs NAME VALUE"))
           (set-value (first rest) (format nil "~{~a~^ ~}" (rest rest))))
          ((equal verb "unset") (unset (or (first rest) (fail "/sealed unset needs NAME"))))
          ((equal verb "list") (list-text))
          ((equal verb "value") (value (or (first rest) (fail "/sealed value needs NAME"))))
          ((equal verb "status") (status))
          ((equal verb "rotate") (rotate))
          ((equal verb "load")
           (multiple-value-bind (names warnings) (load-secrets)
             (format nil "loaded ~d variables into this environment~@[; ~{~a~^; ~}~]"
                     (length names) warnings)))
          ((equal verb "unload")
           (format nil "put ~d variables back" (unload-secrets)))
          (t (fail "unknown /sealed verb ~a: init, set, unset, list, value, status, rotate, load, unload"
                   verb)))
      (sealed-error (condition) (format nil "sealed: ~a" condition)))))

;;; --- the load at start ------------------------------------------------------------------

(defun load-and-say ()
  "Put the store's variables into this process's environment, and say in one
standing notice what happened: the count, or the refusal that names the way
back. A store nobody made yet is not an error — the tools still answer."
  (handler-case
      (multiple-value-bind (names warnings) (load-secrets)
        (cond
          (warnings
           (nle:notice (format nil "sealed: ~{~a~^; ~}" warnings)
                       :level :warning :key "nodecode-sealed"))
          (names
           (nle:notice (format nil "sealed: ~d variables loaded from the encrypted store"
                               (length names))
                       :key "nodecode-sealed"))
          (t
           (nle:notice "sealed: the store is empty; /sealed set NAME VALUE adds one"
                       :key "nodecode-sealed"))))
    (sealed-error (condition)
      (nle:notice (format nil "sealed: ~a" condition) :level :warning :key "nodecode-sealed"))))

;;; --- the declaration --------------------------------------------------------------------

(nle:define-cell sealed
  (:section ("sealed")
    (:guide "run /sealed init once to make the key and the store; then /sealed set NAME VALUE for each variable you keep, and every start loads them")
    ("store" :path
             :doc "the encrypted store file; default sealed/sealed.store under the home")
    ("key_file" :path
                :doc "the file holding the raw 32-byte key, 0600; default sealed/sealed.key")
    ("strict_permissions" :boolean :default nil
                          :doc "refuse to load when the store or the key is readable by other users")
    ("override_existing" :boolean :default t
                         :doc "a stored variable overwrites one this process already has; false leaves the shell's value standing"))
  (:tool "sealed_status" (nlk:contract 'status-call) #'status-tool)
  (:tool "sealed_list" (nlk:contract 'list-call) #'list-tool)
  (:tool "sealed_set" (nlk:contract 'set-call) #'set-tool)
  (:tool "sealed_unset" (nlk:contract 'unset-call) #'unset-tool)
  (:help :sealed "the encrypted store of environment variables: init, set, list, rotate, and the load at start" +primer+)
  (:command "sealed" #'run-sealed
            :description "The encrypted store of environment variables"
            :argument-hint "init | set NAME VALUE | unset NAME | list | value NAME | status | rotate | load | unload"
            :complete (lambda (text session-id)
                        (declare (ignore text session-id))
                        (mapcar (lambda (verb) (list :name verb :value verb))
                                '("init" "set" "unset" "list" "value" "status" "rotate"
                                  "load" "unload"))))
  (:start (lambda ()
            (load-and-say)
            (nle:on-stop (lambda ()
                           (unload-secrets)
                           (nle:notice nil :key "nodecode-sealed"))))))
