;;;; cell.lisp --- the map, the three tools, /wincred, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes secret source: at startup it read the generic
;;;; credentials its map named out of the current user's Windows Credential
;;;; Manager and handed the values to the orchestrator, which put them in the
;;;; process environment so provider keys never sat in plaintext .env. Nodecode
;;;; has no secret-source seam, so the same read is where a session needs it:
;;;; the map is applied at start (and restored at stop), `wincred_apply' does
;;;; it again on demand, `wincred_status' says what the vault holds without
;;;; reading a value out, and `wincred_read' answers one. Read-only: nothing
;;;; here creates, updates or deletes a credential.

(in-package #:nodecode-wincred)

(nlk:define-peripheral wincred :not-running t)

(defparameter +primer+
  "The Windows Credential Manager is reachable through the nodecode-wincred cell: the current
user's own vault, read through advapi32's CredReadW and nothing else. Nothing is enumerated,
nothing is written back, no process runs and no network is reached.
  wincred_status  the map, and for each entry whether the credential is there, how many bytes
                  it is, and whether this process already has the variable set. Reads no value
                  out: start here.
  wincred_read    one credential's value: an environment variable the map names, or a generic
                  credential's target such as hermes/openai or wincred://hermes/openai. The
                  answer is the value alone. It is a secret: put it where the operator asked
                  for it and do not echo it into a chat channel, a commit or a file they did
                  not name.
  wincred_apply   read every mapped credential and set it as an environment variable of this
                  process, so a command started afterwards sees it, the way the original
                  plugin did at startup. Names and byte counts come back, never a value.
The section's map is a list of ENV_VAR=credential-target entries; the target is the NAME of a
credential of type \"generic credential\" in Credential Manager (Control Panel, Credential
Manager, Windows Credentials, Add a generic credential), which is where the operator stores
the key: the value never lives in this folder, the config or a .env file. A value may be UTF-8
or UTF-16 (the Credential UI and PowerShell write UTF-16); one that decodes to neither is
skipped with a warning rather than injected as a mangled key. Credential Manager is
user-scoped: this raises the at-rest bar for backups, cloud sync and dotfiles, and is no
boundary against code already running as the same user. Windows only: elsewhere every read
says so and nothing is applied."
  "What (help :wincred) answers while the cell runs: the original's README, in the cell's
voice.")

;;; --- the map ------------------------------------------------------------------

(defun no-arguments (description)
  "The schema a tool that takes no arguments registers."
  (list :description description
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:json-object))))

(nlk:define-contract credential-read
  (:doc "One credential of the Windows Credential Manager, read-only. The answer is the value alone. It is a secret: put it where the operator asked for it and do not echo it further.")
  ("name" :text :required t
          :doc "an environment variable the wincred map names, such as OPENAI_API_KEY, or a generic credential's target, such as hermes/openai or wincred://hermes/openai"))

(defun trim (text)
  "TEXT without the blank either end, or \"\" for NIL."
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun clip (text)
  "TEXT cut to the section's max_chars, with the cut said."
  (let ((limit (setting :max-chars)))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit)
                     (format nil "~%[... ~a of ~a characters]" limit (length text)))
        text)))

(defun environment-name-p (name)
  "Whether NAME is a name an environment variable could have: a letter or an
underscore, then letters, digits and underscores."
  (and (stringp name)
       (plusp (length name))
       (let ((first (char name 0)))
         (or (and (char>= first #\A) (char<= first #\Z))
             (and (char>= first #\a) (char<= first #\z))
             (char= first #\_)))
       (every (lambda (character)
                (or (char= character #\_)
                    (and (char>= character #\0) (char<= character #\9))
                    (and (char>= character #\A) (char<= character #\Z))
                    (and (char>= character #\a) (char<= character #\z))))
              name)))

(defun credential-target (reference)
  "REFERENCE -- a credential target, bare or with the original's wincred://
scheme -- as the target name, or NIL when it names none."
  (let* ((text (trim reference))
         (prefix "wincred://"))
    (when (and (>= (length text) (length prefix))
               (string-equal prefix (subseq text 0 (length prefix))))
      (setf text (trim (subseq text (length prefix)))))
    (when (plusp (length text)) text)))

(defun parse-mapping (entries)
  "ENTRIES, the section's map, as the original's ENV_VAR -> target mapping
=> (values ALIST WARNINGS), ALIST in the order written. A malformed entry is
skipped with a warning, never raised: the original never raised on a
malformed map either."
  (let ((pairs '())
        (warnings '()))
    (dolist (entry entries)
      (let* ((text (trim entry))
             (equals (position #\= text)))
        (cond
          ((null equals)
           (push (format nil "skipping ~s: a map entry is ENV_VAR=credential-target" text) warnings))
          (t
           (let ((name (trim (subseq text 0 equals)))
                 (target (credential-target (subseq text (1+ equals)))))
             (cond
               ((not (environment-name-p name))
                (push (format nil "skipping ~s: ~s is not an environment variable's name" text name)
                      warnings))
               ((null target)
                (push (format nil "skipping ~s: the credential target is empty" text) warnings))
               ((assoc name pairs :test #'string=)
                (push (format nil "~a is mapped twice; the first target is the one used" name)
                      warnings))
               (t (push (cons name target) pairs))))))))
    (values (nreverse pairs) (nreverse warnings))))

(defun mapping ()
  "The section's map, parsed."
  (parse-mapping (setting :map)))

(defun credential-value (target)
  "TARGET's value from the vault, decoded, or a WINCRED-ERROR saying why not."
  (unless (vault-available-p)
    (error 'wincred-error :target target
                           :detail "Windows Credential Manager is only available on Windows, and this is not Windows"))
  (handler-case (decode-credential (vault-read target))
    (credential-decode-error ()
      (error 'credential-decode-error :target target))))

;;; --- wincred_status ---------------------------------------------------------------

(defun describe-entry (name target)
  "One map entry as a line: the credential behind it and whether this process
already has the variable -- never the value."
  (handler-case
      (let ((value (credential-value target)))
        (format nil "~a <- ~a: ~a~@[; this process already has ~a set~]~%"
                name target
                (if (plusp (length value))
                    (format nil "found (~a byte~:p)" (length value))
                    "empty")
                (and (environment-value name) name)))
    (wincred-error (condition)
      (format nil "~a <- ~a: ~a~%" name target (wincred-error-detail condition)))))

(defun status-text ()
  "What the vault is and what the map names: names, sizes and states, never a
value."
  (multiple-value-bind (pairs warnings) (mapping)
    (let ((available (vault-available-p)))
      (with-output-to-string (answer)
        (format answer "vault: ~a~%"
                (if available
                    "Windows Credential Manager, the current user's generic credentials (read-only)"
                    "unavailable: this is not Windows, so no credential can be read here"))
        (if (null pairs)
            (format answer "map: empty; set map in the wincred section to ENV_VAR=credential-target entries~%")
            (format answer "map: ~a entr~:@p~%~{~a~}" (length pairs)
                    (mapcar (if available
                                (lambda (pair) (describe-entry (car pair) (cdr pair)))
                                (lambda (pair)
                                  (format nil "~a <- ~a: not read here~%" (car pair) (cdr pair))))
                            pairs)))
        (dolist (warning warnings)
          (format answer "warning: ~a~%" warning))
        (when available
          (format answer "nothing is written to the vault; a value is read only when a tool or the start asks for it~%"))))))

(defun status (arguments)
  "wincred_status: the map and the state of every credential it names."
  (declare (ignore arguments))
  (running-settings)
  (clip (status-text)))

;;; --- wincred_read ----------------------------------------------------------------

(defun read-one (report)
  "wincred_read: one credential's value, the value alone."
  (running-settings)
  (let* ((name (trim (credential-read-name report)))
         (target (or (cdr (assoc name (mapping) :test #'string=))
                     (credential-target name))))
    (unless target
      (error 'wincred-error :detail
                             (format nil "~s names no credential: map it in the wincred section, or pass a target such as hermes/openai"
                                     name)))
    (let ((value (credential-value target)))
      (when (zerop (length value))
        (error 'wincred-error :target target
                               :detail (format nil "the generic credential ~s is empty" target)))
      value)))

;;; --- wincred_apply ---------------------------------------------------------------

(defun apply-mapping ()
  "Read every mapped credential and set it in this process's environment, the
way the original's orchestrator did => (values LINES CHANGES FAILURES).
CHANGES is ((NAME . PREVIOUS-VALUE) ...), what a stop puts back."
  (multiple-value-bind (pairs warnings) (mapping)
    (declare (ignore warnings))
    (let ((lines '())
          (changes '())
          (failures '()))
      (dolist (pair pairs)
        (let ((name (car pair))
              (target (cdr pair)))
          (handler-case
              (let ((value (credential-value target)))
                (cond
                  ((zerop (length value))
                   (push (format nil "~a: the credential ~s is empty" name target) failures))
                  ((and (environment-value name) (not (setting :override-existing)))
                   (push (format nil "~a: left alone, this process already has it set" name) lines))
                  (t
                   (let ((previous (environment-value name)))
                     (set-environment-variable name value)
                     (unless (assoc name changes :test #'string=)
                       (push (cons name previous) changes))
                     (push (format nil "~a: set from ~a (~a byte~:p), not shown"
                                   name target (length value))
                           lines)))))
            (wincred-error (condition)
              (push (format nil "~a: ~a" name (wincred-error-detail condition)) failures)))))
      (values (nreverse lines) (nreverse changes) (nreverse failures)))))

(defun apply-tool (arguments)
  "wincred_apply: the map, applied to this process's environment."
  (declare (ignore arguments))
  (running-settings)
  (if (not (vault-available-p))
      "Windows Credential Manager is only available on Windows; nothing was applied here"
      (multiple-value-bind (lines changes failures) (apply-mapping)
        (declare (ignore changes))
        (if (and (null lines) (null failures))
            "map: empty; set map in the wincred section to ENV_VAR=credential-target entries"
            (clip (with-output-to-string (answer)
                    (format answer "applied to this process's environment, values not shown:~%")
                    (dolist (line lines) (format answer "~a~%" line))
                    (dolist (line failures) (format answer "failed: ~a~%" line))))))))

;;; --- /wincred -----------------------------------------------------------------------

(defun run-command (args session-id)
  "The /wincred command: no argument is the status, `apply' applies the map,
and an environment variable's name or a credential's target describes that
one."
  (declare (ignore session-id))
  (running-settings)
  (let ((typed (trim (or args ""))))
    (cond
      ((zerop (length typed)) (clip (status-text)))
      ((string-equal typed "apply") (apply-tool nil))
      (t
       (multiple-value-bind (pairs) (mapping)
         (let ((pair (assoc typed pairs :test #'string-equal)))
           (cond
             (pair (describe-entry (car pair) (cdr pair)))
             ((credential-target typed)
              (describe-entry typed (credential-target typed)))
             (t (format nil "~s is not in the map; it holds ~{~a~^, ~}"
                        typed (mapcar #'car pairs))))))))))

;;; --- the start ------------------------------------------------------------------

(defun restore (change)
  "Put back what one change replaced: the value it had, or nothing."
  (if (cdr change)
      (set-environment-variable (car change) (cdr change))
      (unset-environment-variable (car change))))

(defun start ()
  "Apply the map at boot, the way the original's secret source did, and say on
the board what is missing. The stop puts back exactly the variables the start
touched and clears the notice."
  (multiple-value-bind (pairs warnings) (mapping)
    (let ((problems '()))
      (when (null pairs)
        (push "wincred: map is empty; set map in the wincred section to ENV_VAR=credential-target entries"
              problems))
      (when (and pairs (not (vault-available-p)))
        (push "wincred: Windows Credential Manager is only available on Windows; nothing was applied"
              problems))
      (when (and pairs (vault-available-p))
        (multiple-value-bind (lines changes failures) (apply-mapping)
          (declare (ignore lines))
          (when changes
            (nle:on-stop (lambda ()
                           (dolist (change changes) (restore change)))))
          (dolist (failure failures)
            (push (format nil "wincred: ~a" failure) problems))))
      (dolist (warning warnings)
        (push (format nil "wincred: ~a" warning) problems))
      (when problems
        (nle:notice (format nil "~{~a~^; ~}" (nreverse problems))
                    :level :warning :key "nodecode-wincred")
        (nle:on-stop (lambda () (nle:notice nil :key "nodecode-wincred")))))))

(nle:define-cell wincred
  (:section ("wincred")
    (:guide "store each key in Windows Credential Manager as a generic credential (Credential Manager, Windows Credentials, Add a generic credential) and map an environment variable to its target here; reads are read-only and nothing else is needed")
    ("map" :list :default '()
           :doc "ENV_VAR=credential-target entries, for example \"OPENAI_API_KEY=hermes/openai\"; the target may also be written wincred://hermes/openai")
    ("override_existing" :boolean :default nil
                          :doc "replace an environment variable this process already has; off, the value already there wins, as .env does")
    ("max_chars" :integer :default 20000 :min 200
                  :doc "how much of one answer the model is given"))
  (:tool "wincred_status"
         (no-arguments "The wincred map, and what each credential it names holds: whether it is in this user's Windows Credential Manager, how many bytes, and whether this process already has the variable set. Reads no value out. Start here.")
         'status)
  (:tool "wincred_read" (nlk:contract 'credential-read) 'read-one)
  (:tool "wincred_apply"
         (no-arguments "Read every mapped credential and set it as an environment variable of this process, the way the original plugin did at startup, so a command started afterwards sees it. Names and byte counts are answered, never a value.")
         'apply-tool)
  (:help :wincred "wincred: Windows Credential Manager secrets, read-only" +primer+)
  (:command "wincred" 'run-command
            :description "What the Windows Credential Manager holds; `apply' applies the map"
            :argument-hint "apply")
  (:start #'start))
