;;;; cell.lisp --- the three tools, /secret, and the advice on every tool call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; WHAT THE ORIGINAL DID, KEPT. A key is stored under the name of the
;;;; environment variable a tool reads, and what the conversation is told about
;;;; it is a fingerprint and a length -- never the value. Three layers come off
;;;; that store, and all three are here:
;;;;   1. the gate: a tool call that reads a credential store (a secret-shaped
;;;;      file, a keychain print, the whole environment) is refused before it
;;;;      runs, and says what it read;
;;;;   2. the scrub: every tool result is read for the stored values and they
;;;;      come back as [secret:NAME redacted], the original's marker; a result
;;;;      the scrub could not finish is withheld rather than shown;
;;;;   3. the reader: (secret "NAME") hands the value to a form that uses it,
;;;;      and $NAME hands it to a shell command, so nothing has to paste it.
;;;;
;;;; WHAT THE PORT CHANGED, and only this: the hidden macOS dialog and the
;;;; keychain became an operator command -- /secret add NAME ENV_OR_FILE,
;;;; typed by the person, so the value never passes through the model side of
;;;; the conversation either; the per-secret scope (all / default-only for
;;;; messaging bots) is dropped, because a Nodecode store is one file in one
;;;; home and every lane of that home is the same person; and the pasted-key
;;;; check on the operator's own message has no seam here, so the model is
;;;; asked to run secret_check on a draft instead.

(in-package #:nodecode-secret-guard)

(defparameter +manual+
  "A key is stored under the name a tool reads it by, and the conversation only
ever hears a fingerprint and a length. Three layers come off the store:
  the gate     a tool call that reads a credential store is refused before it
               runs: a secret-shaped file (.env, *credential*, *token*,
               *.pem, id_rsa, auth.json), a keychain print, a password
               manager, or the whole environment dumped (printenv, env).
  the scrub    every tool result comes back with each stored value written
               [secret:NAME redacted]. Use secret_check on a draft before it
               is shared.
  the reader   (secret \"NAME\") hands the value to a form that uses it, and
               $NAME to a shell command, so it never has to be pasted.
The tools:
  secret_request  get a key stored without it passing through this chat; the
                  person adds it with /secret add NAME ENV_OR_FILE
  secret_list     the names held, their fingerprints, lengths and sources
  secret_check    CLEAR, or LEAKED with the names a draft carries
The command:
  /secret                    what is held, and the panel
  /secret add NAME SOURCE    SOURCE is an environment variable or a file
  /secret remove NAME        forget it, and unset the variable
  /secret verify [NAME]      re-fingerprint it: a changed line means the
                             file was edited or the value was rotated outside
Never ask the person to paste a key into the chat."
  "What (help :secret-guard) answers while the cell runs.")

(defparameter +usage+
  "/secret                 what is held, and the panel
/secret add NAME SOURCE  store the value of environment variable or file SOURCE
/secret remove NAME      forget it
/secret verify [NAME]    re-fingerprint it
/secret list [NAME]      the names held, never a value"
  "What /secret says when it does not know the verb.")

;;; --- the boundaries ---------------------------------------------------------------

(nlk:define-contract secret-list-request
  (:doc "List the API keys and tokens this machine holds: for each, its name, a sha256 fingerprint, its length and where it came from -- never a value. Use it to see which key a task can reach before asking for one.")
  ("name" :string :default "" :doc "one name to show; empty lists every name held"))

(nlk:define-contract secret-request-request
  (:doc "Get an API key or token stored without it ever passing through this conversation. Name the key in UPPER_SNAKE_CASE; the person adds it with /secret add NAME SOURCE, where SOURCE is an environment variable or a file that holds it. Answers the name, a fingerprint and the length, never the value. Use it whenever a key is needed or rotated, and never ask the person to paste a key into chat.")
  ("name" :text :required t
          :doc "the variable name the key is known by, e.g. OPENAI_API_KEY")
  ("why" :text :default "" :doc "one sentence on what the key is for"))

(nlk:define-contract secret-check-request
  (:doc "Check a draft before it is shared: does it carry the value of any stored secret? Answers CLEAR, or LEAKED with each name that matched and the draft with the values scrubbed. Local and offline.")
  ("text" :text :required t :doc "the draft about to be pasted, written or sent"))

;;; --- what the tools answer ---------------------------------------------------------

(defun entry-line (entry)
  (format nil "  ~a  ~a  ~d chars~@[  from ~a~]~@[  added ~a~]"
          (held-secret-name entry) (held-secret-fingerprint entry)
          (held-secret-length entry) (held-secret-source entry)
          (held-secret-created entry)))

(defun held-lines ()
  (let ((names (secret-names)))
    (if names
        (format nil "~d secret~:p held~%~{~a~^~%~}" (length names)
                (mapcar (lambda (name) (entry-line (held name))) names))
        "no secrets held yet")))

(defun list-text (&optional (name ""))
  "The names held, or the one NAME, without a value."
  (let ((name (nlk:trimmed (or name ""))))
    (if (plusp (length name))
        (let ((entry (held name)))
          (if entry
              (format nil "~a~@[~%(~a)~]~%~%The value is not returned: (secret \"~a\") reads ~
                          it in a form, and $~a is the variable a shell command sees while ~
                          export_env is on."
                      (entry-line entry)
                      (and (< (held-secret-length entry) +min-scrub-length+)
                           (format nil "too short to scrub: ~d characters is under ~d, so it is ~
                                        held but left wherever it appears in a result"
                                   (held-secret-length entry) +min-scrub-length+))
                      name name)
              (format nil "no secret named ~a is held; /secret lists the ~d stored. ~
                          /secret add ~a ENV_OR_FILE stores one."
                      name (hash-table-count *secrets*) name)))
        (format nil "~a~%~%A value is never returned: (secret \"NAME\") reads one in a form, ~
                    and $NAME is the variable a shell command sees while export_env is on."
                (held-lines)))))

(defun status-text ()
  "What /secret says with nothing typed: what is held, and how the cell runs."
  (format nil "secret-guard: ~a~%redaction ~:[off~;on~], gate ~:[off~;on~], export_env ~:[off~;on~]~
               ~%~@[exempt: ~{~a~^, ~}~%~]store: ~a"
          (held-lines)
          (setting :redact-tool-results) (setting :block-secret-reads) (setting :export-env)
          (let ((allow (setting :allow)))
            (and allow (plusp (length allow)) (coerce allow 'list)))
          (or *store-path* "not running")))

(defun add-text (name source)
  "Store the value /secret add NAME SOURCE names, and say what was stored --
never the value."
  (let ((name (nlk:trimmed (or name "")))
        (source (nlk:trimmed (or source ""))))
    (cond ((zerop (length name))
           (format nil "which name? /secret add NAME SOURCE, where SOURCE is an environment ~
                        variable or a file that holds the value.~%~%~a" +usage+))
          ((not (cl-ppcre:scan +variable-name+ name))
           (format nil "~a is not a name a variable could have: use letters, digits and ~
                        underscores, not starting with a digit -- the name the tools read it by, ~
                        e.g. OPENAI_API_KEY" name))
          ((zerop (length source))
           (format nil "where is the value of ~a? give an environment variable that holds it ~
                        or a file that does: /secret add ~a ENV_OR_FILE" name name))
          (t
           (multiple-value-bind (value from) (read-source source)
             (cond ((null value)
                    (format nil "~a is neither an environment variable that is set nor a file ~
                                 that exists: nothing stored" source))
                   ((zerop (length value))
                    (format nil "~a is empty: nothing stored" from))
                   (t
                    (multiple-value-bind (entry replaced) (register-secret name value from)
                      (format nil "~@[rotated ~]stored ~a: fingerprint ~a, ~d characters, from ~a.~
                                   ~%~@[The value is shorter than ~d characters, so it is held but ~
                                     not scrubbed from results.~%~]The value is not in this ~
                                   transcript: (secret \"~a\") reads it in a form, and $~a is ~
                                   the variable a shell command sees while export_env is on."
                              (and replaced "rotated: ") name
                              (held-secret-fingerprint entry) (held-secret-length entry) from
                              (and (< (held-secret-length entry) +min-scrub-length+) +min-scrub-length+)
                              name name)))))))))

(defun read-source (source)
  "The value SOURCE names and where it came from: environment first, then a
file, its first line. => (values VALUE-OR-NIL DESCRIPTION)."
  (let ((from-env (uiop:getenv source))
        (file (ignore-errors (nlk:expand-home source))))
    (cond ((and from-env (plusp (length from-env)))
           (values from-env (format nil "environment variable ~a" source)))
          ((and file (probe-file file))
           (let ((text (read-text-file file)))
             (values (and text (nlk:trimmed (first-line text)))
                     (format nil "file ~a" source))))
          (t (values nil source)))))

(defun first-line (text)
  (subseq text 0 (or (position #\Newline text) (length text))))

(defun remove-text (name)
  (let ((name (nlk:trimmed (or name ""))))
    (cond ((zerop (length name)) (format nil "which name? /secret remove NAME"))
          ((forget-secret name)
           (format nil "forgot ~a: it is out of the store, and out of this process's ~
                        environment unless it was already set before the cell started" name))
          (t (format nil "no secret named ~a is held" name)))))

(defun verify-text (&optional (name ""))
  "Re-fingerprint what is held: a mismatch is a value changed outside the cell."
  (let* ((name (nlk:trimmed (or name "")))
         (names (if (plusp (length name)) (list name) (secret-names))))
    (cond ((null names) "no secrets held yet: nothing to verify")
          (t (format nil "~{~a~^~%~}"
                     (mapcar (lambda (one)
                               (let ((entry (held one)))
                                 (if (null entry)
                                     (format nil "  ~a  not held" one)
                                     (multiple-value-bind (ok actual) (verify-entry entry)
                                       (format nil "  ~a  ~:[CHANGED~;ok~]  fingerprint ~a~@[ (stored as ~a)~]"
                                               one ok actual (and (not ok) (held-secret-fingerprint entry)))))))
                             names))))))

(defun check-answer (text)
  "secret_check's answer: CLEAR, or LEAKED with the names that matched and the
text with them scrubbed."
  (let ((carriers '()))
    (dolist (entry (scrub-pairs))
      (when (search (held-secret-value entry) text)
        (push (format nil "~a (~a, ~d chars)" (held-secret-name entry)
                      (held-secret-fingerprint entry) (held-secret-length entry))
              carriers)))
    (if (null carriers)
        "CLEAR: no stored secret's value appears in the text."
        (multiple-value-bind (scrubbed hits) (redact-text text)
          (format nil "LEAKED: ~d occurrence~:p of ~d stored value~:p -- ~{~a~^, ~}.~%~
                       Share this instead:~%~a"
                  hits (length carriers) (nreverse carriers) scrubbed)))))

(defun secret-list-tool (request)
  "The secret_list tool."
  (list-text (secret-list-request-name request)))

(defun secret-check-tool (request)
  "The secret_check tool."
  (check-answer (secret-check-request-text request)))

(defun secret-request-tool (request)
  "The secret_request tool."
  (let* ((name (secret-request-request-name request))
         (why (nlk:trimmed (or (secret-request-request-why request) "")))
         (entry (held name)))
    (if entry
        (format nil "~a is stored: fingerprint ~a, ~d characters~@[, from ~a~]. The value is not ~
                     in this conversation -- (secret \"~a\") reads it inside a form, $~a is the ~
                     variable a shell command sees while export_env is on~@[. It is for ~a~]."
                name (held-secret-fingerprint entry) (held-secret-length entry)
                (held-secret-source entry) name name
                (and (plusp (length why)) why))
        (format nil "~a is not stored yet. Ask the person to add it without pasting it into the ~
                     chat: /secret add ~a ENV_OR_FILE, where SOURCE is the environment variable ~
                     or the file that holds it. Then call this tool again~@[ -- it is for ~a~]."
                name name (and (plusp (length why)) why)))))

;;; --- the command ------------------------------------------------------------------

(defun secret-dialog ()
  "The panel /secret answers with: add, forget, verify."
  (nle:list-dialog
   :title "Secrets"
   :summary (format nil "~d held; a value is never shown" (hash-table-count *secrets*))
   :rows (list (nle:ask-row "Add or rotate a key"
                             "Name, then the variable or file that holds it"
                             "/secret add " :placeholder "OPENAI_API_KEY ~/.keys/groq"
                             :verb "store")
               (nle:ask-row "Forget a key" "Its name" "/secret remove "
                             :placeholder "OPENAI_API_KEY" :verb "forget")
               (nle:ask-row "Verify what is held" "Name, or nothing for every key"
                             "/secret verify " :placeholder "every name" :verb "verify"))))

(defun run-slash (args session-id)
  "/secret with nothing typed answers the state and the panel; add, remove,
verify and list do what they say, and never echo a value."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (words (and (plusp (length text))
                     (uiop:split-string text :separator '(#\Space #\Tab)))))
    (if (null words)
        (values (status-text) (secret-dialog))
        (let ((verb (string-downcase (first words))))
          (cond ((member verb '("add" "set") :test #'string=)
                 (values (add-text (or (second words) "") (or (third words) "")) nil))
                ((member verb '("remove" "rm" "delete" "forget") :test #'string=)
                 (values (remove-text (or (second words) "")) nil))
                ((member verb '("verify" "check") :test #'string=)
                 (values (verify-text (or (second words) "")) nil))
                ((member verb '("list" "ls" "show") :test #'string=)
                 (values (list-text (or (second words) "")) (secret-dialog)))
                ((string= verb "help") (values +manual+ nil))
                (t (values (format nil "~a~%~%~a" +usage+ (status-text)) (secret-dialog))))))))

;;; --- the advice on every tool call -----------------------------------------------------

(defun gate-refusal (reason)
  "The model's words for a call that did not run."
  (format nil "ERROR: refused by nodecode-secret-guard: the call ~a. It did not run -- use a ~
               stored secret instead: /secret lists the names, (secret \"NAME\") reads one inside ~
               a form, and /secret add NAME ENV_OR_FILE stores a new one. A person can exempt a ~
               call with a pattern in the secret-guard section's allow list."
          reason))

(defun guard-call (op next)
  "The :TOOL point: a credential read is refused before NEXT runs, and every
text a call answers comes back scrubbed."
  (let ((text (argument-text (getf op :arguments))))
    (let ((reason (and (setting :block-secret-reads) (gate-reason text (setting :allow)))))
      (cond (reason (nle:failure (gate-refusal reason)))
            (t (let ((answer (funcall next op)))
                 (if (and (setting :redact-tool-results) (stringp answer))
                     (redact-text answer)
                     answer)))))))

;;; --- the cell --------------------------------------------------------------------------

(nlk:define-peripheral secret-guard :not-running t)

(nle:define-cell secret-guard
  (:section ("secret-guard")
    (:guide "nothing is required: keys are kept in one private file under the Nodecode home; add one with /secret add NAME ENV_OR_FILE; allow exempts a call the gate refuses")
    ("store_file" :path
                  :doc "where the keys are kept; default is secret-guard/secrets.json under the Nodecode home, written mode 600")
    ("redact_tool_results" :boolean :default t
                           :doc "scrub every stored value out of every tool result")
    ("block_secret_reads" :boolean :default t
                          :doc "refuse a call that reads a secret-shaped file, prints a keychain or password-manager value, or dumps the environment")
    ("allow" :list
             :doc "regular expressions a call may match to pass the gate anyway, e.g. an unrelated file named *token*")
    ("export_env" :boolean :default t
                  :doc "put each stored key into this process's environment, when the variable is not already set, so a shell command can read $NAME"))
  (:hook :tool #'guard-call)
  (:tool "secret_request" (nlk:contract 'secret-request-request) 'secret-request-tool)
  (:tool "secret_list" (nlk:contract 'secret-list-request) 'secret-list-tool)
  (:tool "secret_check" (nlk:contract 'secret-check-request) 'secret-check-tool)
  (:command "secret" 'run-slash
            :description "Keys kept out of the chat: add, list, verify, forget"
            :argument-hint "[add NAME ENV_OR_FILE | remove NAME | verify [NAME]]")
  (:help :secret-guard
         "secret-guard: keys kept out of the conversation - a fingerprint and a length are all the model sees"
         +manual+)
  (:start (lambda ()
            (let ((path (store-path)))
              (setf *store-path* path
                    *secrets* (load-store path)
                    *exported* '())
              (when (setting :export-env) (export-all))
              (nle:notice (format nil "secret-guard: ~d secret~:p held, redaction ~:[off~;on~], gate ~:[off~;on~]"
                                  (hash-table-count *secrets*)
                                  (setting :redact-tool-results)
                                  (setting :block-secret-reads))
                          :key "nodecode-secret-guard")
              (nle:on-stop (lambda ()
                             (nle:notice nil :key "nodecode-secret-guard")
                             (unexport-all)
                             (setf *secrets* (make-hash-table :test #'equal)
                                   *store-path* nil)))))))
