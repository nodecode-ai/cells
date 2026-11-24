;;;; cell.lisp --- the store, the environment, the redaction, /secret-drop.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes plugin: /secret, or the key button beside the
;;;; desktop composer, takes a password in a masked field, writes it into the
;;;; profile's .env, names it in terminal.env_passthrough so the agent's
;;;; terminal carries it, and registers the value for exact-match output
;;;; redaction. The model never sees it, and is told not to ask for a paste.
;;;;
;;;; Nodecode has no profile .env to write into, so this cell makes the same
;;;; bargain one layer down: the values live in a store of its own under the
;;;; home (0600, replaced whole), and at start each one is put into this
;;;; image's environment, which every (sh ...) and the shells under it inherit
;;;; -- that is what env_passthrough was for. A credential-shaped value is
;;;; added to an exact-match redaction on the text that reaches the model, with
;;;; the original's rule for what is safe to search for: a short or plain-word
;;;; value stays out of it, because searching for it would rewrite prose.
;;;;
;;;; WHERE THE TEXT IS CAUGHT. WIRE-MESSAGES-FROM-EVENTS folds durable facts
;;;; into the provider-shaped messages of one turn, and the request is built
;;;; from those. The cell advises that fold, so a value a tool result printed
;;;; goes out as a placeholder while the log keeps what was written.

(defpackage #:nodecode-secret-drop
  (:use #:cl)
  (:export #:redact-text #:store-path #:stored-names))

(in-package #:nodecode-secret-drop)

(nlk:define-peripheral secret-drop :not-running t)

;;; --- constants -------------------------------------------------------------------

(defparameter +min-redaction-chars+ 8
  "A value shorter than this is never used as a global search-and-replace.")

(defparameter +long-redaction-chars+ 24
  "A value this long is specific enough on its own, words or not.")

(defparameter +max-chars+ 8192
  "The longest secret the store takes, as the original bounded it.")

(defparameter +placeholder+ "[SECRET-DROP]"
  "What an exact copy of a stored value becomes on the way to the model.")

(defparameter +hold-back+
  '("OPENAI_API_KEY" "ANTHROPIC_API_KEY" "GEMINI_API_KEY" "GOOGLE_API_KEY"
    "OPENROUTER_API_KEY" "XAI_API_KEY" "MISTRAL_API_KEY")
  "Names, or name prefixes, the original stored but never passed to its
sandboxed commands: this image's own provider credentials are not offered to
every shell command just because someone dropped a value under that name. The
section's hold_back adds to these.")

(defparameter +content-part-types+ '("text" "input_text" "output_text")
  "The content-part types whose `text' member carries words.")

(defvar *secrets* nil
  "The store as an alist of (NAME . VALUE), read at start and kept in step
with the file on every drop and removal. NIL when the cell is not running.")

(defvar *exported* nil
  "The variables this start put into the environment, as (NAME . PRIOR-VALUE):
the prior value is what a stop puts back, NIL meaning the name was unset.")

;;; --- where it lives ----------------------------------------------------------------

(defun store-path ()
  "The store file: the section's store, else secret-drop/store.lisp under the home."
  (let ((named (ignore-errors (setting :store))))
    (if (and (stringp named) (plusp (length named)))
        (pathname named)
        (nlk:home "secret-drop/store.lisp"))))

(defun read-store ()
  "The store file as an alist of (NAME . VALUE), or NIL when it is not there."
  (let ((path (store-path)))
    (handler-case
        (if (probe-file path)
            (with-open-file (in path :direction :input :element-type 'character)
              (let ((*read-eval* nil)
                    (*package* (find-package :keyword)))
                (loop for (name . value) in (read in nil nil)
                      when (and (stringp name) (stringp value))
                        collect (cons name value))))
            nil)
      (error () nil))))

(defun write-store (entries)
  "ENTRIES written to the store file whole, 0600 where the OS says so: a
  half-written store is never read back."
  (let* ((path (store-path))
         (directory (uiop:pathname-directory-pathname path))
         (tmp (merge-pathnames (format nil ".~a.tmp" (file-namestring path)) directory)))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
      (let ((*print-circle* nil)
            (*print-pretty* t))
        (write entries :stream out :escape t)
        (terpri out)))
    (ignore-errors (sb-posix:chmod (namestring tmp) #o600))
    (uiop:rename-file-overwriting-target tmp path)
    (ignore-errors (sb-posix:chmod (namestring path) #o600))
    entries))

(defun stored-names ()
  "The names in the store, newest first, and never a value."
  (mapcar #'car *secrets*))

;;; --- what is accepted ---------------------------------------------------------------

(defun check-name (name)
  "NAME as an environment variable name, uppercased, or a refusal naming it."
  (let ((clean (string-upcase (nlk:trimmed (or name "")))))
    (cond
      ((zerop (length clean))
       (fail "a drop needs a variable name, like DEPLOY_TOKEN"))
      ((> (length clean) 128)
       (fail "the variable name is longer than 128 characters"))
      ((not (or (alpha-char-p (char clean 0)) (char= (char clean 0) #\_)))
       (fail "~a does not start with a letter or an underscore" clean))
      ((loop for ch across clean
             thereis (not (or (alphanumericp ch) (char= ch #\_))))
       (fail "~a holds a character a variable name cannot: letters, digits and _ only" clean))
      (t clean))))

(defun check-value (value)
  "VALUE, trimmed, or a refusal that never repeats it."
  (let ((clean (string-trim '(#\Space #\Tab #\Return #\Newline) (or value ""))))
    (cond
      ((zerop (length clean)) (fail "the secret is empty; nothing was stored"))
      ((> (length clean) +max-chars+)
       (fail "the secret is longer than ~d characters" +max-chars+))
      ((or (find #\Newline clean) (find #\Return clean))
       (fail "a secret cannot carry a line break"))
      ((find (code-char 0) clean) (fail "a secret cannot carry a NUL"))
      (t clean))))

(defun hold-back-p (name)
  "Whether NAME is a variable this cell leaves out of the environment."
  (let ((extra (ignore-errors (setting :hold-back))))
    (loop for item in (append +hold-back+ (and (listp extra) extra))
          thereis (and (stringp item) (plusp (length item))
                       (<= (length item) (length name))
                       (string-equal item name :end2 (length item))))))

;;; --- the environment ----------------------------------------------------------------

(defun export-value (name value)
  "Put VALUE in this image's environment as NAME, remembering what was there.
The child a (sh ...) starts inherits it: that is the whole point of the drop."
  (unless (or (null (ignore-errors (setting :passthrough))) (hold-back-p name))
    (let ((prior (sb-posix:getenv name)))
      (unless (assoc name *exported* :test #'string=)
        (push (cons name prior) *exported*))
      (sb-posix:setenv name value 1)
      t)))

(defun retract-exports ()
  "Put the environment back the way it was before this start."
  (dolist (pair *exported*)
    (ignore-errors
      (if (cdr pair)
          (sb-posix:setenv (car pair) (cdr pair) 1)
          (sb-posix:unsetenv (car pair)))))
  (setf *exported* nil))

(defun exported-p (name)
  (and (assoc name *exported* :test #'string=) t))

;;; --- the redaction ------------------------------------------------------------------

(defun redaction-eligible-p (value)
  "Whether VALUE is safe to install as an exact-match redaction. A short value,
or one made only of letters, would rewrite ordinary text (password,
administrator); digits or symbols mark a credential-shaped value, and a very
long letter string is specific enough on its own. The original's rule."
  (and (stringp value)
       (>= (length value) (or (ignore-errors (setting :min-redaction-chars))
                               +min-redaction-chars+))
       (or (>= (length value) +long-redaction-chars+)
           (loop for ch across value thereis (not (alpha-char-p ch))))))

(defun replace-all (text old new)
  "TEXT with every exact occurrence of OLD replaced by NEW."
  (let ((out (make-string-output-stream))
        (start 0))
    (loop for at = (search old text :start2 start)
          while at
          do (write-string text out :start start :end at)
             (write-string new out)
             (setf start (+ at (length old))))
    (write-string text out :start start)
    (get-output-stream-string out)))

(defun redact-values ()
  "The stored values worth searching for, longest first, so a value that
contains another is replaced before the shorter one splits it."
  (sort (loop for (nil . value) in *secrets*
              when (redaction-eligible-p value) collect value)
        #'> :key #'length))

(defun redact-text (text)
  "TEXT with every exact copy of a credential-shaped stored value replaced.
Prose is left alone, and a value too short or too word-like to search for is
not used at all."
  (if (or (null text) (zerop (length text)) (null *secrets*)
          (null (ignore-errors (setting :redact))))
      text
      (let ((out text))
        (dolist (value (redact-values) out)
          (when (search value out)
            (setf out (replace-all out value +placeholder+)))))))

(defun redact-content (content)
  "CONTENT -- a string, or an array of parts -- with its words redacted."
  (cond
    ((stringp content) (redact-text content))
    ((vectorp content)
     (let ((copy (copy-seq content))
           (changed nil))
       (dotimes (i (length copy))
         (let ((part (aref copy i)))
           (when (hash-table-p part)
             (let ((text (nlk:json-value part :string "text"))
                   (type (nlk:json-value part :string "type")))
               (when (and (stringp text) (stringp type)
                          (member type +content-part-types+ :test #'string=))
                 (let ((scrubbed (redact-text text)))
                   (unless (string= scrubbed text)
                     (setf (aref copy i) (nlk:copy-json-object part "text" scrubbed)
                           changed t))))))))
       (if changed copy content)))
    (t content)))

(defun redact-message (message)
  "MESSAGE, a folded provider message, with its words redacted: a copy when
that changed anything and MESSAGE itself when it did not. The system prompt is
never read."
  (if (not (hash-table-p message))
      message
      (let ((role (nlk:json-value message :string "role")))
        (if (or (and (stringp role) (string-equal role "system"))
                (not (nth-value 1 (gethash "content" message))))
            message
            (let* ((content (gethash "content" message))
                   (scrubbed (redact-content content)))
              (if (eq scrubbed content)
                  message
                  (nlk:copy-json-object message "content" scrubbed)))))))

(defun redact-wire (next &rest args)
  "The advice on the fold: hand the messages back redacted and the fold's
CLEAN-P untouched. A detector that throws passes the text through."
  (multiple-value-bind (messages clean-p) (apply next args)
    (if (null (ignore-errors (setting :redact)))
        (values messages clean-p)
        (handler-case
            (values (if (listp messages) (mapcar #'redact-message messages) messages)
                    clean-p)
          (error () (values messages clean-p))))))

;;; --- what the model is told -------------------------------------------------------

(defparameter +prompt+
  "A secret belongs in the drop, not in the chat. When a task needs a password, token or key, do not ask for it to be pasted into a message or a tool argument: call request_secret with the variable name the task needs, or ask the operator to run /secret-drop NAME VALUE. The value then sits in the environment every shell command inherits: use $NAME and do not print, log or encode it. list_secrets names what is stored and never shows a value; a credential-shaped value printed by a command comes back to you as [SECRET-DROP]."
  "The section this cell keeps in front of the model, on every request.")

(defun standing-section (next session-id)
  "Always supply the drop's guidance, beside the other cells' sections."
  (acons "secret-drop" +prompt+
         (remove "secret-drop" (funcall next session-id) :key #'car :test #'equal)))

;;; --- the tools ----------------------------------------------------------------------

(nlk:define-contract list-secrets-call
  (:doc "List the secrets the operator has dropped into the environment: the variable name, how long the value is, and whether it is exported and redacted. A value is never shown; use $NAME in a shell command instead.")
  ("name" :text :doc "only this variable, as dropped; every stored name when empty"))

(nlk:define-contract request-secret-call
  (:doc "Ask the operator to drop a secret into the environment, when a task needs a password, token or key. Nothing is stored by this call: it says which name is needed and puts it on the operator's board, and they answer with /secret-drop. Never ask for the value in the chat.")
  ("name" :text :required t :doc "the environment variable name the task needs, like DEPLOY_TOKEN")
  ("why" :text :doc "one sentence on what needs it, shown to the operator"))

(defun describe-secret (pair)
  "PAIR as one line: the name, the value's length, and what is done with it."
  (let* ((name (car pair))
         (value (cdr pair))
         (exported (exported-p name))
         (searched (redaction-eligible-p value)))
    (format nil "~a: ~d characters, ~:[not in the environment~;in the environment as $~a~], ~a~:[~;, stored only (not credential-shaped)~]"
            name
            (length value)
            exported
            name
            (if searched "redacted in the transcript" "not searched for in text")
            (not searched))))

(defun list-answer (call)
  "The list_secrets answer: names, never values."
  (handler-case
      (let* ((wanted (nlk:trimmed (or (list-secrets-call-name call) "")))
             (pairs (if (plusp (length wanted))
                        (let ((hit (assoc (string-upcase wanted) *secrets* :test #'string=)))
                          (unless hit
                            (fail "no secret called ~a; ask for the whole list"
                                  (string-upcase wanted)))
                          (list hit))
                        *secrets*)))
        (if (null pairs)
            "No secret is stored: the operator drops one with /secret-drop NAME VALUE."
            (format nil "~{~a~%~}" (mapcar #'describe-secret pairs))))
    (error (condition)
      (nle:failure (format nil "ERROR: SECRET-DROP: ~a" condition)))))

(defun request-answer (call)
  "The request_secret answer: what to tell the operator, and what is still missing."
  (handler-case
      (let* ((name (check-name (request-secret-call-name call)))
             (why (nlk:trimmed (or (request-secret-call-why call) "")))
             (stored (assoc name *secrets* :test #'string=)))
        (if stored
            (format nil "~a is already dropped (~d characters)~@[; use $~a in a command~]. Do not print it."
                    name (length (cdr stored)) (and (exported-p name) name))
            (progn
              (nle:notice (format nil "secret-drop: ~a is needed~@[ for ~a~]; run /secret-drop ~a VALUE"
                                  name (and (plusp (length why)) why) name)
                          :level :info :key "nodecode-secret-drop")
              (format nil "Asked the operator for ~a; nothing is stored yet. Their /secret-drop answer puts it in the environment, not in this conversation. Use $~a in a shell command once they say it is there, and list_secrets confirms it arrived."
                      name name))))
    (error (condition)
      (nle:failure (format nil "ERROR: SECRET-DROP: ~a" condition)))))

;;; --- the drop ------------------------------------------------------------------------

(defun drop (name value)
  "Store VALUE as NAME, export it, and answer one line that does not repeat it."
  (let* ((name (check-name name))
         (value (check-value value))
         (replaced (and (assoc name *secrets* :test #'string=) t)))
    (setf *secrets* (cons (cons name value)
                           (remove name *secrets* :key #'car :test #'string=)))
    (write-store *secrets*)
    (export-value name value)
    (let ((eligible (redaction-eligible-p value)))
      (format nil "~a dropped~:[~; (it replaces the one that was there)~]: stored in ~a~@[, in the environment as $~a~]~a. The value is not shown."
              name
              replaced
              (namestring (store-path))
              (and (exported-p name) name)
              (if eligible
                  ", and an exact copy of it is redacted in the text that reaches the model"
                  ", but it is not searched for in text: it is short or word-like, so copy it with $NAME rather than printing it")))))

(defun forget (name)
  "Take NAME out of the store, the file and the environment."
  (let* ((name (check-name name))
         (hit (assoc name *secrets* :test #'string=)))
    (unless hit (fail "no secret called ~a" name))
    (setf *secrets* (remove name *secrets* :key #'car :test #'string=))
    (write-store *secrets*)
    (when (assoc name *exported* :test #'string=)
      (retract-exports-of name))
    (format nil "~a removed: ~d secret~:p left" name (length *secrets*))))

(defun retract-exports-of (name)
  "Give the environment back the one NAME this cell set."
  (let ((pair (assoc name *exported* :test #'string=)))
    (when pair
      (ignore-errors
        (if (cdr pair)
            (sb-posix:setenv name (cdr pair) 1)
            (sb-posix:unsetenv name)))
      (setf *exported* (remove pair *exported*)))))

(defun list-line ()
  "The stored names, as /secret-drop list answers them."
  (if (null *secrets*)
      "No secret is stored yet: /secret-drop NAME VALUE drops one."
      (format nil "~{~a~%~}" (mapcar #'describe-secret *secrets*))))

(defun doctor-line ()
  "The store, the environment and the redaction, as /secret-drop doctor sees them."
  (format nil "store: ~a (~:[missing~;present~])~%passthrough: ~:[off~;on~]~%redaction: ~:[off~;on~]~%redaction floor: ~d characters, ~d value~:p long enough to search for~%held back: ~:[(none)~;~:*~{~a~^, ~}~]~%stored: ~d (~:[(none)~;~:*~{~a~^, ~}~])~%exported here: ~:[(none)~;~:*~{~a~^, ~}~]"
          (namestring (store-path))
          (and (probe-file (store-path)) t)
          (and (ignore-errors (setting :passthrough)) t)
          (and (ignore-errors (setting :redact)) t)
          (or (ignore-errors (setting :min-redaction-chars))
              +min-redaction-chars+)
          (length (redact-values))
          (remove-if-not #'hold-back-p (stored-names))
          (length *secrets*)
          (stored-names)
          (mapcar #'car *exported*)))

;;; --- /secret-drop ---------------------------------------------------------------------

(defun drop-dialog ()
  "The panel /secret-drop answers with no argument: one field that runs the
command with the name and the value typed into it."
  (nle:list-dialog
   :title "Secret drop"
   :summary "A value typed here is stored here and put in the environment; it never enters the conversation."
   :rows (list (nle:ask-row "Drop a secret" "NAME VALUE" "/secret-drop "
                             :placeholder "DEPLOY_TOKEN s3cr3t-value" :verb "drop"))))

(defun value-dialog (name)
  "The panel /secret-drop NAME answers with: only the value is left to type."
  (nle:list-dialog
   :title "Secret drop"
   :summary (format nil "~a is stored in ~a and put in the environment; the value never enters the conversation."
                    name (namestring (store-path)))
   :rows (list (nle:ask-row (format nil "The value for ~a" name) "VALUE"
                             (format nil "/secret-drop ~a " name)
                             :placeholder "the secret" :verb "drop"))))

(defun run-secret-drop (args session-id)
  "One /secret-drop invocation => (values TEXT DIALOG). With no argument the
dialog offers the field; with a name and a value it drops it; with a name
alone it opens the field for that one value."
  (declare (ignore session-id))
  (let* ((line (nlk:trimmed (or args "")))
         (words (remove "" (uiop:split-string line :separator '(#\Space #\Tab))
                        :test #'string=))
         (verb (first words)))
    (handler-case
        (cond
          ((null verb) (values (list-line) (drop-dialog)))
          ((string-equal verb "list") (values (list-line) nil))
          ((string-equal verb "doctor") (values (doctor-line) nil))
          ((string-equal verb "rm")
           (values (forget (or (second words)
                               (fail "/secret-drop rm needs a name")))
                   nil))
          ((cdr words)
           (values (drop verb (format nil "~{~a~^ ~}" (cdr words))) nil))
          (t (values (format nil "~a: type its value after it, or use the field below."
                             verb)
                     (value-dialog verb))))
      (error (condition) (values (format nil "secret-drop: ~a" condition) nil)))))

(defparameter +manual+
  "A secret dropped here goes into the environment, not into the conversation:
/secret-drop NAME VALUE stores it in a file only its owner reads and puts it in
the environment every shell command inherits, so a command uses $NAME and the
model never sees the value. A credential-shaped value -- at least 8 characters
with a digit or symbol, or at least 24 characters -- is also redacted from the
text that reaches the model, as an exact match; a short or word-like value is
stored but not searched for, so do not print it.
  /secret-drop              the drop field, and what is stored
  /secret-drop NAME         the field for that one value
  /secret-drop NAME VALUE   store it now, in your own input box
  /secret-drop list         the names, never the values
  /secret-drop rm NAME      take it out of the store and the environment
  /secret-drop doctor       the store path, passthrough, redaction, held back
  list_secrets              the model's view: names, lengths, exported, redacted
  request_secret            the model asks for a name; you answer with /secret-drop
The values are this image's environment for as long as it runs: a stop puts
back whatever was there before, and a provider credential such as
OPENAI_API_KEY is stored but held back from the environment unless the section's
hold_back stops naming it."
  "What (help :secret-drop) answers while the cell runs.")

;;; --- the declaration ------------------------------------------------------------------

(nle:define-cell secret-drop
  (:section ("secret-drop")
    (:guide "nothing here is required: /secret-drop NAME VALUE stores a secret, passthrough puts it in the environment, redact hides exact copies on the way to the model")
    ("store" :path :doc "the store file; default secret-drop/store.lisp under the home")
    ("passthrough" :boolean :default t
                   :doc "put each stored value into this image's environment at start, so every shell command inherits $NAME")
    ("hold_back" :list
                 :doc "names, or name prefixes, stored but never put in the environment")
    ("redact" :boolean :default t
              :doc "replace an exact copy of a credential-shaped value in the text that reaches the model")
    ("min_redaction_chars" :integer :default 8 :min 1
                           :doc "shorter values are never used as a global search-and-replace"))
  (:tool "list_secrets" (nlk:contract 'list-secrets-call) #'list-answer)
  (:tool "request_secret" (nlk:contract 'request-secret-call) #'request-answer)
  (:help :secret-drop "secret-drop: put a secret in the environment without it entering the conversation" +manual+)
  (:command "secret-drop" #'run-secret-drop
            :description "Store a secret as an environment variable without showing it to the model"
            :argument-hint "NAME VALUE | NAME | list | rm NAME | doctor"
            :complete (lambda (text session-id)
                        (declare (ignore text session-id))
                        (mapcar (lambda (verb) (list :name verb :value verb))
                                '("list" "rm" "doctor"))))
  (:hook 'nodecode.evolved::wire-messages-from-events #'redact-wire)
  (:hook 'nle::read-harness-sections #'standing-section)
  (:start (lambda ()
            (setf *secrets* (read-store)
                  *exported* nil)
            (loop for (name . value) in *secrets*
                  do (export-value name value))
            (nle:on-stop (lambda ()
                           (retract-exports)
                           (setf *secrets* nil)))
            (nle:notice (format nil "secret-drop: ~d secret~:p stored; /secret-drop drops another"
                                (length *secrets*))
                        :level :info :key "nodecode-secret-drop"))))
