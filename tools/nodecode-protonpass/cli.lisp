;;;; cli.lisp --- running pass-cli, and turning what it wrote into values.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes secret source: at startup it resolved
;;;; ENV_VAR -> pass://vault/item/field references, or a whole vault of
;;;; env-var-titled items, through the official pass-cli and put the values
;;;; in Hermes' environment. A cell has no startup to fill, so the same reads
;;;; are tools the model calls: one reference, one vault's titles, one whole
;;;; vault. Everything here is the part the original owned — the argv, the
;;;; session, the cache and the words a failure gets.
;;;;
;;;; *RUN* is the one seam: a tool builds its argv and hands it to *RUN*, so a
;;;; test asserts the argv and answers the output with no pass-cli installed,
;;;; no Proton account and no network.

(in-package #:nodecode-protonpass)

;;; --- the one seam -----------------------------------------------------------

(defparameter *run* 'run-cli
  "How every read reaches pass-cli: (funcall *RUN* ARGV :ENVIRONMENT ENV) =>
(values STDOUT STDERR STATUS), STATUS an exit code or :TIMEOUT.")

(defun run-cli (argv &key environment)
  "Run ARGV to completion or the section's timeout_seconds, whichever comes
first => (values STDOUT STDERR STATUS)."
  (handler-case (nlk:run-bounded argv :seconds (setting :timeout-seconds) :environment environment)
    (error (condition)
      ;; A CLI that is not installed is the ordinary first state of this cell,
      ;; so it is words the model can act on, not a signalled error: 127 is
      ;; what a shell says for a command it cannot find.
      (values nil
              (format nil "~a could not be run: ~a. Install pass-cli (https://protonpass.github.io/pass-cli/), or set the protonpass section's command."
                      (first argv) condition)
              127))))

;;; --- the session -------------------------------------------------------------

(defvar *getenv* #'uiop:getenv
  "How the cell reads the token's environment variable: UIOP:GETENV, or a
test's own answer.")

(defvar *relogged* nil
  "Whether this start already tried a token login after an authentication
failure: one retry, as the original allowed, never a loop.")

(defun token-name ()
  "The environment variable the section says holds a token, or NIL."
  (let ((name (setting :token-env)))
    (and (stringp name)
         (plusp (length (trim name)))
         (trim name))))

(defun token-value ()
  "The token this image can see, or NIL: nothing is read from a file."
  (let ((name (token-name)))
    (and name (funcall *getenv* name))))

(defun session-directory ()
  "The session directory the cell owns: a token login lands here, so a login
the operator made in their own terminal is never overwritten."
  (nlk:home "protonpass/session/"))

(defun environment (&key reason login)
  "What the pass-cli child is given beyond the environment this image already
has: its telemetry off, the read's reason (an AI-agent token's audit log
records it), and — when a token is configured — the cell's own session
directory and, for a login, the token."
  (let ((token (token-value)))
    (append (list "PROTON_PASS_DISABLE_TELEMETRY=1")
             (and reason (list (format nil "PROTON_PASS_AGENT_REASON=~a" (clip reason 300))))
             (and token
                  (list (format nil "PROTON_PASS_SESSION_DIR=~a"
                                (uiop:native-namestring (session-directory)))
                        (format nil "PROTON_PASS_KEY_PROVIDER=~a" (setting :key-provider))))
             (and token login
                  (list (format nil "PROTON_PASS_PERSONAL_ACCESS_TOKEN=~a" token))))))

(defun login ()
  "Sign pass-cli in with the personal access token TOKEN_ENV names, into the
cell's own session directory. Refused when no token is set: an interactive
pass-cli login belongs to the operator, in their own terminal."
  (let ((token (token-value)))
    (unless token
      (fail "no token set: put one in the environment variable ~a (pass-cli pat create --name nodecode, then grant it vault access), or run pass-cli login in a terminal"
            (or (token-name) "PROTON_PASS_PERSONAL_ACCESS_TOKEN")))
    (ensure-directories-exist (session-directory))
    #-win32 (ignore-errors (sb-posix:chmod (uiop:native-namestring (session-directory)) #o700))
    (multiple-value-bind (out err status)
        (funcall *run* (append (setting :command) (list "login"))
                 :environment (environment :reason "Nodecode: sign in" :login t))
      (declare (ignore out))
      (unless (eql status 0)
        (fail "pass-cli login with the personal access token failed: ~a" (detail err status))))
    (setf *relogged* t)
    t))

(defun run-read (argv &key reason)
  "One pass-cli read => (values STDOUT STDERR STATUS). A failure that says
the session is gone, while a token is configured, is logged in again once and
the read retried, as the original did."
  (multiple-value-bind (out err status)
      (funcall *run* argv :environment (environment :reason reason))
    (when (and (token-value) (not *relogged*) (integerp status) (plusp status)
               (auth-error-p err argv))
      (handler-case (login) (error () nil))
      (multiple-value-setq (out err status)
        (funcall *run* argv :environment (environment :reason reason))))
    (values out (redact err (token-value)) status)))

;;; --- words -------------------------------------------------------------------

(defun trim (text)
  "TEXT without the whitespace a line ends or begins with."
  (string-trim '(#\Space #\Tab #\Return #\Newline) (or text "")))

(defun clip (text limit)
  "TEXT cut at LIMIT characters."
  (if (> (length text) limit) (subseq text 0 limit) text))

(defun replace-all (text old new)
  "TEXT with every occurrence of OLD written NEW."
  (if (or (null old) (zerop (length old))) (or text "")
      (with-output-to-string (out)
        (loop with start = 0
              for at = (search old text :start2 start)
              do (cond (at (write-string text out :start start :end at)
                           (write-string new out)
                           (setf start (+ at (length old))))
                       (t (write-string text out :start start)
                          (return)))))))

(defun redact (text token)
  "TEXT with TOKEN written <redacted>: pass-cli should never echo it, and this
is the defence the original kept for the day it does."
  (replace-all (or text "") token "<redacted>"))

(defun detail (stderr status)
  "A readable one line of what pass-cli complained about. Never its stdout,
which may hold a value."
  (let ((lines (loop for line in (uiop:split-string (or stderr "") :separator '(#\Newline))
                     for trimmed = (trim line)
                     when (plusp (length trimmed)) collect trimmed)))
    (let ((text (clip (format nil "~{~a~^ ~}" (last lines 3)) 240)))
      (if (plusp (length text)) text (format nil "exit ~a" status)))))

(defun first-line (text)
  "The first non-empty line of TEXT, clipped."
  (or (loop for line in (uiop:split-string (or text "") :separator '(#\Newline))
            for trimmed = (trim line)
            when (plusp (length trimmed)) return (clip trimmed 120))
      ""))

(defparameter +auth-markers+
  '("authenticated client" "no session" "not logged" "login flow" "access token"
    "session expired" "expired" "unauthor" "401" "403")
  "What pass-cli says when it has no usable session, as the original read it.
The arguments the read passed are removed first, so an item called \"expired
card\" cannot pass for an authentication failure.")

(defun auth-error-p (stderr echoed)
  "Whether STDERR says the session is gone, ECHOED (the argv) taken out first."
  (let ((text (string-downcase (or stderr ""))))
    (dolist (argument echoed)
      (when (and (stringp argument) (plusp (length argument)))
        (setf text (replace-all text (string-downcase argument) ""))))
    (and (some (lambda (marker) (search marker text)) +auth-markers+) t)))

;;; --- references ---------------------------------------------------------------

(defun reference (text)
  "TEXT as pass-cli reads it: pass://vault/item/field, the scheme optional.
Refused unless it is exactly three non-empty segments — vault and item by
name or by id, the id being what tells two same-titled items apart."
  (let* ((trimmed (trim text))
         (body (if (uiop:string-prefix-p "pass://" trimmed) (subseq trimmed 7) trimmed))
         (parts (and (plusp (length body)) (uiop:split-string body :separator "/"))))
    (unless (and parts
                 (= 3 (length parts))
                 (every (lambda (part) (plusp (length (trim part)))) parts))
      (fail "~s is not a Proton Pass reference: write pass://vault/item/field, naming all three segments (vault and item by name or by id)."
            (or text "")))
    (when (some (lambda (character) (< (char-code character) 32)) body)
      (fail "~s carries a control character, which a reference never does" text))
    (concatenate 'string "pass://" body)))

;;; --- reading ------------------------------------------------------------------

(defvar *cache* (make-hash-table :test #'equal)
  "Values already read, by reference, with the time they were read: the
original's cache, kept in this image's memory only, for cache_ttl_seconds.")

(defun read-reference (ref &key name)
  "One item field, as pass-cli answers it."
  (multiple-value-bind (out err status)
      (run-read (append (setting :command) (list "item" "view" "--" ref))
                :reason (format nil "Nodecode: load ~a" (or name ref)))
    (unless (eql status 0)
      (fail "pass-cli could not read ~a: ~a" ref (detail err status)))
    (let ((value (string-right-trim '(#\Return #\Newline) (or out ""))))
      (when (zerop (length (trim value)))
        (fail "pass-cli answered an empty value for ~a" ref))
      value)))

(defun cached-reference (ref &key name fresh)
  "REF's value: the one this image already read while cache_ttl_seconds
allows, else a fresh read. FRESH reads past the cache."
  (let* ((ttl (setting :cache-ttl-seconds))
         (now (get-universal-time))
         (hit (and (plusp ttl) (not fresh) (gethash ref *cache*))))
    (if (and hit (< (- now (cdr hit)) ttl))
        (car hit)
        (let ((value (read-reference ref :name name)))
          (when (plusp ttl) (setf (gethash ref *cache*) (cons value now)))
          value))))

(defun vault-name (named)
  "The vault a call names, else the section's, else a refusal saying which."
  (let ((vault (trim named)))
    (cond ((plusp (length vault)) vault)
          ((plusp (length (trim (setting :vault)))) (trim (setting :vault)))
          (t (fail "no vault named: pass a vault, or set the protonpass section's vault to the vault name or share id to read when a call names none")))))

(defun items (stdout vault)
  "The item entries in pass-cli's `item list --output json` answer."
  (let* ((payload (handler-case (nlk:decode-json (or stdout "")) (error () nil)))
         (entries (or (nlk:json-value payload :array "items")
                      (and (vectorp payload) (not (stringp payload)) payload))))
    (unless entries
      (fail "pass-cli answered no item list for vault ~a: ~a" vault (clip (trim (or stdout "")) 240)))
    entries))

(defun item-title (item)
  "The title an `item list` entry carries."
  (or (nlk:json-value item :text "content" "title")
      (nlk:json-value item :text "title")))

(defun field-in (table field)
  "TABLE's FIELD, when it holds a non-empty string."
  (when (hash-table-p table)
    (multiple-value-bind (value present) (gethash field table)
      (and present (stringp value) (plusp (length value)) value))))

(defun item-field (item field)
  "The item's FIELD, as the original read it: the typed bodies first, then
the custom (extra) fields."
  (let* ((content (nlk:json-value item :object "content"))
         (typed (and content (nlk:json-value content :object "content"))))
    (or (and typed
             (loop for body being the hash-values of typed
                   for value = (field-in body field)
                   when value return value))
        (loop for extra across (or (and content (nlk:json-value content :array "extra_fields")) #())
              when (equal field (nlk:json-value extra :string "name"))
                do (let ((value (loop for candidate being the hash-values of
                                       (or (nlk:json-value extra :object "content") (make-hash-table))
                                      when (and (stringp candidate) (plusp (length candidate)))
                                        return candidate)))
                     (when value (return value)))))))

(defun env-name-p (text)
  "Whether TEXT is a name an environment variable may have, which is how the
original decided which items of a vault contribute."
  (and (stringp text) (plusp (length text))
       (let ((first (char text 0)))
         (and (or (alpha-char-p first) (char= first #\_))
              (every (lambda (character)
                       (or (char= character #\_) (alphanumericp character)))
                     text)))))
