;;;; cell.lisp --- the primer, the tools, the /protonpass command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes secret source: it resolved references into
;;;; Hermes' environment before startup. Nodecode has no secret-source seam, so
;;;; this cell puts the same reads where a user needs them — in the
;;;; conversation: four tools, one command and a manual topic. No model is
;;;; served here, and no secret is stored: pass-cli keeps the session, and
;;;; every value is read from it when asked for.

(in-package #:nodecode-protonpass)

(defparameter +primer+
  "Proton Pass is reachable through the nodecode-protonpass cell: four tools that run the
pass-cli installed on this machine, and read items out of the operator's own Proton Pass
account. Nothing is stored here — pass-cli keeps the session, and a value is read when a
tool asks for it.
  protonpass_list     the titles of the items in one vault, and no value at all. Start here:
                      a title is what a reference's item segment names.
  protonpass_get      one field of one item: pass://vault/item/field. The scheme may be
                      left off. Vault and item may be names or ids, and an id is what tells
                      two items of the same title apart. The answer is the value alone.
                      `name` is the environment variable the value is for; it is what
                      Proton's audit log shows. `fresh` reads past the cache.
  protonpass_vault    a whole vault the way a bulk import works: every item whose title is
                      a valid environment-variable name contributes its field (password by
                      default). A plain access token is needed — an AI-agent token may not
                      list secrets.
  protonpass_status   whether pass-cli runs here and is signed in, the default vault and
                      field, the cache, how many mappings there are.
A reference is exactly three segments; a relative or two-segment one is refused before
pass-cli runs. Reads are cached in this image for cache_ttl_seconds (300 by default; 0
reads every time), so say fresh when the operator just rotated a key.
Signing in: with a token in the environment variable TOKEN_ENV names, /protonpass login
signs pass-cli in to a session of its own under the Nodecode home; without one, whatever
pass-cli login this process sees is used, and nothing is signed in on the operator's behalf.
When a read fails, the answer says what pass-cli said and what fixes it: an expired token
wants /protonpass login, a missing binary wants pass-cli installed, a missing item wants a
different reference (protonpass_list names the real ones).
How to work: list the vault, read the one reference the operator asked about, and put the
value where they asked for it. A value read here is as sensitive as the key it is: do not
echo it into a chat channel, a commit or a file the operator did not name, and prefer
naming the mapping they already have (help :protonpass, or /protonpass env) over pasting
a value about."
  "What (help :protonpass) answers while the cell runs: the original's README, in the
cell's voice.")

;;; --- the tools' boundaries ----------------------------------------------------

(nlk:define-contract secret-read
  (:doc "Read one field of one Proton Pass item, through the pass-cli signed in on this machine. Give the reference the way Proton writes it: pass://vault/item/field, vault and item by name or by id. The answer is the value alone, and nothing else.")
  ("reference" :text :required t
   :doc "pass://vault/item/field — the item field to read; the pass:// may be left off")
  ("name" :string
   :doc "the environment variable this value is for, when the read is loading one; it is what Proton's audit log records")
  ("fresh" :boolean
   :doc "read past the cache, which otherwise holds a value for cache_ttl_seconds"))

(nlk:define-contract vault-listing
  (:doc "The titles of the items in one Proton Pass vault, and no value at all: how to find the item and field a pass:// reference should name. The vault's name or share id.")
  ("vault" :string
   :doc "the vault's name or share id; defaults to the protonpass section's vault"))

(nlk:define-contract vault-read
  (:doc "A whole vault, the way a bulk import reads one: every item whose title is a valid environment-variable name (OPENAI_API_KEY) contributes its field, and the answer is one NAME=value line each. Needs a plain personal access token — an AI-agent token may not list a vault's secrets.")
  ("vault" :string
   :doc "the vault's name or share id; defaults to the protonpass section's vault")
  ("field" :string
   :doc "the item field to take: password (the default), username, or a custom field's name"))

(defun no-arguments (description)
  "The schema a tool that takes no arguments registers."
  (list :description description
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:json-object))))

;;; --- the tools ------------------------------------------------------------------

(defun read-secret (report)
  "protonpass_get: one item field, the value alone."
  (cached-reference (reference (secret-read-reference report))
                    :name (secret-read-name report)
                    :fresh (secret-read-fresh report)))

(defun agent-token-hint (stderr)
  "The original's one extra sentence when a listing failed for an agent token."
  (if (search "agent" (string-downcase (or stderr "")))
      " (an AI-agent token cannot list a vault's secrets: read one pass://vault/item/field reference at a time)"
      ""))

(defun list-vault-items (report)
  "protonpass_list: the vault's titles, without a value."
  (let ((vault (vault-name (vault-listing-vault report))))
    (multiple-value-bind (out err status)
        (run-read (append (setting :command)
                          (list "item" "list" "--filter-state" "active" "--output" "json" "--" vault))
                  :reason (format nil "Nodecode: list vault ~a" vault))
      (unless (eql status 0)
        (fail "pass-cli could not list vault ~a: ~a~a" vault (detail err status) (agent-token-hint err)))
      (let ((titles (loop for item across (items out vault)
                          for title = (item-title item)
                          when title collect title)))
        (format nil "vault ~s: ~a item~:p~%~{~a~%~}" vault (length titles) titles)))))

(defun read-vault (report)
  "protonpass_vault: one NAME=value line per env-var-titled item."
  (let* ((vault (vault-name (vault-read-vault report)))
         (named (trim (vault-read-field report)))
         (field (if (plusp (length named)) named (setting :field)))
         (token (token-name)))
    (multiple-value-bind (out err status)
        (run-read (append (setting :command)
                          (list "item" "list" "--filter-state" "active" "--show-secrets"
                                "--output" "json" "--" vault))
                  :reason (format nil "Nodecode: load vault ~a" vault))
      (unless (eql status 0)
        (fail "pass-cli could not list vault ~a: ~a~a" vault (detail err status) (agent-token-hint err)))
      (let ((secrets '()) (notes '()))
        (loop for item across (items out vault)
              for title = (item-title item)
              when (and title (env-name-p title))
                do (let ((value (item-field item field)))
                     (cond ((null value)
                            (push (format nil "~a: no non-empty ~a field" title field) notes))
                           ((assoc title secrets :test #'string=)
                            (push (format nil "~a: a second item of that title, the first kept" title) notes))
                           (t (push (cons title value) secrets)))))
        (when token (setf secrets (remove token secrets :key #'car :test #'string=)))
        (setf secrets (nreverse secrets) notes (nreverse notes))
        (with-output-to-string (answer)
          (format answer "vault ~s, field ~a: ~a variable~:p~%" vault field (length secrets))
          (loop for (name . value) in secrets do (format answer "~a=~a~%" name value))
          (loop for note in notes do (format answer "! ~a~%" note)))))))

(defun mappings ()
  "The section's ENV_VAR=pass://... lines, as (NAME . REFERENCE)."
  (loop for line in (setting :mappings)
        for text = (trim line)
        for equals = (position #\= text)
        when (and equals (plusp equals))
          collect (cons (trim (subseq text 0 equals)) (trim (subseq text (1+ equals))))))

(defun mappings-report ()
  "The mappings as a dry run: what a startup would export, never a value."
  (let ((pairs (mappings)))
    (if (null pairs)
        "no mappings yet: add \"NAME=pass://vault/item/field\" lines to the protonpass section's mappings, or read one with /protonpass get pass://vault/item/field"
        (with-output-to-string (answer)
          (format answer "~a mapping~:p, none of the values read here:~%" (length pairs))
          (dolist (pair pairs)
            (format answer "  ~a <- ~a  ~a~%" (car pair) (cdr pair)
                    (if (and (env-name-p (car pair)) (funcall *getenv* (car pair)))
                        "skip (already set in this process)"
                        "would export")))))))

(defun status-text ()
  "What pass-cli is here and who it is signed in as: no value is read."
  (let* ((command (setting :command))
         (version (multiple-value-list
                   (funcall *run* (append command (list "--version"))
                            :environment (environment :reason "Nodecode: status"))))
         (info (multiple-value-list
                (funcall *run* (append command (list "info"))
                         :environment (environment :reason "Nodecode: status")))))
    (with-output-to-string (answer)
      (format answer "pass-cli: ~a~%"
              (if (eql (third version) 0)
                  (first-line (first version))
                  (format nil "not runnable here (~a)" (detail (second version) (third version)))))
      (format answer "signed in: ~a~%"
              (if (eql (third info) 0)
                  "yes"
                  "no — a read will say so; /protonpass login signs in with a token, or run pass-cli login in a terminal"))
      (format answer "command: ~{~a~^ ~}~%" command)
      (format answer "token: ~a~%"
              (if (token-value)
                  (format nil "~a is set; the cell signs in to a session of its own, so a login of your own is left alone"
                          (token-name))
                  "none set (TOKEN_ENV names the variable to put one in); the pass-cli login this process sees is used"))
      (format answer "vault: ~a; field: ~a~%"
              (let ((vault (trim (setting :vault))))
                (if (plusp (length vault)) vault "none (a call must name one)"))
              (setting :field))
      (format answer "cache: ~a~%"
              (if (plusp (setting :cache-ttl-seconds))
                  (format nil "~a s in this image's memory" (setting :cache-ttl-seconds))
                  "off, every read goes to pass-cli"))
      (format answer "mappings: ~a~%" (length (mappings))))))

(defun status (arguments)
  "protonpass_status: the CLI, the session, the defaults."
  (declare (ignore arguments))
  (status-text))

;;; --- the operator's command ------------------------------------------------------

(defun login-once ()
  "Sign pass-cli in with the token, and say where the session went."
  (login)
  (clrhash *cache*)
  (format nil "signed pass-cli in with the token in ~a; the session lives in ~a"
          (token-name) (uiop:native-namestring (session-directory))))

(defun run-command (args session-id)
  "One /protonpass invocation: nothing for a status, or one of env, get, login, forget."
  (declare (ignore session-id))
  (let* ((text (trim args))
         (verb (or (first (uiop:split-string text :separator '(#\Space))) ""))
         (rest-text (trim (subseq text (length verb)))))
    (cond ((member (string-downcase verb) '("" "status") :test #'string=) (status-text))
          ((string-equal verb "env") (mappings-report))
          ((string-equal verb "get")
           (if (plusp (length rest-text))
               (read-reference (reference rest-text))
               "usage: /protonpass get pass://vault/item/field"))
          ((string-equal verb "login") (login-once))
          ((string-equal verb "forget")
           (clrhash *cache*)
           "the cache is empty: the next read goes to pass-cli")
          (t (format nil "/protonpass takes nothing for a status, or one of: env (the mappings, as a dry run), get pass://vault/item/field, login (the token's session), forget (the cache); it read ~s"
                     (or args ""))))))

;;; --- the declaration ---------------------------------------------------------------

(nlk:define-peripheral protonpass :not-running t)

(nle:define-cell protonpass
  (:section ("protonpass")
    (:guide "install pass-cli (https://protonpass.github.io/pass-cli/) and sign in with pass-cli login, or put a token in the variable token_env names; vault and field are optional defaults; nothing else is needed")
    ("command" :list :default '("pass-cli")
     :doc "the argv prefix that runs pass-cli: [\"pass-cli\"] for an install on PATH, or an absolute path when PATH is not where it lives")
    ("token_env" :string :default "PROTON_PASS_PERSONAL_ACCESS_TOKEN"
     :doc "the environment variable holding a personal access token or AI-agent token, for a machine with no interactive login; the cell then signs pass-cli in to a session of its own. Leave it unset to use the pass-cli login this process already sees")
    ("key_provider" :string :default "fs"
     :doc "PROTON_PASS_KEY_PROVIDER for that session: fs keeps its key beside the session, for a server with no keyring")
    ("vault" :string :default ""
     :doc "the vault name or share id a call that names none reads")
    ("field" :string :default "password"
     :doc "the item field the vault read takes when its call names none")
    ("mappings" :list
     :doc "ENV_VAR=pass://vault/item/field lines: the original's env map, which /protonpass env reports and nothing here reads")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one pass-cli call's ceiling; its process tree is killed past it")
    ("cache_ttl_seconds" :integer :default 300 :min 0
     :doc "how long a value read here stays in this image's memory; 0 reads every time"))
  (:help :protonpass
         "protonpass: read Proton Pass secrets through your own pass-cli - (help :protonpass)"
         +primer+)
  (:tool "protonpass_get" (nlk:contract 'secret-read) #'read-secret)
  (:tool "protonpass_list" (nlk:contract 'vault-listing) #'list-vault-items)
  (:tool "protonpass_vault" (nlk:contract 'vault-read) #'read-vault)
  (:tool "protonpass_status"
         (no-arguments "Whether pass-cli runs here and is signed in, the default vault and field, the cache, and how many mappings the section carries.")
         #'status)
  (:command "protonpass" 'run-command
            :description "Proton Pass: pass-cli's session, the mappings, one read, sign in"
            :session nil)
  (:start (lambda ()
            (clrhash *cache*)
            (setf *relogged* nil))))
