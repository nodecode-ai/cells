;;;; cell.lisp --- the primer, the four tools, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes secret source: at startup it walked the
;;;; operator's GPG-encrypted pass store, ran `pass show' for every entry under
;;;; the configured subdirectories and handed the values over as environment
;;;; variables. Nodecode has no secret-source seam, so the same reads are where
;;;; a session needs them -- in the conversation: four tools, one command and a
;;;; manual topic. Nothing is stored here and no value is cached: the store and
;;;; the GPG key stay the operator's.

(in-package #:nodecode-pass-secrets)

(nlk:define-peripheral pass-secrets :not-running t)

(defparameter +primer+
  "The pass store is reachable through the nodecode-pass-secrets cell: four tools that walk a local
GPG-encrypted password store and read entries out of it with the `pass' binary installed on this
machine. Nothing is stored, cached or sent anywhere; a value is read when a tool asks for it.
  pass_list    the entries under the configured subdirectories -- names only, no value. Start
               here: an entry's path inside the store is what pass_show takes.
  pass_show    one entry's value, through `pass show -- NAME', and the value alone. first_line
               takes only the first line, for an entry whose rest is metadata.
  pass_env     the whole store as the environment the original injected: one NAME=value line per
               entry whose leaf name is an environment variable's. A plain read of every value,
               so prefer pass_show for the one key the operator named.
  pass_status  whether pass runs here, where the store is, how many entries each subdirectory
               holds, and which entries no environment could name.
Every entry is a *.gpg file under the store; its leaf name is the environment variable it stands
for (UPPER_SNAKE_CASE), and its path inside the store is the name the tools take.
A read may wait on gpg-agent, which asks for the key's passphrase itself; the timeout setting
bounds one call. When a read fails, the answer is what pass said: a missing binary wants pass
installed, an empty store wants pass init with the GPG key, an unknown entry wants pass_list.
How to work: a value read here is as sensitive as the secret it is -- pass it to the one place
the operator asked for, and do not echo it into a chat channel, a commit or a file they did not
name. (secret \"shared/OPENAI_API_KEY\"), evaluated in this image, reads the same entry without
putting it in the transcript."
  "What (help :pass-secrets) answers while the cell runs: the original's README, in the
cell's voice.")

;;; --- the tools' boundaries -----------------------------------------------------

(nlk:define-contract entry-listing
  (:doc "The entries in the pass store, names only and no value: the path inside the store that pass_show takes, and the environment variable each one is named for.")
  ("subdir" :string
            :doc "one store subdirectory to list instead of the configured ones, such as shared"))

(nlk:define-contract secret-read
  (:doc "One entry of the pass store, read with `pass show -- NAME'. The answer is the value alone. It is a secret: put it where the operator asked for it and do not echo it further.")
  ("name" :text :required t
          :doc "the entry's path inside the store, such as shared/OPENAI_API_KEY")
  ("first_line" :boolean
                :doc "take only the first line, for an entry whose first line is the password and whose rest is metadata"))

(defun no-arguments (description)
  "The schema a tool that takes no arguments registers."
  (list :description description
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:json-object))))

(defun marked-entry (name)
  "NAME as one line of a listing: the entry, and the variable it is named for."
  (let ((leaf (leaf-name name)))
    (if (valid-env-name-p leaf)
        (format nil "~a  env: ~a" name leaf)
        (format nil "~a  (its name is no environment variable's)" name))))

;;; --- pass_list ----------------------------------------------------------------

(defun list-entries (report)
  "pass_list: the store's entry names under one subdirectory or the
 configured ones, marked with the variable each is named for."
  (running-settings)
  (let* ((named (trim (entry-listing-subdir report)))
         (subdirs (if (plusp (length named))
                      (list (string-trim "/ " named))
                      (configured-subdirs)))
         (names (entry-names (store-root) subdirs)))
    (if (null names)
        (format nil "no entries under ~{~a~^, ~} in ~a~@[ (which does not exist: run pass init with your GPG key, or set `store' in the pass-secrets section)~]"
                subdirs (store-root) (null (probe-file (store-root))))
        (clip (format nil "~a entr~:@p under ~{~a~^, ~} (names only, no value):~%~{~a~%~}"
                      (length names) subdirs (mapcar #'marked-entry names))))))

;;; --- pass_show ----------------------------------------------------------------

(defun read-secret (report)
  "pass_show: one entry's value, the value alone."
  (running-settings)
  (show (secret-read-name report)
        :first-line (secret-read-first-line report)))

;;; --- pass_env -----------------------------------------------------------------

(defun bulk-read (arguments)
  "pass_env: the store as the environment the original injected, one
 NAME=value line per env-named entry, and the names it skipped."
  (declare (ignore arguments))
  (running-settings)
  (multiple-value-bind (pairs skipped)
      (env-values (store-root) (configured-subdirs))
    (if (null pairs)
        (format nil "no entry under ~{~a~^, ~} is named for an environment variable; pass_list shows what is there"
                (configured-subdirs))
        (clip (with-output-to-string (answer)
                (format answer "~a secret~:p as NAME=value, read from ~a (a value's own line breaks are kept):~%"
                        (length pairs) (store-root))
                (dolist (pair pairs)
                  (format answer "~a=~a~%" (car pair) (cdr pair)))
                (when skipped
                  (format answer "~%[skipped ~a entr~:@p whose name no environment could hold: ~{~a~^, ~}]"
                          (length skipped) skipped)))))))

;;; --- pass_status --------------------------------------------------------------

(defun subdir-report (root)
  "One line naming each configured subdirectory and how many entries it holds."
  (format nil "~{~a~^, ~}"
          (loop for subdir in (configured-subdirs)
                for files = (entry-files root subdir)
                collect (if (probe-file (merge-pathnames (format nil "~a/" subdir) root))
                            (format nil "~a (~a)" subdir (length files))
                            (format nil "~a (missing)" subdir)))))

(defun status-text ()
  "What pass is and what the store holds: names and counts, never a value."
  (let* ((root (store-root))
         (version (version-line))
         (names (entries))
         (named (count-if (lambda (name) (valid-env-name-p (leaf-name name))) names)))
    (with-output-to-string (answer)
      (format answer "pass: ~a~%"
              (or version
                  (format nil "~s does not run here; install pass (https://www.passwordstore.org/), or set `binary' in the pass-secrets section" (binary))))
      (format answer "store: ~a~@[ (missing: run pass init with your GPG key)~]~%" root (null (probe-file root)))
      (format answer "subdirs: ~a~%" (subdir-report root))
      (format answer "entries: ~a, of which ~a ~:*~[are~;is~:;are~] named for an environment variable~@[; ~a name~:p no environment could hold~]~%"
              (length names) named (- (length names) named)))))

(defun status (arguments)
  "pass_status: the CLI, the store, the subdirectories and their counts."
  (declare (ignore arguments))
  (running-settings)
  (status-text))

;;; --- /pass ----------------------------------------------------------------------

(defun words (text)
  "TEXT split on runs of whitespace, in order, with no shell in between."
  (let ((words '())
        (start nil))
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (if (member character '(#\Space #\Tab #\Newline #\Return))
                 (when start (push (subseq text start index) words) (setf start nil))
                 (unless start (setf start index))))
    (when start (push (subseq text start) words))
    (nreverse words)))

(defun run-pass (arguments)
  "ARGUMENTS handed to pass itself: `pass ls', `pass git', anything."
  (multiple-value-bind (out err status) (call-pass (append (list (binary)) arguments))
    (cond
      ((eq status :timeout)
       (format nil "pass did not finish in ~a s; raise `timeout' in the pass-secrets section, or read one entry at a time"
               (setting :timeout)))
      ((plusp (length out))
       (if (eql status 0)
           (clip out)
           (format nil "~a~%[pass exited ~a~@[~%~a~]]"
                   (clip out) status (unless (zerop (length err)) (clip err)))))
      (t (format nil "pass exited ~a with no output~@[: ~a~]"
                 status (unless (zerop (length err)) (clip err)))))))

(defun run-command (args session-id)
  "The /pass command: with no arguments, what the store is; with arguments,
 `pass ARGS...' itself -- ls, find, git and the rest."
  (declare (ignore session-id))
  (running-settings)
  (let ((typed (trim (or args ""))))
    (if (plusp (length typed))
        (run-pass (words typed))
        (status-text))))

;;; --- the start ------------------------------------------------------------------

(defun start ()
  "Say once, on the board, what is missing: the CLI, or the store's
 directory. A disabled section contributes nothing, as in the original."
  (let ((problem
          (cond
            ((null (version-line))
             (format nil "pass-secrets: ~s does not run here; install pass (https://www.passwordstore.org/) and run pass init with your GPG key, or set `binary' in the pass-secrets section"
                     (binary)))
            ((null (probe-file (store-root)))
             (format nil "pass-secrets: no store at ~a; run pass init with your GPG key, or set `store' in the pass-secrets section"
                     (store-root))))))
    (when problem
      (nle:notice problem :level :warning :key "nodecode-pass-secrets")
      (nle:on-stop (lambda () (nle:notice nil :key "nodecode-pass-secrets"))))))

(nle:define-cell pass-secrets
  (:section ("pass-secrets")
    (:guide "install pass (https://www.passwordstore.org/) and run pass init with your GPG key; subdirs names the store directories the tools read; nothing else is needed")
    ("binary" :string :default "pass"
              :doc "the pass executable, searched on PATH")
    ("store" :directory :default "~/.password-store"
             :doc "the password store directory, as pass init made it")
    ("subdirs" :list :default '("shared")
               :doc "the store subdirectories the tools read, for example [\"shared\", \"work\"]; keep it narrow, since pass_env reads every entry under them")
    ("timeout" :integer :default 30 :min 5
               :doc "seconds one pass call may take before its whole process tree is killed; gpg-agent may ask for a passphrase inside it")
    ("max_chars" :integer :default 20000 :min 200
                 :doc "how much of one answer the model is given"))
  (:tool "pass_list" (nlk:contract 'entry-listing) 'list-entries)
  (:tool "pass_show" (nlk:contract 'secret-read) 'read-secret)
  (:tool "pass_env"
         (no-arguments "Every entry under the configured subdirectories whose leaf name is an environment variable's, as one NAME=value line each: the whole store as the environment the original plugin injected. A plain read of every value, so prefer pass_show for one key.")
         'bulk-read)
  (:tool "pass_status"
         (no-arguments "Whether the pass binary runs here, where the store is, how many entries each configured subdirectory holds, and how many no environment could name. Reads no value.")
         'status)
  (:help :pass-secrets "pass-secrets: a local GPG-encrypted pass store, read as secrets" +primer+)
  (:command "pass" 'run-command
            :description "What the pass store holds; or run pass itself, ls and find included"
            :argument-hint "ls")
  (:start #'start))
