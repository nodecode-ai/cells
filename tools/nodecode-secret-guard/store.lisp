;;;; store.lisp --- the private store, a value's fingerprint, and the scrub.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; WHAT THE ORIGINAL KEPT WHERE. Hermes wrote each key into the macOS
;;;; keychain (service hermes-<NAME>, account <NAME>) and kept a registry of
;;;; names, scopes and sha256 fingerprints beside it, mode 600, so nothing but
;;;; a fingerprint was ever on disk. A Nodecode home may be anywhere and an OS
;;;; keychain is not one API on Linux, macOS and Windows, so the value itself
;;;; lives in one file under the Nodecode home (or at the path the section
;;;; names), written mode 600 and replaced atomically. The registry the
;;;; original kept beside its keychain is the same file's other members: what a
;;;; name answers is still a fingerprint and a length, never a value.

(in-package #:nodecode-secret-guard)

(defparameter +fingerprint-length+ 16
  "How much of the sha256 a name answers with, as the original did: what two
people can compare over a channel, and nothing a value comes back from.")

(defparameter +min-scrub-length+ 6
  "A stored value shorter than this is kept but never scrubbed: six characters
of a value turn up in unrelated text and would wreck every tool result.")

(defparameter +variable-name+ "^[A-Za-z_][A-Za-z0-9_]*$"
  "What a secret may be called: a name a shell would take for a variable, so a
stored secret can be the environment variable a tool reads.")

(defstruct (held-secret (:constructor make-held-secret
                                      (&key name value source fingerprint length created)))
  "One stored secret: its NAME, its VALUE, where it came from, and what is
said about it (FINGERPRINT, LENGTH, CREATED) without the value."
  name value source fingerprint length created)

(defvar *store-path* nil
  "The file the secrets are kept in, or NIL while the cell is not running.")
(defvar *secrets* (make-hash-table :test #'equal)
  "NAME -> the HELD-SECRET. Lives as long as the cell runs.")
(defvar *exported* '()
  "The names this start put into the environment, so stop can take them out.")

;;; --- what a name answers with --------------------------------------------------

(defun hex-string (octets)
  (with-output-to-string (out)
    (loop for byte across octets do (format out "~2,'0x" byte))))

(defun fingerprint (value)
  "The first +FINGERPRINT-LENGTH+ hex digits of VALUE's sha256, as the original read it."
  (subseq (hex-string (ironclad:digest-sequence
                        :sha256 (sb-ext:string-to-octets value :external-format :utf-8)))
          0 +fingerprint-length+))

(defun utc-stamp ()
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun hold (name value source)
  "A HELD-SECRET for NAME, fingerprinted now."
  (make-held-secret :name name :value value :source source
                    :fingerprint (fingerprint value) :length (length value)
                    :created (utc-stamp)))

(defun held (name)
  "The secret called NAME, or NIL."
  (gethash (string name) *secrets*))

(defun secret-names ()
  "Every stored name, sorted."
  (sort (loop for name being the hash-keys of *secrets* collect name) #'string<))

(defun secret (name)
  "The value of the secret called NAME, for a form that uses it -- never for a
form that shows it, since the result is scrubbed on its way back.
(secret \"OPENAI_API_KEY\") in an eval, or $OPENAI_API_KEY in a shell command
when the section's export_env is on. Signals when there is no such secret."
  (let ((entry (held name)))
    (if entry
        (held-secret-value entry)
        (error "no secret named ~a is stored: /secret add ~a ENV_OR_FILE stores one, ~
                and /secret lists the names" name name))))

;;; --- the file ------------------------------------------------------------------

(defun store-path ()
  "The store's pathname: the section's store_file, else a private file under
the Nodecode home."
  (let ((named (nlk:trimmed (or (setting :store-file) ""))))
    (if (plusp (length named))
        (nlk:expand-home named)
        (nlk:home "secret-guard/secrets.json"))))

(defun read-text-file (path)
  "PATH's bytes as a string, or NIL when there is no such file."
  (when (probe-file path)
    (ignore-errors (uiop:read-file-string path))))

(defun stored-entry (name data)
  "NAME's HELD-SECRET out of one member of the store file's `secrets' object,
or NIL when the member carries no value."
  (let ((value (and (hash-table-p data) (gethash "value" data))))
    (when (stringp value)
      (make-held-secret
       :name name :value value
       :source (let ((source (and data (gethash "source" data))))
                 (and (stringp source) source))
       ;; a file edited by hand keeps the registry's word only until
       ;; /secret verify reads the value again
       :fingerprint (let ((fingerprint (and data (gethash "fingerprint" data))))
                      (if (stringp fingerprint) fingerprint (fingerprint value)))
       :length (length value)
       :created (let ((created (and data (gethash "created" data))))
                  (and (stringp created) created))))))

(defun load-store (path)
  "PATH as the store: NAME -> HELD-SECRET. A missing, empty or unreadable file
is an empty store -- the cell starts on a fresh home."
  (let ((table (make-hash-table :test #'equal))
        (text (read-text-file path)))
    (when (and text (plusp (length (nlk:trimmed text))))
      (let* ((root (ignore-errors (nlk:decode-json text)))
             (secrets (and (hash-table-p root) (gethash "secrets" root))))
        (when (hash-table-p secrets)
          (maphash (lambda (name data)
                     (let ((entry (stored-entry name data)))
                         (when entry (setf (gethash name table) entry))))
                   secrets))))
    table))

(defun store-json ()
  "The store as the object written to disk: every name's value, source,
fingerprint, length and date."
  (let ((secrets (make-hash-table :test #'equal)))
    (dolist (name (secret-names))
      (let ((entry (held name)))
        (setf (gethash name secrets)
              (nlk:json-object "value" (held-secret-value entry)
                               "source" (or (held-secret-source entry) "")
                               "fingerprint" (held-secret-fingerprint entry)
                               "length" (held-secret-length entry)
                               "created" (or (held-secret-created entry) "")))))
    (nlk:json-object "version" 1 "secrets" secrets)))

(defun save-store ()
  "Write *SECRETS* to the store's path, mode 600, replaced atomically.
=> the path."
  (let* ((path (or *store-path* (store-path)))
         (temp (pathname (format nil "~a.tmp" (uiop:native-namestring path))))
         (directory (uiop:pathname-directory-pathname path)))
    (ensure-directories-exist path)
    #-win32 (ignore-errors (sb-posix:chmod (uiop:native-namestring directory) #o700))
    (with-open-file (out temp :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
      (write-string (nlk:encode-json-object (store-json)) out))
    #-win32 (ignore-errors (sb-posix:chmod (uiop:native-namestring temp) #o600))
    (uiop:rename-file-overwriting-target temp path)
    #-win32 (ignore-errors (sb-posix:chmod (uiop:native-namestring path) #o600))
    path))

;;; --- the process environment --------------------------------------------------------

(defun put-env (name value)
  #+win32 (setf (uiop:getenv name) value)
  #-win32 (sb-posix:setenv name value 1))

(defun drop-env (name)
  #+win32 (setf (uiop:getenv name) nil)
  #-win32 (ignore-errors (sb-posix:unsetenv name)))

(defun export-one (name value)
  "Put VALUE into this process's environment as NAME, when the name is a
variable's and the variable holds nothing yet."
  (when (and (plusp (length name))
             (cl-ppcre:scan +variable-name+ name)
             (null (uiop:getenv name)))
    (put-env name value)
    (pushnew name *exported* :test #'string=)))

(defun export-all ()
  (maphash (lambda (name entry) (export-one name (held-secret-value entry))) *secrets*))

(defun unexport-all ()
  (dolist (name *exported*) (drop-env name))
  (setf *exported* '()))

;;; --- registering, forgetting, verifying ---------------------------------------------

(defun register-secret (name value &optional source)
  "Keep VALUE under NAME, write the store, and answer the entry.
=> (values ENTRY REPLACED-P)."
  (let* ((name (string name))
         (replaced (and (held name) t))
         (entry (hold name value source)))
    (setf (gethash name *secrets*) entry)
    (when *store-path* (save-store))
    (when *store-path*                       ; a running cell, not a bare call
      (cond ((exported-p name)                ; a rotation reaches what it made
             (ignore-errors (put-env name value)))
            ((setting :export-env) (export-one name value))))
    (values entry replaced)))

(defun exported-p (name)
  (and (member name *exported* :test #'string=) t))

(defun forget-secret (name)
  "Drop NAME from the store and from the environment this start made.
=> the entry that was dropped, or NIL."
  (let ((entry (held name)))
    (when entry
      (let ((name (string name)))
        (remhash name *secrets*)
        (when (exported-p name)
          (drop-env name)
          (setf *exported* (remove name *exported* :test #'string=)))
        (when *store-path* (save-store))))
    entry))

(defun verify-entry (entry)
  "Whether ENTRY's value still fingerprints to what the store says about it.
=> (values OK-P ACTUAL)."
  (let ((actual (fingerprint (held-secret-value entry))))
    (values (equal actual (held-secret-fingerprint entry)) actual)))

;;; --- the scrub -------------------------------------------------------------------

(defun scrub-pairs ()
  "The stored secrets worth scrubbing, longest value first: a shorter value
that is a piece of a longer one must not be replaced first."
  (sort (loop for entry being the hash-values of *secrets*
              when (>= (length (held-secret-value entry)) +min-scrub-length+)
                collect entry)
        #'> :key (lambda (entry) (length (held-secret-value entry)))))

(defun replace-literal (text needle replacement)
  "TEXT with every literal occurrence of NEEDLE replaced by REPLACEMENT.
=> (values TEXT COUNT)."
  (let ((out (make-string-output-stream)) (start 0) (count 0))
    (loop for at = (search needle text :start2 start)
          while at
          do (write-string text out :start start :end at)
             (write-string replacement out)
             (incf count)
             (setf start (+ at (length needle))))
    (write-string text out :start start)
    (values (get-output-stream-string out) count)))

(defun redact-text (text)
  "TEXT with every stored value replaced by [secret:NAME redacted].
=> (values TEXT HITS). The original's own marker, its own order."
  (if (or (not (stringp text)) (zerop (hash-table-count *secrets*)))
      (values text 0)
      (let ((hits 0))
        (dolist (entry (scrub-pairs))
          (multiple-value-bind (fresh count)
              (replace-literal text (held-secret-value entry)
                               (format nil "[secret:~a redacted]" (held-secret-name entry)))
            (setf text fresh)
            (incf hits count)))
        (values text hits))))
