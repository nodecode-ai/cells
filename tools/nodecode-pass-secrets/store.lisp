;;;; store.lisp --- the store on disk, and the one seam to the pass CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original walked the store itself -- every *.gpg under the configured
;;;; subdirectories, sorted -- and asked `pass show' for one entry when it
;;;; needed the value. The same walk is here, so listing costs no process and
;;;; no passphrase, and one seam, *RUN*, is the only place a process starts:
;;;; a test answers it and asserts the argv.

(in-package #:nodecode-pass-secrets)

;;; --- the one seam ------------------------------------------------------------

(defparameter *run* 'run-cli
  "How every pass call reaches the CLI: (funcall *RUN* ARGV) => (values
STDOUT STDERR STATUS), STATUS an exit code or :TIMEOUT. A test rebinds it, so
nothing spawns a process and no passphrase is ever asked.")

(defun trim (text)
  "TEXT without the blank a CLI leaves at either end, or \"\" for NIL."
  (if (stringp text)
      (nlk:trimmed text)
      ""))

(defun first-line (text)
  "TEXT's first line, or NIL when it has none."
  (let ((end (position #\Newline (or text ""))))
    (if end (subseq text 0 end) text)))

(defun run-cli (argv)
  "Run ARGV -- the program first, searched on PATH -- to completion or the
section's timeout, whichever comes first => (values STDOUT STDERR STATUS)."
  (handler-case
      (nlk:run-bounded argv :seconds (setting :timeout))
    (error (condition)
      ;; A binary that is not installed is the ordinary first state of this
      ;; cell, so it is words the model can act on, not a signalled error: 127
      ;; is what a shell says for a command it cannot find.
      (values nil
              (format nil "~a could not be run: ~a" (first argv) condition)
              127))))

(defun call-pass (argv)
  "Run ARGV, answering (values OUT ERR STATUS) with both texts trimmed."
  (multiple-value-bind (out err status) (funcall *run* argv)
    (values (trim out) (trim err) status)))

(defun detail (err status)
  "What a failed call says: its stderr, or the status alone."
  (if (plusp (length (trim err)))
      (trim err)
      (format nil "exit ~a" status)))

(defun binary ()
  "The pass executable the section names, `pass' by default."
  (setting :binary))

(defun store-root ()
  "The store directory the section names, expanded to a pathname."
  (setting :store))

(defun version-line ()
  "The pass CLI's `--version' line, or NIL when it does not run here."
  (multiple-value-bind (out err status) (call-pass (list (binary) "--version"))
    (declare (ignore err))
    (and (eql status 0)
         (plusp (length out))
         (first-line out))))

(defun clip (text)
  "TEXT, cut at `max_chars' with a note saying where, so a store of many
 entries cannot bury the turn."
  (let ((limit (setting :max-chars)))
    (if (> (length text) limit)
        (format nil "~a~%[cut at ~d of ~d characters]" (subseq text 0 limit) limit (length text))
        text)))

;;; --- the walk ------------------------------------------------------------------

(defun configured-subdirs ()
  "The store subdirectories the cell reads: the section's, each without its
 slashes; `shared' when the section names none."
  (let ((configured (setting :subdirs)))
    (or (loop for subdir in configured
              for text = (string-trim "/ " (string subdir))
              when (plusp (length text)) collect text)
        (list "shared"))))

(defun entry-files (root subdir)
  "Every .gpg entry under ROOT/SUBDIR, recursively, sorted by name."
  (let ((directory (merge-pathnames (format nil "~a/" subdir) root)))
    (sort (ignore-errors (directory (merge-pathnames "**/*.gpg" directory)))
          #'string< :key #'namestring)))

(defun pass-name (file root)
  "FILE's name inside the store: its path under ROOT without .gpg, with /
 separators whatever the platform's are."
  (let* ((relative (substitute #\/ #\\ (enough-namestring file root)))
         (length (length relative)))
    (if (and (> length 4) (string-equal ".gpg" relative :start2 (- length 4)))
        (subseq relative 0 (- length 4))
        relative)))

(defun entry-names (root subdirs)
  "Every entry's pass name under SUBDIRS, in order, the first claim kept."
  (let ((seen (make-hash-table :test #'equal))
        (names '()))
    (dolist (subdir subdirs (nreverse names))
      (dolist (file (entry-files root subdir))
        (let ((name (pass-name file root)))
          (unless (gethash name seen)
            (setf (gethash name seen) t)
            (push name names)))))))

(defun entries ()
  "Every entry the section's subdirectories hold, as pass names, and no value."
  (entry-names (store-root) (configured-subdirs)))

(defun leaf-name (name)
  "The last segment of NAME: the environment variable an entry is named for."
  (subseq name (1+ (or (position #\/ name :from-end t) -1))))

;;; --- the names a tool may hand pass -----------------------------------------------

(defun valid-env-name-p (name)
  "Whether NAME can be an environment variable: A-Z, 0-9 and _, not starting
 with a digit. The original's UPPER_SNAKE_CASE rule, word for word."
  (and (stringp name)
       (plusp (length name))
       (let ((first (char name 0)))
         (and (or (char<= #\A first #\Z) (char= #\_ first))
              (every (lambda (character)
                       (or (char<= #\A character #\Z)
                           (char<= #\0 character #\9)
                           (char= #\_ character)))
                     name)))))

(defun safe-name-p (name)
  "Whether NAME is a store path a tool may hand `pass show': not empty, not
 absolute, and with no `..' segment that would leave the store."
  (and (stringp name)
       (plusp (length name))
       (not (char= #\/ (char name 0)))
       (null (search ".." name))
       (null (position (code-char 0) name))))

;;; --- one value --------------------------------------------------------------------

(defun show (name &key first-line)
  "NAME's value, through `pass show -- NAME'. The whole entry, or its first
 line alone when FIRST-LINE. Signals the cell's error when pass refuses it."
  (unless (safe-name-p name)
    (fail "~s is not a pass entry name: give the path inside the store, such as shared/OPENAI_API_KEY, with no leading slash and no .." name))
  (multiple-value-bind (out err status) (call-pass (list (binary) "show" "--" name))
    (unless (eql status 0)
      (fail "pass could not read ~s: ~a. pass_list names what the store holds." name (detail err status)))
    (let ((value (if first-line (trim (first-line out)) out)))
      (when (zerop (length value))
        (fail "pass returned an empty value for ~s." name))
      value)))

(defun secret (name)
  "NAME's value, for a form evaluated in this image: (secret \"shared/OPENAI_API_KEY\").
 The read never reaches the conversation transcript."
  (running-settings)
  (show name))

;;; --- the whole store, as the environment the original injected ------------------------

(defun env-values (root subdirs)
  "The store as (NAME . VALUE) pairs, for every entry under SUBDIRS whose leaf
 name is an environment variable's, first claim kept =>
(values PAIRS SKIPPED), SKIPPED the entries no environment could hold."
  (let ((pairs '())
        (skipped '())
        (seen (make-hash-table :test #'equal)))
    (dolist (name (entry-names root subdirs))
      (let ((env (leaf-name name)))
        (cond
          ((not (valid-env-name-p env)) (push name skipped))
          ((gethash env seen) (push name skipped))
          (t
           (setf (gethash env seen) t)
           (push (cons env (show name)) pairs)))))
    (values (nreverse pairs) (nreverse skipped))))
