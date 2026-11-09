;;;; proc.lisp --- how the cell runs a program, and where it finds one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; pandoc, pdftotext and a LaTeX engine are programs the person already
;;;; has; the cell runs them as argv and reads what they answer. Two specials
;;;; are the seam a test stubs -- *RUN-ARGV* is how one is run,
;;;; *PROGRAM-PRESENT-P* how one is looked up -- and nothing else in the cell
;;;; starts a process or reads PATH.

(in-package #:nodecode-tabella)

(defun run-argv-default (argv)
  "Run ARGV (a list, no shell) and answer its stdout, its stderr and its exit
status. A program's own words, never a signal, whatever it says: a document
that will not convert is a refusal the caller formats, not an error thrown
from inside a parser."
  (multiple-value-bind (out err code)
      (uiop:run-program argv :output :string :error-output :string
                             :ignore-error-status t)
    (values (or out "") (or err "") (or code 0))))

(defun directory-path (text)
  "TEXT, a directory, as a directory pathname, or NIL."
  (ignore-errors (uiop:ensure-directory-pathname (if (plusp (length text)) text "./"))))

(defun find-program (name)
  "The absolute namestring of the executable NAME names, or NIL. A name with
a separator in it is a path and probed as one; anything else is searched for
in PATH, with .exe tried beside it."
  (when (and (stringp name) (plusp (length (nlk:trimmed name))))
    (let ((name (nlk:trimmed name)))
      (if (or (find #\/ name) (find #\\ name))
          (let ((probe (ignore-errors (probe-file (nlk:expand-home name)))))
            (and probe (namestring probe)))
          (loop with separator = (if (uiop:os-windows-p) #\; #\:)
                for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                               :separator (list separator))
                for directory = (and (plusp (length dir)) (directory-path dir))
                when directory
                  do (loop for candidate in (list name (concatenate 'string name ".exe"))
                           for file = (ignore-errors (probe-file (merge-pathnames candidate directory)))
                           when file do (return-from find-program (namestring file)))
                finally (return nil))))))

(defvar *run-argv* #'run-argv-default
  "How the cell runs one program: a function of ARGV answering (values
STDOUT STDERR EXIT). A test binds it; nothing else rebinds it.")

(defvar *program-present-p* #'find-program
  "How the cell looks a program up: a function of a name answering its
namestring or NIL. A test binds it; nothing else rebinds it.")

(defun run! (argv)
  "Run ARGV through *RUN-ARGV*: (values STDOUT STDERR EXIT)."
  (funcall *run-argv* argv))

(defun have-program-p (name)
  "NAME's path when this machine has that program, else NIL."
  (and name (funcall *program-present-p* name)))

(defun temp-file (suffix)
  "A path under the temporary directory that no file holds yet, ending SUFFIX."
  (loop repeat 100
        for path = (merge-pathnames (format nil "tabella-~d-~d~a"
                                            (get-universal-time) (random 1000000) suffix)
                                    (uiop:temporary-directory))
        unless (probe-file path) return path
        finally (fail "no free name in the temporary directory")))

(defun write-text (path text)
  "TEXT written whole to PATH, its folder made first. => PATH's namestring."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
    (write-string text out))
  (namestring path))

(defun mentioned-error (stderr)
  "A program's own last words for a failure, one clipped line, or NIL."
  (let ((text (and stderr (nlk:trimmed stderr))))
    (and text (plusp (length text)) (nlk:clip (nlk:one-line text) 300))))
