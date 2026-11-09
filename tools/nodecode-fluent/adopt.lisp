;;;; adopt.lisp --- reading a program's own help, and keeping what it said.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Adoption runs the program itself, twice at most plus once per subcommand:
;;;; `prog --version' and `prog --help'. That is the whole reach of this cell:
;;;; no shell, no pipeline, the output capped by a timeout, and nothing read
;;;; that the program does not print about itself. The card is plain Lisp data
;;;; in this home's fluent folder, one file per program, and the checker never
;;;; touches the disk: it reads the table START-CELL filled.

(in-package #:nodecode-fluent)

(defparameter *store-dir* nil
  "Where cards are kept; NIL is this home's fluent folder.")

(defparameter *help-timeout* 10
  "Seconds one help run may take before it is killed.")

(defun store-dir ()
  (or *store-dir* (nlk:home "fluent/")))

;;; --- running one program -----------------------------------------------------

(defun run-argv (argv)
  "Run ARGV directly, its output to one temporary file. => (values TEXT EXIT-CODE)."
  (let* ((out (merge-pathnames (format nil "fluent-~36r.txt" (random most-positive-fixnum))
                                (uiop:temporary-directory)))
         (process (ignore-errors
                    (sb-ext:run-program (first argv) (rest argv)
                                        :search t :output out :error out :input nil
                                        :wait nil :if-does-not-exist nil))))
    (unless process
      (return-from run-argv (values "" 127)))
    (loop repeat (* 10 *help-timeout*)
          while (sb-ext:process-alive-p process)
          do (sleep 0.1))
    (when (sb-ext:process-alive-p process)
      (ignore-errors (sb-ext:process-kill process 15))
      (sleep 0.2)
      (when (sb-ext:process-alive-p process)
        (ignore-errors (sb-ext:process-kill process 9))))
    (let ((code (or (ignore-errors (sb-ext:process-exit-code process)) -1))
          (text (if (probe-file out) (uiop:read-file-string out) "")))
      (ignore-errors (delete-file out))
      (values text code))))

(defparameter *runner* #'run-argv
  "How one program is run: (lambda (argv) (values OUTPUT EXIT-CODE)). The tests bind it.")

(defun ask-runner (argv)
  (handler-case (funcall *runner* argv) (error () (values "" 127))))

(defun program-version (program)
  "PROGRAM's own first version line, one short line, or the empty string."
  (multiple-value-bind (text code) (ask-runner (list program "--version"))
    (when (and (plusp (length text)) (zerop code))
      (let* ((line (first (remove-if #'blankp (split-lines text))))
             (line (trimmed (or line ""))))
        (if (> (length line) 60) (subseq line 0 60) line)))))

;;; --- adopting ----------------------------------------------------------------

(defun help-answers-options-p (text root)
  "May a subcommand's help be asked for? Only when the root help says --help
is answered before anything runs: an option named --help, or the usual pointer."
  (or (flag-named (getf root :flags) "--help")
      (not (null (search "for more information" (string-downcase text))))))

(defun subcommand-cards (program subcommands max)
  "The first MAX of SUBCOMMANDS, each with the flags its own --help names."
  (loop for entry in subcommands
        for index from 0 below max
        collect (let ((help (ask-runner (list program (getf entry :name) "--help"))))
                  (let ((parsed (ignore-errors (parse-help help))))
                    (list :name (getf entry :name)
                          :summary (getf entry :summary)
                          :flags (and parsed (getf parsed :flags))
                          :certain (and parsed (getf parsed :certain)))))))

(defun adopt (program &key (max-subcommands 24))
  "Read PROGRAM's own help into a card and keep it.
=> (values CARD PROBLEM): the card kept, or NIL and one line saying why."
  (let ((name (base-name (or program ""))))
    (cond
      ((zerop (length name)) (values nil "Name a program to adopt, e.g. /fluent adopt docker."))
      (t (multiple-value-bind (text code) (ask-runner (list name "--help"))
           (declare (ignore code))
           (if (or (null text) (blankp-with-newlines text))
               (values nil (format nil "~a printed no help here: no such program, or it wants an argument." name))
               (let* ((root (parse-help text))
                      (certain (getf root :certain))
                      (subcommands (if (and certain (help-answers-options-p text root))
                                       (subcommand-cards name (getf root :subcommands) max-subcommands)
                                       '()))
                      (card (list :program name
                                  :version (program-version name)
                                  :flags (getf root :flags)
                                  :subcommands subcommands
                                  :certain certain
                                  :read-at (get-universal-time))))
                 (setf (gethash name *cards*) card)
                 (keep-card card)
                 (values card nil))))))))

(defun blankp-with-newlines (text)
  (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) text))

;;; --- keeping cards -----------------------------------------------------------

(defun card-path (program)
  (merge-pathnames (format nil "~a.card" program) (store-dir)))

(defun keep-card (card)
  "Write CARD beside the others, in one piece."
  (let ((path (card-path (getf card :program))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (let ((*print-readably* t) (*print-pretty* t) (*print-length* nil) (*print-level* nil))
        (prin1 card out)
        (terpri out)))
    path))

(defun read-cards (&optional (dir (store-dir)))
  "Every card kept under DIR, as a hash table by program name."
  (let ((cards (make-hash-table :test #'equal)))
    (when (uiop:directory-exists-p dir)
      (dolist (file (uiop:directory-files dir "*.card"))
        (let ((card (handler-case
                        (with-open-file (in file)
                          (let ((*read-eval* nil)) (read in nil nil)))
                      (error () nil))))
          (when (and (consp card) (stringp (getf card :program)))
            (setf (gethash (getf card :program) cards) card)))))
    cards))

(defun cards (&optional (table *cards*))
  "Every card read so far, by program name."
  (sort (loop for card being the hash-values of table collect card)
        #'string< :key (lambda (card) (getf card :program))))

(defun forget-program (program)
  "Forget PROGRAM: its file, and the card the checker reads. => T when there was one."
  (let* ((name (base-name (or program "")))
         (path (card-path name))
         (had (or (gethash name *cards*) (probe-file path))))
    (remhash name *cards*)
    (when (probe-file path) (ignore-errors (delete-file path)))
    (and had t)))

(defun forget-all ()
  "Forget every program read here."
  (dolist (card (cards)) (forget-program (getf card :program)))
  (clrhash *cards*))
