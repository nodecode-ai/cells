;;;; cell.lisp --- the fluent cell: /fluent, fluent_check, and the pre-call check.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original reads a program's help and man pages, then checks the
;;;; terminal commands an agent runs against them before they run. This
;;;; rewrite reads the --help of the programs the operator adopts, at most one
;;;; level of subcommands, and checks every command an eval call spawns: a
;;;; long option the program does not have is caught before the call runs,
;;;; with the closest real option when there is one.
;;;;
;;;; It judges only what it is sure of: long options, on a command whose help
;;;; was read whole, before --, and never a word the shell expands. A command
;;;; naming a subcommand it has not read is left alone.

(in-package #:nodecode-fluent)

(nlk:define-peripheral fluent :not-running t)

(defun mode ()
  "What a flagged command gets: block, warn or off."
  (or (setting :mode) "block"))

(defparameter +manual+
  "fluent checks the terminal commands you run against each program's own --help, here on this
machine, so an invented option is caught before the command runs.
  /fluent adopt docker      read docker's own --help, and its subcommands' --help (a few seconds)
  /fluent show docker run   the options of `docker run`, as read
  /fluent forget docker    forget it
  /fluent check \"docker run --rmm x\"   what the checker would say, without running anything
  /fluent status           what is read, and the mode
A check reads only the commands an eval call spawns -- (sh \"...\"), run-program, launch-program,
run-shell-command -- and only the words it can see there. It never judges short options,
positional words, subcommand names, or anything after --. In block mode a flagged call does not
run; in warn mode it runs and the flag is noted; off checks nothing.")

;;; --- the contract the model calls --------------------------------------------

(nlk:define-contract fluent-check
  (:doc "Check a shell command against the --help of the program installed here, before running it: an option the program does not have comes back with the closest one it does.")
  ("command" :text :required t
   :doc "the shell command to check, as you would run it, e.g. docker run --rmm alpine")
  ("adopt" :boolean :default nil
   :doc "read the program's --help first when it has not been read yet"))

;;; --- what one eval call spawns -----------------------------------------------

(defparameter +spawn-operators+ '("sh" "run-program" "launch-program" "run-shell-command")
  "The heads whose arguments are a command the call runs.")

(defun operator-name (x)
  "X's symbol name, lowercased, or NIL."
  (and (symbolp x) (string-downcase (symbol-name x))))

(defun dollar-name (x)
  "The strings-object name ($ \"name\") reads, or NIL."
  (and (consp x)
       (equal "$" (operator-name (car x)))
       (stringp (second x))
       (second x)))

(defun word-text (x strings)
  "X as one word, when it is a word: a string, a ($ \"name\") the call carries, or NIL."
  (cond ((stringp x) x)
        ((dollar-name x) (and strings (gethash (dollar-name x) strings)))))

(defun literal-text (x strings)
  "X as one command line, or NIL when it hides the words."
  (or (word-text x strings)
      (and (consp x)
           (member (operator-name (car x)) '("list" "quote") :test #'equal)
           (let ((items (if (equal "quote" (operator-name (car x))) (second x) (rest x))))
             (when (and items (listp items) (every (lambda (i) (word-text i strings)) items))
               (format nil "~{~a~^ ~}" (mapcar (lambda (i) (word-text i strings)) items)))))))

(defun spawn-line (call strings)
  "CALL's command line, from its literal arguments, or NIL."
  (let ((parts '()) (rest (rest call)))
    (loop while rest
          do (if (keywordp (first rest))
                 (setf rest (cddr rest))
                 (let ((text (literal-text (first rest) strings)))
                   (when text (push text parts))
                   (setf rest (rest rest)))))
    (when parts (format nil "~{~a~^ ~}" (nreverse parts)))))

(defun spawn-lines (form-text strings)
  "Every command line FORM-TEXT spawns, in the order written."
  (let ((lines '()))
    (labels ((walk (x)
               (when (consp x)
                 (when (member (operator-name (car x)) +spawn-operators+ :test #'equal)
                   (let ((line (spawn-line x strings)))
                     (when line (push line lines))))
                 (walk (car x))
                 (walk (cdr x)))))
      (when (stringp form-text)
        (ignore-errors
          (with-input-from-string (in form-text)
            (let ((*read-eval* nil) (*package* (find-package :nodecode-fluent)))
              (loop for form = (read in nil :eof)
                    until (eq form :eof)
                    do (walk form))))))
      (nreverse lines))))

(defun call-findings (form-text strings)
  "Every finding the commands FORM-TEXT spawns earn from the cards read so far."
  (loop for line in (spawn-lines form-text strings) append (check-command line)))

;;; --- saying what was found ---------------------------------------------------

(defun finding-line (finding table)
  "One finding in the model's words."
  (let* ((card (gethash (getf finding :program) table))
         (version (and card (getf card :version)))
         (suggestion (getf finding :suggestion)))
    (format nil "~a is not an option of ~a~@[ (~a)~], checked against its own --help here;~@[ did you mean ~a?~]~%"
            (getf finding :token)
            (getf finding :command)
            (and version (plusp (length version)) version)
            (and suggestion (plusp (length suggestion)) suggestion))))

(defun findings-text (findings &optional (table *cards*))
  (format nil "~{~a~}" (mapcar (lambda (f) (finding-line f table)) findings)))

;;; --- the pre-call check ------------------------------------------------------

(defun observe-call (op next)
  "The :TOOL point: a spawned command asking a program for an option its own
help does not have is refused in block mode, and noted in warn mode."
  (let ((mode (mode)))
    (if (equal mode "off")
        (funcall next op)
        (let* ((arguments (getf op :arguments))
               (form (and (hash-table-p arguments) (gethash "form" arguments)))
               (strings (and (hash-table-p arguments) (gethash "strings" arguments)))
               (text (cond ((stringp form) form)
                           ((null form) "")
                           (t (format nil "~s" form))))
               (findings (call-findings text strings)))
          (cond
            ((null findings) (funcall next op))
            ((equal mode "warn")
             (nle:notice (format nil "fluent: ~a" (trimmed (findings-text (list (first findings)))))
                         :level :warning :key "nodecode-fluent")
             (funcall next op))
            (t (nle:failure
                (format nil "ERROR: refused by nodecode-fluent: ~aThe call did not run -- fix the option and run it again, or set mode to warn in the fluent section to let a flagged command run."
                        (findings-text findings)))))))))

;;; --- /fluent -----------------------------------------------------------------

(defparameter +usage+
  "/fluent adopt <program> | show <program> [subcommand] | forget <program> | check <command> | status | mode")

(defun status-text ()
  (let ((all (cards)))
    (if (null all)
        "fluent has read no program yet. /fluent adopt <program> reads its own --help (docker, git, curl, gh ...), and from then on the commands you run are checked against it."
        (format nil "fluent has read ~d program~:p (mode ~a):~%~{~a~^~%~}"
                (length all) (mode)
                (mapcar (lambda (card)
                          (let ((version (getf card :version))
                                (subs (length (getf card :subcommands))))
                            (format nil "  ~a~@[ ~a~] -- ~d option~:p~@[, ~d subcommand~:p read~]~:[~; (not certain)~]"
                                    (getf card :program)
                                    (and version (plusp (length version)) version)
                                    (length (getf card :flags))
                                    (and (plusp subs) subs)
                                    (not (getf card :certain)))))
                        all)))))

(defun adopt-text (words)
  (if (null words)
      "Which program? /fluent adopt <program>, e.g. /fluent adopt docker."
      (multiple-value-bind (card problem)
          (adopt (first words) :max-subcommands (setting :max-subcommands))
        (if (null card)
            (format nil "fluent could not read ~a: ~a" (first words) problem)
            (format nil "fluent is now fluent in ~a~@[ ~a~]: ~d option~:p~@[, and ~d subcommand~:p~] read from its own --help."
                    (getf card :program)
                    (let ((v (getf card :version))) (and v (plusp (length v)) v))
                    (length (getf card :flags))
                    (let ((n (length (getf card :subcommands)))) (and (plusp n) n)))))))

(defun show-text (words)
  (if (null words)
      (status-text)
      (let ((card (gethash (base-name (first words)) *cards*)))
        (if (null card)
            (format nil "fluent has not read ~a yet. /fluent adopt ~a reads it." (first words) (first words))
            (let* ((sub (second words))
                   (node (and sub (find sub (getf card :subcommands) :test #'equal :key (lambda (s) (getf s :name)))))
                   (flags (if sub (and node (getf node :flags)) (getf card :flags))))
              (cond
                ((and sub (null node))
                 (format nil "~a ~a is not among the subcommands read (~{~a~^, ~})."
                         (getf card :program) sub (mapcar (lambda (s) (getf s :name)) (getf card :subcommands))))
                ((null flags) (format nil "fluent read no option of ~a~@[ ~a~]." (getf card :program) sub))
                (t (format nil "~a~@[ ~a~] (~d option~:p):~%~{~a~^~%~}"
                            (getf card :program) sub (length flags)
                            (mapcar (lambda (flag)
                                      (format nil "  ~{~a~^, ~}~@[ -- ~a~]"
                                              (getf flag :names)
                                              (let ((d (getf flag :desc)))
                                                (and d (plusp (length d)) d))))
                                    flags)))))))))

(defun forget-text (words)
  (cond ((null words) "Which program? /fluent forget <program>")
        ((forget-program (first words)) (format nil "fluent has forgotten ~a." (first words)))
        (t (format nil "fluent was not fluent in ~a." (first words)))))

(defun check-text (words)
  (if (null words)
      "Name a command: /fluent check \"docker run --rmm alpine\"."
      (let* ((command (format nil "~{~a~^ ~}" words))
             (program (multiple-value-bind (name rest) (program-of (first (split-commands command)))
                        (declare (ignore rest))
                        name)))
        (if (and program (null (gethash program *cards*)))
            (format nil "fluent has not read ~a yet: /fluent adopt ~a first." program program)
            (let ((findings (check-command command)))
              (if (null findings)
                  "fluent: every long option there is in the help of the program that takes it."
                  (format nil "fluent: ~a" (findings-text findings))))))))

(defun run-slash (args session-id)
  "/fluent: the programs read, what one's help says, and a command checked without running it."
  (declare (ignore session-id))
  (let* ((words (words-of (or args "")))
         (verb (string-downcase (or (first words) ""))))
    (cond ((member verb '("" "status") :test #'equal) (status-text))
          ((member verb '("adopt" "learn" "add") :test #'equal) (adopt-text (rest words)))
          ((member verb '("show" "card" "info") :test #'equal) (show-text (rest words)))
          ((member verb '("forget" "remove" "rm") :test #'equal) (forget-text (rest words)))
          ((equal verb "check") (check-text (rest words)))
          ((equal verb "mode")
           (format nil "fluent's mode is ~a. block refuses a command asking for an option its program does not have; warn runs it and says so; off checks nothing. Change it in the fluent section." (mode)))
          ((member verb '("help" "-h" "--help") :test #'equal) +usage+)
          (t (format nil "fluent: ~a is not one of its choices (adopt, show, forget, check, status, mode)." (first words))))))

;;; --- the tool ----------------------------------------------------------------

(defun report-findings (findings program)
  (if (null findings)
      (format nil "fluent: every long option in that command is in ~a's own --help." program)
      (format nil "fluent: ~aFix the option before running the command." (findings-text findings))))

(defun check-tool (report)
  "fluent_check: what the checker says about one command, optionally reading its program first."
  (let* ((command (fluent-check-command report))
         (adopt-p (fluent-check-adopt report))
         (program (let ((first-command (first (split-commands command))))
                    (and first-command (program-of first-command)))))
    (cond
      ((null program) "fluent: there is no program in that command to check.")
      ((gethash program *cards*) (report-findings (check-command command) program))
      (adopt-p (multiple-value-bind (card problem)
                   (adopt program :max-subcommands (setting :max-subcommands))
                 (if card
                     (report-findings (check-command command) program)
                     (format nil "fluent could not read ~a: ~a" program problem))))
      (t (format nil "fluent has not read ~a yet: ask again with adopt true, or run /fluent adopt ~a." program program)))))

;;; --- the cell ----------------------------------------------------------------

(nle:define-cell fluent
  (:section ("fluent")
    (:guide "adopt a program once with /fluent adopt <program>: it reads that program's own --help; mode block refuses a command asking for an option the program does not have, warn runs it and says so; next step: /fluent status")
    ("mode" :choice :options '("block" "warn" "off") :default "block"
     :doc "what a command with an invented option gets: refused before it runs, run with a note, or nothing")
    ("max_subcommands" :integer :default 24 :min 1
     :doc "how many subcommands one adoption reads the help of, at most")
    ("adopt_at_start" :list
     :doc "programs read at every start, so their options are known before the first command"))
  (:help :fluent "fluent: every command you run is checked against each program's own --help, and an invented option is caught before it runs" +manual+)
  (:tool "fluent_check" (nlk:contract 'fluent-check) 'check-tool)
  (:command "fluent" 'run-slash
   :description "The programs whose own --help fluent has read: adopt, show, forget, check"
   :argument-hint "adopt|show|forget|check|status"
   :session nil)
  (:hook :tool #'observe-call)
  (:start (lambda ()
            (setf *cards* (read-cards))
            (dolist (program (setting :adopt-at-start))
              (unless (gethash (base-name program) *cards*)
                (ignore-errors (adopt program :max-subcommands (setting :max-subcommands)))))
            (nle:on-stop (lambda () (setf *cards* (make-hash-table :test #'equal)))))))
