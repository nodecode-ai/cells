;;;; actions.lisp --- what a Nodecode call intends to change.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The port of the plugin's actions.py, read against this harness's one agent
;;;; tool: a call is an eval form, so its changes arrive as the Lisp a form runs
;;;; -- a (sh "rm -rf x") among them -- and, for a cell's own tool, as the path
;;;; arguments beside its name. A shell command is scanned in one bounded pass
;;;; over its head (4096 characters) and cut into the commands it runs; inline
;;;; interpreter code is not parsed, which is the original's disclosed limit and
;;;; why the fingerprint snapshot is the second line of defence.

(in-package #:nodecode-oathkeeper)

(defparameter +scan-limit+ 4096
  "How much of one command a scan reads.")

(defparameter +delete-commands+ '("rm" "rmdir" "unlink" "shred"))
(defparameter +modify-commands+ '("mv" "truncate" "chmod" "chown" "touch" "tee"))
(defparameter +create-commands+ '("cp" "mkdir" "install" "ln"))
(defparameter +editor-commands+ '("sed" "perl" "ed" "ex" "vim" "nvim" "nano" "code"))
(defparameter +backup-commands+ '("cp" "rsync" "tar" "zip" "7z" "scp")
  "A command that copies or archives a path away: the evidence a require rule accepts.")

(defparameter +spawn-operators+ '("sh" "run-program" "launch-program" "run-shell-command")
  "The heads of a form whose argument is a shell command.")

(defparameter +lisp-delete-verbs+ '("delete-file" "delete-directory" "unlink") "...")
(defparameter +lisp-modify-verbs+ '("write-file" "write-file-text" "edit") "...")
(defparameter +lisp-move-verbs+ '("rename-file") "...")
(defparameter +lisp-copy-verbs+ '("copy-file") "...")
(defparameter +path-keys+
  '("path" "file_path" "filepath" "target_path" "destination" "dest" "filename" "file")
  "The argument names a structured tool carries a path in.")
(defparameter +tool-verbs+
  '(;; fragment of a tool name, the verb it intends
    ("write" . "modify") ("create" . "create") ("patch" . "modify") ("edit" . "modify")
    ("delete" . "delete") ("remove" . "delete") ("move" . "modify") ("rename" . "modify")
    ("copy" . "create"))
  "A tool whose name says it mutates, and the verb it means.")

;;; --- tokens and a command's own words --------------------------------------

(defun shell-tokens (segment)
  "SEGMENT as the words a shell would read: quotes group, a backslash escapes."
  (let ((tokens '()) (word (make-string-output-stream)) (have nil) (quote nil) (index 0))
    (labels ((finish () (when have (push (get-output-stream-string word) tokens) (setf have nil))))
      (loop while (< index (length segment))
            do (let ((character (char segment index)))
                 (cond
                   (quote
                    (cond ((char= character quote) (setf quote nil))
                          ((and (char= character #\\) (< (1+ index) (length segment)))
                           (write-char (char segment (1+ index)) word) (incf index))
                          (t (write-char character word)))
                    (incf index) (setf have t))
                   ((or (char= character #\") (char= character #\'))
                    (setf quote character) (incf index) (setf have t))
                   ((find character '(#\Space #\Tab #\Return))
                    (finish) (incf index))
                   ((char= character #\\)
                    (when (< (1+ index) (length segment))
                      (write-char (char segment (1+ index)) word) (setf have t))
                    (incf index 2))
                   (t (write-char character word) (setf have t) (incf index)))))
      (finish))
    (nreverse tokens)))

(defun command-segments (text)
  "TEXT's commands, one per element: an unquoted ; | && || or newline starts the next."
  (let ((segments '()) (start 0) (index 0) (quote nil))
    (loop while (< index (length text))
          do (let ((character (char text index)))
               (cond
                 (quote (when (char= character quote) (setf quote nil)) (incf index))
                 ((or (char= character #\") (char= character #\')) (setf quote character) (incf index))
                 ((char= character #\\) (incf index 2))
                 ((find character '(#\; #\& #\| #\Newline))
                  (push (subseq text start index) segments) (setf start (1+ index)) (incf index))
                 (t (incf index)))))
    (push (subseq text start) segments)
    (remove-if (lambda (segment) (zerop (length (trim-blanks segment))))
               (mapcar #'trim-blanks (nreverse segments)))))

(defun path-like-p (token)
  "Does TOKEN name a path, rather than a flag, a device or a glob?"
  (and (plusp (length token))
       (not (member token '("-" "/dev/null" "/dev/stdout" "/dev/stderr") :test #'string=))
       (not (some (lambda (character) (find character token)) "*?"))
       (or (find #\/ token) (find #\. token) (char= (char token 0) #\~))))

(defun operands-of (tokens &key (skip-values '()))
  "TOKENS' non-flag operands, each flag named in SKIP-VALUES taking the word after it."
  (let ((out '()) (rest tokens))
    (loop while rest
          do (let ((token (first rest)))
               (cond ((member token skip-values :test #'string=) (setf rest (cddr rest)))
                     ((char= (char token 0) #\-) (setf rest (cdr rest)))
                     (t (push token out) (setf rest (cdr rest))))))
    (nreverse out)))

(defun command-word (tokens)
  "TOKENS' command, a path to it read as its own name."
  (let ((head (first tokens)))
    (and head (subseq head (1+ (or (position #\/ head :from-end t) -1))))))

;;; --- one shell command's actions -------------------------------------------

(defun command-actions (command)
  "The state-changing actions COMMAND intends, as (:verb :target :source) plists."
  (let ((actions '()) (text (subseq command 0 (min (length command) +scan-limit+))))
    (labels
      ((add (verb target)
         (let ((target (trim-blanks (string-trim "'\"" target))))
           (when (path-like-p target)
             (let ((entry (list :verb verb :target (normalize-path target) :source "terminal")))
               (unless (member entry actions :test #'equal) (push entry actions)))))))
      ;; An output redirection writes the file it names; a digit before it is a
      ;; descriptor, and >& a duplication of one.
      (loop for at = (position #\> text)
              then (position #\> text :start (1+ at))
            while at
            do (let ((before (and (plusp at) (char text (1- at))))
                     (after (and (< (1+ at) (length text)) (char text (1+ at)))))
                 (unless (or (and before (digit-char-p before)) (eql after #\&))
                   (let* ((rest (string-left-trim '(#\Space #\Tab) (subseq text (1+ at))))
                          (end (or (position-if (lambda (character)
                                                  (find character '(#\Space #\Tab #\; #\| #\& #\Newline)))
                                                rest)
                                   (length rest))))
                     (add "modify" (subseq rest 0 end))))))
      (when (some (lambda (segment)
                    (multiple-value-bind (tokens found)
                        (let ((tokens (shell-tokens segment)))
                          (values tokens (and tokens (string= "git" (command-word tokens)))))
                      (declare (ignore tokens))
                      (and found
                           (some (lambda (token)
                                   (member token '("checkout" "reset" "clean" "rm" "mv")
                                           :test #'string=))
                                 (shell-tokens segment)))))
                  (command-segments text))
        (add "modify" "."))
      (dolist (segment (command-segments text))
        (let* ((tokens (shell-tokens segment)) (word (command-word tokens)) (rest (rest tokens)))
          (cond
            ((null word) nil)
            ((member word +delete-commands+ :test #'string=)
             (dolist (target (operands-of rest)) (add "delete" target)))
            ((string= word "mv")
             (let ((operands (operands-of rest :skip-values '("-t"))))
               (dolist (target (butlast operands)) (add "modify" target))
               (when operands (add "create" (car (last operands))))))
            ((string= word "cp")
             (dolist (target (operands-of rest :skip-values '("-t"))) (add "create" target)))
            ((member word +modify-commands+ :test #'string=)
             (dolist (target (operands-of rest :skip-values '("-t"))) (add "modify" target))
             (when (string= word "tee")
               (dolist (target (operands-of rest :skip-values '("-t"))) (add "create" target))))
            ((member word +create-commands+ :test #'string=)
             (dolist (target (operands-of rest :skip-values '("-t" "-m" "-o" "-g")))
               (add "create" target)))
            ((member word +editor-commands+ :test #'string=)
             (let ((in-place (some (lambda (token)
                                     (or (string= token "--in-place")
                                         (and (prefixed-p "-i" token) (not (string= token "-i")))
                                         (string= token "-i")))
                                   rest)))
               (when (or in-place (member word '("ed" "ex" "vim" "nvim" "nano" "code")
                                          :test #'string=))
                 (dolist (target (operands-of rest :skip-values '("-e" "-f"))) (add "modify" target)))))
            ((string= word "dd")
             (dolist (token rest)
               (when (prefixed-p "of=" token) (add "modify" (subseq token 3)))))
            (t nil))))
    (nreverse actions))))

;;; --- what a form runs -------------------------------------------------------

(defun operator-name (value)
  "VALUE's symbol name lowercased, or NIL: a form read in any package."
  (and (symbolp value) (string-downcase (symbol-name value))))

(defun dollar-name (value)
  "The strings-object name a ($ \"name\") form reads, or NIL."
  (and (consp value)
       (equal "$" (operator-name (car value)))
       (stringp (second value))
       (second value)))

(defun strings-value (name strings)
  "The text STRINGS carries under NAME, or NIL."
  (let ((value (and strings (hash-table-p strings) (gethash name strings))))
    (and (stringp value) value)))

(defun literal-command (value strings)
  "The command text VALUE alone names, or NIL when it hides one."
  (cond ((stringp value) value)
        ((dollar-name value) (strings-value (dollar-name value) strings))
        ((and (consp value) (member (operator-name (car value)) '("list" "quote") :test #'string=))
         (let ((items (if (equal "quote" (operator-name (car value))) (second value) (rest value))))
           (when (and (listp items) (every (lambda (item) (or (stringp item) (dollar-name item))) items))
             (format nil "~{~a~^ ~}"
                     (mapcar (lambda (item) (or (dollar-name item)
                                                (strings-value item strings)
                                                item))
                             items)))))
        (t nil)))

(defun spawn-call-commands (call strings)
  "CALL's command texts, and whether one of its arguments hides a command."
  (let ((rest (rest call)) (texts '()) (hidden nil))
    (loop while rest
          do (if (keywordp (first rest))
                 (setf rest (cddr rest))
                 (let ((text (literal-command (first rest) strings)))
                   (cond (text (push text texts))
                         (t (setf hidden t)))
                   (setf rest (cdr rest)))))
    (values (nreverse texts) hidden)))

(defun read-forms (text)
  "TEXT's forms, or none when it will not read: a reader error is not a call."
  (let ((forms '()))
    (ignore-errors
      (with-input-from-string (in text)
        (let ((*read-eval* nil) (*package* (find-package :cl-user)))
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (push form forms)))))
    (nreverse forms)))

(defun literal-args (args strings)
  "The paths ARGS names directly, a ($ \"name\") resolved through STRINGS."
  (let ((out '()))
    (dolist (arg args)
      (cond ((stringp arg) (push arg out))
            ((dollar-name arg) (let ((text (strings-value (dollar-name arg) strings)))
                                 (when text (push text out))))))
    (nreverse out)))

(defun form-actions (form-text strings)
  "Every state-changing action the FORM-TEXT a call would run intends."
  (let ((actions '()))
    (labels
      ((add (verb target)
         (let ((target (trim-blanks target)))
           (when (path-like-p target)
             (let ((entry (list :verb verb :target (normalize-path target) :source "terminal")))
               (unless (member entry actions :test #'equal) (push entry actions))))))
       (add-first (verb args)
         (let ((target (first (literal-args args strings))))
           (when target (add verb target))))
       (walk (form)
         (cond
           ((consp form)
            (let ((head (operator-name (car form))))
              (cond
                ((member head +spawn-operators+ :test #'string=)
                 (multiple-value-bind (commands hidden) (spawn-call-commands form strings)
                   (if hidden
                       ;; A command built at run time is read loosely, the whole
                       ;; command's words as if they were the command.
                       (dolist (action (command-actions
                                        (format nil "~{~a~^ ~}"
                                                (literal-args (rest form) strings))))
                         (push action actions))
                       (dolist (command commands)
                         (dolist (action (command-actions command))
                           (unless (member action actions :test #'equal) (push action actions)))))))
                ((member head +lisp-delete-verbs+ :test #'string=) (add-first "delete" (rest form)))
                ((member head +lisp-modify-verbs+ :test #'string=) (add-first "modify" (rest form)))
                ((member head +lisp-move-verbs+ :test #'string=)
                 (let ((paths (literal-args (rest form) strings)))
                   (when paths (add "modify" (first paths)))
                   (when (second paths) (add "create" (second paths)))))
                ((member head +lisp-copy-verbs+ :test #'string=)
                 (let ((paths (literal-args (rest form) strings)))
                   (when paths (add "modify" (first paths)))
                   (when (second paths) (add "create" (second paths)))))
                (t nil)))
            (walk (car form))
            (walk (cdr form)))
           (t nil))))
      (dolist (form (read-forms form-text))
        (walk form)))
    (nreverse actions)))

;;; --- a structured tool's arguments -----------------------------------------

(defun tool-verb (name)
  "The verb a tool NAME intends, or NIL when its name says it only reads."
  (let ((lower (string-downcase (or name ""))))
    (cdr (find-if (lambda (pair) (search (car pair) lower)) +tool-verbs+))))

(defun argument-string (arguments key)
  "ARGUMENTS' member KEY when it is a non-empty string."
  (cond ((hash-table-p arguments) (let ((value (gethash key arguments)))
                                      (and (stringp value) (plusp (length value)) value)))
        ((listp arguments) (let ((value (getf arguments (intern key :keyword))))
                             (and (stringp value) (plusp (length value)) value)))
        (t nil)))

(defun action-arguments (arguments)
  "Every string ARGUMENTS carries under a path-key name."
  (let ((out '()))
    (dolist (key +path-keys+)
      (let ((value (argument-string arguments key)))
        (when value (push value out))))
    (nreverse out)))

(defparameter +form-tools+ '("eval" "terminal" "shell" "bash" "sh")
  "The tools whose arguments are a form or a command this cell reads.")

(defun call-form (tool-name arguments)
  "The form or command text a call carries, when its tool is one that runs one."
  (when (member (string-downcase (or tool-name "")) +form-tools+ :test #'string=)
    (or (argument-string arguments "form")
        (argument-string arguments "command")
        (argument-string arguments "cmd"))))

(defun call-strings (arguments)
  "The strings object a call carries, when it carries one."
  (let ((value (and (hash-table-p arguments) (gethash "strings" arguments))))
    (and (hash-table-p value) value)))

(defun extract-actions (tool-name arguments)
  "All state-changing actions a call intends, as (:verb :target :source) plists."
  (let ((form (call-form tool-name arguments)))
    (if form
        (form-actions form (call-strings arguments))
      (let ((verb (tool-verb tool-name)) (targets (action-arguments arguments)))
        (when (and verb targets)
          (mapcar (lambda (target)
                    (list :verb verb :target (normalize-path target) :source tool-name))
                  targets))))))

(defun backup-targets (tool-name arguments)
  "The paths a call copies or archives away: evidence a require rule accepts."
  (let ((targets '()) (form (call-form tool-name arguments)))
    (labels ((note (command)
               (dolist (segment (command-segments command))
                 (let* ((tokens (shell-tokens segment)) (word (command-word tokens)))
                   (when (and word (member word +backup-commands+ :test #'string=))
                     (let* ((operands (operands-of (rest tokens) :skip-values '("-t")))
                            (sources (if (> (length operands) 1) (butlast operands) operands)))
                       (dolist (source sources)
                         (when (path-like-p source)
                           (pushnew (normalize-path source) targets :test #'string=)))))))))
      (cond
        (form (dolist (one (read-forms form))
                (let ((all '()))
                  (labels ((walk (x)
                             (when (consp x)
                               (when (member (operator-name (car x)) +spawn-operators+
                                             :test #'string=)
                                 (multiple-value-bind (commands hidden)
                                     (spawn-call-commands x (call-strings arguments))
                                   (declare (ignore hidden))
                                   (dolist (command commands) (push command all))))
                               (walk (car x)) (walk (cdr x)))))
                    (walk one))
                  (dolist (command (nreverse all)) (note command)))))   
        (t nil)))
    (nreverse targets)))
