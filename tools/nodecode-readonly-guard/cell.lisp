;;;; cell.lisp --- per-session read-only, plan and writable modes, enforced in code.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of mciocchi/hermes-readonly-guard (MIT). The Hermes
;;;; plugin spreads the mode over one pre_tool_call hook and one command;
;;;; Nodecode is a mono-tool organism whose EVAL form carries the writes and
;;;; the commands alike, so the same shape here is three seams:
;;;;
;;;;   - one NLE:HOOK on the :TOOL point, which in read-only mode refuses
;;;;     every call that is not an inspection, and in plan mode refuses
;;;;     everything but the plan markdown under the plans directory. A call is
;;;;     READ, not grepped: the form's operators are checked against a list of
;;;;     readers, and a (sh ...) call's command against the read-only programs;
;;;;   - one /readonly command, the operator's side of the switch (the agent
;;;;     has no tool that can turn its own guard off);
;;;;   - one NLE:HOOK on NLE::READ-HARNESS-SECTIONS, which is how the note that
;;;;     says which mode is on rides every request.
;;;;
;;;; Modes are per session, held in this image's table, and a writable session
;;;; is a lease: it returns to read-only once the operator has been silent for
;;;; writable_expiry_minutes (0 disables). Nothing leaves this machine.
;;;;
;;;; Config, a top-level key:
;;;;   "readonly-guard": {"default_mode": "readonly",
;;;;                      "writable_expiry_minutes": 30, "plans_dir": ""}

(defpackage #:nodecode-readonly-guard
  (:use #:cl))

(in-package #:nodecode-readonly-guard)

(nlk:define-peripheral readonly-guard :not-running t)

;;; --- the modes --------------------------------------------------------------

(defparameter +modes+ '("readonly" "plan" "writable")
  "The three postures, as the command and the section spell them.")

(defun mode-keyword (text)
  "TEXT as a mode keyword, or NIL when it is not one."
  (and (stringp text)
       (member text +modes+ :test #'string-equal)
       (intern (string-upcase text) :keyword)))

(defun section-default-mode ()
  "The mode a session with no entry of its own is under: the section's
:default-mode, and read-only for anything a typo might have written, so a
missing or misspelled setting can never loosen the guard."
  (or (ignore-errors (mode-keyword (setting :default-mode))) :readonly))

(defvar *sessions* (make-hash-table :test #'equal :synchronized t)
  "Session id -> its mode plist: :MODE, :OWN (chosen here, so it carries the
lease), :TURN (the turn the lease was last refreshed in) and :EXPIRES. The
empty-string key is the session a caller could not name.")

(defun session-key (&aux (turn (ignore-errors (nle:turn))))
  "The running session's id, or \"\" when nothing names one."
  (or (and turn (getf turn :session-id)) ""))

(defun turn-id () (getf (ignore-errors (nle:turn)) :turn-id))

(defun state-for (key) (gethash (or key "") *sessions*))

(defun (setf state-for) (value key) (setf (gethash (or key "") *sessions*) value))

(defun fresh-entry ()
  "One session's entry, every key present: a SETF on a member mutates the
plist the table holds rather than a copy."
  (list :mode nil :own nil :turn nil :expires nil))

(defun entry-for (key) (or (state-for key) (fresh-entry)))

(defun expiry-minutes ()
  "The configured lease in minutes; 0 means no limit."
  (let ((written (ignore-errors (setting :writable-expiry-minutes))))
    (if (and (integerp written) (not (minusp written))) written 30)))

(defun lease-mode (entry)
  "ENTRY's writable mode, lapsed to read-only when the lease ran out.
The clock starts at the beginning of each of the operator's turns, so a turn
already running always finishes."
  (let ((minutes (expiry-minutes)) (now (get-universal-time)) (turn (turn-id)))
    (cond ((zerop minutes) :writable)
          ((and (getf entry :turn) (equal (getf entry :turn) turn))
           :writable)
          ((and (getf entry :expires) (< now (getf entry :expires)))
           (setf (getf entry :turn) turn
                 (getf entry :expires) (+ now (* 60 minutes)))
           :writable)
          (t (setf (getf entry :mode) :readonly
                   (getf entry :expires) nil)
             :readonly))))

(defun mode-now (&optional (key (session-key)))
  "The mode KEY is under right now, after the writable lease is read."
  (let* ((entry (state-for key))
         (mode (or (and entry (getf entry :mode)) (section-default-mode))))
    (if (and (eq mode :writable) entry (getf entry :own))
        (lease-mode entry)
        mode)))

(defun set-mode (key mode)
  "Put KEY in MODE, arming the lease when it is writable."
  (let ((entry (entry-for key)) (minutes (expiry-minutes)))
    (setf (getf entry :mode) mode
          (getf entry :own) t
          (getf entry :turn) (turn-id)
          (getf entry :expires) (and (eq mode :writable) (plusp minutes)
                                      (+ (get-universal-time) (* 60 minutes))))
    (setf (state-for key) entry)
    mode))

;;; --- what a call reads, and what it is allowed to do ------------------------

(defparameter +read-only-tools+
  '("web_search" "websearch" "fetch" "search_files" "session_search"
    "vision_analyze" "skills_list" "skill_view" "tool_search")
  "Tools other than EVAL that a read-only session may call. EVAL is judged
by its form instead.")

(defparameter +read-operators+
  '("quote" "function" "$"
    ;; control
    "let" "let*" "if" "when" "unless" "and" "or" "cond" "case" "ecase"
    "progn" "prog1" "prog2" "values" "multiple-value-bind" "multiple-value-list"
    "dolist" "dotimes" "return" "block" "tagbody" "go" "declare" "the"
    ;; the harness's own readers
    "help" "index" "recall-entry" "recall-log" "list-evals" "eval-status"
    "cells" "config-get" "shipped-cells" "hub-cells"
    ;; pairs, sequences, numbers, strings
    "list" "list*" "cons" "car" "cdr" "caar" "cadr" "cdar" "cddr" "first"
    "second" "third" "fourth" "fifth" "rest" "last" "butlast" "nth" "nthcdr"
    "length" "reverse" "copy-list" "copy-tree" "append" "remove" "remove-if"
    "remove-if-not" "find" "find-if" "position" "count" "member" "assoc"
    "getf" "gethash" "hash-table-count" "hash-table-p" "mapcar" "mapcar*"
    "aref" "svref" "elt" "subseq" "concatenate" "format" "princ-to-string"
    "prin1-to-string" "write-to-string" "string" "string-upcase"
    "string-downcase" "string-trim" "string=" "string-equal" "string<" "search"
    "char" "char-upcase" "char-downcase" "code-char" "char-code"
    "+" "-" "*" "/" "1+" "1-" "=" "/=" "<" ">" "<=" ">=" "mod" "rem"
    "floor" "ceiling" "round" "truncate" "abs" "min" "max" "expt" "sqrt"
    "zerop" "plusp" "minusp" "evenp" "oddp" "not" "null" "eq" "eql"
    "equal" "equalp" "atom" "consp" "listp" "numberp" "integerp" "floatp"
    "stringp" "symbolp" "keywordp" "vectorp" "arrayp" "functionp" "charp"
    "symbol-name" "symbol-value" "symbol-package" "boundp" "fboundp"
    "find-symbol" "find-package" "package-name" "princ" "print" "terpri")
  "Heads a read-only call may name. Every other head is refused, so an
unexamined function or macro -- including a helper the agent defined -- fails
closed rather than being given the benefit of the doubt.")

(defparameter +plan-writers+ '("write-file" "edit")
  "The operators whose own target may be the plan markdown in plan mode.")

(defun operator-name (x)
  "X's symbol name, downcased, or NIL, so a form read in any package is read."
  (and (symbolp x) (string-downcase (symbol-name x))))

(defun literal-path (x strings)
  "The path X alone names, or NIL when the form hides it."
  (cond ((stringp x) x)
        ((and (consp x) (equal "$" (operator-name (car x))) (stringp (second x)) strings
              (hash-table-p strings))
         (gethash (second x) strings))
        (t nil)))

(defun read-forms (text)
  "TEXT's forms, or NIL when it does not read."
  (handler-case
      (with-input-from-string (in text)
        (let ((*read-eval* nil)
              (*package* (find-package :nodecode-readonly-guard))
              (forms '()))
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (push form forms))
          (nreverse forms)))
    (error () nil)))

(defun plans-dir ()
  "The directory the plan markdown lives in: the section's plans_dir, or
.nodecode/plans in the working directory."
  (let ((written (ignore-errors (setting :plans-dir))))
    (uiop:ensure-directory-pathname
     (uiop:merge-pathnames* (if (and written (stringp written) (plusp (length written)))
                                 written
                                 ".nodecode/plans/")
                            (uiop:getcwd)))))

(defun dir-namestring (dir) (uiop:native-namestring (uiop:ensure-directory-pathname dir)))

(defun inside-dir-p (path dir)
  "Whether PATH, as the form spells it, lands inside DIR."
  (and (stringp path)
       (plusp (length path))
       (uiop:string-prefix-p (dir-namestring dir)
                             (uiop:native-namestring (uiop:merge-pathnames* path (uiop:getcwd))))))

(defun read-only-refusal (form strings mode plans)
  "NIL when FORM only reads under MODE, else one line saying what it does
that a read-only session may not. Every head that is not a reader refuses,
so an unexamined function, macro or definition fails closed."
  (block verdict
    (labels ((refuse (control &rest args) (return-from verdict (apply #'format nil control args)))
             (walk-args (args quoted)
               (loop for tail = args then (cdr tail)
                     while (consp tail)
                     do (walk (car tail) quoted)
                     finally (when tail (walk tail quoted))))
             (walk-sh (form)
               (let* ((args (rest form))
                      (shown (remove-if #'keywordp args))
                      (command (and (consp shown) (null (cdr shown))
                                    (literal-path (first shown) strings))))
                 (cond ((null command)
                        (refuse "(sh ...) does not show the command it runs, so it is not a probe"))
                       (t (let ((why (probe-refusal command)))
                            (when why
                              (refuse "the command ~s is not a read-only probe: ~a" command why)))))))
             (walk (x quoted)
               (cond
                 ((consp x)
                  (let ((op (operator-name (car x))))
                    (cond
                      (quoted (walk-args (cdr x) t))
                      ((equal op "quote") nil)
                      ((equal op "function") nil)
                      ((equal op "sh") (walk-sh x))
                      ((equal op "$") nil)
                      ((member op +read-operators+ :test #'equal) (walk-args (cdr x) nil))
                      ((and (member op +plan-writers+ :test #'equal) (eq mode :plan))
                       (let ((path (literal-path (second x) strings)))
                         (cond ((inside-dir-p path plans) (walk-args (cddr x) nil))
                               (path (refuse "~a writes ~a, outside the plans directory ~a"
                                             op path (dir-namestring plans)))
                               (t (refuse "~a writes a file whose path the form does not show" op)))))
                      (t (refuse "~a is not a read-only call" op)))))
                 (t nil))))
      (walk form nil)
      nil)))

;;; --- the read-only programs -------------------------------------------------

(defparameter +readers+
  '(;; program, and the flags that make it write or run something after all
    ("ls") ("cat") ("head") ("tail" "-f" "--follow" "-F" "--retry")
    ("wc") ("file" "-C" "--compile") ("stat") ("pwd") ("tree" "-o")
    ("du") ("df") ("ps") ("uname") ("whoami") ("id") ("which") ("realpath")
    ("readlink") ("cut") ("tr") ("sort" "-o" "--output" "--compress-program")
    ("nl") ("column") ("diff") ("grep")
    ("rg" "--pre" "--pre-glob" "-z" "--search-zip" "--hostname-bin")
    ("jq"))
  "Programs that only read, each with the flags that stop them doing even that.")

(defparameter +find-flags+
  '("-name" "-iname" "-path" "-ipath" "-regex" "-iregex" "-lname" "-type"
    "-size" "-newer" "-mtime" "-mmin" "-user" "-group" "-perm" "-links"
    "-maxdepth" "-mindepth" "-fstype" "-samefile" "-empty" "-readable"
    "-writable" "-executable" "-print" "-print0" "-depth" "-prune" "-quit"
    "-not" "-and" "-or" "-a" "-o")
  "find admits only these primaries: one missed -exec, -delete or -fprintf and
the program is a mutation tool, so this is an allowlist.")

(defparameter +git-readers+
  '(("status") ("log" "--output" "--ext-diff" "--textconv")
    ("diff" "--output" "--ext-diff" "--textconv")
    ("show" "--output" "--ext-diff" "--textconv") ("shortlog" "--output")
    ("blame" "--output") ("grep" "-O" "--open-files-in-pager")
    ("rev-parse") ("ls-files") ("ls-tree") ("cat-file" "--filters" "--textconv")
    ("describe") ("for-each-ref") ("show-ref")
    ("whatchanged" "--output" "--ext-diff" "--textconv") ("version"))
  "git sub-commands that cannot mutate on a positional argument.")

(defparameter +git-listings+
  '(("branch" "-l" "--list" "-a" "--all" "-r" "--remotes" "-v" "--verbose"
     "--show-current")
    ("tag" "-l" "--list") ("stash" "list" "show") ("remote" "-v" "--verbose" "show")
    ("worktree" "list") ("notes" "list") ("reflog" "show")
    ("submodule" "status" "summary"))
  "Sub-commands safe only as a listing: the first word after them must be one
of these, and no positional may follow.")

(defparameter +git-global-value+ '("-C" "--git-dir" "--work-tree" "--namespace" "--super-prefix")
  "Global flags that take the next token as their value.")

(defparameter +git-global-denied+ '("-c" "--config-env" "--exec-path" "--paginate")
  "Refused anywhere in a git command line: config injection, an alternate
exec-path, a pager.")

(defparameter +git-global-only+ '("-p") "Refused only before the sub-command: -p is
--paginate there, and a patch after one.")

(defun flag-name (token)
  "TOKEN's flag part: --output=x -> --output."
  (subseq token 0 (or (position #\= token) (length token))))

(defun denied-flag (token denied)
  "The denied flag TOKEN names in any spelling its program accepts, or NIL."
  (let ((name (flag-name token)))
    (cond ((member name denied :test #'equal) name)
          ((and (> (length name) 2) (char= (char name 0) #\-) (char= (char name 1) #\-))
           (find-if (lambda (option)
                      (and (> (length option) 2) (eql 0 (search name option))))
                    denied))
          ((and (> (length name) 1) (char= (char name 0) #\-) (char/= (char name 1) #\-))
           (loop for index from 1 below (length name)
                 for short = (format nil "-~c" (char name index))
                 when (member short denied :test #'equal) return short)))))

(defun shell-tokens (text)
  "TEXT cut into the words a shell would hand the program.
=> (values TOKENS WHY). A shell operator, a substitution or a second line is
answered in WHY, because a probe is one command and nothing is composed."
  (let ((tokens '()) (word (make-string-output-stream)) (open nil) (i 0) (n (length text)))
    (macrolet ((end-word ()
                 `(let ((text (get-output-stream-string word)))
                    (when (plusp (length text)) (push text tokens)))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond
                   ((find c "();<>|&")
                    (return-from shell-tokens
                      (values nil (format nil "the shell operator ~c is not allowed; one command, no composition" c))))
                   ((char= c #\newline)
                    (return-from shell-tokens
                      (values nil "multiple lines are not a probe; one command only")))
                   ((char= c #\return) (incf i))
                   ((and open (char= c open)) (setf open nil) (incf i))
                   (open (write-char c word) (incf i))
                   ((find c "\"'")
                    (setf open c) (incf i))
                   ((or (char= c #\space) (char= c #\tab))
                    (end-word) (incf i))
                   ((char= c #\`)
                    (return-from shell-tokens
                      (values nil "command substitution is not allowed")))
                   ((char= c #\$)
                    (when (and (< (1+ i) n)
                               (let ((next (char text (1+ i))))
                                 (or (alphanumericp next) (find next "{(*@#?!$_-/"))))
                      (return-from shell-tokens
                        (values nil "a shell expansion is not allowed; a literal dollar is [$]")))
                    (write-char c word) (incf i))
                   (t (write-char c word) (incf i)))))
      (end-word)
      (values (nreverse tokens) nil))))

(defun find-refusal (rest)
  "NIL when find's arguments are all allowlisted primaries, else why."
  (let ((bad (find-if (lambda (token)
                        (and (plusp (length token))
                             (char= (char token 0) #\-)
                             (not (member token +find-flags+ :test #'equal))))
                      rest)))
    (and bad (format nil "find ~a is not a read-only primary" bad))))

(defun git-refusal (rest)
  "NIL when the git command line only reads, else why."
  (let ((tail rest))
    (loop while (and tail (plusp (length (first tail))) (char= (char (first tail) 0) #\-))
          do (let ((token (first tail)))
               (cond ((member token +git-global-denied+ :test #'equal)
                      (return-from git-refusal (format nil "git ~a runs something" token)))
                     ((member token +git-global-only+ :test #'equal)
                      (return-from git-refusal "git -p starts a pager"))
                     ((member token +git-global-value+ :test #'equal) (setf tail (cddr tail)))
                     (t (return (format nil "git ~a is not a read-only option" token))))))
    (let* ((sub (first tail)) (after (rest tail)))
      (cond
        ((null sub) "git needs a sub-command")
        ((second (assoc sub +git-readers+ :test #'equal))
         (let ((bad (find-if (lambda (token)
                               (and (plusp (length token)) (char= (char token 0) #\-)
                                    (denied-flag token (rest (assoc sub +git-readers+ :test #'equal)))))
                             after)))
           (and bad (format nil "git ~a ~a writes a file or runs a program" sub bad))))
        ((assoc sub +git-readers+ :test #'equal) nil)
        ((assoc sub +git-listings+ :test #'equal)
         (let ((flags (rest (assoc sub +git-listings+ :test #'equal))))
           (cond ((null after) nil)
                 ((not (member (first after) flags :test #'equal))
                  (format nil "git ~a ~a is not a listing" sub (first after)))
                 ((find-if (lambda (token) (not (char= (char token 0) #\-))) (rest after))
                  (format nil "git ~a takes an argument here, which may change something" sub))
                 (t nil))))
        (t (format nil "git ~a is not on the read-only list" sub))))))

(defparameter +probe-programs+
  (append (mapcar #'first +readers+) '("find" "git"))
  "Every program a probe may run, derived from the tables above.")

(defun probe-refusal (command)
  "NIL when COMMAND is a single read-only command, else one line saying why."
  (multiple-value-bind (tokens why) (shell-tokens command)
    (or why
        (let* ((program (first tokens)) (rest (rest tokens))
               (reader (and program (assoc program +readers+ :test #'equal))))
          (cond ((null tokens) "the command is empty")
                ((equal program "git") (git-refusal rest))
                ((equal program "find") (find-refusal rest))
                ((null reader) (format nil "~a is not one of the read-only programs" program))
                (t (let ((bad (find-if (lambda (token)
                                         (and (plusp (length token)) (char= (char token 0) #\-)
                                              (denied-flag token (rest reader))))
                                       rest)))
                     (and bad (format nil "~a ~a writes a file or runs a program" program bad)))))))))

;;; --- the verdict on one call ------------------------------------------------

(defun eval-refusal (arguments mode)
  "The refusal an EVAL call with ARGUMENTS earns under MODE, or NIL."
  (let* ((form (and (hash-table-p arguments) (gethash "form" arguments)))
         (strings (and (hash-table-p arguments) (gethash "strings" arguments)))
         (text (cond ((stringp form) form) (form (format nil "~s" form)) (t ""))))
    (when (zerop (length text))
      (return-from eval-refusal nil))
    (let ((forms (read-forms text)) (plans (plans-dir)))
      (if (null forms)
          "the call's form does not read, so nothing about it is shown to be a read"
          (loop for one in forms
                for why = (read-only-refusal one strings mode plans)
                when why return why)))))

(defun call-refusal (name arguments mode)
  "NIL when a call of tool NAME with ARGUMENTS runs under MODE, else why not."
  (cond ((eq mode :writable) nil)
        ((equal name "eval") (eval-refusal arguments mode))
        ((member name +read-only-tools+ :test #'equal) nil)
        (t (format nil "~a is not a read-only tool" name))))

;;; --- what the model is told, in a refusal and in the standing note ----------

(defun mode-label (mode)
  (case mode (:readonly "read-only") (:plan "plan") (t "writable")))

(defun refusal-text (why mode)
  "The refusal the model reads, and the way on."
  (format nil
          "ERROR: refused by nodecode-readonly-guard: this session is in ~a mode, and ~a.~%~@[In plan mode the plan markdown under ~a is the one write that runs; write the plan there and show it in your reply.~%~]Available: inspection calls (help, index, recall-entry, list-evals, and the form's readers) and single read-only commands through (sh \"ls\"), (sh \"git log\") or (sh \"rg ...\") -- one program, no operators, no $(...). Nothing is written, sent or scheduled. Do not retry this call or write it another way round: report what you found in the conversation. The operator leaves the mode with /readonly writable, which affects only this session."
          (mode-label mode) why
          (and (eq mode :plan) (dir-namestring (plans-dir)))))

(defun mode-note (key)
  "The note that rides every request while KEY is not writable, or NIL."
  (let ((mode (mode-now key)) (plans (dir-namestring (plans-dir))))
    (case mode
      (:readonly
       (format nil "Read-only mode is ON for this session (nodecode-readonly-guard): identification with (help ...), (index), (recall-entry ...), (list-evals) and the form's own readers runs, and so does a single read-only command through (sh \"...\"); every other call is refused before it runs. Write nothing, send nothing, schedule nothing. Read what you need and answer in the conversation; the operator leaves the mode with /readonly writable, which affects this session only."))
      (:plan
       (format nil "Plan mode is ON for this session (nodecode-readonly-guard): the same read-only calls run, plus a write of the plan markdown under ~a. Explore, write the plan there as Markdown, show it in your reply, and change nothing else. The operator leaves the mode with /readonly writable, which affects this session only." plans))
      (t nil))))

(defun guard-section (next session-id)
  "The standing harness section: which mode this session is under."
  (let ((text (mode-note (or session-id ""))))
    (if text
        (acons "readonly-guard" text
               (remove "readonly-guard" (funcall next session-id) :key #'car :test #'equal))
        (funcall next session-id))))

;;; --- the one hook -------------------------------------------------------------

(defun guard-call (op next)
  "The :TOOL point: the mode's verdict on the call, and NEXT only when it passes."
  (let* ((key (session-key))
         (mode (mode-now key))
         (why (call-refusal (getf op :name) (getf op :arguments) mode)))
    (if why
        (nle:failure (refusal-text why mode))
        (funcall next op))))

;;; --- the operator's side: one command -----------------------------------------

(defun status-text (&optional (key (session-key)))
  "What /readonly status answers: the mode in force here, and why."
  (let* ((entry (state-for key))
         (mode (mode-now key))
         (source (if (and entry (getf entry :mode)) "this session" "the section's default_mode"))
         (minutes (expiry-minutes)))
    (format nil
            "read-only guard: ~a (~a; session ~s).~%default for new sessions: ~a~%~a~@[~%the lease runs out at ~a~]"
            (mode-label mode) source (or key "")
            (mode-label (section-default-mode))
            (if (plusp minutes)
                (format nil "writable lease: ~a minutes without a message" minutes)
                "writable lease: off, a writable session stays on")
            (and (eq mode :writable) entry (getf entry :expires)
                 (multiple-value-bind (s m h d mo y) (decode-universal-time (getf entry :expires))
                   (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m))))))

(defparameter +usage+ "usage: /readonly [status|readonly|plan|writable|on|off|toggle]")

(defun run-readonly (args session-id)
  "The /readonly command: the operator's side of the switch, for one session."
  (let* ((key (or session-id (session-key)))
         (text (nlk:trimmed (or args "")))
         (head (string-downcase text)))
    (cond ((or (equal head "") (equal head "status")) (status-text key))
          ((equal head "toggle")
           (set-mode key (if (eq (mode-now key) :writable) :readonly :writable))
           (status-text key))
          ((equal head "on") (set-mode key :readonly) (status-text key))
          ((equal head "off") (set-mode key :writable) (status-text key))
          ((mode-keyword head) (set-mode key (mode-keyword head)) (status-text key))
          (t (format nil "~a~%~%~a" +usage+ (status-text key))))))

(defparameter +manual+
  "Read-only guard: a code-enforced posture per session, in three modes. Read-only (the default) refuses every call that is not an inspection -- the manual, the index, a recalled entry, a list of evaluations, the form's own readers -- or a single read-only shell command (ls, cat, grep, git log, rg, find with read-only primaries), so nothing is written, sent or scheduled. Plan mode is the same, plus a write of the plan markdown under the plans directory. Writable blocks nothing, and is a lease: thirty minutes without a message returns the session to read-only. /readonly status reports the mode, where it came from and when a lease runs out; /readonly readonly | plan | writable switches this session; /readonly toggle flips between read-only and writable; on and off are the same two. A new session starts as the section's default_mode. It is a speed bump, not a sandbox: a command that is already running is not affected, and a probe can read anything you can read.")

(nle:define-cell readonly-guard
  (:section ("readonly-guard")
    (:guide "nothing is required; new sessions start at default_mode, and a writable session returns to read-only after writable_expiry_minutes without a message (0 keeps it on); /readonly status shows the mode in force")
    ("default_mode" :choice :options '("readonly" "plan" "writable") :default "readonly"
     :doc "the mode a session that has never been switched is under; a value outside the three reads as read-only")
    ("writable_expiry_minutes" :integer :default 30 :min 0
     :doc "a writable session returns to read-only this many minutes after your last message; 0 keeps it writable")
    ("plans_dir" :string :default ""
     :doc "where plan mode's markdown goes; empty is .nodecode/plans in the working directory"))
  (:command "readonly" 'run-readonly
            :description "Read-only, plan or writable for this session"
            :argument-hint "status | readonly | plan | writable | toggle")
  (:help :readonly-guard
         "readonly-guard: read-only, plan and writable modes per session - /readonly status, /readonly writable"
         +manual+)
  (:hook :tool #'guard-call)
  (:hook 'nle::read-harness-sections #'guard-section)
  (:start (lambda ()
            (clrhash *sessions*)
            (nle:on-stop (lambda () (clrhash *sessions*))))))
