;;;; observe.lisp --- what a wake may do (observe), and what it has spent.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A wake is a fire the organism gives itself: nobody asked, so nothing it
;;;; runs may act. Two pieces the wake runner calls:
;;;;
;;;;   WAKE-CALL-REFUSAL  one tool call of the wake session -> NIL (it only
;;;;                      reads) or the reason it is refused. The test is a
;;;;                      WHITELIST walk, never a list of bad verbs: an
;;;;                      operator the walk does not know refuses the call.
;;;;   WAKE-SPENT-24H     what the wake session and every session under it
;;;;                      spent over the last day, in USD, or NIL when that
;;;;                      is not known (a round with no price, no store):
;;;;                      an unknown spend is never read as zero.
;;;;
;;;; Why not the kernel's REPLAY-SAFE-FORM-P: it admits every COMMON-LISP
;;;; operator it does not refuse by name, so a function designator handed to
;;;; a CL function slips through -- (mapcar 'delete-file ...), (find x l :key
;;;; 'sh), (coerce '(lambda ...) 'function), (read-from-string "#.(...)"),
;;;; (format nil "~/sh/" ...). Replaying a cut-off snippet tolerates that;
;;;; a gate does not. Here a CL operator passes only when it is on
;;;; +PURE-OPERATORS+, a function argument only as #'<a passing operator> or
;;;; a lambda whose body passes, a FORMAT only with a literal control string
;;;; and no ~/ directive, and SH only with a literal command WAKE-SH-REFUSAL
;;;; admits.

(in-package #:nodecode-cron)

(defparameter +pure-operators+
  '(;; control and binding (special forms the walk knows by shape)
    progn prog1 prog2 if when unless and or not values multiple-value-list
    ;; lists
    list list* cons car cdr caar cadr cdar cddr caddr cdddr first second third
    fourth fifth sixth seventh eighth ninth tenth rest last butlast nth nthcdr
    append reverse length list-length endp null listp consp atom getf
    remove-duplicates subseq elt copy-list copy-seq mismatch
    ;; sequences searched by value (a :key or :test must be a function form)
    member assoc rassoc find position count search remove substitute
    ;; sequences walked by a function (its argument must be a function form)
    mapcar mapcan mapc maplist some every notany notevery remove-if
    remove-if-not count-if find-if position-if member-if assoc-if sort
    stable-sort reduce map
    ;; tables
    gethash hash-table-count hash-table-p
    ;; numbers
    + - * / = /= < > <= >= 1+ 1- min max abs floor ceiling round truncate mod
    rem zerop plusp minusp evenp oddp numberp integerp realp float
    parse-integer
    ;; strings, characters, symbols
    string string= string/= string< string> string-equal string-upcase
    string-downcase string-trim string-left-trim string-right-trim
    concatenate char schar aref svref char= char-equal char-code code-char
    stringp characterp symbolp keywordp symbol-name vectorp
    princ-to-string prin1-to-string format
    ;; identity
    eq eql equal equalp type-of identity)
  "The COMMON-LISP operators a wake may call: each only computes over its
arguments. Nothing on it writes, defines, reads code, or turns a computed
value into a call.")

(defparameter +function-positions+
  '((mapcar 0) (mapcan 0) (mapc 0) (maplist 0) (some 0) (every 0) (notany 0)
    (notevery 0) (remove-if 0) (remove-if-not 0) (count-if 0) (find-if 0)
    (position-if 0) (member-if 0) (assoc-if 0) (reduce 0) (sort 1)
    (stable-sort 1) (map 1))
  "Each walking operator and the zero-based place of its function argument.")

(defparameter +function-keywords+ '(:key :test :test-not)
  "Keyword arguments whose value is called.")

(defvar *wake-read-operators*
  '(jobs show runs wake-next)
  "Operators beyond the kernel's read set (NLE::*REPLAY-OPERATORS*) a wake
may call: cron's own reads, and WAKE-NEXT, the wake's own clock (the one
thing it moves, inside the owner's bounds).")

(defun wake-operator-p (symbol)
  (and (symbolp symbol)
       (or (member symbol nle::*replay-operators*)
           (member symbol *wake-read-operators*)
           (and (eq (symbol-package symbol) (find-package '#:common-lisp))
                (member symbol +pure-operators+)))
       t))

(defun sh-symbol-p (symbol)
  (and (symbolp symbol) (string= (symbol-name symbol) "SH") (fboundp symbol)
       (eq (symbol-function symbol) (symbol-function 'nle::sh))))

(defun $-symbol-p (symbol)
  (and (symbolp symbol) (string= (symbol-name symbol) "$")
       (member symbol nle::*replay-operators*)))

;;; The shell: one literal command line, every segment of its pipe a program
;;; on the table with arguments that only read. No redirection, substitution,
;;; sequencing, background or escape: the characters that spell them refuse
;;; the line before any word is read.

(defparameter +sh-refused-characters+ '(#\; #\& #\> #\< #\` #\$ #\( #\) #\{ #\} #\\ #\Newline #\Return)
  "A command line carrying any of these is refused whole.")

(defun sh-words (segment)
  "SEGMENT's words, quotes ' and \" grouping (no escapes: a backslash
never reaches here), or :UNBALANCED."
  (let ((words '()) (word nil) (quote-char nil))
    (flet ((end-word () (when word (push (coerce (nreverse word) 'string) words) (setf word nil))))
      (loop for c across segment
            do (cond (quote-char (if (char= c quote-char) (setf quote-char nil) (push c word)))
                     ((member c '(#\' #\")) (setf quote-char c) (unless word (setf word (list))))
                     ((member c '(#\Space #\Tab)) (end-word))
                     (t (push c word))))
      (when quote-char (return-from sh-words :unbalanced))
      (end-word)
      (nreverse words))))

(defun option-p (word &rest names)
  (some (lambda (name) (or (string= word name)
                           (and (> (length name) 2) (eql 0 (search name word))
                                (> (length word) (length name))
                                (char= (char word (length name)) #\=))))
        names))

(defun any-option (args &rest names)
  (find-if (lambda (a) (apply #'option-p a names)) args))

(defparameter +sh-plain-readers+
  '("ls" "cat" "head" "wc" "grep" "cut" "tr" "date" "uptime" "free" "df"
    "du" "ps" "jq" "stat" "file" "basename" "dirname" "realpath" "readlink"
    "echo" "true" "nproc" "hostname" "id" "pwd" "md5sum" "sha256sum")
  "Programs that only read whatever their arguments.")

(defun sh-program-refusal (words)
  "NIL when WORDS, one pipe segment, only reads; else why not."
  (destructuring-bind (program &rest args) words
    (flet ((no (fmt &rest xs) (return-from sh-program-refusal (apply #'format nil fmt xs))))
      (cond
        ((member program +sh-plain-readers+ :test #'string=) nil)
        ((string= program "tail")
         (when (any-option args "-f" "-F" "--follow") (no "tail follows forever")))
        ((string= program "sort")
         (when (any-option args "-o" "--output") (no "sort -o writes a file")))
        ((string= program "uniq")
         (when (> (count-if-not (lambda (a) (eql 0 (search "-" a))) args) 1)
           (no "uniq with a second file writes it")))
        ((string= program "sed")
         ;; Only a line print: sed's w, e and -i write or run.
         (let ((options (remove-if-not (lambda (a) (eql 0 (search "-" a))) args))
               (script (find-if-not (lambda (a) (eql 0 (search "-" a))) args)))
           (unless (and (equal options '("-n"))
                        script
                        (> (length script) 1)
                        (char= #\p (char script (1- (length script))))
                        (every (lambda (c) (or (digit-char-p c) (char= c #\,))) (subseq script 0 (1- (length script)))))
             (no "sed runs only as sed -n 'N,Mp' FILE"))))
        ((string= program "gh")
         (unless (and (member (first args) '("pr" "issue" "run" "release" "repo") :test #'string=)
                      (member (second args) '("view" "list" "checks" "diff" "status") :test #'string=)
                      (not (any-option args "--web" "-w")))
           (no "gh ~{~a~^ ~} is not a read" (subseq args 0 (min 2 (length args))))))
        ((string= program "rg")
         (when (any-option args "--pre") (no "rg --pre runs a program")))
        ((string= program "find")
         (let ((act (any-option args "-exec" "-execdir" "-ok" "-okdir" "-delete"
                                "-fprint" "-fprint0" "-fprintf" "-fls")))
           (when act (no "find ~a acts" act))))
        ((string= program "nvidia-smi")
         (let ((bad (find-if-not (lambda (a)
                                   (or (member a '("-q" "-L" "-i" "-d" "--query") :test #'string=)
                                       (option-p a "--query-gpu" "--query-compute-apps" "--format" "--id")
                                       (every (lambda (c) (or (alphanumericp c) (member c '(#\, #\. #\_)))) a)))
                                 args)))
           (when (or bad (some (lambda (a) (char= (char a 0) #\-)) (remove-if (lambda (a) (or (member a '("-q" "-L" "-i" "-d" "--query") :test #'string=) (option-p a "--query-gpu" "--query-compute-apps" "--format" "--id"))) args)))
             (no "nvidia-smi ~a sets the card" (or bad "")))))
        ((string= program "journalctl")
         (let ((act (any-option args "-f" "--follow" "--rotate" "--vacuum-size" "--vacuum-time"
                                "--vacuum-files" "--flush" "--sync" "--relinquish-var"
                                "--setup-keys" "--update-catalog")))
           (when act (no "journalctl ~a acts or follows" act))))
        ((string= program "systemctl")
         (let ((verb (find-if-not (lambda (a) (eql 0 (search "-" a))) args)))
           (unless (member verb '("status" "is-active" "is-enabled" "is-failed" "list-units"
                                  "list-timers" "list-unit-files" "show" "cat")
                           :test #'equal)
             (no "systemctl ~a acts" (or verb "")))))
        ((string= program "git")
         (when (any-option args "-c" "--exec-path" "--config-env") (no "git -c sets a program"))
         (let ((verb (loop with rest = args
                           while rest
                           do (let ((a (pop rest)))
                                (cond ((member a '("-C" "--git-dir" "--work-tree") :test #'string=) (pop rest))
                                      ((eql 0 (search "-" a)))
                                      (t (setf args rest) (return a)))))))
           (cond
             ((member verb '("log" "status" "diff" "show" "rev-parse" "ls-files" "blame"
                             "shortlog" "describe" "rev-list" "show-ref" "cat-file" "grep")
                      :test #'equal)
              (when (any-option args "--output" "-o") (no "git ~a --output writes a file" verb)))
             ((equal verb "branch")
              (unless (every (lambda (a) (member a '("-a" "-r" "-v" "-vv" "-l" "--list" "--all" "--remotes" "--show-current") :test #'string=)) args)
                (no "git branch with ~{~a~^ ~} makes or moves a branch" args)))
             (t (no "git ~a acts" (or verb ""))))))
        ((string= program "docker")
         (let ((verb (find-if-not (lambda (a) (eql 0 (search "-" a))) args)))
           (cond ((member verb '("ps" "images" "inspect" "version" "info") :test #'equal) nil)
                 ((equal verb "logs") (when (any-option args "-f" "--follow") (no "docker logs -f follows forever")))
                 ((equal verb "stats") (unless (any-option args "--no-stream") (no "docker stats streams forever")))
                 (t (no "docker ~a acts" (or verb ""))))))
        (t (no "~a is not a reader a wake may run" program))))))

(defun wake-sh-refusal (command)
  "NIL when the shell line COMMAND only reads; else the reason it is refused."
  (cond
    ((not (stringp command)) "sh's command is no literal string")
    ((find-if (lambda (c) (member c +sh-refused-characters+)) command)
     (format nil "sh line carries ~s: a wake runs one plain pipe"
             (string (find-if (lambda (c) (member c +sh-refused-characters+)) command))))
    (t (let ((segments (loop with start = 0
                             for bar = (position #\| command :start start)
                             collect (subseq command start bar)
                             while bar do (setf start (1+ bar)))))
         (dolist (segment segments nil)
           (let ((words (sh-words segment)))
             (cond ((eq words :unbalanced) (return "sh line has an unclosed quote"))
                   ((null words) (return "sh line has an empty pipe segment"))
                   ((find #\/ (first words)) (return (format nil "~a: a wake runs programs by name only" (first words))))
                   ((find #\= (first words)) (return "sh line sets the environment"))
                   (t (let ((no (sh-program-refusal words))) (when no (return no)))))))))))

;;; The walk.

(defun observe-refusal (form strings)
  "NIL when FORM only reads; else the reason. STRINGS is the call's
$ table (a hash table or NIL), which ($ \"key\") and (sh ($ \"key\")) read."
  (let ((budget 4096))
    (labels ((no (fmt &rest args) (return-from observe-refusal (apply #'format nil fmt args)))
             (literal-p (x) (or (stringp x) (numberp x) (keywordp x) (characterp x) (member x '(t nil))))
             (forms (list) (if (and (listp list) (ignore-errors (list-length list)))
                               (mapc #'walk list)
                               (no "a dotted or circular form")))
             (binding-forms (list)
               (unless (and (listp list) (ignore-errors (list-length list))) (no "a malformed binding list"))
               (dolist (b list)
                 (cond ((symbolp b))
                       ((and (consp b) (symbolp (car b)) (ignore-errors (list-length b))) (forms (cdr b)))
                       (t (no "a malformed binding ~s" b)))))
             (lambda-form (tail)
               ;; (LAMBDA-LIST . BODY): the list binds plain names only.
               (unless (and (consp tail) (listp (car tail))
                            (every (lambda (p) (and (symbolp p) (not (member p lambda-list-keywords))))
                                   (car tail)))
                 (no "a lambda list a wake cannot read"))
               (forms (cdr tail)))
             (function-form (x)
               ;; An argument that is called: #'<operator> or a lambda.
               (cond ((and (consp x) (eq (car x) 'function) (symbolp (second x)))
                      (unless (wake-operator-p (second x)) (no "#'~s is not an operator a wake may call" (second x))))
                     ((and (consp x) (eq (car x) 'function) (consp (second x)) (eq (car (second x)) 'lambda))
                      (lambda-form (cdr (second x))))
                     ((and (consp x) (eq (car x) 'lambda)) (lambda-form (cdr x)))
                     (t (no "~s is called, but is no #'operator or lambda" x))))
             (string-arg (x)
               (cond ((stringp x) x)
                     ((and (consp x) ($-symbol-p (car x)) (stringp (second x)) (null (cddr x)))
                      (let ((text (and (hash-table-p strings) (gethash (second x) strings))))
                        (or (and (stringp text) text) (no "($ ~s) names no string" (second x)))))
                     (t nil)))
             (call (x &aux (head (car x)) (args (cdr x)))
               (case head
                 (quote nil)
                 (function (function-form x))
                 (lambda (lambda-form args))
                 ((let let*) (binding-forms (first args)) (forms (rest args)))
                 ((dolist dotimes)
                  (unless (and (consp (first args)) (symbolp (car (first args)))) (no "a malformed ~(~a~)" head))
                  (forms (cdr (first args))) (forms (rest args)))
                 ((multiple-value-bind destructuring-bind)
                  (unless (and (listp (first args)) (every #'symbolp (first args))) (no "a malformed ~(~a~)" head))
                  (forms (rest args)))
                 (cond (dolist (clause args) (if (consp clause) (forms clause) (no "a malformed cond"))))
                 ((case ecase typecase etypecase)
                  (walk (first args))
                  (dolist (clause (rest args)) (if (consp clause) (forms (cdr clause)) (no "a malformed ~(~a~)" head))))
                 (ignore-errors (forms args))
                 (handler-case
                  (walk (first args))
                  (dolist (clause (rest args))
                    (if (and (consp clause) (listp (second clause))) (forms (cddr clause)) (no "a malformed handler-case"))))
                 (format
                  (unless (and (member (first args) '(nil t)) (stringp (second args)))
                    (no "format writes only to NIL or T, with a literal control"))
                  (when (search "~/" (second args)) (no "format's ~~/ calls a function by name"))
                  (forms (cddr args)))
                 (t
                  (cond
                    ;; The wake may read its own status, and read Discord.
                    ((and (eq head 'wake) (equal args '(:status))) nil)
                    ((and (let ((ncd (find-package "NCD")))
                            (and ncd (eq head (find-symbol "REQUEST" ncd))))
                          (stringp (first args)) (string-equal (first args) "GET"))
                     (forms (rest args)))
                    ((sh-symbol-p head)
                     (let ((command (string-arg (first args))))
                       (unless command (no "sh's command must be a literal or ($ \"key\")"))
                       (unless (every #'literal-p (rest args)) (no "sh's options must be literals"))
                       (let ((reason (wake-sh-refusal command))) (when reason (no "~a" reason)))))
                    ((not (wake-operator-p head))
                     (no "~s is not an operator a wake may call" head))
                    (t
                     (let ((at (second (assoc head +function-positions+))))
                       (loop for arg in args for i from 0
                             for previous = nil then (nth (1- i) args)
                             do (if (or (eql i at) (member previous +function-keywords+))
                                    (function-form arg)
                                    (walk arg)))))))))
             (walk (x)
               (when (minusp (decf budget)) (no "a form too large to read through"))
               (cond ((atom x) nil)
                     ((not (ignore-errors (list-length x))) (no "a dotted or circular form"))
                     ((consp (car x)) (if (eq (caar x) 'lambda)
                                          (progn (lambda-form (cdar x)) (forms (cdr x)))
                                          (no "~s is no operator" (car x))))
                     ((symbolp (car x)) (call x))
                     (t (no "~s is no operator" (car x))))))
      (walk form)
      nil)))

(defun op-argument (arguments key)
  (cond ((hash-table-p arguments) (gethash key arguments))
        ((stringp arguments) (let ((decoded (ignore-errors (nlk:decode-json arguments))))
                               (and (hash-table-p decoded) (gethash key decoded))))))

(defun wake-call-refusal (op)
  "NIL when OP, one tool call of the wake session -- (:NAME :ARGUMENTS
:CALL-ID ...) -- only reads; else the reason it is refused, a line the
model reads back. Only eval runs, and only a snippet whose every toplevel
form passes the whitelist walk (OBSERVE-REFUSAL)."
  (let ((name (getf op :name)))
    (if (not (equal name "eval"))
        (format nil "a wake observes: only eval runs, not ~a" name)
        (let* ((arguments (getf op :arguments))
               (text (op-argument arguments "form"))
               (strings (op-argument arguments "strings"))
               (forms (and (stringp text) (nle::snippet-forms text))))
          (cond
            ((not (stringp text)) "a wake's eval carries no form")
            ((null forms) "a wake's eval must read as forms")
            (t (dolist (form forms nil)
                 (let ((reason (observe-refusal form strings)))
                   (when reason
                     (return (format nil "a wake observes, it does not act: ~a" reason)))))))))))

;;; Spend: read off the event log -- every turn.usage round of the wake's
;;; session and of each session under it (its forks, a note's session), so
;;; the reader holds on any core, whatever projections it keeps.

(defparameter +wake-tree-sql+
  "with recursive tree (session_id, depth) as (
          select ?1, 0
          union
          select d.session_id, t.depth + 1 from session_directory d join tree t on d.parent = t.session_id
           where t.depth < 64)
   select session_id from tree"
  "SESSION ?1 and every session whose parent chain reaches it.")

(defparameter +wake-spend-scan+ 5000
  "The most rounds read off one session; a day that fills it is unknown.")

(defun utc-stamp (universal-time)
  (multiple-value-bind (s m h day month year) (decode-universal-time universal-time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d" year month day h m s)))

(defun wake-session-tree (session)
  (nlk::with-store-handle (handle)
    (mapcar #'first (nlk::sql-rows handle +wake-tree-sql+ session))))

(defun wake-spent-24h (session now)
  "USD SESSION and every session under it spent in the day before the
universal time NOW, voided attempts counted (they were paid for); NIL when
that is not known -- a round with no price, more rounds than one read
takes, or no store: unknown is never read as zero. Further values: the cost
known so far, the unpriced rounds, all rounds."
  (handler-case
      (if (not (nlk:store-open-p))
          (values nil nil nil nil)
          (let ((from (utc-stamp (- now 86400))) (to (utc-stamp now))
                (known 0d0) (unpriced 0) (rounds 0) (cut nil))
            (dolist (id (wake-session-tree session))
              (let ((events (nlk:events :session-id id :kind (list nlk::+kind-turn-usage+)
                                        :order :newest :limit +wake-spend-scan+ :as :instances)))
                (dolist (event events (when (= (length events) +wake-spend-scan+) (setf cut t)))
                  (let ((at (nlk::event-occurred-at event)))
                    (cond ((string< at from) (return))
                          ((string< to at))
                          (t (incf rounds)
                             (if (nlk:turn-usage-event-cost-known event)
                                 (incf known (nlk:turn-usage-event-cost-usd event))
                                 (incf unpriced))))))))
            (values (and (zerop unpriced) (not cut) known) known unpriced rounds)))
    (error () (values nil nil nil nil))))
