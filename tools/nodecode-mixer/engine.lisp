;;;; engine.lisp --- the mixer's four dials, read off one EVAL call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Sahil-SS9's hermes-agent-mixer. Nodecode is a mono-tool
;;;; organism: the model calls EVAL, and the call's form carries what it
;;;; changes -- (write-file path text), (edit path old new), (patch ...),
;;;; (delete-file path), a (with-open-file ... :direction :output) -- and the
;;;; commands it runs, (sh "npm install left-pad"). This file READS that form
;;;; once and answers the four dials: it never greps the text, so a comment
;;;; naming package.json is not a dependency, and a string that says rm is not
;;;; a command.
;;;;
;;;; The stages run in a fixed order -- plan, scope, deps, lines, ask -- and
;;;; the first denial wins. Mixer only restricts: it denies, asks, or says
;;;; nothing; it never allows, so the harness's own prompts still run.

(defpackage #:nodecode-mixer
  (:use #:cl)
  (:export #:start-cell #:stop-cell))

(in-package #:nodecode-mixer)

(declaim (ftype (function (keyword) t) setting))

;;; --- small text ---------------------------------------------------------------

(defun starts-with (prefix text)
  "TEXT begins with PREFIX."
  (let ((n (length prefix)))
    (and (>= (length text) n) (string= prefix text :end2 n))))

(defun ends-with (suffix text)
  "TEXT ends with SUFFIX."
  (let ((n (length suffix)) (m (length text)))
    (and (>= m n) (string= suffix text :start2 (- m n)))))

(defun basename (path)
  "PATH's last component."
  (let* ((path (substitute #\/ #\\ (or path "")))
         (slash (position #\/ path :from-end t)))
    (if slash (subseq path (1+ slash)) path)))

(defun split-lines (text)
  "TEXT's lines, a trailing newline closing the last one rather than opening
an empty one."
  (let ((text (or text ""))
        (lines '())
        (start 0))
    (loop for i from 0 below (length text)
          when (char= #\Newline (char text i))
            do (push (subseq text start i) lines)
               (setf start (1+ i)))
    (when (< start (length text))
      (push (subseq text start) lines))
    (nreverse lines)))

(defun line-count (text)
  "The lines TEXT is, by an editor's count."
  (length (split-lines text)))

(defun changed-line-count (before after)
  "Added plus removed lines between BEFORE and AFTER: the common head and
tail stripped, what is left counted on both sides."
  (let* ((b (split-lines before))
         (a (split-lines after))
         (n (length b))
         (m (length a))
         (head 0))
    (loop while (and (< head n) (< head m) (equal (nth head b) (nth head a)))
          do (incf head))
    (let ((tail 0))
      (loop while (and (< tail (- n head)) (< tail (- m head))
                       (equal (nth (- n 1 tail) b) (nth (- m 1 tail) a)))
            do (incf tail))
      (+ (- n head tail) (- m head tail)))))

(defun replace-once (text old new)
  "TEXT with OLD replaced by NEW once, or NIL when OLD is not there."
  (let ((at (and (stringp old) (plusp (length old)) (search old text))))
    (and at
         (concatenate 'string (subseq text 0 at) new (subseq text (+ at (length old)))))))

(defun split-words (text)
  "TEXT as words: single and double quotes group, and a backslash escapes."
  (let ((words '()) (current (make-string-output-stream)) (started nil)
        (quote nil) (i 0))
    (flet ((finish ()
             (when started (push (get-output-stream-string current) words) (setf started nil))))
      (loop while (< i (length text))
            do (let ((c (char text i)))
                 (cond
                   (quote (if (char= c quote)
                              (setf quote nil)
                              (progn (write-char c current) (setf started t))))
                   ((or (char= c #\") (char= c #\')) (setf quote c) (setf started t))
                   ((char= c #\\) (setf started t) (incf i)
                    (when (< i (length text)) (write-char (char text i) current)))
                   ((member c '(#\Space #\Tab #\Newline #\Return))
                    (finish))
                   (t (write-char c current) (setf started t)))
                 (incf i)))
      (finish))
    (nreverse words)))

(defun split-commands (text)
  "TEXT's commands, one per element: an unquoted ; & | or newline starts the
next one, and a quoted one is part of the word it sits in."
  (let ((commands '()) (start 0) (i 0) (quote nil) (n (length (or text "")))
        (text (or text "")))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond ((char= c #\\) (incf i 2))
                     (quote (when (char= c quote) (setf quote nil)) (incf i))
                     ((member c '(#\" #\')) (setf quote c) (incf i))
                     ((member c '(#\; #\& #\| #\Newline))
                      (push (subseq text start i) commands)
                      (setf start (1+ i))
                      (incf i))
                     (t (incf i)))))
    (push (subseq text start) commands)
    (remove "" (mapcar (lambda (line) (string-trim '(#\Space #\Tab #\Return) line))
                       (nreverse commands))
            :test #'equal)))

;;; --- paths and globs -----------------------------------------------------------

(defun expand-home (path)
  "PATH with a leading ~ taken to the home directory."
  (if (and (plusp (length path)) (char= #\~ (char path 0)))
      (let ((home (namestring (user-homedir-pathname))))
        (concatenate 'string (string-right-trim "/" home) (subseq path 1)))
      path))

(defun normalize-path (path)
  "PATH with forward slashes, doubled separators, '.' and '..' resolved."
  (let* ((path (substitute #\/ #\\ (or path "")))
         (absolute (and (plusp (length path)) (char= #\/ (char path 0))))
         (parts (remove "" (uiop:split-string path :separator "/") :test #'equal))
         (stack '()))
    (dolist (part parts)
      (cond ((equal part ".") nil)
            ((equal part "..")
             (cond ((and stack (not (equal (first stack) ".."))) (pop stack))
                   ((not absolute) (push part stack))))
            (t (push part stack))))
    (format nil "~a~{~a~^/~}" (if absolute "/" "") (nreverse stack))))

(defun resolve-path (path base)
  "PATH against BASE: absolute stays as it is, relative joins."
  (let ((path (expand-home (or path ""))))
    (if (and (plusp (length path)) (char= #\/ (char path 0)))
        (normalize-path path)
        (normalize-path (concatenate 'string (string-right-trim "/" (or base "")) "/" path)))))

(defun normalize-glob (glob)
  "GLOB as a pattern: home expanded, forward slashes, no leading ./ ."
  (let ((glob (expand-home (substitute #\/ #\\ (string-trim '(#\Space #\Tab) (or glob ""))))))
    (loop while (starts-with "./" glob) do (setf glob (subseq glob 2)))
    (let ((trimmed (string-right-trim "/" glob)))
      (if (zerop (length trimmed)) glob trimmed))))

(defun absolute-glob-p (glob)
  "GLOB anchors at the filesystem root rather than at the workspace."
  (and (plusp (length glob)) (char= #\/ (char glob 0))))

(defun glob-match (pattern path)
  "PATTERN matches the whole PATH: ** spans separators, * and ? do not."
  (labels ((walk (p s)
             (cond
               ((= p (length pattern)) (= s (length path)))
               ((and (char= #\* (char pattern p))
                     (< (1+ p) (length pattern))
                     (char= #\* (char pattern (1+ p))))
                (let ((p2 (+ p 2)))
                  (if (and (< p2 (length pattern)) (char= #\/ (char pattern p2)))
                      (or (walk (1+ p2) s)
                          (loop for i from s below (length path)
                                when (char= #\/ (char path i))
                                  return (walk (1+ p2) (1+ i))))
                      (loop for i from s to (length path)
                            when (walk p2 i)
                              return t))))
               ((char= #\* (char pattern p))
                (let ((limit (or (position #\/ path :start s) (length path))))
                  (loop for i from s to limit
                        when (walk (1+ p) i)
                          return t)))
               ((char= #\? (char pattern p))
                (and (< s (length path))
                     (not (char= #\/ (char path s)))
                     (walk (1+ p) (1+ s))))
               (t (and (< s (length path))
                       (char= (char pattern p) (char path s))
                       (walk (1+ p) (1+ s)))))))
    (walk 0 0)))

(defun allowed-path-p (path globs root)
  "PATH is inside GLOBS, read against ROOT. No globs means everything is."
  (let ((patterns (remove "" (mapcar #'normalize-glob globs) :test #'equal)))
    (or (null patterns)
        (let ((path (normalize-path path)))
          (loop for pattern in patterns
                thereis (let ((full (if (absolute-glob-p pattern)
                                        pattern
                                        (concatenate 'string (string-right-trim "/" root)
                                                     "/" pattern))))
                          (glob-match full path)))))))

;;; --- what a call changes -------------------------------------------------------

(defstruct (write-op (:constructor make-write-op (verb path &key whole-p after old new)))
  "One thing a call writes: the verb, its path, and what it puts there."
  verb path whole-p after old new)

(defstruct (request (:constructor make-request))
  "What one call does: its writes, the commands it runs, and a definition
patch, whose target the form does not name."
  (writes '())
  (commands '())
  patch-p)

(defparameter +spawn-operators+ '("sh" "run-program" "launch-program" "run-shell-command")
  "The heads whose argument is a command the call runs.")

(defun head-name (x)
  "X's symbol name lowercased, or NIL: symbols compare by name, so a form
read in any package is recognized."
  (and (symbolp x) (string-downcase (symbol-name x))))

(defun dollar-name (x)
  "The strings-object name a ($ \"name\") form reads, or NIL."
  (and (consp x)
       (equal "$" (head-name (car x)))
       (stringp (second x))
       (null (cddr x))
       (second x)))

(defun literal-text (x strings)
  "The text X names: a string, or a ($ \"name\") the strings object resolves."
  (cond ((stringp x) x)
        ((dollar-name x) (and (hash-table-p strings) (gethash (dollar-name x) strings)))
        (t nil)))

(defun spawn-command (form strings)
  "FORM's command text, or NIL when one of its arguments hides it."
  (let ((rest (rest form)) (parts '()) (hidden nil))
    (loop while rest
          do (if (keywordp (first rest))
                 (setf rest (cddr rest))
                 (let ((text (literal-text (first rest) strings)))
                   (if (text-p text) (push text parts) (setf hidden t))
                   (setf rest (cdr rest)))))
    (when (and (not hidden) parts)
      (format nil "~{~a~^ ~}" (nreverse parts)))))

(defun text-p (x)
  (and (stringp x) (plusp (length x))))

(defun note-write (request form strings)
  "Read the write FORM names, when it names one."
  (let ((verb (head-name (car form)))
        (path (literal-text (second form) strings)))
    (cond
      ((member verb '("write-file" "append-file") :test #'equal)
       (when path
         (push (make-write-op verb path :whole-p t :after (literal-text (third form) strings))
               (request-writes request))))
      ((equal verb "edit")
       (when path
         (push (make-write-op verb path
                              :old (literal-text (third form) strings)
                              :new (literal-text (fourth form) strings))
               (request-writes request))))
      ((member verb '("delete-file") :test #'equal)
       (when path
         (push (make-write-op verb path) (request-writes request))))
      ((equal verb "rename-file")
       (let ((to (literal-text (third form) strings)))
         (when to (push (make-write-op verb to) (request-writes request)))))
      ((equal verb "with-open-file")
       (let* ((spec (second form))
              (direction (and (consp spec) (member "output" (mapcar #'head-name (cddr spec))
                                                    :test #'equal)))
              (target (and (consp spec) (literal-text (second spec) strings))))
         (when (and target direction)
           (push (make-write-op verb target :whole-p t) (request-writes request)))))
      ((equal verb "patch")
       (setf (request-patch-p request) t))
      ((member verb +spawn-operators+ :test #'equal)
       (let ((command (spawn-command form strings)))
         (when command (push command (request-commands request))))))))

(defun request-from-form (form strings)
  "What reading FORM names."
  (let ((request (make-request)))
    (labels ((walk (x)
               (when (consp x)
                 (let ((name (head-name (car x))))
                   (when name (note-write request x strings))
                   (unless (member name '("quote" "function") :test #'equal)
                     (dolist (item x) (walk item)))))))
      (walk form))
    request))

(defun first-string-literal (text start)
  "The Lisp string literal at or after START: its value, and the index after
it, or NIL."
  (let ((open (position #\" text :start (min start (length text)))))
    (when open
      (let ((out (make-string-output-stream)) (i (1+ open)) (n (length text)))
        (loop while (< i n)
              do (let ((c (char text i)))
                   (cond ((char= c #\\) (incf i)
                          (when (< i n) (write-char (char text i) out)))
                         ((char= c #\") (return))
                         (t (write-char c out))))
                 (incf i))
        (when (and (< (1+ open) n) (find #\" text :start (1+ open)))
          (values (get-output-stream-string out) (1+ i)))))))

(defun loose-request (text)
  "The writes TEXT names, found without reading it: a heredoc body or a form
the reader refuses still names its target."
  (let ((request (make-request)))
    (dolist (verb '("write-file" "edit" "patch" "delete-file"))
      (let ((at 0)
            (needle (format nil "(~a" verb)))
        (loop for found = (search needle text :start2 at)
              while found
              do (setf at (+ found 1 (length needle)))
                 (if (equal verb "patch")
                     (setf (request-patch-p request) t)
                     (multiple-value-bind (value end) (first-string-literal text at)
                       (when value
                         (push (make-write-op verb value :whole-p (equal verb "write-file"))
                               (request-writes request))
                         (setf at end)))))))
    request))

(defun read-request (text strings)
  "What the call's text names. TEXT is read as Lisp when it reads; when it
does not (a raw-text body), the write verbs it spells are found in it."
  (handler-case
      (with-input-from-string (in text)
        (let ((*read-eval* nil)
              (*package* (find-package '#:nodecode-mixer))
              (request (make-request)))
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (let ((one (request-from-form form strings)))
                     (setf (request-writes request)
                           (append (request-writes one) (request-writes request))
                           (request-commands request)
                           (append (request-commands one) (request-commands request))
                           (request-patch-p request)
                           (or (request-patch-p one) (request-patch-p request)))))
          request))
    (error () (loose-request text))))

;;; --- dependencies ---------------------------------------------------------------

(defun package-name-of (spec)
  "The package name at the head of SPEC: requests>=2, foo[extra]~=1, @scope/x."
  (let* ((spec (string-trim '(#\Space #\Tab #\Return #\") spec))
         (end (or (position-if (lambda (c) (find c "=<>!~;[@ ")) spec) (length spec))))
    (subseq spec 0 end)))

(defun manifest-kind (path)
  "The manifest kind PATH is, or NIL."
  (let ((name (string-downcase (basename path))))
    (cond ((equal name "package.json") :npm)
          ((equal name "pyproject.toml") :python)
          ((equal name "cargo.toml") :cargo)
          ((equal name "go.mod") :go)
          ((and (starts-with "requirements" name) (ends-with ".txt" name)) :requirements)
          (t nil))))

(defun npm-names (text)
  (let ((json (handler-case (shasht:read-json text) (error () nil)))
        (names '()))
    (when (hash-table-p json)
      (dolist (key '("dependencies" "devDependencies" "peerDependencies" "optionalDependencies"))
        (let ((value (gethash key json)))
          (when (hash-table-p value)
            (maphash (lambda (name version)
                       (declare (ignore version))
                       (pushnew name names :test #'equal))
                     value)))))
    names))

(defun quoted-names (line)
  "The names the quoted strings of LINE spell."
  (let ((names '()) (i 0))
    (loop
      for open = (position #\" line :start i)
      while open
      do (let ((close (position #\" line :start (1+ open))))
           (if close
               (let ((name (package-name-of (subseq line (1+ open) close))))
                 (when (and (plusp (length name)) (alpha-char-p (char name 0)))
                   (pushnew name names :test #'equal))
                 (setf i (1+ close)))
               (return))))
    names))

(defun toml-key-name (line)
  "The name LINE opens a table entry with (name = ...), or NIL."
  (let ((eq (position #\= line)))
    (when (and eq (plusp eq))
      (let ((name (package-name-of (subseq line 0 eq))))
        (and (plusp (length name)) (alpha-char-p (char name 0)) name)))))

(defun python-names (text)
  "Names in a pyproject.toml's dependency lists: the quoted requirement
strings, and the keys of optional-dependency and dependency-group tables."
  (let ((names '()) (section "") (array-p nil))
    (dolist (raw (split-lines text))
      (let ((line (string-trim '(#\Space #\Tab #\Return) raw)))
        (cond
          ((and (plusp (length line)) (char= #\[ (char line 0)))
           (setf section (string-downcase (string-trim "[] " line)))
           (setf array-p nil))
          ((or (search "dependencies" section) (search "dependency-groups" section) array-p)
           (when (search "dependencies" line) (setf array-p t))
           (when (find #\] line) (setf array-p nil))
           (when (or (search "optional-dependencies" section)
                     (equal section "dependency-groups"))
             (let ((key (toml-key-name line)))
               (when key (pushnew key names :test #'equal))))
           (dolist (name (quoted-names line)) (pushnew name names :test #'equal))))))
    names))

(defun cargo-names (text)
  (let ((names '()) (section ""))
    (dolist (raw (split-lines text))
      (let ((line (string-trim '(#\Space #\Tab #\Return) raw)))
        (cond
          ((and (plusp (length line)) (char= #\[ (char line 0)))
           (setf section (string-downcase (string-trim "[] " line))))
          ((starts-with "dependencies." section)
           (let ((name (string-trim '(#\Space)
                                     (subseq section (length "dependencies.")))))
             (when (plusp (length name)) (pushnew name names :test #'equal))))
          ((member section '("dependencies" "dev-dependencies" "build-dependencies")
                   :test #'equal)
           (let ((key (toml-key-name line)))
             (when key (pushnew key names :test #'equal)))))))
    names))

(defun go-names (text)
  (let ((names '()) (in-block nil))
    (dolist (raw (split-lines text))
      (let ((line (string-trim '(#\Space #\Tab #\Return) raw)))
        (cond
          ((starts-with "require (" line) (setf in-block t))
          ((and in-block (char= #\) (char line 0))) (setf in-block nil))
          ((starts-with "require " line)
           (let ((words (split-words (subseq line (length "require ")))))
             (when words (pushnew (first words) names :test #'equal))))
          (in-block
           (let ((words (split-words line)))
             (when words (pushnew (first words) names :test #'equal)))))))
    names))

(defun requirements-names (text)
  (let ((names '()))
    (dolist (raw (split-lines text))
      (let ((line (string-trim '(#\Space #\Tab #\Return) raw)))
        (unless (or (zerop (length line))
                    (char= #\# (char line 0))
                    (char= #\- (char line 0)))
          (let ((name (package-name-of line)))
            (when (plusp (length name)) (pushnew name names :test #'equal))))))
    names))

(defun manifest-names (kind text)
  "The dependency names TEXT declares, as KIND's manifest spells them."
  (case kind
    (:npm (npm-names text))
    (:python (python-names text))
    (:cargo (cargo-names text))
    (:go (go-names text))
    (:requirements (requirements-names text))
    (t '())))

(defun install-new-packages (command)
  "The package names COMMAND would install, or NIL when it only restores."
  (let* ((words (split-words command))
         (program (and words (basename (first words))))
         (args (rest words)))
    (labels ((non-flag (items)
               (remove-if (lambda (w) (or (zerop (length w)) (char= #\- (char w 0)))) items))
             (names (items)
               (remove-if (lambda (w) (member w '("." "..") :test #'equal)) (non-flag items))))
      (cond
        ((member program '("npm" "pnpm" "yarn" "bun") :test #'equal)
         (when (member (first args) '("add" "install" "i") :test #'equal)
           (names (rest args))))
        ((member program '("pip" "pip3") :test #'equal)
         (when (member "install" args :test #'equal)
           (let ((after (cdr (member "install" args :test #'equal))))
             (unless (intersection after '("-r" "--requirement" "-e" "--editable") :test #'equal)
               (names after)))))
        ((equal program "uv")
         (cond ((equal (first args) "add") (names (rest args)))
               ((and (equal (first args) "pip") (member "install" args :test #'equal))
                (let ((after (cdr (member "install" args :test #'equal))))
                  (unless (intersection after '("-r" "--requirement") :test #'equal)
                    (names after))))))
        ((member program '("poetry" "cargo") :test #'equal)
         (when (equal (first args) "add") (names (rest args))))
        ((equal program "go")
         (when (equal (first args) "get")
           (remove-if (lambda (w) (or (search "..." w) (char= #\. (char w 0))))
                      (non-flag (rest args)))))
        (t nil)))))

(defun install-ecosystem (command)
  "The ecosystem COMMAND installs into."
  (let ((program (basename (first (split-words command)))))
    (cond ((member program '("npm" "pnpm" "yarn" "bun") :test #'equal) "npm")
          ((equal program "cargo") "cargo")
          ((equal program "go") "go")
          (t "pypi"))))

;;; --- shell: write targets, and what plan mode may run ----------------------------

(defun unquoted-text (text)
  "TEXT with quoted regions emptied: what the shell itself would read."
  (let ((out (make-string-output-stream)) (quote nil) (i 0) (n (length (or text "")))
        (text (or text "")))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond (quote (when (char= c quote) (setf quote nil)))
                     ((member c '(#\" #\')) (setf quote c) (write-char c out))
                     ((char= c #\\) (incf i) (write-char c out)
                      (when (< i n) (write-char (char text i) out)))
                     (t (write-char c out)))
               (incf i)))
    (get-output-stream-string out)))

(defun redirect-targets (text)
  "The tokens TEXT redirects into, one per > and >>."
  (let ((bare (unquoted-text text)) (targets '()) (i 0) (n (length (or text ""))))
    (setf n (length bare))
    (loop while (< i n)
          do (if (char= #\> (char bare i))
                 (let ((start (1+ i)))
                   (loop while (and (< start n) (member (char bare start) '(#\> #\|))) do (incf start))
                   (let ((end (or (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\; #\& #\|))) bare :start start) n)))
                     (push (subseq bare start end) targets)
                     (setf i end)))
                 (incf i)))
    (nreverse targets)))

(defun command-write-targets (command)
  "The paths COMMAND appears to write, best effort: (path . directory-p)."
  (let ((words (split-words command))
        (targets '()))
    (when words
      (let ((program (basename (first words)))
            (args (rest words)))
        (flet ((target (path directory-p)
                 (let ((path (string-trim '(#\" #\') path)))
                   (unless (or (zerop (length path))
                               (member path '("/dev/null" "&1" "&2") :test #'equal))
                     (push (cons path directory-p) targets)))))
          (loop for word in words
                for next in (rest words)
                when (or (equal word ">") (equal word ">>"))
                  do (target next nil))
          (loop for word in words
                when (and (plusp (length word)) (char= #\> (char word 0)))
                  do (let ((rest (string-left-trim ">" word)))
                       (when (plusp (length rest)) (target rest nil))))
          (cond
            ((equal program "tee")
             (dolist (word (remove-if (lambda (w) (char= #\- (char w 0))) args))
               (target word nil)))
            ((member program '("touch" "rm" "truncate") :test #'equal)
             (dolist (word (remove-if (lambda (w) (char= #\- (char w 0))) args))
               (target word nil)))
            ((member program '("mkdir" "rmdir") :test #'equal)
             (dolist (word (remove-if (lambda (w) (char= #\- (char w 0))) args))
               (target word t)))
            ((member program '("cp" "mv" "ln" "install") :test #'equal)
             (let ((rest (remove-if (lambda (w) (char= #\- (char w 0))) args)))
               (when (cdr rest) (target (car (last rest)) nil))))
            ((and (equal program "sed")
                  (find-if (lambda (w) (and (char= #\- (char w 0)) (find #\i w))) args))
             (let ((rest (remove-if (lambda (w) (char= #\- (char w 0))) args)))
               (when (cdr rest) (target (car (last rest)) nil))))
            ((equal program "dd")
             (dolist (word args)
               (when (starts-with "of=" word) (target (subseq word 3) nil))))))))
    (nreverse targets)))

(defparameter +read-only-programs+
  '("ls" "cat" "head" "tail" "wc" "grep" "rg" "find" "sed" "sort" "uniq" "cut" "diff"
    "cmp" "stat" "du" "df" "file" "jq" "echo" "printf" "basename" "dirname" "realpath"
    "cd" "pwd" "which" "date" "whoami" "true" "test" "tree" "nl")
  "What plan mode lets the shell run, with the argument checks below.")

(defparameter +git-read-only+
  '("status" "log" "diff" "show" "grep" "ls-files" "ls-tree" "blame" "describe" "shortlog"
    "rev-parse" "merge-base" "cat-file" "show-ref" "whatchanged" "name-rev" "for-each-ref")
  "The git subcommands plan mode reads with.")

(defun program-read-only-p (program args)
  "PROGRAM's ARGS keep it read-only."
  (cond
    ((equal program "find")
     (not (find-if (lambda (w) (or (member w '("-delete" "-exec" "-execdir" "-ok" "-okdir" "-fls")
                                    :test #'equal)
                                    (starts-with "-fprint" w)))
                   args)))
    ((equal program "sed")
     (and (find-if (lambda (w) (member w '("-n" "-En" "-ne" "-nE") :test #'equal)) args)
          (not (find-if (lambda (w) (and (char= #\- (char w 0))
                                          (not (char= #\/ (char w 1)))
                                          (find #\i w)))
                        args))))
    ((equal program "sort") (not (find-if (lambda (w) (member w '("-o" "--output") :test #'equal)) args)))
    ((equal program "uniq") (<= (length (remove-if (lambda (w) (char= #\- (char w 0))) args)) 1))
    (t t)))

(defun git-read-only-p (args)
  "GIT's ARGS stay read-only."
  (let ((subcommand (find-if (lambda (w) (not (char= #\- (char w 0)))) args)))
    (cond
      ((null subcommand) t)
      ((equal subcommand "config")
       (and (member "--get" args :test #'equal) (not (member "--unset" args :test #'equal))))
      ((equal subcommand "tag") (or (null (rest (member subcommand args)))
                                     (find-if (lambda (w) (member w '("-l" "--list") :test #'equal)) args)))
      ((equal subcommand "branch")
       (not (find-if (lambda (w) (member w '("-d" "-D" "-m" "-M" "-f" "-c" "-C" "--delete" "--move")
                                     :test #'equal))
                     args)))
      ((equal subcommand "remote")
       (every (lambda (w) (member w '("-v" "--verbose") :test #'equal))
              (rest (member subcommand args))))
      (t (member subcommand +git-read-only+ :test #'equal)))))

(defun redirects-read-only-p (text)
  "Every redirect in TEXT goes nowhere or into this shell."
  (every (lambda (target)
           (or (equal target "/dev/null")
               (starts-with "&" target)
               (equal target "")))
         (redirect-targets text)))

(defun read-only-command-p (command)
  "COMMAND writes nothing and runs no project code, as plan mode reads it."
  (let* ((bare (unquoted-text command))
         (words (split-words command)))
    (and words
         (not (find-if (lambda (needle) (search needle bare))
                       '("$(" "`" "<(" ">(" "<<" ";")))
         (not (find #\< bare))
         (redirects-read-only-p command)
         (let ((program (basename (first words))))
           (cond
             ((equal program "git") (git-read-only-p (rest words)))
             ((member program +read-only-programs+ :test #'equal)
              (program-read-only-p program (rest words)))
             (t nil))))))

;;; --- the dials, per session ------------------------------------------------------

(defvar *saved* nil
  "The saved defaults, as a plist: what the section said, or what /mixer save
last wrote.")

(defvar *overrides* (make-hash-table :test #'equal)
  "Session id -> a hash of the dials that session overrode.")

(defvar *approvals* (make-hash-table :test #'equal)
  "Session id and rule key -> T, once the operator allowed that file or package.")

(defvar *lines-used* (make-hash-table :test #'equal)
  "Session id -> lines reserved in the current turn.")

(defvar *turn-of-session* (make-hash-table :test #'equal)
  "Session id -> the turn the budget above counts for.")

(defun session-overrides (session-id)
  (and session-id (gethash session-id *overrides*)))

(defun set-override (session-id key value)
  (let ((table (or (session-overrides session-id)
                   (setf (gethash session-id *overrides*) (make-hash-table :test #'equal)))))
    (setf (gethash key table) value)
    value))

(defun dial-value (session-id key default)
  "SESSION-ID's own KEY, or DEFAULT: a dial set to nothing is still set."
  (let ((table (session-overrides session-id)))
    (if (null table)
        default
        (multiple-value-bind (value present) (gethash key table)
          (if present value default)))))

(defun overridden-p (session-id key)
  "Whether SESSION-ID set KEY itself, even to nothing."
  (let ((table (session-overrides session-id)))
    (and table (nth-value 1 (gethash key table)))))

(defun override (session-id key)
  "The session's own value for KEY, or NIL when it took the saved one."
  (and (overridden-p session-id key) (gethash key (session-overrides session-id))))

(defun dials-for (session-id)
  "The four dials in force: the session's own, else the saved defaults."
  (let ((saved (or *saved* '())))
    (list :edits (dial-value session-id :edits (getf saved :edits))
          :scope (dial-value session-id :scope (getf saved :scope))
          :lines (dial-value session-id :lines (getf saved :lines))
          :deps (dial-value session-id :deps (getf saved :deps)))))

(defun approve (session-id key)
  (when session-id (setf (gethash key (or (gethash session-id *approvals*)
                                           (setf (gethash session-id *approvals*)
                                                 (make-hash-table :test #'equal))))
                         t))
  key)

(defun approved-p (session-id key)
  (let ((table (and session-id (gethash session-id *approvals*))))
    (and table (gethash key table) t)))

(defun new-turn-p (session-id turn-id)
  "SESSION-ID's first call of TURN-ID."
  (multiple-value-bind (known present) (gethash session-id *turn-of-session*)
    (or (not present) (not (equal known turn-id)))))

(defun reserve-lines (session-id turn-id need limit)
  "Reserve NEED lines of LIMIT for SESSION-ID's TURN-ID.
=> allowed, used (after the reservation), remaining."
  (when (new-turn-p session-id turn-id)
    (setf (gethash session-id *turn-of-session*) turn-id
          (gethash session-id *lines-used*) 0))
  (let* ((used (+ (gethash session-id *lines-used* 0) need))
         (allowed (<= used limit)))
    (setf (gethash session-id *lines-used*) used)
    (values allowed (- used need) (max (- limit used) 0))))

(defun lines-used (session-id)
  (gethash session-id *lines-used* 0))

;;; --- one change, read to what the dials ask --------------------------------------

(defstruct (change (:constructor make-change))
  verb path resolved before after lines new-deps dep-kind)

(defun read-file-text (path)
  "PATH's text, or NIL when it is not there."
  (when (and (stringp path) (probe-file path))
    (handler-case (uiop:read-file-string path) (error () nil))))

(defun make-change-from-write (write session base)
  "WRITE read as the dials need it: its resolved path, the text before and
after, the lines it changes, and the dependency names it adds."
  (let* ((path (write-op-path write))
         (resolved (resolve-path path base))
         (whole-p (write-op-whole-p write))
         (before (read-file-text resolved))
         (kind (manifest-kind resolved))
         (after (cond (whole-p (write-op-after write))
                      ((and (write-op-old write) before)
                       (replace-once before (write-op-old write) (write-op-new write)))
                      (t nil)))
         (lines (cond
                  ((member (write-op-verb write) '("delete-file") :test #'equal) 0)
                  ((and whole-p after) (changed-line-count (or before "") after))
                  ((and (write-op-old write) (write-op-new write))
                   (if after
                       (changed-line-count (or before "") after)
                       (+ (line-count (write-op-old write)) (line-count (write-op-new write)))))
                  (t 0)))
         (new-deps (when kind
                     (if after
                         (set-difference (manifest-names kind after)
                                         (manifest-names kind (or before ""))
                                         :test #'equal)
                         (set-difference (manifest-names kind (or (write-op-new write) ""))
                                         (manifest-names kind (or (write-op-old write) ""))
                                         :test #'equal)))))
    (make-change :verb (write-op-verb write)
                 :path path
                 :resolved resolved
                 :before before
                 :after after
                 :lines lines
                 :new-deps (remove-duplicates new-deps :test #'equal)
                 :dep-kind kind)))

;;; --- the decision ---------------------------------------------------------------

(defparameter +no-bypass+
  "Do not retry it through another route: a shell command that writes this is read the same way."
  "Every refusal says the same thing: the gate reads every route.")

(defun deny (stage text)
  (list :action :deny :stage stage :text text))

(defun ask (stage text key)
  (list :action :ask :stage stage :text text :key key))

(defun edit-key (change)
  (format nil "edit:~a" (change-resolved change)))

(defun dep-key (names)
  (format nil "dep:~{~a~^,~}" names))

(defun readable-globs (globs)
  (if globs (format nil "~{~a~^, ~}" globs) "(none)"))

(defun plan-write-text (change)
  (format nil "ERROR: blocked by mixer:edits: plan mode is read-only; ~a was prevented. ~a Leave plan mode with /mixer edits auto (or ask)."
          (change-verb change) +no-bypass+))

(defun scope-text (path globs)
  (format nil "ERROR: blocked by mixer:scope: '~a' is not in scope (allowed globs: ~a). ~a Add a glob with /mixer scope add <glob>, or write inside the allowed paths."
          path (readable-globs globs) +no-bypass+))

(defun lines-text (need used budget remaining)
  (format nil "ERROR: blocked by mixer:lines: this change is ~a lines (added+removed), and the turn has already reserved ~a of its ~a-line budget, ~a left. ~a Stop and summarise the turn, or raise the budget with /mixer lines N."
          need used budget remaining +no-bypass+))

(defun deps-block-text (names)
  (format nil "ERROR: blocked by mixer:deps: this would add new dependencies (~{~a~^, ~}). ~a Raise it deliberately with /mixer deps ask or allow."
          names +no-bypass+))

(defun deps-ask-text (names)
  (format nil "mixer:deps: this would add new dependencies (~{~a~^, ~}). The call waits for your approval - deny it if you did not intend a new dependency."
          names))

(defun edits-ask-text (change)
  (format nil "mixer:edits needs your approval for ~a on ~a. The call waits for you - approve, allow this file for the session, or deny."
          (change-verb change) (change-resolved change)))

(defun session-root (session-id)
  "The folder SESSION-ID works in, or this image's: what a relative glob
anchors at, and what a relative write resolves against."
  (let* ((cwd (and (stringp session-id) (ignore-errors (nlk:find-session-cwd session-id))))
         (text (typecase cwd
                 (pathname (namestring cwd))
                 (string cwd)
                 (t (namestring (uiop:getcwd))))))
    (string-right-trim "/" (normalize-path text))))

(defun decide (request session base dials turn-id)
  "Run the stages on REQUEST in order - plan, scope, deps, lines, ask - and
answer the first decision, or NIL when nothing restricts it."
  (let* ((edits (string-downcase (or (getf dials :edits) "auto")))
         (globs (getf dials :scope))
         (budget (or (getf dials :lines) 0))
         (deps (string-downcase (or (getf dials :deps) "ask")))
         (writes (request-writes request))
         (commands (request-commands request))
         (changes (mapcar (lambda (write) (make-change-from-write write session base)) writes)))

    ;; 1. plan: read-only, and the shell limited to what writes nothing.
    (when (equal edits "plan")
      (when changes
        (return-from decide (deny :edits (plan-write-text (first changes)))))
      (when (request-patch-p request)
        (return-from decide
          (deny :edits (format nil "ERROR: blocked by mixer:edits: plan mode is read-only; patch was prevented. ~a Leave plan mode with /mixer edits auto (or ask)."
                                +no-bypass+))))
      (loop for command in commands
            for index from 1
            unless (read-only-command-p command)
              do (return-from decide
                   (deny :edits (format nil "ERROR: blocked by mixer:edits: plan mode limits the shell to read-only commands; segment ~a (~a) is not read-only. ~a Leave plan mode with /mixer edits auto (or ask)."
                                         index command +no-bypass+)))))

    ;; 2. scope: every target, the shell's best-effort ones included.
    (when globs
      (dolist (change changes)
        (unless (allowed-path-p (change-resolved change) globs base)
          (return-from decide (deny :scope (scope-text (change-resolved change) globs)))))
      (when (request-patch-p request)
        (return-from decide
          (deny :scope (format nil "ERROR: blocked by mixer:scope: cannot identify the files this patch touches. ~a Use a path-based edit instead."
                                +no-bypass+))))
      (dolist (command commands)
        (dolist (target (command-write-targets command))
          (let ((resolved (resolve-path (car target) base)))
            (unless (or (allowed-path-p resolved globs base)
                        (and (cdr target)
                             (allowed-path-p (concatenate 'string (string-right-trim "/" resolved) "/x")
                                             globs base)))
              (return-from decide
                (deny :scope
                      (format nil "ERROR: blocked by mixer:scope: this command writes '~a' (best-effort read of the command), which is not in scope (allowed globs: ~a). ~a Add a glob with /mixer scope add <glob>, or write inside the allowed paths."
                              resolved (readable-globs globs) +no-bypass+))))))))

    ;; 3. deps: new names in a manifest, or a package an install command adds.
    (when (member deps '("ask" "block") :test #'equal)
      (let ((found '()))
        (dolist (change changes)
          (setf found (append found (change-new-deps change))))
        (dolist (command commands)
          (setf found (append found (install-new-packages command))))
        (setf found (remove-duplicates found :test #'equal))
        (when found
          (return-from decide
            (if (equal deps "block")
                (deny :deps (deps-block-text found))
                (ask :deps (deps-ask-text found) (dep-key found)))))))

    ;; 4. lines: reserved before it runs, so a denied call cannot spend twice.
    (when (plusp budget)
      (let ((total (reduce #'+ changes :key #'change-lines :initial-value 0)))
        (when (plusp total)
          (multiple-value-bind (allowed used remaining)
              (reserve-lines session turn-id total budget)
            (unless allowed
              (return-from decide
                (deny :lines (lines-text total used budget remaining))))))))

    ;; 5. ask: the operator's approval, one rule key per file.
    (when (equal edits "ask")
      (let ((unapproved (find-if (lambda (change) (not (approved-p session (edit-key change))))
                                 changes)))
        (when (and unapproved (not (approved-p session "*")))
          (return-from decide (ask :edits (edits-ask-text unapproved) (edit-key unapproved))))))
    nil))
