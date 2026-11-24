;;;; cell.lisp --- a slot queue, a memory gate, a cap and a watchdog for heavy shell jobs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Hermes's headroom, rewritten against Nodecode's own seams. A heavy shell
;;;; command -- an install, a build, a test suite, a browser, a compiler --
;;;; does four things here that a light one does not, and the model is told
;;;; each of them in the command's own result: it waits for one of a few
;;;; machine-wide slots, it waits until the host has the memory to start it,
;;;; it runs with its parallelism and its Node heap bounded, and it runs under
;;;; a watchdog that stops a process of the job when available memory crosses
;;;; the critical line.
;;;;
;;;; The seam is the :TOOL point, which wraps one tool dispatch: OP is
;;;; (:name "eval" :arguments HASH :call-id ID) and the arguments carry the
;;;; form the model wrote. The cell reads that form's own spawn calls
;;;; ((sh "..."), (uiop:run-program "..."), a ($ "name") in the strings
;;;; object), classifies each command, and hands NEXT a rebuilt form with the
;;;; lightened command in place -- so what runs is what the form the model
;;;; wrote says, and the difference is said in the result. A call that has to
;;;; wait and cannot is answered with FAILURE, the same refusal a tool of its
;;;; own gives; the turn does not die.
;;;;
;;;; What the original bought with systemd --user scopes -- a hard memory
;;;; cap, a raised OOM score, a cgroup of the job's own -- a Lisp image
;;;; cannot take for one child without a helper, so this keeps the three
;;;; that need none: the queue, the gate and the watchdog. README.md, Gaps.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(in-package #:nodecode-headroom)

(defparameter *proc-root* "/proc/"
  "Where the kernel's tables are read from. A test points this at a
fabricated tree, which is the only I/O this cell has.")

(defparameter +sample-seconds+ 2
  "How often the watchdog looks at memory, and how long a gate sleeps.")

(defparameter +event-limit+ 200
  "How many events the log keeps, newest first.")

(defun plist-get (plist key)
  (getf plist key))

(defun format-mb (mb)
  (cond ((null mb) "?")
        ((>= mb 1024) (format nil "~,1f GB" (/ mb 1024.0)))
        (t (format nil "~d MB" mb))))

;;; --- the host, read-only ----------------------------------------------------

(defstruct memory
  "MemTotal, MemAvailable and swap, in MB, as /proc/meminfo reports them."
  total available swap-total swap-used)

(defun read-proc (relative)
  "RELATIVE's text under *PROC-ROOT*, or NIL when it is unreadable or gone."
  (let ((path (merge-pathnames relative *proc-root*)))
    (when (probe-file path)
      (ignore-errors (uiop:read-file-string path)))))

(defun read-memory ()
  "The host's memory as a MEMORY, or NIL where /proc/meminfo cannot be read."
  (let ((text (read-proc "meminfo")))
    (when text
      (let ((values (make-hash-table :test #'equal)))
        (dolist (line (uiop:split-string text :separator '(#\Newline)))
          (let ((colon (position #\: line)))
            (when colon
              (let* ((key (subseq line 0 colon))
                     (kb (parse-integer (subseq line (1+ colon)) :junk-allowed t)))
                (when (and kb (member key '("MemTotal" "MemAvailable" "SwapTotal" "SwapFree")
                                      :test #'equal))
                  (setf (gethash key values) kb))))))
        (when (gethash "MemTotal" values)
          (flet ((mb (key) (floor (or (gethash key values) 0) 1024)))
            (make-memory :total (mb "MemTotal")
                         :available (mb "MemAvailable")
                         :swap-total (mb "SwapTotal")
                         :swap-used (- (mb "SwapTotal") (mb "SwapFree")))))))))

(defun copy-object (object)
  "OBJECT's entries in a fresh table of the same test."
  (let ((copy (make-hash-table :test (hash-table-test object)
                                :size (max 8 (hash-table-size object)))))
    (maphash (lambda (key value) (setf (gethash key copy) value)) object)
    copy))

(defun read-pressure ()
  "Memory pressure, (VALUES SOME FULL) as PSI's avg10 percentages, or NIL."
  (let ((text (read-proc "pressure/memory")))
    (when text
      (flet ((avg (kind)
               (let ((start (search (format nil "~a avg10=" kind) text)))
                 (when start
                   (let ((end (position #\Space text :start (+ start (length kind) 7))))
                     (ignore-errors
                       (let ((value (subseq text (+ start (length kind) 7) end)))
                         (parse-integer value :end (or (position #\. value) (length value))))))))))
        (let ((some (avg "some")) (full (avg "full")))
          (when some (values some (or full 0))))))))

(defun cpu-count ()
  "How many processors /proc/cpuinfo lists, at least one."
  (let ((text (read-proc "cpuinfo")))
    (max 1
         (if text
             (loop for line in (uiop:split-string text :separator '(#\Newline))
                   count (uiop:string-prefix-p "processor" line))
             1))))

(defun proc-pids ()
  "Every pid /proc lists, as integers."
  (loop for dir in (ignore-errors (uiop:subdirectories (merge-pathnames "*/" *proc-root*)))
        for name = (car (last (pathname-directory dir)))
        when (and name (plusp (length name)) (every #'digit-char-p name))
          collect (parse-integer name)))

(defun process-facts (pid)
  "PID's (VALUES RSS-MB NAME), or NIL when it is gone or unreadable."
  (let ((text (read-proc (format nil "~d/status" pid))))
    (when text
      (let ((name "?") (rss 0))
        (dolist (line (uiop:split-string text :separator '(#\Newline)))
          (cond ((uiop:string-prefix-p "Name:" line)
                 (setf name (string-trim " " (subseq line 5))))
                ((uiop:string-prefix-p "VmRSS:" line)
                 (setf rss (or (parse-integer (subseq line 6) :junk-allowed t) 0)))))
        (values (floor rss 1024) name)))))

(defun process-parent (pid)
  "PID's parent, or NIL. The stat line's comm may hold spaces and parens, so
the fields are read after its last close paren."
  (let ((text (read-proc (format nil "~d/stat" pid))))
    (when text
      (let ((close (position #\) text :from-end t)))
        (when close
          (let ((fields (uiop:split-string (string-trim " " (subseq text (1+ close)))
                                            :separator '(#\Space))))
            (ignore-errors (parse-integer (second fields)))))))))

(defun descendants (root)
  "Every pid whose ancestry reaches ROOT."
  (let ((children (make-hash-table)))
    (dolist (pid (proc-pids))
      (let ((parent (process-parent pid)))
        (when parent (push pid (gethash parent children)))))
    (let ((seen '()) (queue (list root)))
      (loop while queue
            for pid = (pop queue)
            do (dolist (child (gethash pid children))
                 (unless (member child seen)
                   (push child seen)
                   (push child queue))))
      seen)))

(defun top-processes (&key (limit 5) (min-mb 40))
  "The largest processes on the host, ((MB NAME PID) ...), biggest first."
  (let ((rows '()))
    (dolist (pid (proc-pids))
      (multiple-value-bind (mb name) (process-facts pid)
        (when (and mb (>= mb min-mb))
          (push (list mb (or name "?") pid) rows))))
    (subseq (sort rows #'> :key #'first) 0 (min limit (length rows)))))

;;; --- what a command costs ----------------------------------------------------

(defstruct segment start end text)

(defparameter +separators+ '(#\; #\& #\| #\Newline)
  "What ends one simple command and starts the next when unquoted.")

(defun split-segments (command)
  "COMMAND cut at its unquoted ; & | and newlines, as SEGMENTs with offsets.
Quotes are respected well enough to classify; the result is never run."
  (let ((segments '()) (start 0) (i 0) (n (length command)) (quote nil))
    (labels ((push-segment (end)
               (let* ((raw (subseq command start end))
                      (lead (- (length raw) (length (string-left-trim '(#\Space #\Tab) raw))))
                      (text (string-trim '(#\Space #\Tab) raw)))
                 (when (plusp (length text))
                   (push (make-segment :start (+ start lead) :end (+ start lead (length text))
                                       :text text)
                         segments)))))
      (loop while (< i n)
            do (let ((ch (char command i)))
                 (cond (quote
                        (cond ((char= ch #\\) (incf i))
                              ((char= ch quote) (setf quote nil))))
                       ((or (char= ch #\') (char= ch #\")) (setf quote ch))
                       ((member ch +separators+)
                        (push-segment i)
                        (when (and (< (1+ i) n) (char= (char command (1+ i)) ch))
                          (incf i))
                        (setf start (1+ i)))
                       (t nil)))
               (incf i))
      (push-segment n))
    (nreverse segments)))

(defparameter +wrappers+
  (list "^[A-Za-z_][A-Za-z0-9_]*=(?:'[^']*'|\"(?:[^\"\\\\]|\\\\.)*\"|[^\\s'\"]*)\\s+"
        "^(?:env|time|nice|nohup|exec|command|builtin|stdbuf|sudo|caffeinate|ionice)(?:\\s+-\\S+)*\\s+")
  "Leading assignments and wrappers a command word may hide behind.")

(defun command-words (text)
  "TEXT with its leading env assignments and wrappers cut off; its command word first."
  (loop
    for before = text
    do (dolist (pattern +wrappers+)
         (multiple-value-bind (start end) (cl-ppcre:scan pattern text)
           (when (and start (zerop start))
             (setf text (subseq text end)))))
    unless (string/= text before) return text))

(defparameter +rules+
  '(("js-install" "js" "^(?:npm|pnpm|yarn|bun)\\s+(?:install|i|ci|add|update|upgrade)\\b|^yarn\\s*$")
    ("js-script" "js" "^(?:npm|pnpm|yarn|bun)\\s+(?:run(?:-script)?\\s+)?(?:build|test|t|e2e|lint|typecheck|type-check|check|compile|bundle|start)\\b")
    ("js-tool" "js" "^(?:(?:npx|bunx)(?:\\s+-\\S+)*\\s+|(?:pnpm|yarn)\\s+(?:exec\\s+|dlx\\s+)?)?(?:jest|vitest|playwright|cypress|tsc|vite|webpack|next|nuxt|esbuild|turbo|nx|eslint|rollup|svelte-check|astro)(?:\\s|$)")
    ("py-install" "python" "^(?:pip3?|python3?(?:\\.\\d+)?\\s+-m\\s+pip|uv\\s+pip|pipx)\\s+install\\b")
    ("py-test" "python" "^(?:pytest|py\\.test|python3?(?:\\.\\d+)?\\s+-m\\s+(?:pytest|unittest|mypy)|mypy|pyright|tox|nox)(?:\\s|$)")
    ("rust" "rust" "^cargo\\s+(?:\\+\\S+\\s+)?(?:build|b|test|t|check|c|clippy|run|r|install|bench|nextest)\\b|^rustc\\s")
    ("go" "go" "^go\\s+(?:build|test|install|vet|run|generate|mod\\s+download)\\b")
    ("jvm" "jvm" "^(?:\\./gradlew|gradlew|gradle|mvn|sbt|lein|bazel|javac)(?:\\s|$)")
    ("native" "native" "^(?:make|gmake|ninja|cmake\\s+--build|meson\\s+(?:compile|test)|gcc|g\\+\\+|clang|clang\\+\\+|cc|zig\\s+build|swift\\s+build|dotnet\\s+(?:build|test|publish|restore))(?:\\s|$)")
    ("container" "container" "^(?:docker|podman|nerdctl)\\s+(?:build|compose\\s+(?:up|build)|run)\\b")
    ("media" "media" "^(?:ffmpeg|magick|convert|whisper|llama-cli|ollama\\s+(?:run|serve|pull)|blender|tesseract|ocrmypdf)(?:\\s|$)")
    ("browser" "browser" "^(?:chromium|chromium-browser|google-chrome(?:-stable)?|firefox|python3?\\s+-m\\s+playwright\\s+install)(?:\\s|$)"))
  "The command families that are heavy, (NAME KIND PATTERN). A family is what
decides the flag a rewrite adds, so the table is data.")

(defparameter +query-only+ "^\\S+(?:\\s+\\S+){0,2}\\s+(?:--version|-V|--help|-h|help)\\s*$"
  "Asking a tool about itself is never heavy.")

(defun compile-patterns (strings)
  "STRINGS as scanners, a bad pattern dropped rather than fatal."
  (loop for pattern in strings
        for scanner = (ignore-errors (cl-ppcre:create-scanner pattern))
        when scanner collect scanner))

(defun any-match (scanners text)
  (some (lambda (scanner) (cl-ppcre:scan scanner text)) scanners))

(defun classify-command (command)
  "Whether COMMAND is heavy, and what it matched: a plist
(:heavy BOOL :rules (NAME...) :kinds (KIND...))."
  (let ((matches '()) (custom nil))
    (when (and (stringp command) (plusp (length (string-trim " " command))))
      (unless (any-match (setting :exclude) command)
        (dolist (segment (split-segments command))
          (let ((words (command-words (segment-text segment))))
            (unless (or (zerop (length words)) (cl-ppcre:scan +query-only+ words))
              (let ((rule (find-if (lambda (rule) (cl-ppcre:scan (third rule) words)) +rules+)))
                (cond (rule (push rule matches))
                      ((any-match (setting :extra) words)
                       (push (list "custom" "custom" nil) matches)
                       (setf custom t)))))))))
    (list :heavy (and matches t)
          :rules (remove-duplicates (mapcar #'first matches) :test #'equal)
          :kinds (remove-duplicates (mapcar #'second matches) :test #'equal)
          :custom (and custom t))))

;;; --- making it lighter -------------------------------------------------------

(defun text-at (text index)
  "TEXT's character at INDEX, or NIL."
  (and (<= 0 index) (< index (length text)) (char text index)))

(defun splice (text start end replacement)
  "TEXT with REPLACEMENT where START..END stood."
  (concatenate 'string (subseq text 0 start) replacement (subseq text end)))

(defun jobs-for (budget-mb cpus)
  "How many workers BUDGET-MB can feed on CPUS, at least one."
  (let ((per (if (plusp budget-mb) (floor budget-mb 700) 1)))
    (max 1 (min cpus (max 1 per)))))

(defun node-based-p (words)
  "Whether WORDS boots a JavaScript runtime the Node heap cap applies to."
  (cl-ppcre:scan "^(?:npm|pnpm|yarn|bun|bunx|npx|node)\\b" words))

(defun lighten (command &key budget-mb (cpus (cpu-count)) low-memory)
  "COMMAND with its parallelism and Node heap bounded, and why:
(VALUES COMMAND CHANGES). A light command comes back as itself, unchanged."
  (let ((verdict (classify-command command)))
    (if (not (plist-get verdict :heavy))
        (values command nil)
        (let* ((jobs (jobs-for budget-mb cpus))
               (cap (and (plusp budget-mb) (max 128 (round (* budget-mb 0.7)))))
               (edits '())
               (changes '()))
          (dolist (segment (split-segments command))
            (let* ((words (command-words (segment-text segment)))
                   (rule (find-if (lambda (rule) (cl-ppcre:scan (third rule) words)) +rules+)))
              (when rule
                (let ((kind (second rule)))
                  (when (and cap (node-based-p words)
                             (not (search "max-old-space-size" (segment-text segment)))
                             (not (cl-ppcre:scan "(?:^|\\s)NODE_OPTIONS=" (segment-text segment))))
                    (push (list :at (segment-start segment) :text
                                (format nil "NODE_OPTIONS=\"${NODE_OPTIONS:+$NODE_OPTIONS }--max-old-space-size=~d\" " cap))
                          edits)
                    (setf changes (adjoin (format nil "Node heap capped at ~d MB" cap) changes
                                          :test #'equal)))
                  (labels ((suffix (flag why)
                             (push (list :at (segment-end segment) :text (concatenate 'string " " flag)) edits)
                             (setf changes (adjoin why changes :test #'equal))))
                    (cond
                      ((and (cl-ppcre:scan "(?:^|\\s|/)jest(?:\\s|$)" words) low-memory
                            (not (cl-ppcre:scan "\\s(?:--runInBand|-i|--maxWorkers|-w\\s?\\d)" words)))
                       (suffix "--runInBand" "jest --runInBand: one process"))
                      ((and (cl-ppcre:scan "(?:^|\\s|/)playwright\\s+test\\b" words) low-memory
                            (not (cl-ppcre:scan "\\s(?:--workers|-j)(?:=|\\s)" words)))
                       (suffix "--workers=1" "playwright --workers=1"))
                      ((and (equal kind "native") (cl-ppcre:scan "^make\\b" words))
                       (multiple-value-bind (match jobs-matched)
                             (cl-ppcre:scan-to-strings "\\s(?:-j|--jobs)\\s*(\\d+)" (segment-text segment))
                           (declare (ignore match))
                           (when (and jobs-matched (> (parse-integer (aref jobs-matched 0)) jobs))
                           (push (list :at (segment-end segment) :text (format nil " -j~d" jobs)) edits)
                           (setf changes (adjoin (format nil "make -j~d" jobs) changes :test #'equal)))))
                      ((and (equal kind "rust") (cl-ppcre:scan "^cargo\\s+\\S+" words))
                       (suffix (format nil "-j ~d" jobs) (format nil "cargo -j ~d" jobs)))
                      ((and (equal kind "go") (cl-ppcre:scan "^go\\s+\\S+" words)
                            (not (cl-ppcre:scan "\\s-p\\s*\\d" words)))
                       (suffix (format nil "-p ~d" jobs) (format nil "go -p ~d" jobs)))
                      ((and (equal kind "jvm") (cl-ppcre:scan "^gradle" words)
                            (not (search "--max-workers" words)))
                       (suffix (format nil "--max-workers=~d" jobs)
                               (format nil "gradle --max-workers=~d" jobs)))))))))
          (if (null edits)
              (values command nil)
              (let ((out command))
                (loop for edit in (sort (copy-list edits) #'> :key (lambda (edit) (plist-get edit :at)))
                      do (let ((at (plist-get edit :at)))
                           (setf out (splice out at at (plist-get edit :text)))))
                (values out (reverse changes))))))))

;;; --- the budget ---------------------------------------------------------------

(defun positive-or (value fallback)
  (if (and (integerp value) (plusp value)) value fallback))

(defun budget (mem)
  "What a heavy job starting now would get, from MEM: a plist of :SLOTS,
:RESERVE, :NEED, :CRITICAL and :CAP in MB. The autos are the original's."
  (let* ((total (if mem (memory-total mem) 2048))
         (reserve (positive-or (setting :reserve-mb)
                               (max 256 (min 2048 (round (* total 0.15))))))
         (min-start (positive-or (setting :min-start-mb)
                                 (max 192 (min 1024 (round (* total 0.10))))))
         (critical (positive-or (setting :critical-mb)
                                (max 96 (min 512 (round (* total 0.05))))))
         (slots (positive-or (setting :max-heavy-jobs)
                             (cond ((< total 4096) 1) ((< total 16384) 2) (t 4))))
         (cap (positive-or (setting :job-memory-max-mb)
                           (if mem (max min-start (- (memory-available mem) reserve)) 0))))
    (list :slots slots :reserve reserve :need (+ reserve min-start)
          :critical critical :cap cap)))

(defun current-budget () (budget (read-memory)))

;;; --- what happened, kept -------------------------------------------------------

(defvar *lock* (sb-thread:make-mutex :name "headroom")
  "One lock over the event log, the running jobs and the notes.")

(defvar *running* nil "True while the cell is started.")
(defvar *events* '() "Newest first: (:TIME :KIND :TEXT ...).")
(defvar *jobs* '() "Heavy calls in flight here: (:command :started :cap).")
(defvar *stops* '() "Processes the watchdog stopped: (:command :pid :mb :available).")
(defvar *slots* nil "The semaphore holding the machine-wide slots, or NIL.")
(defvar *notes* (make-hash-table :test #'equal) "Session id -> the lines it has not read yet.")
(defvar *warned* (make-hash-table :test #'equal) "Session id -> when it was last told memory is tight.")

(defun record-event (kind &rest fields)
  "Keep one event, newest first."
  (when *running*
    (sb-thread:with-mutex (*lock*)
      (push (list* :time (get-universal-time) :kind kind fields) *events*)
      (when (> (length *events*) +event-limit+)
        (setf *events* (subseq *events* 0 +event-limit+))))))

(defun add-note (session-id line)
  (when (and *running* session-id line)
    (sb-thread:with-mutex (*lock*)
      (push line (gethash session-id *notes*)))))

(defun take-notes (session-id)
  "The lines SESSION-ID has not been told yet, and clear them."
  (sb-thread:with-mutex (*lock*)
    (prog1 (reverse (gethash session-id *notes*))
      (remhash session-id *notes*))))

;;; --- the commands a form runs ---------------------------------------------------

(defun command-literal (value strings)
  "VALUE as a command: (VALUES TEXT KIND KEY), KIND :STRING with KEY the
strings-object name, :LITERAL with KEY the printed literal, or NIL."
  (cond
    ((stringp value) (values value :literal (prin1-to-string value)))
    ((and (consp value) (stringp (second value))
          (symbolp (first value)) (string-equal "$" (symbol-name (first value)))
          strings)
     (let ((text (gethash (second value) strings)))
       (when (stringp text) (values text :string (second value)))))
    (t nil)))

(defun argv-literal (value strings)
  "A (list \"npm\" \"run\") argv as one command line, or NIL."
  (when (and (consp value) (member (symbol-name (first value)) '("LIST" "QUOTE") :test #'equal))
    (let ((items (if (string-equal "QUOTE" (symbol-name (first value)))
                     (second value)
                     (rest value))))
      (when (and items (listp items)
                 (every (lambda (item) (or (stringp item) (command-literal item strings))) items))
        (format nil "~{~a~^ ~}"
                (mapcar (lambda (item) (or (command-literal item strings) item)) items))))))

(defun spawn-call (form strings)
  "FORM's command when it is one of the spawn calls, as (VALUES TEXT KIND KEY)."
  (let* ((head (and (consp form) (symbolp (first form))
                    (string-downcase (symbol-name (first form)))))
         (argument (second form)))
    (cond
      ((equal head "sh") (command-literal argument strings))
      ((member head '("run-program" "launch-program" "run-shell-command") :test #'equal)
       (or (command-literal argument strings)
           (let ((argv (argv-literal argument strings)))
             (when argv (values argv :argv nil)))))
      (t nil))))

(defun spawn-commands (form-text strings)
  "The shell commands FORM-TEXT runs, each (:TEXT :KIND :KEY), in the order read."
  (let ((calls '()))
    (labels ((walk (node)
               (when (consp node)
                 (multiple-value-bind (text kind key) (spawn-call node strings)
                   (when text
                     (push (list :text text :kind kind :key key) calls)))
                 (walk (car node))
                 (walk (cdr node)))))
      (ignore-errors
        (with-input-from-string (in form-text)
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (walk form)))))
    (nreverse calls)))

(defun put-back (form strings call new-text)
  "FORM and STRINGS with CALL's command replaced by NEW-TEXT.
(VALUES FORM STRINGS CHANGED). A literal that cannot be found exactly once
is left alone, and CHANGED says so."
  (ecase (plist-get call :kind)
    (:string
     (setf (gethash (plist-get call :key) strings) new-text)
     (values form strings t))
    (:literal
     (let* ((printed (plist-get call :key))
            (at (search printed form))
            (again (and at (search printed form :start2 (+ at (length printed))))))
       (if (and at (null again))
           (values (splice form at (+ at (length printed)) (prin1-to-string new-text)) strings t)
           (values form strings nil))))
    (:argv (values form strings nil))))

;;; --- the watchdog ----------------------------------------------------------------

(defun heaviest-new (baseline)
  "The largest process started since BASELINE, (VALUES PID MB NAME), or NIL.
No baseline, no victim: a watchdog that could not see what was already running
never guesses."
  (let ((rows '()) (self (sb-posix:getpid)))
    (dolist (pid (and baseline (remove self (set-difference (descendants self) baseline))))
      (multiple-value-bind (mb name) (process-facts pid)
        (when mb (push (list mb pid name) rows))))
    (let ((top (first (sort rows #'> :key #'first))))
      (when top (values (second top) (first top) (third top))))))

(defun signal-pid (pid signal)
  (ignore-errors (sb-posix:kill pid signal)))

(defun watch (flag command baseline critical)
  "Stop a process of the job when available memory falls under CRITICAL, until
FLAG is cleared. One thread per watched call, its own flag, its own baseline."
  (handler-case
      (loop while (car flag)
            do (sleep +sample-seconds+)
               (when (car flag)
                 (let ((mem (read-memory)))
                   (when (and mem (< (memory-available mem) critical))
                     (multiple-value-bind (pid mb name) (heaviest-new baseline)
                       (when pid
                         (signal-pid pid sb-posix:sigterm)
                         (sb-thread:with-mutex (*lock*)
                           (push (list :command command :pid pid :mb mb
                                       :available (memory-available mem)) *stops*))
                         (record-event :killed :command command
                                       :text (format nil "stopped ~a (~d MB) at ~d MB free"
                                                     name mb (memory-available mem)))))))))
    (error () nil)))

;;; --- the hook --------------------------------------------------------------------

(defun note-line (lines)
  "The lines as one bracketed line for the model."
  (format nil "~&[headroom] ~{~a~^; ~}." lines))

(defun refusal-text (reason)
  (format nil "ERROR: headroom: ~a; the command did not run -- ~a"
          reason
          "retry when the heavy jobs finish, or make it lighter (one worker, a smaller build)"))

(defun run-heavy (op next form strings calls verdict)
  "Run the heavy call CALLS describe with a slot, the memory gate and the
watchdog, and say in its result what it cost."
  (declare (ignore verdict))
  (let* ((budget (current-budget))
         (deadline (+ (get-universal-time) (setting :queue-timeout-s)))
         (label (plist-get (first calls) :text))
         (notes '())
         (waited 0)
         (started (get-universal-time)))
    (unless (sb-thread:wait-on-semaphore *slots* :timeout (setting :queue-timeout-s))
      (record-event :blocked :command label
                    :text (format nil "no slot in ~ds" (setting :queue-timeout-s)))
      (return-from run-heavy
        (nle:failure (refusal-text
                      (format nil "~d heavy job~:p already running (~d at a time), and no slot came free in ~ds"
                              (length *jobs*) (plist-get budget :slots) (setting :queue-timeout-s))))))
    (unwind-protect
         (progn
           ;; The memory gate: a job may not start into a host that has no room for it.
           (loop
             (let ((mem (read-memory)))
               (when (or (null mem) (>= (memory-available mem) (plist-get budget :need)))
                 (return)))
             (if (>= (get-universal-time) deadline)
                 (return-from run-heavy
                   (nle:failure (refusal-text
                                 (format nil "waited ~ds and only ~d MB are free; a heavy job needs ~d MB"
                                         (setting :queue-timeout-s)
                                         (let ((mem (read-memory))) (if mem (memory-available mem) 0))
                                         (plist-get budget :need)))))
                 (progn
                   (setf waited (- (get-universal-time) started))
                   (sleep +sample-seconds+))))
           (let ((cap (plist-get budget :cap)))
             (sb-thread:with-mutex (*lock*)
               (push (list :command label :started started :cap cap) *jobs*))
             (when (> waited 2)
               (push (format nil "waited ~ds for a slot and for memory" waited) notes))
             (when (> (length *jobs*) 1)
               (push (format nil "~d heavy jobs running here" (length *jobs*)) notes)))
           (multiple-value-bind (new-form new-strings changes)
               (rewrite-form form strings calls budget)
             (dolist (change changes)
               (push (format nil "ran lighter: ~a" change) notes))
             (when (> waited 2)
               (record-event :queued :command label :text (format nil "waited ~ds" waited)))
             (let* ((arguments (getf op :arguments))
                    (new-op (if (and (null changes) (eq new-form form))
                                op
                                (let ((copy (copy-object arguments)))
                                  (setf (gethash "form" copy) new-form)
                                  (when (and new-strings (not (eq new-strings strings)))
                                    (setf (gethash "strings" copy) new-strings))
                                  (list* :name (getf op :name) :arguments copy
                                         (loop for (key value) on op by #'cddr
                                               unless (member key '(:name :arguments))
                                                 append (list key value))))))
                    (flag (and (setting :watch) (list t)))
                    (baseline (and flag (ignore-errors (descendants (sb-posix:getpid)))))
                    (thread (and flag (ignore-errors
                                        (sb-thread:make-thread
                                         (lambda () (watch flag label baseline (plist-get budget :critical)))
                                         :name "headroom-watch")))))
               (declare (ignore thread))
               (unwind-protect
                    (let ((result (funcall next new-op)))
                      (when flag (setf (car flag) nil))
                      (dolist (stop (sb-thread:with-mutex (*lock*) (copy-list *stops*)))
                        (when (equal (plist-get stop :command) label)
                          (push (format nil "stopped ~d MB of it at ~d MB free (watchdog)"
                                        (plist-get stop :mb) (plist-get stop :available))
                                notes)))
                      (let ((text (if (stringp result) result
                                      (princ-to-string (or result "")))))
                        (if notes (concatenate 'string text (note-line (reverse notes))) text)))
                 (when flag (setf (car flag) nil))
                 (sb-thread:with-mutex (*lock*)
                   (setf *jobs* (remove label *jobs* :key (lambda (job) (plist-get job :command))
                                                      :test #'equal :count 1)))
                 (record-event :finished :command label
                               :text (format nil "~ds, cap ~d MB" (- (get-universal-time) started)
                                             (plist-get budget :cap)))))))
      (sb-thread:signal-semaphore *slots*))))

(defun rewrite-form (form strings calls budget)
  "FORM and STRINGS with each heavy CALL lightened. (VALUES FORM STRINGS CHANGES)."
  (let ((changes '()) (out form) (object strings))
    (dolist (call calls)
      (let ((verdict (classify-command (plist-get call :text))))
        (when (plist-get verdict :heavy)
          (multiple-value-bind (new why)
              (lighten (plist-get call :text)
                       :budget-mb (plist-get budget :cap)
                       :low-memory (< (plist-get budget :cap) 1536))
            (when why
              (multiple-value-bind (rewritten rewritten-strings changed)
                  (put-back out object call new)
                (setf out rewritten object rewritten-strings)
                (if changed
                    (setf changes (append changes why))
                    (push (format nil "could not rewrite `~a` in place" (plist-get call :text))
                          changes))))))))
    (values out object changes)))

(defun headroom-tool (op next)
  "The :TOOL point: a heavy shell command in an eval form runs queued, gated,
lightened and watched. Every other call runs untouched."
  (if (not (and *running* (not (equal (setting :mode) "off")) (equal (getf op :name) "eval")))
      (funcall next op)
      (let* ((arguments (getf op :arguments))
             (form (and (hash-table-p arguments) (gethash "form" arguments)))
             (strings (and (hash-table-p arguments) (gethash "strings" arguments))))
        (if (not (stringp form))
            (funcall next op)
            (let ((calls (spawn-commands form (and (hash-table-p strings) strings))))
              (if (null calls)
                  (funcall next op)
                  (let ((heavy (remove-if-not (lambda (call)
                                                 (plist-get (classify-command (plist-get call :text)) :heavy))
                                               calls)))
                    (cond ((null heavy) (funcall next op))
                          ((equal (setting :mode) "observe")
                           (dolist (call heavy)
                             (record-event :observed :command (plist-get call :text)
                                           :text (format nil "would cap at ~d MB"
                                                         (plist-get (current-budget) :cap))))
                           (funcall next op))
                          (t (handler-case (run-heavy op next form strings heavy nil)
                               (error (condition)
                                 (record-event :error :text (princ-to-string condition))
                                 (let ((result (funcall next op)))
                                   (if (stringp result)
                                       (concatenate 'string result
                                                    (format nil "~&[headroom] guard error, the command ran unguarded: ~a"
                                                            condition))
                                       result)))))))))))))

(defun headroom-round (op next)
  "The :ROUND point: when memory is too tight to start a heavy job, say so
once every ten minutes in that session."
  (when (and *running* (not (equal (setting :mode) "off")))
    (let* ((session (getf op :session-id))
           (turn (getf op :turn))
           (mem (read-memory)))
      (when (and session turn mem)
        (let ((need (plist-get (budget mem) :need)))
          (when (and (< (memory-available mem) need)
                     (> (- (get-universal-time) (gethash session *warned* 0)) 600))
            (setf (gethash session *warned*) (get-universal-time))
            (nlk:record-turn-context-prompt
             turn :text
             (format nil "[headroom] Host memory is tight: ~d MB free of ~d MB. Heavy commands wait for ~d MB before starting; prefer one heavy job at a time and single-worker options."
                     (memory-available mem) (memory-total mem) need)))))))
  (funcall next op))

;;; --- the picture ---------------------------------------------------------------

(defun job-json (job)
  (nlk:json-object "command" (plist-get job :command)
                   "running_s" (- (get-universal-time) (plist-get job :started))
                   "cap_mb" (plist-get job :cap)))

(defun event-json (event)
  (nlk:json-object "time" (plist-get event :time)
                   "kind" (string-downcase (symbol-name (plist-get event :kind)))
                   "command" (or (plist-get event :command) "")
                   "text" (or (plist-get event :text) "")))

(defun process-json (row)
  (nlk:json-object "command" (second row) "mb" (first row) "pid" (third row)
                   "this_image" (eql (third row) (sb-posix:getpid))))

(defun snapshot (&key explain)
  "Everything /headroom status and headroom_status show, as a JSON object."
  (let* ((mem (read-memory))
         (budget (and mem (budget mem)))
         (pressure (multiple-value-list (read-pressure))))
    (nlk:json-object
     "mode" (if *running* (setting :mode) "off")
     "memory" (and mem (nlk:json-object "total_mb" (memory-total mem)
                                         "available_mb" (memory-available mem)
                                         "swap_used_mb" (memory-swap-used mem)
                                         "swap_total_mb" (memory-swap-total mem)))
     "budget" (and budget (nlk:json-object "next_job_cap_mb" (plist-get budget :cap)
                                            "reserved_for_the_image_mb" (plist-get budget :reserve)
                                            "start_needs_available_mb" (plist-get budget :need)
                                            "critical_mb" (plist-get budget :critical)
                                            "heavy_job_slots" (plist-get budget :slots)))
     "pressure" (and (first pressure)
                      (nlk:json-object "some_avg10" (first pressure) "full_avg10" (second pressure)))
     "cpus" (cpu-count)
     "running_here" (coerce (mapcar #'job-json (sb-thread:with-mutex (*lock*) (copy-list *jobs*)))
                            'vector)
     "top_processes" (coerce (mapcar #'process-json (top-processes)) 'vector)
     "recent" (coerce (mapcar #'event-json
                              (sb-thread:with-mutex (*lock*) (subseq *events* 0 (min 8 (length *events*)))))
                      'vector)
     "explain" (if (and explain (stringp explain) (plusp (length (string-trim " " explain))))
                   (explain-text explain)
                   ""))))

(defun status-text ()
  "The operator's one-screen picture."
  (let* ((mem (read-memory))
         (budget (and mem (budget mem)))
         (lines '()))
    (push (if mem
              (format nil "headroom (~a): ~a free of ~a, swap ~a/~a, ~d CPU"
                      (if *running* (setting :mode) "off")
                      (format-mb (memory-available mem)) (format-mb (memory-total mem))
                      (format-mb (memory-swap-used mem)) (format-mb (memory-swap-total mem))
                      (cpu-count))
              "headroom: memory figures unavailable on this platform")
          lines)
    (multiple-value-bind (some full) (read-pressure)
      (when some
        (push (format nil "memory pressure (PSI avg10): some ~d%, full ~d%" some full) lines)))
    (when budget
      (push (format nil "next heavy job: cap ~a, starts when ~a are free, ~d at a time; watchdog line ~a"
                    (format-mb (plist-get budget :cap)) (format-mb (plist-get budget :need))
                    (plist-get budget :slots) (format-mb (plist-get budget :critical)))
            lines))
    (let ((jobs (sb-thread:with-mutex (*lock*) (copy-list *jobs*))))
      (if jobs
          (dolist (job jobs)
            (push (format nil "running: `~a` ~ds, cap ~a"
                          (plist-get job :command)
                          (- (get-universal-time) (plist-get job :started))
                          (format-mb (plist-get job :cap)))
                  lines))
          (push "no heavy jobs running" lines)))
    (let ((top (top-processes)))
      (when top
        (push (format nil "largest processes: ~{~a~^; ~}"
                      (mapcar (lambda (row)
                                (format nil "~a ~a~@[ [this image]~]"
                                        (second row) (format-mb (first row))
                                        (eql (third row) (sb-posix:getpid))))
                              top))
              lines)))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defun events-text (&key (limit 15))
  (let ((events (sb-thread:with-mutex (*lock*) (subseq *events* 0 (min limit (length *events*))))))
    (if (null events)
        "headroom: no events yet"
        (format nil "~{~a~^~%~}"
                (mapcar (lambda (event)
                          (format nil "~a ~a ~@[`~a` ~]~a"
                                  (multiple-value-bind (s m h day month year)
                                      (decode-universal-time (plist-get event :time))
                                    (declare (ignore year))
                                    (format nil "~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" month day h m s))
                                  (string-downcase (symbol-name (plist-get event :kind)))
                                  (plist-get event :command)
                                  (or (plist-get event :text) "")))
                        events)))))

(defun explain-text (command)
  "What headroom would do with COMMAND right now."
  (let ((verdict (classify-command command)))
    (if (not (plist-get verdict :heavy))
        (format nil "light: `~a` runs untouched" command)
        (let* ((budget (current-budget))
               (cap (plist-get budget :cap)))
          (with-output-to-string (out)
            (format out "heavy (~{~a~^, ~})~%" (plist-get verdict :rules))
            (format out "memory cap if started now: ~a (+swap)~%" (format-mb cap))
            (multiple-value-bind (new changes)
                (lighten command :budget-mb cap :low-memory (< cap 1536))
              (if changes
                  (progn (format out "rewritten to: ~a~%" new)
                         (dolist (change changes) (format out "  - ~a~%" change)))
                  (format out "runs as written~%"))))))))

;;; --- the tool, the command, the manual -----------------------------------------

(nlk:define-contract headroom-status
  (:doc "Show this machine's memory headroom before starting heavy work: free RAM and swap, memory pressure, the memory cap the next heavy command would get, the heavy jobs running now, the largest processes and recent queue waits, caps and watchdog stops. Pass a shell command as explain to see whether headroom treats it as heavy and how it would lighten it. Call it before installs, builds, test suites or browsers on a small machine, and after a command was stopped.")
  ("explain" :string :default ""
             :doc "a shell command to classify, or empty"))

(defun headroom-status-tool (report)
  "The headroom_status tool: the snapshot as JSON."
  (nlk:encode-json-object (snapshot :explain (headroom-status-explain report))))

(defparameter +usage+ "/headroom [status | events | explain <command>]")

(defun run-headroom (args session-id)
  "The /headroom command."
  (declare (ignore session-id))
  (let* ((text (string-trim " " (or args "")))
         (space (position #\Space text))
         (sub (string-downcase (if space (subseq text 0 space) text)))
         (rest (if space (string-trim " " (subseq text (1+ space))) "")))
    (cond ((or (zerop (length sub)) (equal sub "status")) (status-text))
          ((equal sub "events") (events-text))
          ((equal sub "explain")
           (if (zerop (length rest)) "usage: /headroom explain <command>" (explain-text rest)))
          (t +usage+))))

(defparameter +manual+
  "headroom: a memory guard for heavy shell jobs on this machine.
  /headroom                 memory free, the next job's cap, the jobs running now
  /headroom events          recent queue waits, caps and watchdog stops
  /headroom explain CMD     whether a command counts as heavy, and what it would run as
  (headroom_status)         the same picture as JSON, for the model; \"explain\": \"CMD\" adds the verdict
A heavy command -- an install, a build, a test suite, a browser, a compiler -- waits for
one of a few machine-wide slots, waits until the host has the memory to start it, runs
with single-worker flags and a Node heap cap, and is watched: when available memory
crosses the critical line, a process of the job is stopped and the model is told. The
section is `headroom'; mode enforce, observe or off turns the whole guard off; lighten
and watch turn one half off."
  "What (help :headroom) answers while the cell runs.")

(nlk:define-peripheral headroom :not-running t)

(defun headroom-settings (values table)
  "The section's values with the two pattern lists compiled once."
  (declare (ignore table))
  (append (list :extra (compile-patterns (getf values :extra-heavy))
                :exclude (compile-patterns (getf values :exclude)))
          values))

(defun start-headroom ()
  "Reset what the last run kept, take the slots, and give the stop back."
  (setf *events* nil *jobs* nil *stops* nil *running* t)
  (clrhash *notes*)
  (clrhash *warned*)
  (setf *slots* (sb-thread:make-semaphore
                 :count (plist-get (current-budget) :slots)))
  (record-event :started :text (format nil "~d slots, mode ~a"
                                       (plist-get (current-budget) :slots)
                                       (setting :mode)))
  (nle:on-stop
   (lambda ()
     (setf *running* nil *slots* nil *jobs* nil)
     (record-event :stopped))))

(nle:define-cell headroom
  (:section ("headroom")
    (:guide "mode enforce queues, caps and watches heavy shell jobs, observe only logs what it would do, off does nothing; max_heavy_jobs 0 picks one below 4 GB of RAM, two below 16 GB, else four; /headroom status shows the budget and /headroom explain <command> says what one would run as")
    ("mode" :choice :options '("enforce" "observe" "off") :default "enforce"
     :doc "enforce queues, caps and watches heavy jobs; observe only logs what it would do; off does nothing")
    ("max_heavy_jobs" :integer :default 0 :min 0
     :doc "heavy jobs at once; 0 = auto (1 below 4 GB of RAM, 2 below 16 GB, else 4)")
    ("reserve_mb" :integer :default 0 :min 0
     :doc "memory kept for this image and the rest of the system; 0 = auto (15% of RAM, 256 MB to 2 GB)")
    ("min_start_mb" :integer :default 0 :min 0
     :doc "free memory a heavy job needs beyond the reserve; 0 = auto (10% of RAM, 192 MB to 1 GB)")
    ("critical_mb" :integer :default 0 :min 0
     :doc "available memory under which the watchdog stops a process of the job; 0 = auto (5% of RAM, 96 MB to 512 MB)")
    ("job_memory_max_mb" :integer :default 0 :min 0
     :doc "the ceiling a job's cap is planned against; 0 = whatever is free when it starts, minus the reserve")
    ("queue_timeout_s" :integer :default 300 :min 1
     :doc "how long a heavy command waits for a slot and for memory before it is refused with an explanation")
    ("lighten" :boolean :default t
     :doc "add single-worker flags and a Node heap cap before the command runs")
    ("watch" :boolean :default t
     :doc "stop a process of a heavy job when available memory crosses the critical line")
    ("extra_heavy" :list
     :doc "regular expressions matched against a command's words, for work this table does not know")
    ("exclude" :list
     :doc "regular expressions matched against the whole command: never treated as heavy"))
  (:settings (lambda (values table) (headroom-settings values table)))
  (:tool "headroom_status" (nlk:contract 'headroom-status) #'headroom-status-tool)
  (:help :headroom
   "headroom: free memory, the cap a heavy command would get, and what headroom did to recent ones"
   +manual+)
  (:command "headroom" #'run-headroom
            :description "Memory headroom, heavy jobs and recent watchdog stops"
            :argument-hint "[status | events | explain <command>]")
  (:hook :tool #'headroom-tool)
  (:hook :round #'headroom-round)
  (:start (lambda () (start-headroom))))

