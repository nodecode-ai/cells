;;;; pty.lisp --- a shell on a pseudo-terminal of its own, owned by the image.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; OPEN-TERMINAL starts the operator's shell on a fresh pty, the way a
;;;; terminal emulator does and the desktop's node-pty did: posix_openpt for the
;;;; pair, then posix_spawn with POSIX_SPAWN_SETSID, so the shell leads a
;;;; session of its own and the slave it opens as fd 0 becomes its controlling
;;;; terminal. That is what makes ^C reach the foreground job, job control
;;;; work, and a resize (TIOCSWINSZ on the master) arrive as SIGWINCH. SBCL's
;;;; own RUN-PROGRAM :PTY does not do this: its child joins a new process group
;;;; of the image's session and holds the slave as a plain file, so ^C is
;;;; ignored and `ps' shows no tty. posix_spawn (not fork) because the image
;;;; runs threads; every call here is plain libc, no C compiled.
;;;;
;;;; Each terminal has one reader thread: it polls the master, appends what
;;;; arrives to a bounded backlog (so a page that reconnects gets its screen
;;;; back) and hands it to every attached sink; at end of file it reaps the
;;;; shell and tells the sinks its exit code. The child inherits nothing of the
;;;; image but fds 0-2 (closefrom 3 on Linux, CLOEXEC_DEFAULT on macOS), so a
;;;; shell never holds the gateway's listening socket or the store open.

(in-package #:nodecode-terminal)

;;; --- libc -------------------------------------------------------------------

(defconstant +spawn-setsigdef+ #x04)
(defconstant +spawn-setsigmask+ #x08)
(defconstant +spawn-setsid+ #+darwin #x400 #-darwin #x80)
(defconstant +spawn-cloexec-default+ #+darwin #x4000 #-darwin 0)
(defconstant +tiocswinsz+ #+darwin #x80087467 #-darwin #x5414)
(defconstant +fd-cloexec+ 1 "FD_CLOEXEC, the same on Linux and macOS.")
(defconstant +opaque-size+ 1024
  "Bytes allocated for each opaque libc object (posix_spawnattr_t, the file
actions, a sigset_t): more than any of them takes on Linux or macOS.")

(sb-alien:define-alien-routine ("posix_openpt" %posix-openpt) sb-alien:int (flags sb-alien:int))
(sb-alien:define-alien-routine ("grantpt" %grantpt) sb-alien:int (fd sb-alien:int))
(sb-alien:define-alien-routine ("unlockpt" %unlockpt) sb-alien:int (fd sb-alien:int))
(sb-alien:define-alien-routine ("ptsname_r" %ptsname-r) sb-alien:int
  (fd sb-alien:int) (buf sb-sys:system-area-pointer) (size sb-alien:unsigned-long))
(sb-alien:define-alien-routine ("sigemptyset" %sigemptyset) sb-alien:int (set sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("sigfillset" %sigfillset) sb-alien:int (set sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("posix_spawnattr_init" %attr-init) sb-alien:int (attr sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("posix_spawnattr_destroy" %attr-destroy) sb-alien:int (attr sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("posix_spawnattr_setflags" %attr-setflags) sb-alien:int
  (attr sb-sys:system-area-pointer) (flags sb-alien:short))
(sb-alien:define-alien-routine ("posix_spawnattr_setsigdefault" %attr-setsigdefault) sb-alien:int
  (attr sb-sys:system-area-pointer) (set sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("posix_spawnattr_setsigmask" %attr-setsigmask) sb-alien:int
  (attr sb-sys:system-area-pointer) (set sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("posix_spawn_file_actions_init" %fa-init) sb-alien:int (fa sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("posix_spawn_file_actions_destroy" %fa-destroy) sb-alien:int (fa sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("posix_spawn_file_actions_addopen" %fa-addopen) sb-alien:int
  (fa sb-sys:system-area-pointer) (fd sb-alien:int) (path sb-alien:c-string) (oflag sb-alien:int) (mode sb-alien:unsigned-int))
(sb-alien:define-alien-routine ("posix_spawn_file_actions_adddup2" %fa-adddup2) sb-alien:int
  (fa sb-sys:system-area-pointer) (fd sb-alien:int) (newfd sb-alien:int))
(sb-alien:define-alien-routine ("posix_spawn_file_actions_addchdir_np" %fa-addchdir) sb-alien:int
  (fa sb-sys:system-area-pointer) (path sb-alien:c-string))
#-darwin
(sb-alien:define-alien-routine ("posix_spawn_file_actions_addclosefrom_np" %fa-addclosefrom) sb-alien:int
  (fa sb-sys:system-area-pointer) (from sb-alien:int))
(sb-alien:define-alien-routine ("posix_spawn" %posix-spawn) sb-alien:int
  (pid (* sb-alien:int)) (path sb-alien:c-string) (fa sb-sys:system-area-pointer)
  (attr sb-sys:system-area-pointer) (argv sb-sys:system-area-pointer) (envp sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("ioctl" %ioctl-winsize) sb-alien:int
  (fd sb-alien:int) (request sb-alien:unsigned-long) (winsize sb-sys:system-area-pointer))

(defun check (what result)
  "RESULT of libc call WHAT when it is not -1 (or, for the posix_spawn family,
an error number other than 0); a TERMINAL-ERROR naming the call otherwise."
  (if (eql result -1)
      (error 'terminal-error :message (format nil "~a: ~a" what (sb-int:strerror (sb-alien:get-errno))))
      result))

(defun check-spawn (what result)
  "The posix_spawn family answers its error number rather than setting errno."
  (unless (zerop result)
    (error 'terminal-error :message (format nil "~a: ~a" what (sb-int:strerror result))))
  result)

(define-condition terminal-error (error)
  ((message :initarg :message :reader terminal-error-message))
  (:report (lambda (condition stream) (write-string (terminal-error-message condition) stream))))

(defun foreign-strings (strings &aux (count (length strings)))
  "STRINGS as a NULL-ended char*[] in foreign memory => (values SAP FREE),
FREE a function releasing it."
  (let* ((vector (sb-alien:make-alien sb-sys:system-area-pointer (1+ count)))
         (sap (sb-alien:alien-sap vector))
         (held '()))
    (loop for string in strings
          for i from 0
          do (let* ((octets (sb-ext:string-to-octets string :external-format :utf-8 :null-terminate t))
                    (memory (sb-alien:make-alien (sb-alien:unsigned 8) (length octets))))
               (push memory held)
               (loop for octet across octets
                     for j from 0
                     do (setf (sb-sys:sap-ref-8 (sb-alien:alien-sap memory) j) octet))
               (setf (sb-sys:sap-ref-sap sap (* i 8)) (sb-alien:alien-sap memory))))
    (setf (sb-sys:sap-ref-sap sap (* count 8)) (sb-sys:int-sap 0))
    (values sap (lambda () (mapc #'sb-alien:free-alien held) (sb-alien:free-alien vector)))))

(defmacro with-opaque ((&rest names) &body body)
  "BODY with each of NAMES bound to the SAP of +OPAQUE-SIZE+ zeroed foreign bytes."
  (let ((aliens (loop for name in names collect (gensym (symbol-name name)))))
    `(let ,(loop for alien in aliens collect `(,alien (sb-alien:make-alien (sb-alien:unsigned 8) +opaque-size+)))
       (unwind-protect
            (let ,(loop for name in names for alien in aliens collect `(,name (sb-alien:alien-sap ,alien)))
              ,@(loop for name in names collect `(dotimes (i +opaque-size+) (setf (sb-sys:sap-ref-8 ,name i) 0)))
              ,@body)
         ,@(loop for alien in aliens collect `(sb-alien:free-alien ,alien))))))

(defun set-winsize (fd cols rows)
  "Tell the pty on FD its size: the kernel then signals SIGWINCH to the
foreground job of the session it controls."
  (with-opaque (winsize)
    (setf (sb-sys:sap-ref-16 winsize 0) rows
          (sb-sys:sap-ref-16 winsize 2) cols)
    (check "ioctl TIOCSWINSZ" (%ioctl-winsize fd +tiocswinsz+ winsize))))

(defun open-pty ()
  "A fresh pty pair => (values MASTER-FD SLAVE-PATH). The master is closed on
exec, so no other child of the image inherits it."
  (let ((master (check "posix_openpt" (%posix-openpt (logior sb-posix:o-rdwr sb-posix:o-noctty)))))
    (handler-bind ((error (lambda (condition) (declare (ignore condition)) (sb-posix:close master))))
      (check "grantpt" (%grantpt master))
      (check "unlockpt" (%unlockpt master))
      (sb-posix:fcntl master sb-posix:f-setfd +fd-cloexec+)
      (with-opaque (buffer)
        (check-spawn "ptsname_r" (%ptsname-r master buffer 512))
        (values master (sb-alien::c-string-to-string buffer :utf-8 'character))))))

(defun spawn-on-pty (slave program args environment cwd)
  "Start PROGRAM with ARGS and ENVIRONMENT (strings NAME=VALUE) in CWD, leading
a new session whose controlling terminal is SLAVE => the child's pid."
  (multiple-value-bind (argv free-argv) (foreign-strings (cons program args))
    (multiple-value-bind (envp free-envp) (foreign-strings environment)
      (unwind-protect
           (with-opaque (attr actions all none)
             (check-spawn "posix_spawnattr_init" (%attr-init attr))
             (check-spawn "posix_spawn_file_actions_init" (%fa-init actions))
             (unwind-protect
                  (progn
                    (%sigfillset all)
                    (%sigemptyset none)
                    (check-spawn "setflags" (%attr-setflags attr (logior +spawn-setsid+ +spawn-setsigdef+
                                                                          +spawn-setsigmask+ +spawn-cloexec-default+)))
                    (check-spawn "setsigdefault" (%attr-setsigdefault attr all))
                    (check-spawn "setsigmask" (%attr-setsigmask attr none))
                    ;; Opened without O_NOCTTY by the new session's leader: the
                    ;; slave becomes its controlling terminal.
                    (check-spawn "addopen" (%fa-addopen actions 0 slave sb-posix:o-rdwr 0))
                    (check-spawn "adddup2 1" (%fa-adddup2 actions 0 1))
                    (check-spawn "adddup2 2" (%fa-adddup2 actions 0 2))
                    #-darwin (check-spawn "addclosefrom" (%fa-addclosefrom actions 3))
                    (when cwd (check-spawn "addchdir" (%fa-addchdir actions cwd)))
                    (sb-alien:with-alien ((pid sb-alien:int 0))
                      (check-spawn (format nil "posix_spawn ~a" program)
                                   (%posix-spawn (sb-alien:addr pid) program actions attr argv envp))
                      pid))
               (%fa-destroy actions)
               (%attr-destroy attr)))
        (funcall free-envp)
        (funcall free-argv)))))

;;; --- a terminal --------------------------------------------------------------

(defconstant +backlog-limit+ (* 256 1024)
  "The most bytes of a terminal's output kept for a page that attaches later,
the desktop's BACKLOG_LIMIT.")

(defvar *command* nil
  "The program and arguments every terminal runs, NIL for the operator's own
login shell (LOGIN-SHELL).")

(defvar *environment* nil
  "The environment every terminal starts with, NIL for SHELL-ENVIRONMENT.")

(defvar *terminals* (make-hash-table :test 'equal)
  "Every terminal this image runs, by id.")

(defvar *terminals-lock* (sb-thread:make-mutex :name "terminals"))

(defstruct (terminal (:constructor %make-terminal))
  (id "" :type string)
  (pid 0 :type integer)
  (fd -1 :type integer)
  (cwd nil)
  (cols 80 :type integer)
  (rows 24 :type integer)
  (backlog (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (sinks '())
  (exit nil)
  (closing nil)
  (reader nil)
  (lock (sb-thread:make-mutex :name "terminal")))

(defun clamp (value default low high)
  "VALUE as a whole number from LOW to HIGH, DEFAULT when it is not a number."
  (if (realp value) (max low (min high (round value))) default))

(defun login-shell ()
  "The operator's own shell as (PROGRAM . ARGS): $SHELL as a login shell when
it names a program, else the system's."
  (let ((shell (sb-posix:getenv "SHELL")))
    (list (if (and shell (plusp (length shell)) (char= #\/ (char shell 0)) (probe-file shell))
              shell
              #+darwin "/bin/zsh" #-darwin "/bin/bash")
          "-l")))

(defun shell-environment ()
  "The image's environment for a shell, as a terminal emulator sets it."
  (append (remove-if (lambda (entry)
                       (some (lambda (name) (uiop:string-prefix-p name entry))
                             '("TERM=" "COLORTERM=" "TERM_PROGRAM=")))
                     (sb-ext:posix-environ))
          '("TERM=xterm-256color" "COLORTERM=truecolor" "TERM_PROGRAM=nodecode")))

(defun new-terminal-id ()
  (format nil "t~36r" (random (expt 36 10) (load-time-value (make-random-state t)))))

(defun open-terminal (&key cwd cols rows (command (or *command* (login-shell)))
                        (environment (or *environment* (shell-environment))))
  "Start COMMAND (the operator's login shell) on a pty of its own, COLS x ROWS,
in CWD (a directory; the home when NIL) => the terminal."
  (let ((cols (clamp cols 80 1 1000))
        (rows (clamp rows 24 1 500))
        (cwd (and cwd (uiop:directory-exists-p cwd) (uiop:native-namestring (uiop:directory-exists-p cwd)))))
    (multiple-value-bind (master slave) (open-pty)
      (handler-bind ((error (lambda (condition) (declare (ignore condition)) (sb-posix:close master))))
        (set-winsize master cols rows)
        (let* ((pid (spawn-on-pty slave (first command) (rest command) environment cwd))
               (terminal (%make-terminal :id (new-terminal-id) :pid pid :fd master
                                         :cwd (or cwd (uiop:native-namestring (user-homedir-pathname)))
                                         :cols cols :rows rows)))
          (sb-thread:with-mutex (*terminals-lock*)
            (setf (gethash (terminal-id terminal) *terminals*) terminal))
          (setf (terminal-reader terminal)
                (sb-thread:make-thread #'read-terminal :name (format nil "terminal ~a" (terminal-id terminal))
                                                       :arguments (list terminal)))
          terminal)))))

(defun find-terminal (id)
  (sb-thread:with-mutex (*terminals-lock*) (values (gethash id *terminals*))))

(defun list-terminals ()
  (sb-thread:with-mutex (*terminals-lock*)
    (sort (alexandria:hash-table-values *terminals*) #'string< :key #'terminal-id)))

(defun keep-output (terminal octets count)
  "Append COUNT of OCTETS to TERMINAL's backlog, cut from the front to
+BACKLOG-LIMIT+, and hand them to each sink => nothing. Called with its lock."
  (let* ((backlog (terminal-backlog terminal))
         (total (+ (fill-pointer backlog) count)))
    (when (> total +backlog-limit+)
      (let* ((drop (- total +backlog-limit+))
             (keep (max 0 (- (fill-pointer backlog) drop))))
        (replace backlog backlog :start2 (- (fill-pointer backlog) keep))
        (setf (fill-pointer backlog) keep)))
    (let ((start (max 0 (- count +backlog-limit+))))
      (loop for i from start below count do (vector-push-extend (aref octets i) backlog)))))

(defun tell-sinks (terminal kind data)
  (dolist (sink (sb-thread:with-mutex ((terminal-lock terminal)) (copy-list (terminal-sinks terminal))))
    (ignore-errors (funcall sink kind data))))

(defun wait-status-code (status)
  "A waitpid STATUS as a shell reports it: the exit code, or 128 + the signal."
  (cond ((sb-posix:wifexited status) (sb-posix:wexitstatus status))
        ((sb-posix:wifsignaled status) (+ 128 (sb-posix:wtermsig status)))
        (t status)))

(defun read-terminal (terminal &aux (fd (terminal-fd terminal))
                                 (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
  "TERMINAL's reader: its output to the backlog and the sinks until the pty
closes, then the shell reaped and its exit code told."
  (unwind-protect
       (loop
         (when (sb-unix:unix-simple-poll fd :input 250)
           (let ((count (sb-sys:with-pinned-objects (buffer)
                          (sb-unix:unix-read fd (sb-sys:vector-sap buffer) (length buffer)))))
             (when (or (null count) (<= count 0))
               (return))
             (let ((octets (subseq buffer 0 count)))
               (sb-thread:with-mutex ((terminal-lock terminal))
                 (keep-output terminal octets count))
               (tell-sinks terminal :output octets))))
         (when (terminal-closing terminal)
           (let ((alive (ignore-errors (sb-posix:kill (terminal-pid terminal) 0) t)))
             (unless alive (return)))))
    (let ((code (handler-case (multiple-value-bind (pid status) (sb-posix:waitpid (terminal-pid terminal) 0)
                                (declare (ignore pid))
                                (wait-status-code status))
                  (error () -1))))
      (ignore-errors (sb-posix:close fd))
      (sb-thread:with-mutex ((terminal-lock terminal))
        (setf (terminal-exit terminal) code))
      (tell-sinks terminal :exit code)
      (sb-thread:with-mutex (*terminals-lock*)
        (remhash (terminal-id terminal) *terminals*)))))

(defun terminal-write (terminal data)
  "Send DATA (a string, as UTF-8, or octets) to TERMINAL as typed input."
  (let ((octets (if (stringp data) (sb-ext:string-to-octets data :external-format :utf-8) data))
        (fd (terminal-fd terminal)))
    (sb-sys:with-pinned-objects (octets)
      (loop with start = 0
            while (< start (length octets))
            do (let ((written (sb-unix:unix-write fd octets start (- (length octets) start))))
                 (unless (and written (plusp written)) (return))
                 (incf start written))))
    (length octets)))

(defun terminal-resize (terminal cols rows)
  "Make TERMINAL COLS x ROWS; its foreground job gets SIGWINCH."
  (let ((cols (clamp cols (terminal-cols terminal) 1 1000))
        (rows (clamp rows (terminal-rows terminal) 1 500)))
    (unless (terminal-exit terminal)
      (set-winsize (terminal-fd terminal) cols rows))
    (setf (terminal-cols terminal) cols (terminal-rows terminal) rows)))

(defun terminal-attach (terminal sink)
  "Add SINK, a function of (KIND DATA) told :OUTPUT octets and :EXIT code, to
TERMINAL => the backlog so far, which the sink must draw first. Taken under
the lock, so no output falls between the backlog and the first :OUTPUT."
  (sb-thread:with-mutex ((terminal-lock terminal))
    (push sink (terminal-sinks terminal))
    (copy-seq (coerce (terminal-backlog terminal) '(simple-array (unsigned-byte 8) (*))))))

(defun terminal-detach (terminal sink)
  (sb-thread:with-mutex ((terminal-lock terminal))
    (setf (terminal-sinks terminal) (remove sink (terminal-sinks terminal)))))

(defun close-terminal (terminal &key (grace 2))
  "End TERMINAL: SIGHUP to its session, as a closed terminal window sends,
then SIGKILL after GRACE seconds if the shell is still there => its exit code."
  (setf (terminal-closing terminal) t)
  (let ((pid (terminal-pid terminal)))
    (ignore-errors (sb-posix:kill (- pid) sb-posix:sighup))
    (ignore-errors (sb-posix:kill pid sb-posix:sighup))
    (let ((reader (terminal-reader terminal)))
      (unless (and reader (sb-thread:join-thread reader :default nil :timeout grace))
        (ignore-errors (sb-posix:kill (- pid) sb-posix:sigkill))
        (ignore-errors (sb-posix:kill pid sb-posix:sigkill))
        (when reader (sb-thread:join-thread reader :default nil :timeout grace)))))
  (terminal-exit terminal))
