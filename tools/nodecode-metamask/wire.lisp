;;;; wire.lisp --- the mm CLI: where it is, how it runs, what it answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No key is in this cell and none is in the CLI's argv: every wallet call is
;;;; one run of the MetaMask agent wallet CLI, `mm --json', and what it prints
;;;; comes back as it printed it. mm answers its own envelope
;;;; {"ok": true, "data": ...} or {"ok": false, "error": {...}}, and in
;;;; --json mode it may write NDJSON first -- a {"_notice": ...} line per step
;;;; (a 2FA pause, a per-step outcome) before the envelope -- so the reader
;;;; below takes every object in the output, keeps the notices, and answers the
;;;; envelope with them beside it. A login token or a BYOK mnemonic reaches mm
;;;; from the environment (MM_CLI_TOKEN, MM_MNEMONIC) and never from here.

(in-package #:nodecode-metamask)

;;; --- the program ------------------------------------------------------------------

(defun program-path (name)
  "NAME as a program: the path itself when it names one, else the first file of
that name on PATH; NIL when there is none. A directory of that name is no
program."
  (when (and (stringp name) (plusp (length name)))
    (flet ((use (path)
             (when (and path (not (uiop:directory-pathname-p path)))
               (uiop:native-namestring path))))
      (if (or (find #\/ name) (find #\\ name))
          (use (ignore-errors (probe-file name)))
          (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                               :separator '(#\: #\;))
                for path = (ignore-errors
                             (probe-file
                              (merge-pathnames name
                                               (uiop:ensure-directory-pathname
                                                (if (plusp (length dir)) dir ".")))))
                thereis (use path))))))

(defvar *serial* 0
  "A counter that keeps one run's temporary files apart from the next's.")

(defun temporary (kind)
  "A pathname in the system's temporary directory that nothing else is using."
  (merge-pathnames (format nil "mm-~d-~d.~a"
                            (get-universal-time) (incf *serial*) kind)
                   (uiop:temporary-directory)))

(defun file-text (path)
  "PATH's contents as text, or \"\" when it holds none."
  (if (probe-file path)
      (handler-case (uiop:read-file-string path :external-format :utf-8)
        (error () ""))
      ""))

(defun program-run (program args timeout)
  "Run PROGRAM with ARGS and standard input at /dev/null, for at most TIMEOUT
seconds. => (values STDOUT STDERR EXIT-CODE TIMED-OUT-P). The streams are
read back whole from files and a program past its deadline is killed, so a
wallet call that never answers cannot hold the turn. The environment is the
image's own: that is where mm finds its login and its token."
  (let ((out (temporary "out")) (err (temporary "err")))
    (unwind-protect
         (let* ((process (sb-ext:run-program program args
                                             :search nil
                                             :input nil
                                             :output out
                                             :error err
                                             :if-output-exists :supersede
                                             :wait nil))
                (deadline (+ (get-internal-real-time)
                             (* timeout internal-time-units-per-second)))
                (timed-out nil))
           (loop
             (unless (sb-ext:process-alive-p process) (return))
             (when (>= (get-internal-real-time) deadline)
               (setf timed-out t)
               (ignore-errors (sb-ext:process-kill process 15))
               (sleep 0.1)
               (when (sb-ext:process-alive-p process)
                 (ignore-errors (sb-ext:process-kill process 9)))
               (return))
             (sleep 0.02))
           (sb-ext:process-wait process)
           (values (file-text out) (file-text err)
                   (ignore-errors (sb-ext:process-exit-code process))
                   timed-out))
      (ignore-errors (delete-file out))
      (ignore-errors (delete-file err)))))

(defvar *run* #'program-run
  "The one seam that runs the program: (PROGRAM ARGS TIMEOUT) => (values STDOUT
STDERR EXIT-CODE TIMED-OUT-P). Tests replace it; nothing else does.")

;;; --- what mm prints ----------------------------------------------------------------

(defun decoded-object (text)
  "TEXT as one decoded JSON object, or NIL when it is not exactly one."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (when (and text (plusp (length text)))
      (handler-case
          (let ((value (nlk:decode-json text :whole t)))
            (and (hash-table-p value) value))
        (error () nil)))))

(defun objects-in (text)
  "Every JSON object TEXT carries, outermost first: the whole of it when that
is one value, else a line at a time (NDJSON), else from its first { to its
last }."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (cond
      ((or (null text) (zerop (length text))) '())
      ((decoded-object text) (list (decoded-object text)))
      (t (let ((lines (loop for line in (nlk:lines text)
                            for object = (decoded-object line)
                            when object collect object)))
           (or lines
               (let* ((open (position #\{ text))
                      (close (position #\} text :from-end t)))
                 (when (and open close (< open close))
                   (let ((object (decoded-object (subseq text open (1+ close)))))
                     (when object (list object)))))))))))

(defun envelope-of (stdout stderr)
  "The CLI's own envelope among the objects in its output, with every
{\"_notice\": ...} line kept beside it under \"notices\"; NIL when it reported
none."
  (let* ((objects (append (objects-in stdout) (objects-in stderr)))
         (notices (remove-if-not (lambda (object) (hash-table-p (gethash "_notice" object)))
                                 objects))
         (envelope (find-if (lambda (object) (nth-value 1 (gethash "ok" object))) objects))
         (bare (find-if-not (lambda (object) (member object notices)) objects)))
    (cond
      (envelope
       (when notices (setf (gethash "notices" envelope) (coerce notices 'vector)))
       envelope)
      (bare (nlk:make-json-object "ok" t "data" bare))
      (t nil))))

(defun error-text (envelope)
  "The CLI's own refusal as one sentence: its code, its message, its hint."
  (let* ((error (nlk:json-value envelope :object "error"))
         (code (nlk:json-value error :string "code"))
         (message (nlk:json-value error :string "message"))
         (hint (nlk:json-value error :string "hint")))
    (format nil "~@[~a: ~]~a~@[ -- ~a~]"
            code (or message "mm refused the command") hint)))

(defun command-line (args)
  "ARGS as a person would type them, for a refusal that says what failed."
  (format nil "mm ~{~a~^ ~}" args))

(defun mm-envelope (args &key (timeout (setting :timeout-seconds)) (json t))
  "Run `mm ARGS --json' here and answer the envelope it printed, notices and
all. A program that is not there, a run past TIMEOUT and output that is no
JSON each refuse with METAMASK-ERROR, as the plugin refused for the same
reasons."
  (let* ((program (setting :program))
         (found (program-path program)))
    (unless found
      (fail "metamask.program is ~s: no such program on PATH -- install the MetaMask agent wallet CLI with \"npm i -g @metamask/agent-wallet\" (Node.js 22.18+), or give its path in the metamask section"
            program))
    (let ((argv (append args (when json '("--json")))))
      (multiple-value-bind (stdout stderr exit timed-out)
          (funcall *run* found argv timeout)
        (when timed-out
          (fail "~a did not finish within ~as; raise metamask.timeout_seconds, or check `mm doctor`"
                (command-line argv) timeout))
        (let ((envelope (envelope-of stdout stderr)))
          (unless envelope
            (let ((noise (nlk:trimmed (or stderr stdout ""))))
              (fail "~a exited ~a and printed no JSON~@[: ~a~]"
                    (command-line argv) exit
                    (when (plusp (length noise)) (nlk:clip (nlk:one-line noise) 300)))))
          envelope)))))

(defun mm-answer (args &key (timeout (setting :timeout-seconds)))
  "The envelope `mm ARGS' printed, refusing in the CLI's own words when it said
ok=false."
  (let ((envelope (mm-envelope args :timeout timeout)))
    (unless (eq t (gethash "ok" envelope))
      (fail "~a" (error-text envelope)))
    envelope))

(defun scrubbed (value)
  "VALUE with every JSON null read as false, so an answer decodes and encodes
again unchanged elsewhere."
  (cond
    ((eq value :null) nil)
    ((hash-table-p value)
     (let ((copy (make-hash-table :test #'equal)))
       (maphash (lambda (key member) (setf (gethash key copy) (scrubbed member))) value)
       copy))
    ((and (vectorp value) (not (stringp value)))
     (map 'vector #'scrubbed value))
    (t value)))

(defun rendered (object &rest extra)
  "OBJECT as the compact JSON the model reads, with EXTRA key/value pairs set
over it first."
  (loop for (key value) on extra by #'cddr
        do (setf (gethash key object) value))
  (nlk:encode-json-object (scrubbed object)))
