;;;; cli.lisp --- the wslc command line, as this cell talks to it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's hermes_wslc_sandbox/wslc.py carried over: the binary
;;;; resolution order, `ps' read as NDJSON and `inspect' as an array,
;;;; existence decided by the exit status rather than by a message (wslc
;;;; follows the Windows display language, so its prose is localized), and the
;;;; `run --help' probe that decides which hardening flags this build takes
;;;; instead of assuming Docker's parity.
;;;;
;;;; *CLI* is the one seam a test rebinds: no wslc runs for a test.

(in-package #:nodecode-wslc)

;;; --- the seam -----------------------------------------------------------------

(defun bounded-cli (argv &key seconds)
  "ARGV through NLK:RUN-BOUNDED: its stdout, its stderr and its exit status --
:TIMEOUT when the bound fell first and the child's whole tree was killed."
  (nlk:run-bounded argv :seconds (or seconds 60)))

(defparameter *cli* #'bounded-cli
  "How this cell starts a process: (LAMBDA (ARGV &KEY SECONDS) => stdout,
stderr, status). A test rebinds it, so nothing is spawned.")

(defun call-cli (argv &key seconds)
  "Run ARGV through *CLI*, answering its stdout, stderr and status."
  (funcall *cli* argv :seconds seconds))

;;; --- finding the CLI ------------------------------------------------------------

(defparameter +names+ '("wslc" "wslc.exe" "container.exe")
  "The names the CLI is on PATH under; `container.exe' ships beside it as an alias.")

(defparameter +windows-tails+ '("WSL/wslc.exe" "WSL/container.exe")
  "Where WSL 3.0.3+ puts the CLI, under %ProgramFiles%.")

(defvar *binary* nil
  "The CLI this start resolved, so one start asks PATH once.")

(defun first-line (text)
  "TEXT's first line, trimmed, or NIL when there is none."
  (let ((line (first (uiop:split-string (or text "") :separator '(#\Newline)))))
    (let ((trimmed (and line (string-trim '(#\Space #\Tab #\Return) line))))
      (and trimmed (plusp (length trimmed)) trimmed))))

(defun executable (path)
  "PATH as a native namestring when it names a file on this machine, or NIL."
  (when (stringp path)
    (let ((text (string-trim '(#\Space #\Tab) path)))
      (when (plusp (length text))
        (let ((found (ignore-errors (probe-file (uiop:parse-native-namestring text)))))
          (and found (uiop:native-namestring found)))))))

(defun on-path (name)
  "NAME's file in the first PATH directory holding it, or NIL."
  (let ((found (ignore-errors (nlk::executable-on-path name))))
    (and found (uiop:native-namestring found))))

(defun resolve-binary ()
  "The wslc CLI: the section's `binary', else PATH, else the default Windows
install location. WSLC-ERROR when there is none, saying what installs one."
  (let ((named (setting :binary)))
    (cond
      ((executable named))
      (named
       (fail "binary names ~s, and no such file is on this machine" named))
      (t
       (or (some #'on-path +names+)
           (let ((root (or (uiop:getenv "ProgramFiles") "C:/Program Files")))
             (loop for tail in +windows-tails+
                   for path = (concatenate 'string root "/" tail)
                   thereis (executable path)))
           (fail "the WSL Containers CLI (wslc) was not found: it ships with WSL 3.0.3+, so `wsl --update` and then `wslc --version` should work in a terminal; wslc.binary may name it outright"))))))

(defun binary ()
  "The resolved CLI: asked once per start."
  (or *binary* (setf *binary* (resolve-binary))))

(defun version (&optional (exe (binary)))
  "`wslc --version' first line, or NIL when the call failed."
  (multiple-value-bind (out err status) (call-cli (list exe "--version") :seconds 10)
    (declare (ignore err))
    (and (eql 0 status) (first-line out))))

;;; --- what this build of wslc takes ------------------------------------------------

(defparameter +hardening-flags+ '("--security-opt" "--cap-drop" "--pids-limit" "--init" "--storage-opt")
  "The Docker flags wslc may not implement. `run' exits 1 on an unknown option,
so probing them is what keeps a wslc sandbox from pretending to Docker's parity.")

(defvar *run-flags* (make-hash-table :test #'equal)
  "Per binary, the options `wslc run --help' printed: one probe, cached.")

(defun supported-run-flags (exe)
  "The options `wslc run --help' names, or NIL when the probe answered nothing:
callers read NIL as `assume nothing beyond the flags wslc has always taken'."
  (multiple-value-bind (cached known) (gethash exe *run-flags*)
    (if known
        cached
        (setf (gethash exe *run-flags*)
              (let ((flags '()))
                (multiple-value-bind (out err status)
                    (call-cli (list exe "run" "--help") :seconds 10)
                  (declare (ignore err))
                  (when (eql 0 status)
                    (dolist (token (uiop:split-string (substitute #\Space #\, (or out ""))
                                                       :separator '(#\Space #\Tab #\Newline #\Return)))
                      (when (and (> (length token) 2)
                                 (char= #\- (char token 0))
                                 (char= #\- (char token 1))
                                 (alpha-char-p (char token 2)))
                        (pushnew token flags :test #'equal)))))
                (nreverse flags))))))

(defun missing-hardening-flags (&optional (exe (binary)))
  "The Docker hardening flags this wslc build does not take. A failed probe
answers the whole list, the original's `assume nothing'."
  (let ((supported (supported-run-flags exe)))
    (if (null supported)
        (copy-list +hardening-flags+)
        (remove-if (lambda (flag) (member flag supported :test #'equal)) +hardening-flags+))))

;;; --- reading the runtime ----------------------------------------------------------

(defun field (object name)
  "OBJECT's member NAME, or NIL when OBJECT is not an object."
  (and (hash-table-p object) (gethash name object)))

(defun containers (&key (all t) (exe (binary)))
  "`wslc ps --format json' as a list of objects. wslc emits NDJSON here, one
object a line, and a localized banner is skipped rather than parsed."
  (multiple-value-bind (out err status)
      (call-cli (append (list exe "ps") (when all (list "-a")) (list "--format" "json"))
                :seconds 30)
    (declare (ignore err))
    (unless (eql 0 status)
      (fail "`wslc ps` failed with exit status ~a" status))
    (loop for line in (uiop:split-string (or out "") :separator '(#\Newline))
          for text = (string-trim '(#\Space #\Tab #\Return) line)
          when (and (plusp (length text)) (char= #\{ (char text 0)))
            append (let ((object (ignore-errors (nlk:decode-json text))))
                     (if (hash-table-p object) (list object) '())))))

(defun inspect-container (exe name)
  "`wslc inspect NAME --format json' as an object, or NIL when the object is
not there -- decided by the exit status, since the message is localized."
  (multiple-value-bind (out err status)
      (call-cli (list exe "inspect" name "--format" "json") :seconds 30)
    (declare (ignore err))
    (when (eql 0 status)
      (let ((object (ignore-errors (nlk:decode-json (or out "")))))
        (cond ((hash-table-p object) object)
              ((and (vectorp object) (plusp (length object)))
               (let ((first (aref object 0)))
                 (and (hash-table-p first) first)))
              (t nil))))))

(defun container-running-p (payload)
  "Whether PAYLOAD, an `inspect' answer, says the container runs."
  (let ((state (field payload "State")))
    (and (hash-table-p state) (field state "Running") t)))

(defun start-container (exe name)
  "Start a stopped container; WSLC-ERROR when the runtime would not."
  (multiple-value-bind (out err status) (call-cli (list exe "start" name) :seconds 120)
    (declare (ignore out))
    (unless (eql 0 status)
      (fail "`wslc start ~a` failed with exit status ~a~@[: ~a~]"
            name status (first-line err)))))

(defun stop-container (exe name)
  "Stop a running container, answering whether it worked."
  (multiple-value-bind (out err status) (call-cli (list exe "stop" name) :seconds 120)
    (declare (ignore out err))
    (eql 0 status)))

(defun remove-container (exe name)
  "Remove a container, answering whether it worked."
  (multiple-value-bind (out err status) (call-cli (list exe "rm" "-f" name) :seconds 60)
    (declare (ignore out err))
    (eql 0 status)))

;;; --- naming and starting ----------------------------------------------------------

(defparameter +name-prefix+ "nodecode-wslc"
  "Every sandbox this cell starts is named with it, and only such a name is
this cell's to remove.")

(defparameter +max-name-length+ 63
  "The container name width wslc takes, as Docker does.")

(defun safe-name (text)
  "TEXT cut down to a container name: letters, digits, dot, dash, underscore."
  (let* ((cleaned (with-output-to-string (out)
                    (loop for char across (or text "")
                          do (write-char (if (or (alphanumericp char) (find char "_.-"))
                                             char
                                             #\-)
                                         out))))
         (trimmed (string-trim "-." cleaned)))
    (if (plusp (length trimmed)) trimmed "default")))

(defun random-suffix ()
  "Eight hex characters: a non-persistent sandbox's own run."
  (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes 4) 'list)))

(defun sandbox-container-name (sandbox &key persistent)
  "The container name for SANDBOX: reproducible across sessions when
persistent, since that is what makes the sandbox resumable, and unique per
start when not, so a leftover from a crash is never mistaken for this one."
  (let ((base (format nil "~a-~a~@[-~a~]" +name-prefix+ (safe-name sandbox)
                       (unless persistent (random-suffix)))))
    (subseq base 0 (min (length base) +max-name-length+))))

(defun own-name-p (name)
  "Whether NAME is a container this cell started: its prefix, and nothing else."
  (and (stringp name)
       (uiop:string-prefix-p (concatenate 'string +name-prefix+ "-") (string-downcase name))))

(defun shell-quote (text)
  "TEXT in single quotes, for a shell: an inner quote is closed, escaped, reopened."
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for char across text
          do (if (char= char #\') (write-string "'\\''" out) (write-char char out)))
    (write-char #\' out)))

(defun build-run-argv (exe name image &key workdir env volumes cpus memory-mb network labels)
  "`wslc run -d' for one long-lived sandbox, kept alive by `sleep infinity'."
  (let ((argv (list exe "run" "-d" "--name" name)))
    (dolist (label labels)
      (setf argv (append argv (list "--label" (format nil "~a=~a" (car label) (cdr label))))))
    (when workdir
      (setf argv (append argv (list "--workdir" workdir))))
    (unless network
      (setf argv (append argv (list "--network" "none"))))
    (dolist (pair env)
      (setf argv (append argv (list "--env" (format nil "~a=~a" (car pair) (cdr pair))))))
    (dolist (spec volumes)
      (setf argv (append argv (list "--volume" spec))))
    (when (and cpus (plusp cpus))
      (setf argv (append argv (list "--cpus" (format nil "~a" cpus)))))
    (when (and memory-mb (plusp memory-mb))
      (setf argv (append argv (list "--memory" (format nil "~am" memory-mb)))))
    (append argv (list image "sleep" "infinity"))))
