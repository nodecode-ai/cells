;;;; machine.lisp --- the smolvm CLI: one program, one call, one answer shape.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A smol machine is a microVM on this computer with its own Linux kernel,
;;;; booted from an OCI image, so a command run in one reads and writes nothing
;;;; of the host's files. The plugin this cell rewrites drove those machines
;;;; through their Python SDK; the engine's own CLI drives the same machines,
;;;; and this cell calls it -- `smolvm machine run' for a one-off that is
;;;; removed when the command exits, `machine create' and `machine exec' for one
;;;; that keeps its files between calls, and `machine ls', `stop' and `delete'
;;;; to see and free them.
;;;;
;;;; A call answers the same plist whatever ran it, so the JSON a model reads
;;;; and the lines /smol prints are written once:
;;;;
;;;;   (:output "text" :exit-code 0 :timed-out-p nil)
;;;;
;;;; Nothing here writes a file of its own: a machine's disk belongs to the
;;;; engine.

(in-package #:nodecode-smol)

;;; --- the seams -----------------------------------------------------------------------

(defvar *run* nil
  "NIL runs a command on this computer; a function of (ARGV TIMEOUT) answering the
plist above is what a test or a layer substitutes.")

(defvar *which* nil
  "NIL probes for the binary; a function of the name answering a boolean is what a
test or a layer substitutes.")

;;; --- the machine a call gets ----------------------------------------------------------

(defun fnv1a-32 (string)
  "STRING's FNV-1a hash, as a 32-bit non-negative fixnum."
  (let ((hash #x811c9dc5))
    (dotimes (index (length string) hash)
      (setf hash (logand #xffffffff
                          (* (logxor hash (logand #xffff (char-code (char string index))))
                             #x01000193))))))

(defun session-machine-name ()
  "The name of the machine this session keeps: the same one its every call reuses,
and the same one after a restart of this process."
  (let ((session (getf (nle:turn) :session-id)))
    (format nil "nodecode-smol-~(~8,'0x~)" (fnv1a-32 (if (stringp session) session "default")))))

(defun on-path-p (name)
  "Whether NAME is a file in one of PATH's directories."
  (let ((path (uiop:getenv "PATH")))
    (and path
         (loop for directory in (uiop:split-string path :separator (if (uiop:os-windows-p) '(#\;) '(#\:)))
               thereis (let* ((candidate (merge-pathnames name (uiop:ensure-directory-pathname directory)))
                              (found (or (probe-file candidate)
                                         (and (uiop:os-windows-p)
                                              (probe-file (make-pathname :type "exe" :defaults candidate))))))
                         (and found t))))))

(defun binary-present-p ()
  "Whether the engine's CLI the smol section names is on this machine."
  (let ((name (setting :binary)))
    (if *which*
        (funcall *which* name)
        (or (and (find #\/ name) (probe-file name) t) (on-path-p name)))))

(defun demand-binary ()
  "Refuse unless the engine's CLI is here: the cell's one prerequisite, and the
setting to fix when it is missing."
  (unless (binary-present-p)
    (fail "~a is not on this machine: install the smolvm CLI from smolmachines.com, or set `binary` in the smol section"
          (setting :binary))))

;;; --- one program per shape of call ------------------------------------------------------

(defun extra-args ()
  "The extra arguments the section hands the engine, for a flag this cell does not name."
  (coerce (setting :extra-args) 'list))

(defun net-args (network)
  "The engine's outbound-network switch, when NETWORK is on."
  (when network (list "--net")))

(defun one-off-argv (command image network)
  "ARGV of a machine booted from IMAGE for COMMAND alone, removed when it exits."
  (append (list (setting :binary) "machine" "run")
          (net-args network)
          (list "--image" image)
          (extra-args)
          (list "--" "bash" "-c" command)))

(defun create-argv (name image network)
  "ARGV of the machine NAME, made once and then reused by every call that names it."
  (append (list (setting :binary) "machine" "create")
          (net-args network)
          (list "--name" name "--image" image)
          (extra-args)))

(defun exec-argv (name command)
  "ARGV of COMMAND in the machine NAME."
  (list (setting :binary) "machine" "exec" "--name" name "--" "bash" "-c" command))

(defun list-argv ()
  "ARGV of the machines this computer holds."
  (list (setting :binary) "machine" "ls"))

(defun stop-argv (name)
  (list (setting :binary) "machine" "stop" "--name" name))

(defun delete-argv (name)
  (list (setting :binary) "machine" "delete" "--name" name))

;;; --- running one ----------------------------------------------------------------------

(defun launch (argv timeout)
  "ARGV's combined output and its exit code, run on this computer. A command that
outlives TIMEOUT seconds is killed and answers 124, the code timeout(1) uses."
  (handler-case
      (let* ((process (uiop:launch-program argv :output :stream :error-output :output
                                                 :ignore-error-status t))
             (stream (uiop:process-info-output process))
             (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second)))
             (collected '())
             (finished nil)
             (code 124))
        (unwind-protect
             (progn
               (loop
                 (loop for char = (read-char-no-hang stream)
                       while char do (push char collected))
                 (cond ((sb-ext:process-alive-p process)
                        (if (> (get-internal-real-time) deadline) (return) (sleep 0.01)))
                       (t ;; the process is gone; one more read drains the pipe
                        (loop for char = (read-char-no-hang stream)
                              while char do (push char collected))
                        (setf finished t)
                        (return))))
               (setf code (if finished (or (ignore-errors (uiop:wait-process process)) 0) 124)))
          (unless finished (ignore-errors (sb-ext:process-kill process 9)))
          (ignore-errors (close stream)))
        (list :output (coerce (nreverse collected) 'string)
              :exit-code code
              :timed-out-p (not finished)))
    (error (condition)
      (fail "~a could not be run: ~a" (first argv) condition))))

(defun run-argv (argv timeout)
  "ARGV's answer, from *RUN* when a test or a layer set one, else from this computer."
  (if *run* (funcall *run* argv timeout) (launch argv timeout)))

(defun exec-in (name command image network timeout)
  "COMMAND in NAME's machine, made from IMAGE when this is the first call to name it.
=> (values RESULT NOTES); a create that failed is a note, never the answer."
  (let* ((created (run-argv (create-argv name image network) timeout))
         (notes (unless (zerop (getf created :exit-code))
                  (list (format nil "~a: ~a" name (first-line (getf created :output)))))))
    (values (run-argv (exec-argv name command) timeout) notes)))

(defun first-line (text)
  "TEXT up to its first line break, trimmed: one line of a CLI's complaint."
  (let* ((text (or text ""))
         (break (position #\Newline text)))
    (nlk:trimmed (if break (subseq text 0 break) text))))

;;; --- what a caller reads ----------------------------------------------------------------

(defun clip (text limit)
  "TEXT as much of its first LIMIT characters as one answer carries.
=> (values TEXT CLIPPED-P)"
  (if (> (length text) limit) (values (subseq text 0 limit) t) (values text nil)))

(defun seconds-since (start)
  (max 0.0 (/ (- (get-internal-real-time) start) (float internal-time-units-per-second))))

(defun image-for (call)
  "The image a machine for CALL boots: the one the call names, else the section's."
  (let ((asked (nlk:trimmed (or (smol-run-image call) ""))))
    (if (plusp (length asked)) asked (setting :image))))

(defun machine-for (call)
  "The machine CALL names, or the one its session keeps when it asks to keep one."
  (let ((named (nlk:trimmed (or (smol-run-machine call) ""))))
    (cond ((plusp (length named)) named)
          ((smol-run-keep call) (session-machine-name))
          (t nil))))

(defun run-notes (result timeout clipped-p limit)
  "What an answer says about the run itself, beyond the command's own output."
  (append (when (getf result :timed-out-p)
            (list (format nil "the command outlived ~d second~:p and was killed" timeout)))
          (when clipped-p
            (list (format nil "output clipped to ~d characters; raise max_output, or write the rest to a file in the machine" limit)))
          (when (and (not (getf result :timed-out-p)) (not (zerop (getf result :exit-code))))
            (list (format nil "the command exited ~d" (getf result :exit-code))))))

(defun run-answer (call)
  "CALL's command in a machine, as the JSON string a model reads."
  (demand-binary)
  (let* ((command (smol-run-command call))
         (image (image-for call))
         (network (and (setting :network) (not (smol-run-no-network call))))
         (timeout (or (smol-run-timeout-seconds call) (setting :timeout-seconds)))
         (limit (setting :max-output))
         (named (machine-for call))
         (started (get-internal-real-time))
         (result nil)
         (notes '()))
    (multiple-value-setq (result notes)
      (if named
          (exec-in named command image network timeout)
          (values (run-argv (one-off-argv command image network) timeout) '())))
    (multiple-value-bind (output clipped-p) (clip (or (getf result :output) "") limit)
      (nlk:encode-json-object
       (nlk:make-json-object
        "ok" (and (not (getf result :timed-out-p)) (zerop (getf result :exit-code)))
        "exit_code" (getf result :exit-code)
        "output" output
        "clipped" (and clipped-p t)
        "machine" named
        "image" image
        "network" (and network t)
        "seconds" (/ (round (* 100 (seconds-since started))) 100.0)
        "notes" (coerce (append notes (run-notes result timeout clipped-p limit)) 'vector))))))

(defun answer (call)
  "The tool's answer to CALL, a refusal included: always a JSON string."
  (handler-case (run-answer call)
    (smol-error (condition)
      (nlk:encode-json-object
       (nlk:make-json-object "ok" nil "error" (smol-error-detail condition))))))

;;; --- the machines on this computer -------------------------------------------------------

(defun name-shaped-p (word)
  "Whether WORD reads as a machine name: a letter or digit, then letters, digits, `_',
`-', `.'."
  (and (plusp (length word))
       (alphanumericp (char word 0))
       (every (lambda (char)
                (or (alphanumericp char) (find char "_-.")))
              word)))

(defun machine-names (text)
  "The machine names in TEXT, the CLI's own listing: the first word of each line that
reads as one, the header left out."
  (let ((names '()))
    (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)))
      (let ((first (first (uiop:split-string (string-trim '(#\Space #\Tab #\Return) line)
                                              :separator '(#\Space #\Tab)))))
        (when (and first (name-shaped-p first)
                   (not (member (string-upcase first) '("NAME" "MACHINES" "MACHINE") :test #'string=)))
          (push first names))))
    (nreverse names)))

(defun machines-answer (&optional name)
  "The machines this computer holds, or the one NAME names, as the JSON string a model
reads."
  (demand-binary)
  (let* ((result (run-argv (list-argv) (setting :timeout-seconds)))
         (names (machine-names (getf result :output)))
         (wanted (nlk:trimmed (or name "")))
         (shown (if (plusp (length wanted))
                    (remove-if-not (lambda (one) (string= one wanted)) names)
                    names)))
    (if (zerop (getf result :exit-code))
        (nlk:encode-json-object
         (nlk:make-json-object
          "ok" t
          "machines" (coerce shown 'vector)
          "raw" (getf result :output)
          "notes" (coerce (append (when (and (plusp (length wanted)) (null shown))
                                    (list (format nil "no machine named ~a on this computer" wanted)))
                                  (list "a name is the first column of `smolvm machine ls`; raw is its text, and a machine's state, image and resources are the CLI's own columns"))
                           'vector)))
        (nlk:encode-json-object
         (nlk:make-json-object
          "ok" nil
          "error" (format nil "smolvm machine ls exited ~d: ~a"
                           (getf result :exit-code) (first-line (getf result :output))))))))
