;;;; read.lisp --- the machine's process rows: one reader per kind of system.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A row is a plist (:PID :PARENT-PID :NAME :PATH :STARTED :THREADS) and
;;;; nothing else is claimed about a process. Three readers answer rows, and
;;;; every one of them only reads:
;;;;
;;;;   :PROC        Linux: /proc/PID/stat for the PID, the parent, the name
;;;;                and the start time (clock ticks since boot, laid on the
;;;;                boot time /proc/stat carries), /proc/PID/status for the
;;;;                thread count, and the /proc/PID/exe link for the image.
;;;;   :PS          macOS and the other BSDs, where /proc does not exist:
;;;;                `ps -axo pid=,ppid=,etime=,comm=', whose ETIME turns a
;;;;                start time into an age. No thread count.
;;;;   :POWERSHELL  Windows: Get-CimInstance Win32_Process, the same rows
;;;;                Task Manager prints, created time and image path included.
;;;;
;;;; Times are UNIVERSAL-TIME internally and become ISO 8601 UTC at the
;;;; boundary. A reader answers NIL for what it cannot see -- another
;;;; account's image, a process that exited between two reads -- and the
;;;; table reports that as an absent path and an unknown parent, never as a
;;;; failure of the call.

(in-package #:nodecode-process-table)

;;; --- small text helpers -----------------------------------------------------

(defun split-lines (text)
  "TEXT as lines, the empty ones dropped."
  (let ((lines '()) (start 0))
    (loop for at from 0 below (length text)
          when (char= (char text at) #\Newline)
            do (let ((line (subseq text start at)))
                 (when (plusp (length line)) (push line lines)))
               (setf start (1+ at)))
    (let ((last (subseq text start)))
      (when (plusp (length last)) (push last lines)))
    (nreverse lines)))

(defun words (text)
  "TEXT split on runs of space and tab."
  (let ((words '()) (start nil))
    (loop for at from 0 below (length text)
          for character = (char text at)
          do (if (member character '(#\Space #\Tab #\Return))
                 (when start (push (subseq text start at) words) (setf start nil))
                 (unless start (setf start at))))
    (when start (push (subseq text start) words))
    (nreverse words)))

(defun slurp (path)
  "PATH's text read line by line, NIL when it cannot be opened.
Line-wise on purpose: a file under /proc reports a length of zero, so a
read sized from the length would come back empty."
  (handler-case
      (with-open-file (in path :direction :input :if-does-not-exist nil
                               :element-type 'character)
        (when in
          (with-output-to-string (out)
            (loop for line = (read-line in nil nil)
                  while line
                  do (write-line line out)))))
    (error () nil)))

(defun link-target (path)
  "What the symbolic link PATH points at, or NIL."
  #+sbcl (ignore-errors (sb-posix:readlink path))
  #-sbcl nil)

(defun parse-etimes (text)
  "Seconds since start from `ps''s ETIME, [[dd-]hh:]mm:ss, or NIL."
  (handler-case
      (let* ((dash (position #\- text))
             (days (if dash (parse-integer text :end dash) 0))
             (clock (uiop:split-string (if dash (subseq text (1+ dash)) text)
                                        :separator ":")))
        (let ((numbers (mapcar (lambda (part) (parse-integer part)) clock)))
          (case (length numbers)
            (3 (+ (* 86400 days) (* 3600 (first numbers)) (* 60 (second numbers)) (third numbers)))
            (2 (+ (* 86400 days) (* 60 (first numbers)) (second numbers)))
            (1 (+ (* 86400 days) (first numbers)))
            (t nil))))
    (error () nil)))

(defun iso-time (universal)
  "UNIVERSAL as the ISO 8601 UTC stamp the table prints, or NIL."
  (when universal
    (multiple-value-bind (second minute hour day month year)
        (decode-universal-time universal 0)
      (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
              year month day hour minute second))))

(defun parse-iso (text)
  "UNIVERSAL-TIME for the leading YYYY-MM-DDTHH:MM:SS of TEXT, or NIL."
  (when (and (stringp text) (>= (length text) 19))
    (handler-case
        (encode-universal-time (parse-integer text :start 17 :end 19)
                               (parse-integer text :start 14 :end 16)
                               (parse-integer text :start 11 :end 13)
                               (parse-integer text :start 8 :end 10)
                               (parse-integer text :start 5 :end 7)
                               (parse-integer text :start 0 :end 4)
                               0)
      (error () nil))))

(defun trimmed-error (text)
  "TEXT trimmed when it is a string with anything in it, NIL otherwise: what
a command said on its way to failing, or nothing worth adding."
  (let ((line (and (stringp text) (nlk:trimmed text))))
    (and line (plusp (length line)) line)))

;;; --- which reader ----------------------------------------------------------

(defun reader-kind ()
  "The reader this run uses: the section's `reader', or what the system is."
  (let ((chosen (or (setting :reader) "auto")))
    (cond ((string-equal chosen "auto")
           (cond ((uiop:os-windows-p) :powershell)
                 ((probe-file #p"/proc/stat") :proc)
                 (t :ps)))
          ((string-equal chosen "proc") :proc)
          ((string-equal chosen "ps") :ps)
          ((string-equal chosen "powershell") :powershell)
          (t (fail "~s is no reader" chosen)))))

(defun ticks-per-second ()
  "Clock ticks a second, for the /proc start time. Linux's own value is 100."
  (or (setting :clock-ticks) 100))

(defun read-timeout ()
  (or (setting :timeout-seconds) 10))

;;; --- Linux: /proc ----------------------------------------------------------

(defun proc-boot-time ()
  "The boot time /proc/stat carries in `btime', as a UNIVERSAL-TIME."
  (let ((stat (slurp #p"/proc/stat")))
    (and stat
         (loop for line in (split-lines stat)
               when (uiop:string-prefix-p "btime " line)
                 do (return (+ 2208988800 (parse-integer line :start 6)))))))

(defun proc-stat-row (pid ticks boot)
  "One row from /proc/PID/stat, or NIL.
The command name sits in parentheses and may hold a space or a closing
parenthesis of its own, so the fields are read from the LAST `)' on."
  (let* ((text (slurp (format nil "/proc/~d/stat" pid)))
         (close (and text (position #\) text :from-end t)))
         (open (and text (position #\( text))))
    (when (and text close open (< open close))
      (let ((fields (words (subseq text (1+ close))))
            (name (subseq text (1+ open) close))
            (threads (proc-threads pid)))
        (when (>= (length fields) 20)
          (list :pid pid
                :parent-pid (parse-integer (nth 1 fields) :junk-allowed t)
                :name name
                :path (link-target (format nil "/proc/~d/exe" pid))
                :started (and boot ticks (plusp ticks)
                              (+ boot (floor (or (parse-integer (nth 19 fields) :junk-allowed t) 0)
                                             ticks)))
                :threads threads))))))

(defun proc-threads (pid)
  "The thread count /proc/PID/status carries, or NIL."
  (let ((status (slurp (format nil "/proc/~d/status" pid))))
    (and status
         (loop for line in (split-lines status)
               when (uiop:string-prefix-p "Threads:" line)
                 do (return (parse-integer line :start 8 :junk-allowed t))))))

(defun proc-pids ()
  "Every PID directory in /proc, newest number first."
  (sort (loop for path in (ignore-errors (directory #p"/proc/*/"))
              for name = (car (last (pathname-directory path)))
              for pid = (and name (every #'digit-char-p name) (parse-integer name))
              when pid collect pid)
        #'<))

(defun proc-rows ()
  "Every process /proc reports."
  (let ((boot (proc-boot-time)) (ticks (ticks-per-second)) (rows '()))
    (dolist (pid (proc-pids))
      (let ((row (ignore-errors (proc-stat-row pid ticks boot))))
        (when row (push row rows))))
    rows))

;;; --- macOS and the BSDs: ps -------------------------------------------------

(defun ps-rows ()
  "Every process `ps' reports: PID, parent, name, path and an age-derived
start time. No thread count."
  (multiple-value-bind (output error status)
      (nlk:run-bounded (list "ps" "-axo" "pid=,ppid=,etime=,comm=")
                       :seconds (read-timeout))
    (unless (eql status 0)
      (fail "ps did not answer (status ~a)~@[: ~a~]" status (trimmed-error error)))
    (let ((now (get-universal-time)) (rows '()))
      (dolist (line (split-lines (or output "")))
        (let* ((fields (words line))
               (pid (and (>= (length fields) 4) (parse-integer (first fields) :junk-allowed t)))
               (parent (and pid (parse-integer (second fields) :junk-allowed t)))
               (elapsed (and pid (parse-etimes (third fields)))))
          (when pid
            (let ((image (format nil "~{~a~^ ~}" (nthcdr 3 fields))))
              (push (list :pid pid :parent-pid (or parent 0)
                          :name (or (car (last (uiop:split-string image :separator "/"))) image)
                          :path (and (plusp (length image)) image)
                          :started (and elapsed (- now elapsed))
                          :threads nil)
                    rows)))))
      (nreverse rows))))

;;; --- Windows: Get-CimInstance ----------------------------------------------

(defparameter +powershell-script+
  (concatenate 'string
               "Get-CimInstance Win32_Process | ForEach-Object { "
               "'{0}|{1}|{2}|{3}|{4}' -f $_.ProcessId, $_.ParentProcessId, $_.Name, "
               "$_.CreationDate.ToUniversalTime().ToString('o'), $_.ExecutablePath }")
  "The one query the Windows reader runs: the rows Task Manager reads,
created time and image path included, one line each, `|' separated.")

(defun powershell-rows ()
  "Every process Win32_Process reports."
  (multiple-value-bind (output error status)
      (nlk:run-bounded (list "powershell" "-NoProfile" "-NonInteractive" "-Command"
                             +powershell-script+)
                       :seconds (read-timeout))
    (unless (eql status 0)
      (fail "powershell did not answer (status ~a)~@[: ~a~]" status (trimmed-error error)))
    (let ((rows '()))
      (dolist (line (split-lines (or output "")))
        (let ((fields (uiop:split-string line :separator "|")))
          (when (>= (length fields) 4)
            (let ((pid (parse-integer (first fields) :junk-allowed t))
                  (parent (parse-integer (second fields) :junk-allowed t)))
              (when pid
                (push (list :pid pid :parent-pid (or parent 0)
                            :name (third fields)
                            :path (let ((path (fourth fields)))
                                    (and path (plusp (length path)) path))
                            :started (parse-iso (fourth fields))
                            :threads nil)
                      rows))))))
      (nreverse rows))))

;;; --- the seam ---------------------------------------------------------------

(defvar *row-source* nil
  "A function of no arguments answering rows, standing in for the machine's
own reader: what a test binds so it reads a table it wrote.")

(defun read-rows ()
  "Every row this machine's reader reports."
  (if *row-source*
      (funcall *row-source*)
      (case (reader-kind)
        (:proc (proc-rows))
        (:ps (ps-rows))
        (:powershell (powershell-rows))
        (t (fail "no reader for this system")))))
