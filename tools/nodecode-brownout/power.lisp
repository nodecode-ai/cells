;;;; power.lisp --- the host's power reading, and the three sources it comes from.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the original's core/power.py. Every reader answers a SAMPLE
;;;; or NIL and never guesses: a machine with no battery (a desktop, a
;;;; container) has no reading, and the engine says so instead of inventing one.
;;;;
;;;; The sources, in the order READ-POWER-FROM-HOST tries them:
;;;;   a NUT UPS, when the section names one (`upsc <name>`);
;;;;   macOS `pmset -g batt`;
;;;;   Linux /sys/class/power_supply.
;;;; Both commands are looked up on PATH as the user's own tools, output
;;;; parsed as text; nothing here reaches the network.

(in-package #:nodecode-brownout)

(defstruct (sample (:constructor make-sample (&key (time 0) on-ac pct (source "unknown") os-minutes)))
  "One reading of the host's power. TIME is this process's own clock in
seconds, which is all the runway forecast needs; PCT is the charge 0-100, or
NIL when the source has no battery figure."
  (time 0 :type real)
  on-ac
  (pct nil)
  (source "unknown" :type string)
  (os-minutes nil))

(defun now-seconds ()
  "This process's clock, in seconds: monotonic within a run, never a date."
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(defun nonblank (text)
  (and (stringp text) (plusp (length (string-trim " " text))) (string-trim " " text)))

;;; --- macOS: pmset -------------------------------------------------------------

(defun digits-before (text at)
  "The integer ending just before position AT in TEXT, or NIL."
  (let ((start at))
    (loop while (and (plusp start) (digit-char-p (char text (1- start))))
          do (decf start))
    (and (< start at) (ignore-errors (parse-integer text :start start :end at)))))

(defun pmset-minutes-left (raw)
  "The OS's own time-left estimate in RAW, in minutes, or NIL. Kept for
comparison only: the runway the cell acts on is the measured one."
  (let ((at (search " remaining" raw)))
    (when at
      (let* ((end (position #\Space raw :end at :from-end t))
             (colon (and end (position #\: raw :start (or end 0) :end at))))
        (when colon
          (let ((hours (digits-before raw colon))
                (minutes (ignore-errors (parse-integer raw :start (1+ colon) :end at))))
            (and hours minutes (+ (* 60 hours) minutes))))))))

(defun parse-pmset (raw)
  "A SAMPLE read off `pmset -g batt` output, or NIL when it names no battery."
  (let ((at (and (stringp raw) (search "%" raw))))
    (when at
      (let ((pct (digits-before raw at)))
        (when pct
          (make-sample :time (now-seconds)
                       :on-ac (not (null (search "'AC Power'" raw)))
                       :pct pct
                       :source "pmset"
                       :os-minutes (pmset-minutes-left raw)))))))

;;; --- Linux: /sys/class/power_supply --------------------------------------------

(defun read-trimmed (path)
  "PATH's text, trimmed, or NIL when it cannot be read."
  (handler-case
      (let ((text (uiop:read-file-string path)))
        (string-trim '(#\Space #\Tab #\Newline #\Return) text))
    (error () nil)))

(defun parse-sysfs (root)
  "A SAMPLE read off /sys/class/power_supply under ROOT, or NIL when the
machine has no battery there."
  (when (uiop:directory-exists-p root)
    (let ((on-ac nil) (pct nil) (battery nil))
      (dolist (device (ignore-errors (uiop:subdirectories root)))
        (let ((kind (read-trimmed (merge-pathnames "type" device))))
          (cond ((equal kind "Mains")
                 (when (equal (read-trimmed (merge-pathnames "online" device)) "1")
                   (setf on-ac t)))
                ((equal kind "Battery")
                 (setf battery t)
                 (let ((capacity (read-trimmed (merge-pathnames "capacity" device))))
                   (when (and capacity (every #'digit-char-p capacity))
                     (setf pct (ignore-errors (parse-integer capacity)))))
                 (when (member (read-trimmed (merge-pathnames "status" device))
                               '("Charging" "Full") :test #'equal)
                   (setf on-ac t))))))
      (when battery
        (make-sample :time (now-seconds) :on-ac on-ac :pct pct :source "sysfs")))))

;;; --- a NUT UPS: upsc -----------------------------------------------------------

(defun parse-upsc (raw)
  "A SAMPLE read off `upsc <name>` output, or NIL when it carries no
ups.status line."
  (let* ((rows (loop for line in (uiop:split-string (or raw "") :separator '(#\Newline))
                     for at = (search ": " line)
                     when at collect (cons (subseq line 0 at) (subseq line (+ at 2)))))
         (status (cdr (assoc "ups.status" rows :test #'equal))))
    (when (nonblank status)
      (let ((words (uiop:split-string status :separator '(#\Space))))
        (make-sample :time (now-seconds)
                     :on-ac (and (member "OL" words :test #'equal)
                                 (not (member "OB" words :test #'equal)))
                     :pct (let ((charge (cdr (assoc "battery.charge" rows :test #'equal))))
                            (and charge (ignore-errors (parse-integer charge :junk-allowed t))))
                     :source "nut"
                     :os-minutes (let ((runtime (cdr (assoc "battery.runtime" rows :test #'equal))))
                                   (and runtime
                                        (ignore-errors
                                          (/ (parse-integer runtime :junk-allowed t) 60)))))))))

;;; --- the host -------------------------------------------------------------------

(defun run-quietly (argv)
  "ARGV's standard output, or NIL when the program is absent or fails. The
user's own CLI, looked up on PATH: no shell, no interpolation."
  (handler-case
      (uiop:run-program argv :output :string :error-output :string
                            :ignore-error-status t)
    (error () nil)))

(defun read-power-from-host (&optional ups)
  "The best reading this host offers: the configured UPS, then pmset, then
sysfs. NIL when none of them can say."
  (or (let ((name (nonblank ups)))
        (and name (let ((out (run-quietly (list "upsc" name))))
                    (and (nonblank out) (parse-upsc out)))))
      (let ((out (run-quietly '("pmset" "-g" "batt"))))
        (and (nonblank out) (parse-pmset out)))
      (parse-sysfs #p"/sys/class/power_supply/")))
