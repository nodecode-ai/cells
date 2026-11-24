;;;; watch.lisp --- one printer sample against the last, and the events between.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure: no network, no clock of its own, no file. CELL.LISP reads the
;;;; printer and keeps the state this compares, so the diff is testable on
;;;; its own. Ported from the original's classify.py.

(in-package #:nodecode-octoprint)

(defparameter +state-version+ 1
  "The watch state file's version; a file of another version is not replaced.")

(defparameter +error-states+ '("Error" "Offline after error")
  "Job states that are an error, not a pause and not a disconnect.")

(defparameter +closed-connections+ '("Closed" "Offline")
  "Connection states that mean there is no printer on the other end.")

(defparameter +printing-states+ '("Printing" "Printing from SD")
  "Job states in which a print is running.")

(defparameter +pause-from+ (append +printing-states+ '("Pausing" "Resuming"))
  "The states a job may pause from.")

(defparameter +complete-from+ (append +printing-states+ '("Pausing" "Paused" "Resuming" "Finishing"))
  "The states a job may finish from.")

(defparameter +cancel-from+ (append +printing-states+ '("Pausing" "Paused" "Resuming" "Cancelling" "Finishing"))
  "The states a job may be cancelled from.")

;;; --- reading decoded JSON ---------------------------------------------------------

(defun jget (object key)
  "KEY's member of the decoded JSON OBJECT, or NIL."
  (and (hash-table-p object) (gethash key object)))

(defun jnum (value)
  "VALUE when it is a JSON number, else NIL; a boolean is not one."
  (and (realp value) (float value)))

(defun jstr (value)
  (and (stringp value) value))

(defun json-of (&rest pairs)
  "A JSON object from (KEY VALUE) pairs, KEY a string; a VALUE of :ABSENT is
left out, so a member the printer did not report is missing rather than false."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          unless (eq value :absent) do (setf (gethash key object) value))
    object))

(defun job-name (sample)
  "The job's filename, or a phrase for one OctoPrint did not name."
  (let ((name (getf sample :filename)))
    (if (and name (plusp (length name))) name "(no filename)")))

;;; --- what the printer's own strings mean --------------------------------------------

(defun kind-of (job-state connection-state)
  "One of error, disconnected, paused, printing or other, from OctoPrint's two
state strings. Error wins over a disconnect, and a closed connection wins over
a leftover Paused string. HTTP 409 is not an input here."
  (cond
    ((or (member job-state +error-states+ :test #'string=)
         (string= connection-state "Error"))
     "error")
    ((or (member connection-state +closed-connections+ :test #'string=)
         (string= job-state "Offline"))
     "disconnected")
    ((string= job-state "Paused") "paused")
    ((member job-state +printing-states+ :test #'string=) "printing")
    (t "other")))

;;; --- the events between two samples -------------------------------------------------

(defun event (kind text)
  (list :kind kind :text text))

(defun reported-p (reported kind)
  (and (member kind reported :test #'string=) t))

(defun stalled-p (previous sample now stall-minutes)
  "Whether SAMPLE shows a print that has not moved since PREVIOUS, for long enough."
  (let ((filepos (getf sample :filepos)))
    (and (numberp filepos)
         (equal (getf previous :filepos) filepos)
         (equal (getf previous :completion) (getf sample :completion))
         (equal (getf previous :filename) (getf sample :filename))
         (member (or (getf previous :job-state) "") +printing-states+ :test #'string=)
         (numberp (getf previous :stall-since))
         (>= (- now (getf previous :stall-since)) (* 60 stall-minutes)))))

(defun events-for (previous sample now stall-minutes)
  "Events since PREVIOUS, as a list of (:KIND :TEXT). The first stored sample
is quiet unless the printer is already down."
  (let ((kind (or (getf sample :kind) ""))
        (job (or (getf sample :job-state) "")))
    (if (null previous)
        (cond ((string= kind "disconnected")
               (list (event "printer_disconnected" "The printer is disconnected, not paused.")))
              ((string= kind "error")
               (list (event "error"
                            (format nil "Printer error: ~a."
                                    (let ((detail (getf sample :error-text)))
                                      (if (and detail (plusp (length detail))) detail job))))))
              (t '()))
        (let ((previous-job (or (getf previous :job-state) ""))
              (previous-kind (or (getf previous :kind) ""))
              (reported (or (getf previous :reported) '()))
              (found '()))
          (cond
            ((and (string= kind "disconnected") (not (string= previous-kind "disconnected")))
             (list (event "printer_disconnected" "The printer is disconnected, not paused.")))
            ((and (string= kind "error") (not (string= previous-kind "error")))
             (list (event "error"
                          (format nil "Printer error: ~a."
                                  (let ((detail (getf sample :error-text)))
                                    (if (and detail (plusp (length detail))) detail job))))))
            ((member kind '("disconnected" "error") :test #'string=) '())
            (t
             (when (and (string= job "Paused")
                        (member previous-job +pause-from+ :test #'string=)
                        (not (reported-p reported "paused")))
               (push (event "paused" (format nil "Print paused: ~a." (job-name sample))) found))
             (when (and (string= job "Cancelling")
                        (not (string= previous-job "Cancelling"))
                        (member previous-job +cancel-from+ :test #'string=)
                        (not (reported-p reported "cancelled")))
               (push (event "cancelled" (format nil "Print cancelled: ~a." (job-name sample))) found))
             (let ((completion (getf sample :completion)))
               (when (and (string= job "Operational")
                          (not (reported-p reported "cancelled"))
                          (not (member "cancelled" (mapcar (lambda (item) (getf item :kind)) found)
                                       :test #'string=))
                          (not (reported-p reported "complete"))
                          (or (string= previous-job "Cancelling")
                              (member previous-job +complete-from+ :test #'string=)))
                 (if (and (member previous-job +complete-from+ :test #'string=)
                          (numberp completion) (>= completion 100))
                     (push (event "complete" (format nil "Print finished: ~a." (job-name sample))) found)
                     (push (event "cancelled" (format nil "Print cancelled: ~a." (job-name sample))) found))))
             (when (and (member job +printing-states+ :test #'string=)
                        (not (reported-p reported "stalled"))
                        (stalled-p previous sample now stall-minutes))
               (push (event "stalled"
                            (format nil "No file progress for ~d minutes while printing ~a."
                                    stall-minutes (job-name sample)))
                     found))
             (nreverse found)))))))

(defun next-state (previous sample events now stall-minutes)
  "The record to keep of SAMPLE, and the events it has already reported."
  (let* ((job (or (getf sample :job-state) ""))
         (previous-job (if previous (or (getf previous :job-state) "") ""))
         (new-print (and previous
                          (member job +printing-states+ :test #'string=)
                          (not (member previous-job +printing-states+ :test #'string=))))
         (generation (cond ((null previous) 1)
                           (new-print (1+ (or (getf previous :generation) 1)))
                           (t (or (getf previous :generation) 1))))
         (reported (if (or (null previous) new-print)
                       '()
                       (copy-list (or (getf previous :reported) '()))))
         (filepos (getf sample :filepos))
         (stalled-same (and previous
                             (equal (getf previous :filepos) filepos)
                             (equal (getf previous :completion) (getf sample :completion))
                             (equal (getf previous :filename) (getf sample :filename))
                             (member previous-job +printing-states+ :test #'string=)
                             (member job +printing-states+ :test #'string=)
                             (numberp filepos))))
    (dolist (one events)
      (pushnew (getf one :kind) reported :test #'string=))
    (list :version +state-version+
          :job-state job
          :connection-state (or (getf sample :connection-state) "")
          :kind (or (getf sample :kind) "")
          :filename (getf sample :filename)
          :completion (getf sample :completion)
          :filepos filepos
          :print-time-left (getf sample :print-time-left)
          :generation generation
          :reported (sort reported #'string<)
          :stall-since (cond (stalled-same (getf previous :stall-since))
                             ((and (member job +printing-states+ :test #'string=) (numberp filepos)) now)
                             (t nil))
          :saved-at now)))

(defun compare (previous sample now stall-minutes)
  "PREVIOUS (NIL for none) against SAMPLE at NOW.
=> (values STATE EVENTS NOTE). A previous state is never dropped here."
  (let* ((events (events-for previous sample now stall-minutes))
         (state (next-state previous sample events now stall-minutes))
         (note (cond (events "New print event.")
                     ((null previous) "No earlier sample is stored, so this check only records the current state.")
                     (t "Nothing new to report."))))
    (values state events note)))

;;; --- the state kept on disk ---------------------------------------------------------

(defun state-to-json (state)
  "STATE as the JSON object the state file holds: no API key, no chat name."
  (let* ((failure (getf state :failure))
         (body (json-of "version" (or (getf state :version) +state-version+)
                        "job_state" (or (getf state :job-state) "")
                        "connection_state" (or (getf state :connection-state) "")
                        "kind" (or (getf state :kind) "")
                        "filename" (getf state :filename)
                        "completion" (or (getf state :completion) :absent)
                        "filepos" (or (getf state :filepos) :absent)
                        "print_time_left" (or (getf state :print-time-left) :absent)
                        "generation" (or (getf state :generation) 0)
                        "reported" (coerce (or (getf state :reported) '()) 'vector)
                        "stall_since" (or (getf state :stall-since) :absent)
                        "saved_at" (or (getf state :saved-at) 0))))
    (when failure
      (setf (gethash "failure" body)
            (json-of "code" (car failure) "at" (cdr failure))))
    body))

(defun state-from-json (object)
  (let ((reported (jget object "reported"))
        (failure (jget object "failure")))
    (list :version (jget object "version")
          :job-state (or (jstr (jget object "job_state")) "")
          :connection-state (or (jstr (jget object "connection_state")) "")
          :kind (or (jstr (jget object "kind")) "")
          :filename (jstr (jget object "filename"))
          :completion (jnum (jget object "completion"))
          :filepos (jnum (jget object "filepos"))
          :print-time-left (jnum (jget object "print_time_left"))
          :generation (or (jget object "generation") 1)
          :reported (if (vectorp reported) (coerce reported 'list) '())
          :stall-since (jnum (jget object "stall_since"))
          :saved-at (jnum (jget object "saved_at"))
          :failure (let ((code (jstr (jget failure "code"))))
                     (and code (cons code (or (jnum (jget failure "at")) 0)))))))
