;;;; store.lisp --- the job board: one JSON file, written atomically, capped.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original keeps every job -- a local brief and a handoff alike -- in one
;;;; jobs.json under the plugin's data folder, rewritten whole under a lock and
;;;; capped at the newest 500 (store.py:9-71). The same file, the same cap and
;;;; the same atomic replace here, one folder further along: the cell's own
;;;; state_dir when the section names one, else staff/ under the Nodecode home.

(in-package #:nodecode-staff)

(defconstant +max-jobs+ 500 "How many jobs the board keeps: the newest, as the original caps it.")
(defconstant +max-task+ 20000 "A task's text, in characters, as the original bounds it.")
(defconstant +max-result+ 200000 "A pasted result's text, in characters.")

;;; --- a path a person typed ------------------------------------------------

(defun path-from-string (text)
  "TEXT as a pathname: `~' and `~/' from this account's home, anything else
as written."
  (let ((text (trim text)))
    (if (and (> (length text) 1)
             (char= #\~ (char text 0))
             (member (char text 1) '(#\/ #\\)))
        (merge-pathnames (subseq text 2) (user-homedir-pathname))
        (uiop:parse-native-namestring text))))

(defun state-dir ()
  "The folder the board lives in: the section's state_dir, or staff/ under the
Nodecode home."
  (let ((said (ignore-errors (getf (running-settings) :state-dir))))
    (if (and said (plusp (length (trim said))))
        (uiop:ensure-directory-pathname (path-from-string said))
        (uiop:ensure-directory-pathname (nlk:home "staff/")))))

(defun jobs-file ()
  "The board's one file."
  (merge-pathnames "jobs.json" (state-dir)))

;;; --- a time and an id -----------------------------------------------------

(defun now-iso ()
  "The current UTC time, ISO 8601 to the second, as the original stamps one."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun new-id (prefix)
  "A fresh job id: PREFIX, a dash, and twelve hex digits."
  (format nil "~a-~12,'0x" prefix (random (expt 16 12))))

;;; --- reading and writing the file ------------------------------------------

(defun job-of (object)
  "OBJECT, a job as the file holds it, as a plist."
  (list :id (or (field object "id") "")
        :kind (or (field object "kind") "")
        :status (or (field object "status") "")
        :specialist (or (field object "specialist") "")
        :title (or (field object "title") "")
        :task (or (field object "task") "")
        :created-at (or (field object "created_at") "")
        :updated-at (or (field object "updated_at") "")
        :output (or (field object "output") "")
        :result (or (field object "result") "")
        :error (or (field object "error") "")
        :delivery (or (field object "delivery") "")))

(defun job-json (job)
  "JOB, a plist, as the object the file holds."
  (nlk:json-object "id" (getf job :id)
                    "kind" (getf job :kind)
                    "status" (getf job :status)
                    "specialist" (getf job :specialist)
                    "title" (getf job :title)
                    "task" (getf job :task)
                    "created_at" (getf job :created-at)
                    "updated_at" (getf job :updated-at)
                    "output" (getf job :output)
                    "result" (getf job :result)
                    "error" (getf job :error)
                    "delivery" (getf job :delivery)))

(defun read-jobs ()
  "Every job on the board, oldest first. A file that does not decode is an
error a person reads, not silently an empty board."
  (let ((file (jobs-file)))
    (if (probe-file file)
        (let ((object (nlk:decode-json (uiop:read-file-string file))))
          (loop for item across (nlk:json-array object "jobs") collect (job-of item)))
        '())))

(defun save-jobs (jobs)
  "JOBS to the board's file, atomically and privately, the newest +MAX-JOBS+ kept."
  (let* ((file (jobs-file))
         (kept (last jobs (min (length jobs) +max-jobs+))))
    (ensure-directories-exist file)
    (nlk:write-file-atomically
     file
     (format nil "~a~%" (nlk:encode-json-object (nlk:json-object "jobs" (coerce (mapcar #'job-json kept) 'vector))))
     :mode :private)
    kept))

(defmacro with-jobs ((var) &body body)
  "BODY with VAR bound to the board, saved when it answers: one read-modify-write
under the cell's lock, and one more lock on the folder across the write."
  `(with-staff-lock
     (let ((,var (read-jobs)))
       (multiple-value-prog1 (progn ,@body)
         (setf ,var (save-jobs ,var))))))

;;; --- the board's four verbs -----------------------------------------------

(defun new-job (&key (kind "local") (status "running") specialist title task)
  "A job, not yet on the board."
  (let ((now (now-iso)))
    (list :id (new-id (if (string= kind "handoff") "handoff" "staff"))
          :kind kind :status status
          :specialist (or specialist "") :title (or title "") :task (or task "")
          :created-at now :updated-at now
          :output "" :result "" :error "" :delivery "")))

(defun add-job (job)
  "JOB onto the board."
  (with-jobs (jobs)
    (setf jobs (append jobs (list job)))
    job))

(defun plist-set (plist key value)
  "PLIST with KEY bound to VALUE."
  (let ((cell (member key plist :test #'eq)))
    (if cell
        (progn (setf (second cell) value) plist)
        (append plist (list key value)))))

(defun find-job (id)
  "The job ID names, or NIL."
  (find id (read-jobs) :key (lambda (job) (getf job :id)) :test #'string=))

(defun update-job (id &rest fields)
  "JOB ID with FIELDS set and its updated_at stamped; the job, or NIL when the
board holds none of that id."
  (with-jobs (jobs)
    (let ((job (find id jobs :key (lambda (job) (getf job :id)) :test #'string=)))
      (when job
        (let ((new (reduce (lambda (job pair) (plist-set job (first pair) (second pair)))
                           (loop for (key value) on fields by #'cddr collect (list key value))
                           :initial-value job)))
          (setf new (plist-set new :updated-at (now-iso)))
          (setf jobs (substitute new job jobs :test #'eq))
          new)))))

(defun list-jobs (&key kind status (limit 20))
  "The board's newest jobs, newest first, KIND and STATUS narrowing it."
  (let ((matching (remove-if-not (lambda (job)
                                   (and (or (null kind) (string= kind (getf job :kind)))
                                        (or (null status) (string= status (getf job :status)))))
                                 (read-jobs))))
    (subseq (reverse matching) 0 (min (length matching) (max 1 (min (or limit 20) 200))))))
