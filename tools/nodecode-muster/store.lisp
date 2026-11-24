;;;; store.lisp --- one JSON document on this machine, and the flights it keeps.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original kept flights, requeues and a boot marker in SQLite behind a
;;;; writer thread. A cell has no database driver, so the same rows are one
;;;; JSON document under the Nodecode home, written whole and atomically after
;;;; each sweep: a store that will not read is a refusal naming the file, not
;;;; a store quietly replaced. Each row keeps only what the board draws - the
;;;; label, the counts, the last note - never a prompt or a tool's output.

(in-package #:nodecode-muster)

(nlk:define-peripheral muster :not-running t :lock "muster")

(defparameter +store-name+ "muster.json")

(defparameter +flight-keys+
  '((:id . "id") (:session . "session") (:parent . "parent") (:turn . "turn")
    (:label . "label") (:calls . "calls") (:note . "note") (:answer . "answer")
    (:state . "state") (:started . "started") (:last-seen . "last_seen")
    (:ended . "ended") (:requeues . "requeues") (:boot . "boot"))
  "The columns a row keeps, and the name each has in the document.")

(defvar *flights* (make-hash-table :test #'equal)
  "Every flight this home knows, by its id: the fork's own session id.")

(defvar *boot* ""
  "This start's mark. A row still live under another mark was cut off by the
image going down, and reads LOST.")

(defun setting-int (key default)
  (let ((value (ignore-errors (setting key))))
    (if (and (integerp value) (not (minusp value))) value default)))

(defun store-directory ()
  "The folder the ledger lives in: the section's dir, else muster/ under the home."
  (uiop:ensure-directory-pathname
   (let ((dir (ignore-errors (setting :dir))))
     (if (and (stringp dir) (plusp (length dir))) dir (nlk:home "muster/")))))

(defun store-file ()
  (merge-pathnames +store-name+ (store-directory)))

(defun plist-set (plist key value)
  "PLIST with KEY answered VALUE: the cell's row is its own, so it is edited
in place and answered."
  (let ((cell (member key plist :test #'eq)))
    (if cell
        (progn (setf (second cell) value) plist)
        (list* key value plist))))

(defun clip (text limit &key (ellipsis "..."))
  "TEXT cut to LIMIT characters, an ellipsis standing for what was cut."
  (cond ((not (stringp text)) nil)
        ((<= (length text) limit) text)
        ((<= limit 3) (subseq text 0 limit))
        (t (concatenate 'string (subseq text 0 (- limit (length ellipsis))) ellipsis))))

(defun flight-label (row)
  "A fork's task as its row shows it: one line, clipped."
  (let ((text (getf row :label)))
    (when (stringp text)
      (clip (string-trim '(#\Space #\Tab #\Newline) (substitute #\Space #\Newline text)) 80))))

(defun absorb (row now)
  "ROW, one fork as its call reported it, folded into the ledger at NOW."
  (let* ((id (getf row :id))
         (flight (and (stringp id) (plusp (length id)) (gethash id *flights*)))
         (state (status-state (getf row :status))))
    (cond
      (flight
       (setf flight (plist-set flight :last-seen now)
             flight (if (getf row :calls) (plist-set flight :calls (getf row :calls)) flight)
             flight (if (getf row :answer) (plist-set flight :answer (clip (getf row :answer) 200)) flight)
             flight (if (getf row :note) (plist-set flight :note (clip (getf row :note) 160)) flight)
             flight (if (getf row :turn) (plist-set flight :turn (getf row :turn)) flight))
       (when state
         (setf flight (plist-set flight :state state))
         (unless (live-state-p state)
           (setf flight (plist-set flight :ended (or (getf flight :ended) now)))))
       (setf (gethash id *flights*) flight))
      (t
       (let* ((ms (getf row :ms))
              (started (if (and (numberp ms) (not (minusp ms)))
                           (- now (round ms 1000))
                           now))
              (state (or state "RUNNING")))
         (setf (gethash id *flights*)
               (list :id id :session id
                     :parent (getf row :parent) :turn (getf row :turn)
                     :label (flight-label row) :calls (or (getf row :calls) 0)
                     :note (and (stringp (getf row :note)) (clip (getf row :note) 160))
                     :answer (and (stringp (getf row :answer)) (clip (getf row :answer) 200))
                     :state state :started started :last-seen now
                     :ended (unless (live-state-p state) now)
                     :requeues 0 :boot *boot*)))))))

(defun flights ()
  "Every row, newest first: live ones above the rest, as the original sorted."
  (let ((rows (loop for flight being the hash-values of *flights* collect flight)))
    (sort rows (lambda (a b)
                 (let ((alive-a (live-state-p (getf a :state)))
                       (alive-b (live-state-p (getf b :state))))
                   (cond ((and alive-a (not alive-b)) t)
                         ((and alive-b (not alive-a)) nil)
                         (t (> (or (getf a :started) 0) (or (getf b :started) 0)))))))))

(defun find-flight (prefix)
  "The one flight whose id starts with PREFIX, or NIL. An id that names more
than one row answers the prefix itself, so the report says so."
  (let ((prefix (string-downcase (string-trim " " (or prefix ""))))
        (found '()))
    (when (plusp (length prefix))
      (maphash (lambda (id flight)
                 (when (and (>= (length id) (length prefix))
                            (string= prefix id :end2 (length prefix)))
                   (push flight found)))
               *flights*))
    (cond ((= 1 (length found)) (first found))
          ((> (length found) 1) :many)
          (t nil))))

;;; --- the document ------------------------------------------------------------

(defun flight-json (flight)
  (let ((object (nlk:json-object)))
    (dolist (pair +flight-keys+)
      (let ((value (getf flight (car pair))))
        (when value
          (setf (gethash (cdr pair) object)
                (if (or (stringp value) (numberp value)) value (princ-to-string value))))))
    ;; a boolean-ish marker the reader does not depend on
    (setf (gethash "live" object) (if (live-state-p (getf flight :state)) t :false))
    object))

(defun save-store (&aux (rows (flights)))
  (let* ((object (nlk:json-object "version" 1 "boot" *boot*
                                   "flights" (coerce (mapcar #'flight-json rows) 'vector)))
         (path (store-file)))
    (nlk:write-file-atomically path (format nil "~a~%" (nlk:encode-json-object object))
                               :mode #o600 :directory-mode #o700)
    path))

(defun read-field (object key) (gethash key object))

(defun load-store ()
  "The rows on disk, into *FLIGHTS*. A file that will not read is a refusal
naming it; a row still live under another boot is LOST, found at a start."
  (let ((path (store-file)))
    (clrhash *flights*)
    (when (probe-file path)
      (let* ((text (ignore-errors (nlk:read-text path)))
             (object (and text (plusp (length (nlk:trimmed text)))
                          (ignore-errors (nlk:decode-json text)))))
        (unless (hash-table-p object)
          (fail "muster: ~a will not read as a store; move it aside and start again"
                (namestring path)))
        (let ((rows (read-field object "flights")))
          (when (vectorp rows)
            (loop for object across rows
                  when (hash-table-p object)
                    do (let ((flight
                               (loop for pair in +flight-keys+
                                     for value = (read-field object (cdr pair))
                                     when (and value (or (stringp value) (numberp value)))
                                       append (list (car pair) value))))
                         (when (getf flight :id)
                           (setf (gethash (getf flight :id) *flights*) flight))))))))
    *flights*))

(defun reconcile-lost (&aux (now (get-universal-time)))
  "A row live under another boot was cut off by the image going down: LOST."
  (let ((count 0))
    (maphash (lambda (id flight)
               (declare (ignore id))
               (when (and (live-state-p (getf flight :state))
                          (not (equal *boot* (getf flight :boot))))
                 (setf (getf flight :state) "LOST"
                       (getf flight :ended) now)
                 (incf count)))
             *flights*)
    count))
