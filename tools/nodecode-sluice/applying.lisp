;;;; applying.lisp --- land the units the operator approved.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every file is all or nothing, written atomically, and a file that moved
;;;; in the real workspace since the stage was taken is a conflict that is
;;;; reported and left alone. A path that would leave the workspace through a
;;;; symlink is refused, as the plugin refused it.

(in-package #:nodecode-sluice)

(define-condition sluice-conflict (error)
  ((detail :initarg :detail :reader conflict-detail))
  (:report (lambda (condition stream) (write-string (conflict-detail condition) stream))))

(defun conflict (format-control &rest arguments)
  (error 'sluice-conflict :detail (apply #'format nil format-control arguments)))

;;; --- where a unit lands -----------------------------------------------------

(defun target-path (stage path)
  "PATH's place in the real workspace, refusing an absolute path, a .., or a
directory reached through a symlink out of the workspace."
  (when (or (absolute-path-string-p path)
            (member ".." (uiop:split-string path :separator "/") :test #'equal))
    (conflict "~a: unsafe path" path))
  (let ((root (stage-root stage))
        (dir (directory-of path)))
    (loop while (and dir (not (uiop:directory-exists-p (path-join root dir))))
          do (setf dir (directory-of dir)))
    (when dir
      (let ((resolved (strip-slash (native (truename (path-join root dir))))))
        (unless (path-under-p resolved root)
          (conflict "~a: path leaves the workspace through a symlink" path))))
    (path-join root path)))

(defun expect-base (stage path real)
  "REAL must be what the baseline says PATH was."
  (let ((base (gethash path (stage-manifest stage)))
        (current (entry-for real)))
    (cond ((null current) (conflict "~a: the file no longer exists in the workspace" path))
          ((null base) nil)
          ((not (same-entry-p base current))
           (conflict "~a: the file changed in the workspace since the stage was taken" path)))))

(defun atomic-write (destination bytes mode)
  (ensure-directories-exist destination)
  (let* ((dir (or (directory-of destination) "/"))
         (temporary (format nil "~a/.sluice-~d" dir (random 1000000))))
    (with-open-file (out temporary :direction :output :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
      (write-sequence bytes out))
    (when mode (ignore-errors (sb-posix:chmod temporary mode)))
    (uiop:rename-file-overwriting-target temporary destination)
    destination))

(defun lines-bytes (lines newline)
  (sb-ext:string-to-octets
   (with-output-to-string (out)
     (format out "~{~a~^~%~}" lines)
     (when (and newline (plusp (length lines))) (terpri out)))
   :external-format :utf-8))

(defun copy-from-stage (stage path real)
  (let ((entry (entry-for (path-join (stage-path stage) path))))
    (cond
      ((null entry) (conflict "~a: nothing in the stage for it" path))
      ((eq (entry-kind entry) :link)
       (ensure-directories-exist real)
       (let ((temporary (format nil "~a.sluice-link" real)))
         (ignore-errors (delete-file temporary))
         (sb-posix:symlink (entry-target entry) temporary)
         (uiop:rename-file-overwriting-target temporary real)))
      (t (let ((bytes (file-bytes (path-join (stage-path stage) path))))
           (if bytes
               (atomic-write real bytes (entry-mode entry))
               (conflict "~a: the stage's copy could not be read" path)))))))

(defun prune-empty-parents (root path)
  (let ((dir (directory-of path)))
    (loop while (and dir (path-under-p (path-join root dir) root)
                     (not (equal (strip-slash (path-join root dir)) root)))
          do (unless (ignore-errors (sb-posix:rmdir (path-join root dir)))
               (return))
             (setf dir (directory-of dir)))))

;;; --- hunks ------------------------------------------------------------------

(defun apply-hunks (stage path units)
  (let* ((real (target-path stage path))
         (entry (entry-for real)))
    (unless entry (conflict "~a: the file no longer exists in the workspace" path))
    (when (eq (entry-kind entry) :link) (conflict "~a: the path is now a symlink" path))
    (let* ((text (text-lines real))
           (lines (if text (coerce text 'list) (conflict "~a is no longer plain text" path)))
           (newline (trailing-newline-p real))
           (cursor 0)
           (offset 0))
      (dolist (unit (sort (copy-list units) #'< :key #'unit-i1))
        (let* ((old (unit-old unit))
               (new (unit-new unit))
               (count (length old))
               (want (+ (unit-i1 unit) offset))
               (pos (when (and (>= want cursor) (<= (+ want count) (length lines))
                               (equal (subseq lines want (+ want count)) old))
                      want)))
          (unless pos
            (let ((best nil))
              (loop for i from cursor to (- (length lines) count)
                    when (equal (subseq lines i (+ i count)) old)
                      do (when (or (null best) (< (abs (- i want)) (abs (- best want))))
                           (setf best i)))
              (setf pos best)))
          (unless pos
            (conflict "~a: the change at line ~d no longer applies; the file moved underneath the review"
                      path (1+ (unit-i1 unit))))
          (setf lines (append (subseq lines 0 pos) new (subseq lines (+ pos count)))
                offset (+ offset (- (length new) count))
                cursor (+ pos (length new)))))
      (atomic-write real (lines-bytes lines newline) (entry-mode entry)))))

;;; --- one path ---------------------------------------------------------------

(defun apply-path (stage path units)
  "Land every selected unit of PATH, or conflict."
  (let* ((kinds (mapcar #'unit-kind units))
         (real (target-path stage path))
         (entry (entry-for (path-join (stage-path stage) path))))
    (cond
      ((member :rename kinds)
       (let* ((rename (find :rename units :key #'unit-kind))
              (old-path (unit-old-path rename))
              (old-real (target-path stage old-path)))
         (when (and (entry-for real) (not (same-entry-p (entry-for real) entry)))
           (conflict "~a: a different file already exists in the workspace" path))
         (unless (same-entry-p (entry-for real) entry)
           (copy-from-stage stage path real))
         (expect-base stage old-path old-real)
         (ignore-errors (delete-file old-real))
         (prune-empty-parents (stage-root stage) old-path)))
      ((member :delete kinds)
       (when (entry-for real)
         (expect-base stage path real)
         (ignore-errors (delete-file real))
         (prune-empty-parents (stage-root stage) path)))
      ((member :replace kinds)
       (unless (same-entry-p (entry-for real) entry)
         (when (entry-for real) (expect-base stage path real))
         (copy-from-stage stage path real)))
      (t
       (when (member :hunk kinds)
         (apply-hunks stage path (remove-if-not (lambda (unit) (eq :hunk (unit-kind unit))) units)))
       (when (and (member :mode kinds) entry (eq (entry-kind entry) :file)
                  (uiop:file-exists-p real) (not (symlink-p real)))
         (ignore-errors (sb-posix:chmod real (entry-mode entry))))))))

(defun rebase (stage touched removed)
  "Make the baseline agree with what the real workspace now holds."
  (dolist (path removed)
    (remhash path (stage-manifest stage))
    (ignore-errors (delete-file (path-join (base-path stage) path))))
  (dolist (path touched)
    (let ((entry (entry-for (path-join (stage-root stage) path)))
          (base (path-join (base-path stage) path)))
      (ignore-errors (delete-file base))
      (cond (entry
             (copy-file-entry (path-join (stage-root stage) path) base entry)
             (setf (gethash path (stage-manifest stage)) entry))
            (t (remhash path (stage-manifest stage))))))
  (setf (stage-last-rev stage) "")
  (save-stage stage))

(defun apply-units (stage units selected)
  "Land the units whose id is in SELECTED. => a plist of what landed."
  (let ((chosen (remove-if-not (lambda (unit) (member (unit-id unit) selected)) units))
        (applied 0) (files '()) (conflicts '()) (removed '()))
    (let ((by-path (make-hash-table :test #'equal)))
      (dolist (unit chosen) (push unit (gethash (unit-path unit) by-path)))
      (dolist (path (sort (loop for key being the hash-keys of by-path collect key) #'string<))
        (let ((group (nreverse (gethash path by-path))))
          (handler-case
              (progn
                (apply-path stage path group)
                (incf applied (length group))
                (push path files)
                (dolist (unit group)
                  (when (eq (unit-kind unit) :rename)
                    (push (unit-old-path unit) removed))))
            (sluice-conflict (condition)
              (push (cons path (conflict-detail condition)) conflicts))
            (error (condition)
              (push (cons path (princ-to-string condition)) conflicts))))))
    (rebase stage (nreverse files) (nreverse removed))
    (list :applied applied :files (nreverse files) :conflicts (nreverse conflicts))))
