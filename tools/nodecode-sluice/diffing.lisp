;;;; diffing.lisp --- what changed between the baseline and the stage.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin's units are kept: an edit to a text file is one unit per
;;;; contiguous block of changed lines, and a created, deleted, renamed,
;;;; non-text, symlink or mode change is a unit of its own. What the model
;;;; reads is numbered, and the numbers are what /sluice apply takes.

(in-package #:nodecode-sluice)

(defparameter *max-diff-lines* 4000
  "Past this many lines on either side one file is replaced whole, not hunked.")

(defparameter *max-body-lines* 30
  "How many lines of a block the review prints before it says how many are left.")

(defstruct (unit (:constructor make-unit (id path kind &key i1 old new added removed summary body old-path)))
  id        ; the number /sluice apply takes
  path      ; the workspace-relative path the unit lands on
  kind      ; :hunk :create :delete :rename :replace :mode
  i1        ; a hunk's first old line, 0 based
  old       ; the hunk's old lines
  new       ; the hunk's new lines
  (added 0)
  (removed 0)
  summary
  body      ; lines the review prints, prefixes included
  old-path) ; a rename's source

;;; --- reading text ---------------------------------------------------------

(defun file-bytes (path)
  (ignore-errors
    (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
      (when in
        (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
          (read-sequence bytes in)
          bytes)))))

(defun text-lines (path)
  "PATH's lines as a vector, or NIL when it is missing or not plain text."
  (let* ((bytes (file-bytes path))
         (text (and bytes (not (find 0 bytes))
                    (handler-case (sb-ext:octets-to-string bytes :external-format :utf-8)
                      (error () nil)))))
    (when text
      (let ((lines (uiop:split-string text :separator '(#\Newline))))
        (when (and lines (equal "" (car (last lines))) (> (length lines) 1))
          (setf lines (butlast lines)))
        (coerce lines 'vector)))))

(defun trailing-newline-p (path)
  (let ((bytes (file-bytes path)))
    (and bytes (plusp (length bytes))
         (= 10 (aref bytes (1- (length bytes)))))))

(defun file-line-count (path)
  (let ((lines (text-lines path)))
    (if lines (length lines) 0)))

;;; --- the line diff ---------------------------------------------------------

(defun diff-lines (old new)
  "OLD -> NEW as blocks of (START OLD-LINES NEW-LINES), START 0 based in OLD.
A longest-common-subsequence walk, capped: past the cap one block replaces all."
  (let ((n (length old)) (m (length new)))
    (if (or (> n *max-diff-lines*) (> m *max-diff-lines*)
            (> (* (1+ n) (1+ m)) 4000000))
        (if (and (zerop n) (zerop m))
            '()
            (list (list 0 (coerce old 'list) (coerce new 'list))))
        (let* ((width (1+ m))
               (table (make-array (* (1+ n) width) :element-type 'fixnum :initial-element 0))
               (blocks '())
               (block nil))
          (labels ((at (i j) (aref table (+ (* i width) j)))
                   (open-block (start)
                     (setf block (list start '() '()))
                     (push block blocks)
                     block))
            (loop for i from (1- n) downto 0 do
              (loop for j from (1- m) downto 0 do
                (setf (aref table (+ (* i width) j))
                      (if (equal (aref old i) (aref new j))
                          (1+ (at (1+ i) (1+ j)))
                          (max (at (1+ i) j) (at i (1+ j)))))))
            (let ((i 0) (j 0))
              (loop while (or (< i n) (< j m)) do
                (cond ((and (< i n) (< j m) (equal (aref old i) (aref new j)))
                       (setf block nil)
                       (incf i)
                       (incf j))
                      ((and (< j m) (or (>= i n) (>= (at i (1+ j)) (at (1+ i) j))))
                       (let ((b (or block (open-block i))))
                         (setf (third b) (nconc (third b) (list (aref new j)))))
                       (incf j))
                      (t
                       (let ((b (or block (open-block i))))
                         (setf (second b) (nconc (second b) (list (aref old i)))))
                       (incf i))))))
          (nreverse blocks)))))

;;; --- one file into units ----------------------------------------------------

(defun body-of (lines prefix)
  (loop for line in lines
        for index from 0
        while (< index *max-body-lines*)
        collect (format nil "~a~a" prefix line)))

(defun hunk-units (path base-file stage-file)
  "The units for one edited text file, unnumbered."
  (let ((old (text-lines base-file)) (new (text-lines stage-file)))
    (if (or (null old) (null new))
        (list (make-unit 0 path :replace
                         :summary "replaced, and not plain text"
                         :added (file-line-count stage-file)))
        (loop for block in (diff-lines old new)
              collect (destructuring-bind (start old-lines new-lines) block
                        (make-unit 0 path :hunk :i1 start
                                   :old old-lines :new new-lines
                                   :added (length new-lines) :removed (length old-lines)
                                   :summary (format nil "line ~d: -~d +~d" (1+ start)
                                                    (length old-lines) (length new-lines))
                                   :body (append (body-of old-lines "-")
                                                 (body-of new-lines "+"))))))))

(defun create-unit (path stage)
  (let ((file (path-join (stage-path stage) path)))
    (make-unit 0 path :create
               :added (file-line-count file)
               :summary (format nil "new file, ~d lines" (file-line-count file))
               :body (body-of (or (coerce (text-lines file) 'list) '()) "+"))))

(defun delete-unit (path stage)
  (let ((file (path-join (base-path stage) path)))
    (make-unit 0 path :delete
               :removed (file-line-count file)
               :summary (format nil "deleted, ~d lines" (file-line-count file))
               :body (body-of (or (coerce (text-lines file) 'list) '()) "-"))))

(defun rev-of (units)
  (digest-string
   (format nil "~s" (mapcar (lambda (unit)
                                (list (unit-id unit) (unit-path unit) (unit-kind unit)
                                      (unit-i1 unit) (unit-old unit) (unit-new unit)
                                      (unit-old-path unit)))
                              units))))

(defun changed-units (stage)
  "The stage against its baseline => (values UNITS REV), units numbered from 1.
REV is a digest of the units; the review and the apply must agree on it."
  (let* ((now (scan (stage-path stage)))
         (before (stage-manifest stage))
         (paths (sort (remove-duplicates
                        (append (loop for key being the hash-keys of before collect key)
                                (loop for key being the hash-keys of now collect key))
                        :test #'equal)
                      #'string<))
         (creates '()) (deletes '()) (changes '()))
    (dolist (path paths)
      (let ((was (gethash path before)) (is (gethash path now)))
        (cond ((and (null was) is) (push (list path is) creates))
              ((and was (null is)) (push (list path was) deletes))
              ((same-entry-p was is) nil)
              ((or (not (eq (entry-kind was) (entry-kind is)))
                   (eq (entry-kind is) :link)
                   (not (equal (entry-digest was) (entry-digest is))))
               (push (list path is was) changes))
              (t (push (list path is was) changes)))))
    (let ((renames '()))
      ;; a delete and a create with the same bytes are one rename
      (dolist (deleted deletes)
        (let ((twin (find-if (lambda (created)
                               (same-entry-p (second created) (second deleted)))
                             creates)))
          (when twin
            (setf creates (remove twin creates :test #'eq))
            (push (list (first deleted) (first twin)) renames))))
      (let ((units '()))
        (flet ((add (unit) (push unit units)))
          (dolist (pair creates)
            (add (create-unit (first pair) stage)))
          (dolist (pair deletes)
            (add (delete-unit (first pair) stage)))
          (dolist (rename renames)
            (add (make-unit 0 (second rename) :rename :old-path (first rename)
                            :summary (format nil "renamed from ~a" (first rename)))))
          (dolist (change changes)
            (destructuring-bind (path is was) change
              (cond
                ((null was) (add (create-unit path stage)))
                ((and (eq (entry-kind is) :file) (eq (entry-kind was) :file)
                      (equal (entry-digest is) (entry-digest was)))
                 (add (make-unit 0 path :mode
                                 :summary (format nil "mode ~o -> ~o" (entry-mode was) (entry-mode is)))))
                ((and (eq (entry-kind is) :file) (eq (entry-kind was) :file))
                 (dolist (unit (hunk-units path (path-join (base-path stage) path)
                                           (path-join (stage-path stage) path)))
                   (add unit)))
                (t (add (make-unit 0 path :replace
                                   :added (if (eq (entry-kind is) :file)
                                              (file-line-count (path-join (stage-path stage) path))
                                              0)
                                   :summary (format nil "~(~a~) -> ~(~a~)"
                                                    (entry-kind was) (entry-kind is))))))))
        (let ((sorted (stable-sort (nreverse units) #'string< :key #'unit-path))
              (id 0))
          (dolist (unit sorted)
            (setf (unit-id unit) (incf id)))
          (values sorted (rev-of sorted))))))))
