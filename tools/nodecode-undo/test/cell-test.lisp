;;;; cell-test.lisp --- the store, the three hooks, and the guard against a later edit.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A hook is funcalled by hand, the way the guard slice does it, so the
;;;; engine's receipt machinery is not what these tests prove: what they prove
;;;; is the cell's own reading of a path, what it keeps, and what it puts back.

(in-package #:nodecode.test)

(define-test-slice "undo" "UNDO-CELL-" :start nodecode-undo:start-cell)

(defun undo-cell-path (name text)
  "A scratch file under the home holding TEXT, and its namestring."
  (let ((path (namestring (nlk:home (format nil "undo-cell-scratch/~a" name)))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string text out))
    path))

(defun undo-cell-write (path text)
  "WRITE-FILE through the installed hook, with NEXT a plain file write."
  (funcall (cell-hook "nodecode-undo" 'nodecode.evolved::write-file)
           (lambda (p new-text)
             (ensure-directories-exist p)
             (with-open-file (out p :direction :output :if-exists :supersede
                                    :if-does-not-exist :create)
               (write-string new-text out))
             (format nil "wrote ~a" p))
           path text))

(defun undo-cell-entry-for (path)
  "The newest entry whose paths include PATH, or NIL."
  (find-if (lambda (entry)
             (some (lambda (item) (equal path (getf item :path))) (getf entry :items)))
           (nodecode-undo::entries)))

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "undo"
  (:hooks 'nodecode.evolved::write-file)
  (:command "undo")
  (:refused ("max_file_mb" 0)))

;;; --- what a write leaves ---------------------------------------------------

(deftest undo-cell-puts-back-an-overwritten-file (with-cell-stop ((undo-start))
  (let* ((path (undo-cell-path "over.txt" "original"))
         (listed (progn (undo-cell-write path "changed")
                        (nodecode-undo::run-slash "" "")))
         (entry (undo-cell-entry-for path)))
    (is (equal "changed" (uiop:read-file-string path)) "the write itself went through")
    (is-present (found (search path listed)) "the call is listed with the path it touched" found)
    (is entry "and reads back as one entry")
    (is (equal :file (getf (getf (first (getf entry :items)) :pre) :kind)) "the file is held as it was before")
    (nodecode-undo::run-slash (format nil "restore ~a" (getf entry :id)) "")
    (is (equal "original" (uiop:read-file-string path)) "the old text is back"))))

(deftest undo-cell-leaves-an-unchanged-write-off-the-list (with-cell-stop ((undo-start))
  (let* ((path (undo-cell-path "same.txt" "unchanged"))
         (before (length (nodecode-undo::entries))))
    (undo-cell-write path "unchanged")
    (is (= before (length (nodecode-undo::entries))) "a write that changed nothing keeps no entry")
    (is (equal "unchanged" (uiop:read-file-string path)) "and the write still ran"))))

;;; --- the guard against a later edit -----------------------------------------

(deftest undo-cell-refuses-a-file-changed-since (with-cell-stop ((undo-start))
  (let* ((path (undo-cell-path "conflict.txt" "v1")))
    (undo-cell-write path "v2")
    (let ((entry (undo-cell-entry-for path)))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (write-string "v3" out))
      (is-carrying (answer (nodecode-undo::run-slash (format nil "restore ~a" (getf entry :id)) ""))
        ("changed since" "the conflict is named"))
      (is (equal "v3" (uiop:read-file-string path)) "the newer text survives an ordinary restore")
      (nodecode-undo::run-slash (format nil "restore ~a --force" (getf entry :id)) "")
      (is (equal "v1" (uiop:read-file-string path)) "--force puts the snapshot back anyway")
      (is (nodecode-undo::entries) "and the restore left an entry of its own, so it can be undone")))))

;;; --- what a shell command removes -------------------------------------------

(deftest undo-cell-reads-the-targets-a-simple-rm-names (with-cell-stop ((undo-start))
  (is (equal '("/tmp/undo-cell-a" "/tmp/undo-cell-b") (nodecode-undo::shell-targets "rm -rf /tmp/undo-cell-a /tmp/undo-cell-b")) "an rm's arguments are its targets")
  (is (null (nodecode-undo::shell-targets "rm -rf /tmp/x | tee /tmp/y")) "a pipeline is not covered")
  (is (null (nodecode-undo::shell-targets "ls -la /tmp")) "a command that removes nothing has no targets")
  (let* ((path (undo-cell-path "gone.txt" "keep me"))
         (hook (cell-hook "nodecode-undo" 'nodecode.evolved::sh)))
    (funcall hook (lambda (command &rest args)
                    (declare (ignore args))
                    (delete-file path)
                    (format nil "ran ~a" command))
             (format nil "rm ~a" path))
    (is (not (probe-file path)) "the rm ran")
    (let ((entry (undo-cell-entry-for path)))
      (is entry "the deletion was snapshotted")
      (nodecode-undo::run-slash (format nil "restore ~a" (getf entry :id)) "")
      (is (equal "keep me" (uiop:read-file-string path)) "and the file comes back")))))
