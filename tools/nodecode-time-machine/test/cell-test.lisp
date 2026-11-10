;;;; cell-test.lisp --- the store, the rewind, the fork, and the effect ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test starts the cell against a scratch folder of its own, so the
;;;; store the run leaves behind is only ever its own home's. Nothing here
;;;; dials anything: the cell has no network at all.

(in-package #:nodecode.test)

(define-test-slice "time-machine" "TIME-MACHINE-CELL-" :start nodecode-time-machine:start-cell)

;;; --- a scratch folder ---------------------------------------------------------

(defun tm-dir (name)
  "A fresh scratch folder under the run's home."
  (let ((root (string-right-trim "/" (namestring (nlk:home (format nil "tm-scratch/~a" name))))))
    (when (probe-file root)
      (ignore-errors (uiop:delete-directory-tree (pathname (concatenate 'string root "/"))
                                                 :validate t)))
    (ensure-directories-exist (concatenate 'string root "/"))
    root))

(defun tm-file (root name text)
  "NAME inside ROOT holding TEXT; answers its namestring."
  (let ((path (concatenate 'string root "/" name)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (write-string text out))
    path))

(defun tm-text (path)
  (and (probe-file path) (uiop:read-file-string path)))

(defun tm-root ()
  "The folder the running cell protects."
  (multiple-value-bind (root refusal) (nodecode-time-machine::workspace-root)
    (declare (ignore refusal))
    root))

(defun tm-checkpoint (&optional label)
  "One restore point of the scratch folder through the tool the model calls."
  (nodecode-time-machine::checkpoint-tool
   (nodecode-time-machine::make-checkpoint-request :label (or label ""))))

(defun tm-id (text)
  "The checkpoint id the checkpoint tool's answer opens with."
  (subseq text 11 (or (search " " text :start2 11) 11)))

(defun tm-sh-op (command)
  "A :TOOL point OP for one shell call."
  (let ((args (make-hash-table :test #'equal)))
    (setf (gethash "command" args) command)
    (list :name "sh" :arguments args :call-id "call-1")))

(defun tm-through-hook (op &optional (answer "ran"))
  "One tool call through the cell's :TOOL hook, with its body answering ANSWER."
  (funcall (cell-hook "nodecode-time-machine" :tool)
           op
           (lambda (ignored) (declare (ignore ignored)) answer)))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "time-machine"
  (:config "workspace" (tm-dir "life"))
  (:hooks :tool)
  (:command "time-machine")
  (:help :time-machine)
  (:refused ("max_file_mb" 0)))

;;; --- a checkpoint, and the list of them ---------------------------------------

(deftest time-machine-cell-keeps-what-a-checkpoint-saw (with-cell-stop ((time-machine-start "workspace" (tm-dir "keep")))
  (let* ((root (tm-root))
         (a (tm-file root "one.txt" "first"))
         (answer (tm-checkpoint "before the change"))
         (id (tm-id answer)))
    (is (search "checkpoint ckpt_" answer) "the answer names the checkpoint it took")
    (is (search "1 file" answer) "and how much it holds")
    (is-carrying (listing (nodecode-time-machine::list-tool (nodecode-time-machine::make-list-request :limit 20)))
      ("before the change" "the list shows it by its label"))
    (tm-file root "one.txt" "second")
    (is-carrying (preview (nodecode-time-machine::rewind-tool (nodecode-time-machine::make-rewind-request :checkpoint-id id)))
      ("rewind preview" "a rewind previews unless it is confirmed")
      ("revert one.txt" "the changed file is named"))
    (is (equal "second" (tm-text a)) "the preview wrote nothing")
    (is (search "rewound ckpt_" (nodecode-time-machine::rewind-tool (nodecode-time-machine::make-rewind-request :checkpoint-id id :confirm t))) "the confirmed rewind reports what it did")
    (is (equal "first" (tm-text a)) "the file is back as the checkpoint held it"))))

(deftest time-machine-cell-removes-what-the-checkpoint-never-saw (with-cell-stop ((time-machine-start "workspace" (tm-dir "added")))
  (let* ((root (tm-root))
         (id (tm-id (tm-checkpoint "before the extra file")))
         (extra (tm-file root "extra.txt" "new")))
    (is (probe-file extra) "the extra file is there")
    (nodecode-time-machine::rewind-tool (nodecode-time-machine::make-rewind-request :checkpoint-id id :confirm t))
    (is (not (probe-file extra)) "a rewind removes a file the checkpoint never saw"))))

(deftest time-machine-cell-rewinds-only-the-paths-it-is-given (with-cell-stop ((time-machine-start "workspace" (tm-dir "paths")))
  (let* ((root (tm-root))
         (a (tm-file root "kept.txt" "one"))
         (b (tm-file root "back.txt" "one"))
         (id (tm-id (tm-checkpoint "two files"))))
    (tm-file root "kept.txt" "two")
    (tm-file root "back.txt" "two")
    (nodecode-time-machine::rewind-tool
     (nodecode-time-machine::make-rewind-request :checkpoint-id id :confirm t :paths (vector "back.txt")))
    (is (equal "two" (tm-text a)) "a path outside the list is left as it is")
    (is (equal "one" (tm-text b)) "the path inside the list is put back"))))

(deftest time-machine-cell-keeps-what-a-rewind-replaced (with-cell-stop ((time-machine-start "workspace" (tm-dir "trash")))
  (let* ((root (tm-root))
         (a (tm-file root "file.txt" "before"))
         (id (tm-id (tm-checkpoint "one file"))))
    (tm-file root "file.txt" "after")
    (let ((report (nodecode-time-machine::apply-rewind (nodecode-time-machine::find-manifest id))))
      (is (plusp (getf report :trashed)) "what the rewind replaced is copied to the trash")
      (is (probe-file (nodecode-time-machine::home-string (format nil "time-machine/trash/~a/file.txt" (getf report :trash)))) "and the trash names the run")
      (is (equal "before" (tm-text a)) "the file itself is back")))))

;;; --- the fork ------------------------------------------------------------------

(deftest time-machine-cell-forks-a-checkpoint-into-a-sibling (with-cell-stop ((time-machine-start "workspace" (tm-dir "fork")))
  (let ((root (tm-root)))
    (tm-file root "held.txt" "as of the checkpoint")
    (let* ((id (tm-id (tm-checkpoint "before the fork")))
           (answer (nodecode-time-machine::fork-tool
                    (nodecode-time-machine::make-fork-request :checkpoint-id id :name "branch")))
           (start (search "into " answer))
           (forked (subseq answer (+ start 5) (search ":" answer :start2 start))))
      (is (search "forked checkpoint ckpt_" answer) "the fork names the checkpoint it came from")
      (is (probe-file (concatenate 'string forked "/")) "the sibling folder is there")
      (is (equal "as of the checkpoint" (tm-text (concatenate 'string forked "/held.txt"))) "and holds the checkpoint's bytes")))))

;;; --- the effect ledger, and the hook that writes it -----------------------------

(deftest time-machine-cell-records-a-push-and-its-compensation (with-cell-stop ((time-machine-start "workspace" (tm-dir "effects")))
  (tm-checkpoint "before the push")
  (is-carrying (answer (tm-through-hook (tm-sh-op "git push origin main")))
    ("point of no return" "the result says what the call crossed")
    ("git push" "and which command crossed it")
    ("force-with-lease" "with the command that compensates it"))
  (let ((entry (first (nodecode-time-machine::read-effects))))
    (is (equal "git push" (getf entry :kind)) "the ledger holds the effect")
    (is (search "force-with-lease" (getf entry :compensation)) "and its compensation"))))

(deftest time-machine-cell-leaves-a-call-that-runs-nothing-risky-alone (with-cell-stop ((time-machine-start "workspace" (tm-dir "quiet")))
  (let ((before (length (nodecode-time-machine::read-effects))))
    (is (equal "ran" (tm-through-hook (tm-sh-op "ls -la") "ran")) "a call with no effect answers what its body answered")
    (is (= before (length (nodecode-time-machine::read-effects))) "and writes nothing into the ledger"))))

(deftest time-machine-cell-takes-a-restore-point-before-a-change (with-cell-stop ((time-machine-start "workspace" (tm-dir "auto")))
  (tm-file (tm-root) "auto.txt" "before")
  (let* ((before (length (nodecode-time-machine::manifests)))
         (answer (tm-through-hook (tm-sh-op "rm -f auto.txt") "removed")))
    (is (= (1+ before) (length (nodecode-time-machine::manifests))) "a call that changes files earns one restore point")
    (is (search "restore point ckpt_" answer) "the result tells the model where the restore point is")
    (is (search "rewind ckpt_" answer) "and how to go back to it"))))
