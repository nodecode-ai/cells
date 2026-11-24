;;;; cell-test.lisp --- the safe-swap cell against a scratch directory and a stubbed SH.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here touches the network or the operator's quarantine: the cell is
;;;; started on a temporary quarantine, and the hook's NEXT runs the command it
;;;; is handed through sh in a temporary workspace, so what the rewrite produced
;;;; is what really runs -- and what a file check can then find, or not find.

(in-package #:nodecode.test)

(define-test-slice "safe-swap" "SAFE-SWAP-CELL-"
  :start nodecode-safe-swap:start-cell)

(define-cell-lifecycle-tests "safe-swap"
  (:hooks (find-symbol "SH" '#:nodecode.evolved))
  (:help :safe-swap)
  (:command "safe-swap")
  (:refused ("quarantine_dir" "trash"))
  (:idle nodecode-safe-swap:safe-swap-error
         (safe-swap:status)
         (safe-swap:receipts)
         (safe-swap:plan "rm -rf build")))

;;; --- fixtures ----------------------------------------------------------------

(defparameter +swap-hook-key+ "nodecode-safe-swap")

(defun sswap-rules (quarantine &key (rid "abcdef"))
  "The rule plist the hook builds, on QUARANTINE, with a receipt id a test names."
  (list :rm t :single-rm t :push t :reset t :restore t :branch t :clean t :stash t
        :qdir quarantine :rid rid :trusted nil :note t :notify t))

(defun sswap-run (directory)
  "A NEXT for the hook: run the command it is handed through sh, in DIRECTORY."
  (lambda (command &rest rest)
    (declare (ignore rest))
    (multiple-value-bind (output errors status)
        (nlk:run-bounded (list "sh" "-c" command) :directory directory)
      (values (concatenate 'string (or output "") (or errors "")) status))))

(defun sswap-start (quarantine &rest pairs)
  "The cell started on a config naming QUARANTINE as the quarantine directory."
  (apply #'safe-swap-start
         (append (list "quarantine_dir"
                       (if (pathnamep quarantine) (namestring quarantine) quarantine))
                 pairs)))

(defun sswap-hook ()
  "The function this cell put on SH."
  (cell-hook +swap-hook-key+ (find-symbol "SH" '#:nodecode.evolved)))

(defmacro with-swap-trash ((quarantine) &body body)
  "BODY with QUARANTINE a fresh, existing quarantine directory."
  `(with-temp-trees ((,quarantine "swap-trash"))
     (ensure-directories-exist (uiop:ensure-directory-pathname ,quarantine))
     ,@body))

;;; --- what the rules decide -----------------------------------------------------

(deftest safe-swap-cell-rewrite-table ()
  (let* ((rules (sswap-rules "/tmp/quarantine"))
         (rm (nodecode-safe-swap:analyse "rm -rf build" rules))
         (harmless (nodecode-safe-swap:analyse "ls -la" rules))
         (push (nodecode-safe-swap:rewrite-command "git push --force origin main" rules))
         (reset (nodecode-safe-swap:rewrite-command "git reset --hard HEAD~1" rules))
         (root (nodecode-safe-swap:analyse "rm -rf /" rules))
         (subst (nodecode-safe-swap:analyse "rm -rf $(echo x)" rules)))
    (let ((text (or (nodecode-safe-swap:analysis-rewrite rm) "")))
      (is (search "SS_RCPT=abcdef" text) "rm -r is swapped under the receipt id"))
    (is (search "mv --" (or (nodecode-safe-swap:analysis-rewrite rm) "")) "by a move, not a delete")
    (is (null (nodecode-safe-swap:analysis-skips rm)) "a swapped command has nothing to explain")
    (is (search "--force-with-lease --force-if-includes" (or push "")) "a force push becomes a lease")
    (is (search "snapshot" (or reset "")) "a hard reset takes a snapshot first")
    (is (null (nodecode-safe-swap:analysis-rewrite harmless)) "a harmless command is left alone")
    (is (null (nodecode-safe-swap:analysis-skips harmless)) "and says nothing about it")
    (let ((why (or (first (nodecode-safe-swap:analysis-skips root)) "")))
      (is (search "protected path" why) "removing / is refused by the rule"))
    (let ((why (or (first (nodecode-safe-swap:analysis-skips subst)) "")))
      (is (search "unsupported syntax" why) "a command substitution is not read"))))

;;; --- the hook, end to end ---------------------------------------------------------

(deftest safe-swap-cell-hook-swaps-then-undo-restores ()
  (with-swap-trash (quarantine)
    (with-temp-workspace (project "build/one.txt")
      (with-cell-stop ((sswap-start quarantine))
        (let* ((answer (funcall (sswap-hook) (sswap-run project) "rm -r build"))
               (base (namestring (uiop:ensure-directory-pathname quarantine)))
               (id (first (nodecode-safe-swap::receipt-ids base)))
               (receipt (nodecode-safe-swap::receipt-plist base (or id ""))))
          (is (search "[safe-swap]" answer) "the model is told what ran")
          (is (search "Undo: /safe-swap undo" answer) "and how to put it back")
          (is (not (probe-file (merge-pathnames "build" project))) "the folder left the workspace")
          (is (equal "rm" (getf receipt :kind)) "a receipt of the rm kind is on disk")
          (let ((original (or (getf receipt :original) "")))
            (is (search "rm -r build" original) "naming the command it swapped"))
          (let ((out (safe-swap:undo (or id "last"))))
            (is (search "restored:" out) "the undo puts it back")
            (is (probe-file (merge-pathnames "build/one.txt" project)) "the file is back in the workspace")))))))

(deftest safe-swap-cell-reports-what-was-not-swapped ()
  (with-swap-trash (quarantine)
    (with-cell-stop ((sswap-start quarantine))
      (let ((answer (funcall (sswap-hook)
                             (lambda (&rest args)
                               (declare (ignore args))
                               (values "ran" 0))
                             "rm -rf /")))
        (is (search "NOT swapped" answer) "a destructive command left alone is reported")
        (is (search "protected path" answer) "with the rule that left it alone")))))

;;; --- the command ---------------------------------------------------------------

(deftest safe-swap-cell-command-answers ()
  (with-swap-trash (quarantine)
    (with-cell-stop ((sswap-start quarantine))
      (let ((list (cell-entry +swap-hook-key+ "safe-swap" "list"))
            (purge (cell-entry +swap-hook-key+ "safe-swap" "purge"))
            (status (cell-entry +swap-hook-key+ "safe-swap" "status"))
            (root (namestring (uiop:ensure-directory-pathname quarantine))))
        (is (search "No receipts" list) "an empty list says so")
        (is (search "No receipts to purge" purge) "purging nothing says so")
        (is (search root status) "status names the quarantine it runs on")
        (is (search "was run as" (safe-swap:plan "rm -rf build")) "a plan says what it would run as")
        (is (search "protected path" (safe-swap:plan "rm -rf /")) "and why one would be left alone"))))
)