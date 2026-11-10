;;;; cell.lisp --- what the operator and the model call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One command, two tools, one observer. The original registered ten
;;;; observer hooks against Hermes; Nodecode observes a tool call in one place,
;;;; so the hook here is the :TOOL point and the graph is built from what a
;;;; call's arguments, its result and the working tree say it touched.

(in-package #:nodecode-file-lineage)

(defparameter +help+
  "/file-lineage                 artifacts made in this session
/file-lineage <file>          where a file came from
/file-lineage receipt <file>  write <file>.receipt.json beside it
/file-lineage verify <file>   check the file and its sources against recorded hashes
/file-lineage mermaid <file>  the provenance graph as Mermaid
/file-lineage stats           what the store holds"
  "What /file-lineage answers with no verb it knows.")

(defun unknown-text (ref)
  (format nil "file-lineage: nothing recorded for ~a. Only a file a tool call wrote is tracked.~%~a" ref +help+))

(defun command-cwd (session-id)
  (or *cwd*
      (and (stringp session-id) (plusp (length session-id))
           (ignore-errors (nlk:find-session-cwd session-id)))
      (ignore-errors (uiop:getcwd))))

(defun rest-of (text first-word)
  "TEXT past the FIRST-WORD it opens with."
  (string-trim '(#\Space #\Tab) (subseq text (min (length text) (length first-word)))))

(defun run-command (args session-id)
  "One /file-lineage invocation => the answer's text."
  (handler-case
      (let* ((text (string-trim '(#\Space #\Tab) (or args "")))
             (word (first (uiop:split-string text :separator '(#\Space #\Tab))))
             (verb (string-downcase (or word "")))
             (rest (string-trim '(#\Space #\Tab) (or (and word (rest-of text word)) "")))
             (cwd (command-cwd session-id)))
        (cond ((zerop (length text)) (list-text (and (stringp session-id) session-id)))
              ((member verb '("help" "-h" "--help") :test #'equal) +help+)
              ((string= verb "stats") (stats-text))
              ((member verb '("receipt" "verify" "mermaid") :test #'equal)
               (if (zerop (length rest))
                   (format nil "usage: /file-lineage ~a <file>" verb)
                   (let ((artifact (find-artifact rest :cwd cwd)))
                     (cond ((null artifact) (unknown-text rest))
                           ((string= verb "receipt")
                            (or (write-sidecar (getf artifact :path))
                                "file-lineage: could not write the receipt"))
                           ((string= verb "verify") (verify-text (getf artifact :path)))
                           (t (mermaid-text (getf artifact :path)))))))
              (t (let ((artifact (find-artifact text :cwd cwd)))
                   (if artifact (explain-text (getf artifact :path)) (unknown-text text))))))
    (error (condition) (format nil "file-lineage: ~a" condition))))

;;; --- the tools the model calls ----------------------------------------------------

(nlk:define-contract show-ask
  (:doc "Where a file the agent wrote came from: its SHA-256, the tool call that made it, and the files and URLs it was made from, each labelled observed, evidenced or inferred. Use it when the operator asks where a file or a figure came from, or before reusing a file you did not write.")
  ("ref" :string :required t :doc "the file: a path, a basename, or the label /file-lineage lists"))

(nlk:define-contract verify-ask
  (:doc "Check a file the agent wrote against the hashes recorded when it was made, and check each file it was made from: fresh, modified or missing. Use it before trusting a file someone else may have changed.")
  ("ref" :string :required t :doc "the file: a path, a basename, or the label /file-lineage lists"))

(defun tool-show (ask)
  (let ((ref (show-ask-ref ask)))
    (let ((artifact (find-artifact ref :cwd (command-cwd (getf (nle:turn) :session-id)))))
      (if artifact (explain-text (getf artifact :path)) (unknown-text ref)))))

(defun tool-verify (ask)
  (let ((ref (verify-ask-ref ask)))
    (let ((artifact (find-artifact ref :cwd (command-cwd (getf (nle:turn) :session-id)))))
      (if artifact (verify-text (getf artifact :path)) (unknown-text ref)))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell file-lineage
  (:section ("file-lineage")
    (:guide "nothing here is required. sidecar writes a receipt beside every artifact; the rest bound what one tool call may cost: the files the working-tree walk reads, the files one call may record, and how far back a source counts as inferred")
    ("sidecar" :boolean :default nil
               :doc "write <file>.receipt.json beside every artifact when its call ends (receipts are always available on demand with /file-lineage receipt)")
    ("track_shell" :boolean :default t
                   :doc "attribute the files a shell, eval or patch call changed in the session's folder")
    ("snapshot_max_files" :integer :default 2000 :min 1
                          :doc "files the working-tree walk reads after a shell call; a larger tree is skipped")
    ("max_artifacts_per_call" :integer :default 50 :min 1
                              :doc "files one call may record; the rest are counted, not recorded")
    ("session_window_calls" :integer :default 40 :min 0
                            :doc "earlier things this session saw, counted as inferred sources; 0 disables it"))
  (:help :file-lineage
         "a receipt for every file the agent writes: its hash, the call that made it, and the sources behind it, labelled observed, evidenced or inferred; /file-lineage"
         +primer+)
  (:tool "file_lineage_show" (nlk:contract 'show-ask) 'tool-show)
  (:tool "file_lineage_verify" (nlk:contract 'verify-ask) 'tool-verify)
  (:command "file-lineage" 'run-command
            :description "Where a file the agent wrote came from"
            :argument-hint "[<file> | receipt <file> | verify <file> | mermaid <file> | stats]")
  (:hook :tool #'observe-call)
  (:start (lambda ()
            (load-store)
            (clrhash *seen*)
            (nle:on-stop (lambda ()
                           (save-store)
                           (clrhash *artifacts*)
                           (clrhash *seen*))))))
