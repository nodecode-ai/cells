;;;; cell-test.lisp --- file-lineage, with no network and no operator.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A tool call reaches the observer as one op and one NEXT: the tests hand
;;;; it a NEXT that writes a file, so the graph is built exactly as a real
;;;; call builds it, on this thread, in the test home's own folder. The two
;;;; tools go through the real dispatch and contract; /file-lineage through the
;;;; registered command the way SLASH runs it.

(in-package #:nodecode.test)

(define-test-slice "file-lineage" "FILE-LINEAGE-CELL-"
  :start nodecode-file-lineage:start-cell)

(define-cell-lifecycle-tests "file-lineage"
  (:hooks :tool)
  (:help :file-lineage)
  (:command "file-lineage")
  (:running
   (is (nle::find-tool "file_lineage_show") "file_lineage_show is registered")
   (is (nle::find-tool "file_lineage_verify") "file_lineage_verify is registered")
   (is (zerop (hash-table-count nodecode-file-lineage::*artifacts*)) "the store starts empty"))
  (:stopped
   (is (zerop (hash-table-count nodecode-file-lineage::*artifacts*)) "stop forgets the graph in memory"))
  (:refused ("snapshot_max_files" 0)))

;;; --- fixtures ---------------------------------------------------------------------

(defun file-lineage-scratch (name)
  "A folder of this test run's own, empty."
  (let ((dir (nlk:home (format nil "test-~a/" name))))
    (ensure-directories-exist dir)
    dir))

(defun file-lineage-file (dir name text)
  (let ((path (merge-pathnames name dir)))
    (ensure-directories-exist path)
    (nlk:write-file-atomically path text)
    path))

(defun file-lineage-op (name arguments &key (call-id "call-1"))
  "The :TOOL op of NAME called with ARGUMENTS (an alist of string pairs)."
  (list :name name :call-id call-id
        :arguments (apply #'nlk:make-json-object arguments)))

(defun file-lineage-observe (op next &optional (cwd nil))
  "Run OP through the observer with NEXT, the folder CWD standing in for the session's."
  (let ((nodecode-file-lineage::*cwd* (and cwd (namestring cwd))))
    (nodecode-file-lineage::observe-call op next)))

(defun file-lineage-entry (&rest args)
  "Run /file-lineage ARGS as SLASH runs it."
  (cell-entry "nodecode-file-lineage" "file-lineage" (format nil "~{~a~^ ~}" args)))

;;; --- the observer -----------------------------------------------------------------

(deftest file-lineage-cell-a-write-file-call-leaves-a-receipt ()
  (with-cell-stop ((file-lineage-start))
    (let* ((dir (file-lineage-scratch "write"))
           (source (file-lineage-file dir "source.md" "the operator's notes"))
           (target (file-lineage-file dir "note.md" "draft")))
      (file-lineage-observe
       (file-lineage-op "write-file" (list "path" (namestring target)
                                       "text" (format nil "made from ~a and https://example.com/page" (namestring source))))
       (lambda (op) (declare (ignore op)) (nlk:write-file-atomically target "made from the notes") "wrote the file")
       dir)
      (let ((artifact (nodecode-file-lineage:find-artifact (namestring target))))
        (is artifact "the written file is an artifact")
        (is (search "sha256:" (getf artifact :sha256)) "with its hash")
        (is (equal "write-file" (getf artifact :tool)) "and the tool that wrote it")
        (let ((linked (find (namestring source) (getf artifact :sources)
                            :key (lambda (source) (getf source :locator)) :test #'equal)))
          (is linked "the file the same call named is one of its sources"))))))

(deftest file-lineage-cell-the-command-explains-a-file ()
  (with-cell-stop ((file-lineage-start))
    (let* ((dir (file-lineage-scratch "explain"))
           (brief (file-lineage-file dir "brief.md" "the brief"))
           (target (file-lineage-file dir "report.md" "half a report")))
      (file-lineage-observe (file-lineage-op "write-file" (list "path" (namestring target)
                                                         "text" (format nil "from ~a" (namestring brief))))
                        (lambda (op) (declare (ignore op)) "ok")
                        dir)
      (let ((answer (file-lineage-entry (namestring target))))
        (is (search "report.md" answer) "the answer names the file")
        (is (search "write-file" answer) "and the call that made it")
        (is (search "sha256:" answer) "and its hash"))
      (let ((listing (file-lineage-entry)))
        (is (search "report.md" listing) "the list names it too")
        (is (search "1 src" listing) "with its source count")))))

(deftest file-lineage-cell-a-shell-call-attributes-what-it-changed ()
  (with-cell-stop ((file-lineage-start))
    (let* ((dir (file-lineage-scratch "shell"))
           (out (merge-pathnames "out.txt" dir)))
      (file-lineage-observe
       (file-lineage-op "sh" (list "command" "echo hi > out.txt"))
       (lambda (op) (declare (ignore op)) (nlk:write-file-atomically out "hi") "wrote")
       dir)
      (let ((artifact (nodecode-file-lineage:find-artifact (namestring out))))
        (is artifact "the file the command wrote is an artifact")
        (is (equal "sh" (getf artifact :tool)) "attributed to the shell call")))))

(deftest file-lineage-cell-verify-sees-a-changed-file ()
  (with-cell-stop ((file-lineage-start))
    (let* ((dir (file-lineage-scratch "verify"))
           (target (file-lineage-file dir "table.md" "| a | 1 |")))
      (file-lineage-observe (file-lineage-op "write-file" (list "path" (namestring target) "text" "x"))
                        (lambda (op) (declare (ignore op)) "ok")
                        dir)
      (is (search "fresh" (file-lineage-entry "verify" (namestring target))) "untouched reads fresh")
      (nlk:write-file-atomically target "| a | 2 |")
      (let ((answer (file-lineage-entry "verify" (namestring target))))
        (is (search "modified" answer) "a changed file reads modified"))
      (let ((graph (file-lineage-entry "mermaid" (namestring target))))
        (is (search "graph LR" graph) "and the graph names it")))))

(deftest file-lineage-cell-the-sidecar-rides-beside-the-file ()
  (with-cell-stop ((file-lineage-start "sidecar" t))
    (let* ((dir (file-lineage-scratch "sidecar"))
           (target (file-lineage-file dir "brief.md" "# brief")))
      (file-lineage-observe (file-lineage-op "write-file" (list "path" (namestring target) "text" "x"))
                        (lambda (op) (declare (ignore op)) "ok")
                        dir)
      (let ((sidecar (concatenate 'string (namestring target) ".receipt.json")))
        (is (probe-file sidecar) "a receipt lies beside the file")
        (let ((receipt (ignore-errors (nlk:decode-json (uiop:read-file-string sidecar)))))
          (is (hash-table-p receipt) "and it is JSON")
          (is (stringp (nodecode-file-lineage::json-field receipt "receipt_sha256")) "with its own digest"))))))

;;; --- the tools --------------------------------------------------------------------

(deftest file-lineage-cell-the-tools-answer-through-the-dispatch ()
  (with-cell-stop ((file-lineage-start))
    (let* ((dir (file-lineage-scratch "tools"))
           (target (file-lineage-file dir "chart.md" "a chart")))
      (file-lineage-observe (file-lineage-op "write-file" (list "path" (namestring target) "text" "x"))
                        (lambda (op) (declare (ignore op)) "ok")
                        dir)
      (let ((show (nle::message-content
                   (execute-wire-call "call-show" "file_lineage_show"
                                      (nlk:encode-json-object (nlk:json-object "ref" (namestring target))))))
            (verify (nle::message-content
                     (execute-wire-call "call-verify" "file_lineage_verify"
                                        (nlk:encode-json-object (nlk:json-object "ref" (namestring target)))))))
        (is (search "chart.md" show) "file_lineage_show explains the file")
        (is (search "fresh" verify) "file_lineage_verify checks it")))))

(deftest file-lineage-cell-the-contract-refuses-a-missing-ref ()
  (let ((refusal (signals-error nlk:contract-refusal
                     (nlk:contract-decode (nlk:contract 'nodecode-file-lineage::show-ask)
                                           (nlk:make-json-object "ref" nil)))))
    (is refusal "ref is required at the boundary")))
