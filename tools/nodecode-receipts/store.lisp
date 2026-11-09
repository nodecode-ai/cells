;;;; store.lisp --- where an attempt lives: one file under the receipts folder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An attempt is one JSON file, attempts/att_<hex>.json, holding the spec
;;;; as it was normalized, the criteria digest, when it started and -- once
;;;; it is sealed -- the receipt, as the exact document that was sealed. A
;;;; sealed attempt is never written again, so a later pass cannot overwrite
;;;; an earlier failure; a new run is a new attempt under the same task id.
;;;; The folder is the cell's own: nothing here writes anywhere else.

(in-package #:nodecode-receipts)

(defun receipts-directory ()
  "The folder this cell keeps its receipts in: the section's `directory', or
receipts/ under the nodecode home."
  (let ((configured (nlk:trimmed (or (setting :directory) ""))))
    (if (plusp (length configured))
        (uiop:ensure-directory-pathname (nlk:expand-home configured))
        (nlk:home "receipts/"))))

(defun attempts-directory ()
  "The folder the attempt files sit in."
  (merge-pathnames "attempts/" (receipts-directory)))

(defun exports-directory ()
  "The folder an export is written into."
  (merge-pathnames "exports/" (receipts-directory)))

(defun new-attempt-id ()
  "A fresh attempt id, att_ and 24 hex characters."
  (let* ((entropy (format nil "~a-~a-~a" (nlk:iso-now) (get-universal-time)
                          (random most-positive-fixnum)))
         (digest (nlk:sha256-text entropy)))
    (format nil "att_~a" (subseq digest 7 31))))

(defun attempt-path (attempt-id)
  "The file ATTEMPT-ID is kept in; the id is validated, so it is never a path."
  (merge-pathnames (format nil "~a.json" attempt-id) (attempts-directory)))

(defun attempt-json (attempt)
  "ATTEMPT, a plist, as the JSON document its file holds."
  (jobj "schema_version" +schema-version+
        "attempt_id" (getf attempt :id)
        "task_id" (getf attempt :task-id)
        "criteria_digest" (getf attempt :digest)
        "started_at" (getf attempt :started-at)
        "sealed_at" (or (getf attempt :sealed-at) +null+)
        "state" (if (getf attempt :sealed-at) "sealed" "open")
        "spec" (spec-json (getf attempt :spec))
        "receipt" (or (getf attempt :receipt) +null+)))

(defun save-attempt (attempt)
  "Write ATTEMPT to its file, atomically, private to the operator. => ATTEMPT."
  (unless (check-id-p (getf attempt :id)) (fail "attempt id is invalid"))
  (nlk:write-file-atomically (attempt-path (getf attempt :id)) (json-text (attempt-json attempt))
                             :parents t :mode :private :directory-mode #o700)
  attempt)

(defun read-attempt (attempt-id)
  "The attempt ATTEMPT-ID as a plist, or NIL when there is no such file."
  (unless (check-id-p attempt-id) (fail "attempt_id is invalid"))
  (let ((path (attempt-path attempt-id)))
    (unless (probe-file path) (return-from read-attempt nil))
    (let ((document (handler-case (nlk:decode-json (uiop:read-file-string path))
                      (error () nil))))
      (unless (hash-table-p document)
        (fail "attempt file ~a is unreadable" attempt-id))
      (let ((spec (decode-spec (jget document "spec"))))
        (list :id (jstring document "attempt_id")
              :task-id (jstring document "task_id")
              :digest (jstring document "criteria_digest")
              :started-at (jstring document "started_at")
              :sealed-at (jstring document "sealed_at")
              :spec spec
              :receipt-text (let ((receipt (jget document "receipt")))
                              (and (hash-table-p receipt) (decoded-json-text receipt)))
              :receipt (let ((receipt (jget document "receipt")))
                         (and (hash-table-p receipt) receipt)))))))

(defun decode-spec (object)
  "SPEC as SAVE-ATTEMPT wrote it, back as the canonical plist."
  (unless (hash-table-p object) (fail "a stored attempt carries no spec"))
  (list :schema-version (jinteger object "schema_version")
        :task-id (jstring object "task_id")
        :workspace-root (jstring object "workspace_root")
        :allow-loopback-http (jboolean object "allow_loopback_http")
        :checks (loop for check in (jitems (jget object "checks"))
                      collect (decode-check check))))

(defun decode-check (object)
  "One check as SAVE-ATTEMPT wrote it, back as its canonical plist."
  (let ((kind (jstring object "kind")))
    (cond
      ((equal kind "file")
       (list :id (jstring object "id") :kind "file" :path (jstring object "path")
                     :contains (jstring object "contains")
                     :json-keys (jitems (jget object "json_keys"))))
      ((equal kind "test")
       (list :id (jstring object "id") :kind "test"
                     :command (jstring object "command") :workdir (jstring object "workdir")
                     :expected-exit (jinteger object "expected_exit")
                     :timeout (jinteger object "timeout")
                     :contains (jstring object "contains")))
      (t (list :id (jstring object "id") :kind "website" :url (jstring object "url")
                       :expected-status (jinteger object "expected_status")
                       :contains (jstring object "contains"))))))

(defun attempt-files ()
  "Every attempt file in the folder, oldest name first."
  (let ((folder (attempts-directory)))
    (and (probe-file folder)
         (sort (uiop:directory-files folder "*.json") #'string< :key #'namestring))))

(defun list-attempts (task-id)
  "Every attempt kept for TASK-ID, oldest first."
  (let ((found '()))
    (dolist (path (attempt-files))
      (let* ((name (pathname-name path))
             (attempt (handler-case (read-attempt name) (error () nil))))
        (when (and attempt (equal task-id (getf attempt :task-id)))
          (push attempt found))))
    (sort found #'string< :key (lambda (attempt) (or (getf attempt :started-at) "")))))

(defun open-attempt-for (task-id)
  "The attempt still open under TASK-ID, if one is."
  (find-if (lambda (attempt) (null (getf attempt :sealed-at))) (list-attempts task-id)))

(defun export-filename (attempt-id format)
  "The file an export of ATTEMPT-ID and FORMAT is written to."
  (merge-pathnames (format nil "receipt-~a.~a" attempt-id format) (exports-directory)))

(defun write-export (attempt-id format text)
  "TEXT as the export of ATTEMPT-ID in FORMAT, under the exports folder."
  (let ((path (export-filename attempt-id format)))
    (nlk:write-file-atomically path text :parents t :mode :private :directory-mode #o700)
    path))
