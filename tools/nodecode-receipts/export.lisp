;;;; export.lisp --- the receipt as a document: JSON, and a page anyone can read.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An export is written from the sealed receipt and nothing else. Public is
;;;; the default: absolute paths, the command line and the raw output are
;;;; dropped, so the page can be handed to someone else. The page carries no
;;;; script and no remote asset, so it opens anywhere.

(in-package #:nodecode-receipts)
(defparameter +redacted-path+ "<redacted-path>"
  "What a public export writes where an absolute path stood.")

(defparameter +redacted-command+ "<redacted-command>"
  "What a public export writes where the command line stood.")

(defun check-object (node)
  "NODE's checks, a list of the objects a receipt carries."
  (remove-if-not #'hash-table-p (jitems (jget node "checks"))))

(defun public-receipt (receipt)
  "RECEIPT with the operator's paths, command lines and raw output dropped:
the view an export shows by default."
  (let ((copy (copy-table receipt)))
    (let ((spec (jget copy "spec")))
      (when (hash-table-p spec)
        (let ((spec-copy (copy-table spec)))
          (setf (gethash "workspace_root" spec-copy) +redacted-path+
                (gethash "checks" spec-copy) (public-checks (jget spec "checks")))
          (setf (gethash "spec" copy) spec-copy))))
    (setf (gethash "checks" copy) (public-checks (jget copy "checks"))
          (gethash "public_export" copy) t
          (gethash "raw_command_output_omitted" copy) t)
    copy))

(defun public-checks (checks)
  "CHECKS with each test's workdir and command replaced and its output dropped."
  (loop for check in (jitems checks)
               when (hash-table-p check)
                 collect (let ((copy (copy-table check)))
                           (dolist (key '("workdir" "command"))
                             (when (gethash key copy) (setf (gethash key copy) +redacted-path+)))
                           (when (equal (gethash "kind" copy) "test")
                             (setf (gethash "command" copy) +redacted-command+))
                           (let ((evidence (jget copy "evidence")))
                             (when (hash-table-p evidence)
                               (let ((evidence-copy (copy-table evidence)))
                                 (remhash "output" evidence-copy)
                                 (remhash "command" evidence-copy)
                                 (when (gethash "workdir" evidence-copy)
                                   (setf (gethash "workdir" evidence-copy) +redacted-path+))
                                 (setf (gethash "evidence" copy) evidence-copy))))
                           copy)))

(defun json-export (receipt public)
  "RECEIPT as the JSON document an export holds."
  (decoded-json-text (if public (public-receipt receipt) receipt)))

;;; --- the page -------------------------------------------------------------------

(defun html-escape (text)
  "TEXT with what a page reads as markup escaped."
  (with-output-to-string (out)
    (loop for character across (or text "")
          do (case character
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char character out))))))

(defun html-rows (object)
  "OBJECT's members as table rows, keys sorted, nested objects flattened."
  (with-output-to-string (out)
    (dolist (key (jkeys object))
      (let ((value (gethash key object)))
        (format out "<tr><th>~a</th><td>~a</td></tr>~%"
                (html-escape key)
                (html-escape (cond ((stringp value) value)
                                   ((integerp value) (princ-to-string value))
                                   ((eq value t) "yes")
                                   ((null value) "--")
                                   (t (decoded-json-text value)))))))))

(defun html-check-card (check)
  "One check of the receipt as a card: what it wanted, what it answered."
  (let ((state (or (jstring check "state") "unverified")))
    (with-output-to-string (out)
      (format out "<section class=\"card ~a\">~%" (html-escape state))
      (format out "<h2>~a <span class=\"kind\">~a</span> <span class=\"state\">~a</span></h2>~%"
              (html-escape (or (jstring check "id") "check"))
              (html-escape (or (jstring check "kind") "check"))
              (html-escape state))
      (format out "<p class=\"reason\">~a</p>~%" (html-escape (or (jstring check "reason") "")))
      (let ((evidence (jget check "evidence")))
        (when (hash-table-p evidence)
          (format out "<table>~a</table>~%" (html-rows evidence))))
      (format out "</section>~%"))))

(defun html-export (receipt public)
  "RECEIPT as a standalone page: state, task, digest, one card per check."
  (let* ((view (if public (public-receipt receipt) receipt))
         (state (or (jstring view "state") "unverified")))
    (with-output-to-string (out)
      (format out "<!DOCTYPE html>~%<html lang=\"en\"><head><meta charset=\"utf-8\">~%")
      (format out "<title>Receipt ~a: ~a</title>~%"
              (html-escape (or (jstring view "task_id") "")) (html-escape state))
      (write-string "<style>body{background:#14161a;color:#e6e6e6;font:15px/1.5 system-ui,sans-serif;margin:2rem auto;max-width:52rem;padding:0 1rem}h1{font-size:1.35rem}.state{font-weight:600}.verified .state,.verified h1{color:#4ec97a}.failed .state,.failed h1{color:#e05c5c}.unverified .state,.unverified h1{color:#e0b44c}.card{border:1px solid #2a2e36;border-radius:8px;padding:.75rem 1rem;margin:1rem 0}.kind{font-size:.8rem;color:#8b93a1}.reason{color:#c8ccd4}table{border-collapse:collapse;width:100%}th,td{text-align:left;padding:.15rem .5rem;border-bottom:1px solid #24272e;vertical-align:top}th{color:#8b93a1;font-weight:500;width:30%}</style>~%" out)
      (format out "</head><body class=\"~a\">~%" (html-escape state))
      (format out "<h1>Receipt for ~a: ~a</h1>~%"
              (html-escape (or (jstring view "task_id") "")) (html-escape state))
      (format out "<p>Attempt ~a, started ~a, checked ~a.</p>~%"
              (html-escape (or (jstring view "attempt_id") ""))
              (html-escape (or (jstring view "started_at") "--"))
              (html-escape (or (jstring view "checked_at") "--")))
      (format out "<p>Criteria digest <code>~a</code>.</p>~%"
              (html-escape (or (jstring view "criteria_digest") "")))
      (dolist (check (check-object view)) (write-string (html-check-card check) out))
      (let ((disclosures (jitems (jget view "disclosures"))))
        (when disclosures
          (format out "<ul>~%")
          (dolist (line disclosures)
            (format out "<li>~a</li>~%" (html-escape (if (stringp line) line ""))))
          (format out "</ul>~%")))
      (write-string "</body></html>~%" out))))
