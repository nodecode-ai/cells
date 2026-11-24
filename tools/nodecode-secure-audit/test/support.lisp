;;;; support.lisp --- the secure-audit test runner and its helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic posture) under a SECURE-AUDIT-CELL- name
;;;; prefix, so RUN-SECURE-AUDIT-TESTS runs exactly that slice. Nothing here
;;;; reaches outside the image: the scan and the redactor read a string, and
;;;; the audit is pointed at a directory the test makes and removes.

(in-package #:nodecode.test)

(define-test-slice "secure-audit" "SECURE-AUDIT-CELL-" :start nodecode-secure-audit:start-cell)

(defun sec-tool (name &rest pairs)
  "A registered tool called the way the engine's body calls it: the decoded
contract record built from PAIRS."
  (let ((entry (nle::find-tool name))
        (object (apply #'nlk:make-json-object pairs)))
    (unless entry (error "no tool named ~a" name))
    (funcall (third entry)
             (cond ((string= name "security_scan_text")
                    (nodecode-secure-audit::decode-scan-text-request object))
                   ((string= name "security_redact_text")
                    (nodecode-secure-audit::decode-redact-text-request object))
                   (t (nodecode-secure-audit::decode-audit-request object))))))

(defun sec-scan (text)
  "security_scan_text on TEXT."
  (sec-tool "security_scan_text" "text" text))

(defun sec-redact (text)
  "security_redact_text on TEXT."
  (sec-tool "security_redact_text" "text" text))

(defun sec-slash (args)
  "/secure-audit ARGS in a session, answering its text."
  (nodecode-secure-audit::run-slash args "s1"))

(defun sec-home (config-text)
  "A scratch home holding CONFIG-TEXT as its config, answering
(values DIRECTORY THUNK); THUNK removes the tree."
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/secure-audit-~a/"
                       (string-right-trim "/" (namestring (uiop:temporary-directory)))
                       (random 1000000)))))
    (ensure-directories-exist dir)
    (when config-text
      (with-open-file (out (merge-pathnames "config.jsonc" dir)
                           :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string config-text out)))
    (values dir (lambda () (ignore-errors (uiop:delete-directory-tree dir :validate t))))))
