;;;; cell-test.lisp --- lifecycle; /security; the report puts what to patch first.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "security-patches"
  (:fixture with-stubbed-security)
  (:config "refresh_hours" 6)
  (:help :security)
  (:command "security")
  (:running (is nodecode-security-patches::*worker* "the refresher runs")
            (is (= 6 (nodecode-security-patches::setting :refresh-hours))))
  (:stopped (is (null nodecode-security-patches::*worker*) "and stops"))
  (:refused ("refresh_hours" 0)))

(deftest security-patches-the-report-puts-what-to-patch-first (with-stubbed-security)
  (setf *sp-outputs* '(("php" "8.2.10" 0) ("node" "v24.22.1" 0)))
  (let* ((rows (nodecode-security-patches:check))
         (first-row (first rows))
         (node (find "nodejs-2026-10" rows :test #'equal :key (lambda (r) (getf r :id))))
         (railway (find "railway-2026-10-21" rows :test #'equal :key (lambda (r) (getf r :id))))
         (text (nodecode-security-patches:report :rows rows)))
    (is (equal "php2shell" (getf first-row :id)) "the vulnerable critical sorts first")
    (is (eq :vulnerable (getf first-row :status)))
    (is (eq :ok (getf node :status)) "a fixed node passes")
    (is (eq :manual (getf railway :status)) "a platform breach has no machine check")
    (is (search "PATCH CRITICAL (exploited) php2shell" text))
    (is (search "8.2.10 < 8.2.35" text))
    (is (search "READ  HIGH (exploited) railway-2026-10-21" text))
    (is (search "1 to patch" text))))

(deftest security-patches-slash-shows-one-advisory-in-full (with-stubbed-security)
  (let ((text (nodecode-security-patches:run-slash "show php2shell" nil)))
    (is (search "Fixed in: 8.5.12, 8.4.27, 8.3.36, 8.2.35" text))
    (is (search "session.upload_progress.enabled = 0" text) "the stopgap")
    (is (search "https://www.php.net/" text) "the sources"))
  (is (search "no advisory named nope" (nodecode-security-patches:run-slash "show nope" nil)))
  (is (search "php2shell" (nodecode-security-patches:run-slash "" nil)) "bare /security lists them"))
