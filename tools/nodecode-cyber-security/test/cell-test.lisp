;;;; cell-test.lisp --- lifecycle; /security; the report puts what to patch first.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "cyber-security"
  (:fixture with-stubbed-security)
  (:config "refresh_hours" 6)
  (:help :security)
  (:command "security")
  (:running (is nodecode-cyber-security::*worker* "the refresher runs")
            (is (= 6 (nodecode-cyber-security::setting :refresh-hours))))
  (:stopped (is (null nodecode-cyber-security::*worker*) "and stops"))
  (:refused ("refresh_hours" 0)))

(deftest cyber-security-the-report-puts-what-to-patch-first (with-stubbed-security)
  (setf *sp-outputs* '(("php" "8.2.10" 0)))
  (let* ((rows (nodecode-cyber-security:check))
         (first-row (first rows))
         (railway (find "railway-2026-10-21" rows :test #'equal :key (lambda (r) (getf r :id))))
         (text (nodecode-cyber-security:report :rows rows)))
    (is (equal "php2shell" (getf first-row :id)) "the vulnerable critical sorts first")
    (is (eq :vulnerable (getf first-row :status)))
    (is (eq :manual (getf railway :status)) "a platform breach has no machine check")
    (is (search "PATCH CRITICAL (exploited) php2shell" text))
    (is (search "8.2.10 < 8.2.35" text))
    (is (search "READ  HIGH (exploited) railway-2026-10-21" text))
    (is (search "1 to patch" text)))
  ;; The same advisory on a release that carries the fix: the feed drops and
  ;; adds advisories, so the passing case rests on the one this test already
  ;; names, not on a second id the feed may retire.
  (setf *sp-outputs* '(("php" "8.2.35" 0)))
  (is (eq :ok (getf (find "php2shell" (nodecode-cyber-security:check) :test #'equal :key (lambda (r) (getf r :id)))
                    :status))
      "a fixed php passes"))

(deftest cyber-security-slash-shows-one-advisory-in-full (with-stubbed-security)
  (let ((text (nodecode-cyber-security:run-slash "show php2shell" nil)))
    (is (search "Fixed in: 8.5.12, 8.4.27, 8.3.36, 8.2.35" text))
    (is (search "session.upload_progress.enabled = 0" text) "the stopgap")
    (is (search "https://www.php.net/" text) "the sources"))
  (is (search "no advisory named nope" (nodecode-cyber-security:run-slash "show nope" nil)))
  (is (search "php2shell" (nodecode-cyber-security:run-slash "" nil)) "bare /security lists them")
  (let* ((panel (nth-value 1 (nodecode-cyber-security:run-slash "" nil)))
         (runs (remove nil (mapcar (lambda (row) (getf (getf row :run) :enter)) (getf panel :rows)))))
    (is (member "/security show php2shell" runs :test #'equal) "a row per advisory opens its fix")
    (is (equal '("/security check" "/security refresh") (last runs 2)) "then the checks and the feed")
    (is (equal '("/security check") (remove nil (mapcar (lambda (row) (getf (getf row :ask) :line)) (getf panel :rows))))))
  (is (search "nope is not one of its choices" (nodecode-cyber-security:run-slash "nope" nil))))
