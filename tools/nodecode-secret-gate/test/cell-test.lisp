;;;; cell-test.lisp --- lifecycle, the rules, the gate, the audit, the receipts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring and the rules the original
;;;; shipped: a start installs one advice, three tools, one command and one
;;;; manual topic and a stop takes them all back, a section the declaration
;;;; cannot read refuses and leaves nothing behind, a known provider format is
;;;; LEAKED and blocks while a token-shaped string or a credential path only
;;;; needs a person, the answer never carries the secret itself, every tool
;;;; result is audited without being changed, and the receipt ring stays
;;;; bounded.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "secret-gate"
  (:hooks :tool)
  (:command "secret-gate")
  (:help :secret-gate)
  (:running (is (nle::find-tool "secret_scan") "the scan tool is registered")
            (is (nle::find-tool "secret_gate_check") "and the gate")
            (is (nle::find-tool "secret_gate_receipts") "and the receipts")
            (is (= 100 (nodecode-secret-gate::setting :max-receipts)) "the ring holds a hundred")
            (is (eq t (nodecode-secret-gate::setting :audit-tool-results)) "every tool result is audited by default")
            (is (equal "4.0" (nodecode-secret-gate::setting :min-entropy)) "the entropy threshold is the original's four bits"))
  (:stopped (is (null (nle::find-tool "secret_scan")) "stop unregisters the tools")
            (is (null (gate-hook)) "and takes the advice off :tool"))
  (:refused ("max_receipts" 0) ("min_entropy" "9.0")))

(deftest secret-gate-cell-a-string-where-the-array-belongs-refuses (with-saved-globals (nle:*tools*))
  (let ((refuse (lambda () (secret-gate-start "max_receipts" "lots"))))
    (is (signals-error nlk:config-refusal (funcall refuse)) "an integer is never coerced"))
  (is (null (nle::find-tool "secret_scan")) "and the refused start installed nothing")
  (is (null (gate-hook)) "not even the advice"))

;;; --- the rules ----------------------------------------------------------------

(deftest secret-gate-cell-a-known-provider-format-is-leaked (with-cell-stop ((secret-gate-start))
  (let ((answer (gate-scan +leaky+)))
    (is (search "LEAKED" answer) "an AWS key id is leaked")
    (is (search "aws_access_key" answer) "the rule that fired is named")
    (is (search "AKIA... (20 chars)" answer) "the evidence is the prefix and the length")
    (is (not (search "AKIAIOSFODNN7EXAMPLE" answer)) "and never the secret itself"))
  (is (search "github_token" (gate-scan "ghp_1234567890abcdefghijABCDEFGHIJ123456")) "a GitHub token is named")
  (is (search "jwt" (gate-scan "Authorization: Bearer eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0In0.dBjftJeZ4CVPmB92K27uhbUJU1p1r_wW1g")) "a JWT is named")
  (is (search "private_key" (gate-scan "-----BEGIN RSA PRIVATE KEY-----")) "a key block is named")))

(deftest secret-gate-cell-a-token-shaped-string-needs-a-person (with-cell-stop ((secret-gate-start))
  (let ((answer (gate-scan +randomish+)))
    (is (search "REVIEW" answer) "a long high-entropy string is not conclusive")
    (is (search "high_entropy_token" answer) "the rule is the entropy one")
    (is (not (search "k3Jd9Xk2Lm4Qp7Rt1Vw8Yz3Bc6Nf0Gh5" answer)) "and the token itself is masked"))
  (is (search "secret_path" (gate-scan "open('/home/dev/.ssh/id_ed25519')")) "a credential path is REVIEW")
  (is (search "CLEAR" (gate-scan "the build is green and the port is 8080")) "plain prose is clear")
  (is (search "CLEAR" (gate-scan "version 1.2.3-rc4 of the thing")) "and so is a version string") ))

(deftest secret-gate-cell-the-gate-fails-closed (with-cell-stop ((secret-gate-start))
  (let ((blocked (gate-check +leaky+ "a public issue")))
    (is (search "BLOCK: LEAKED for a public issue" blocked) "leaked content blocks, and says where it was going")
    (is (search "receipt HSG-" blocked) "with a receipt")
    (is (search "do not send" blocked) "and what to do about it"))
  (is (search "WARN: REVIEW" (gate-check +randomish+)) "a reviewable string warns")
  (is (search "ALLOW: CLEAR" (gate-check "nothing to see here")) "and clean text is allowed")))

;;; --- the audit on every tool result ---------------------------------------------

(deftest secret-gate-cell-every-tool-result-is-audited (with-cell-stop ((secret-gate-start))
  (is (equal +leaky+ (gate-audit +leaky+)) "the result comes back untouched")
  (let ((receipt (first (gate-receipts))))
    (is (equal "tool_result" (getf receipt :kind)) "a receipt is recorded")
    (is (equal "LEAKED" (getf receipt :verdict)) "with the verdict")
    (is (equal "BLOCK" (getf receipt :gate)))
    (is (not (search "AKIAIOSFODNN7EXAMPLE" (format nil "~s" receipt))) "and no secret material in it"))
  (let ((before (length (gate-receipts))))
    (is (equal "all quiet" (gate-audit "all quiet")) "a clean result passes")
    (is (= before (length (gate-receipts))) "and is not receipted"))))

(deftest secret-gate-cell-the-audit-can-be-turned-off (with-cell-stop ((secret-gate-start "audit_tool_results" nil))
  (is (equal +leaky+ (gate-audit +leaky+)) "the result is still handed back")
  (is (null (gate-receipts)) "but nothing was recorded")
  (is (search "LEAKED" (gate-scan +leaky+)) "the tool itself still works")))

;;; --- receipts and the command ----------------------------------------------------

(deftest secret-gate-cell-the-receipt-ring-is-bounded (with-cell-stop ((secret-gate-start "max_receipts" 2))
  (gate-check +leaky+)
  (gate-check +randomish+)
  (gate-check +leaky+)
  (is (= 2 (length (gate-receipts))) "the ring keeps the newest two")
  (let ((listed (gate-receipts-tool 1)))
    (is (search "2 receipts held" listed) "the tool says how many are held")
    (let* ((lines (uiop:split-string listed :separator '(#\Newline)))
           (ids (remove-if-not (lambda (line) (search "HSG-" line)) lines)))
      (is (= 1 (length ids)) "and lists the one asked for")))))

(deftest secret-gate-cell-the-command-answers-both-ways (with-cell-stop ((secret-gate-start))
  (is (search "secret gate: 0 receipts held" (gate-slash "")) "with nothing typed it answers its state")
  (gate-check +randomish+)
  (is (search "1 receipt held" (gate-slash "")) "and counts what it holds")
  (is (search "LEAKED" (gate-slash +leaky+)) "with text it previews the verdict")
  (is (search "AKIA... (20 chars)" (gate-slash +leaky+)) "masked, as the tool answers")))
