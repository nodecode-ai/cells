;;;; scan-test.lisp --- the scan collects with no model, and the decider sorts what is left.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every source answers from a table here (*FETCH*), the news search is a
;;;; stub, and the decider is a function of the item's text, so each test
;;;; pins one rule of what reaches the agent.

(in-package #:nodecode.test)

(defparameter *sp-kev*
  "{\"vulnerabilities\":[
     {\"cveID\":\"CVE-2026-1\",\"vendorProject\":\"Acme\",\"product\":\"Web\",\"vulnerabilityName\":\"Acme Web RCE\",\"dateAdded\":\"2026-10-21\",\"shortDescription\":\"remote code execution\"},
     {\"cveID\":\"CVE-2025-9\",\"vendorProject\":\"Old\",\"product\":\"Thing\",\"vulnerabilityName\":\"old\",\"dateAdded\":\"2025-01-01\",\"shortDescription\":\"old\"}]}")

(defun sp-advisory (ghsa type severity summary ecosystem name &optional cve)
  (format nil "{\"ghsa_id\":\"~a\",\"type\":\"~a\",\"severity\":\"~a\",\"summary\":\"~a\",\"html_url\":\"https://github.com/advisories/~a\",\"cve_id\":~:[null~;\"~:*~a\"~],\"description\":\"d\",\"vulnerabilities\":[{\"package\":{\"ecosystem\":\"~a\",\"name\":\"~a\"},\"first_patched_version\":null}]}"
          ghsa type severity summary ghsa cve ecosystem name))

(defparameter *sp-sources*
  `(("known_exploited" ,*sp-kev*)
    ("severity=critical" ,(format nil "[~a,~a,~a]"
                                  (sp-advisory "GHSA-aaaa-aaaa-aaaa" "reviewed" "critical" "Pillow command injection" "pip" "pillow" "CVE-2026-2")
                                  (sp-advisory "GHSA-bbbb-bbbb-bbbb" "reviewed" "critical" "Pillow heap overflow" "pip" "pillow")
                                  (sp-advisory "GHSA-cccc-cccc-cccc" "reviewed" "critical" "Tool crash on a crafted file" "npm" "tool")))
    ("severity=high" "[]")
    ("type=malware" ,(format nil "[~a,~a]"
                             (sp-advisory "GHSA-dddd-dddd-dddd" "malware" "critical" "Malicious code in big-lib" "npm" "big-lib")
                             (sp-advisory "GHSA-eeee-eeee-eeee" "malware" "critical" "Malicious code in typo-squat" "npm" "typo-squat")))
    ("downloads/point" "{\"big-lib\":{\"downloads\":2000000},\"typo-squat\":{\"downloads\":400}}")) "URL substring => body every source answers with 200.")

(defun sp-source (url)
  (let ((row (find-if (lambda (r) (search (first r) url)) *sp-sources*)))
    (if row (values (second row) 200) (values "" 404))))

(defmacro with-scan-sources ((&key (decide '(lambda (text listed) (declare (ignore listed))
                                              (values (if (search "Pillow" text) 0.9 0.1) 0.9))))
                             &body body)
  "BODY with every scan source stubbed, no news search, and DECIDE as the decider."
  `(with-stubbed-security
     (with-saved-globals (nodecode-cyber-security::*decide* nodecode-cyber-security::*news-search*)
       (setf nodecode-cyber-security::*fetch* 'sp-source
             nodecode-cyber-security::*news-search* (constantly nil)
             nodecode-cyber-security::*decide* ,decide)
       ,@body)))

(defun sp-kept (result why)
  (remove why (getf result :keep) :key (lambda (i) (getf i :why)) :test-not #'eq))

(deftest cyber-security-the-scan-hands-the-agent-only-what-passes ()
  (with-scan-sources ()
    (let* ((r (nodecode-cyber-security:scan :since "2026-10-20T00:00:00Z" :entries '()))
           (decided (sp-kept r :decider)))
      (is (eq :ok (getf r :decider)))
      (is (equal '("CVE-2026-1") (mapcar (lambda (i) (getf i :id)) (sp-kept r :exploited))) "an exploited item in the window goes on without the decider; the old one is outside it")
      (is (= 1 (length decided)) "the decider keeps Pillow and drops the crash")
      (is (equal '("GHSA-aaaa-aaaa-aaaa" "GHSA-bbbb-bbbb-bbbb") (getf (first decided) :ids)) "two advisories of one package reach the agent as one item")
      (is (equal '("GHSA-dddd-dddd-dddd") (mapcar (lambda (i) (getf i :id)) (sp-kept r :popular-malware))) "malware in a package with real downloads is kept, by count and not by model")
      (is (equal '((:minor-or-elsewhere . 1) (:already-listed . 0) (:unpopular-malware . 1)) (getf r :dropped)))
      (is (equal '((:kev 1 1) (:github 5 5) (:news :skipped)) (getf r :sources)) "each source says what it gave, and a missing news search says so"))))

(deftest cyber-security-the-scan-skips-what-the-feed-or-an-earlier-scan-holds ()
  (with-scan-sources ()
    (let* ((dir (sp-scratch-dir "scan-seen"))
           (seen (merge-pathnames "seen.json" dir))
           (entries (nodecode-cyber-security::read-feed
                     "{\"advisories\":[{\"id\":\"acme\",\"title\":\"Acme\",\"summary\":\"s\",\"cves\":[\"CVE-2026-1\"],\"sources\":[\"https://github.com/advisories/GHSA-cccc-cccc-cccc\"]}]}"))
           (listed '()))
      (setf nodecode-cyber-security::*decide*
            (lambda (text options) (setf listed options) (values (if (search "Pillow" text) 0.9 0.1) 0.9)))
      (let ((first-scan (nodecode-cyber-security:scan :since "2026-10-20T00:00:00Z" :entries entries :seen seen)))
        (is (null (sp-kept first-scan :exploited)) "a CVE the feed carries is not handed on again")
        (is (equal '((:kev 1 0) (:github 5 4) (:news :skipped)) (getf first-scan :sources)))
        (is (equal '(("acme" . "Acme: s")) listed) "the decider is shown what the feed already lists")
        (is (probe-file seen) "what the decider sorted is written to the ledger"))
      (let ((second-scan (nodecode-cyber-security:scan :since "2026-10-20T00:00:00Z" :entries entries :seen seen)))
        (is (null (getf second-scan :keep)) "a second scan of the same window hands on nothing")
        (is (equal '((:kev 1 0) (:github 5 0) (:news :skipped)) (getf second-scan :sources)))))))

(deftest cyber-security-the-scan-says-when-the-decider-is-down ()
  (with-scan-sources (:decide (lambda (text listed) (declare (ignore text listed)) (error "401 invalid key")))
    (let* ((dir (sp-scratch-dir "scan-down"))
           (seen (merge-pathnames "seen.json" dir))
           (r (nodecode-cyber-security:scan :since "2026-10-20T00:00:00Z" :entries '() :seen seen)))
      (is (eq :unavailable (getf r :decider)))
      (is (search "401" (getf r :error)) "the reason is kept")
      (is (= 2 (length (sp-kept r :unsorted))) "everything the decider would have sorted goes on, unsorted")
      (is (not (probe-file seen)) "nothing is marked sorted, so the next scan asks again")
      (is (search "UNAVAILABLE" (nodecode-cyber-security:scan-report :since "2026-10-20T00:00:00Z" :entries '())) "the report the agent reads says the decider did not answer"))))

(deftest cyber-security-the-decider-is-nodecode-decide-v1 ()
  (is (equal '(:provider "nodecode" :model "foresight-decide-v1") nodecode-cyber-security:*decider*))
  (let* ((req (shasht:read-json (nodecode-cyber-security::decision-request "item" '(("acme" . "Acme")))))
         (qs (gethash "questions" req)))
    (is (equal "item" (gethash "state" req)))
    (is (equal "choice" (gethash "type" (gethash "kind" qs))))
    (is (gethash "act" (gethash "criteria" (gethash "kind" qs))))
    (is (gethash "acme" (gethash "criteria" (gethash "same" qs))) "the listed advisories are the second question's options"))
  (is (equal '(0.75 0.25) (multiple-value-list (nodecode-cyber-security::decision
                                                 "{\"answers\":{\"kind\":{\"type\":\"choice\",\"probabilities\":{\"act\":0.75}},\"same\":{\"type\":\"choice\",\"probabilities\":{\"new\":0.25}}}}"))))
  (is (null (nodecode-cyber-security::decision "not json")))
  (let ((model :unset))
    (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                (setf model (list nle::*auxiliary-model* fallback-p))
                                "{\"answers\":{\"kind\":{\"type\":\"choice\",\"probabilities\":{\"act\":0.5}}}}")
      (is (equal '(0.5 1.0) (multiple-value-list (nodecode-cyber-security::ask-decider "item" '()))))
      (is (equal (list nodecode-cyber-security:*decider* nil) model) "the side call runs on Decide v1 with no fallback to another model"))))

(deftest cyber-security-the-scan-names-what-has-aged-out ()
  (let ((entries (nodecode-cyber-security::read-feed
                  "{\"advisories\":[{\"id\":\"old\",\"date\":\"2026-09-01\",\"exploited\":false},{\"id\":\"old-exploited\",\"date\":\"2026-09-01\",\"exploited\":true},{\"id\":\"fresh\",\"date\":\"2026-10-20\",\"exploited\":false}]}")))
    (is (equal '("old") (nodecode-cyber-security::expiring entries (nodecode-cyber-security::parse-iso "2026-10-22T00:00:00Z"))) "past 14 days and not exploited is due to go; exploited stays for the agent to judge")))
