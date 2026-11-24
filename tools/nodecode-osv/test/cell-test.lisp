;;;; cell-test.lisp --- the OSV cell, end to end over a scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one network call is NODECODE-OSV::HTTP; WITH-SCRIPTED-OSV stubs that
;;;; name, records every url asked for, and answers a fixture -- so no test
;;;; opens a socket. The lifecycle tests are the machinery's own: a start
;;;; installs the manual topic, the tool and /osv, a stop takes all three
;;;; back, a second start leaves one of everything, and a bad setting refuses.

(in-package #:nodecode.test)

(define-test-slice "osv" "OSV-CELL-" :start nodecode-osv:start-cell)

;;; --- the scripted seam ---------------------------------------------------------------

(defmacro with-scripted-osv ((urls &key answer) &body body-forms)
  "Run BODY-FORMS with NODECODE-OSV::HTTP recording each url into URLS and
answering (values BODY STATUS): ANSWER is a function of the url."
  (let ((url (gensym "URL")))
    `(let ((,urls '()))
       (with-stubbed-fdefinition (nodecode-osv::http (method ,url &rest arguments)
                                 (declare (ignore method arguments))
                                 (setf ,urls (append ,urls (list ,url)))
                                 (funcall ,answer ,url))
         ,@body-forms))))

(defun osv-json (object)
  "OBJECT as the JSON text OSV answers with."
  (nlk:encode-json-object object))

(defun osv-record (&rest pairs)
  "A decoded OSV record from alternating key/value PAIRS."
  (apply #'nlk:make-json-object pairs))

(defun cve-fixture (url)
  "The CVE record for the vulns endpoint: package data only on its aliases."
  (declare (ignore url))
  (values (osv-json
           (osv-record "id" "CVE-2024-3651"
                        "summary" "Denial of Service via Quadratic Complexity in kjd/idna"
                        "aliases" (vector "GHSA-jjg7-2v4v-x38h" "PYSEC-2024-60")
                        "published" "2024-07-07T00:00:00Z"
                        "modified" "2026-09-08T00:00:00Z"
                        "severity" (vector (osv-record "type" "CVSS_V3"
                                                        "score" "CVSS:3.0/AV:L/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:H"))
                        "affected" (vector (osv-record
                                            "package" (osv-record "name" "idna" "ecosystem" "PyPI")
                                            "ranges" (vector (osv-record
                                                              "type" "GIT"
                                                              "repo" "https://github.com/kjd/idna"
                                                              "events" (vector (osv-record "introduced" "0"))))))
                        "details" "A vulnerability was identified in the kjd/idna library, specifically within idna.encode()."
                        "references" (vector (osv-record "type" "ADVISORY"
                                                          "url" "https://nvd.nist.gov/vuln/detail/CVE-2024-3651"))))
          200))

(defun alias-fixture (url)
  "The alias records: the GHSA one carries the package range and the fix."
  (cond ((cl:search "GHSA-" url)
         (values (osv-json
                  (osv-record "id" "GHSA-jjg7-2v4v-x38h"
                               "aliases" (vector "CVE-2024-3651")
                               "database_specific" (osv-record "severity" "MODERATE"
                                                                "cwe_ids" (vector "CWE-1333"))
                               "affected" (vector (osv-record
                                                   "package" (osv-record "name" "idna" "ecosystem" "PyPI")
                                                   "ranges" (vector (osv-record
                                                                     "type" "ECOSYSTEM"
                                                                     "events" (vector (osv-record "introduced" "0")
                                                                                      (osv-record "fixed" "3.7"))))))))
                 200))
        (t (values (osv-json (osv-record "id" "PYSEC-2024-60"
                                          "affected" (vector (osv-record
                                                              "package" (osv-record "name" "idna" "ecosystem" "PyPI")
                                                              "ranges" (vector (osv-record
                                                                                "type" "ECOSYSTEM"
                                                                                "events" (vector (osv-record "introduced" "0.1")
                                                                                                 (osv-record "fixed" "3.7"))))))))
                   200))))

(defun all-records (url)
  "Every fixture behind one entry point."
  (if (cl:search "CVE-2024-3651" url)
      (cve-fixture url)
      (alias-fixture url)))

;;; --- the lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "osv"
  (:help :osv)
  (:command "osv")
  (:refused ("max_chars" 100) ("timeout_seconds" 0) ("alias_lookups" -1))
  (:idle nodecode-osv:osv-error (osv:lookup "CVE-2024-3651")))

;;; --- what the model reads ------------------------------------------------------------------

(deftest osv-cell-merges-the-alias-records ()
  (with-cell-stop ((osv-start))
    (with-scripted-osv (urls :answer #'all-records)
      (let ((block (nodecode-osv::lookup "CVE-2024-3651")))
        (is (cl:search "OSV: CVE-2024-3651" block) "the block names the id")
        (is (cl:search "https://osv.dev/vulnerability/CVE-2024-3651" block) "and links its page")
        (is (cl:search "Aliases: GHSA-jjg7-2v4v-x38h, PYSEC-2024-60" block) "with its aliases")
        (is (cl:search "PyPI idna: >= 0.1, < 3.7 (fixed in 3.7) [PYSEC-2024-60]" block) "package data is merged from the alias records, with its source")
        (is (cl:search "Severity: CVSS_V3 CVSS:3.0/AV:L/AC:L" block) "severity is OSV's own vector")
        (is (cl:search "Weaknesses: CWE-1333" block) "and the weaknesses")
        (is (cl:search "Source: OSV.dev." block) "the block says where it came from")
        (is (= 3 (length urls)) "one request for the id and one per alias, and no more")))))

(deftest osv-cell-the-tool-and-the-command-answer-the-same-record ()
  (with-cell-stop ((osv-start))
    (with-scripted-osv (urls :answer #'all-records)
      (let* ((arguments (nodecode-osv::decode-osv-lookup (nlk:json-object "id" "2024-3651")))
             (answer (nodecode-osv::lookup-tool arguments)))
        (is (cl:search "Aliases: GHSA-jjg7-2v4v-x38h" answer) "the tool answers the alias ids")
        (is (cl:search "CVE-2024-3651" (first urls)) "a bare year-serial is sent as a CVE id")
        (let ((replied (cell-entry "nodecode-osv" "osv" "CVE-2024-3651" "osv-cell-command")))
          (is (cl:search "OSV: CVE-2024-3651" replied) "/osv answers the same record")
          (is (cl:search "Affected packages:" replied) "with the package lines")))
      (let ((usage (cell-entry "nodecode-osv" "osv" "" "osv-cell-usage")))
        (is (cl:search "usage: /osv" usage) "/osv alone says how it is used")))))

(deftest osv-cell-an-unknown-id-is-a-refusal-naming-it ()
  (with-cell-stop ((osv-start))
    (with-scripted-osv (urls :answer (lambda (url)
                                        (declare (ignore url))
                                        (values (osv-json (osv-record "code" 5
                                                                       "message" "Vulnerability not found"))
                                                404)))
      (is (cl:search "OSV has no record CVE-2099-0001"
                     (refusal-text nodecode-osv:osv-error (osv:lookup "CVE-2099-0001"))) "the refusal names the id that was not found"))))

(deftest osv-cell-an-id-that-is-no-osv-id-is-refused-before-any-request ()
  (with-cell-stop ((osv-start))
    (with-scripted-osv (urls :answer #'all-records)
      (is (cl:search "is not an OSV id"
                     (refusal-text nodecode-osv:osv-error (osv:lookup "../../etc/passwd"))) "a path cannot become a request")
      (is (cl:search "is not a GHSA id"
                     (refusal-text nodecode-osv:osv-error (osv:lookup "GHSA-nope"))) "and a short advisory id is refused too")
      (is (null urls) "neither reached the network"))))
