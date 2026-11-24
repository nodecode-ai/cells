;;;; support.lisp --- the security-scan test slice, a scratch folder, one seam.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; SECURITY-SCAN-CELL- name prefix, so RUN-SECURITY-SCAN-TESTS runs exactly this
;;;; slice. Nothing of the cell is stubbed but the network: SECSCAN:*HTTP* is
;;;; the cell's one call, and NO-NETWORK fails a test that forgot it. A
;;;; scratch folder under the platform's temporary directory stands in for the
;;;; repository under review, and the scan's record and reports go under the
;;;; run's own home.

(in-package #:nodecode.test)

(define-test-slice "security-scan" "SECURITY-SCAN-CELL-" :start nodecode-security-scan:start-cell)

(defun no-network (method url &rest keys)
  (declare (ignore keys))
  (error "the test reached the network: ~a ~a" method url))

(defvar *stub-http* nil
  "The answer the cell's one network call gives while a test binds it.")

(defmacro with-scan-cell ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS (allowed_origins, say) and
stopped on unwind, and its one network call either the test's stub or a refusal."
  `(let ((secscan:*http* (or *stub-http* #'no-network)))
     (with-cell-stop ((security-scan-start ,@pairs))
       ,@body)))

(defmacro with-dir ((var) &body body)
  "BODY over a fresh temporary folder named VAR, removed on unwind."
  `(let ((,var (merge-pathnames (format nil "secscan-~a/" (gensym)) (uiop:temporary-directory))))
     (ensure-directories-exist ,var)
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,var :validate t)))))

(defun put (root path text)
  "PATH under ROOT holding TEXT, its parents made."
  (let ((file (uiop:subpathname* root path)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string text out))
    file))

(defun only-scan ()
  "The scan of the one start this test made."
  (loop for id being the hash-keys of secscan::*scans*
        do (return (secscan::find-scan id))))

(defun only-id ()
  (getf (only-scan) :id))

(defun digest-of ()
  (getf (only-scan) :digest))

(defun finding-payload (&key (digest (digest-of)) (title "SQL built by concatenation") (severity "high")
                          (path "src/app.lisp") (line 2) (rule "nc/sql-injection") (cwe "CWE-89")
                          (coverage '("C1")) (attempt "w1"))
  "A canonical worker result, as one reviewer's tool call carries it."
  (flet ((units ()
           (coerce (mapcar (lambda (unit)
                             (nlk:json-object "unit" unit "state" "reviewed" "note" "read it"))
                           coverage)
                   'vector)))
    (nlk:json-object
     "scanId" (only-id)
     "attemptId" attempt
     "workerRole" "review"
     "snapshotDigest" digest
     "candidates" (vector (nlk:json-object
                            "title" title "severity" severity
                            "summary" (format nil "~a is passed to the query at ~a:~d" title path line)
                            "ruleId" rule
                            "locations" (vector (nlk:json-object "path" path "startLine" line "endLine" line))
                            "cwe" (vector cwe)))
     "coverage" (units))))

(defun receipt (id status &key positive negative (note ""))
  (nlk:json-object "findingId" id "status" status
                   "positiveControl" (and positive t)
                   "negativeControl" (and negative t)
                   "note" note))
