;;;; cell-test.lisp --- lifecycle, the inventory, the catalog, the diff, OSV, the tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test here builds a folder of its own with the package.json and
;;;; .dist-info files a machine would have, so nothing reads the real one and
;;;; no package manager runs. The OSV call is the cell's one seam and every
;;;; test binds it: a scan that would dial is a test that forgot its stub.

(in-package #:nodecode.test)

(define-test-slice "bumblebee" "BUMBLEBEE-CELL-" :start nodecode-bumblebee:start-cell)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "bumblebee"
  (:help :bumblebee)
  (:command "bumblebee")
  (:running (is (nle::find-tool "bumblebee_guard_scan") "the scan tool is registered")
            (is (nle::find-tool "bumblebee_guard_status") "the status tool")
            (is (nle::find-tool "bumblebee_guard_diff") "the diff tool")
            (is (= 6 (nodecode-bumblebee::setting :max-depth)) "max_depth defaults to six")
            (is (= 20000 (nodecode-bumblebee::setting :max-packages)))
            (is (eq t (nodecode-bumblebee::setting :osv-enabled)) "osv is on by default"))
  (:stopped (is (null (nle::find-tool "bumblebee_guard_scan")) "stop unregisters the tools"))
  (:refused ("max_depth" 0))
  (:idle nodecode-bumblebee:bumblebee-error (bumblebee:status)))

;;; --- a folder as a machine would have it ------------------------------------------

(defun bumblebee-tree (&rest packages)
  "A scratch folder holding the named packages: (:npm NAME VERSION) or
(:pypi NAME VERSION), each the way its manager leaves it on disk."
  (let ((root (merge-pathnames (format nil "bumblebee-~a/" (gensym)) (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "keep" root))
    (dolist (package packages)
      (destructuring-bind (ecosystem name version) package
        (ecase ecosystem
          (:npm
           (let* ((dir (merge-pathnames (format nil "node_modules/~a/" name) root))
                  (file (merge-pathnames "package.json" dir)))
             (ensure-directories-exist file)
             (with-open-file (out file :direction :output :if-exists :supersede
                                        :if-does-not-exist :create)
               (format out "{\"name\": \"~a\", \"version\": \"~a\"}" name version))))
          (:pypi
           (let ((dir (merge-pathnames (format nil "site-packages/~a-~a.dist-info/" name version)
                                        root)))
             (ensure-directories-exist (merge-pathnames "METADATA" dir)))))))
    root))

(defmacro with-bumblebee-tree ((var &rest packages) &body body)
  `(let ((,var (bumblebee-tree ,@(mapcar (lambda (package) `(list ,@package)) packages))))
     (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree ,var :validate t)))
       ,@body)))

(defun forget-scans ()
  "A test starts with no scan and no baseline, as a fresh home has none."
  (dolist (file (list (nodecode-bumblebee::summary-file)
                       (nodecode-bumblebee::baseline-file)))
    (when (probe-file file) (ignore-errors (delete-file file))))
  (nle:notice nil :key "nodecode-bumblebee"))

(defmacro with-quiet-cells (&body body)
  "Start a bumblebee with OSV off -- the catalog needs no network -- and
leave nothing of it or its files behind."
  `(with-cell-stop ((bumblebee-start "osv_enabled" nil))
     (forget-scans)
     (unwind-protect (progn ,@body)
       (forget-scans))))

(defun notice-text ()
  (princ-to-string (nlk:standing-notices)))

;;; --- the inventory and the catalog -------------------------------------------------

(deftest bumblebee-cell-a-curated-match-is-a-finding ()
  (with-quiet-cells
    (with-bumblebee-tree (root (:npm "left-pad" "1.3.0") (:npm "event-stream" "3.3.6"))
      (let ((report (bumblebee:scan (namestring root))))
        (is (search "bumblebee: 1 known-compromised package among 2 packages" report)
            (format nil "one match out of two packages; the report was ~a" report))
        (is (search "[critical] npm/event-stream@3.3.6" report) "the finding names the version and how bad it is")
        (is (search "bitcoin stealer" report) "and why it is bad")
        (is (null (search "npm/left-pad" report)) "a package the catalog does not know is no finding")
        (is (search "known-compromised" (notice-text)) "a scan with findings stands as a notice the model reads")))))

(deftest bumblebee-cell-a-pypi-name-matches-whatever-its-spelling ()
  (with-quiet-cells
    ;; a dist-info folder writes an underscore where PyPI writes a dash
    (with-bumblebee-tree (root (:pypi "ultralytics" "8.3.42"))
      (let ((report (bumblebee:scan (namestring root))))
        (is (search "[high] pypi/ultralytics@8.3.42" report)
            (format nil "the PyPI match is exact; the report was ~a" report))))))

(deftest bumblebee-cell-a-clean-scan-says-clean-and-clears-the-notice ()
  (with-quiet-cells
    (with-bumblebee-tree (root (:npm "left-pad" "1.3.0"))
      (let ((report (bumblebee:scan (namestring root))))
        (is (search "bumblebee: clean" report) "a machine with nothing known bad says so")
        (is (search "match none of the" report) "and against what it was checked")
        (is (null (search (namestring root) (notice-text))) "a clean scan clears the standing notice")
        (is (search (string-right-trim "/" (namestring root)) (bumblebee:status)) "while the status still names the last scan")))))

;;; --- the baseline and the diff -----------------------------------------------------

(deftest bumblebee-cell-a-diff-names-what-appeared-since-the-baseline ()
  (with-quiet-cells
    (with-bumblebee-tree (root (:npm "event-stream" "3.3.6"))
      (bumblebee:scan (namestring root))
      (is (search "baseline taken" (bumblebee:diff "set_baseline")) "a baseline is stored")
      (let ((package (merge-pathnames "node_modules/ua-parser-js/" root)))
        (ensure-directories-exist (merge-pathnames "package.json" package))
        (with-open-file (out (merge-pathnames "package.json" package) :direction :output
                                                                     :if-exists :supersede
                                                                     :if-does-not-exist :create)
          (write-string "{\"name\": \"ua-parser-js\", \"version\": \"0.7.29\"}" out)))
      (bumblebee:scan (namestring root))
      (let ((text (bumblebee:diff "show")))
        (is (search "1 added" text) (format nil "the new one is the diff; it said ~a" text))
        (is (search "+ npm/ua-parser-js@0.7.29" text) "named, exactly")
        (is (search "2 now" text) "and what the catalog holds now"))
      (is (search "no baseline" (refusal-text nodecode-bumblebee:bumblebee-error
                                               (progn (delete-file (nodecode-bumblebee::baseline-file))
                                                      (bumblebee:diff "show")))) "without a baseline the diff refuses and says which call takes one"))))

;;; --- OSV, the one network call -------------------------------------------------------

(deftest bumblebee-cell-osv-counts-come-from-one-stubbed-batch ()
  (with-cell-stop ((bumblebee-start))
    (forget-scans)
    (unwind-protect
         (with-bumblebee-tree (root (:npm "left-pad" "1.3.0") (:npm "event-stream" "3.3.6"))
           (let ((calls '()))
             (let ((nodecode-bumblebee::*http* (lambda (method url &rest arguments)
                           (declare (ignore arguments))
                           (push (cons method url) calls)
                           (values (nlk:encode-json-object
                                    (nlk:json-object
                                     "results"
                                     (vector (nlk:json-object "vulns" (vector (nlk:json-object "id" "GHSA-1")
                                                                               (nlk:json-object "id" "GHSA-2")))
                                             (nlk:json-object))))
                                   200 nil))))
             (let ((report (bumblebee:scan (namestring root))))
               (is (= 1 (length calls)) (format nil "one batch, not one call a package; it made ~a; switch ~s" calls (nodecode-bumblebee::setting :osv-enabled)))
               (is (equal :post (caar calls)) "a POST")
               (is (equal nodecode-bumblebee::*osv-batch-url* (cdar calls)) "to OSV's batch endpoint")
               (is (search "osv: 2 advisories across 2 packages checked" report)
                   (format nil "the count is in the report; it said ~a" report))
               (is (null (search "osv was not asked" report)) (format nil "no note; the report ~a" report))))))
      (forget-scans))))

(deftest bumblebee-cell-a-failed-osv-call-is-a-note-not-a-failed-scan ()
  (with-cell-stop ((bumblebee-start))
    (forget-scans)
    (unwind-protect
         (with-bumblebee-tree (root (:npm "event-stream" "3.3.6"))
           (let ((nodecode-bumblebee::*http* (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (values "upstream is down" 503 nil))))
             (let ((report (bumblebee:scan (namestring root))))
               (is (search "[critical] npm/event-stream@3.3.6" report) "the curated finding still stands")
               (is (search "503" report) "and the advisory layer says why it has none"))))
      (forget-scans))))

;;; --- refusals and the tools ----------------------------------------------------------

(deftest bumblebee-cell-a-scan-with-no-folder-refuses-naming-the-setting ()
  (with-quiet-cells
    (is (search "bumblebee.roots"
                (refusal-text nodecode-bumblebee:bumblebee-error (bumblebee:scan))) "with nothing named and no root set, the refusal names the setting")
    (is (search "no scan yet"
                (refusal-text nodecode-bumblebee:bumblebee-error (bumblebee:status))) "and a status before any scan says how to make one")))

(deftest bumblebee-cell-the-tools-answer-through-the-dispatch-seam (with-session-store ("s1"))
  (with-quiet-cells
    (with-bumblebee-tree (root (:npm "event-stream" "3.3.6"))
      (let ((result (execute-wire-call "call-1" "bumblebee_guard_scan"
                                       (format nil "{\"path\": ~s, \"osv\": false}"
                                               (namestring root)))))
        (is (equal "tool" (gethash "role" result)))
        (is (search "[critical] npm/event-stream@3.3.6"
                    (princ-to-string (nle::message-content result))) "the tool answers the report itself"))
      (let ((status (execute-wire-call "call-2" "bumblebee_guard_status" "{\"preview\": true}")))
        (is (search "last scan" (princ-to-string (nle::message-content status)))))
      (let ((refused (execute-wire-call "call-3" "bumblebee_guard_diff" "{\"action\": \"nope\"}")))
        (is (search "contract_refused" (princ-to-string (nle::message-content refused))) "an action outside the declared set is refused before the body runs")))))
