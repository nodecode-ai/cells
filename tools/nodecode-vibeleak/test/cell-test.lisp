;;;; cell-test.lisp --- lifecycle, the local review, the API tools, the advisory.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here reads a scratch tree built by this run and calls the API
;;;; through a scripted seam: no network, no clock, no file of the operator's.
;;;; The tools are called the way a model's call reaches them, through the
;;;; real dispatch and its contract, so the schema the provider is shown and
;;;; the body that runs are one shape.

(in-package #:nodecode.test)

(vibeleak-forget-trees)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "vibeleak"
  (:help :vibeleak)
  (:command "vibeleak")
  (:running
   (is (nle::find-tool "vibeleak_review") "the local review tool is registered")
   (is (nle::find-tool "vibeleak_scan") "and the scan tool")
   (is (nle::find-tool "vibeleak_report") "and the report tool")
   (is (nle::find-tool "vibeleak_compare") "and the compare tool")
   (is (equal "https://www.vibeleak.app" (nodecode-vibeleak::setting :origin)) "the origin defaults to the one VibeLeak serves")
   (is (= 200 (nodecode-vibeleak::setting :max-files)) "and one review is bounded"))
  (:stopped
   (is (null (nle::find-tool "vibeleak_review")) "stop takes the tools back out"))
  (:refused ("max_files" 0))
  (:idle nodecode-vibeleak:vibeleak-error
         (nodecode-vibeleak::owner-key)))

;;; --- the local review --------------------------------------------------------

(defparameter +php-source+
  (format nil "<?php~%// a comment with eval($_GET['x']) that must not count~%$out = eval($_GET['code']);~%"))

(deftest vibeleak-cell-local-review-finds-request-input-in-eval ()
  (let ((root (vibeleak-tree (cons "public/index.php" +php-source+)
                             (cons "README.md" "# prose, not a candidate")
                             (cons "composer.json" "{\"require\":{\"php\":\"^8.4\"}}")
                             (cons ".gitignore" "node_modules/\n.env\n"))))
    (with-vibeleak ("root" root)
      (let* ((json (vibeleak-json (vibeleak-tool "vibeleak_review")))
             (findings (coerce (vibeleak-field json "findings") 'list))
             (rules (mapcar (lambda (row) (gethash "rule" row)) findings))
             (eval-finding (find "php-request-eval" findings
                                 :key (lambda (row) (gethash "rule" row)) :test #'equal)))
        (is (member "php-request-eval" rules :test #'equal) "the eval() candidate is reported")
        (is (equal "public/index.php" (gethash "file" eval-finding)) "with the file it came from")
        (is (= 3 (gethash "line" eval-finding)) "and the line, comments blanked not skipped")
        (is (notany (lambda (row) (search "sk-" (gethash "evidence" row))) findings) "no finding quotes the source back")
        (is (= 1 (vibeleak-field (vibeleak-field json "php") "source_files")) "the PHP file counts once")
        (is (equal "^8.4" (vibeleak-field (vibeleak-field json "php") "requirement")) "and the composer requirement is named")
        (is (equal "0" (princ-to-string (vibeleak-field (vibeleak-field json "privacy") "network_requests"))) "a local review dials nothing")))))

(deftest vibeleak-cell-local-review-skips-sensitive-files-unopened ()
  (let ((root (vibeleak-tree (cons ".env" "SECRET=sk-proj-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n")
                             (cons "keys/server.pem" "-----BEGIN PRIVATE KEY-----\n")
                             (cons "app.php" "<?php echo 1;\n"))))
    (with-vibeleak ("root" root)
      (let* ((json (vibeleak-json (vibeleak-tool "vibeleak_review")))
             (coverage (vibeleak-field json "coverage"))
             (reasons (coerce (vibeleak-field coverage "reasons") 'list)))
        (is (find "sensitive-file" reasons :key (lambda (row) (gethash "reason" row)) :test #'equal) "a sensitive name is excluded under its own reason")
        (is (= 1 (vibeleak-field coverage "files_read"))
            (format nil "and only the readable source was read (~a)" coverage))
        (is (not (vibeleak-field coverage "truncated")) "the walk finished inside its bounds")))))

(deftest vibeleak-cell-local-review-needs-a-folder ()
  (with-vibeleak ()
    (let ((text (vibeleak-tool "vibeleak_review")))
      (is (search "ERROR: vibeleak:" text) "a review with no folder refuses in words")
      (is (search "root" text) "and names the setting to fix"))))

;;; --- the owner-approved assessment --------------------------------------------

(deftest vibeleak-cell-scan-without-a-key-refuses ()
  (with-vibeleak ()
    (let ((text (vibeleak-tool "vibeleak_scan" "domain" "example.com")))
      (is (search "ERROR: vibeleak:" text) "a scan with no key refuses rather than dialling")
      (is (search "api_key" text) "and names the setting that holds it"))))

(deftest vibeleak-cell-scan-posts-one-host-under-one-unit ()
  (with-vibeleak ("api_key" "owner-key")
    (with-scripted-vibeleak (calls :answer (lambda (call)
                                             (declare (ignore call))
                                             (values (vibeleak-scan-body :grade "B"
                                                                          :extra-findings
                                                                          (list (vibeleak-finding "Request input reaches eval()" "high")))
                                                     201)))
      (let* ((json (vibeleak-json (vibeleak-tool "vibeleak_scan" "domain" "Example.COM")))
             (call (first calls))
             (scan (vibeleak-field json "scan")))
        (is (= 1 (length calls)) "one call starts one assessment")
        (is (eq :post (vibeleak-call-method call)) "a POST")
        (is (search "/api/v1/scans" (vibeleak-call-url call)) "to the scans route")
        (is (search "www.vibeleak.app" (vibeleak-call-url call)) "on the configured origin")
        (is (equal "Bearer owner-key" (vibeleak-header call "authorization")) "with the owner key in the header, and nowhere else")
        (is (search "\"maxUnits\":1" (vibeleak-call-content call)) "one scan unit, and no more")
        (is (search "https://example.com" (vibeleak-call-content call)) "the host as a URL")
        (is (equal "B" (vibeleak-field scan "grade")) "the grade the API answered")
        (is (equal 1 (length (coerce (vibeleak-field scan "findings") 'list))) "and its findings")
        (is (not (search "owner-key" (nlk:encode-json-object json))) "the key is not echoed back")))))

(deftest vibeleak-cell-compare-answers-what-changed ()
  (with-vibeleak ("api_key" "owner-key")
    (with-scripted-vibeleak (calls :answer
                                    (lambda (call)
                                      (if (search "22222222" (vibeleak-call-url call))
                                          (values (vibeleak-scan-body
                                                   :id "22222222-2222-4222-8222-222222222222"
                                                   :extra-findings (list (vibeleak-finding "X-Powered-By leaks" "low")))
                                                  200)
                                          (values (vibeleak-scan-body
                                                   :extra-findings
                                                   (list (vibeleak-finding "X-Powered-By leaks" "low")
                                                         (vibeleak-finding "Request input reaches eval()" "high")))
                                                  200))))
      (let* ((json (vibeleak-json (vibeleak-tool "vibeleak_compare"
                                                  "before" "11111111-1111-4111-8111-111111111111"
                                                  "after" "22222222-2222-4222-8222-222222222222")))
             (persisting (coerce (vibeleak-field json "persisting") 'list))
             (gone (coerce (vibeleak-field json "not_observed") 'list)))
        (is (= 2 (length calls)) "both scans are read")
        (is (equal '("X-Powered-By leaks") persisting) "what persisted is named")
        (is (equal '("Request input reaches eval()") gone) "and what was not observed again")
        (is (not (vibeleak-field json "fixes_verified")) "without claiming a fix")))))

(deftest vibeleak-cell-a-hostname-is-checked-before-it-travels ()
  (is (equal "example.com" (nodecode-vibeleak:normalize-domain "https://WWW.Example.com/path")) "a URL reads as its hostname")
  (is (signals-error nodecode-vibeleak:vibeleak-error
        (nodecode-vibeleak:normalize-domain "127.0.0.1")) "an IP literal is refused")
  (is (signals-error nodecode-vibeleak:vibeleak-error
        (nodecode-vibeleak:normalize-domain "internal.local")) "a reserved suffix is refused"))

;;; --- the advisory -----------------------------------------------------------

(deftest vibeleak-cell-the-php-advice-is-an-observation ()
  (is (equal "upstream-fix-version-observed"
             (getf (nodecode-vibeleak:assess-php :version "8.4.27") :version-assessment)) "a version at the fix is observed, not proved")
  (is (equal "potentially-affected"
             (getf (nodecode-vibeleak:assess-php :version "8.4.10") :version-assessment)) "one under it is a candidate")
  (is (equal "unknown" (getf (nodecode-vibeleak:assess-php) :version-assessment)) "and no version answers unknown")
  (is (signals-error nodecode-vibeleak:vibeleak-error
        (nodecode-vibeleak:assess-php :version "8.4.27-1ubuntu")) "a packaged version is refused, not guessed"))
