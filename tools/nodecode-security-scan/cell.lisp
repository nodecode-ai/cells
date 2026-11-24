;;;; cell.lisp --- a security review as facts on disk, its nine tools, its /security, its reports.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A scan is one JSON file under the home: the folder it reviews, what the
;;;; walk found, the chunks a reviewer reads, the candidates it returned, the
;;;; coverage it claimed, the receipts that confirm or refute them, the chains
;;;; they compose, and -- once it is sealed -- the four reports beside it.
;;;; The file is the record, so a scan outlives a restart and a second
;;;; process reads the same one. Nothing here runs the code it reviews: the
;;;; tools read files, and the one network call is OSV, a package query.
;;;;
;;;; The tools are the original's nine, by name and argument: start, get,
;;;; checkpoint, submit_worker_result, record_validations, record_chains,
;;;; finalize, cancel, export. The command is /security. What a finding
;;;; carries is what the original's report carries: a rule, a severity, the
;;;; exact files and lines, its evidence state, its taxonomy, its validation
;;;; status and a stable fingerprint -- never a secret's value.

(in-package #:nodecode-security-scan)

;;; --- what the cell runs on ------------------------------------------------------

(defun settings ()
  "The section's typed settings, or the refusal that says why there are none."
  (or (running-settings)
      (fail "security-scan is not running: there is no security-scan section to read; add one to ~~/.nodecode/config.jsonc and (restart-cells \"nodecode-security-scan\")")))

(defvar *http* #'nlk:http
  "The cell's one network call, NLK:HTTP; a test binds another.")

;;; --- a record as a wire object, and back ----------------------------------------
;;; A plist is an object, a list an array, a keyword its lowercase name,
;;; NIL and T the two JSON booleans; the other way every key comes back as
;;; the kebab-case keyword of its wire name. One conversion, both directions,
;;; so the file on disk and the value in the image never disagree.

(defun wire-name (key)
  (substitute #\_ #\- (string-downcase (string key))))

(defun key-name (name)
  (intern (substitute #\- #\_ (string-upcase name)) :keyword))

(defun to-wire (value)
  "VALUE as JSON-OBJECT builds it."
  (cond ((null value) nil)
        ((eq value t) t)
        ((keywordp value) (string-downcase (symbol-name value)))
        ((hash-table-p value) value)
        ((and (consp value) (keywordp (first value)))
         (apply #'nlk:make-json-object
                (loop for (key item) on value by #'cddr
                      append (list (wire-name key) (to-wire item)))))
        ((listp value) (coerce (mapcar #'to-wire value) 'vector))
        (t value)))

(defun from-wire (value)
  "VALUE as the plists and lists this cell runs on."
  (cond ((hash-table-p value)
         (loop for key being the hash-keys of value using (hash-value item)
               append (list (key-name key) (from-wire item))))
        ((vectorp value) (map 'list #'from-wire value))
        (t value)))

(defun decoded (body)
  "BODY as an object: a decoded answer, or JSON text read once."
  (cond ((hash-table-p body) body)
        ((and (stringp body) (plusp (length body))) (ignore-errors (nlk:decode-json body)))
        (t nil)))

(defun iget (object key)
  "KEY of OBJECT, a wire object (a hash table with string keys) or a plist."
  (cond ((hash-table-p object) (gethash key object))
        ((and (consp object) (keywordp (first object))) (getf object (key-name key)))
        ((consp object) (getf object key))
        (t nil)))

(defun istr (object key &optional (default ""))
  (let ((value (iget object key)))
    (if (stringp value) value default)))

(defun iint (object key &optional default)
  (let ((value (iget object key)))
    (if (integerp value) value default)))

(defun ibool (object key &optional default)
  (let ((value (iget object key)))
    (if (null value) default t)))

(defun iarray (object key)
  "KEY of OBJECT as a list, whatever it arrived as."
  (let ((value (iget object key)))
    (cond ((vectorp value) (coerce value 'list))
          ((and (consp value) (not (keywordp (first value)))) value)
          ((and (consp value) (keywordp (first value))) (list value))
          (t nil))))

(defun objects (value)
  "VALUE as a list of objects, from a vector, a list of them, or one."
  (cond ((vectorp value) (coerce value 'list))
        ((null value) nil)
        ((and (consp value) (keywordp (first value))) (list value))
        ((consp value) value)
        (t nil)))

;;; --- names, time, text ----------------------------------------------------------

(defun now ()
  "The current UTC instant as the reports write it."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defvar *stamp-counter* 0
  "How many scans this process has named; the counter keeps two in one second apart.")

(defun stamp ()
  "Six lowercase hex digits, so two scans in one second still differ."
  (string-downcase
  (subseq (nlk:sha256-text (format nil "~a-~a" (now) (incf *stamp-counter*))) 7 13)))

(defun scan-id ()
  (format nil "scan-~a-~a" (subseq (now) 0 10) (stamp)))

(defun scan-id-p (id)
  (and (stringp id) (<= 6 (length id) 80)
       (every (lambda (c) (or (and (< (char-code c) 128) (alphanumericp c)) (find c "-._"))) id)))

(defun slug (text)
  "TEXT as a rule id's tail: lowercase, dashes, nothing else."
  (let ((out (make-string-output-stream)))
    (loop with dash = nil
          for c across (string-downcase (or text ""))
          do (cond ((or (and (< (char-code c) 128) (alphanumericp c)))
                    (write-char c out) (setf dash nil))
                   ((not dash) (write-char #\- out) (setf dash t))))
    (let ((text (string-trim "-" (get-output-stream-string out))))
      (if (plusp (length text)) (subseq text 0 (min 60 (length text))) "finding"))))

(defun clip (text limit)
  (let ((text (or text "")))
    (if (> (length text) limit) (format nil "~a..." (subseq text 0 (- limit 3))) text)))

(defun one-line (text)
  (with-output-to-string (out)
    (loop for c across (or text "") do (write-char (if (member c '(#\Newline #\Return #\Tab)) #\Space c) out))))

;;; --- the store ------------------------------------------------------------------

(defvar *scans* (make-hash-table :test #'equal)
  "Every scan this process has read or written, by id; the file is the record.")

(defun scan-path (id)
  (nlk:home (format nil "security-scans/~a/scan.json" id)))

(defun scan-dir (id)
  (nlk:home (format nil "security-scans/~a/" id)))

(defun save-scan (scan)
  (setf (gethash (getf scan :id) *scans*) scan)
  (nlk:write-file-atomically (scan-path (getf scan :id))
                             (nlk:encode-json-object (to-wire scan))
                             :mode #o600 :directory-mode #o700)
  scan)

(defun find-scan (id)
  (when (scan-id-p id)
    (or (gethash id *scans*)
        (let ((file (scan-path id)))
          (when (probe-file file)
            (setf (gethash id *scans*)
                  (from-wire (nlk:decode-json (uiop:read-file-string file)))))))))

(defun scan-or-fail (id)
  (or (find-scan id)
      (fail "no scan ~s: security_scan_start answers the id of the scan it makes, and security_scan_get section summary lists the work" id)))

(defun scan-or-fail-open (id)
  "A scan that may still be changed."
  (let ((scan (scan-or-fail id)))
    (when (equal (getf scan :status) "sealed")
      (fail "scan ~a is sealed: its reports are final and later triage does not change them; start a new scan" id))
    (when (equal (getf scan :status) "cancelled")
      (fail "scan ~a was cancelled: start a new scan" id))
    scan))

(defun note (scan text)
  "TEXT on the scan's activity, newest last, at most 200 kept."
  (let* ((activity (append (getf scan :activity) (list (list :at (now) :text (clip (one-line text) 400)))))
         (kept (if (> (length activity) 200) (subseq activity (- (length activity) 200)) activity)))
    (setf (getf scan :activity) kept)))

;;; --- the walk -------------------------------------------------------------------

(defparameter +ignored-dirs+
  '(".git" ".hg" ".svn" ".idea" ".vscode" ".cache" ".venv" "venv" "__pycache__" "node_modules"
    "vendor" "third_party" "bower_components" "target" "dist" "build" "site-packages" ".next")
  "Folders a review never reads: another project's copy, or output.")

(defparameter +vendored-words+ '("test" "tests" "spec" "specs" "__tests__" "testing" "fixtures" "fixture"
                                 "examples" "example" "docs" "doc" "evals" "benchmarks")
  "Path words a standard scan skips and a deep scan reads.")

(defparameter +doc-suffixes+ '("md" "markdown" "rst" "adoc" "txt" "png" "jpg" "jpeg" "gif" "svg" "ico"
                               "pdf" "lock" "csv" "map" "min.js" "min.css")
  "Files a standard scan skips and a deep scan reads.")

(defparameter +max-bytes+ (* 8 1024 1024)
  "A file over this is skipped and named in the report, as the original's walk did.")

(defun ignored-dir-p (name)
  (or (find name +ignored-dirs+ :test #'string=) (uiop:string-prefix-p "." name)))

(defun relative (root file)
  (string-right-trim "/" (namestring (uiop:enough-pathname file root))))

(defun file-bytes (file)
  "FILE's size in octets, or 0 when it cannot be read."
  (handler-case (with-open-file (in file :element-type '(unsigned-byte 8))
                  (file-length in))
    (error () 0)))

(defun read-text (file)
  "FILE's text, or NIL when it is not text a review can quote."
  (handler-case
      (let ((text (uiop:read-file-string file :external-format :utf-8)))
        (if (find #\Null text) nil text))
    (error () nil)))

(defun line-count (text)
  (1+ (count #\Newline text)))

(defun standard-skip (path)
  "Why a standard scan skips PATH, or NIL."
  (let ((segments (uiop:split-string path :separator "/"))
        (name (car (last (uiop:split-string path :separator "/")))))
    (cond ((some (lambda (segment) (member (string-downcase segment) +vendored-words+ :test #'string=))
                 (butlast segments 1))
           "a test, example, doc or eval folder")
          ((some (lambda (suffix) (uiop:string-suffix-p (string-downcase name) suffix)) +doc-suffixes+)
           "a document, asset or lockfile")
          (t nil))))

(defun walk (root &key deep)
  "Every file of ROOT a review reads, in path order, and every one it skips with
the reason. => (values FILES SKIPPED), a file (:path REL :lines N)."
  (let ((files '()) (skipped '()))
    (labels ((skip (path reason) (push (list :path path :reason reason) skipped))
             (visit (dir)
               (dolist (sub (sort (uiop:subdirectories dir) #'string< :key #'namestring))
                 (let ((name (car (last (pathname-directory sub)))))
                   (if (ignored-dir-p name)
                       (skip (relative root sub) "another project's copy or generated output")
                       (visit sub))))
               (dolist (file (sort (uiop:directory-files dir) #'string< :key #'namestring))
                 (let ((path (relative root file)))
                   (cond
                     ((and (not deep) (standard-skip path)) (skip path (standard-skip path)))
                     ((> (file-bytes file) +max-bytes+) (skip path "over 8 MB"))
                     (t (let ((text (read-text file)))
                          (if (null text)
                              (skip path "not text")
                              (push (list :path path :lines (line-count text)) files)))))))))
      (visit root))
    (values (sort files #'string< :key (lambda (file) (getf file :path))) (nreverse skipped))))

(defun chunk-files (files max-lines)
  "FILES grouped into chunks of about MAX-LINES, in order."
  (let ((chunks '()) (current '()) (lines 0) (n 0))
    (labels ((flush ()
               (when current
                 (push (list :id (format nil "C~d" (incf n))
                             :paths (mapcar (lambda (file) (getf file :path)) (nreverse current))
                             :lines lines :state "planned")
                       chunks)
                 (setf current '() lines 0))))
      (dolist (file files)
        (when (and current (> (+ lines (getf file :lines)) max-lines)) (flush))
        (push file current)
        (incf lines (getf file :lines)))
      (flush))
    (nreverse chunks)))

(defun changed-files (root)
  "The paths git reports changed in ROOT, for a diff scan; refuses when git
cannot answer for the folder."
  (let* ((output (handler-case
                     (uiop:run-program (list "git" "-C" (namestring root) "status" "--porcelain=v1"
                                              "--untracked-files=all")
                                       :output :string :error-output :string)
                   (error (condition)
                     (fail "diff mode needs git to list the changes in ~a: ~a; use standard or deep"
                           (namestring root) (one-line (princ-to-string condition))))))
         (paths '()))
    (loop for line in (nlk:lines output)
          when (> (length line) 3)
            do (let ((path (subseq line 3)))
                 (when (and (> (length path) 2) (char= (char path 0) #\"))
                   (setf path (subseq path 1 (max 1 (1- (length path))))))
                 (push path paths)))
    (nreverse paths)))

;;; --- the dependency check, the one network call ----------------------------------

(defun clean-version (version)
  "VERSION with the range punctuation off, or NIL when it names no release."
  (let* ((version (string-trim " ^~>=<v" (or version "")))
         (version (car (uiop:split-string version :separator " |")))
         (version (and version (car (uiop:split-string version :separator "-")))))
    (and version (plusp (length version)) (digit-char-p (char version 0)) version)))

(defun json-packages (root file ecosystem keys)
  (let ((path (uiop:subpathname* root file)))
    (when (probe-file path)
      (let ((json (ignore-errors (nlk:decode-json (uiop:read-file-string path)))))
        (when (hash-table-p json)
          (loop for key in keys
                for table = (gethash key json)
                when (hash-table-p table)
                  append (loop for name being the hash-keys of table using (hash-value version)
                               for clean = (clean-version version)
                               when clean collect (list :ecosystem ecosystem :name name :version clean))))))))

(defun requirements-packages (root)
  (let ((path (uiop:subpathname* root "requirements.txt")))
    (when (probe-file path)
      (loop for line in (nlk:lines (uiop:read-file-string path))
            for clean = (string-trim " " line)
            when (and (plusp (length clean))
                      (not (member (char clean 0) '(#\# #\-)))
                      (search "==" clean))
              collect (list :ecosystem "PyPI"
                            :name (string-trim " " (subseq clean 0 (search "==" clean)))
                            :version (string-trim " " (subseq clean (+ 2 (search "==" clean)))))))))

(defun declared-packages (root)
  (append (json-packages root "package.json" "npm" '("dependencies" "devDependencies"))
          (json-packages root "composer.json" "Packagist" '("require" "require-dev"))
          (requirements-packages root)))

(defun advisories-of (package answer)
  "The vulns ANSWER lists for PACKAGE, as (name, id, summary)."
  (loop for vuln in (objects (iget answer "vulns"))
        collect (list :package (getf package :name)
                      :id (istr vuln "id" "unknown")
                      :summary (clip (one-line (istr vuln "summary" "no summary")) 200))))

(defun check-dependencies (root)
  "Read the manifests under ROOT and ask OSV about them. Never fails the scan:
a check that cannot run is one line the report carries."
  (let ((packages (declared-packages root)))
    (cond
      ((null packages)
       (list :state "none" :packages 0 :advisories '() :note "no lockfile or manifest this cell reads"))
      (t
       (handler-case
           (let* ((url (format nil "~a/v1/querybatch" (string-right-trim "/" (setting :osv-url))))
                  (body (nlk:encode-json-object
                         (nlk:json-object
                          "queries" (coerce (mapcar (lambda (package)
                                                       (nlk:json-object
                                                        "package" (nlk:json-object
                                                                   "name" (getf package :name)
                                                                   "ecosystem" (getf package :ecosystem))
                                                        "version" (getf package :version)))
                                                     packages)
                                             'vector))))
                  (answer (funcall *http* :post url :content body :timeout 20
                                        :headers '(("content-type" . "application/json"))))
                  (json (decoded (if (listp answer) (first answer) answer))))
             (if (null json)
                 (list :state "failed" :packages (length packages) :advisories '()
                       :note "OSV answered nothing this cell could read")
                 (list :state "ok" :packages (length packages)
                       :advisories (loop for package in packages
                                         for result in (objects (iget json "results"))
                                         append (advisories-of package result))
                       :note "")))
         (error (condition)
           (list :state "failed" :packages (length packages) :advisories '()
                 :note (clip (one-line (princ-to-string condition)) 300))))))))

;;; --- findings -------------------------------------------------------------------

(defparameter +severities+ '("critical" "high" "medium" "low" "informational")
  "The original's five levels, worst first.")

(defun severity (level)
  (let ((level (string-downcase (or level ""))))
    (if (member level +severities+ :test #'string=) level "informational")))

(defun severity-rank (level)
  (or (position (severity level) +severities+ :test #'string=) 5))

(defun finding-id (scan)
  (format nil "F~d" (1+ (length (getf scan :findings)))))

(defun locations-of (candidate)
  (loop for location in (objects (iget candidate "locations"))
        for path = (istr location "path")
        when (plusp (length path))
          collect (list :path path
                        :start-line (or (iint location "startLine") 1)
                        :end-line (or (iint location "endLine") (iint location "startLine") 1))))

(defun candidate-finding (scan candidate)
  (let* ((title (clip (one-line (istr candidate "title" "a candidate the review returned")) 200))
         (locations (locations-of candidate))
         (rule (istr candidate "ruleId"))
         (finding (list :id (finding-id scan)
                        :rule-id (if (plusp (length rule)) rule (format nil "nc/~a" (slug title)))
                        :title title
                        :summary (clip (one-line (istr candidate "summary" title)) 1200)
                        :severity (severity (istr candidate "severity" "informational"))
                        :locations locations
                        :evidence-state (if locations "cited" "uncited")
                        :validation "NOT_RUN"
                        :taxonomy (list :cwe (loop for cwe in (objects (iget candidate "cwe")) collect (princ-to-string cwe))
                                        :owasp (loop for owasp in (objects (iget candidate "owasp")) collect (princ-to-string owasp)))
                        :fingerprint (nlk:sha256-text (format nil "~a|~a|~a" title
                                                              (mapcar (lambda (l) (getf l :path)) locations)
                                                              (getf scan :digest))))))
    finding))

(defun reportable (scan)
  (stable-sort (copy-list (getf scan :findings))
               (lambda (a b) (< (severity-rank (getf a :severity)) (severity-rank (getf b :severity))))))

(defun reviewed-chunks (scan)
  (remove-if-not (lambda (chunk) (equal (getf chunk :state) "reviewed")) (getf scan :chunks)))

(defun gaps (scan)
  "The chunks no reviewer returned a result for."
  (remove-if (lambda (chunk) (equal (getf chunk :state) "reviewed")) (getf scan :chunks)))

;;; --- the reports ----------------------------------------------------------------

(defun finding-lines (finding)
  (let ((locations (getf finding :locations)))
    (if locations
        (format nil "~{~a~^, ~}" (mapcar (lambda (l) (format nil "`~a:~a`" (getf l :path) (getf l :start-line))) locations))
        "no location")))

(defun report-markdown (scan)
  "SCAN as the Markdown report the original's workers hand a person."
  (with-output-to-string (out)
    (format out "# Security scan ~a~2%" (getf scan :id))
    (format out "- Repository: `~a`~%- Mode: ~a~%- Started: ~a~%- Status: ~a~%"
            (getf scan :path) (getf scan :mode) (getf scan :created-at)
            (or (getf scan :status) "open"))
    (format out "- Snapshot: ~a~%- Files read: ~d of ~d~%- Chunks reviewed: ~d of ~d~%"
            (getf scan :digest)
            (length (getf scan :files)) (+ (length (getf scan :files)) (length (getf scan :skipped)))
            (length (reviewed-chunks scan)) (length (getf scan :chunks)))
    (let ((dependencies (getf scan :dependencies)))
      (format out "- Dependency check: ~a~@[ (~d packages, ~d advisories)~]~@[ -- ~a~]~2%"
              (getf dependencies :state)
              (getf dependencies :packages)
              (length (getf dependencies :advisories))
              (let ((note (getf dependencies :note)))
                (and note (plusp (length note)) note))))
    (format out "## Findings~2%")
    (if (null (reportable scan))
        (format out "No finding was recorded.~@[ ~d chunk~:p ~a not reviewed, so this is not a clean result.~]~2%"
                (length (gaps scan)) (length (gaps scan)))
        (dolist (finding (reportable scan))
          (format out "### ~a -- ~a (~a)~2%~a~2%"
                  (getf finding :id) (getf finding :title) (getf finding :severity)
                  (getf finding :summary))
          (format out "- Rule: ~a~%- Evidence: ~a~%- Evidence state: ~a~%- Validation: ~a~%"
                  (getf finding :rule-id) (finding-lines finding)
                  (getf finding :evidence-state) (or (getf finding :validation) "NOT_RUN"))
          (let ((cwe (getf (getf finding :taxonomy) :cwe))
                (owasp (getf (getf finding :taxonomy) :owasp)))
            (when (or cwe owasp)
              (format out "- Taxonomy: ~{[~a]~^ ~}~%" (append cwe owasp))))
          (format out "~%")))
    (let ((chains (getf scan :chains)))
      (when chains
        (format out "## Chains~2%")
        (dolist (chain chains)
          (format out "- ~a -- ~a: ~a (~{~a~^, ~})~%" (getf chain :id) (getf chain :title)
                  (getf chain :summary) (mapcar (lambda (f) (getf f :id)) (getf chain :findings))))
        (format out "~%")))
    (let ((skipped (getf scan :skipped)))
      (when skipped
        (format out "## What was not read~2%")
        (dolist (entry (subseq skipped 0 (min 40 (length skipped))))
          (format out "- `~a`: ~a~%" (getf entry :path) (getf entry :reason)))
        (when (> (length skipped) 40)
          (format out "- and ~d more~%" (- (length skipped) 40)))
        (format out "~%")))
    (format out "## How to read this~2%")
    (format out "A review is the model's reading of the code, so check its conclusions. A finding marked~%CONFIRMED was proved by a test; NOT_RUN is no proof either way. Zero findings with~%unreviewed chunks does not mean the code is clean.~%")))

(defun locations-json (finding)
  (coerce (mapcar (lambda (location)
                     (nlk:json-object "path" (getf location :path)
                                      "startLine" (getf location :start-line)
                                      "endLine" (getf location :end-line)))
                   (getf finding :locations))
          'vector))

(defun finding-json (finding)
  (nlk:json-object
   "findingId" (getf finding :id)
   "ruleId" (getf finding :rule-id)
   "title" (getf finding :title)
   "summary" (getf finding :summary)
   "severity" (nlk:json-object "level" (getf finding :severity))
   "evidenceState" (getf finding :evidence-state)
   "locations" (locations-json finding)
   "taxonomy" (nlk:json-object "cwe" (coerce (getf (getf finding :taxonomy) :cwe) 'vector)
                                "owasp" (coerce (getf (getf finding :taxonomy) :owasp) 'vector))
   "validation" (nlk:json-object "status" (or (getf finding :validation) "NOT_RUN")
                                  "note" (or (getf finding :validation-note) ""))
   "fingerprints" (nlk:json-object "primary" (getf finding :fingerprint))))

(defun findings-json (scan)
  "The canonical findings document, one object."
  (nlk:json-object
   "format" 1
   "scanId" (getf scan :id)
   "status" (or (getf scan :status) "open")
   "snapshotDigest" (getf scan :digest)
   "producer" (nlk:json-object "name" "nodecode-security-scan" "version" "0.1.0")
   "findings" (coerce (mapcar #'finding-json (reportable scan)) 'vector)))

(defparameter +sarif-levels+ '(("critical" . "error") ("high" . "error") ("medium" . "warning")
                               ("low" . "note") ("informational" . "note")))

(defparameter +sarif-scores+ '(("critical" . 9.5) ("high" . 8.0) ("medium" . 5.0)
                               ("low" . 2.0) ("informational" . 0.0)))

(defun rule-ids (scan)
  (sort (remove-duplicates (mapcar (lambda (finding) (getf finding :rule-id)) (reportable scan))
                           :test #'string=)
        #'string<))

(defun sarif-rules (scan)
  (coerce
   (loop for rule in (rule-ids scan)
         for group = (remove-if-not (lambda (finding) (string= rule (getf finding :rule-id)))
                                    (reportable scan))
         collect (nlk:json-object
                  "id" rule
                  "shortDescription" (nlk:json-object "text" (getf (first group) :title))
                  "properties" (nlk:json-object
                                "tags" (coerce (sort (remove-duplicates
                                                      (cons "security" (cons "security-severity"
                                                                             (loop for finding in group
                                                                                   append (getf (getf finding :taxonomy) :cwe))))
                                                      :test #'string=)
                                                     #'string<)
                                               'vector)
                                "security-severity"
                                (princ-to-string (loop for finding in group
                                                       maximize (or (cdr (assoc (getf finding :severity)
                                                                                 +sarif-scores+ :test #'string=))
                                                                    0.0))))))
   'vector))

(defun sarif-result (scan finding)
  (nlk:json-object
   "ruleId" (getf finding :rule-id)
   "ruleIndex" (position (getf finding :rule-id) (rule-ids scan) :test #'string=)
   "level" (or (cdr (assoc (getf finding :severity) +sarif-levels+ :test #'string=)) "note")
   "message" (nlk:json-object "text" (getf finding :summary))
   "locations" (coerce (mapcar (lambda (location)
                                  (nlk:json-object
                                   "physicalLocation"
                                   (nlk:json-object
                                    "artifactLocation" (nlk:json-object "uri" (getf location :path))
                                    "region" (nlk:json-object "startLine" (getf location :start-line)
                                                               "endLine" (getf location :end-line)))))
                                (getf finding :locations))
                        'vector)
   "partialFingerprints" (nlk:json-object "nodecodeSecurity/v1" (getf finding :fingerprint))
   "properties" (nlk:json-object "evidenceState" (getf finding :evidence-state)
                                  "findingId" (getf finding :id)
                                  "severity" (getf finding :severity))))

(defun sarif-json (scan)
  (nlk:json-object
   "$schema" "https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/schemas/sarif-schema-2.1.0.json"
   "version" "2.1.0"
   "runs" (coerce
           (list (nlk:json-object
                  "tool" (nlk:json-object
                          "driver" (nlk:json-object
                                    "name" "nodecode-security-scan"
                                    "version" "0.1.0"
                                    "rules" (sarif-rules scan)))
                  "results" (coerce (mapcar (lambda (finding) (sarif-result scan finding))
                                             (reportable scan))
                                    'vector)
                  "properties" (nlk:json-object "scanId" (getf scan :id)
                                                 "status" (or (getf scan :status) "open"))))
           'vector)))

(defun csv-cell (text)
  "TEXT quoted for CSV, and a value starting a formula prefixed so a
spreadsheet cannot run it."
  (let ((text (or text "")))
    (when (and (plusp (length text))
               (member (char (string-left-trim '(#\Space #\Tab) text) 0) '(#\= #\+ #\- #\@)))
      (setf text (concatenate 'string "'" text)))
    (format nil "\"~a\"" (with-output-to-string (out)
                           (loop for c across text
                                 do (if (char= c #\") (write-string "\"\"" out) (write-char c out)))))))

(defun findings-csv (scan)
  (with-output-to-string (out)
    (format out "~{~a~^,~}~%"
            (mapcar #'csv-cell '("findingId" "title" "severity" "evidenceState" "path" "startLine"
                                  "cwe" "owasp" "validation")))
    (dolist (finding (reportable scan))
      (let ((locations (or (getf finding :locations) (list (list :path "" :start-line "")))))
        (dolist (location locations)
          (format out "~{~a~^,~}~%"
                  (mapcar #'csv-cell
                          (list (getf finding :id) (getf finding :title) (getf finding :severity)
                                (getf finding :evidence-state) (getf location :path)
                                (princ-to-string (getf location :start-line))
                                (format nil "~{~a~^;~}" (getf (getf finding :taxonomy) :cwe))
                                (format nil "~{~a~^;~}" (getf (getf finding :taxonomy) :owasp))
                                (or (getf finding :validation) "NOT_RUN")))))))))

(defun write-reports (scan)
  "The four reports beside the record. => the folder."
  (let ((dir (scan-dir (getf scan :id))))
    (nlk:write-file-atomically (uiop:subpathname* dir "report.md") (report-markdown scan)
                               :mode #o600 :directory-mode #o700)
    (nlk:write-file-atomically (uiop:subpathname* dir "findings.json")
                               (nlk:encode-json-object (findings-json scan))
                               :mode #o600 :directory-mode #o700)
    (nlk:write-file-atomically (uiop:subpathname* dir "results.sarif")
                               (nlk:encode-json-object (sarif-json scan))
                               :mode #o600 :directory-mode #o700)
    (nlk:write-file-atomically (uiop:subpathname* dir "findings.csv") (findings-csv scan)
                               :mode #o600 :directory-mode #o700)
    (namestring dir)))
