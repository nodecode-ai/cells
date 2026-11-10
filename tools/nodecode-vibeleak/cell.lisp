;;;; cell.lisp --- the four tools, /vibeleak, and the one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original gave a user two things in a desktop page: an assessment of a
;;;; website they own, and a read-only review of a local folder. Both are here
;;;; as tools the model calls and one command an operator types. The desktop
;;;; page, its globe and its dialogs are the part a folder cannot carry; the
;;;; approval it enforced is the call itself, which names the host and spends
;;;; one scan unit.

(in-package #:nodecode-vibeleak)

(defparameter +manual+
  "VibeLeak is on through the nodecode-vibeleak cell. It does two things.

A LOCAL REVIEW, and never the network: vibeleak_review walks one folder and
answers review candidates -- PHP request input reaching eval(), unserialize()
or a command function; an upload-progress or display_errors setting; a
provider key shape in readable source; an .env ignore policy worth a look --
each with the file and line it came from, plus a coverage block counting
every file not read and why. Sensitive files (.env, credentials, private
keys) are skipped by name and never opened. Nothing is executed and nothing
is deployed; a candidate is not a confirmed vulnerability. /vibeleak reads
the configured root and prints the few lines worth reading first.

An OWNER-APPROVED ASSESSMENT of a website you own: vibeleak_scan POSTs one
host to VibeLeak under one scan unit and needs an owner API key (Pro plan,
scopes scan:write, scan:read, findings:read) in the api_key setting --
vibeleak_report reads that saved scan's evidence by its id, and
vibeleak_compare reads two scans of one host and answers what persisted,
what is newly observed and what was not observed again. The only origin
contacted is the origin setting (www.vibeleak.app by default). See
(help :vibeleak) while the cell runs."
  "The primer the model reads; the tools' descriptions say the same.")

;;; --- the local review ---------------------------------------------------------

(defun resolve-root (text)
  "TEXT as a directory pathname: a leading `~' resolved, a relative path
against this process's directory."
  (let* ((text (string-trim '(#\Space #\Tab) (or text "")))
         (text (if (and (>= (length text) 2) (char= #\~ (char text 0)) (char= #\/ (char text 1)))
                   (namestring (merge-pathnames (subseq text 2) (user-homedir-pathname)))
                   text)))
    (uiop:ensure-directory-pathname (uiop:ensure-absolute-pathname text (uiop:getcwd)))))

(defun review-root-of (request)
  "The folder REQUEST names, else the configured root; a refusal when there
is neither."
  (let* ((given (string-trim '(#\Space #\Tab) (or (local-review-path request) "")))
         (configured (setting :root)))
    (cond ((plusp (length given)) (resolve-root given))
          (configured (resolve-root configured))
          (t (fail "name a folder to review, or set \"root\" in the vibeleak section")))))

(defun finding-json (finding)
  (nlk:make-json-object
   "rule" (finding-rule finding) "title" (finding-title finding)
   "severity" (string-downcase (symbol-name (finding-severity finding)))
   "file" (finding-file finding) "line" (finding-line finding)
   "module" (finding-module finding) "category" (finding-category finding)
   "certainty" (finding-certainty finding)
   "evidence" (finding-evidence finding)
   "remediation" (finding-remediation finding)))

(defun coverage-json (coverage)
  (let ((plist (copy-list coverage)))
    (nlk:make-json-object
     "entries_seen" (getf plist :entries-seen 0)
     "files_read" (getf plist :files-read 0)
     "bytes_read" (getf plist :bytes-read 0)
     "skipped" (getf plist :skipped 0)
     "files_skipped" (getf plist :files-skipped 0)
     "directories_skipped" (getf plist :directories-skipped 0)
     "omitted_findings" (getf plist :omitted-findings 0)
     "truncated" (and (getf plist :truncated) t)
     "reasons" (coerce (loop for (reason . count) in (getf plist :reasons)
                              collect (nlk:make-json-object "reason" (string reason)
                                                             "count" count))
                        'vector))))

(defun review-json (report)
  (nlk:make-json-object
   "root" (namestring (review-root report))
   "project" (car (last (pathname-directory (review-root report))))
   "findings" (coerce (mapcar #'finding-json (review-findings report)) 'vector)
   "counts" (nlk:make-json-object
              "total" (length (review-findings report))
              "high" (count :high (review-findings report) :key #'finding-severity)
              "medium" (count :medium (review-findings report) :key #'finding-severity)
              "low" (count :low (review-findings report) :key #'finding-severity))
   "coverage" (coverage-json (review-coverage report))
   "php" (nlk:make-json-object
           "detected" (and (getf (review-php report) :detected) t)
           "source_files" (getf (review-php report) :source-files 0)
           "requirement" (or (getf (review-php report) :requirement) "")
           "runtime_established" nil)
   "advisory" (nlk:make-json-object
                "guidance" (getf (assess-php) :guidance)
                "reviewed_advisories" (coerce (getf (assess-php) :advisories) 'vector))
   "privacy" (nlk:make-json-object "uploaded" nil "network_requests" 0
                                    "raw_source_stored" nil)
   "security_verified" nil
   "limitations" (coerce (review-limitations report) 'vector)))

(defun review-tool (request)
  "The vibeleak_review tool: a bounded, read-only review of one folder."
  (handler-case
      (let* ((root (review-root-of request))
             (max-files (or (and (local-review-max-files request)
                                  (plusp (local-review-max-files request))
                                  (local-review-max-files request))
                            (setting :max-files))))
        (unless (uiop:directory-exists-p root)
          (fail "~a is not a folder this process can read" (namestring root)))
        (nlk:encode-json-object (review-json (review root :max-files max-files))))
    (vibeleak-error (condition) (nle:failure (format nil "ERROR: vibeleak: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: vibeleak: ~a" condition)))))

;;; --- the owner-approved assessment ----------------------------------------------

(defun owner-key ()
  "The owner API key, or the refusal that names where it goes."
  (let ((key (setting :api-key)))
    (unless (and (stringp key) (plusp (length (string-trim '(#\Space #\Tab) key))))
      (fail
       "no owner API key: set \"api_key\" in the vibeleak section (an owner key with scan:write, scan:read and findings:read, from the VibeLeak dashboard); the local review needs no key"))
    (string-trim '(#\Space #\Tab) key)))

(defun scan-answer (scan)
  (nlk:make-json-object
   "domain" (getf scan :domain)
   "scan_id" (or (getf scan :scan-id) "")
   "grade" (or (getf scan :grade) "")
   "score" (or (getf scan :score) 0)
   "failed" (and (getf scan :failed) t)
   "findings" (coerce (loop for finding in (getf scan :findings)
                             collect (nlk:make-json-object
                                      "title" (or (getf finding :title) "Finding")
                                      "severity" (string-downcase (symbol-name (getf finding :severity)))
                                      "module" (or (getf finding :module) "")
                                      "category" (or (getf finding :category) "")
                                      "remediation" (or (getf finding :remediation) "")
                                      "evidence" (or (getf finding :evidence) "")))
                        'vector)
   "coverage" (if (getf scan :coverage)
                   (nlk:make-json-object
                    "status" (or (getf (getf scan :coverage) :status) "not-provided")
                    "completed" (coerce (getf (getf scan :coverage) :completed) 'vector)
                    "failed" (coerce (getf (getf scan :coverage) :failed) 'vector))
                   (nlk:make-json-object "status" "not-provided"))))

(defun scan-tool (request)
  "The vibeleak_scan tool: one host, one approved assessment, one scan unit."
  (handler-case
      (let* ((key (owner-key))
             (result (start-scan (scan-start-domain request) :key key))
             (scan (getf result :scan)))
        (nlk:encode-json-object
         (nlk:make-json-object
          "state" "completed"
          "request_id" (getf result :request-id)
          "units" 1
          "scan" (scan-answer scan)
          "message" (getf result :message))))
    (vibeleak-error (condition) (nle:failure (format nil "ERROR: vibeleak: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: vibeleak: ~a" condition)))))

(defun report-tool (request)
  "The vibeleak_report tool: one saved scan's evidence, and optionally the
saved markdown report."
  (handler-case
      (let* ((key (owner-key))
             (scan (get-scan (scan-report-scan-id request) :key key))
             (markdown (when (scan-report-markdown request)
                         (report-markdown (scan-report-scan-id request) :key key))))
        (nlk:encode-json-object
         (nlk:make-json-object
          "scan" (scan-answer scan)
          "markdown" (or markdown "")
          "message" "A saved observation, not a new scan, proof of ownership or a security guarantee.")))
    (vibeleak-error (condition) (nle:failure (format nil "ERROR: vibeleak: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: vibeleak: ~a" condition)))))

(defun compare-tool (request)
  "The vibeleak_compare tool: two saved scans of one host, observed apart."
  (handler-case
      (let* ((key (owner-key))
             (before (get-scan (scan-compare-before request) :key key))
             (after (get-scan (scan-compare-after request) :key key))
             (comparison (compare-findings (getf before :findings) (getf after :findings))))
        (nlk:encode-json-object
         (nlk:make-json-object
          "before" (scan-answer before)
          "after" (scan-answer after)
          "fixes_verified" nil
          "persisting" (coerce (mapcar (lambda (finding) (or (getf finding :title) ""))
                                        (getf comparison :persisting)) 'vector)
          "newly_observed" (coerce (mapcar (lambda (finding) (or (getf finding :title) ""))
                                            (getf comparison :newly-observed)) 'vector)
          "not_observed" (coerce (mapcar (lambda (finding) (or (getf finding :title) ""))
                                          (getf comparison :not-observed)) 'vector)
          "message" (getf comparison :message))))
    (vibeleak-error (condition) (nle:failure (format nil "ERROR: vibeleak: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: vibeleak: ~a" condition)))))

;;; --- /vibeleak ---------------------------------------------------------------

(defun review-text (report)
  "The review as the few lines an operator reads first: what was read, the
worst candidates, and what was not read."
  (let* ((findings (review-findings report))
         (coverage (review-coverage report)))
    (with-output-to-string (out)
      (format out "vibeleak review -- ~a~%" (namestring (review-root report)))
      (format out "  ~d candidate(s) in ~d file(s) read, ~d entries skipped~a~%"
              (length findings) (getf coverage :files-read 0) (getf coverage :skipped 0)
              (if (getf coverage :truncated) ", coverage bounded" ""))
      (dolist (finding (if (> (length findings) 8) (subseq findings 0 8) findings))
        (format out "  [~a] ~a -- ~a:~d~%"
                (string-upcase (symbol-name (finding-severity finding)))
                (finding-title finding) (finding-file finding) (finding-line finding)))
      (when (> (length findings) 8)
        (format out "  (+~d more; the tool answers all of them)~%" (- (length findings) 8)))
      (when (null findings)
        (format out "  nothing to report in the scope read; this is not a security guarantee~%"))
      (format out "  read only, no network: sensitive files were skipped, not opened.~%"))))

(defun run-command (args session-id)
  "One /vibeleak invocation: no argument reviews the configured root, an
argument reviews that folder."
  (declare (ignore session-id))
  (handler-case
      (let* ((given (string-trim '(#\Space #\Tab) (or args "")))
             (root (if (plusp (length given)) (resolve-root given)
                       (or (and (setting :root) (resolve-root (setting :root)))
                           (fail "set \"root\" in the vibeleak section, or type /vibeleak FOLDER")))))
        (unless (uiop:directory-exists-p root)
          (fail "~a is not a folder this process can read" (namestring root)))
        (review-text (review root :max-files (setting :max-files))))
    (vibeleak-error (condition) (nle:failure (format nil "ERROR: vibeleak: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: vibeleak: ~a" condition)))))

;;; --- the boundaries -----------------------------------------------------------

(nlk:define-contract local-review
  (:doc "Read-only review of the source and configuration files under one
folder: PHP request input reaching eval(), unserialize() or a command
function, an upload-progress or display_errors setting, a provider key shape
in readable source, and an .env ignore policy worth a look -- each with the
file and line it came from, plus a coverage block counting every file not
read and why. Sensitive files (.env, credentials, private keys) are skipped
by name and never opened. Reads files only: no network, nothing executed.")
  ("path" :string :default ""
   :doc "the folder to review, when it is not the configured root")
  ("max_files" :integer
   :doc "how many files this review reads at most (default: the configured max_files)"))

(nlk:define-contract scan-start
  (:doc "Ask VibeLeak for one assessment of one website the operator owns,
under exactly one scan unit, and answer its grade, its findings worst first
and its coverage. The host travels to the configured VibeLeak origin with
the owner API key in the Authorization header; nothing else leaves this
machine. Needs an owner key (Pro plan, scopes scan:write, scan:read,
findings:read) in the api_key setting.")
  ("domain" :text :required t :doc "the public hostname to assess (a website you own or are authorized to assess)")
  ("project" :string :default "" :doc "an optional short label kept beside the request"))

(nlk:define-contract scan-report
  (:doc "Read one saved VibeLeak assessment by the scan id the API issued:
its grade, its findings worst first, its coverage, and optionally the saved
markdown report. A saved observation, not a new scan and not a guarantee.")
  ("scan_id" :text :required t :doc "the saved scan's id, as a UUID")
  ("markdown" :boolean :doc "also fetch the saved markdown report (default false)"))

(nlk:define-contract scan-compare
  (:doc "Read two saved VibeLeak assessments of one host and answer what
persisted, what is newly observed and what was not observed again. Report
observations, not proof that a fix worked.")
  ("before" :text :required t :doc "the baseline scan id")
  ("after" :text :required t :doc "the later scan id of the same host"))

;;; --- the cell -----------------------------------------------------------------

(nle:define-cell vibeleak
  (:section ("vibeleak")
    (:guide "root is the folder /vibeleak reviews; an owner API key is needed only for website assessments; next step: /vibeleak")
    ("api_key" :secret
     :doc "VibeLeak owner API key: needs scan:write, scan:read and findings:read. Website assessments only; the local review needs no key.")
    ("origin" :string :default "https://www.vibeleak.app"
     :doc "the fixed VibeLeak origin the assessments go to")
    ("root" :path
     :doc "the folder a review reads when a tool call names none: /vibeleak with no argument reviews it")
    ("max_files" :integer :default 200 :min 1
     :doc "how many files one review reads at most")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one request's ceiling against the VibeLeak API"))
  (:help :vibeleak "vibeleak: an owner-approved website assessment and a read-only local PHP/source review" +manual+)
  (:command "vibeleak" 'run-command
            :description "Review a local folder for review candidates (read-only, no network)")
  (:tool "vibeleak_review" (nlk:contract 'local-review) #'review-tool)
  (:tool "vibeleak_scan" (nlk:contract 'scan-start) #'scan-tool)
  (:tool "vibeleak_report" (nlk:contract 'scan-report) #'report-tool)
  (:tool "vibeleak_compare" (nlk:contract 'scan-compare) #'compare-tool))
