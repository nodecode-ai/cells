;;;; rules.lisp --- the local review: which files are read, and what is said
;;;; about them, plus the PHP advisory assessment both workflows share.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here reads files and never connects to anything: the walk is
;;;; bounded, a sensitive file is skipped by NAME before it is opened, and a
;;;; finding cites the file and line it came from without quoting the line
;;;; back. Three properties are the original's design and are kept here:
;;;;
;;;; 1. A fixed skip list (.env, keys, credentials) so the review never
;;;;    reads a secret it did not need; its contents are never shown.
;;;; 2. Bounded coverage: entries, files, bytes, depth and findings all have
;;;;    a ceiling, and every file not read is counted under a reason.
;;;; 3. Nothing executes and nothing deploys: a finding is a review
;;;;    candidate, never a confirmed vulnerability.

(in-package #:nodecode-vibeleak)

;;; --- what the walk skips, and what it reads --------------------------------

(defparameter +skip-directories+
  '("node_modules" "vendor" ".git" ".next" ".nuxt" ".cache" ".hermes" "dist"
    "build" "coverage" "artifacts" ".turbo")
  "Dependency and generated folders: counted once, never enumerated.")

(defparameter +sensitive-name-scanner+
  (cl-ppcre:create-scanner
   "^(?:\\.env(?:\\.|$)|\\.npmrc$|\\.pypirc$|\\.netrc$|credentials$|id_rsa$|id_ed25519$|auth\\.json$)|\\.(?:pem|p12|pfx|key)$"
   :case-insensitive-mode t)
  "File names whose CONTENTS the review never reads: raw environment files,
credentials and private keys.")

(defparameter +candidate-scanner+
  (cl-ppcre:create-scanner
   "^(?:\\.gitignore$|Dockerfile$|\\.htaccess$)|\\.(?:php|ini|conf|json|[cm]?js|jsx|ts|tsx|ya?ml)$"
   :case-insensitive-mode t)
  "The source and configuration formats the review reads.")

(defparameter +secret-scanner+
  (cl-ppcre:create-scanner
   "\\b(?:sk-(?:proj-|ant-)?[A-Za-z0-9_-]{32,}|AIza[A-Za-z0-9_-]{30,}|ghp_[A-Za-z0-9]{30,}|sb_secret_[A-Za-z0-9_-]{20,})\\b")
  "A provider key shape worth a look. The value itself is never reported.")

(defun sensitive-name-p (name)
  "T when NAME is a file the review must not read the contents of."
  (and (cl-ppcre:scan +sensitive-name-scanner+ name) t))

(defun candidate-name-p (name)
  "T when NAME is a format the review reads."
  (and (cl-ppcre:scan +candidate-scanner+ name) t))

;;; --- one finding ------------------------------------------------------------

(defstruct finding
  "One review candidate. FILE is relative to the reviewed root, LINE is
where the pattern was first seen, and EVIDENCE never quotes the source."
  (rule "" :type string)
  (title "" :type string)
  (severity :medium)
  (file "" :type string)
  (line 1 :type integer)
  (module "" :type string)
  (category "" :type string)
  (certainty "review-required" :type string)
  (evidence "Content withheld. Review this file and line locally." :type string)
  (remediation "" :type string))

(defparameter +severity-rank+ '((:high . 0) (:medium . 1) (:low . 2) (:info . 3)))

(defun severity-rank (severity)
  (or (cdr (assoc severity +severity-rank+)) 9))

(defun line-number-at (text position)
  "The 1-based line of POSITION in TEXT."
  (1+ (loop for index from 0 below (min position (length text))
            count (char= #\Newline (char text index)))))

(defun match-line (scanner text)
  "The line TEXT first matches SCANNER on, or NIL."
  (let ((start (cl-ppcre:scan scanner text)))
    (and start (line-number-at text start))))

;;; --- the PHP rules ----------------------------------------------------------

(defparameter +php-request-eval+
  (cl-ppcre:create-scanner "\\beval\\s*\\(\\s*\\$_(?:GET|POST|REQUEST|COOKIE)\\b"
                            :case-insensitive-mode t))

(defparameter +php-request-unserialize+
  (cl-ppcre:create-scanner "\\bunserialize\\s*\\(\\s*\\$_(?:GET|POST|REQUEST|COOKIE)\\b"
                            :case-insensitive-mode t))

(defparameter +php-request-command+
  (cl-ppcre:create-scanner
   "\\b(?:exec|shell_exec|system|passthru)\\s*\\(\\s*\\$_(?:GET|POST|REQUEST|COOKIE)\\b"
   :case-insensitive-mode t))

(defparameter +php-session-serializer+
  (cl-ppcre:create-scanner "\\bini_set\\s*\\(\\s*['\"]session\\.serialize_handler['\"]"
                            :case-insensitive-mode t))

(defparameter +php-upload-progress+
  (cl-ppcre:create-scanner "session\\.upload_progress\\.enabled\\s*=\\s*(?:1|on|true)\\b"
                            :case-insensitive-mode t))

(defparameter +php-display-errors+
  (cl-ppcre:create-scanner "display_errors\\s*=\\s*(?:1|on|true)\\b"
                            :case-insensitive-mode t))

(defun strip-php-comments (text)
  "TEXT with PHP comments blanked to spaces, newlines kept, so a line number
still means the line a reviewer would open."
  (flet ((blank (string start end match-start match-end &rest rest)
           ;; CL-PPCRE calls a replacement function with the whole string and
           ;; the match's span (and more); the span is what gets blanked, so
           ;; the text keeps its length and every line number still means the
           ;; line a reviewer would open.
           (declare (ignore start end rest))
           (let ((copy (subseq string match-start match-end)))
             (loop for index below (length copy)
                   unless (char= (char copy index) #\Newline)
                     do (setf (char copy index) #\Space))
             copy)))
    (let ((text (cl-ppcre:regex-replace-all "(?s)/\\*.*?\\*/" text #'blank)))
      (cl-ppcre:regex-replace-all "(?m)(?://|#)[^\\n]*" text #'blank))))

(defun scan-php (file text findings)
  "FINDINGS with the PHP rules' findings for FILE pushed on, so the caller
keeps what this answers rather than a local list nobody reads."
  (flet ((add (rule title severity scanner remediation &optional (certainty "review-required"))
           (let ((line (match-line scanner text)))
             (when line
               (push (make-finding :rule rule :title title :severity severity :file file
                                   :line line :module "php" :category "input-handling"
                                   :certainty certainty :remediation remediation)
                     findings)))))
    (add "php-request-eval" "Request input reaches eval()" :high +php-request-eval+
         "Remove dynamic code evaluation. Parse structured input, validate it and use an explicit allowlist of supported operations.")
    (add "php-request-unserialize" "Request input reaches unserialize()" :high +php-request-unserialize+
         "Avoid deserializing untrusted PHP objects. Prefer a validated data format and review the reachable call paths.")
    (add "php-request-command" "Request input reaches a command-execution function" :high +php-request-command+
         "Remove request-controlled commands. Use a fixed operation allowlist and validate the arguments; confirm the reachable route.")
    (add "php-session-serializer" "Runtime session serializer changes need review" :medium +php-session-serializer+
         "Review the PHP session advisory prerequisites and the effective upload-progress settings. This pattern alone does not confirm a vulnerability.")
    (add "php-upload-progress" "Session upload progress is enabled in a local config" :medium +php-upload-progress+
         "Confirm whether this configuration serves production and review the PHP session advisory. Local configuration is not the effective server setting."
         "runtime-confirmation-required")
    (add "php-display-errors" "PHP error display is enabled in a local config" :medium +php-display-errors+
         "Keep detailed errors out of production responses. Review the effective deployment configuration before changing this local setting.")
    findings))

;;; --- one file ---------------------------------------------------------------

(defparameter +php-file-scanner+
  (cl-ppcre:create-scanner "\\.php$" :case-insensitive-mode t)
  "A file name the PHP rules read: the extension, in any case.")

(defparameter +config-file-scanner+
  (cl-ppcre:create-scanner "\\.(?:ini|conf|ya?ml)$" :case-insensitive-mode t)
  "A configuration file name whose effective settings the PHP rules read.")

(defparameter +composer-scanner+
  (cl-ppcre:create-scanner "(?:^|/)composer\\.(?:json|lock)$")
  "The manifest whose php requirement is worth naming.")

(defun file-php-p (name)
  (and (cl-ppcre:scan +php-file-scanner+ name) t))

(defun scan-text (file text)
  "The findings one file answers, and the PHP requirement it names.
=> (values FINDINGS PHP-P PHP-REQUIREMENT)"
  (let ((name (file-namestring file))
        (findings '())
        (requirement nil))
    (when (cl-ppcre:scan +composer-scanner+ file)
      (multiple-value-bind (match groups)
          (cl-ppcre:scan-to-strings "\"php\"\\s*:\\s*\"([^\"]{1,60})\"" text)
        (declare (ignore match))
        (setf requirement (and groups (aref groups 0)))))
    (let ((code (if (file-php-p name) (strip-php-comments text) text)))
      (when (or (file-php-p name) (cl-ppcre:scan +config-file-scanner+ name))
        (setf findings (scan-php file code findings))))
    (let ((line (match-line +secret-scanner+ text)))
      (when line
        (push (make-finding :rule "source-provider-secret"
                            :title "Possible hardcoded secret in readable source"
                            :severity :high :file file :line line
                            :module "source" :category "secrets"
                            :remediation "Review the value locally without copying it into chat. If it is a real exposed credential, remove the exposure, rotate it through the provider and review access. Publishable keys are not automatically secrets.")
              findings)))
    (values (nreverse findings) (file-php-p name) requirement)))

;;; --- the walk ----------------------------------------------------------------

(defun list-files (directory)
  "Every file directly under DIRECTORY, a `.env` and a `.gitignore` among
them: a review has to see a sensitive name to skip it."
  (uiop:directory-files directory))

(defun list-directories (directory)
  "Every immediate subdirectory of DIRECTORY."
  (uiop:subdirectories directory))

(defstruct review
  "One local review: the tree read, what it found, and what it could not read."
  (root nil)
  (findings '())
  (coverage '())
  (php '())
  (limitations '()))

(defun note-exclusion (reasons reason)
  "COUNT one excluded entry under REASON."
  (incf (gethash reason reasons 0)))

(defun read-file-bounded (path limit)
  "PATH as text when it is under LIMIT bytes, else (values NIL SIZE)."
  (handler-case
      (let ((size (with-open-file (in path :element-type '(unsigned-byte 8))
                    (file-length in))))
        (if (> size limit)
            (values nil size)
            (values (uiop:read-file-string path :external-format :utf-8) size)))
    (error () (values nil nil))))

(defun relative-name (path root)
  "PATH as the name a finding cites: relative to ROOT when it sits under
it, else its own file name."
  (let* ((prefix (namestring root))
         (name (namestring path)))
    (if (and (<= (length prefix) (length name))
             (string= prefix name :end2 (length prefix)))
        (subseq name (length prefix))
        (file-namestring path))))

(defun review (root &key (max-files 200) (max-entries 12000) (max-file-bytes 262144)
                        (max-total-bytes 8388608) (max-depth 18) (max-findings 100))
  "A bounded, read-only review of the source and configuration files under
ROOT. Reads only a fixed set of formats, never a sensitive file's contents,
and counts every exclusion under a reason. => a REVIEW."
  (let* ((root (uiop:ensure-directory-pathname root))
         (findings '())
         (reasons (make-hash-table :test #'equal))
         (coverage (list :entries-seen 0 :files-read 0 :bytes-read 0 :skipped 0
                         :files-skipped 0 :directories-skipped 0 :omitted-findings 0
                         :truncated nil :env-contents-read nil))
         (pending (list (cons root 0)))
         (visited (make-hash-table :test #'equal))
         (php nil)
         (php-files 0)
         (php-requirement nil)
         (gitignore nil))
    (labels ((inc (key &optional (by 1)) (incf (getf coverage key) by))
             (skip (reason &key directory)
               (inc :skipped)
               (inc (if directory :directories-skipped :files-skipped))
               (note-exclusion reasons reason))
             (add (rows)
               (dolist (row rows)
                 (if (< (length findings) max-findings)
                     (push row findings)
                     (progn (setf (getf coverage :truncated) t)
                            (inc :omitted-findings))))))
      (loop while pending
            for (directory . depth) = (pop pending)
            unless (gethash (namestring directory) visited)
              do (setf (gethash (namestring directory) visited) t)
                 (handler-case
                    (progn
                     (dolist (entry (list-files directory))
                       (when (> (incf (getf coverage :entries-seen)) max-entries)
                         (setf (getf coverage :truncated) t)
                         (return))
                       (let ((name (file-namestring entry)))
                         (cond
                           ((sensitive-name-p name) (skip "sensitive-file"))
                           ((not (candidate-name-p name)) (skip "unsupported-file-type"))
                           ((>= (getf coverage :files-read) max-files)
                            (setf (getf coverage :truncated) t)
                            (skip "file-limit"))
                           (t
                            (multiple-value-bind (text size) (read-file-bounded entry max-file-bytes)
                              (cond
                                ((null text) (skip (if size "file-size-limit" "native-file-refusal")))
                                ((> (+ (getf coverage :bytes-read) (or size 0)) max-total-bytes)
                                 (setf (getf coverage :truncated) t)
                                 (skip "total-size-limit"))
                                (t
                                 (inc :files-read)
                                 (inc :bytes-read (or size 0))
                                 (let ((relative (relative-name entry root)))
                                   (multiple-value-bind (rows is-php requirement)
                                       (scan-text relative text)
                                     (add rows)
                                     (when is-php (setf php t))
                                     (when (file-php-p name) (incf php-files))
                                     (setf php-requirement (or requirement php-requirement))
                                     (when (string-equal name ".gitignore") (setf gitignore text))))))))))
                       (dolist (child (list-directories directory))
                         (let ((name (car (last (pathname-directory child)))))
                           (cond
                             ((member name +skip-directories+ :test #'string-equal)
                              (skip "dependency-or-generated-directory" :directory t))
                             ((>= depth max-depth)
                              (setf (getf coverage :truncated) t)
                              (skip "depth-limit" :directory t))
                             (t (setf pending (append pending (list (cons child (1+ depth)))))))))))
                    (error () (skip "native-directory-refusal" :directory t))))
      (unless (and gitignore (cl-ppcre:scan "(?m)^\\s*/?(?:\\*\\*/)?\\.env(?:[.*]|\\s*$)" gitignore))
        (add (list (make-finding :rule "env-ignore-policy"
                                 :title "Review environment-file ignore policy"
                                 :severity :low :file ".gitignore" :line 1
                                 :module "repository" :category "secrets"
                                 :remediation "Confirm that real environment files are excluded from commits and deployment output. This textual hint does not establish Git tracking state or public exposure."))))
      (make-review
       :root root
       :findings (stable-sort findings (lambda (left right)
                                          (or (< (severity-rank (finding-severity left))
                                                 (severity-rank (finding-severity right)))
                                              (and (eq (severity-rank (finding-severity left))
                                                       (severity-rank (finding-severity right)))
                                                   (string< (finding-file left) (finding-file right))))))
       :coverage (append coverage
                          (list :reasons (loop for reason being the hash-keys of reasons using (hash-value count)
                                               when (plusp count) collect (cons reason count))))
       :php (list :detected php :source-files php-files :requirement php-requirement
                  :runtime-established nil)
       :limitations '("Bounded static review of permitted files, not a complete security audit."
                      "Raw environment files, credentials and private keys were not read."
                      "A review candidate does not confirm exploitation or a deployed fix.")))))

;;; --- the PHP advisories -------------------------------------------------------

(defparameter +php-fixes+ '(((8 2) . 35) ((8 3) . 36) ((8 4) . 27) ((8 5) . 12))
  "The upstream fix each stable branch has, as the original's catalog had it.")

(defparameter +php-version-scanner+
  (cl-ppcre:create-scanner "^(?:5|7|8)\\.[0-9]{1,2}\\.[0-9]{1,3}$")
  "A stable PHP version and nothing else: 8.4.27, not 8.4.27-1ubuntu.")

(defun assess-php (&key version source (vendor-fix-attested nil))
  "The offline PHP advisory reading for VERSION. Evidence is not an
exploitation test: a version above a fix is an observation, never a proof."
  (when (and version (not (cl-ppcre:scan +php-version-scanner+ version)))
    (fail "~a is not a stable PHP version such as 8.4.27" version))
  (when (and vendor-fix-attested (not (equal source "owner-runtime")))
    (fail "a vendor backport needs owner runtime evidence"))
  (when (and vendor-fix-attested (null version))
    (fail "a vendor attestation needs a runtime version"))
  (let ((assessment "unknown") (fixed nil) (branch nil) (patch nil))
    (when version
      (destructuring-bind (major minor rest) (uiop:split-string version :separator ".")
        (setf branch (list (parse-integer major) (parse-integer minor)))
        (setf patch (parse-integer rest)))
      (let ((ceiling (cdr (assoc branch +php-fixes+ :test #'equal))))
        (if ceiling
            (progn
              (setf fixed (format nil "~d.~d.~d" (first branch) (second branch) ceiling))
              (setf assessment (if (>= patch ceiling)
                                   "upstream-fix-version-observed"
                                   "potentially-affected")))
            (setf assessment "vendor-verification-required")))
      (when vendor-fix-attested (setf assessment "vendor-fix-owner-attested")))
    (list :version version
          :evidence-source (if version (or source "unknown") "unknown")
          :version-assessment assessment
          :upstream-fixed-version fixed
          :vendor-fix-owner-attested (and vendor-fix-attested t)
          :vulnerability-confirmed nil
          :security-verified nil
          :advisories '("CVE-2026-109618" "CVE-2026-109619")
          :configuration-advisory "CVE-2026-115560 is not assessed here: the October 23 advisory also requires checking effective session upload-progress configuration and application serializer changes."
          :guidance "Confirm the serving runtime and the hosting vendor's package fixes. A public header may be missing, stale or rewritten, and a vendor backport cannot be inferred from a version. No target requests or exploit probes are made by this assessment.")))

;;; --- comparing two reports --------------------------------------------------

(defun finding-key (finding)
  "What two reports are matched by: the module, the category and the title."
  (list (or (getf finding :module) "") (or (getf finding :category) "")
        (or (getf finding :title) "")))

(defun compare-findings (before after)
  "BEFORE and AFTER as lists of finding plists: what persisted, what is new
and what was not observed again. Observations, not proof of a fix."
  (let ((old (make-hash-table :test #'equal))
        (new (make-hash-table :test #'equal)))
    (dolist (finding before) (setf (gethash (finding-key finding) old) finding))
    (dolist (finding after) (setf (gethash (finding-key finding) new) finding))
    (list :fixes-verified nil
          :persisting (loop for key being the hash-keys of new
                            when (gethash key old) collect (gethash key new))
          :newly-observed (loop for key being the hash-keys of new
                                unless (gethash key old) collect (gethash key new))
          :not-observed (loop for key being the hash-keys of old
                              unless (gethash key new) collect (gethash key old))
          :message "These are report observations, not proof that issues were fixed. Missing coverage, rubric differences, truncation or transient responses can change the results.")))
