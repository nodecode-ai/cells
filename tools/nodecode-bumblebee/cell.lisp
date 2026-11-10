;;;; cell.lisp --- the supply-chain check: what is installed, what is known bad, what changed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin wrapped a separately installed scanner and ran its
;;;; shell script once per check. This rewrite reads the same facts itself:
;;;; an inventory off the disk (scan.lisp), a curated catalog of releases
;;;; that were compromised, and one OSV.dev batch call for advisory counts.
;;;; A scan is written under the home, so status and diff read one scan
;;;; later without walking anything again; a baseline is a second file, and
;;;; a diff is what appeared and what went away since it was taken.
;;;;
;;;; The model reaches it three ways: the bumblebee_guard_* tools, the
;;;; bumblebee: functions from eval, and /bumblebee for the operator. What a
;;;; findings scan says to the operator the model needs too, so a scan with
;;;; findings leaves a standing notice under this folder's key -- the rewrite
;;;; of the original's rate-limited context injection, without the rate limit
;;;; and without a hook: a notice is read from the notices block, and a clean
;;;; scan clears it, so a clean machine costs no context.

(in-package #:nodecode-bumblebee)

;;; --- the manual -------------------------------------------------------------------

(defparameter +primer+
  "Known-compromised packages among those installed here, through the nodecode-bumblebee
cell: the bumblebee_guard_scan, _status and _diff tools, and the Lisp verbs
(bumblebee:scan \"/path/to/project\"), (bumblebee:status), (bumblebee:diff \"show\") from
eval. Every verb answers a string; a refusal is ERROR: BUMBLEBEE-ERROR with the reason.
  bumblebee:scan [FOLDER]   read every package.json in a node_modules folder and every
      .dist-info folder below FOLDER, and match (ecosystem, name, version) exactly against
the curated catalog of releases compromised in their own right. FOLDER defaults to the
first bumblebee.roots entry. Reads files; runs no package manager and no shell.
  bumblebee:status          the last scan without walking anything: when it ran, how many
      packages, which findings, how many OSV advisories.
  bumblebee:diff show|set_baseline   what appeared and disappeared since the baseline.
Doctrine: a curated match is a fact about exactly these bytes and high confidence --
investigate, never auto-ignore. osv counts are advisories (vulnerabilities, not confirmed
malware): deduplicate aliases and check that the version is affected before acting. Zero
findings means clean against this catalog, NOT intrusion-free: file contents are never
scanned, and only npm and PyPI are inventoried. /bumblebee status|scan FOLDER|diff."
  "What (help :bumblebee) answers while the cell runs.")

;;; --- the settings, and where a scan is kept -------------------------------------------

(defun settings ()
  "The section's typed settings, or the refusal that says there are none."
  (or (running-settings)
      (fail "bumblebee is not running: there is no bumblebee section to read; add one to the config and (restart-cells \"nodecode-bumblebee\")")))

(defun summary-file ()
  (nlk:home "bumblebee/summary.json"))

(defun baseline-file ()
  (nlk:home "bumblebee/baseline.json"))

(defun read-json-file (path)
  "PATH as a JSON object, or NIL when it is not there or unreadable."
  (when (probe-file path)
    (handler-case (nlk:decode-json (uiop:read-file-string path))
      (error () nil))))

(defun write-json-file (path object)
  (nlk:write-file-atomically path (nlk:encode-json-object object)
                             :mode #o600 :directory-mode #o700)
  path)

(defun field (object type key)
  (and object (nlk:json-value object type key)))

(defun summary-findings (summary)
  (or (field summary :array "findings") #()))

(defun finding-object (finding)
  (nlk:json-object "ecosystem" (getf finding :ecosystem)
                   "name" (getf finding :name)
                   "version" (getf finding :version)
                   "path" (getf finding :path)
                   "severity" (getf finding :severity)
                   "catalog" (getf finding :catalog)
                   "why" (getf finding :why)))

(defun finding-key (finding)
  "One finding as the string a diff compares: ecosystem/name@version."
  (format nil "~a/~a@~a" (field finding :string "ecosystem")
          (field finding :string "name") (field finding :string "version")))

(defun keys-of (findings)
  (sort (remove-duplicates (loop for finding across findings collect (finding-key finding))
                           :test #'string=)
        #'string<))

;;; --- a folder to scan -------------------------------------------------------------

(defun roots ()
  (coerce (or (setting :roots) '()) 'list))

(defun resolve-folder (path)
  "PATH, or the section's first root, as an existing folder's pathname."
  (let* ((named (and path (nlk:trimmed (princ-to-string path))))
         (chosen (or (and named (plusp (length named)) named)
                     (first (roots)))))
    (unless chosen
      (fail "no folder to scan: name one, or set bumblebee.roots to the folders holding your projects"))
    (let ((folder (uiop:ensure-directory-pathname (nlk:expand-home chosen))))
      (unless (uiop:directory-exists-p folder)
        (fail "~a is not a folder" (trimmed-native folder)))
      folder)))

;;; --- the report ----------------------------------------------------------------------

(defun finding-line (finding)
  (format nil "  [~a] ~a -- ~a~%      ~a"
          (field finding :string "severity") (finding-key finding)
          (field finding :string "path") (field finding :string "why")))

(defun osv-line (summary)
  (let ((checked (or (field summary :integer "osv_checked") 0)))
    (when (plusp checked)
      (format nil "osv: ~d advisories across ~d packages checked (vulnerabilities, not confirmed malware)"
              (or (field summary :integer "osv_total") 0) checked))))

(defun report-text (summary &key preview)
  "SUMMARY as a person reads it: what was found, and what the check cannot see."
  (let* ((findings (summary-findings summary))
         (count (length findings))
         (packages (or (field summary :integer "packages") 0))
         (root (or (field summary :string "root") "?"))
         (lines (list
                 (if (plusp count)
                     (format nil "bumblebee: ~d known-compromised package~:p among ~d packages under ~a"
                             count packages root)
                     (format nil "bumblebee: clean -- ~d packages under ~a match none of the ~d curated known-compromised releases"
                             packages root (catalog-size))))))
    (when (plusp (or (field summary :integer "truncated") 0))
      (push (format nil "  only the first ~d packages were inventoried (bumblebee.max_packages)"
                    (or (setting :max-packages) 0))
            lines))
    (loop for finding across (if preview findings findings)
          do (push (finding-line finding) lines))
    (let ((osv (osv-line summary)))
      (when osv (push osv lines)))
    (let ((note (field summary :string "osv_note")))
      (when (and note (plusp (length note)))
        (push (format nil "  osv was not asked: ~a" note) lines)))
    (format nil "~{~a~^~%~}" (nreverse lines))))

;;; --- scanning -----------------------------------------------------------------------

(defun scan-folder (path &key osv)
  "Walk PATH, match the catalog, ask OSV when it is on, keep the scan, say
it. => the report's text."
  (settings)
  (let* ((folder (resolve-folder path))
         (started (get-internal-real-time))
         (osv-wanted (if (null osv) (setting :osv-enabled) osv)))
    (multiple-value-bind (packages truncated)
        (inventory folder :max-depth (setting :max-depth)
                           :max-packages (setting :max-packages))
      (let ((found (findings packages))
            (osv-total 0) (osv-checked 0) (osv-note nil))
        (when (and osv-wanted packages)
          (handler-case
              (multiple-value-bind (total checked)
                  (osv-counts packages :limit (setting :osv-max-packages)
                                      :timeout (setting :timeout-seconds))
                (setf osv-total total osv-checked checked))
            (error (condition)
              (setf osv-note (nlk:one-line (princ-to-string condition))))))
        (let* ((summary
                 (nlk:json-object
                  "root" (trimmed-native folder)
                  "scanned_at" (nlk:iso-now)
                  "packages" (length packages)
                  "truncated" (if truncated 1 0)
                  "findings" (coerce (mapcar #'finding-object found) 'vector)
                  "osv_total" osv-total
                  "osv_checked" osv-checked
                  "osv_note" (or osv-note "")
                  "duration_ms" (round (* 1000 (- (get-internal-real-time) started))
                                       internal-time-units-per-second)))
               (text (report-text summary :preview t)))
          (write-json-file (summary-file) summary)
          (announce summary)
          text)))))

(defun announce (summary)
  "A scan with findings stands as a notice under this folder's key, which
every request's notices block carries; a clean scan clears it."
  (let ((count (length (summary-findings summary))))
    (if (plusp count)
        (nle:notice (format nil "bumblebee: ~d known-compromised package~:p under ~a -- /bumblebee status"
                            count (field summary :string "root"))
                    :key "nodecode-bumblebee" :level :warning)
        (nle:notice nil :key "nodecode-bumblebee"))))

(defun latest-summary ()
  "The last scan, or a refusal naming the way to one."
  (settings)
  (or (read-json-file (summary-file))
      (fail "no scan yet: run bumblebee_guard_scan, (bumblebee:scan \"FOLDER\") or /bumblebee scan FOLDER")))

(defun status-text (&key preview)
  (let* ((summary (latest-summary))
         (findings (summary-findings summary))
         (head (format nil "bumblebee: last scan ~a of ~a -- ~d packages, ~d known-compromised~@[, osv checked ~d packages~]"
                        (or (field summary :string "scanned_at") "?")
                        (or (field summary :string "root") "?")
                        (or (field summary :integer "packages") 0)
                        (length findings)
                        (let ((checked (or (field summary :integer "osv_checked") 0)))
                          (and (plusp checked) checked)))))
    (if (or (not preview) (zerop (length findings)))
        head
        (format nil "~a~%~{~a~^~%\n~}" head
                (loop for finding across findings collect (finding-line finding))))))

;;; --- the baseline and the diff -------------------------------------------------------

(defun diff-text (action)
  (let* ((summary (latest-summary))
         (now (keys-of (summary-findings summary))))
    (cond
      ((string-equal (or action "show") "set_baseline")
       (let ((baseline (nlk:json-object
                        "taken_at" (nlk:iso-now)
                        "root" (or (field summary :string "root") "?")
                        "findings" (or (field summary :array "findings") #()))))
         (write-json-file (baseline-file) baseline)
         (format nil "bumblebee: baseline taken at ~a -- ~d known-compromised package~:p in it"
                 (field baseline :string "taken_at") (length now))))
      (t
       (let ((baseline (read-json-file (baseline-file))))
         (unless baseline
           (fail "no baseline yet: bumblebee_guard_diff with action=set_baseline, or /bumblebee diff set_baseline"))
         (let* ((before (keys-of (summary-findings baseline)))
                (added (set-difference now before :test #'string=))
                (removed (set-difference before now :test #'string=)))
           (format nil "bumblebee: since the baseline of ~a (~a): ~d added, ~d gone, ~d now~{~%  + ~a~}~{~%  - ~a~}"
                   (or (field baseline :string "taken_at") "?")
                   (or (field baseline :string "root") "?")
                   (length added) (length removed) (length now)
                   added removed)))))))

;;; --- the verbs the model and the operator reach --------------------------------------

(defun scan (&optional folder)
  "Scan FOLDER (default: the first bumblebee.roots entry) and answer the report."
  (scan-folder folder))

(defun status (&key preview)
  "The last scan's facts, without walking anything."
  (status-text :preview preview))

(defun diff (&optional (action "show"))
  "What appeared and went away since the baseline, or take a new one."
  (diff-text action))

(defun run-slash (args session-id)
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (head (string-downcase (subseq text 0 space)))
         (rest (and space (nlk:trimmed (subseq text (1+ space))))))
    (cond ((string= head "help")
           "usage: /bumblebee [status | scan FOLDER | diff [show|set_baseline]]")
          ((string= head "scan") (scan-folder rest))
          ((string= head "diff") (diff-text (if (and rest (plusp (length rest))) rest "show")))
          (t (status-text)))))

;;; --- what the model calls -------------------------------------------------------------

(nlk:define-contract scan-request
  (:doc "Scan a folder's installed npm and PyPI packages against the curated catalog of known-compromised releases, and optionally OSV.dev advisories.")
  ("path" :text
   :doc "the folder to scan, e.g. a project holding node_modules or a virtualenv; leave it out to use the first bumblebee.roots entry")
  ("osv" :boolean :default nil
   :doc "also ask OSV.dev for advisory counts, which costs one network call; leave it out to follow the bumblebee.osv_enabled setting"))

(nlk:define-contract status-request
  (:doc "Report the last scan without scanning again.")
  ("preview" :boolean :default nil
   :doc "list every known-compromised finding, not only the counts"))

(nlk:define-contract diff-request
  (:doc "Compare the last scan with the stored baseline, or store this scan as the new baseline.")
  ("action" :text :options '("show" "set_baseline") :default "show"
   :doc "show compares the last scan with the baseline; set_baseline stores the last scan as the baseline"))

(defun tool-scan (request)
  (scan-folder (scan-request-path request) :osv (scan-request-osv request)))

(defun tool-status (request)
  (status-text :preview (status-request-preview request)))

(defun tool-diff (request)
  (diff-text (diff-request-action request)))

;;; --- the declaration -----------------------------------------------------------------

(nle:define-cell bumblebee
  (:section ("bumblebee")
    (:guide "nothing here is required: roots names the folders bumblebee_guard_scan walks when a call names none; osv_enabled turns OSV.dev's advisory counts on (one network call per scan)")
    ("roots" :list :default '()
             :doc "folders scanned when a scan names none, e.g. [\"~/projects\"]")
    ("max_depth" :integer :default 6 :min 1
                 :doc "how many folders deep the walk goes below each root")
    ("max_packages" :integer :default 20000 :min 1
                    :doc "most packages inventoried in one scan; the report says when it stopped")
    ("osv_enabled" :boolean :default t
                   :doc "ask OSV.dev for advisory counts, one batch call per scan")
    ("osv_max_packages" :integer :default 50 :min 1
                        :doc "most packages OSV is asked about in one scan, at one query each")
    ("timeout_seconds" :integer :default 20 :min 1
                       :doc "the OSV call's ceiling; a timeout is reported, never a failed scan"))
  (:tool "bumblebee_guard_scan" (nlk:contract 'scan-request) 'tool-scan)
  (:tool "bumblebee_guard_status" (nlk:contract 'status-request) 'tool-status)
  (:tool "bumblebee_guard_diff" (nlk:contract 'diff-request) 'tool-diff)
  (:help :bumblebee "bumblebee:scan: known-compromised packages among those installed in a folder; /bumblebee" +primer+)
  (:command "bumblebee" 'run-slash
            :catches 'bumblebee-error
            :argument-hint "status | scan FOLDER | diff [show|set_baseline]"
            :description "Supply-chain check: known-compromised packages among those installed here"))
