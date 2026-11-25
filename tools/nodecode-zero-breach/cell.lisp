;;;; cell.lisp --- one machine, checked for the traces of known spyware, with
;;;; the gaps kept apart from the clean answer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four calls and one command over archivegenocide's own scanner (vendored at
;;;; a pinned commit, see vendor/VENDOR.md): zerobreach_scan checks this
;;;; machine, zerobreach_phone reads a phone collection instead,
;;;; zerobreach_last re-reads the newest saved report, and zerobreach_describe
;;;; says what the tool checks and cannot see without scanning at all.
;;;; /zero-breach does the same from the operator's side.
;;;;
;;;; The tool it wraps is careful about one distinction and this cell keeps it:
;;;; a check that could not run is a GAP, never a clean result, and a clean
;;;; result is never a bill of health. The engine's own caveats (must_relay)
;;;; travel with every answer.

(in-package #:nodecode-zero-breach)

(nlk:define-peripheral zero-breach :not-running t)

;;; --- what the engine's verdict means, in words -------------------------------------

(defparameter *verdict-words*
  '(("INDICATORS_FOUND" . "known indicators were found: read every hit's tier and the evidence behind it")
    ("NO_KNOWN_INDICATORS" . "nothing known was found, and every check in scope ran")
    ("NO_KNOWN_INDICATORS_WITH_GAPS" . "nothing known was found, but some checks could not run: a gap is not a clean result")
    ("REFERENCES_ONLY" . "only saved references were found: a trace of a visit, not a sign of spyware")
    ("WITHDRAWN_ONLY" . "only indicators their publishers withdrew were found"))
  "The engine's verdict strings, said in plain words.")

(defun verdict-words (status)
  "STATUS as plain words, or a sentence saying this cell does not know it."
  (or (cdr (assoc status *verdict-words* :test #'string-equal))
      (format nil "the engine answered ~a, which this cell does not know" status)))

;;; --- how a report is said ------------------------------------------------------------

(defparameter *listed-gaps* 8 "The most gap lines one answer spells out.")
(defparameter *listed-hits* 10 "The most hits one answer spells out.")

(defun report-text (report &key (heading "Zero Breach"))
  "REPORT, the plist GATHER-SCAN answers, as the lines a person reads."
  (with-output-to-string (s)
    (let ((set (getf report :indicator-set))
            (hits (getf report :hits))
            (gaps (getf report :gaps)))
        (format s "~a: ~a~%" heading (verdict-words (getf report :status)))
        (format s "Status ~a (exit ~a: ~a).~%"
                (getf report :status) (getf report :exit) (getf report :exit-meaning))
        (format s "Scanned ~a~@[ (the engine's own scan type: ~a)~] on ~a ~a in ~,1f s with the engine at version ~a.~%"
                (getf report :scope)
                (let ((type (getf report :scan-type)))
                  (and type (not (string-equal type (getf report :scope))) type))
                (getf report :platform) (getf report :machine)
                (or (getf report :seconds) 0) (or (getf report :tool-version) "?"))
        (format s "Checked ~a things; ~a check(s) could not run.~%"
                (or (getf report :checked) 0) (length gaps))
        (when set
          (format s "Indicator set: ~a indicators, ~@[~a days old~]~a.~%"
                  (or (getf set :count) "unknown")
                  (getf set :age-days)
                  (cond ((getf set :stale) " (STALE: treat a clean result as weaker)")
                        ((getf set :freshness-unknown) " (freshness unknown)")
                        (t ""))))
        (if hits
            (progn
              (format s "~&Found ~d hit(s):~%" (length hits))
              (loop for hit in (subseq hits 0 (min *listed-hits* (length hits)))
                    do (format s "  - ~a~@[ (~a~@[, ~a~])~]~@[ at ~a~]~@[ in the ~a check~]~%"
                               (or (getf hit :indicator) "?")
                               (getf hit :family) (getf hit :tier)
                               (getf hit :location) (getf hit :module)))
              (when (> (length hits) *listed-hits*)
                (format s "  ... and ~d more in the full report.~%" (- (length hits) *listed-hits*))))
            (format s "Found no indicator match.~%"))
        (when gaps
          (format s "~&Could not be checked (~d):~%" (length gaps))
          (loop for gap in (subseq gaps 0 (min *listed-gaps* (length gaps)))
                do (format s "  - ~a~%" gap))
          (when (> (length gaps) *listed-gaps*)
            (format s "  ... and ~d more in the full report.~%" (- (length gaps) *listed-gaps*))))
        (when (getf report :worth-a-look)
          (format s "~&Worth a look, NOT findings (~d): ~{~a~^; ~}.~%"
                  (length (getf report :worth-a-look))
                  (loop for row in (getf report :worth-a-look)
                        for n from 1 to 3
                        collect (format nil "~@[~a: ~]~a" (getf row :location) (getf row :what)))))
        (when (getf report :doors)
          (format s "Doors left open, prevention and not a finding (~d): ~{~a~^; ~}.~%"
                  (length (getf report :doors))
                  (loop for row in (getf report :doors) for n from 1 to 4
                        collect (or (getf row :title) "?"))))
        (when (getf report :next-steps)
          (format s "~&What to do next:~%")
          (dolist (step (getf report :next-steps))
            (format s "  - ~a~%" step)))
        (when (getf report :caveats)
          (format s "~&The engine's caveats, relayed as they are:~%")
          (dolist (caveat (getf report :caveats))
            (format s "  - ~a~%" caveat)))
        (format s "~&Reports: ~a (the full one carries paths, names and browsing details: keep it private)~@[; sharing summary ~a~].~%"
                (getf report :full-report) (getf report :summary-report)))))

;;; --- the calls a model makes -----------------------------------------------------------

(nlk:define-contract zerobreach-scan
  (:doc "Check THIS machine for the traces of known mercenary spyware (Pegasus, Predator,
Graphite, Candiru and 40 more families): browser profiles, startup items, programs,
DNS and trust settings, Wi-Fi, and the doors left open. Read-only, and it contacts no
network during a scan. The answer keeps two things apart that must not be merged:
what was checked and came back clean, and what could not be checked at all. A clean
result is never a bill of health. Pick the scope honestly: quick skips the file
fingerprints and says so as a gap, standard covers them, full adds web caches.
Use it when the operator asks whether a machine has spyware on it, or asks for a
security check of this computer.")
  ("scope" :text
           :doc "quick (the default: browsers, host, startup items; file fingerprints skipped and reported as a gap), standard (adds them, the vendor's recommended scope), full (adds raw web caches and more file types), browser (browser profiles only), targeted (reads the startup-file copies in target)")
  ("target" :text
            :doc "the folder a targeted scan reads: copies of Prefetch, Amcache or startup files")
  ("patterns" :boolean
              :doc "also run the optional Candiru file-pattern check; it needs the pinned YARA-X wheel installed in the same Python, and says so as a gap when it is not")
  ("feeds" :boolean
           :doc "use indicator sets downloaded with the toolkit's own --update as well as the bundled set; the bundled set alone is the default")
  ("timeout" :integer
             :doc "seconds the scan may take before it is stopped; the cell's timeout setting when absent"))

(nlk:define-contract zerobreach-phone
  (:doc "Check a phone instead of this computer: an AndroidQF collection folder, an Android
bug report (.zip made on the phone), or a local iPhone backup folder. Same report
shape as zerobreach_scan, and the same rule: a clean result is not a clean bill of
health. Use it when the operator asks whether a phone or a backup carries spyware.")
  ("folder" :text :required t
            :doc "the collection folder, the bug-report .zip, or the iPhone backup folder")
  ("feeds" :boolean
           :doc "use indicator sets downloaded with --update as well as the bundled set")
  ("timeout" :integer
             :doc "seconds the scan may take before it is stopped"))

(nlk:define-contract zerobreach-last
  (:doc "Re-read the newest saved report and answer it again, with no scan and no process
started: what the last check found, what it could not check, and where the report
is. Use it when the operator asks what the last check said, or when a scan's answer
scrolled away.")
  ("summary_only" :boolean
                  :doc "answer the verdict and the counts only, without the per-check lines"))

(nlk:define-contract zerobreach-describe
  (:doc "What this tool checks, what each check reads and cannot see, the meaning of each
exit code, and the rules the tool sets for an assistant relaying a result - answered
from the engine itself with no scan. Use it when the operator asks what the check
covers, whether a named spyware family is in scope, or what a clean result means.")
  ("module" :text
            :doc "one check's name (browser, hash, unix-host, doors, wifi-settings, phones-backup ...): answer that one alone"))

(defun scan-answer (&key scope target patterns feeds timeout)
  "The zerobreach_scan answer."
  (report-text (gather-scan :scope (or scope (setting :scope))
                             :target target
                             :patterns patterns
                             :feeds (if (null feeds) (setting :feeds) feeds)
                             :timeout timeout)))

(defun phone-answer (&key folder feeds timeout)
  "The zerobreach_phone answer."
  (report-text (gather-scan :scope "default" :phone folder
                             :feeds (if (null feeds) (setting :feeds) feeds)
                             :timeout timeout)
               :heading (format nil "Zero Breach, phone collection ~a" folder)))

(defun newest-report ()
  "The newest report this profile saved, or NIL and why not."
  (let* ((directory (report-directory))
         (files (and (probe-file directory)
                     (directory (merge-pathnames "zero-breach-*.full.json" directory)))))
    (if files
        (first (sort files #'> :key #'file-write-date))
        (values nil (format nil "no scan has been saved in ~a yet: run one with zerobreach_scan"
                            (namestring directory))))))

(defun last-answer (&key summary-only)
  "The zerobreach_last answer."
  (multiple-value-bind (path missing) (newest-report)
    (if (null path)
        (format nil "Zero Breach: ~a" missing)
        (let* ((report (read-report path))
               (plist (plist-report report nil :exit (field report :integer "exit_code")
                                    :seconds nil
                                    :full (namestring path)
                                    :summary nil)))
          (if summary-only
              (format nil "Zero Breach: ~a (status ~a, exit ~a). Checked ~a things; ~a check(s) could not run; ~d hit(s). Saved ~a.~%"
                      (verdict-words (getf plist :status)) (getf plist :status) (getf plist :exit)
                      (or (getf plist :checked) 0) (length (getf plist :gaps))
                      (length (getf plist :hits)) (namestring path))
              (report-text plist :heading "Zero Breach, the last saved check"))))))

(defun describe-document ()
  "The engine's own --describe document, decoded, with no scan run."
  (let* ((script (engine-script))
         (argv (append (timeout-argv 120)
                       (list (setting :python) "-X" "utf8" script "--describe" "--no-banner"))))
    (multiple-value-bind (out err code) (funcall *run* argv)
      (unless (and (plusp (length out)) (char= #\{ (char out 0)))
        (fail "the engine answered no description (exit ~a): ~a" code (trimmed-output err out)))
      (read-json out))))

(defun describe-text (&key module)
  "The zerobreach_describe answer."
  (let* ((doc (describe-document))
         (modules (field doc :object "modules"))
         (names (sort (loop for name being the hash-keys of modules collect name) #'string<))
         (rules (coerce (or (field doc :array "instructions_for_assistants") #()) 'list)))
    (with-output-to-string (s)
      (format s "Zero Breach Toolkit ~a: ~a~%"
              (or (field doc :string "tool" "version") "?")
              (or (field doc :string "tool" "summary") ""))
      (format s "It runs ~d checks, all local and read-only:~%" (length names))
      (dolist (name (if module
                        (remove-if-not (lambda (n) (search module n :test #'char-equal)) names)
                        names))
        (let ((entry (field modules :object name)))
          (format s "~&  ~a~@[ (~{~a~^, ~})~]: ~a~%"
                  name
                  (coerce (or (field entry :array "platforms") #()) 'list)
                  (or (field entry :string "reads") ""))
          (let ((blind (coerce (or (field entry :array "blind_spots") #()) 'list)))
            (when blind
              (format s "      cannot see: ~{~a~^; ~}.~%" (subseq blind 0 (min 3 (length blind))))))))
      (format s "~&Exit codes:~%")
      (loop for code from 0 to 4
            for text = (field doc :string "exit_codes" (princ-to-string code))
            when text do (format s "  ~d: ~a~%" code text))
      (when rules
        (format s "~&The tool's rules for an assistant relaying a result:~%")
        (dolist (rule rules) (format s "  - ~a~%" rule)))
      (multiple-value-bind (path missing) (newest-report)
        (declare (ignore missing))
        (when path
          (format s "~&The newest saved check: ~a.~%" (namestring path)))))))

(defun scan-tool (arguments)
  "The zerobreach_scan tool."
  (scan-answer :scope (zerobreach-scan-scope arguments)
               :target (zerobreach-scan-target arguments)
               :patterns (zerobreach-scan-patterns arguments)
               :feeds (zerobreach-scan-feeds arguments)
               :timeout (zerobreach-scan-timeout arguments)))

(defun phone-tool (arguments)
  "The zerobreach_phone tool."
  (phone-answer :folder (zerobreach-phone-folder arguments)
                :feeds (zerobreach-phone-feeds arguments)
                :timeout (zerobreach-phone-timeout arguments)))

(defun last-tool (arguments)
  "The zerobreach_last tool."
  (last-answer :summary-only (zerobreach-last-summary-only arguments)))

(defun describe-tool (arguments)
  "The zerobreach_describe tool."
  (describe-text :module (zerobreach-describe-module arguments)))

;;; --- the operator's side ---------------------------------------------------------------

(defparameter +usage+ "zero-breach scan [quick|standard|full|browser] | phone FOLDER | describe [CHECK] | last")

(defun run-slash (args session-id)
  "One /zero-breach invocation => its text."
  (declare (ignore session-id))
  (let ((words (nlk:split-words (or args ""))))
    (handler-case
        (let ((head (first words)))
          (cond
            ((null head) (scan-answer))
            ((string-equal head "scan") (scan-answer :scope (second words)))
            ((string-equal head "phone")
             (if (second words)
                 (phone-answer :folder (second words))
                 (format nil "zero-breach phone needs a folder: ~a" +usage+)))
            ((string-equal head "describe") (describe-text :module (second words)))
            ((string-equal head "last") (last-answer))
            (t (format nil "zero-breach did not know \"~a\". ~a" head +usage+))))
      (error (e) (format nil "zero-breach: ~a" e)))))

;;; --- the manual -------------------------------------------------------------------------

(defparameter +primer+
  "Checks THIS machine for the traces of known mercenary spyware -- browser
profiles, startup items, programs, DNS and trust settings, doors left open --
read-only, with no network during a scan.
  (zero-breach:scan-answer :scope \"standard\")
  (zero-breach:phone-answer :folder \"/path/to/backup\")
  (zero-breach:last-answer)              the newest saved report, no scan
  (zero-breach:describe-text)            what it checks and cannot see
/zero-breach scan [quick|standard|full|browser], /zero-breach phone FOLDER,
/zero-breach describe [CHECK] and /zero-breach last print the same as lines.
Two things are never merged: what was checked and came back clean, and what
could not be checked (a gap). A clean result means no known indicator was
found in what could be read; the spyware this looks for is built to defeat
such checks, and the traces expire. Relay the engine's caveats as they are,
and never turn a gap into a clean answer."
  "What (help :zero-breach) answers while the cell runs.")

;;; --- the cell ---------------------------------------------------------------------------

(nle:define-cell zero-breach
  (:section ("zero-breach")
    (:guide "scope is the default a bare /zero-breach uses; toolkit points at another copy of the scanner; timeout bounds one run in seconds; report_directory is where the JSON reports are kept")
    ("scope" :string :default "quick"
             :doc "the scope a bare /zero-breach scan uses: quick, standard, full or browser")
    ("toolkit" :path :default ""
               :doc "the toolkit's scan folder; empty means the copy vendored inside this cell")
    ("timeout" :integer :default 1800 :min 30
               :doc "seconds one scan may take before it is stopped")
    ("python" :string :default "python3"
              :doc "the Python 3 interpreter that runs the scanner (3.8 or newer, standard library only)")
    ("report_directory" :path :default ""
                        :doc "where a scan's JSON reports are kept; empty means this profile's zero-breach folder")
    ("feeds" :boolean :default nil
             :doc "use indicator sets downloaded with the toolkit's --update as well as the bundled set")
    ("iocs" :path :default ""
            :doc "one more indicator file to scan with (the curated tab-separated format, or a STIX 2 bundle); the bundled set is always used"))
  (:tool "zerobreach_scan" (nlk:contract 'zerobreach-scan) 'scan-tool)
  (:tool "zerobreach_phone" (nlk:contract 'zerobreach-phone) 'phone-tool)
  (:tool "zerobreach_last" (nlk:contract 'zerobreach-last) 'last-tool)
  (:tool "zerobreach_describe" (nlk:contract 'zerobreach-describe) 'describe-tool)
  (:help :zero-breach "zero-breach: check this machine for the traces of known spyware, with the gaps kept as gaps" +primer+)
  (:command "zero-breach" 'run-slash
            :description "Check this machine for traces of known spyware; the gaps stay gaps"
            :argument-hint "scan [quick|standard|full|browser]|phone FOLDER|describe [CHECK]|last"))
