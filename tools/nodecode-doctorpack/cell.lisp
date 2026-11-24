;;;; cell.lisp --- the three tools, /doctorpack, the manual, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original registers three tools, a slash command and a CLI command
;;;; against Hermes; this one registers the same three verbs, /doctorpack, and
;;;; the manual topic (help :doctorpack). A model's call cannot choose a
;;;; path: the audit tools take a home to read, and the one that writes takes
;;;; no path at all -- its report lands under <home>/doctorpack/reports/,
;;;; whatever the model says.
;;;;
;;;; Nothing here answers a condition into the turn: a tool whose audit
;;;; failed answers the same JSON shape with success=false, as the original's
;;;; handler did, because a diagnostic that throws is a diagnostic nobody
;;;; reads.

(in-package #:nodecode-doctorpack)

;;; --- what a model may ask --------------------------------------------------------

(defparameter +audit-doc+
  "Audit this machine's Nodecode home: config.jsonc, the cells folder and recent logs, with redacted evidence and ok/warn/fail/unknown findings. Use when Nodecode seems misconfigured, unsafe or unreliable. Reads no network, writes nothing."
  "What the model is told the audit does.")

(defparameter +cells-doc+
  "Inspect the Nodecode cells folder and recent cell-related logs. Use when a cell is installed but its tools are missing, or cell errors are suspected. Reads no network, writes nothing."
  "What the model is told the cell check does.")

(defparameter +postmortem-doc+
  "Write a local redacted incident postmortem from this home's config, cells and log evidence, under the home's doctorpack/reports directory, and answer its filename with the structured findings."
  "What the model is told the postmortem does.")

(nlk:define-contract config-audit-call
  (:doc +audit-doc+)
  ("home" :text :default ""
          :doc "which Nodecode home to read; the active one when empty")
  ("lookback_hours" :integer
                    :doc "how far back log evidence counts, at least 1; the doctorpack.lookback_hours setting when absent"))

(nlk:define-contract cell-check-call
  (:doc +cells-doc+)
  ("home" :text :default ""
          :doc "which Nodecode home to read; the active one when empty")
  ("lookback_hours" :integer
                    :doc "how far back log evidence counts, at least 1; the setting when absent")
  ("cell_name" :text :default ""
               :doc "the folder under cells/ to answer for, e.g. nodecode-doctorpack"))

(nlk:define-contract postmortem-call
  (:doc +postmortem-doc+)
  (:unknown :refuse)
  ("incident_title" :text :default ""
                    :doc "a short incident title; the default names a Nodecode diagnostic incident")
  ("lookback_hours" :integer
                    :doc "how far back log evidence counts, at least 1; the setting when absent")
  ("max_evidence" :integer
                  :doc "how many redacted evidence snippets to keep, at least 1; the setting when absent"))

;;; --- the three verbs -------------------------------------------------------------

(defmacro answering (body)
  "BODY's payload as the JSON text a tool answers, or the same shape with
success=false when it signalled: a diagnostic never throws into the turn."
  `(handler-case (json-text ,body)
     (error (condition)
       (json-text (redact-object
                   (nlk:json-object "success" nil
                                    "status" "error"
                                    "error" "doctorpack_internal_error"
                                    "message" (nlk:one-line condition)))))))

(defun audit-tool (call)
  "The doctorpack_audit tool: the config audit of CALL's home."
  (answering (config-audit (home-directory (config-audit-call-home call))
                            (config-audit-call-lookback-hours call))))

(defun cells-tool (call)
  "The doctorpack_cells tool: the cells folder and cell log evidence."
  (answering (cell-check (home-directory (cell-check-call-home call))
                          (cell-check-call-cell-name call)
                          (cell-check-call-lookback-hours call))))

(defun postmortem-tool (call)
  "The doctorpack_postmortem tool: one redacted report under the home."
  (answering (postmortem (home-directory "")
                          (postmortem-call-incident-title call)
                          (postmortem-call-max-evidence call)
                          (postmortem-call-lookback-hours call))))

;;; --- /doctorpack ----------------------------------------------------------------

(defparameter +usage+
  "usage: /doctorpack [scan | cells | postmortem] [--lookback-hours N] [--max-evidence N] [--incident-title TEXT] [--home PATH] [--json]"
  "What an unreadable command line answers with.")

(defun summary-text (payload)
  "PAYLOAD as the lines /doctorpack prints: the status, the counts, the report
file, then each finding with its fix."
  (let ((counts (nlk:json-value payload :object "finding_counts")))
    (format nil "~{~a~%~^~}"
            (append (list (format nil "Doctorpack ~a status: ~a"
                                   (nlk:json-value payload :string "kind")
                                   (nlk:json-value payload :string "status"))
                          (format nil "Nodecode home: ~a" (nlk:json-value payload :string "nodecode_home"))
                          (format nil "Findings: fail=~d warn=~d unknown=~d ok=~d"
                                  (or (nlk:json-value counts :integer "fail") 0)
                                  (or (nlk:json-value counts :integer "warn") 0)
                                  (or (nlk:json-value counts :integer "unknown") 0)
                                  (or (nlk:json-value counts :integer "ok") 0)))
                    (when (nlk:json-value payload :string "report_filename")
                      (list (format nil "Report: ~a (under the home's doctorpack/reports/)"
                                    (nlk:json-value payload :string "report_filename"))))
                    (list "")
                    (loop for f across (nlk:json-array payload "findings")
                          for line = (format nil "[~a] ~a --- ~a"
                                             (nlk:json-value f :string "status")
                                             (nlk:json-value f :string "title")
                                             (nlk:json-value f :string "summary"))
                          for fix = (nlk:json-value f :string "recommendation")
                          collect line
                          when (and fix (plusp (length fix))) collect (format nil "  fix: ~a" fix))))))

(defun parse-flag (word)
  "WORD as the flag name it is, or NIL."
  (and (uiop:string-prefix-p "--" word) (subseq word 2)))

(defun run-slash (args session-id)
  "The /doctorpack command: ARGS as a small command line, answering the
summary in words or, under --json, the whole payload."
  (declare (ignore session-id))
  (let ((words (remove "" (uiop:split-string (or args "") :separator '(#\Space #\Tab)) :test #'string=))
        (verb "scan")
        (option-values (make-hash-table :test 'equal))
        (json nil))
    (let ((verb-seen nil))
      (loop while words
            for word = (pop words)
            do (let ((flag (parse-flag word)))
                 (cond ((and flag (plusp (length flag)))
                        (if (string= flag "json")
                            (setf json t)
                            (setf (gethash flag option-values)
                                  (string-trim "\"'" (or (pop words) "")))))
                       ((not verb-seen)
                        (setf verb (string-downcase word) verb-seen t))))))
    (labels ((option (name) (let ((value (gethash name option-values)))
                              (and value (plusp (length value)) value)))
             (number (name)
             (let ((value (option name)))
               (and value (ignore-errors (parse-integer value))))))
      (handler-case
          (let ((payload
                  (cond ((member verb '("scan" "audit" "config") :test #'string=)
                         (config-audit (home-directory (option "home")) (number "lookback-hours")))
                        ((member verb '("cells" "plugins") :test #'string=)
                         (cell-check (home-directory (option "home"))
                                      (or (option "cell") (option "cell-name") "")
                                      (number "lookback-hours")))
                        ((string= verb "postmortem")
                         (postmortem (home-directory (option "home"))
                                      (or (option "incident-title") "")
                                      (number "max-evidence")
                                      (number "lookback-hours")))
                        (t (return-from run-slash
                             (format nil "~a~%no verb named ~s" +usage+ verb))))))
            (if json (json-text payload) (summary-text payload)))
        (error (condition)
          (format nil "Doctorpack could not finish: ~a" (nlk:one-line condition)))))))

;;; --- the manual -----------------------------------------------------------------

(defparameter +manual+
  "Doctorpack reads this machine's Nodecode home and answers what is wrong with
it, in evidence: three tools and one command, all local, no network.
  doctorpack_audit      the config audit: config.jsonc present, its sections,
      a credential pasted inline, and the recent warning/error lines of the
      home's logs. Answers indented JSON: status, finding_counts, findings
      (each with id, status ok|warn|fail|unknown, severity, confidence,
      summary, evidence, recommendation).
  doctorpack_cells      the cells folder: which folders are there, which carry
      no .asd, and the recent cell-related log lines; pass cell_name to answer
      for one folder.
  doctorpack_postmortem writes one redacted markdown report under
      <home>/doctorpack/reports/ and answers its filename and the findings.
  /doctorpack [scan | cells | postmortem] [--lookback-hours N]
      [--max-evidence N] [--incident-title TEXT] [--home PATH] [--json]
Everything returned has been through the same redaction as the report: keys,
tokens, passwords, bearer credentials, URL userinfo, addresses and phone
numbers are masked before a finding leaves. Missing evidence is `unknown',
never `fail': a home with no log file is not a broken home. The section is
doctorpack: home (which home to read), lookback_hours, max_evidence and
include_logs (the operator's switch for log evidence)."
  "What (help :doctorpack) answers while the cell runs.")

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell doctorpack
  (:section ("doctorpack")
    (:guide "nothing here is required: with no section at all the cell audits the home it runs in; home points the audit at another profile, lookback_hours bounds how old a log line may be, include_logs turns log evidence off")
    ("home" :path :doc "which Nodecode home to read; the active one when unset")
    ("lookback_hours" :integer :default 24 :min 1
                      :doc "how far back a log line counts, in hours")
    ("max_evidence" :integer :default 20 :min 1
                    :doc "how many redacted evidence snippets a postmortem keeps")
    ("include_logs" :boolean :default t
                    :doc "whether config_audit reads the home's logs"))
  (:tool "doctorpack_audit" (nlk:contract 'config-audit-call) 'audit-tool)
  (:tool "doctorpack_cells" (nlk:contract 'cell-check-call) 'cells-tool)
  (:tool "doctorpack_postmortem" (nlk:contract 'postmortem-call) 'postmortem-tool)
  (:command "doctorpack" 'run-slash
            :description "Local diagnostics: the config, the cells folder, a redacted postmortem"
            :argument-hint "scan | cells | postmortem")
  (:help :doctorpack "doctorpack: audit this home's config, cells and logs, and write a redacted postmortem" +manual+))
