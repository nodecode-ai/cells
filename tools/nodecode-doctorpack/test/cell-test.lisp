;;;; cell-test.lisp --- lifecycle, the three tools, the redaction, /doctorpack.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over real files: that a
;;;; start installs the three tools, the command and the manual and a stop
;;;; takes them back; that the audit reads a home's config, counts an inline
;;;; credential as a warning and never lets one reach the answer; that the
;;;; lookback actually bounds a log line; that a home with nothing in it
;;;; answers `unknown' rather than `fail'; that a cell folder without an .asd
;;;; is a warning and a name that is not there is `unknown'; that a postmortem
;;;; lands as a file under the home with the evidence redacted; and that
;;;; /doctorpack prints the summary or the whole payload.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "doctorpack"
  (:command "doctorpack")
  (:help :doctorpack)
  (:running (is (nle::find-tool "doctorpack_audit") "the audit tool is registered")
            (is (nle::find-tool "doctorpack_cells") "the cells tool is registered")
            (is (nle::find-tool "doctorpack_postmortem") "the postmortem tool is registered")
            (is (= 24 (nodecode-doctorpack::setting :lookback-hours)) "lookback defaults to 24 hours")
            (is (= 20 (nodecode-doctorpack::setting :max-evidence)) "20 evidence snippets")
            (is (nodecode-doctorpack::setting :include-logs) "log evidence is on by default"))
  (:stopped (is (null (nle::find-tool "doctorpack_audit")) "stop unregisters the tools"))
  (:refused ("lookback_hours" 0) ("max_evidence" 0)))

;;; --- the config audit --------------------------------------------------------------

(deftest doctorpack-cell-audit-reads-the-config-and-masks-a-pasted-key ()
  (let ((home (doctorpack-fixture-home (cons "config.jsonc" (doctorpack-config-text))
                                        (cons "logs/agent.log" (doctorpack-log-text)))))
    (with-cell-stop ((doctorpack-start "home" home))
      (let ((payload (doctorpack-answer "doctorpack_audit")))
        (is (equal "config_audit" (nlk:json-value payload :string "kind")))
        (is (equal "ok" (doctorpack-status payload "config.present")) "the config is there")
        (is (equal "warn" (doctorpack-status payload "config.inline_secret")) "a credential pasted into config.jsonc is a warning")
        (is (equal "warn" (doctorpack-status payload "logs.recent_issues")) "a cell load error in the log is evidence")
        (is (equal "warn" (nlk:json-value payload :string "status")) "a warn outranks ok")
        (let ((text (doctorpack-raw "doctorpack_audit")))
          (is (null (search "sk-abcdefghijklmnopqrstuvwxyz" text)) "the pasted key never rides back")
          (is (search "[REDACTED]" text) "and the answer says it was masked")))))
  (let ((home (doctorpack-fixture-home (cons "config.jsonc" (doctorpack-config-text)))))
    (with-cell-stop ((doctorpack-start "home" home))
      (is (equal "unknown" (doctorpack-status (doctorpack-answer "doctorpack_audit") "logs.recent_issues")) "a home with no log file says unknown, never fail"))))

(deftest doctorpack-cell-audit-lets-the-lookback-expire-an-old-line ()
  (let ((home (doctorpack-fixture-home (cons "config.jsonc" (doctorpack-config-text))
                                        (cons "logs/agent.log" (doctorpack-old-log-text)))))
    (with-cell-stop ((doctorpack-start "home" home))
      (is (equal "ok" (doctorpack-status (doctorpack-answer "doctorpack_audit" "lookback_hours" 24)
                                         "logs.recent_issues")) "a line eight days old is outside a 24 hour lookback")
      (is (equal "warn" (doctorpack-status (doctorpack-answer "doctorpack_audit" "lookback_hours" 240)
                                            "logs.recent_issues")) "and inside a ten day one"))))

(deftest doctorpack-cell-a-home-without-evidence-is-unknown-not-fail ()
  (let ((home (doctorpack-fixture-home)))
    (with-cell-stop ((doctorpack-start "home" home))
      (let ((payload (doctorpack-answer "doctorpack_audit")))
        (is (equal "unknown" (doctorpack-status payload "config.present")) "no config is unknown")
        (is (equal "unknown" (doctorpack-status payload "logs.recent_issues")) "no logs is unknown")
        (is (equal "unknown" (nlk:json-value payload :string "status")) "and the audit as a whole is unknown, never fail")))))

;;; --- the cells folder --------------------------------------------------------------

(deftest doctorpack-cell-check-counts-folders-and-names-an-unloadable-one ()
  (let ((home (doctorpack-fixture-home
               (cons "cells/nodecode-example/nodecode-example.asd"
                     "(defsystem \"nodecode-example\")")
               (cons "cells/loose-folder/README.md" "no manifest here\n"))))
    (with-cell-stop ((doctorpack-start "home" home))
      (let ((payload (doctorpack-answer "doctorpack_cells")))
        (is (equal "ok" (doctorpack-status payload "cells.discovered")) "two folders are found")
        (is (member "nodecode-example" (coerce (nlk:json-array payload "cells") 'list)
                    :test #'string=) "and named")
        (is (equal "warn" (doctorpack-status payload "cells.no_manifest")) "a folder with no .asd loads nothing")
        (is (equal "ok" (doctorpack-status (doctorpack-answer "doctorpack_cells"
                                                             "cell_name" "nodecode-example")
                                           "cells.target_installed")) "a named folder that carries its .asd is installed")
        (is (equal "warn" (doctorpack-status (doctorpack-answer "doctorpack_cells"
                                                                "cell_name" "loose-folder")
                                             "cells.target_unloadable")))
        (is (equal "unknown" (doctorpack-status (doctorpack-answer "doctorpack_cells"
                                                                  "cell_name" "nowhere")
                                               "cells.target_not_found")) "a name that is not there is unknown, not fail")))))

;;; --- the postmortem -----------------------------------------------------------------

(deftest doctorpack-cell-postmortem-writes-a-redacted-report-under-the-home ()
  (let* ((home (doctorpack-fixture-home (cons "config.jsonc" (doctorpack-config-text))
                                         (cons "logs/agent.log" (doctorpack-log-text))))
         (payload nil))
    (with-cell-stop ((doctorpack-start "home" home))
      (setf payload (doctorpack-answer "doctorpack_postmortem"
                                        "incident_title" "Gateway stopped at 415-555-0137"))
      (let ((path (nlk:json-value payload :string "report_path")))
        (is (probe-file path) "the report is a file that exists")
        (is (search "doctorpack/reports/" (uiop:native-namestring path)) "under the home's own reports folder")
        (let ((text (uiop:read-file-string path)))
          (is (search "# Gateway stopped at" text) "the title heads the report")
          (is (search "[REDACTED_PHONE]" text) "the number in the title is masked")
          (is (search "## Findings" text) "the findings are in it")
          (is (null (search "sk-abcdefghijklmnopqrstuvwxyz" text)) "and no pasted key is")
          (is (search "REDACTED_HOME" text) "nor is the home's own path")))
      (is (equal "postmortem" (nlk:json-value payload :string "kind")))
      (is (plusp (or (nlk:json-value payload :integer "evidence_count") 0)) "evidence rode into the report"))
    ;; a second report never overwrites the first
    (with-cell-stop ((doctorpack-start "home" home))
      (let* ((first (nlk:json-value (doctorpack-answer "doctorpack_postmortem"
                                                       "incident_title" "twice")
                                     :string "report_path"))
             (second (nlk:json-value (doctorpack-answer "doctorpack_postmortem"
                                                        "incident_title" "twice")
                                      :string "report_path")))
        (is (probe-file first))
        (is (probe-file second))
        (is (not (equal first second)) "a second report of the same title takes the next free name")))))

;;; --- the redaction itself -----------------------------------------------------------

(deftest doctorpack-cell-redaction-masks-secrets-and-identifiers ()
  (let ((text (nodecode-doctorpack:redact
               "api_key=\"s3cr3tvalue\" Authorization: Bearer abc.def.ghi \
token=ghp_abcdefghijklmnopqrstuvwxyz12 https://user:pw@example.test/x \
write to someone@example.test or call 415-555-0137")))
    (is (null (search "s3cr3tvalue" text)) "the assigned value is masked")
    (is (null (search "abc.def.ghi" text)) "a bearer credential is masked")
    (is (null (search "ghp_abcdefghijklmnopqrstuvwxyz12" text)) "a token shape is masked")
    (is (null (search "user:pw@" text)) "a URL's userinfo is masked")
    (is (search "[REDACTED_EMAIL]" text) "an address is masked as one")
    (is (search "[REDACTED_PHONE]" text) "a phone shape is masked as one")
    (is (search "api_key=" text) "the key's name survives, so the finding still says which one")))

(deftest doctorpack-cell-redaction-walks-a-whole-object ()
  (let ((object (nlk:json-object "endpoint" "https://example.test"
                                  "webhook_token" "abc123"
                                  "nested" (nlk:json-object "note" "write to someone@example.test"))))
    (let ((redacted (nodecode-doctorpack::redact-object object)))
      (is (equal "[REDACTED]" (gethash "webhook_token" redacted)) "a credential-named member is masked whatever it holds")
      (is (equal "https://example.test" (gethash "endpoint" redacted)) "a plain value is kept")
      (is (search "[REDACTED_EMAIL]" (gethash "note" (gethash "nested" redacted))) "and a nested string is redacted too"))))

;;; --- /doctorpack --------------------------------------------------------------------

(deftest doctorpack-cell-command-answers-a-summary-or-the-whole-payload ()
  (let ((home (doctorpack-fixture-home (cons "config.jsonc" (doctorpack-config-text)
                                             ))))
    (with-cell-stop ((doctorpack-start "home" home))
      (let ((text (nodecode-doctorpack::run-slash "scan" nil)))
        (is (search "Doctorpack config_audit status:" text) "the summary names the verb and the status")
        (is (search "Findings: fail=" text) "and the counts")
        (is (search "config.jsonc present" text) "and the findings by title"))
      (let* ((text (nodecode-doctorpack::run-slash "cells --json" nil))
             (payload (nlk:decode-json text)))
        (is (equal "cell_check" (nlk:json-value payload :string "kind")) "--json answers the whole payload"))
      (let ((text (nodecode-doctorpack::run-slash "postmortem --incident-title \"a bad day\"" nil)))
        (is (search "Doctorpack postmortem status:" text) "postmortem is a verb of its own")
        (is (search "Report: " text) "and it names the file it wrote"))
      (is (search "usage: /doctorpack" (nodecode-doctorpack::run-slash "nonsense" nil)) "an unknown verb answers the usage line"))))
