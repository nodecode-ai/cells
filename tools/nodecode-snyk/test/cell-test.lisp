;;;; cell-test.lisp --- the snyk cell against a stubbed Snyk CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test installs Snyk, needs an account or touches the network: every
;;;; run goes through NODECODE-SNYK::*RUN*, a stub that records the command
;;;; line it was handed and answers the output the test chose. What is under
;;;; test is the part this cell owns — the argv it builds out of a declared
;;;; call, and the string it answers from what came back.

(in-package #:nodecode.test)

(define-test-slice "snyk" "SNYK-CELL-" :start nodecode-snyk:start-cell)

(define-cell-lifecycle-tests "snyk"
  (:help :snyk)
  (:command "snyk")
  (:running
   (is (equal '("snyk") (nodecode-snyk::setting :command)) "the section's default command is the CLI on PATH")
   (is (nle::find-tool "snyk_code_scan") "the scan tools are registered")
   (is (nle::find-tool "snyk_version") "and the account's own")
   (is (= 10 (count-if (lambda (entry) (eql 0 (search "snyk_" (first entry)))) nle::*tools*)) "ten of them, the original's plugin had thirteen"))
  (:stopped
   (is (null (nle::find-tool "snyk_code_scan")) "and stop takes every one back")
   (is (null (nle::find-tool "snyk_version"))))
  (:refused ("command" 5)
            ("timeout_seconds" -1)
            ("max_output_chars" 10)
            ("severity_threshold" "nonsense")))

;;; --- the stub --------------------------------------------------------------

(defvar *calls* nil
  "The command lines the stub was handed, newest first.")

(defmacro with-snyk-cli ((&key (out "") (err "") (status 0)) &body body)
  "BODY with the cell started and every Snyk run answered by a stub: OUT,
ERR and STATUS stand in for the CLI's, and *CALLS* records the argv."
  `(let* ((*calls* nil)
          (nodecode-snyk::*run* (lambda (argv)
                                   (push argv *calls*)
                                   (values ,out ,err ,status))))
     (with-cell-stop ((snyk-start))
       ,@body)))

(defun snyk-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "snyk-call" name arguments)))

(defun snyk-ran (expected description)
  "Assert that the last run handed to the stub was EXPECTED, saying WHY."
  (is (equal expected (first *calls*)) description))

;;; --- one command line per tool --------------------------------------------

(deftest snyk-cell-a-scan-tool-builds-its-command-line ()
  (with-snyk-cli (:out "{\"ok\": true}")
    (let ((answer (snyk-call "snyk_code_scan" "{\"path\": \"/tmp/project\"}")))
      (snyk-ran '("snyk" "code" "test" "--json" "--severity-threshold=low" "/tmp/project")
                "code test the absolute path, at the section's severity floor")
      (is (search "exit 0 (no issues found)" answer) "the answer says how the run ended")
      (is (search "{\"ok\": true}" answer) "and carries what the CLI wrote"))
    (snyk-call "snyk_sca_scan"
               "{\"path\": \"/tmp/project\", \"severity_threshold\": \"high\", \"command\": \"python3\", \"dev\": false}")
    (snyk-ran '("snyk" "test" "--json" "--all-projects" "--severity-threshold=high"
                "--command=python3" "/tmp/project")
              "test with all_projects as it defaults, and a false switch passed as nothing")
    (snyk-call "snyk_container_scan" "{\"image\": \"node:20-alpine\", \"exclude_base_image_vulns\": true}")
    (snyk-ran '("snyk" "container" "test" "--json" "--severity-threshold=low"
                "--exclude-base-image-vulns" "node:20-alpine")
              "container test on an image, a switch passed only when true")
    (snyk-call "snyk_iac_scan" "{\"path\": \"/tmp/infra\", \"scan\": \"resource-changes\"}")
    (snyk-ran '("snyk" "iac" "test" "--json" "--severity-threshold=low"
                "--scan=resource-changes" "/tmp/infra")
              "iac test, the Terraform plan mode as a value flag")
    (snyk-call "snyk_sbom_scan" "{\"file\": \"/tmp/bom.cdx.json\"}")
    (snyk-ran '("snyk" "sbom" "test" "--json" "--file=/tmp/bom.cdx.json" "--severity-threshold=low")
              "sbom test names the document with --file")
    (snyk-call "snyk_aibom" "{\"path\": \"/tmp/pyproject\"}")
    (snyk-ran '("snyk" "aibom" "/tmp/pyproject") "aibom takes the project directory")))

(deftest snyk-cell-a-missing-argument-is-refused-before-anything-runs ()
  (with-snyk-cli ()
    (let ((answer (snyk-call "snyk_container_scan" "{}")))
      (is (search "image" answer) "the refusal names the member the call left out")
      (is (null *calls*) "and the CLI never ran"))))

(deftest snyk-cell-a-relative-path-is-refused ()
  (with-snyk-cli ()
    (let ((answer (snyk-call "snyk_code_scan" "{\"path\": \"src\"}")))
      (is (search "absolute path" answer) "the model is told what to do")
      (is (search "pwd" answer) "and how to find it")
      (is (null *calls*) "the CLI never ran on the wrong directory"))))

(deftest snyk-cell-a-code-scan-has-no-critical-rung ()
  (with-snyk-cli ()
    (with-cell-stop ((snyk-start "severity_threshold" "critical"))
      (snyk-call "snyk_code_scan" "{\"path\": \"/tmp/project\"}")
      (is (member "--severity-threshold=high" (first *calls*) :test #'equal) "the section's critical is high where Snyk Code has no such rung")
      (snyk-call "snyk_sca_scan" "{\"path\": \"/tmp/project\"}")
      (is (member "--severity-threshold=critical" (first *calls*) :test #'equal) "and critical stands where the scanner does have it"))))

;;; --- what the answer says ------------------------------------------------

(deftest snyk-cell-an-answer-says-how-the-run-ended ()
  (with-snyk-cli (:status 1 :out "{\"vulnerabilities\": []}")
    (is (search "exit 1 (issues found)" (snyk-call "snyk_sca_scan" "{\"path\": \"/tmp/project\"}")) "one is findings, not an error"))
  (with-snyk-cli (:status 3)
    (is (search "exit 3 (no supported projects found)"
                (snyk-call "snyk_sca_scan" "{\"path\": \"/tmp/project\"}")) "three is a manifest Snyk does not read"))
  (with-snyk-cli (:status :timeout)
    (let ((answer (snyk-call "snyk_code_scan" "{\"path\": \"/tmp/project\"}")))
      (is (search "timeout_seconds" answer) "a run that passed its bound says so")
      (is (search "killed" answer) "and that it was killed")))
  (with-snyk-cli (:status 2 :err "not authenticated: run snyk auth")
    (is (search "stderr: not authenticated" (snyk-call "snyk_code_scan" "{\"path\": \"/tmp/project\"}")) "what the CLI complained about reaches the model")))

(deftest snyk-cell-a-long-answer-is-cut-to-the-section-s-limit ()
  (with-snyk-cli (:out (make-string 4000 :initial-element #\x))
    (with-cell-stop ((snyk-start "max_output_chars" 500))
      (let ((answer (snyk-call "snyk_code_scan" "{\"path\": \"/tmp/project\"}")))
        (is (search "cut at 500 of 4000 characters" answer) "the cut is said, with both sizes")
        (is (< (length answer) 1500) "and the answer stays small")))))

;;; --- the section names the CLI -------------------------------------------

(deftest snyk-cell-the-section-names-the-cli-that-runs ()
  (with-snyk-cli ()
    (with-cell-stop ((snyk-start "command" (vector "npx" "-y" "snyk@1.1306.0")))
      (snyk-call "snyk_version" "{}")
      (snyk-ran '("npx" "-y" "snyk@1.1306.0" "--version")
                "the pinned release runs without a global install"))))

(deftest snyk-cell-a-cli-that-is-not-installed-answers-words ()
  (with-snyk-cli ()
    (multiple-value-bind (out err status)
        (nodecode-snyk::run-cli '("snyk-not-installed-in-this-image" "--version"))
      (declare (ignore out))
      (is (eql 127 status) "a shell's answer for a command it cannot find")
      (is (search "npm install -g snyk" err) "names the install")
      (is (search "npx" err) "and the section that needs no install"))))

;;; --- the operator's command ----------------------------------------------

(deftest snyk-cell-the-command-reports-the-cli-and-the-account ()
  (with-snyk-cli (:out "1.1306.0")
    (let ((answer (nodecode-snyk::run-command "" nil)))
      (is (search "1.1306.0" answer) "the version the CLI answered")
      (snyk-ran '("snyk" "whoami") "the account is asked last")
      (is (equal '("snyk" "--version") (second *calls*)) "the version is asked first")
      (is (search "/snyk login" answer) "and the answer says how to sign in")))
  (with-snyk-cli ()
    (nodecode-snyk::run-command "login" nil)
    (snyk-ran '("snyk" "auth") "login signs in")
    (nodecode-snyk::run-command "logout" nil)
    (snyk-ran '("snyk" "logout") "logout ends the session")
    (is (search "one of: login, logout, version" (nodecode-snyk::run-command "frobnicate" nil)) "and a word it does not know is told what it takes")))

;;; --- the manual ----------------------------------------------------------

(deftest snyk-cell-the-manual-carries-the-original-s-skill ()
  (with-snyk-cli ()
    (let ((text (cddr (assoc :snyk nle::*help-topics*))))
      (is (search "snyk_code_scan" text) "every tool is named")
      (is (search "snyk_sca_scan" text) "the dependency scanner too")
      (is (search "absolute" text) "with the path rule")
      (is (search "python3" text) "and the Python one")
      (is (search "Snyk's cloud" text) "and that a scan uploads code")
      (is (search "Ask before scanning a private" text) "so the model asks first"))))
