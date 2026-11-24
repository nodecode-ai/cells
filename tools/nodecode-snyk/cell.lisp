;;;; cell.lisp --- the primer, the section, the /snyk command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin was an MCP server entry plus one workflow skill. The
;;;; entry is the ten tools declared below; the skill is +PRIMER+, which the
;;;; model reads with (help :snyk) and whose one-line summary rides every
;;;; request. Nothing hooks the provider path: no model is served here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "snyk": {"command": ["snyk"], "timeout_seconds": 600,
;;;;            "max_output_chars": 20000, "severity_threshold": "high"}
;;;; A vetoed section ("enabled": false) installs nothing. The account's
;;;; token is the Snyk CLI's own, so no secret stands in this section.

(in-package #:nodecode-snyk)

(defparameter +primer+
  "Snyk is available through the nodecode-snyk cell: ten tools that run the Snyk CLI on this
machine. Every path they take is absolute — call (sh \"pwd\") first, a relative one is refused.
  snyk_auth        sign in. A browser window opens; finish it there. Once per machine, and
                   again whenever a scan says the account is not authenticated.
  snyk_trust       let Snyk scan one folder, when a scan answers that the folder is not trusted.
                   A one-way grant on this machine: run it only for the folder the operator
                   asked about, after they asked.
  snyk_version     the CLI's version.   snyk_logout   end the account's session.
  snyk_code_scan   static analysis of source (Apex, C/C++, Dart/Flutter, Elixir, Go, Groovy,
                   Java/Kotlin, Javascript, .NET, PHP, Python, Ruby, Rust, Scala, Swift,
                   Typescript, VB.NET). Give it the directory, not one file: data flow needs
                   the files around it.
  snyk_sca_scan    known-vulnerable open-source dependencies, from the project's manifests and
                   lockfiles. The package manager must be installed to resolve the tree; a
                   Python project needs command \"python3\". all_projects is true unless you
                   say false; monorepos want it true, one project wants path on that project.
  snyk_container_scan  OS packages and application dependencies inside a container image. Pass
                   file as the Dockerfile so the fix names the layer that added each finding.
  snyk_iac_scan    misconfigurations in Terraform, Kubernetes, CloudFormation, ARM and
                   Serverless files; for a Terraform plan, scan \"resource-changes\".
  snyk_sbom_scan   known vulnerabilities in an existing SBOM (CycloneDX 1.4-1.6, SPDX 2.3
                   JSON); every component needs a purl.
  snyk_aibom       an AI bill of materials of a Python project. A gated preview feature: it
                   needs the CLI's preview channel and may answer that it is unavailable.
Each scan answers the command it ran, how it ended (exit 0 nothing found, 1 issues found, 2 the
scan failed, 3 no supported project, 127 the CLI is not installed here), then the CLI's own
JSON, cut at max_output_chars.
How to work: sign in, scan the folder the operator named (a large tree wants severity_threshold
\"high\", then medium or low when they want everything), read the findings, name each one as
file:line, the rule and the fix, apply the fixes they agree to, then run the same scan again
and report the before and after counts. Python SCA without command \"python3\" returns nothing
useful. A finding the .snyk policy ignores says so unless include_ignores is set.
Privacy: the scans are network calls — SAST uploads source, SCA uploads the dependency graph —
to Snyk's cloud. Ask before scanning a private or client repository, and never scan a folder
full of secrets or .env files. A free plan has monthly test limits; \"limit reached\" is not a
bug.
The operator signs in, out and reads the CLI's version with /snyk, /snyk login, /snyk logout
and /snyk version."
  "What (help :snyk) answers while the cell runs: the original's workflow skill, in the
cell's voice.")

;;; --- the operator's command --------------------------------------------------

(defun status-text ()
  "The CLI's version, then the account it is signed in to."
  (with-output-to-string (stream)
    (format stream "~a" (run-tool (command-line '("--version") '()) :scan nil))
    (format stream "~a" (run-tool (command-line '("whoami") '()) :scan nil
                                   :note "the account this machine's Snyk CLI is signed in to"))
    (format stream "~%Sign in with /snyk login; end the session with /snyk logout.~%")))

(defun run-command (args session-id)
  "One /snyk invocation: nothing for a status (the CLI's version and the
signed-in account), or the one CLI command the operator names."
  (declare (ignore session-id))
  (let ((word (string-downcase (nlk:trimmed (or args "")))))
    (cond ((member word '("" "status") :test #'string=) (status-text))
          ((member word '("login" "auth") :test #'string=)
           (run-tool (command-line '("auth") '()) :scan nil
                     :note "a browser window opens: finish the Snyk sign-in there"))
          ((string= word "logout")
           (run-tool (command-line '("logout") '()) :scan nil
                     :note "the Snyk CLI's own token is cleared"))
          ((member word '("version" "--version") :test #'string=)
           (run-tool (command-line '("--version") '()) :scan nil
                     :note "the Snyk CLI's version"))
          (t (format nil "/snyk takes nothing for a status, or one of: login, logout, version; it read ~s"
                     (or args ""))))))

(nle:define-cell snyk
  (:section ("snyk")
    (:guide "install the Snyk CLI (npm install -g snyk), or set command to [\"npx\", \"-y\", \"snyk@1.1306.0\"] to run the pinned release without one; sign in with /snyk login; nothing else is needed")
    ("command" :list :default '("snyk")
     :doc "the argv prefix that runs the Snyk CLI: [\"snyk\"] for an install on PATH, [\"npx\", \"-y\", \"snyk@1.1306.0\"] for the pinned release without one")
    ("timeout_seconds" :integer :default 600 :min 1
     :doc "one Snyk run's ceiling; its whole process tree is killed past it")
    ("max_output_chars" :integer :default 20000 :min 200
     :doc "how much of what the CLI wrote one tool result carries")
    ("severity_threshold" :choice :options '("low" "medium" "high" "critical") :default "low"
     :doc "the floor a scan reports when its call names none; low reports everything, and a code scan has no critical rung so reads it as high"))
  (:help :snyk
         "snyk: scan code, dependencies, containers and IaC for vulnerabilities with the Snyk CLI - (help :snyk)"
         +primer+)
  (:tool "snyk_code_scan" (nlk:contract 'code-scan) #'code-scan)
  (:tool "snyk_sca_scan" (nlk:contract 'sca-scan) #'sca-scan)
  (:tool "snyk_container_scan" (nlk:contract 'container-scan) #'container-scan)
  (:tool "snyk_iac_scan" (nlk:contract 'iac-scan) #'iac-scan)
  (:tool "snyk_sbom_scan" (nlk:contract 'sbom-scan) #'sbom-scan)
  (:tool "snyk_aibom" (nlk:contract 'aibom) #'aibom)
  (:tool "snyk_auth" (nlk:contract 'auth) #'auth)
  (:tool "snyk_trust" (nlk:contract 'trust) #'trust)
  (:tool "snyk_logout"
         (no-arguments "End the Snyk CLI's session on this machine by clearing its stored token.")
         #'logout)
  (:tool "snyk_version"
         (no-arguments "The installed Snyk CLI's version, for a compatibility check or a bug report.")
         #'version)
  (:command "snyk" 'run-command
            :description "Snyk: the CLI's version and account, or sign in and out"
            :session nil))
