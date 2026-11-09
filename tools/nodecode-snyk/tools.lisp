;;;; tools.lisp --- the ten tools, and the CLI flags each one carries.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Each tool is one declaration (NLK:DEFINE-CONTRACT) of the arguments a
;;;; model may pass, and one body that turns the record into a Snyk command
;;;; line. The declaration is the shape the provider is shown and the shape
;;;; the body reads, so the two cannot drift; a member the declaration does
;;;; not carry has no reader the body could use.
;;;;
;;;; The Snyk argument names are the original MCP server's, unchanged, so a
;;;; model that read the plugin's documentation calls the same things. What
;;;; changed is the other end: the MCP server turned them into these same
;;;; CLI flags, and this cell writes them itself.

(in-package #:nodecode-snyk)

;;; --- snyk_code_scan -------------------------------------------------------

(nlk:define-contract code-scan
  (:doc "Static analysis of source code for security vulnerabilities (Snyk Code, SAST), without running it. Give a directory, not a single file: data flow needs the files around it.")
  ("path" :text :required t
   :doc "the absolute path of the file or directory to scan")
  ("severity_threshold" :string :options '("low" "medium" "high")
   :doc "report only findings at or above this severity; Snyk Code has no critical rung")
  ("org" :string :doc "the Snyk organization id or slug the results belong to")
  ("include_ignores" :boolean :doc "include the findings the project's .snyk policy ignores")
  ("debug" :boolean :slot debug-log :doc "add Snyk Code's debug logging to the output"))

(defparameter +code-scan-flags+
  '(("severity_threshold" "--severity-threshold" :value)
    ("org" "--org" :value)
    ("include_ignores" "--include-ignores" :boolean)
    ("debug" "-d" :boolean))
  "The flags snyk code test takes, as (MEMBER FLAG KIND).")

(defun code-scan (report)
  "snyk_code_scan: Snyk Code over one path."
  (run-tool (command-line '("code" "test" "--json")
                           (flag-arguments (encode-code-scan report) +code-scan-flags+
                                           (code-severity (severity (code-scan-severity-threshold report))))
                           (absolute (code-scan-path report)))
            :scan t))

;;; --- snyk_sca_scan --------------------------------------------------------

(nlk:define-contract sca-scan
  (:doc "Known-vulnerable open-source dependencies (Snyk Open Source, SCA), read from the project's manifests and lockfiles. The package manager must be installed to resolve the tree; a Python project needs command \"python3\".")
  ("path" :text :required t :doc "the absolute path of the project directory")
  ("all_projects" :boolean :default t
   :doc "scan every supported manifest under the directory; true unless the call says false")
  ("all_sub_projects" :boolean :doc "scan every sub-project of a multi-project Gradle build")
  ("dev" :boolean :doc "include development-only dependencies (npm, Maven, RubyGems)")
  ("severity_threshold" :string :options '("low" "medium" "high" "critical")
   :doc "report only findings at or above this severity")
  ("org" :string :doc "the Snyk organization id or slug the results belong to")
  ("file" :string :doc "one manifest to inspect instead of auto-detection, e.g. package.json")
  ("package_manager" :string
   :options '("npm" "yarn" "maven" "gradle" "pip" "uv" "composer" "rubygems" "nuget" "golangdep" "govendor" "gomodules")
   :doc "the package manager of a manifest whose name is not the standard one")
  ("command" :string :doc "the Python executable to resolve a Python project with; python3 for a pip or uv project")
  ("exclude" :string :doc "comma-separated directory or file names to leave out of an all_projects scan, e.g. node_modules,tests")
  ("fail_on" :string :options '("all" "upgradable" "patchable")
   :doc "which finding makes the run exit non-zero")
  ("print_deps" :boolean :doc "print the dependency tree before the findings")
  ("show_vulnerable_paths" :string :options '("none" "some" "all")
   :doc "how many vulnerable dependency paths to show")
  ("ignore_policy" :boolean :doc "ignore every .snyk and organization policy rule for this scan")
  ("include_ignores" :boolean :doc "include the findings the policies ignore")
  ("project_name" :string :doc "the project name the results are reported under")
  ("remote_repo_url" :string :doc "the repository URL to associate the project with")
  ("target_reference" :string :doc "a branch or version tag that tells this project's runs apart")
  ("policy_path" :string :doc "a .snyk policy file outside the project root")
  ("strict_out_of_sync" :string :options '("true" "false")
   :doc "whether a lockfile out of step with its manifest fails the scan (npm, pnpm, Yarn)")
  ("skip_unresolved" :boolean :doc "skip the Python packages the environment does not hold")
  ("prune_repeated_subdependencies" :boolean :doc "collapse repeated sub-dependencies in the printed tree")
  ("trust_policies" :boolean :doc "apply the ignore rules that ship inside the dependencies themselves")
  ("yarn_workspaces" :boolean :doc "detect and scan Yarn workspaces")
  ("maven_aggregate_project" :boolean :doc "scan every module of a multi-module Maven project")
  ("unmanaged" :boolean :doc "scan a C++ project, or one no package manager manages, by file signatures")
  ("scan_all_unmanaged" :boolean :doc "find every Maven, JAR, WAR and AAR file recursively"))

(defparameter +sca-scan-flags+
  '(("all_projects" "--all-projects" :boolean)
    ("all_sub_projects" "--all-sub-projects" :boolean)
    ("dev" "--dev" :boolean)
    ("severity_threshold" "--severity-threshold" :value)
    ("org" "--org" :value)
    ("file" "--file" :value)
    ("package_manager" "--package-manager" :value)
    ("command" "--command" :value)
    ("exclude" "--exclude" :value)
    ("fail_on" "--fail-on" :value)
    ("print_deps" "--print-deps" :boolean)
    ("show_vulnerable_paths" "--show-vulnerable-paths" :value)
    ("ignore_policy" "--ignore-policy" :boolean)
    ("include_ignores" "--include-ignores" :boolean)
    ("project_name" "--project-name" :value)
    ("remote_repo_url" "--remote-repo-url" :value)
    ("target_reference" "--target-reference" :value)
    ("policy_path" "--policy-path" :value)
    ("strict_out_of_sync" "--strict-out-of-sync" :value)
    ("skip_unresolved" "--skip-unresolved" :boolean)
    ("prune_repeated_subdependencies" "--prune-repeated-subdependencies" :boolean)
    ("trust_policies" "--trust-policies" :boolean)
    ("yarn_workspaces" "--yarn-workspaces" :boolean)
    ("maven_aggregate_project" "--maven-aggregate-project" :boolean)
    ("unmanaged" "--unmanaged" :boolean)
    ("scan_all_unmanaged" "--scan-all-unmanaged" :boolean))
  "The flags snyk test takes, as (MEMBER FLAG KIND).")

(defun sca-scan (report)
  "snyk_sca_scan: Snyk Open Source over one project."
  (run-tool (command-line '("test" "--json")
                           (flag-arguments (encode-sca-scan report) +sca-scan-flags+
                                           (severity (sca-scan-severity-threshold report)))
                           (absolute (sca-scan-path report)))
            :scan t))

;;; --- snyk_container_scan ---------------------------------------------------

(nlk:define-contract container-scan
  (:doc "Known vulnerabilities in a container image's OS packages and application dependencies. Pass the Dockerfile as file so the remediation names the layer that added each finding.")
  ("image" :text :required t
   :doc "the image to test: a registry name (node:20-alpine), a local image id, or docker-archive:image.tar / oci-archive:image.tar")
  ("file" :string :doc "the absolute path of the Dockerfile the image was built from")
  ("severity_threshold" :string :options '("low" "medium" "high" "critical")
   :doc "report only findings at or above this severity")
  ("org" :string :doc "the Snyk organization id or slug the results belong to")
  ("platform" :string :doc "the platform to test in a multi-architecture image, e.g. linux/amd64")
  ("policy_path" :string :doc "a .snyk policy file outside the project root")
  ("project_name" :string :doc "the project name the results are reported under")
  ("print_deps" :boolean :doc "print the OS and application dependencies before the findings")
  ("exclude_base_image_vulns" :boolean :doc "report only what the application layers added, not what the base image brought")
  ("exclude_app_vulns" :boolean :doc "scan the OS packages alone")
  ("app_vulns" :boolean :doc "scan application dependencies too (the default since CLI 1.1090.0)")
  ("exclude_node_modules" :boolean :doc "leave node_modules out of a Node.js image's application scan"))

(defparameter +container-scan-flags+
  '(("file" "--file" :value)
    ("severity_threshold" "--severity-threshold" :value)
    ("org" "--org" :value)
    ("platform" "--platform" :value)
    ("policy_path" "--policy-path" :value)
    ("project_name" "--project-name" :value)
    ("print_deps" "--print-deps" :boolean)
    ("exclude_base_image_vulns" "--exclude-base-image-vulns" :boolean)
    ("exclude_app_vulns" "--exclude-app-vulns" :boolean)
    ("app_vulns" "--app-vulns" :boolean)
    ("exclude_node_modules" "--exclude-node-modules" :boolean))
  "The flags snyk container test takes, as (MEMBER FLAG KIND).")

(defun container-scan (report)
  "snyk_container_scan: one container image."
  (run-tool (command-line '("container" "test" "--json")
                           (flag-arguments (encode-container-scan report) +container-scan-flags+
                                           (severity (container-scan-severity-threshold report)))
                           (container-scan-image report))
            :scan t))

;;; --- snyk_iac_scan ---------------------------------------------------------

(nlk:define-contract iac-scan
  (:doc "Misconfigurations in infrastructure as code: Terraform (.tf, .tf.json, plan files), Kubernetes, AWS CloudFormation, Azure Resource Manager and Serverless Framework files.")
  ("path" :text :required t :doc "the absolute path of the file or directory to scan")
  ("severity_threshold" :string :options '("low" "medium" "high" "critical")
   :doc "report only misconfigurations at or above this severity")
  ("org" :string :doc "the Snyk organization id or slug the results belong to")
  ("report" :boolean :doc "share the results with the Snyk web UI as a project; it cannot be combined with rules")
  ("target_name" :string :doc "the project name the report is filed under")
  ("target_reference" :string :doc "a branch or commit that tells this project's runs apart")
  ("remote_repo_url" :string :doc "the repository URL to associate the project with")
  ("policy_path" :string :doc "a .snyk policy file outside the scanned directory")
  ("ignore_policy" :boolean :doc "ignore every .snyk and organization policy rule for this scan")
  ("rules" :string :doc "a custom rules bundle (.tar.gz) from the snyk-iac-rules SDK; it cannot be combined with report")
  ("scan" :string :options '("resource-changes" "planned-values")
   :doc "for a Terraform plan: the proposed changes, or the whole planned state")
  ("var_file" :string :doc "a Terraform variable definitions (.tfvars) file outside the scanned directory")
  ("project_tags" :string :doc "the tags the reported project carries, e.g. dept=finance")
  ("project_business_criticality" :string :doc "the reported project's business criticality, e.g. critical,high")
  ("project_environment" :string :doc "the reported project's environment, e.g. frontend,backend")
  ("project_lifecycle" :string :doc "the reported project's lifecycle, e.g. production,sandbox"))

(defparameter +iac-scan-flags+
  '(("severity_threshold" "--severity-threshold" :value)
    ("org" "--org" :value)
    ("report" "--report" :boolean)
    ("target_name" "--target-name" :value)
    ("target_reference" "--target-reference" :value)
    ("remote_repo_url" "--remote-repo-url" :value)
    ("policy_path" "--policy-path" :value)
    ("ignore_policy" "--ignore-policy" :boolean)
    ("rules" "--rules" :value)
    ("scan" "--scan" :value)
    ("var_file" "--var-file" :value)
    ("project_tags" "--project-tags" :value)
    ("project_business_criticality" "--project-business-criticality" :value)
    ("project_environment" "--project-environment" :value)
    ("project_lifecycle" "--project-lifecycle" :value))
  "The flags snyk iac test takes, as (MEMBER FLAG KIND).")

(defun iac-scan (report)
  "snyk_iac_scan: one Terraform, Kubernetes, CloudFormation or ARM tree."
  (run-tool (command-line '("iac" "test" "--json")
                           (flag-arguments (encode-iac-scan report) +iac-scan-flags+
                                           (severity (iac-scan-severity-threshold report)))
                           (absolute (iac-scan-path report)))
            :scan t))

;;; --- snyk_sbom_scan --------------------------------------------------------

(nlk:define-contract sbom-scan
  (:doc "Known vulnerabilities in the components of an existing SBOM: CycloneDX JSON 1.4-1.6 or SPDX JSON 2.3, every component identified by a purl.")
  ("file" :text :required t :doc "the absolute path of the SBOM document")
  ("severity_threshold" :string :options '("low" "medium" "high" "critical")
   :doc "report only findings at or above this severity")
  ("org" :string :doc "the Snyk organization id or slug the results belong to")
  ("debug" :boolean :slot debug-log :doc "add the CLI's debug logging to the output"))

(defparameter +sbom-scan-flags+
  '(("severity_threshold" "--severity-threshold" :value)
    ("org" "--org" :value)
    ("debug" "-d" :boolean))
  "The flags snyk sbom test takes, as (MEMBER FLAG KIND).")

(defun sbom-scan (report)
  "snyk_sbom_scan: one SBOM document."
  (run-tool (command-line '("sbom" "test" "--json")
                           (append (list (concatenate 'string "--file="
                                                      (absolute (sbom-scan-file report) :what "file")))
                                   (flag-arguments (encode-sbom-scan report) +sbom-scan-flags+
                                                   (severity (sbom-scan-severity-threshold report)))))
            :scan t))

;;; --- snyk_aibom ------------------------------------------------------------

(nlk:define-contract aibom
  (:doc "An AI bill of materials (CycloneDX 1.6 JSON) of a Python project: the models, datasets and tools it uses. A gated preview feature: it needs the CLI's preview release channel, and may answer that it is unavailable.")
  ("path" :text :required t :doc "the absolute path of the Python project directory")
  ("json_file_output" :string :doc "the absolute path to write the AIBOM to instead of reading it here"))

(defparameter +aibom-flags+
  '(("json_file_output" "--json-file-output" :value))
  "The flags snyk aibom takes, as (MEMBER FLAG KIND).")

(defun aibom (report)
  "snyk_aibom: one Python project's AI bill of materials."
  (run-tool (command-line '("aibom")
                           (flag-arguments (encode-aibom report) +aibom-flags+)
                           (absolute (aibom-path report)))
            :scan nil))

;;; --- the account's own verbs ------------------------------------------------

(nlk:define-contract auth
  (:doc "Sign the Snyk CLI in to an account. It opens a browser window and waits for the login to finish there; a token passed instead is stored without a browser. The token lives in the Snyk CLI's own config, never in Nodecode.")
  ("token" :string :doc "an API token to store instead of signing in through a browser"))

(defun auth (report)
  "snyk_auth: the browser sign-in, or a token."
  (run-tool (command-line '("auth")
                           (let ((token (auth-token report)))
                             (and token (list token))))
            :scan nil
            :note "a browser window opens unless a token was given: finish the Snyk sign-in there"))

(nlk:define-contract trust
  (:doc "Let the Snyk CLI scan one folder. It is a one-way grant on this machine, asked for only when a scan answers that the folder is not trusted, and only for the folder the operator named.")
  ("path" :text :required t :doc "the absolute path of the folder to trust"))

(defun trust (report)
  "snyk_trust: one folder."
  (run-tool (command-line '("trust") (list "-d" (absolute (trust-path report))))
            :scan nil
            :note "the folder is now trusted for scanning on this machine"))

(defun logout (arguments)
  "snyk_logout: clear the CLI's stored token."
  (declare (ignore arguments))
  (run-tool (command-line '("logout") '()) :scan nil))

(defun version (arguments)
  "snyk_version: the CLI's own version."
  (declare (ignore arguments))
  (run-tool (command-line '("--version") '()) :scan nil))

(defun no-arguments (description)
  "The schema a tool that takes no arguments registers."
  (list :description description
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:json-object))))
