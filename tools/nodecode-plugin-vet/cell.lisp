;;;; cell.lisp --- the declaration: one section, one tool, one command, one topic.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original mounted eight audit skills on the model and ran its checks in
;;;; TypeScript. Here the checks are Lisp in scan.lisp, the method they cite is
;;;; the manual topic below, and the model calls the same verb a person types.

(in-package #:nodecode-plugin-vet)

;;; --- the method, as (help :plugin-vet) reads it -----------------------------------

(defparameter +manual+
  "plugin-vet: a supply-chain gate for anything about to be installed -- a dsh-style
plugin, a cell, an npm package or a folder. One call scores five dimensions and names the
audit skill a person continues with.

Call the plugin_vet tool, or type /plugin-vet, with one target:
  owner/repo[@ref]        a GitHub repository, optionally at a commit
  npm:name@version        a package, as the registry publishes it
  a directory path        what is on this machine, read only

What it reads is data, never code: package.json, the licence file, lockfiles, scripts,
workflows and shipped prose. Nothing is written, nothing is executed. The GitHub API is
asked about the repository itself; the raw host for package.json and the licence; the npm
registry for a package. A GitHub token in the environment variable the section names lifts
the anonymous rate limit and changes nothing else.

The nine checks and the skill section each one hands off to:
  license              dependency-audit §3       SPDX id declared, licence file present, copyleft noted
  sbom                 dependency-audit §7       direct and dev counts, floating versions, lockfile
  commit-lock          supply-chain-review §3    git and workflow references pinned to a 40-character commit
  install-scripts      supply-chain-review §1    preinstall/install/postinstall/prepare: download-and-run, encoded payload, credential or profile writes
  network-exfil        dependency-audit §4.4     receiver hosts, raw IPs and cheap TLDs in scripts and manifests
  obfuscation          supply-chain-review §1    a long encoded blob beside a decoder, generated code
  source               dependency-audit §4.3     archived, forked, stars, age
  maintenance          security-audit §3         last push or publish
  data-responsibility  prompt-injection-review §2 shipped prose that overrides or hides

The five dimensions weigh license 0.25, source 0.20, dependencies 0.15, build scripts 0.25
and maintenance 0.15. The verdict is FAIL when any check fails, WARN when one warns, and
SKIP when nothing could be read at all (usually offline). The gate is the section's
`gate': warn reports only, deny holds a FAIL against the install.

The pack it comes from has eight skills, and each check above points into one of them:
security-audit, dependency-audit, supply-chain-review, prompt-injection-review,
threat-model, secret-scan, vuln-intel, incident-response. This cell carries their checks,
not their prose: read the section a finding names before you act on a green card."
  "What (help :plugin-vet) answers, and the topic every request carries a line of.")

;;; --- the boundary ----------------------------------------------------------------

(nlk:define-contract vet-call
  (:doc "Score the supply-chain risk of a plugin, a cell, an npm package or a folder before it is installed. Read-only: nothing in the target is written or run. Answers a five-dimension risk card whose findings each name the audit skill section to continue with.")
  ("target" :text :required t
   :doc "owner/repo[@ref] for a GitHub repository, npm:name@version for a registry package, or a directory path on this machine")
  ("checks" :array :of :string
   :doc "run only these checks, from license, sbom, commit-lock, install-scripts, network-exfil, obfuscation, source, maintenance, data-responsibility; omitted, all nine run")
  ("policy" :text :options '("inherit" "warn" "deny") :default "inherit"
   :doc "what a FAIL means for the install: inherit the section's gate, warn report only, or deny"))

(defun requested-checks (call)
  (let ((checks (vet-call-checks call)))
    (cond ((null checks) nil)
          ((vectorp checks) (coerce checks 'list))
          ((listp checks) checks)
          (t nil))))

(defun vet-tool (call)
  "The model's call: the card, or the failure it repairs from."
  (handler-case
      (vet (vet-call-target call)
           :requested (requested-checks call)
           :policy (or (vet-call-policy call) "inherit"))
    (plugin-vet-error (condition)
      (nle:failure (format nil "ERROR: ~a" (plugin-vet-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: plugin-vet: ~a" (nlk:one-line (princ-to-string condition)))))))

(defun run-slash (args session-id)
  "The operator's /plugin-vet: the same card, one target typed."
  (declare (ignore session-id))
  (let ((raw (nonblank args)))
    (if (null raw)
        "Plugin Vet: /plugin-vet owner/repo[@ref], npm:name@version, or a directory - a five-dimension supply-chain risk card, read-only. With no target, this line."
        (handler-case
            (vet raw :policy (setting :gate))
          (plugin-vet-error (condition) (plugin-vet-error-detail condition))
          (error (condition)
            (format nil "plugin-vet: ~a" (nlk:one-line (princ-to-string condition))))))))

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell plugin-vet
  (:section ("plugin-vet")
    (:guide "nothing is required: a token only lifts the GitHub rate limit; max_files, max_file_bytes and timeout_seconds bound one scan; gate decides what a FAIL means")
    ("token_env" :env :doc "environment variable holding a GitHub token, for a higher API rate")
    ("max_files" :integer :default 400 :min 1 :doc "how many files one local scan reads")
    ("max_file_bytes" :integer :default 262144 :min 1024 :doc "the ceiling for one file read")
    ("timeout_seconds" :integer :default 15 :min 1 :doc "one request's ceiling")
    ("gate" :choice :options '("warn" "deny") :default "warn"
     :doc "warn reports only; deny holds a FAIL against the install"))
  (:tool "plugin_vet" (nlk:contract 'vet-call) 'vet-tool)
  (:command "plugin-vet" 'run-slash
            :description "Supply-chain risk card for a plugin before you install it"
            :argument-hint "OWNER/REPO[@REF] | NPM:NAME@VERSION | DIR")
  (:help :plugin-vet
         "plugin-vet: a five-dimension supply-chain risk card for a plugin, a package or a folder"
         +manual+))
