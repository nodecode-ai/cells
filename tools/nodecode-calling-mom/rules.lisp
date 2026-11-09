;;;; rules.lisp --- which tool calls deserve a second look, as data.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the original's rules.py: the same two tiers and the same
;;;; list, reordered and trimmed to the patterns that survive a reading.
;;;; Nothing here is invented:
;;;;
;;;;   * Claude Code's auto-mode classifier publishes what it blocks by
;;;;     default: download-and-execute, production deploys, force push,
;;;;     destructive git, terraform destroy, secret-manager writes, IAM
;;;;     grants, merging unapproved pull requests, flags that disarm a guard,
;;;;     tunnels, the cloud metadata endpoint.
;;;;   * Codex's shell-command reader flags forced rm and looks through
;;;;     sudo/env/bash -c wrappers; a regex matched anywhere in the text
;;;;     cannot be hidden behind a wrapper or an absolute path.
;;;;   * Real runs: a curl PUT to .../merge bypassed a gh-only list.
;;;;
;;;; critical = irreversible or public, and it may reach the expensive
;;;; advisor. watch = privileged access to another machine: worth one look,
;;;; but never the critical tier's budget.
;;;;
;;;; A rule is (:id NAME :pattern REGEX). The regex is matched anywhere in
;;;; the command text, case insensitively; the first hit names the tier.

(in-package #:nodecode-calling-mom)

(defun gap (stop &optional (chars "[^\\n]"))
  "Up to 256 characters of one line that do not contain another STOP word:
bounded, so a long line stays cheap, and stopping at the next STOP word, so
each start scans only to its neighbour."
  (format nil "(?:(?!\\b(?:~a)\\b)~a){0,256}" stop chars))

(defparameter +gh-write+ "(?:-X\\s*|--request[= ]\\s*|--method[= ]\\s*)(?:PUT|POST|PATCH|DELETE)\\b"
  "An HTTP write of a call that spells its method.")

(defparameter +curl+ "curl|wget")
(defparameter +no-pipe+ "[^\\n|;]")
(defparameter +flags+ "(?:\\s+-{1,2}[\\w=./:@-]+(?:\\s+[\\w=./:@-]+)?){0,8}"
  "A command's own options, loosely: kubectl -n prod, --context p, git -C dir.")

(defparameter +critical-files+
  (list (cons "terraform" "\\.tf$")
        (cons "migration" "migrations?/")
        (cons "dockerfile" "(^|/)Dockerfile$")
        (cons "compose" "docker-compose")
        (cons "ci-workflow" "\\.github/workflows/")
        (cons "manifest" "(^|/)(k8s|kubernetes|manifests|helm|charts)/.*\\.ya?ml$")
        (cons "playbook" "(^|/)(ansible|playbooks)/.*\\.ya?ml$"))
  "Files a change to which is worth a look: (ID . REGEX).")

(defparameter +critical-commands+
  (list
   ;; download and execute
   (cons "download-and-run" (format nil "\\b(?:~a)\\b~a\\|\\s*(?:sudo\\s+)?(?:ba|z|da)?sh\\b" +curl+ (gap +curl+ +no-pipe+)))
   (cons "process-substitution" "\\bbash\\s+<\\(\\s*(?:curl|wget)\\b")
   ;; git: public or irreversible
   (cons "force-push" (format nil "\\bgit\\b~a\\bpush\\b~a(?:--force\\b|\\s-f\\b|--delete\\b|\\s:\\S)" (gap "git") (gap "git")))
   (cons "push-to-main" (format nil "\\bgit\\b~a\\bpush\\b~a\\b(?:main|master)\\b" (gap "git") (gap "git")))
   (cons "tag" "\\bgit\\s+tag\\b(?!\\s+(?:-l\\b|--list\\b))\\s+\\S")
   (cons "reset-hard" "\\bgit\\s+reset\\s+--hard\\b")
   (cons "checkout-all" "\\bgit\\s+checkout\\s+--\\s+\\.")
   (cons "clean" "\\bgit\\s+clean\\s+-\\w*f")
   (cons "stash-drop" "\\bgit\\s+stash\\s+(?:drop|clear)\\b")
   (cons "amend" (format nil "\\bgit\\s+commit\\b~a--amend\\b" (gap "git")))
   (cons "branch-delete" "\\bgit\\s+branch\\s+(?-i:-D)\\b")
   (cons "remote" "\\bgit\\s+remote\\s+(?:add|set-url)\\b")
   ;; GitHub: merge, release, bot commands, access
   (cons "merge-pr" "\\bgh\\s+pr\\s+(?:merge|close)\\b")
   (cons "release" "\\bgh\\s+release\\s+(?:create|delete|upload|edit)\\b")
   (cons "workflow-run" "\\bgh\\s+workflow\\s+run\\b")
   (cons "repo-write" "\\bgh\\s+repo\\s+(?:delete|fork|create|edit|archive)\\b")
   (cons "gh-comment" "\\bgh\\s+(?:issue|pr)\\s+comment\\b")
   (cons "gh-endpoint" (format nil "\\bgh\\s+api\\b~a/(?:merge|releases|comments|dispatches|collaborators)\\b" (gap "gh")))
   (cons "api-write" (format nil "\\bgh\\s+api\\b~a~a" (gap "gh") +gh-write+))
   (cons "api-write" (format nil "\\b(?:curl|wget)\\b~a~a~aapi\\.github\\.com" (gap +curl+) +gh-write+ (gap +curl+)))
   (cons "api-write" (format nil "\\b(?:curl|wget)\\b~aapi\\.github\\.com~a~a" (gap +curl+) (gap +curl+) +gh-write+))
   ;; infrastructure
   (cons "terraform" "\\b(?:terraform|tofu)\\s+(?:apply|destroy)\\b")
   (cons "pulumi" "\\b(?:pulumi|cdk|terragrunt)\\s+destroy\\b")
   (cons "kubectl" (format nil "\\bkubectl\\b~a\\s+(?:apply|delete|replace|drain|cordon|exec|port-forward)\\b" +flags+))
   (cons "kubectl-secret" (format nil "\\bkubectl\\b~a\\bcreate\\s+secret\\b" (gap "kubectl")))
   (cons "helm" "\\bhelm\\s+(?:install|upgrade|uninstall|delete|rollback)\\b")
   (cons "ansible" "\\bansible-playbook\\b")
   (cons "docker-prune" "\\bdocker\\s+(?:system|volume|image)\\s+(?:prune|rm)\\b")
   (cons "compose-down" (format nil "\\bdocker\\s+compose\\s+down\\b~a\\s-v\\b" (gap "docker")))
   ;; data and disks
   (cons "drop" "\\bDROP\\s+(?:TABLE|DATABASE|SCHEMA)\\b")
   (cons "truncate" "\\bTRUNCATE\\s+TABLE\\b")
   (cons "zfs" "\\bzfs\\s+destroy\\b")
   (cons "zpool" "\\bzpool\\s+destroy\\b")
   (cons "mkfs" "\\bmkfs(?:\\.\\w+)?\\b")
   (cons "wipefs" "\\bwipefs\\b")
   (cons "dd" "\\bdd\\s+if=\\S+\\s+of=/dev/")
   ;; cloud, secrets, access
   (cons "s3-rm" "\\baws\\s+s3\\s+(?:rm|rb)\\b")
   (cons "gsutil-rm" (format nil "\\bgsutil\\b~a\\brm\\s+-r" (gap "gsutil")))
   (cons "iam" "\\baws\\s+iam\\s+(?:attach|put|create|delete|update)\\w*")
   (cons "azure-role" "\\baz\\s+role\\s+assignment\\b")
   (cons "gcloud-iam" (format nil "\\bgcloud\\b~a\\badd-iam-policy-binding\\b" (gap "gcloud")))
   (cons "secrets" "\\baws\\s+secretsmanager\\s+(?:put|delete|create|update)\\w*")
   (cons "vault" "\\bvault\\s+(?:kv\\s+)?(?:put|delete|destroy)\\b")
   (cons "infisical" "\\binfisical\\s+secrets\\s+(?:set|delete)\\b")
   (cons "certbot" "\\bcertbot\\b")
   (cons "route53" (format nil "\\broute53\\b~a\\bchange-resource-record-sets\\b" (gap "route53")))
   ;; a recursive delete of a broad or unverifiable target: not `rm -f one-file'
   (cons "rm-broad" "\\brm\\s+(?:-\\w+\\s+)*-\\w*[rRf]\\w*\\s+(?:-\\w+\\s+)*(?:--\\s+)?[\"']?(?:/|~|\\$|\\.\\.?(?:/|\\s|$)|\\*)")
   ;; guards switched off, tunnels, instance metadata
   (cons "insecure" (format nil "\\bcurl\\b~a\\s(?:-k|--insecure)\\b" (gap +curl+)))
   (cons "flag-off" "--(?:no-verify|dangerously-skip-permissions|no-sandbox|yes-always)\\b")
   (cons "tunnel" "\\bngrok\\b")
   (cons "tunnel" "\\bcloudflared\\s+tunnel\\b")
   (cons "reverse-tunnel" (format nil "\\bssh\\b~a\\s-R\\b" (gap "ssh")))
   (cons "metadata" "169\\.254\\.169\\.254"))
  "The critical tier: (ID . REGEX), matched anywhere in the command text.")

(defparameter +watch-commands+
  (list (cons "remote-shell" "\\b(?:ssh|scp|rsync|pct|qm)\\s"))
  "The watch tier: privileged access to another machine, one look each.")

(defun scanners (rules extra)
  "RULES plus EXTRA patterns, compiled: a list of (ID SCANNER)."
  (append (loop for (id . pattern) in rules
                 collect (list id (cl-ppcre:create-scanner pattern :case-insensitive-mode t)))
          (loop for pattern in extra
                when (and (stringp pattern) (plusp (length pattern)))
                  collect (list "yours" (ignore-errors
                                          (cl-ppcre:create-scanner pattern :case-insensitive-mode t))))))

(defun first-hit (text scanned)
  "The ID of the first rule in SCANNED that TEXT matches, or NIL."
  (loop for (id scanner) in scanned
        when (and scanner (cl-ppcre:scan scanner text))
          return id))

(defun classify (command &key extra (critical +critical-commands+) (watch +watch-commands+))
  "=> (values TIER ID) for a command line: :CRITICAL, :WATCH, or NIL.
EXTRA adds critical regexes of the operator's own; CRITICAL and WATCH
replace the defaults when given."
  (let* ((text (if (stringp command) command (format nil "~a" command)))
         (id (first-hit text (scanners critical extra))))
    (if id
        (values :critical id)
        (let ((id (first-hit text (scanners watch nil))))
          (if id (values :watch id) (values nil nil))))))

(defun critical-file (path &key (patterns +critical-files+))
  "=> (values T ID) when PATH is a file a change to which is critical."
  (let ((text (if (stringp path) path (format nil "~a" path))))
    (loop for (id . pattern) in patterns
          for scanner = (cl-ppcre:create-scanner pattern :case-insensitive-mode t)
          when (cl-ppcre:scan scanner text)
            do (return (values t id)))))
