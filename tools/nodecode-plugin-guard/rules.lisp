;;;; rules.lisp --- the rule catalog: what a scan looks for, and how loud it is.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every rule has a stable id, one default severity and one line of
;;;; message, so a finding can be discussed and excluded across runs: the
;;;; ids are the original scanner's subjects in its order, renumbered under
;;;; a PG prefix. The code rules are literal needles matched against the
;;;; lowercased text of one source file, once per rule per file -- no
;;;; parser, no import, and the reason the report calls itself a review aid
;;;; rather than a verdict.

(in-package #:nodecode-plugin-guard)

(defparameter *severities* '("info" "low" "medium" "high" "critical")
  "The severity names, quietest first.")

(defun severity-rank (name)
  "The number of NAME, a severity string, or 0 for one this build does not know."
  (let ((position (position (string-downcase (or name "")) *severities* :test #'string=)))
    (or position 0)))

(defun rank-name (rank)
  "The severity string at RANK."
  (or (nth rank *severities*) "info"))

(defparameter *rules*
  '(("PG001" "high" "manifest"     "no plugin or cell declaration was found")
    ("PG002" "medium" "project"    "no LICENSE file was found")
    ("PG003" "low" "project"       "no SECURITY.md policy was found")
    ("PG004" "low" "project"       "no automated tests were found")
    ("PG101" "high" "python"       "dynamic code execution")
    ("PG102" "high" "python"       "unsafe deserialization")
    ("PG103" "high" "python"       "process execution")
    ("PG104" "high" "python"       "sensitive path reference")
    ("PG105" "high" "python"       "TLS verification disabled")
    ("PG106" "info" "python"       "network-capable module imported")
    ("PG107" "medium" "python"     "secret-like environment variable read")
    ("PG108" "medium" "python"     "destructive filesystem operation")
    ("PG110" "high" "python"       "privileged registration surface")
    ("PG112" "medium" "python"     "concrete outbound request")
    ("PG201" "critical" "secrets"  "secret material in the tree")
    ("PG202" "high" "dependencies" "remote dependency is not pinned")
    ("PG203" "low" "dependencies"  "dependency has no upper bound")
    ("PG204" "high" "dependencies" "remote script piped to a shell"))
  "Every rule: (ID SEVERITY CATEGORY one-line description).")

(defun rule-row (id)
  "The catalog row for ID, or NIL."
  (assoc id *rules* :test #'string=))

(defun rule-severity (id)
  "ID's default severity string."
  (or (second (rule-row id)) "info"))

(defun rule-description (id)
  "ID's one-line description."
  (or (fourth (rule-row id)) "finding"))

(defparameter *code-rules*
  '(("PG101" ("eval(" "exec(" "__import__(" "compile("))
    ("PG102" ("pickle.load" "marshal.load" "yaml.load(" "dill.load" "torch.load("))
    ("PG103" ("subprocess." "os.system(" "os.popen(" "popen("
              "uiop:run-program" "run-program" "shell-command"))
    ("PG104" ("/.ssh/" "id_rsa" "id_ed25519" "/.aws/" "/.netrc"
              "/.kube/config" "/.gnupg/" "login data" "cookies.sqlite" "wallet.dat"))
    ("PG105" ("verify=false" "verify = false" ":verify nil" "insecureskipverify"))
    ("PG106" ("import requests" "import httpx" "import aiohttp" "import socket"
              "urllib.request" "import openai" "drakma" "dexador" "nlk:http"))
    ("PG108" ("rmtree(" "os.remove(" "os.unlink(" ".unlink(" "delete-directory"
              "delete-file" "rm -rf" "shutil.move("))
    ("PG110" ("allow_tool_override" "pre_gateway_dispatch" "register_middleware"
              "inject_message" "unregister-tool"))
    ("PG112" ("requests.get(" "requests.post(" "httpx.get(" "httpx.post("
              "urlopen(" "http.client" "http-fetch" "curl " "wget ")))
  "Source rules: (ID NEEDLES), the needles lowercase literals. A match
reports the first one the file carries, with its line.")

(defparameter *code-types* '("py" "pyi" "lisp" "asd" "cl" "js" "mjs" "cjs" "ts"
                             "sh" "bash" "zsh" "rb" "go" "rs" "lua" "php" "pl")
  "The extensions a source rule reads; everything else is read for secrets only.")

(defparameter *secret-needles*
  '(("-----BEGIN" . "possible private key")
    ("ghp_" . "possible GitHub token")
    ("gho_" . "possible GitHub token")
    ("ghu_" . "possible GitHub token")
    ("ghs_" . "possible GitHub token")
    ("xoxb-" . "possible Slack token")
    ("xoxp-" . "possible Slack token")
    ("akia" . "possible AWS access key")
    ("sk-" . "possible API key"))
  "Literal prefixes that look like committed credentials, and what they look like.")

(defparameter *sensitive-names*
  '(".env" ".netrc" ".npmrc" "auth.json" "credentials.json"
    "id_dsa" "id_ed25519" "id_rsa")
  "Filenames that usually hold credentials.")

(defparameter *sensitive-suffixes* '("pem" "key" "p12")
  "Extensions that usually hold credentials.")

(defparameter *standard-env*
  '("api_url" "base_url" "token_env" "home" "path" "user" "shell" "term"
    "lang" "tmp" "tmpdir" "temp" "pwd" "ci")
  "Names an environment read may mention without being a credential.")

(defparameter *secret-env-words* '("api_key" "apikey" "token" "secret" "password" "passwd")
  "Words in an environment variable's name that make it a credential.")

(defun rules-text ()
  "The whole catalog as the (help :plugin-guard) text and /plugin-guard rules print it."
  (with-output-to-string (out)
    (format out "~&plugin-guard rules: a review aid for a plugin or cell folder, never a verdict.~%")
    (dolist (row *rules*)
      (format out "  ~a  ~10a ~a~%" (first row) (string-upcase (second row)) (fourth row)))))
