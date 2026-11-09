;;;; package.lisp --- the package, the peripheral's condition, and the one
;;;; table of what the cell is allowed to inherit.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of s-a-s-k-i-a/hermes-kimi-code: the same delegate-only,
;;;; fail-closed Kimi Code ACP boundary, written against Nodecode's contract.
;;;; See NOTICE.

(defpackage #:nodecode-kimi-code-delegate
  (:use #:cl))

(in-package #:nodecode-kimi-code-delegate)

;; NAME-ERROR and FAIL, plus *KIMI-CODE-DELEGATE*, RUNNING-SETTINGS and
;; DEFINE-VERB (a DEFUN whose body opens with the "not running" refusal).
(nlk:define-peripheral kimi-code-delegate :not-running t)

(defparameter +version+ "0.1.0"
  "This cell's version, told to the CLI in the ACP handshake.")

(defparameter +protocol-version+ 1
  "The one ACP protocol version this client speaks, and refuses.")

(defparameter +models+
  '(("k3" . "kimi-code/k3")
    ("k3-256k" . "kimi-code/k3-256k"))
  "The Kimi models a delegated call may pick, as the CLI names them.")

(defparameter *max-prompt-chars* 100000
  "One delegated prompt's ceiling, past which the call is refused.")

(defparameter *max-wire-line-chars* 1048576
  "One ACP wire line's ceiling; a longer one is a protocol error, not text.")

(defparameter *max-diagnostic-chars* 4096
  "How much of the child's stderr a refusal carries, redacted and bounded.")

(defparameter *max-cli-line-chars* 1000
  "How much of the CLI's output the doctor command quotes back.")

(defparameter +inherited-environment+
  '("PATH" "TMPDIR" "TMP" "TEMP" "LANG" "LC_ALL" "LC_CTYPE"
    "SYSTEMROOT" "WINDIR" "COMSPEC" "PATHEXT"
    "USERPROFILE" "HOMEDRIVE" "HOMEPATH" "APPDATA" "LOCALAPPDATA"
    "KIMI_CODE_HOME" "KIMI_DISABLE_TELEMETRY" "KIMI_CODE_NO_AUTO_UPDATE"
    "KIMI_CLI_NO_AUTO_UPDATE" "KIMI_DISABLE_CRON"
    "HTTP_PROXY" "HTTPS_PROXY" "ALL_PROXY" "NO_PROXY"
    "http_proxy" "https_proxy" "all_proxy" "no_proxy"
    "SSL_CERT_FILE" "SSL_CERT_DIR" "NODE_EXTRA_CA_CERTS")
  "The only names the Kimi child inherits, so unrelated keys, tokens and
credentials of this process never reach it.")

(defparameter +default-command+ "kimi"
  "The CLI looked for on PATH when the section names none.")
