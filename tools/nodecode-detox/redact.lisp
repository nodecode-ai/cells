;;;; redact.lisp --- the shapes of a secret this cell knows how to take out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original handed this to the host's own redaction helpers, and said
;;;; plainly that pattern matching cannot catch every secret. The rewrite
;;;; carries its own small set of patterns, and says the same: what a pattern
;;;; does not know is stored as it arrived. Every record is redacted before it
;;;; is written and before a report quotes any of its lines.

(in-package #:nodecode-detox)

(defparameter +redactions+
  ;; (pattern . replacement), in order: a token's shape first, then the
  ;; `key: value' and `Bearer token' shapes that carry one.
  '(;; provider and platform tokens, by their own prefixes
    ("\\b(sk-[A-Za-z0-9_-]{16,})" . "sk-[redacted]")
    ("\\b(pk-[A-Za-z0-9_-]{16,})" . "pk-[redacted]")
    ("\\b(gh[pousr]_[A-Za-z0-9]{20,})" . "gh*_[redacted]")
    ("\\b(github_pat_[A-Za-z0-9_]{20,})" . "github_pat_[redacted]")
    ("\\b(xox[abposr]-[A-Za-z0-9-]{10,})" . "xox*-[redacted]")
    ("\\b(AKIA[0-9A-Z]{16})" . "AKIA[redacted]")
    ("\\b(AIza[0-9A-Za-z_-]{30,})" . "AIza[redacted]")
    ("\\b(hf_[A-Za-z0-9]{20,})" . "hf_[redacted]")
    ("\\b(npm_[A-Za-z0-9]{20,})" . "npm_[redacted]")
    ;; a credential said after a scheme word, and a JSON field's value
    ("(?i)(bearer\\s+)[A-Za-z0-9._~+/=-]{12,}" . "\\1[redacted]")
    ("(?i)(\"(?:api[_-]?key|token|secret|password|passwd|authorization)\"\\s*:\\s*\")[^\"]{8,}\""
     . "\\1[redacted]\"")
    ;; a `KEY=value' or `key: value' assignment on a line
    ("(?i)\\b([A-Za-z0-9_]*(?:api[_-]?key|token|secret|password|passwd))\\s*[=:]\\s*[^\\s\"']{8,}"
     . "\\1=[redacted]")))

(defparameter +private-key-scanner+
  (cl-ppcre:create-scanner "-----BEGIN [A-Z ]*PRIVATE KEY-----[\\s\\S]*?-----END [A-Z ]*PRIVATE KEY-----")
  "A whole PEM block, its body and all.")

(defun redact-secrets (text)
  "TEXT with the secrets these patterns know written `[redacted]'."
  (let ((clean (cl-ppcre:regex-replace-all +private-key-scanner+ text
                                           "-----BEGIN [redacted] PRIVATE KEY-----")))
    (loop for (pattern . replacement) in +redactions+
          do (setf clean (cl-ppcre:regex-replace-all pattern clean replacement)))
    clean))
