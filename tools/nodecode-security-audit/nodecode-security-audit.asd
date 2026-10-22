;;;; nodecode-security-audit.asd --- a knowledge cell: Cloudflare's security-audit
;;;; skill, read by the index and never loaded.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One prose define-skill, security-audit.lisp, which the index lists and
;;;; (help 'security-audit) reads; skill/ beside it is Cloudflare's
;;;; skills/security-audit folder, unchanged, whose guides, schema and
;;;; validators that skill names. Nothing here is compiled or run at boot.

(defsystem "nodecode-security-audit"
  :description "Cloudflare's security-audit skill: multi-phase security audits with independently verified, machine-readable findings"
  :license "MIT"
  :version "0.1.0"
  :class nodecode.kernel:layer
  :depends-on ("nodecode")
  :properties ((:knowledge . t))
  :components nil)
