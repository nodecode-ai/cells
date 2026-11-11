;;;; nodecode-taint-sentry.asd --- provenance and taint tracking for tool results.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL, which reads the
;;;; `taint-sentry' section, registers two tools, one slash command, its
;;;; manual topic and one :TOOL hook.
;;;;
;;;; Rewritten from YAN-XBT's Hermes Agent plugin of the same name; see NOTICE.
;;;; Everything here is cl-ppcre and hash tables: no network, no key, no model
;;;; in the decision path, nothing written outside the cell's own memory.

(defsystem "nodecode-taint-sentry"
  :description "Stamps every tool result with where it came from, then gates credential reads, egress and instruction-file writes while a session carries untrusted content"
  :author "YAN-XBT (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "sanitize")
               (:file "taint")
               (:file "sources")
               (:file "policy")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-taint-sentry/test"))))

(defsystem "nodecode-taint-sentry/test"
  :description "Taint Sentry tests. Registered into the shared nodecode.test registry; RUN-TAINT-SENTRY-TESTS filters by the TAINT-SENTRY-CELL- name prefix."
  :author "YAN-XBT (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-taint-sentry" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-taint-sentry-tests)))
