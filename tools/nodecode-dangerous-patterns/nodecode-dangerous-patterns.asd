;;;; nodecode-dangerous-patterns.asd --- the operator's own shell-command
;;;; patterns, read before a call runs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral cell: one START-CELL, one :TOOL hook, one slash command.
;;;; The folder loader (kernel cells.lisp) loads this system at boot when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; after recovery, which compiles the "dangerous-patterns" section's rules
;;;; and hooks them onto the core's :TOOL point.

(defsystem "nodecode-dangerous-patterns"
  :description "Custom dangerous command patterns: user regex rules that refuse a call or ask first"
  :license "MIT"
  :author "scross01 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-dangerous-patterns/test"))))

(defsystem "nodecode-dangerous-patterns/test"
  :description "Dangerous-patterns tests. Registered into the shared nodecode.test registry; RUN-DANGEROUS-PATTERNS-TESTS filters by the PATTERNS- name prefix."
  :license "MIT"
  :author "scross01 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-dangerous-patterns" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-dangerous-patterns-tests)))
