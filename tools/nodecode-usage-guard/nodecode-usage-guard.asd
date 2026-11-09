;;;; nodecode-usage-guard.asd --- subscription usage windows, watched before they are spent.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `usage-guard' section, puts (help :usage-guard) on the manual,
;;;; registers the usage_guard tool and /usage-guard, and watches every
;;;; turn's first round for a window near its limit.
;;;;
;;;; Rewritten from mister-ideas/usage-guard (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-usage-guard"
  :description "Usage guard: reads subscription usage windows and says when one is spent"
  :license "MIT"
  :author "mister-ideas (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-usage-guard/test"))))

(defsystem "nodecode-usage-guard/test"
  :description "Usage guard tests. Registered into the shared nodecode.test registry; RUN-USAGE-GUARD-TESTS filters by the USAGE-GUARD-CELL- name prefix."
  :license "MIT"
  :author "mister-ideas (original), Nodecode (rewrite)"
  :depends-on ("nodecode-usage-guard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-usage-guard-tests)))
