;;;; nodecode-plugin-guard.asd --- static review of a plugin or cell folder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, not part of the organism core: the folder loader
;;;; loads it at boot when its directory sits under ~/.nodecode/cells/. It
;;;; registers one read-only model tool, plugin_guard_scan, one command,
;;;; /plugin-guard, and the manual topic (help :plugin-guard). No thread, no
;;;; worker, no network: a review is one walk of a tree and a few hundred
;;;; literal matches over what it read.

(defsystem "nodecode-plugin-guard"
  :description "Static review of a plugin or cell folder: nothing in it is run"
  :license "MIT"
  :version "0.1.0"
  :author "mauricemohr88-debug (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "rules")
               (:file "scan")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-plugin-guard/test"))))

(defsystem "nodecode-plugin-guard/test"
  :description "Plugin-guard tests. Registered into the shared nodecode.test registry; RUN-PLUGIN-GUARD-TESTS filters by the PLUGIN-GUARD-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-plugin-guard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-plugin-guard-tests)))
