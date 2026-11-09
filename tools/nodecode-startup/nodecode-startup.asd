;;;; nodecode-startup.asd --- what launches itself at sign-in, and what was switched off.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `startup' section, registers two tools,
;;;; /startup and the manual topic (help :startup).
;;;;
;;;; A rewrite of Yicheng-Mark/hermes-windows-plugins' windows-startup plugin
;;;; (see NOTICE): the same question -- what runs at sign-in, from where, and
;;;; what the person switched off -- answered for Windows, Linux and macOS.
;;;; Read-only: nothing is enabled, disabled or deleted, and no network is used.

(defsystem "nodecode-startup"
  :description "What launches itself at sign-in, and what was switched off"
  :license "MIT"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-startup/test"))))

(defsystem "nodecode-startup/test"
  :description "Startup tests. Registered into the shared nodecode.test registry; RUN-STARTUP-TESTS filters by the STARTUP-CELL- name prefix."
  :license "MIT"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :depends-on ("nodecode-startup" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-startup-tests)))
