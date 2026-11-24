;;;; nodecode-pushover.asd --- Pushover pushes as a tool, a command and a hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `pushover' section, registers the pushover_notify tool, the
;;;; /pushover command and one :frame hook, and puts the primer on
;;;; (help :pushover).
;;;;
;;;; Rewritten from lightx's Hermes Agent plugin of the same name; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to Pushover and
;;;; quri escapes the form body.

(defsystem "nodecode-pushover"
  :description "Pushover: a push notification to your phone when the agent needs you"
  :author "lightx (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-pushover/test"))))

(defsystem "nodecode-pushover/test"
  :description "Pushover tests. Registered into the shared nodecode.test registry; RUN-PUSHOVER-TESTS filters by the PUSHOVER-CELL- name prefix."
  :author "lightx (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-pushover" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pushover-tests)))
