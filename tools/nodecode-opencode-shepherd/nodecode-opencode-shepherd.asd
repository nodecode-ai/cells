;;;; nodecode-opencode-shepherd.asd --- watch opencode sessions, and answer them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `opencode' section, registers the seven tools and the /opencode
;;;; command, puts the primer on (help :opencode), and starts the watcher.
;;;;
;;;; Rewritten from okazakee's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http speaks to the
;;;; opencode server the person already runs.

(defsystem "nodecode-opencode-shepherd"
  :description "Watch over opencode sessions: what stopped, what asks, what wants permission, and the reply"
  :author "okazakee (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-opencode-shepherd/test"))))

(defsystem "nodecode-opencode-shepherd/test"
  :description "The shepherd's tests: every opencode request stubbed, no network."
  :author "okazakee (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-opencode-shepherd" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-opencode-shepherd-tests)))
