;;;; nodecode-secret-handoff.asd --- a password the model never reads, typed
;;;; straight into the page it is for.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A nodecode rewrite of aean0x/hermes-secret-handoff (MIT).
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode's
;;;; src names this system. The folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; after recovery, which reads the `secret-handoff' section, registers the
;;;; request_secret tool and /secret-handoff, puts the manual on
;;;; (help :secret-handoff) and answers a loopback page the operator types a
;;;; password into. The value goes from that form over CDP into the focused
;;;; password field of the browser tab and nowhere else.
;;;;
;;;; Every dependency is already in the serving image: NLK:HTTP for the CDP
;;;; target list, and websocket-driver (a dependency of nodecode itself) for
;;;; the CDP session.

(defsystem "nodecode-secret-handoff"
  :description "A password typed into the page it is for, and never into the transcript"
  :license "MIT"
  :author "aean0x (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-secret-handoff/test"))))

(defsystem "nodecode-secret-handoff/test"
  :description "secret-handoff cell tests: the CDP target list, the injection and the answer page stubbed; no browser, no network."
  :license "MIT"
  :author "aean0x (original), Nodecode (rewrite)"
  :depends-on ("nodecode-secret-handoff" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-secret-handoff-tests)))
