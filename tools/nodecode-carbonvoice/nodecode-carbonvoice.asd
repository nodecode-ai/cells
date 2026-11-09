;;;; nodecode-carbonvoice.asd --- Carbon Voice chats, read and answered from a session.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `carbonvoice' section, registers the four carbonvoice_* tools,
;;;; the /carbonvoice command, and puts the primer on (help :carbonvoice).
;;;;
;;;; Rewritten from PhononX's Hermes Agent plugin of the same name; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to the Carbon Voice
;;;; API and quri escapes the ids that ride a path.

(defsystem "nodecode-carbonvoice"
  :description "Carbon Voice: read the messages that came in, and answer in the thread"
  :author "PhononX (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-carbonvoice/test"))))

(defsystem "nodecode-carbonvoice/test"
  :description "Carbon Voice tests. Registered into the shared nodecode.test registry; RUN-CARBONVOICE-TESTS filters by the CARBONVOICE-CELL- name prefix."
  :author "PhononX (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-carbonvoice" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-carbonvoice-tests)))
