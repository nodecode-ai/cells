;;;; nodecode-openmail.asd --- OpenMail inboxes as nine tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `openmail' section, registers the nine tools and puts the
;;;; primer on (help :openmail).
;;;;
;;;; Rewritten from openmailsh's Hermes Agent plugin; see NOTICE. Every
;;;; dependency rides with nodecode: nlk:http speaks to the service.

(defsystem "nodecode-openmail"
  :description "OpenMail: send, reply, read threads, extract attachment text and manage inboxes"
  :author "openmailsh (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-openmail/test"))))

(defsystem "nodecode-openmail/test"
  :description "OpenMail tests. Registered into the shared nodecode.test registry; RUN-OPENMAIL-TESTS filters by the OPENMAIL-CELL- name prefix."
  :author "openmailsh (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-openmail" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-openmail-tests)))
