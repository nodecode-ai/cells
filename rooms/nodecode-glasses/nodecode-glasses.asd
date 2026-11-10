;;;; nodecode-glasses.asd --- smart glasses as a channel, for Nodecode.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of 100yenadmin/glasses-platform (MIT) for Nodecode: the
;;;; original's WebSocket listener beside a gateway is four small HTTP verbs
;;;; under one gateway route here, and the turn is the kernel's own ingress.
;;;; See NOTICE.

(defsystem "nodecode-glasses"
  :description "Smart glasses as a channel: a wearer's words and camera in, short replies out"
  :license "MIT"
  :author "100yenadmin (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-glasses/test"))))

(defsystem "nodecode-glasses/test"
  :description "The device verbs, a query's path into a session, and the camera tool; registered into the shared nodecode.test registry under GLASSES-CELL-."
  :license "MIT"
  :depends-on ("nodecode-glasses" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-glasses-tests)))
