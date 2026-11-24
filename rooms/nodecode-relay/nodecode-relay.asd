;;;; nodecode-relay.asd --- HTTPS-grade sign-in and pairing in front of the page.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: the folder's own listener sits in
;;;; front of the local page, and every request through it carries a password
;;;; session or a paired device's token.

(defsystem "nodecode-relay"
  :description "Relay: a second listener in front of your page, with a password and paired devices"
  :license "MIT"
  :author "sorsama (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "auth")
               (:file "server")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-relay/test"))))

(defsystem "nodecode-relay/test"
  :description "Relay tests: the gate, the pairing codes and the proxied page, offline."
  :license "MIT"
  :depends-on ("nodecode-relay" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:nodecode.test '#:run-relay-tests)))
