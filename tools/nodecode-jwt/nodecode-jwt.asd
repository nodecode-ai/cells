;;;; nodecode-jwt.asd --- a JSON Web Token decoded: header, claims, expiry.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when this directory sits
;;;; under ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://jwt' is accepted. It reads the `jwt' config section and
;;;; contributes one tool (jwt_decode), one command (/jwt) and one manual topic
;;;; (help :jwt); nothing starts a thread, dials a host or reads a file.

(defsystem "nodecode-jwt"
  :description "Decode a JSON Web Token: header, claims, expiry and warnings"
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "decode")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-jwt/test"))))

(defsystem "nodecode-jwt/test"
  :description "Tests for the jwt cell; RUN-JWT-TESTS filters the shared registry by the JWT- name prefix."
  :license "MIT"
  :depends-on ("nodecode-jwt" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "decode-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-jwt-tests)))
