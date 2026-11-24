;;;; nodecode-clack.asd --- encrypted agent-to-agent messaging over a Clack relay.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `clack' section, registers the nine clack_* tools, the /clack
;;;; command and the manual topic, and hands the model the skill's rules in
;;;; (help :clack).
;;;;
;;;; Rewritten from adroidian's Hermes Agent plugin of the same name; see
;;;; NOTICE. Ironclad carries the curve arithmetic (X25519, Ed25519, SHA-256,
;;;; SHA-512); the ChaCha20-Poly1305 AEAD and the relay's RSA pin check are
;;;; here, in the open.

(defsystem "nodecode-clack"
  :description "Clack: end-to-end encrypted, receipted messaging between agent peers over a relay"
  :author "adroidian (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode" "ironclad" "quri")
  :serial t
  :components ((:file "package")
               (:file "crypto")
               (:file "envelope")
               (:file "client")
               (:file "relay")
               (:file "join")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-clack/test"))))

(defsystem "nodecode-clack/test"
  :description "Clack tests: the lifecycle, the relay pin, the AEAD and envelope round trip, and a stubbed relay for every verb. RUN-CLACK-TESTS filters by the CLACK-CELL- prefix."
  :author "adroidian (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :depends-on ("nodecode-clack" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-clack-tests)))
