;;;; nodecode-identity.asd --- the identity files, and a line's last use.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `identity' section, registers the six identity_* tools
;;;; and /identity, puts the files into every turn's standing context, and
;;;; hooks the :frame and :tool points that stamp a line the model repeats.
;;;; See README.md beside this file.
;;;;
;;;; Every dependency is already in the serving image. The .asd sits INSIDE
;;;; its folder: install by putting — or symlinking — the directory under
;;;; ~/.nodecode/cells/.

(defsystem "nodecode-identity"
  :description "The identity files, when each line was last used, and the profiles"
  :license "MIT"
  :version "0.1.0"
  :author "indigokarasu (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-identity/test"))))

(defsystem "nodecode-identity/test"
  :description "Identity tests. Registered into the shared nodecode.test registry; RUN-IDENTITY-TESTS filters by the IDENTITY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-identity" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-identity-tests)))
