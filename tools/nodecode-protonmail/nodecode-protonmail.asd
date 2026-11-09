;;;; nodecode-protonmail.asd --- Proton Mail read straight from Proton's API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `protonmail' section and registers the four tools.
;;;;
;;;; Rewritten from Maytrella007's Hermes Agent plugin; see NOTICE.

(defsystem "nodecode-protonmail"
  :description "Proton Mail without Bridge: folders, inbox, a decrypted message, the read flag"
  :author "Maytrella007 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "ironclad" "cl-base64" "babel")
  :serial t
  :components ((:file "package")
               (:file "srp")
               (:file "api")
               (:file "send")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-protonmail/test"))))

(defsystem "nodecode-protonmail/test"
  :description "Proton Mail tests, against a scripted API and a scripted gpg; nothing leaves the machine."
  :author "Maytrella007 (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-protonmail" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-protonmail-tests)))
