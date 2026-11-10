;;;; nodecode-channel-xchat.asd --- X Chat (encrypted X DMs) adapter.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-CELL when channels.xchat is
;;;; present in the shared config. The .asd sits inside its own folder so the
;;;; kit finds it through the folder registry.

(defsystem "nodecode-channel-xchat"
  :description "Talk to nodecode over X Chat: X's end-to-end encrypted direct messages"
  :license "MIT"
  :version "0.1.0"
  :author "NousResearch (original), Nodecode (rewrite)"
  ;; The kit brings what the wire is made of (cl+ssl, usocket, babel,
  ;; cl-base64, cl-ppcre); ironclad's SHA-256 is what the sealing is made of.
  :depends-on ("nodecode-channel-kit" "ironclad/digest/sha256")
  :serial t
  :components ((:file "package")
               (:file "crypto")
               (:file "rest")
               (:file "adapter"))
  :in-order-to ((test-op (test-op "nodecode-channel-xchat/test"))))

(defsystem "nodecode-channel-xchat/test"
  :description "X Chat adapter tests, registered under the CHANNEL-XCHAT- prefix."
  :license "MIT"
  :depends-on ("nodecode-channel-xchat" "nodecode-channel-kit/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (declare (ignore operation component))
                    (uiop:symbol-call '#:nodecode.test '#:run-channel-tests)))
