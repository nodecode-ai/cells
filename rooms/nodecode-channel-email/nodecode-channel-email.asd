;;;; nodecode-channel-email.asd --- Email channel adapter add-on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-ADDON when channels.email is
;;;; present in the shared config. The .asd sits inside its own folder so
;;;; the kit finds it through the folder registry.
;;;;
;;;; A mailbox is read over IMAP (polled, read-only) and answered over SMTP,
;;;; so the organism needs no public address and no mail server of its own.

(defsystem "nodecode-channel-email"
  :description "Talk to nodecode by email: an IMAP inbox in, SMTP replies out"
  :license "MIT"
  :version "0.1.0"
  ;; The kit brings what the wire is made of (cl+ssl, usocket, babel,
  ;; cl-base64, cl-ppcre) and the organism the rest.
  :depends-on ("nodecode-channel-kit")
  :serial t
  :components ((:file "package")
               (:file "mime")
               (:file "gate")
               (:file "compose")
               (:file "wire")
               (:file "adapter")
               (:file "probe"))
  :in-order-to ((test-op (test-op "nodecode-channel-email/test"))))

(defsystem "nodecode-channel-email/test"
  :description "Email adapter tests, registered under the CHANNEL-EMAIL- prefix."
  :license "MIT"
  :depends-on ("nodecode-channel-email" "nodecode-channel-kit/test")
  :pathname "test/"
  :serial t
  :components ((:file "fake-mail")
               (:file "mime-test")
               (:file "adapter-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-email-tests)))
