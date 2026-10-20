;;;; nodecode-channel-whatsapp.asd --- WhatsApp channel adapter cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-CELL when channels.whatsapp is
;;;; present in the shared config. The .asd sits inside its own folder so the
;;;; kit finds it through the folder registry; it is installed from the add-on
;;;; hub (/setup, Choose) or cloned into ~/.nodecode/cells/.
;;;;
;;;; WhatsApp is reached as a linked device through driver/, a Go program
;;;; over whatsmeow that this cell builds on its first start and speaks to over
;;;; stdin and stdout: no public address, no Meta business account.

(defsystem "nodecode-channel-whatsapp"
  :description "Talk to nodecode through WhatsApp, as a linked device"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-channel-kit")
  :serial t
  :components ((:file "package")
               (:file "markdown")
               (:file "events")
               (:file "plans")
               (:file "driver")
               (:file "adapter")
               (:file "probe")
               (:static-file "driver/main.go")
               (:static-file "driver/go.mod")
               (:static-file "driver/go.sum"))
  :in-order-to ((test-op (test-op "nodecode-channel-whatsapp/test"))))

(defsystem "nodecode-channel-whatsapp/test"
  :description "WhatsApp adapter tests, registered under the CHANNEL-WHATSAPP- prefix."
  :license "MIT"
  :depends-on ("nodecode-channel-whatsapp" "nodecode-channel-kit/test")
  :pathname "test/"
  :serial t
  :components ((:file "fake-driver")
               (:file "markdown-test")
               (:file "events-test")
               (:file "plans-test")
               (:file "driver-test")
               (:file "adapter-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-whatsapp-tests)))
