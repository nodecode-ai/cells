;;;; nodecode-channel-whatsapp-agent.asd --- WhatsApp Agent Platform channel cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-CELL when channels.whatsapp-agent
;;;; is present in the shared config. Meta's /agent/v1 API is plain HTTPS:
;;;; one long poll in, one message out, no driver and no public address.

(defsystem "nodecode-channel-whatsapp-agent"
  :description "Talk to Nodecode on WhatsApp, through Meta's WhatsApp Agent Platform API"
  :license "MIT"
  :author "ahtishamdilawar (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode-channel-kit" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "format")
               (:file "events")
               (:file "state")
               (:file "media")
               (:file "adapter"))
  :in-order-to ((test-op (test-op "nodecode-channel-whatsapp-agent/test"))))

(defsystem "nodecode-channel-whatsapp-agent/test"
  :description "WhatsApp Agent Platform adapter tests, registered under the CHANNEL- prefix."
  :license "MIT"
  :author "ahtishamdilawar (original), Nodecode (rewrite)"
  :depends-on ("nodecode-channel-whatsapp-agent" "nodecode-channel-kit/test")
  :pathname "test/"
  :serial t
  :components ((:file "adapter-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-channel-tests)))
