;;;; nodecode-channel-imessage.asd --- iMessage channel adapter add-on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-CELL when channels.imessage is
;;;; present in the shared config. The .asd sits inside its own folder so the
;;;; kit finds it through the folder registry.
;;;;
;;;; iMessage has no bot API: the bot is an Apple ID signed into Messages on a
;;;; Mac that runs a BlueBubbles server (https://github.com/BlueBubblesApp/bluebubbles-server,
;;;; Apache-2.0, self-hosted). The organism polls that server's REST API, so it
;;;; needs no public address and holds no socket; the Mac needs no route to it.

(defsystem "nodecode-channel-imessage"
  :description "Talk to nodecode over iMessage, through a BlueBubbles server"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-channel-kit")
  :serial t
  :components ((:file "package")
               (:file "text")
               (:file "events")
               (:file "rest")
               (:file "probe")
               (:file "adapter"))
  :in-order-to ((test-op (test-op "nodecode-channel-imessage/test"))))

(defsystem "nodecode-channel-imessage/test"
  :description "iMessage adapter tests, registered under the CHANNEL-IMESSAGE- prefix."
  :license "MIT"
  :depends-on ("nodecode-channel-imessage" "nodecode-channel-kit/test")
  :pathname "test/"
  :serial t
  :components ((:file "fake-bluebubbles")
               (:file "text-test")
               (:file "events-test")
               (:file "rest-test")
               (:file "adapter-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-imessage-tests)))
