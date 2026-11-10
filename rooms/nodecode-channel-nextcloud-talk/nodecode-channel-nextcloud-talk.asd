;;;; nodecode-channel-nextcloud-talk.asd --- Nextcloud Talk channel adapter cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-CELL when channels.nextcloud-talk
;;;; is present in the shared config. The .asd sits inside its own folder: the
;;;; kit finds it through the folder registry. The account reaches Talk over
;;;; its own OCS REST API, so the organism needs no public address.

(defsystem "nodecode-channel-nextcloud-talk"
  :description "Talk to nodecode through a Nextcloud Talk account"
  :license "MIT"
  :version "0.1.0"
  :author "smilinTux (original), Nodecode (rewrite)"
  :depends-on ("nodecode-channel-kit" "cl-base64")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-channel-nextcloud-talk/test"))))

(defsystem "nodecode-channel-nextcloud-talk/test"
  :description "Nextcloud Talk adapter tests, registered under the CHANNEL-NEXTCLOUD-TALK- prefix."
  :license "MIT"
  :author "smilinTux (original), Nodecode (rewrite)"
  :depends-on ("nodecode-channel-nextcloud-talk" "nodecode-channel-kit/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (declare (ignore operation component))
                    (uiop:symbol-call '#:nodecode.test '#:run-channel-tests)))
