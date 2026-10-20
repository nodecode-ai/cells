;;;; nodecode-channel-x.asd --- X (Twitter) channel adapter add-on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-CELL when channels.x is present in
;;;; the shared config. The .asd sits inside its own folder so the kit finds it
;;;; through the folder registry.
;;;;
;;;; X has no bot API a session cookie opens, and X's rules allow suspending an
;;;; account that scripts the website. The adapter speaks as an account of its
;;;; own through one child, bridge.py, which computes the web client's
;;;; transaction id with tweetkit-x (https://github.com/nsozturk/tweetkit-x,
;;;; MIT). It answers replies under mentions, never tweets on its own.

(defsystem "nodecode-channel-x"
  :description "Talk to nodecode over X (Twitter), as a bot account of its own"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-channel-kit" "sb-concurrency")
  :serial t
  :components ((:file "package")
               (:file "events")
               (:file "bridge")
               (:file "rest")
               (:file "probe")
               (:file "adapter"))
  :in-order-to ((test-op (test-op "nodecode-channel-x/test"))))

(defsystem "nodecode-channel-x/test"
  :description "X adapter tests, registered under the CHANNEL- prefix."
  :license "MIT"
  :depends-on ("nodecode-channel-x" "nodecode-channel-kit/test")
  :pathname "test/"
  :serial t
  :components ((:file "events-test")
               (:file "rest-test")
               (:file "adapter-test"))
  :perform (test-op (operation component)
                    (declare (ignore operation component))
                    (uiop:symbol-call '#:nodecode.test '#:run-channel-tests)))
