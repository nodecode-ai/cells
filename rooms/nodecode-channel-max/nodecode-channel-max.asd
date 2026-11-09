;;;; nodecode-channel-max.asd --- MAX messenger channel adapter cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-CELL when channels.max is
;;;; enabled in the shared config. The .asd sits inside its own folder:
;;;; the kit finds it through the folder registry.

(defsystem "nodecode-channel-max"
  :description "Talk to nodecode through a MAX messenger bot"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-channel-kit")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-channel-max/test"))))

(defsystem "nodecode-channel-max/test"
  :description "MAX adapter tests, registered under the CHANNEL- prefix."
  :license "MIT"
  :depends-on ("nodecode-channel-max" "nodecode-channel-kit/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (declare (ignore operation component))
                    (uiop:symbol-call '#:nodecode.test '#:run-channel-tests)))
