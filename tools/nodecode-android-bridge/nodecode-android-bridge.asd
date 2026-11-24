;;;; nodecode-android-bridge.asd --- an Android phone's verbs, over a relay's JSON routes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. The folder loader loads it at boot
;;;; when its directory sits under ~/.nodecode/cells/; START-CELL reads the
;;;; `android-bridge' section, registers the fifty-eight android_* tools and
;;;; puts the manual on (help :android-bridge).
;;;;
;;;; Rewritten from TechFlipsi's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http is the one dial.

(defsystem "nodecode-android-bridge"
  :description "Android Bridge: drive a paired Android phone over its relay - screen, taps, typing, files, camera, SMS"
  :author "TechFlipsi (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-android-bridge/test"))))

(defsystem "nodecode-android-bridge/test"
  :description "Android Bridge tests. Registered into the shared nodecode.test registry; RUN-ANDROID-BRIDGE-TESTS filters by the ANDROID-BRIDGE-CELL- prefix."
  :author "TechFlipsi (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-android-bridge" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-android-bridge-tests)))
