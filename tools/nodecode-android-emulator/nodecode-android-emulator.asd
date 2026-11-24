;;;; nodecode-android-emulator.asd --- an Android emulator's verbs, over adb.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `android-emulator' section, registers the eighteen tools and
;;;; puts the manual on (help :android-emulator).
;;;;
;;;; Rewritten from PIR8-Software's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:run-bounded starts adb,
;;;; nlk:define-contract types each tool's arguments, and the paths a write may
;;;; land under are checked against the home, the cwd and the temp dir.

(defsystem "nodecode-android-emulator"
  :description "Android Emulator: drive a headless emulator with adb - screenshots, taps, apps, GPS, battery, logcat"
  :author "PIR8-Software (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-android-emulator/test"))))

(defsystem "nodecode-android-emulator/test"
  :description "Android Emulator tests. Registered into the shared nodecode.test registry; RUN-ANDROID-EMULATOR-TESTS filters by the ANDROID-EMULATOR-CELL- name prefix."
  :author "PIR8-Software (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-android-emulator" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-android-emulator-tests)))
