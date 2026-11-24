;;;; nodecode-klipper.asd --- a Klipper printer's state and job, through Moonraker.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `klipper' section, registers the three printer tools and
;;;; /klipper, and puts the manual on (help :klipper).

(defsystem "nodecode-klipper"
  :description "Klipper printer over Moonraker: progress, temperatures, pause, resume, cancel"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "watch")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-klipper/test"))))

(defsystem "nodecode-klipper/test"
  :description "Klipper tests. Registered into the shared nodecode.test registry; RUN-KLIPPER-TESTS filters by the KLIPPER-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-klipper" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-klipper-tests)))
