;;;; nodecode-windows-devices.asd --- Windows devices and the drivers behind them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, not part of the organism core: the folder loader
;;;; loads it at boot when its directory sits under ~/.nodecode/cells/, and
;;;; the gateway calls START-CELL after recovery, which reads the
;;;; `windows-devices' section, registers the two read-only device tools and
;;;; /windows-devices, and puts the manual on (help :windows-devices).

(defsystem "nodecode-windows-devices"
  :description "Which Windows device is unhappy, what its problem code means and what driver is behind it"
  :license "MIT"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cim")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-windows-devices/test"))))

(defsystem "nodecode-windows-devices/test"
  :description "The device tools answered from a stubbed CIM, no PowerShell on the machine."
  :license "MIT"
  :depends-on ("nodecode-windows-devices" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-windows-devices-tests)))
