;;;; nodecode-off-peak.asd --- Ollama Cloud's off-peak window and the ledger of approved work.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, not part of the organism core: the folder loader
;;;; loads it at boot when its directory sits under ~/.nodecode/cells/, and
;;;; the gateway calls START-CELL, which reads the `off-peak' section,
;;;; registers /off-peak, the off_peak tool and (help :off-peak), and starts
;;;; the reminder worker. Everything it reads is local: the image's clock and
;;;; the user's own markdown ledger.

(defsystem "nodecode-off-peak"
  :description "Ollama Cloud's off-peak window, its countdown, and the ledger of approved work"
  :author "onkanat (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-off-peak/test"))))

(defsystem "nodecode-off-peak/test"
  :description "The off-peak cell's tests: the window math, the ledger, approval, the verbs."
  :author "onkanat (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-off-peak" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-off-peak-tests)))
