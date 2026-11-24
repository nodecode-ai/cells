;;;; nodecode-neuralwatt.asd --- NeuralWatt's read-only analytics as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `neuralwatt' section, registers the nine tools, the /neuralwatt
;;;; command and (help :neuralwatt), and stops cleanly with their inverses.

(defsystem "nodecode-neuralwatt"
  :description "NeuralWatt account views: health, prices, balance, spend, energy and usage"
  :license "MIT"
  :author "0xGingi (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-neuralwatt/test"))))

(defsystem "nodecode-neuralwatt/test"
  :description "NeuralWatt tests: the slice's lifecycle, the tools over stubbed exchanges, a refused section."
  :license "MIT"
  :author "0xGingi (original), Nodecode (rewrite)"
  :depends-on ("nodecode-neuralwatt" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-neuralwatt-tests)))
