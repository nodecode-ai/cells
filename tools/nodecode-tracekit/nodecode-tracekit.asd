;;;; nodecode-tracekit.asd --- a Tracekit signer outside the agent, on the :TOOL point.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader (kernel cells.lisp) loads it at boot when
;;;; its directory sits under ~/.nodecode/cells/; the gateway calls this
;;;; system's START-CELL after recovery, which hooks the cell's GATE onto the
;;;; core's :TOOL point and registers /tracekit.
;;;;
;;;; A rewrite of the Hermes plugin in Cygnux-Labs/Tracekit
;;;; (integrations/hermes), MIT, Copyright (c) 2026 Cygnux Labs. The cell
;;;; speaks the signer's own RPC over HTTP from Lisp, so nothing of the
;;;; original (no Python, no tracekit-ai package) is carried.

(defsystem "nodecode-tracekit"
  :description "Tracekit: a signer outside the agent decides every tool call and signs the run"
  :license "MIT"
  :version "0.1.0"
  :author "Cygnux-Labs (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tracekit/test"))))

(defsystem "nodecode-tracekit/test"
  :description "Tracekit tests: a scripted signer on a loopback port, and the hook's verdicts."
  :license "MIT"
  :author "Cygnux-Labs (original), Nodecode (rewrite)"
  :depends-on ("nodecode-tracekit" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tracekit-tests)))
