;;;; nodecode-airlock.asd --- commands through your Airlock policy gate, with signed receipts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `airlock' section, registers the airlock_run tool, /airlock and
;;;; (help :airlock).
;;;;
;;;; A rewrite of justrunme/hermes-airlock-plugin (see NOTICE). The peer is an
;;;; Airlock HTTP API the operator runs (https://github.com/justrunme/airlock):
;;;; the policy gate, the disposable sandbox and the Ed25519 receipts are its
;;;; work, and this folder is the client half, in Common Lisp, on the
;;;; dependencies Nodecode already carries plus ironclad for the one curve.

(defsystem "nodecode-airlock"
  :description "Run commands through your Airlock policy gate, and check the signed receipt"
  :license "MIT"
  :author "justrunme (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "ironclad" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "jcs")
               (:file "receipts")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-airlock/test"))))

(defsystem "nodecode-airlock/test"
  :description "Airlock tests: the lifecycle, the canonical form, the signed fixture, and a stubbed Airlock for every verb. RUN-AIRLOCK-TESTS filters by the AIRLOCK-CELL- prefix."
  :license "MIT"
  :author "justrunme (original), Nodecode (rewrite)"
  :depends-on ("nodecode-airlock" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "fixtures")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-airlock-tests)))
