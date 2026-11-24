;;;; nodecode-iroh.asd --- agents on two machines exchange tasks over Iroh QUIC.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `iroh' section, puts the iroh_peer_* tools on the model's
;;;; catalog, /iroh on the slash catalog and the primer on (help :iroh).
;;;;
;;;; Rewritten from rahlquist's hermes-iroh-interconnect (see NOTICE). The
;;;; Iroh endpoint itself is the original's Rust sidecar, driven over its
;;;; stdio JSON-RPC; everything around it is Lisp here.

(defsystem "nodecode-iroh"
  :description "Iroh interconnect: pair agents by ticket and exchange tasks with them over Iroh QUIC"
  :license "MIT"
  :author "rahlquist (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "iroh")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-iroh/test"))))

(defsystem "nodecode-iroh/test"
  :description "Iroh interconnect tests. Registered into the shared nodecode.test registry; RUN-IROH-TESTS filters by the IROH-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-iroh" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-iroh-tests)))
