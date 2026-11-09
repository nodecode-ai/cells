;;;; nodecode-trove.asd --- lossless recall over every session's log.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of misterdas/hermes-trove. Optional cell, not part of the
;;;; organism core: the folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/, and the gateway calls START-CELL after
;;;; recovery, which reads the `trove' section and registers six tools and
;;;; /trove. Nothing is kept of its own: every answer reads the durable
;;;; event log the image already writes.

(defsystem "nodecode-trove"
  :description "Lossless recall: search, read and cite every session's durable log"
  :license "MIT"
  :author "misterdas (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-trove/test"))))

(defsystem "nodecode-trove/test"
  :description "Trove tests. Registered into the shared nodecode.test registry; RUN-TROVE-TESTS filters by the TROVE-CELL- name prefix."
  :license "MIT"
  :author "misterdas (original), Nodecode (rewrite)"
  :depends-on ("nodecode-trove" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-trove-tests)))
