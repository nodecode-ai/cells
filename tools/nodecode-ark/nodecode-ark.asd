;;;; nodecode-ark.asd --- encrypted, deduplicated snapshots of the brain, one file back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `ark' section, registers the ark_checkpoint tool, /ark and
;;;; (help :ark) on the manual.
;;;;
;;;; A Nodecode rewrite of medomohamed22/hermes-ark (see NOTICE).

(defsystem "nodecode-ark"
  :description "Encrypted, deduplicated snapshots of config and cells, restorable one file at a time"
  :license "MIT"
  :version "0.1.0"
  :author "medomohamed22 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ark/test"))))

(defsystem "nodecode-ark/test"
  :description "Ark cell tests. Registered into the shared nodecode.test registry; RUN-ARK-TESTS filters by the ARK-CELL- name prefix."
  :license "MIT"
  :author "medomohamed22 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-ark" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ark-tests)))
