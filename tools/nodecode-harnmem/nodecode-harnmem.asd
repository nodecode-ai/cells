;;;; nodecode-harnmem.asd --- HarnMem memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `harnmem' section, registers the five harnmem_* tools and
;;;; /harnmem, and rides the standing harness section with the memory
;;;; snapshot.
;;;;
;;;; A Nodecode rewrite of ys118/harnmem's Hermes plugin (see NOTICE).

(defsystem "nodecode-harnmem"
  :description "HarnMem memory: durable, cross-session memory in plain Markdown, shared with the other HarnMem harnesses"
  :license "MIT"
  :version "0.1.0"
  :author "ys118 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-harnmem/test"))))

(defsystem "nodecode-harnmem/test"
  :description "HarnMem tests. Registered into the shared nodecode.test registry; RUN-HARNMEM-TESTS filters by the HARNMEM- name prefix."
  :license "MIT"
  :author "ys118 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-harnmem" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-harnmem-tests)))
