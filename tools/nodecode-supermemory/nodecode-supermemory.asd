;;;; nodecode-supermemory.asd --- Supermemory memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `supermemory' section, registers the four supermemory_* tools
;;;; and /supermemory, and starts the worker that writes each finished turn
;;;; to the container as a conversation document.
;;;;
;;;; A Nodecode rewrite of supermemoryai/hermes-supermemory (see NOTICE).

(defsystem "nodecode-supermemory"
  :description "Supermemory: long-term memory, a profile and per-turn capture, through Supermemory's cloud or a self-hosted server"
  :license "MIT"
  :version "0.1.0"
  :author "supermemoryai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-supermemory/test"))))

(defsystem "nodecode-supermemory/test"
  :description "Supermemory tests. Registered into the shared nodecode.test registry; RUN-SUPERMEMORY-TESTS filters by the SUPERMEMORY-CELL- name prefix."
  :license "MIT"
  :author "supermemoryai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-supermemory" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-supermemory-tests)))
