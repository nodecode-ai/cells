;;;; nodecode-vram.asd --- Ollama's GPU room, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `vram' section, registers the three tools cell.lisp declares
;;;; and puts (help :vram) on the manual.
;;;;
;;;; A rewrite of jimmy77733/coffee-vram (see NOTICE).

(defsystem "nodecode-vram"
  :description "GPU room and the local Ollama models loaded in it: fit checks, unloads, status"
  :license "MIT"
  :version "0.1.0"
  :author "jimmy77733 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-vram/test"))))

(defsystem "nodecode-vram/test"
  :description "The vram cell's tests. Registered into the shared nodecode.test registry; RUN-VRAM-TESTS filters by the VRAM-CELL- name prefix."
  :license "MIT"
  :author "jimmy77733 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-vram" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-vram-tests)))
