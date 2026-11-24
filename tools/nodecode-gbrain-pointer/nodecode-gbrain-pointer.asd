;;;; nodecode-gbrain-pointer.asd --- GBrain pointers as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `gbrain-pointer' section, registers gbrain_pointer and
;;;; /gbrain-pointer, hooks the :ROUND point, and starts the worker that
;;;; searches GBrain in the background.
;;;;
;;;; A Nodecode rewrite of praggybuilds/hermes-gbrain-pointer (see NOTICE).

(defsystem "nodecode-gbrain-pointer"
  :description "GBrain pointers: a background search of your own GBrain MCP server, injected as recall pointers before a turn"
  :license "MIT"
  :version "0.1.0"
  :author "praggybuilds (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gbrain-pointer/test"))))

(defsystem "nodecode-gbrain-pointer/test"
  :description "GBrain pointer tests. Registered into the shared nodecode.test registry; RUN-GBRAIN-POINTER-TESTS filters by the GBRAIN-POINTER-CELL- name prefix."
  :license "MIT"
  :author "praggybuilds (original), Nodecode (rewrite)"
  :depends-on ("nodecode-gbrain-pointer" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (declare (ignore operation component))
                    (uiop:symbol-call '#:nodecode.test '#:run-gbrain-pointer-tests)))
