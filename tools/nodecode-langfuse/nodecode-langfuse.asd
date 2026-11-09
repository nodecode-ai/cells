;;;; nodecode-langfuse.asd --- turns, model calls and tool calls to Langfuse.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `langfuse' section, hooks the :frame and :tool points and
;;;; registers /langfuse. See README.md beside this file for what a batch
;;;; holds.
;;;;
;;;; Every dependency is already in the serving image. The .asd sits INSIDE
;;;; its folder: install by putting — or symlinking — the directory under
;;;; ~/.nodecode/cells/.

(defsystem "nodecode-langfuse"
  :description "Langfuse: one trace per turn, one generation per model call, one span per tool call"
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-langfuse/test"))))

(defsystem "nodecode-langfuse/test"
  :description "Langfuse tests. Registered into the shared nodecode.test registry; RUN-LANGFUSE-TESTS filters by the LANGFUSE-CELL- name prefix."
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode-langfuse" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-langfuse-tests)))
