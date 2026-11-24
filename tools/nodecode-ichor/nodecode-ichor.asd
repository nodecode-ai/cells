;;;; nodecode-ichor.asd --- the moments you correct the agent, as preference pairs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of mystiquemide's ichor (MIT).
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL, which reads
;;;; the `ichor' section, hooks the :FRAME point so a completed turn that
;;;; answers a correction is recorded, registers /ichor and puts the manual on
;;;; (help :ichor). Pairs live in one JSONL file under the home; nothing
;;;; leaves the machine unless the user runs /ichor share.

(defsystem "nodecode-ichor"
  :description "Corrections captured as preference pairs, scrubbed locally"
  :license "MIT"
  :author "mystiquemide (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ichor/test"))))

(defsystem "nodecode-ichor/test"
  :description "Ichor tests. Registered into the shared nodecode.test registry; RUN-ICHOR-TESTS filters by the ICHOR-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-ichor" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ichor-tests)))
