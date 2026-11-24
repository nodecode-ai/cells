;;;; nodecode-skill-lineage.asd --- where each skill version's content came from.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of simin-yuan/hermes-skill-lineage. Optional ASDF cell, not
;;;; part of the organism core: the folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/, and the gateway calls START-CELL
;;;; after recovery, which reads the `skill_lineage' section, registers the two
;;;; tools, /lineage and (help :lineage). Nothing runs between calls: the
;;;; chain is a file under the home.

(defsystem "nodecode-skill-lineage"
  :description "Where each skill version's content came from, in a hash chain you can verify"
  :license "Apache-2.0"
  :author "simin-yuan (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "lineage")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-skill-lineage/test"))))

(defsystem "nodecode-skill-lineage/test"
  :description "Skill-lineage tests. Registered into the shared nodecode.test registry; RUN-SKILL-LINEAGE-TESTS filters by the LINEAGE-CELL- name prefix."
  :license "Apache-2.0"
  :author "simin-yuan (original), Nodecode (rewrite)"
  :depends-on ("nodecode-skill-lineage" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-skill-lineage-tests)))
