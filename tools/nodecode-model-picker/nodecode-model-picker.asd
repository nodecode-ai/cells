;;;; nodecode-model-picker.asd --- each turn takes the cheapest tier that can do it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `model-picker' section, advises NLE:SUBMIT and the :TOOL
;;;; point, registers escalate_model and /model-picker, and puts the manual
;;;; on (help :model-picker). Every dependency is already in the serving
;;;; image.

(defsystem "nodecode-model-picker"
  :description "Route each turn to a cheap, standard or expert model tier, and escalate on failure"
  :license "MIT"
  :author "aean0x (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-model-picker/test"))))

(defsystem "nodecode-model-picker/test"
  :description "Model picker tests. Registered into the shared nodecode.test registry; RUN-MODEL-PICKER-TESTS filters by the MODEL-PICKER- name prefix."
  :license "MIT"
  :depends-on ("nodecode-model-picker" "nodecode/test")
  :components ((:file "test/cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-model-picker-tests)))
