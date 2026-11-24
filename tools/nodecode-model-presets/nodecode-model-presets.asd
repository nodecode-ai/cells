;;;; nodecode-model-presets.asd --- named model, provider and effort picks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `model-presets' section, puts (help :model-presets) on the
;;;; manual, registers the model_presets tool and /preset.
;;;;
;;;; A rewrite of will-lynas's hermes-model-presets (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-model-presets"
  :description "Model presets: named model, provider and reasoning-effort picks, landed with /preset or one tool call"
  :license "MIT"
  :version "0.1.0"
  :author "will-lynas (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-model-presets/test"))))

(defsystem "nodecode-model-presets/test"
  :description "Model presets tests. Registered into the shared nodecode.test registry; RUN-MODEL-PRESETS-TESTS filters by the MODEL-PRESETS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-model-presets" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-model-presets-tests)))
