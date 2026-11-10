;;;; nodecode-model-drift-watch.asd --- is the model you use still as good as it was?
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `model-drift-watch' section, registers the model_drift tool,
;;;; /model-drift-watch and (help :model-drift-watch).
;;;;
;;;; Ported from the Hermes plugin model-drift-watch (see NOTICE).

(defsystem "nodecode-model-drift-watch"
  :description "Model Drift Watch: catch a model that quietly got worse"
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "stats")
               (:file "suite")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-model-drift-watch/test"))))

(defsystem "nodecode-model-drift-watch/test"
  :description "Model Drift Watch tests. Registered into the shared nodecode.test registry; RUN-MODEL-DRIFT-WATCH-TESTS filters by the MODEL-DRIFT-WATCH-CELL- name prefix."
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode-model-drift-watch" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-model-drift-watch-tests)))
