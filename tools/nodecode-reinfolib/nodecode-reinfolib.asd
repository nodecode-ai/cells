;;;; nodecode-reinfolib.asd --- MLIT's real estate information library, one quarter
;;;; at a time.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes plugin jp-reinfolib. Optional ASDF cell, not part
;;;; of the organism core: the folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/, and the gateway calls START-CELL after
;;;; recovery, which reads the `reinfolib' section, registers the
;;;; reinfolib_quarter tool, /reinfolib and (help :reinfolib).

(defsystem "nodecode-reinfolib"
  :description "MLIT real estate transaction prices for one Japanese city code and one quarter"
  :license "MIT"
  :version "0.1.0"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-reinfolib/test"))))

(defsystem "nodecode-reinfolib/test"
  :description "Reinfolib tests. Registered into the shared nodecode.test registry; RUN-REINFOLIB-TESTS filters by the REINFOLIB-CELL- name prefix."
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode-reinfolib" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-reinfolib-tests)))
