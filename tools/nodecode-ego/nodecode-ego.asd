;;;; nodecode-ego.asd --- the ego lite browser, through its loopback relay.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of loop2zero's Hermes plugin. An optional cell, not part of the
;;;; core: the folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery, which
;;;; reads the `ego' section, registers the five relay tools, /ego and
;;;; (help :ego).

(defsystem "nodecode-ego"
  :description "ego lite browser: read and save pages through the local relay"
  :license "MIT"
  :version "0.1.0"
  :author "loop2zero (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ego/test"))))

(defsystem "nodecode-ego/test"
  :description "ego tests. Registered into the shared nodecode.test registry; RUN-EGO-TESTS filters by the EGO-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-ego" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ego-tests)))
