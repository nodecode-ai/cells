;;;; nodecode-deepcanary.asd --- the attention inbox: what needs the operator.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of Oscar-Williams/dsh-deepcanary (MIT).
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode's
;;;; src names this system. The folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; after recovery, which reads the `deepcanary' section, registers the six
;;;; attention tools, puts /deepcanary and the manual on (help :deepcanary),
;;;; and hooks the :ROUND point so an item the operator was told about reaches
;;;; the model's next request. The inbox is one JSON file under the home.
;;;; Every dependency is already in the serving image.

(defsystem "nodecode-deepcanary"
  :description "An attention inbox with levels and quiet hours for the operator"
  :license "MIT"
  :version "0.1.0"
  :author "Oscar-Williams (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-deepcanary/test"))))

(defsystem "nodecode-deepcanary/test"
  :description "DeepCanary tests. Registered into the shared nodecode.test registry; RUN-DEEPCANARY-TESTS filters by the DEEPCANARY-CELL- name prefix."
  :license "MIT"
  :author "Oscar-Williams (original), Nodecode (rewrite)"
  :depends-on ("nodecode-deepcanary" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-deepcanary-tests)))
