;;;; nodecode-fabric.asd --- a shared markdown memory, and the recall that ranks it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. The folder loader loads it at
;;;; boot when its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL after recovery, which reads the `fabric' section, registers
;;;; the six tools and the /fabric command, hooks the :ROUND point so a turn's
;;;; own prompt recalls what earlier work wrote, and puts the manual on
;;;; (help :fabric).
;;;;
;;;; See NOTICE: this folder is a rewrite of esaradev/icarus-plugin.

(defsystem "nodecode-fabric"
  :description "fabric: a shared markdown memory of what was done, recalled before a turn"
  :license "MIT"
  :version "0.1.0"
  :author "esaradev (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fabric/test"))))

(defsystem "nodecode-fabric/test"
  :description "fabric tests. Registered into the shared nodecode.test registry; RUN-FABRIC-TESTS filters by the FABRIC-CELL- name prefix."
  :license "MIT"
  :author "esaradev (original), Nodecode (rewrite)"
  :depends-on ("nodecode-fabric" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fabric-tests)))
