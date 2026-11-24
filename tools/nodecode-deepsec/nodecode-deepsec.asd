;;;; nodecode-deepsec.asd --- a local security review of a tree, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `deepsec' section, registers the four tools cell.lisp lists,
;;;; adds /deepsec and puts (help :deepsec) on the manual.
;;;;
;;;; Ported from the DeepSec Shield plugin of Unclecheng-li/DeepSec (see
;;;; NOTICE). Every dependency rides with nodecode.

(defsystem "nodecode-deepsec"
  :description "A local security review of a tree: secrets, risky code, agent-config and supply-chain checks, as tools"
  :license "MIT"
  :version "0.1.0"
  :author "Unclecheng-li (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "engine")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-deepsec/test"))))

(defsystem "nodecode-deepsec/test"
  :description "DeepSec tests. Registered into the shared nodecode.test registry; RUN-DEEPSEC-TESTS filters by the DEEPSEC-CELL- name prefix."
  :license "MIT"
  :author "Unclecheng-li (original), Nodecode (rewrite)"
  :depends-on ("nodecode-deepsec" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-deepsec-tests)))
