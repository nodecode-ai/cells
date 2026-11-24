;;;; nodecode-reme.asd --- automatic long-term memory from a ReMe service.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode cell, NOT part of the organism core: nothing in Nodecode names
;;;; this system. A rewrite of agentscope-ai/ReMe's Hermes Agent plugin (see
;;;; NOTICE): the folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `reme' config section, observes the :FRAME point for turns
;;;; ending, advises the harness-section read so recalled notes ride every
;;;; request, starts one writer thread and registers the reme_recall tool and
;;;; /reme.

(defsystem "nodecode-reme"
  :description "Long-term memory from a ReMe service: recalls notes before each turn, records each completed turn"
  :license "Apache-2.0"
  :author "agentscope-ai (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-reme/test"))))

(defsystem "nodecode-reme/test"
  :description "ReMe tests. Registered into the shared nodecode.test registry; RUN-REME-TESTS filters by the REME-CELL- name prefix."
  :license "Apache-2.0"
  :author "agentscope-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-reme" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-reme-tests)))
