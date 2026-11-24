;;;; nodecode-sequenzy.asd --- Sequenzy email marketing as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `sequenzy' section and registers the seven sequenzy_* tools,
;;;; the /sequenzy command and the (help :sequenzy) topic.
;;;;
;;;; A Nodecode rewrite of Sequenzy/skills (see NOTICE).

(defsystem "nodecode-sequenzy"
  :description "Sequenzy email marketing: subscribers, audience, campaigns, delivery stats and transactional sends"
  :license "MIT"
  :version "0.1.0"
  :author "Sequenzy (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-sequenzy/test"))))

(defsystem "nodecode-sequenzy/test"
  :description "Sequenzy tests. Registered into the shared nodecode.test registry; RUN-SEQUENZY-TESTS filters by the SEQUENZY-CELL- name prefix."
  :license "MIT"
  :author "Sequenzy (original), Nodecode (rewrite)"
  :depends-on ("nodecode-sequenzy" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-sequenzy-tests)))
