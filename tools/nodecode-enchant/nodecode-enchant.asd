;;;; nodecode-enchant.asd --- a draft rewritten, reviewed, and taken back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `enchant' section, registers the three tools and the /enchant
;;;; command, and puts the manual on (help :enchant).
;;;;
;;;; Rewritten from noxius-studium's Hermes Agent plugin enchanted-composer;
;;;; see NOTICE. Every dependency rides with nodecode: the rewrite is one side
;;;; call through NLE:COMPLETE on the auxiliary target, and the review is a
;;;; word diff of the two texts, computed here.

(defsystem "nodecode-enchant"
  :description "Enchant: rewrite a draft, review the changes group by group, undo or redo"
  :author "noxius-studium (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "enhancers")
               (:file "review")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-enchant/test"))))

(defsystem "nodecode-enchant/test"
  :description "Enchant tests. Registered into the shared nodecode.test registry; RUN-ENCHANT-TESTS filters by the ENCHANT-CELL- name prefix."
  :author "noxius-studium (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-enchant" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-enchant-tests)))
