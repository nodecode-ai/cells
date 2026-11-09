;;;; nodecode-axioma.asd --- signal marks in a reply, and advisory cards.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of percyhendrux123-prog/hermes-axioma. Optional quicklisp/ASDF
;;;; cell, not part of the organism core: the folder loader loads it at boot
;;;; when its directory sits under ~/.nodecode/cells/, and the gateway calls
;;;; START-CELL after recovery, which reads the `axioma' config section,
;;;; hooks the :TOOL point, and registers the axioma_card tool, /axioma and
;;;; the manual topic :axioma.

(defsystem "nodecode-axioma"
  :description "Signal marks in a reply, and an advisory card before a risky call"
  :license "MIT"
  :author "percyhendrux123-prog (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-axioma/test"))))

(defsystem "nodecode-axioma/test"
  :description "Axioma tests. Registered into the shared nodecode.test registry; RUN-AXIOMA-TESTS filters by the AXIOMA-CELL- name prefix."
  :license "MIT"
  :author "percyhendrux123-prog (original), Nodecode (rewrite)"
  :depends-on ("nodecode-axioma" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-axioma-tests)))
