;;;; nodecode-jev-skill-router.asd --- name the one skill that fits a request.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. A rewrite of ydmw74's
;;;; jev-skill-router (see NOTICE): one skill_select call answers at most one
;;;; skill from the live roster, or an abstain, so the model loads the right
;;;; thing -- or nothing.

(defsystem "nodecode-jev-skill-router"
  :description "Jev skill routing: skill_select names at most one kept skill that fits a request, before anything is loaded"
  :license "MIT"
  :version "0.1.0"
  :author "ydmw74 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "router")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-jev-skill-router/test"))))

(defsystem "nodecode-jev-skill-router/test"
  :description "Tests for the Jev skill router. Registered into the shared nodecode.test registry; RUN-JEV-SKILL-ROUTER-TESTS filters by the JEV-SKILL-ROUTER-CELL- prefix."
  :license "MIT"
  :depends-on ("nodecode-jev-skill-router" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-jev-skill-router-tests)))
