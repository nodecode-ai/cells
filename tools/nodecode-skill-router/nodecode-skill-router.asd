;;;; nodecode-skill-router.asd --- name the one skill that fits a request.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. A rewrite of DECRUX9812's
;;;; typesafe-skill-router (see NOTICE): the request is read before the
;;;; model call and the one skill from the live roster that fits it is named
;;;; in a single <skill_relevance> line on the user message. Nothing is said
;;;; when nothing fits.

(defsystem "nodecode-skill-router"
  :description "TypeSafe skill routing: name the one kept skill that fits the request, before the model call"
  :license "MIT"
  :version "0.1.0"
  :author "DECRUX9812 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "router")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-skill-router/test"))))

(defsystem "nodecode-skill-router/test"
  :description "Tests for the skill router. Registered into the shared nodecode.test registry; RUN-SKILL-ROUTER-TESTS filters by the SKILL-ROUTER-CELL- prefix."
  :license "MIT"
  :depends-on ("nodecode-skill-router" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-skill-router-tests)))
