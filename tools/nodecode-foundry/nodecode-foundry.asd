;;;; nodecode-foundry.asd --- a working cell scaffolded from one sentence.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of dolepee's Hermes plugin dolepee/hermes-plugin-foundry as a
;;;; Nodecode cell: the same five verbs (plan, build, test, list, catalog),
;;;; the same two archetypes (a folder watcher, a JSON API poller), and the
;;;; same one generated root, writing Nodecode cells instead of Hermes
;;;; plugins. Nothing in the core names this system.

(defsystem "nodecode-foundry"
  :description "Turns a one-sentence request into a working watch-or-poll Nodecode cell, then checks it"
  :license "MIT"
  :author "dolepee (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "templates")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-foundry/test"))))

(defsystem "nodecode-foundry/test"
  :description "Foundry tests: the request parser, the two templates, the written folder, the checks and the command."
  :license "MIT"
  :depends-on ("nodecode-foundry" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-foundry-tests)))
