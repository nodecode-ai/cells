;;;; nodecode-davinci-resolve.asd --- DaVinci Resolve work from a Nodecode session.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `davinci_resolve' section, registers the seven
;;;; tools, /resolve and (help :resolve).
;;;;
;;;; A rewrite of wassermanproductions's hermes-davinci-resolve-plugin (see
;;;; NOTICE). Every dependency rides with nodecode.

(defsystem "nodecode-davinci-resolve"
  :description "DaVinci Resolve: live project, media and render control, and importable FCPXML and marker CSV"
  :license "MIT"
  :version "0.1.0"
  :author "wassermanproductions (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "bridge")
               (:file "export")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-davinci-resolve/test"))))

(defsystem "nodecode-davinci-resolve/test"
  :description "Resolve cell tests. Registered into the shared nodecode.test registry; RUN-DAVINCI-RESOLVE-TESTS filters by the RESOLVE-CELL- name prefix."
  :license "MIT"
  :author "wassermanproductions (original), Nodecode (rewrite)"
  :depends-on ("nodecode-davinci-resolve" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-davinci-resolve-tests)))
