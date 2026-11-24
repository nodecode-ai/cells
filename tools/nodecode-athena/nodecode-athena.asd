;;;; nodecode-athena.asd --- the local app store: search, install, run, read, stop.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `athena' section, registers the seven tools and the /athena
;;;; command, and puts the primer on (help :athena).
;;;;
;;;; Rewritten from athenaforhermes's Hermes Agent plugin of the same name;
;;;; see NOTICE. Every dependency rides with nodecode: nlk:http reads the
;;;; catalogue, and a shell command the person already has (git, node, sh)
;;;; installs and runs an app.

(defsystem "nodecode-athena"
  :description "Athena app store: search a catalogue of AI apps, install one, run it, read its log, stop it"
  :author "athenaforhermes (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-athena/test"))))

(defsystem "nodecode-athena/test"
  :description "Athena tests. Registered into the shared nodecode.test registry; RUN-ATHENA-TESTS filters by the ATHENA-CELL- name prefix."
  :author "athenaforhermes (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-athena" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-athena-tests)))
