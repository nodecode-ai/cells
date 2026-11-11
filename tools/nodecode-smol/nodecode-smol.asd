;;;; nodecode-smol.asd --- one shell command in a smol machine: a microVM on this
;;;; computer with its own Linux kernel, apart from this computer's files.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `smol' section and registers the smol_run and smol_machines
;;;; tools, /smol and (help :smol).
;;;;
;;;; Rewritten from smol-machines/hermes-smolmachines (see NOTICE). Every
;;;; dependency rides with nodecode: the machines are the engine's own, driven
;;;; through the smolvm CLI the user installs, and nothing else is installed.

(defsystem "nodecode-smol"
  :description "Runs shell commands in a local smol machine: a microVM with its own kernel, apart from your files"
  :license "MIT"
  :author "smol-machines (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "machine")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-smol/test"))))

(defsystem "nodecode-smol/test"
  :description "smol tests. Registered into the shared nodecode.test registry; RUN-SMOL-TESTS filters by the SMOL-CELL- name prefix."
  :license "MIT"
  :author "smol-machines (original), Nodecode (rewrite)"
  :depends-on ("nodecode-smol" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-smol-tests)))
