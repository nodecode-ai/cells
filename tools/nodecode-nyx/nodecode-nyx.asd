;;;; nodecode-nyx.asd --- Nyx memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `nyx' section, prepares the data folder, erases what the
;;;; quarantine no longer holds, registers the four tools, /nyx and the manual
;;;; topic.
;;;;
;;;; A Nodecode rewrite of sixgodgit/nyx's Hermes plugin (see NOTICE).

(defsystem "nodecode-nyx"
  :description "Nyx: local, single-user long-term memory on your own disk"
  :license "MIT"
  :version "0.1.0"
  :author "sixgodgit (original), Nodecode (rewrite)"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-nyx/test"))))

(defsystem "nodecode-nyx/test"
  :description "Nyx tests. Registered into the shared nodecode.test registry; RUN-NYX-TESTS filters by the NYX-CELL- name prefix."
  :license "MIT"
  :author "sixgodgit (original), Nodecode (rewrite)"
  :depends-on ("nodecode-nyx" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-nyx-tests)))
