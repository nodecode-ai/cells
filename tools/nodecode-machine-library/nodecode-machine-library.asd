;;;; nodecode-machine-library.asd --- Machine Library research search and document extract, as two tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `machine-library' section, registers the two tools cell.lisp
;;;; lists and puts the plugin's guidance on the manual as
;;;; (help :machine-library).
;;;;
;;;; A rewrite of SpaceFrontiers's hermes-machine-library (see NOTICE): the
;;;; original plugged into Hermes's web_search and web_extract verbs, so this
;;;; cell gives the model the same two verbs against the same API, with no
;;;; harness provider interface to plug into.

(defsystem "nodecode-machine-library"
  :description "Machine Library research search and document extract over papers, books, patents and standards, with citable sources"
  :author "SpaceFrontiers (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-machine-library/test"))))

(defsystem "nodecode-machine-library/test"
  :description "Machine Library cell tests. Registered into the shared nodecode.test registry; RUN-MACHINE-LIBRARY-TESTS filters by the MACHINE-LIBRARY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-machine-library" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-machine-library-tests)))
