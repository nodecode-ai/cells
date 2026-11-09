;;;; nodecode-degeneracy-guard.asd --- a repetition guard on tool-call arguments.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of simin-yuan/hermes-degeneracy-guard. Optional ASDF cell, not
;;;; part of the organism core: the folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/, and the gateway calls START-CELL
;;;; after recovery, which reads the `degeneracy_guard' section, hooks the
;;;; :TOOL point (a call whose argument looks like a repetition loop is asked
;;;; about, refused, or only recorded), observes the :ROUND point for the
;;;; long-session notice, and registers the degeneracy_check tool and
;;;; /degeneracy.

(defsystem "nodecode-degeneracy-guard"
  :description "Asks about a tool call whose argument looks like a repetition loop"
  :license "Apache-2.0"
  :author "simin-yuan (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "detector")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-degeneracy-guard/test"))))

(defsystem "nodecode-degeneracy-guard/test"
  :description "Degeneracy-guard tests. Registered into the shared nodecode.test registry; RUN-DEGENERACY-GUARD-TESTS filters by the DEGENERACY-GUARD-CELL- name prefix."
  :license "Apache-2.0"
  :author "simin-yuan (original), Nodecode (rewrite)"
  :depends-on ("nodecode-degeneracy-guard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-degeneracy-guard-tests)))
