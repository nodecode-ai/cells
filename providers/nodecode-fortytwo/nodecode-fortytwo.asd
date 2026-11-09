;;;; nodecode-fortytwo.asd --- Fortytwo, Qwen3.8 27B on its API, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `fortytwo' section and installs the hooks cell.lisp lists.
;;;;
;;;; Rewritten from Fortytwo-Network's Hermes plugin (see NOTICE): the same
;;;; provider profile, the same attribution headers and the same four
;;;; reasoning efforts, as a Nodecode provider. Every dependency rides with
;;;; nodecode.

(defsystem "nodecode-fortytwo"
  :description "Fortytwo: Qwen3.8 27B on Fortytwo's API as a Nodecode provider"
  :license "MIT"
  :author "Fortytwo-Network (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "fortytwo" :name "Fortytwo" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fortytwo/test"))))

(defsystem "nodecode-fortytwo/test"
  :description "Fortytwo tests. Registered into the shared nodecode.test registry; RUN-FORTYTWO-TESTS filters by the FORTYTWO-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-fortytwo" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fortytwo-tests)))
