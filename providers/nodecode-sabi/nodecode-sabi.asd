;;;; nodecode-sabi.asd --- Sabi, the local routing proxy, as a Nodecode provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `sabi' section and installs the hooks cell.lisp lists.
;;;;
;;;; Rewritten from vizuh/sabi's Hermes `sabi-metadata' plugin (see NOTICE):
;;;; the same loopback Chat Completions lane and the same opaque attribution
;;;; headers, as a Nodecode provider. Every dependency rides with nodecode.

(defsystem "nodecode-sabi"
  :description "Sabi: the local routing proxy as a Nodecode provider, with its attribution headers"
  :license "MIT"
  :author "vizuh (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, no key needed
  :properties ((:serves (:provider "sabi" :name "Sabi")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-sabi/test"))))

(defsystem "nodecode-sabi/test"
  :description "Sabi tests. Registered into the shared nodecode.test registry; RUN-SABI-TESTS filters by the SABI-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-sabi" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-sabi-tests)))
