;;;; nodecode-aistupidlevel.asd --- AI Stupid Level, its Smart Router, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `aistupidlevel' section and installs the hooks cell.lisp lists.
;;;;
;;;; Rewritten from StudioPlatforms/aistupidlevel-sdk's Hermes plugin (see
;;;; NOTICE). Every dependency rides with nodecode.

(defsystem "nodecode-aistupidlevel"
  :description "AI Stupid Level: its Smart Router, where a model id is a routing mode, as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :author "StudioPlatforms (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "aistupidlevel" :name "AI Stupid Level" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-aistupidlevel/test"))))

(defsystem "nodecode-aistupidlevel/test"
  :description "AI Stupid Level tests. Registered into the shared nodecode.test registry; RUN-AISTUPIDLEVEL-TESTS filters by the AISTUPIDLEVEL-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-aistupidlevel" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-aistupidlevel-tests)))
