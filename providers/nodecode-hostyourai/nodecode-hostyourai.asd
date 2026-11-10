;;;; nodecode-hostyourai.asd --- HostYourAI, its hyai/auto router, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `hostyourai' section and installs the hooks cell.lisp lists.
;;;;
;;;; Ported from cogitor/hermes-hostyourai (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-hostyourai"
  :description "HostYourAI: the hyai/auto router as a provider, with a task and a policy hint per request"
  :license "MIT"
  :author "cogitor (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "hostyourai" :name "HostYourAI" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hostyourai/test"))))

(defsystem "nodecode-hostyourai/test"
  :description "HostYourAI tests. Registered into the shared nodecode.test registry; RUN-HOSTYOURAI-TESTS filters by the HOSTYOURAI-CELL- name prefix."
  :license "MIT"
  :author "cogitor (original), Nodecode (rewrite)"
  :depends-on ("nodecode-hostyourai" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hostyourai-tests)))
