;;;; nodecode-acedatacloud.asd --- Ace Data Cloud's GPT models, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `acedatacloud' section and installs the hooks cell.lisp lists.
;;;;
;;;; Rewritten from AceDataCloud/AceDataCloudModelProviderHermes (see NOTICE).
;;;; Every dependency rides with nodecode.

(defsystem "nodecode-acedatacloud"
  :description "Ace Data Cloud: its GPT-4.1 models, over the OpenAI chat wire, as a provider"
  :license "MIT"
  :author "AceDataCloud (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "acedatacloud" :name "Ace Data Cloud" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-acedatacloud/test"))))

(defsystem "nodecode-acedatacloud/test"
  :description "Ace Data Cloud tests. Registered into the shared nodecode.test registry; RUN-ACEDATACLOUD-TESTS filters by the ACEDATACLOUD-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-acedatacloud" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-acedatacloud-tests)))
