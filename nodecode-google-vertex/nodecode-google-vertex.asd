;;;; nodecode-google-vertex.asd --- Google Vertex AI as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `google-vertex' section and installs the hooks cell.lisp
;;;; lists.
;;;;
;;;; Ported from oh-my-pi's google-vertex provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-google-vertex"
  :description "Google Vertex AI: Gemini, Claude and partner models on a Google Cloud project, with an API key or Application Default Credentials"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "adc")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-google-vertex/test"))))

(defsystem "nodecode-google-vertex/test"
  :description "Google Vertex AI tests. Registered into the shared nodecode.test registry; RUN-GOOGLE-VERTEX-TESTS filters by the GOOGLE-VERTEX-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-google-vertex" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-google-vertex-tests)))
