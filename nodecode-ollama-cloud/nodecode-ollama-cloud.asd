;;;; nodecode-ollama-cloud.asd --- Ollama Cloud, over Ollama's own /api/chat wire, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `ollama-cloud' section, registers the ollama-cloud lane and
;;;; installs the hooks cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's ollama-cloud provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-ollama-cloud"
  :description "Ollama Cloud: Ollama's native /api/chat wire as a Nodecode provider lane"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ollama-cloud/test"))))

(defsystem "nodecode-ollama-cloud/test"
  :description "Ollama Cloud tests. Registered into the shared nodecode.test registry; RUN-OLLAMA-CLOUD-TESTS filters by the OLLAMA-CLOUD-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-ollama-cloud" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ollama-cloud-tests)))
