;;;; nodecode-ngrok-ai-gateway.asd --- the ngrok AI Gateway as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `ngrok-ai-gateway' section and installs the hooks cell.lisp
;;;; lists.
;;;;
;;;; Ported from ngrok/hermes-ngrok's model-providers/ngrok (see NOTICE).
;;;; Every dependency rides with nodecode.

(defsystem "nodecode-ngrok-ai-gateway"
  :description "ngrok AI Gateway: one access key for every model provider, with failover"
  :license "MIT"
  :version "0.1.0"
  :author "ngrok (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it
  ;; installs it, then asks for its key
  :properties ((:serves (:provider "ngrok" :name "ngrok AI Gateway" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ngrok-ai-gateway/test"))))

(defsystem "nodecode-ngrok-ai-gateway/test"
  :description "ngrok AI Gateway tests. Registered into the shared nodecode.test registry; RUN-NGROK-AI-GATEWAY-TESTS filters by the NGROK-AI-GATEWAY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-ngrok-ai-gateway" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ngrok-ai-gateway-tests)))
