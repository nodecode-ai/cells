;;;; nodecode-cloudflare-ai-gateway.asd --- Cloudflare AI Gateway as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cloudflare-ai-gateway' section and installs the hooks cell.lisp
;;;; lists.
;;;;
;;;; Ported from oh-my-pi's cloudflare-ai-gateway provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-cloudflare-ai-gateway"
  :description "Cloudflare AI Gateway: Anthropic, OpenAI and Workers AI models through one gateway"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cloudflare-ai-gateway/test"))))

(defsystem "nodecode-cloudflare-ai-gateway/test"
  :description "Cloudflare AI Gateway tests. Registered into the shared nodecode.test registry; RUN-CLOUDFLARE-AI-GATEWAY-TESTS filters by the CLOUDFLARE-AI-GATEWAY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-cloudflare-ai-gateway" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cloudflare-ai-gateway-tests)))
