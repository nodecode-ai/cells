;;;; nodecode-cloudflare-workers-ai.asd --- Cloudflare Workers AI as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cloudflare-workers-ai' section and installs the hooks cell.lisp
;;;; lists.
;;;;
;;;; A rewrite of AhmetArif0's Hermes plugin hermes-cloudflare-workers-ai-provider
;;;; (see NOTICE). Every dependency rides with nodecode.

(defsystem "nodecode-cloudflare-workers-ai"
  :description "Cloudflare Workers AI: its models, context windows and per-model reasoning effort"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "cloudflare-workers-ai" :name "Cloudflare Workers AI" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cloudflare-workers-ai/test"))))

(defsystem "nodecode-cloudflare-workers-ai/test"
  :description "Cloudflare Workers AI tests. Registered into the shared nodecode.test registry; RUN-CLOUDFLARE-WORKERS-AI-TESTS filters by the CLOUDFLARE-WORKERS-AI-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-cloudflare-workers-ai" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cloudflare-workers-ai-tests)))
