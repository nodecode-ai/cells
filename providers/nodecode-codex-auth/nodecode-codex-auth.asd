;;;; nodecode-codex-auth.asd --- the ChatGPT Codex login as a credential.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader (kernel cells.lisp) loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; after recovery, which reads the `codex-auth' config section and hooks the
;;;; kernel's :CREDENTIAL point.
;;;;
;;;; The kernel resolves a provider credential through one chain — config,
;;;; auth.json's api_keys, the :CREDENTIAL point, the environment, "public" —
;;;; and offers the parsed store to whatever answers that point. This folder is
;;;; what does: a ChatGPT subscription's OAuth token on an openai-family lane,
;;;; with the transport that login implies (the ChatGPT backend's address, its
;;;; originator, the chatgpt-account-id header, the cache discriminator that
;;;; keeps its requests in a shard of their own). None of it is in the kernel.
;;;;
;;;; Every dependency is already in the serving image (shasht, cl-base64,
;;;; uiop ride with nodecode). The folder is providers/nodecode-codex-auth of
;;;; nodecode-ai/cells: install it from the hub (`nodecode add nc://codex-auth',
;;;; or /setup, Choose), or copy the folder into ~/.nodecode/cells/. Presence
;;;; is enabled; the package is named after the system, which is how the loader
;;;; finds START-CELL.

(defsystem "nodecode-codex-auth"
  :description "ChatGPT Codex OAuth credentials for openai-family lanes"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "credential")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-codex-auth/test"))))

(defsystem "nodecode-codex-auth/test"
  :description "Codex-auth tests. Registered into the shared nodecode.test registry; RUN-CODEX-AUTH-TESTS filters by the CODEX-AUTH-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-codex-auth" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-codex-auth-tests)))
