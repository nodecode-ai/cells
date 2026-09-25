;;;; nodecode-codex-auth.asd --- the ChatGPT Codex login as a credential.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional quicklisp/ASDF add-on, NOT part of the organism core: nothing in
;;;; src/src names this system. The folder loader (kernel addons.lisp) loads it
;;;; at boot when its directory sits under ~/.nodecode/addons/; the gateway
;;;; calls START-ADDON after recovery, which reads the `codex-auth' config
;;;; section and hooks the kernel's :CREDENTIAL point.
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
;;;; uiop ride with nodecode). The .asd sits at the top of this folder, one of
;;;; nodecode-ai/addons: install it from the add-on hub (/setup, Choose), or
;;;; copy the folder into ~/.nodecode/addons/. Presence is enabled; the package
;;;; is named after the system, which is how the loader finds START-ADDON.

(defsystem "nodecode-codex-auth"
  :description "ChatGPT Codex OAuth credentials for openai-family lanes"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "credential")
               (:file "addon"))
  :in-order-to ((test-op (test-op "nodecode-codex-auth/test"))))

(defsystem "nodecode-codex-auth/test"
  :description "Codex-auth tests. Registered into the shared nodecode.test registry; RUN-CODEX-AUTH-TESTS filters by the CODEX-AUTH-ADDON- name prefix."
  :license "MIT"
  :depends-on ("nodecode-codex-auth" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "addon-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-codex-auth-tests)))
