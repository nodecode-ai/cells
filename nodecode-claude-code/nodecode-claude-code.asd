;;;; nodecode-claude-code.asd --- the Claude Code CLI as a provider lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional quicklisp/ASDF add-on, NOT part of the organism core: nothing in
;;;; src/src names this system. The folder loader (kernel addons.lisp) loads it
;;;; at boot when its directory sits under ~/.nodecode/addons/; the gateway
;;;; calls START-ADDON after recovery, which reads the `claude-code' config
;;;; section, registers the claude-code lane and hooks the request path.
;;;;
;;;; A round on the claude-code provider is authored by the operator's own
;;;; logged-in `claude' CLI and sent by Nodecode: the CLI is handed the
;;;; round's history, system prompt and tools, writes the Messages request it
;;;; would make, and a loopback relay takes that request instead of letting it
;;;; leave. Nodecode's own Anthropic transport then sends those exact bytes
;;;; and parses the stream, so retries, the fold, the tool loop, the context
;;;; engine and the usage record stay Nodecode's. Tools run in Nodecode, never
;;;; in the CLI. The shape follows NousResearch's
;;;; hermes-plugin-claude-subscription-directsdk.
;;;;
;;;; Every dependency is already in the serving image (usocket rides with
;;;; dexador, uiop with nodecode). The .asd sits at the top of this folder,
;;;; one of nodecode-ai/addons: copy the folder into ~/.nodecode/addons/.
;;;; Presence is enabled; the package is named after the system, which is how
;;;; the loader finds START-ADDON in it.

(defsystem "nodecode-claude-code"
  :description "Claude Code's own login as a provider lane: the claude CLI writes each request, Nodecode sends it"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "relay")
               (:file "cli")
               (:file "addon"))
  :in-order-to ((test-op (test-op "nodecode-claude-code/test"))))

(defsystem "nodecode-claude-code/test"
  :description "Claude-code tests. Registered into the shared nodecode.test registry; RUN-CLAUDE-CODE-TESTS filters by the CLAUDE-CODE-ADDON- name prefix."
  :license "MIT"
  :depends-on ("nodecode-claude-code" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "addon-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-claude-code-tests)))
