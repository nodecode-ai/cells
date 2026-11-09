;;;; nodecode-agent-builder.asd --- a specialist agent's blueprint, drafted and kept.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `agent-builder' section, registers the agent_builder tool and
;;;; /agent-builder, and puts the vocabulary on the manual as
;;;; (help :agent-builder).
;;;;
;;;; Ported from itsflownium/hermes-agent-builder (see NOTICE): the original
;;;; was a desktop section with thirteen builder actions over a profile-scoped
;;;; sqlite store, every model call going through the host's own facade. Here
;;;; the actions are Lisp verbs, the store is one JSON file a draft under the
;;;; home, and the model calls are NLE:COMPLETE, Nodecode's own side call.

(defsystem "nodecode-agent-builder"
  :description "Draft, refine, try and keep specialist agent blueprints"
  :author "itsflownium (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "blueprint")
               (:file "store")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-agent-builder/test"))))

(defsystem "nodecode-agent-builder/test"
  :description "Agent builder cell tests. Registered into the shared nodecode.test registry; RUN-AGENT-BUILDER-TESTS filters by the AGENT-BUILDER-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-agent-builder" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-agent-builder-tests)))
