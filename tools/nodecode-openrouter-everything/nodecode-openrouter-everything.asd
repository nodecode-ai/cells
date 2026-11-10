;;;; nodecode-openrouter-everything.asd --- OpenRouter's non-chat APIs, as verbs and tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `openrouter-everything' section, registers the five tools and
;;;; /openrouter-everything, and puts the manual on (help :openrouter-everything).
;;;;
;;;; A rewrite of dadhalfdev's openrouter-everything plugin (see NOTICE): the
;;;; plugin registered five tools inside Hermes and rewrote Hermes' config.yaml
;;;; to make OpenRouter the provider for web, image, video, speech and
;;;; transcription. This cell keeps the calls that are not a provider lane and
;;;; speaks OpenRouter's REST API itself.

(defsystem "nodecode-openrouter-everything"
  :description "OpenRouter's decisions, extractions, reranking, embeddings and web search on one key, as verbs and tools"
  :author "dadhalfdev (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "verbs")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-openrouter-everything/test"))))

(defsystem "nodecode-openrouter-everything/test"
  :description "openrouter-everything cell tests. Registered into the shared nodecode.test registry; RUN-OPENROUTER-EVERYTHING-TESTS filters by the OPENROUTER-EVERYTHING-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-openrouter-everything" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-openrouter-everything-tests)))
