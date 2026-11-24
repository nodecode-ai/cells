;;;; nodecode-xai-collections.asd --- xAI Collections as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `xai-collections' section, registers the one read-only
;;;; xai_collections_search tool and /collections, and refuses a section with
;;;; no key.
;;;;
;;;; A Nodecode rewrite of Julientalbot/hermes-xai's collections module (see
;;;; NOTICE). Two REST calls on https://api.x.ai/v1, nothing more.

(defsystem "nodecode-xai-collections"
  :description "xAI Collections: passages from the documents you indexed there, read only"
  :license "MIT"
  :version "0.1.0"
  :author "Julientalbot (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-xai-collections/test"))))

(defsystem "nodecode-xai-collections/test"
  :description "xAI Collections tests. Registered into the shared nodecode.test registry; RUN-XAI-COLLECTIONS-TESTS filters by the XAI-COLLECTIONS-CELL- name prefix."
  :license "MIT"
  :author "Julientalbot (original), Nodecode (rewrite)"
  :depends-on ("nodecode-xai-collections" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-xai-collections-tests)))
