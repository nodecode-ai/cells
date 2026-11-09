;;;; nodecode-skill-retrieval.asd --- a cell, NOT part of the organism core.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the skill-retrieval plugin for Hermes Agent: BM25 ranking
;;;; over a library of SKILL.md skills, a tool and a /command for asking by
;;;; hand, and the best matches of a turn's own input injected into its
;;;; request. See README.md and NOTICE.

(defsystem "nodecode-skill-retrieval"
  :description "BM25 over a SKILL.md library: a search tool, /skill-search, and each turn's best matches injected"
  :license "MIT"
  :version "0.1.0"
  :author "moonlight-lupin (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-skill-retrieval/test"))))

(defsystem "nodecode-skill-retrieval/test"
  :description "The skill-retrieval cell's tests, registered into the shared nodecode.test registry under the SKILL-RETRIEVAL-CELL- prefix."
  :license "MIT"
  :depends-on ("nodecode-skill-retrieval" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-skill-retrieval-tests)))
