;;;; nodecode-token-safety.asd --- a Base ERC-20 pre-trade safety check, as two tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `token-safety' section and puts the two tools, the /token-safety
;;;; command and the manual topic on.
;;;;
;;;; Rewritten from rezearcher's Hermes Agent skill on the same subject; see
;;;; NOTICE. The original was a SKILL.md that told the agent to run curl; this
;;;; cell makes the same two GETs through the image's own HTTP call and answers
;;;; the report as JSON a model reads, with no shell and nothing fetched at run
;;;; time.

(defsystem "nodecode-token-safety"
  :description "Grey Ridge token safety: honeypot, transfer tax and owner risk for a Base ERC-20, as two model tools"
  :author "rezearcher (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-token-safety/test"))))

(defsystem "nodecode-token-safety/test"
  :description "Token safety tests. Registered into the shared nodecode.test registry; RUN-TOKEN-SAFETY-TESTS filters by the TOKEN-SAFETY-CELL- name prefix."
  :author "rezearcher (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-token-safety" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-token-safety-tests)))
