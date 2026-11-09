;;;; nodecode-provenance.asd --- quoted provenance for what the agent keeps.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. A Nodecode rewrite of simin-yuan's hermes-provenance-gate
;;;; (see NOTICE). Every dependency rides with nodecode: cl-ppcre is the
;;;; serving image's pattern engine, and nothing here reaches the network.

(defsystem "nodecode-provenance"
  :description "Provenance gate: flags or refuses a memory write that states a user decision the user's own words do not back"
  :author "simin-yuan (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "patterns")
               (:file "text")
               (:file "claims")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-provenance/test"))))

(defsystem "nodecode-provenance/test"
  :description "Provenance gate tests: the detector, the quotes, the tools and the hook, with no network."
  :author "simin-yuan (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :depends-on ("nodecode-provenance" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-provenance-tests)))
