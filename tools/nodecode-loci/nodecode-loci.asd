;;;; nodecode-loci.asd --- local Loci models, through the app's own MCP endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `loci' section, finds the token the Loci app publishes, and puts
;;;; three tools, /loci and the manual topic on.
;;;;
;;;; The original is a Hermes Agent plugin: a manifest that discovers a Mac
;;;; app's local MCP server and two skills of guidance for the tools it
;;;; advertises. This cell speaks that JSON-RPC itself through the image's own
;;;; HTTP call -- same endpoint, same token, same tools, same arguments -- with
;;;; no Python and nothing fetched at run time.

(defsystem "nodecode-loci"
  :description "Loci: local models on this Mac, through the Loci app's own MCP endpoint"
  :license "MIT"
  :version "0.1.0"
  :author "AskLoci (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "rpc")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-loci/test"))))

(defsystem "nodecode-loci/test"
  :description "Loci tests. Registered into the shared nodecode.test registry; RUN-LOCI-TESTS filters by the LOCI-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "AskLoci (original), Nodecode (rewrite)"
  :depends-on ("nodecode-loci" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-loci-tests)))
