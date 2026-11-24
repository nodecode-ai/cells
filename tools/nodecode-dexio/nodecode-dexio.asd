;;;; nodecode-dexio.asd --- a shared Dexio wiki as memory, for the model and its team.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `dexio' section, puts the wiki's paragraph in every request,
;;;; recalls the pages matching each turn's message, and registers the six
;;;; dexio_* tools and /dexio.
;;;;
;;;; A Nodecode rewrite of dexio-wiki/hermes-plugin at d057150 (see NOTICE).

(defsystem "nodecode-dexio"
  :description "Dexio wiki memory: the pages that match each turn, and tools to search, read and write them"
  :license "MIT"
  :version "0.1.0"
  :author "dexio-wiki (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-dexio/test"))))

(defsystem "nodecode-dexio/test"
  :description "Dexio tests. Registered into the shared nodecode.test registry; RUN-DEXIO-TESTS filters by the DEXIO-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "dexio-wiki (original), Nodecode (rewrite)"
  :depends-on ("nodecode-dexio" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-dexio-tests)))
