;;;; nodecode-web-provider.asd --- omp's keyless web search engines, as a verb the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `web-provider' section and puts (help :engines) on the manual.
;;;;
;;;; Ported from oh-my-pi's web provider (see NOTICE). Every dependency rides
;;;; with nodecode. The folder is not nodecode-web: Nodecode ships a cell by
;;;; that name (the browser tab), and `web' is the core's own config key.

(defsystem "nodecode-web-provider"
  :description "Web search engines: Google, Startpage, DuckDuckGo, Ecosia, Mojeek, SearXNG and their merge, keyless"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "engines")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-web-provider/test"))))

(defsystem "nodecode-web-provider/test"
  :description "Web search engine tests. Registered into the shared nodecode.test registry; RUN-WEB-PROVIDER-TESTS filters by the WEB-PROVIDER-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-web-provider" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-web-provider-tests)))
