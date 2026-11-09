;;;; nodecode-jina.asd --- Jina Reader and Jina Search, as two tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `jina' section and registers the two tools cell.lisp lists.
;;;;
;;;; A rewrite of Adolanium's hermes-jina (see NOTICE). The original
;;;; registered a web backend inside Hermes, so Hermes' own web_extract and
;;;; web_search calls were routed to Jina when the operator chose it;
;;;; Nodecode has no such switchboard, so its two operations are tools in
;;;; their own right, posting to the same two endpoints.

(defsystem "nodecode-jina"
  :description "Jina Reader: any web page as clean Markdown, and Jina Search's sources, as two tools"
  :author "Adolanium (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "quri")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-jina/test"))))

(defsystem "nodecode-jina/test"
  :description "Jina cell tests. Registered into the shared nodecode.test registry; RUN-JINA-TESTS filters by the JINA-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-jina" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-jina-tests)))
