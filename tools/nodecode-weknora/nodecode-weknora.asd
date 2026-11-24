;;;; nodecode-weknora.asd --- WeKnora knowledge bases as four tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `weknora' section, registers the four tools
;;;; (list_knowledge_bases, search, read_document, ask), puts /weknora and the
;;;; manual on (help :weknora).
;;;;
;;;; Read-only against the deployment: GETs, the two retrieval POSTs, one
;;;; session create and the chat POST that answers. Every dependency is
;;;; already in the serving image (dexador and quri ride with nodecode).
;;;;
;;;; The .asd sits INSIDE its folder: install by putting - or symlinking -
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-weknora"
  :description "WeKnora knowledge bases as four tools: hybrid search, document reading, cited answers"
  :license "MIT"
  :version "0.1.0"
  :author "Tencent (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-weknora/test"))))

(defsystem "nodecode-weknora/test"
  :description "WeKnora tests. Registered into the shared nodecode.test registry; RUN-WEKNORA-TESTS filters by the WEKNORA-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Tencent (original), Nodecode (rewrite)"
  :depends-on ("nodecode-weknora" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-weknora-tests)))
