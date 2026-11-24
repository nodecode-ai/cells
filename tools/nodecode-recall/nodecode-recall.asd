;;;; nodecode-recall.asd --- a private, on-device index of the operator's own documents.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `recall' section and installs the five tools, the /recall
;;;; command, the manual topic and the ambient hint cell.lisp lists.
;;;;
;;;; Everything is standard library: a JSON index file under the organism's
;;;; home, no database binding, no network, no embeddings.

(defsystem "nodecode-recall"
  :description "Private on-device index of your own documents: search, read and cite local files"
  :license "MIT"
  :author "memeshee (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "guard")
               (:file "extract")
               (:file "index")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-recall/test"))))

(defsystem "nodecode-recall/test"
  :description "Recall tests. Registered into the shared nodecode.test registry; RUN-RECALL-TESTS filters by the RECALL-CELL- name prefix."
  :license "MIT"
  :author "memeshee (original), Nodecode (rewrite)"
  :depends-on ("nodecode-recall" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-recall-tests)))
