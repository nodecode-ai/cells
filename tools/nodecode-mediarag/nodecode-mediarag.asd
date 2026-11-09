;;;; nodecode-mediarag.asd --- one local index over your own documents, notes,
;;;; images and audio.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `mediarag' section, registers the five tools
;;;; (search, add_note, ingest_dir, add_source, status), puts /mediarag and the
;;;; manual on (help :mediarag).
;;;;
;;;; The corpus is one file under the section's corpus_root. Nothing here dials
;;;; a provider: the only network call is a user-named http(s) source.

(defsystem "nodecode-mediarag"
  :description "Local search over your documents, notes, images and audio"
  :license "MIT"
  :version "0.1.0"
  :author "dyiapanis (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "corpus")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mediarag/test"))))

(defsystem "nodecode-mediarag/test"
  :description "MediaRAG tests. Registered into the shared nodecode.test registry; RUN-MEDIARAG-TESTS filters by the MEDIARAG-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "dyiapanis (original), Nodecode (rewrite)"
  :depends-on ("nodecode-mediarag" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mediarag-tests)))
