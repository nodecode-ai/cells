;;;; nodecode-memobase.asd --- Memobase memory as two model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `memobase' section, registers the two tools the original
;;;; plugin gave its harness (memobase_search, memobase_profile), puts
;;;; /memobase and the manual topic on, and hands the model a primer.
;;;;
;;;; The original is a Hermes Agent memory-provider plugin: a Python
;;;; MemoryProvider that recalls a self-hosted Memobase user profile and
;;;; event timeline over HTTP and writes finished turns back for offline
;;;; profile extraction. This cell makes the same requests through the
;;;; image's own HTTP call -- no Python, nothing fetched at run time, no
;;;; dependency of its own.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-memobase"
  :description "Memobase memory: your self-hosted user profile and event timeline as model tools"
  :license "MIT"
  :version "0.1.0"
  :author "fred0m (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-memobase/test"))))

(defsystem "nodecode-memobase/test"
  :description "Memobase tests. Registered into the shared nodecode.test registry; RUN-MEMOBASE-TESTS filters by the MEMOBASE-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "fred0m (original), Nodecode (rewrite)"
  :depends-on ("nodecode-memobase" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-memobase-tests)))
