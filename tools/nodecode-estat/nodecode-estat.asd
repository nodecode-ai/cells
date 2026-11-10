;;;; nodecode-estat.asd --- Japanese government statistics, as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of the Hermes Agent plugin `jp-estat' by TakeshiTGAL.
;;;; The original gave its harness three read-only tools over the e-Stat API
;;;; (search tables, describe one table, fetch its numbers); this cell gives
;;;; the same three, the same host and the same data, as text a model reads.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting the directory under
;;;; ~/.nodecode/cells/. Presence is enabled; the package is named after the
;;;; system, which is how the loader finds START-CELL.

(defsystem "nodecode-estat"
  :description "Japan's official statistics from e-Stat: search tables, describe one, read its figures"
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "estat")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-estat/test"))))

(defsystem "nodecode-estat/test"
  :description "e-Stat tests. They register into the shared nodecode.test registry; RUN-ESTAT-TESTS runs the ESTAT-CELL- slice."
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode-estat" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-estat-tests)))
