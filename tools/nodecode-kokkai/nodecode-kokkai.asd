;;;; nodecode-kokkai.asd --- Japan's National Diet proceedings as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes plugin `jp-kokkai' by TakeshiTGAL: three tools
;;;; the model calls -- `kokkai_search', `kokkai_speech', `kokkai_watch' --
;;;; and one slash command, /kokkai, over the National Diet Library's Diet
;;;; Proceedings Search System (kokkai.ndl.go.jp). No key, no subprocess, no
;;;; file beyond the watches the operator saves.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting the directory under
;;;; ~/.nodecode/cells/. Presence is enabled; the package is named after the
;;;; system, which is how the loader finds START-CELL.

(defsystem "nodecode-kokkai"
  :description "Japan Diet meetings, speech excerpts, and a daily watch for new ones"
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "watch")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-kokkai/test"))))

(defsystem "nodecode-kokkai/test"
  :description "Kokkai tests. They register into the shared nodecode.test registry; RUN-KOKKAI-TESTS runs the KOKKAI-CELL- slice."
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode-kokkai" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-kokkai-tests)))
