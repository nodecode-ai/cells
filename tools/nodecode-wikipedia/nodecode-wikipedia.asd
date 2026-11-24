;;;; nodecode-wikipedia.asd --- a Wikipedia article as a context reference.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes plugin `wiki-ref' by EloquentBrush0x: one tool
;;;; the model calls, `wikipedia', and one slash command, /wiki, that answer
;;;; with the lead section of a Wikipedia article, or with one named section
;;;; with its subsections. The only peer is the MediaWiki API on
;;;; <language>.wikipedia.org; no key, no file, no subprocess.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting the directory under
;;;; ~/.nodecode/cells/. Presence is enabled; the package is named after the
;;;; system, which is how the loader finds START-CELL.

(defsystem "nodecode-wikipedia"
  :description "A Wikipedia article or one section of it, as a reference the model or /wiki reads"
  :license "MIT"
  :author "EloquentBrush0x (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wiki")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-wikipedia/test"))))

(defsystem "nodecode-wikipedia/test"
  :description "Wikipedia tests. They register into the shared nodecode.test registry; RUN-WIKIPEDIA-TESTS runs the WIKIPEDIA-CELL- slice."
  :license "MIT"
  :author "EloquentBrush0x (original), Nodecode (rewrite)"
  :depends-on ("nodecode-wikipedia" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-wikipedia-tests)))
