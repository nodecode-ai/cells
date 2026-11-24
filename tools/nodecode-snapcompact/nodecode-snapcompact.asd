;;;; nodecode-snapcompact.asd --- transcripts as dense bitmap frames.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `snapcompact' section and registers the snapcompact tool and
;;;; the /snapcompact command.
;;;;
;;;; Rewritten from vimona3ds's Hermes Agent plugin of the same name; see
;;;; NOTICE. Nothing here needs a package beside nodecode: the glyphs, the
;;;; layout and the PNG encoder are this folder's own files.

(defsystem "nodecode-snapcompact"
  :description "Snapcompact: transcripts as dense bitmap frames a vision model reads back cheaply"
  :author "vimona3ds (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "font")
               (:file "png")
               (:file "render")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-snapcompact/test"))))

(defsystem "nodecode-snapcompact/test"
  :description "snapcompact tests. Registered into the shared nodecode.test registry; RUN-SNAPCOMPACT-TESTS filters by the SNAPCOMPACT-CELL- name prefix."
  :author "vimona3ds (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-snapcompact" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-snapcompact-tests)))
