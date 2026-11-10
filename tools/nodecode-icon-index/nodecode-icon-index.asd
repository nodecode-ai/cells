;;;; nodecode-icon-index.asd --- 40 free CC0 3D icons, their embed code, and their files.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `icon-index' section, registers icon_search, icon_get and
;;;; icon_save plus /icon-index, and puts the manual on (help :icon-index).
;;;;
;;;; Rewritten from minutechreview's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: the catalog is bundled as
;;;; data, and nlk:http fetches the one file icon_save writes.

(defsystem "nodecode-icon-index"
  :description "Free CC0 3D icons: search them, get embed code, save the files into a project"
  :author "minutechreview (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-icon-index/test"))))

(defsystem "nodecode-icon-index/test"
  :description "icon-index tests. Registered into the shared nodecode.test registry; RUN-ICON-INDEX-TESTS filters by the ICON-INDEX-CELL- name prefix."
  :author "minutechreview (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-icon-index" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-icon-index-tests)))
