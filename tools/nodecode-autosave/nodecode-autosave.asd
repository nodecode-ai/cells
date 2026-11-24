;;;; nodecode-autosave.asd --- pre-edit versions of the files a tool changes.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A peripheral: one around-hook on the :TOOL point, one /autosave command,
;;;; one manual topic. See src/CELLS.md, "Declaring a cell".

(defsystem "nodecode-autosave"
  :description "Keeps a pre-edit version of every file a tool changes"
  :license "Apache-2.0"
  :author "xiaoyaner0201 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-autosave/test"))))

(defsystem "nodecode-autosave/test"
  :description "Autosave tests: the lifecycle, the snapshot the hook keeps, restore, the git shadow ref."
  :license "Apache-2.0"
  :author "xiaoyaner0201 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-autosave" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-autosave-tests)))
