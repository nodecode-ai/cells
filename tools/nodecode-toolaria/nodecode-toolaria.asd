;;;; nodecode-toolaria.asd --- spill oversized tool results, fetch slices back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The .asd sits inside its folder: a cell is a directory carrying its
;;;; .asd, presence under ~/.nodecode/cells/ is enabled. The system is named
;;;; by the file, and the folder's contributions are filed under its name.

(defsystem "nodecode-toolaria"
  :description "Spills oversized tool results to disk and fetches them back by slice"
  :license "MIT"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-toolaria/test"))))

(defsystem "nodecode-toolaria/test"
  :description "Toolaria tests: the spill, the handle, every fetch mode, /rescuer."
  :license "MIT"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-toolaria" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-toolaria-tests)))
