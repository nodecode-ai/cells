;;;; nodecode-tameru.asd --- old tool results, kept small, nothing lost.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of 0xWhiteMage/hermes-tameru-plugin, a context engine
;;;; for another agent that shortens old tool results in place and keeps the
;;;; exact original behind one tool, tameru_expand. Here the shortening is a
;;;; :TOOL point hook and the original lives in this cell's own store.

(defsystem "nodecode-tameru"
  :description "Shorten big tool results to the lines that matter, keeping every distinct error and the exact original"
  :license "MIT"
  :version "0.1.0"
  :author "0xWhiteMage (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tameru/test"))))

(defsystem "nodecode-tameru/test"
  :description "Tameru cell tests. Registered into the shared nodecode.test registry; RUN-TAMERU-TESTS filters by the TAMERU name prefix."
  :license "MIT"
  :author "0xWhiteMage (original), Nodecode (rewrite)"
  :depends-on ("nodecode-tameru" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tameru-tests)))
