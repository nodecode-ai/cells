;;;; nodecode-kata.asd --- the routine compiler: a :TOOL hook, one tool, /kata.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;; A rewrite of opeblow/kata (https://github.com/opeblow/kata) at commit
;;;; cc59b08f1bbeeb02524c060faac04edb34de0564, MIT, rewritten in Common Lisp.

(defsystem "nodecode-kata"
  :description "Routines: repeated tool calls mined, compiled, one call to replay"
  :license "MIT"
  :version "0.1.0"
  :author "opeblow (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-kata/test"))))

(defsystem "nodecode-kata/test"
  :depends-on ("nodecode-kata" "nodecode/test")
  :components ((:file "test/cell-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-kata-tests)))
