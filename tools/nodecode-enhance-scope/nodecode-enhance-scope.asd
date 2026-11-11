;;;; nodecode-enhance-scope.asd --- your text at the end of every system prompt.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `enhance' section, advises the two functions the system prompt is
;;;; read through, hooks the :tool point, registers /enhance-scope and puts the
;;;; manual on (help :enhance-scope).
;;;;
;;;; A port of wufeng0105's dsh-enhance (see NOTICE).

(defsystem "nodecode-enhance-scope"
  :description "Enhance: your text at the end of every system prompt, the opening line rewritten, and a tool veto"
  :license "MIT"
  :author "wufeng0105 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-enhance-scope/test"))))

(defsystem "nodecode-enhance-scope/test"
  :description "Enhance tests. Registered into the shared nodecode.test registry; RUN-ENHANCE-SCOPE-TESTS filters by the ENHANCE-SCOPE-CELL- name prefix."
  :license "MIT"
  :author "wufeng0105 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-enhance-scope" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-enhance-scope-tests)))
