;;;; nodecode-kame-rotate.asd --- one chat call through a pool of API keys.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `kame' section, puts the tools and the /kame command the
;;;; declaration in cell.lisp lists on the organism, and (help :kame) on the
;;;; manual.
;;;;
;;;; A rewrite of KAME's provider half for Hermes (see NOTICE): the same key
;;;; pool, the same reading of a refusal, the next key instead of a failed
;;;; call. Every dependency rides with nodecode (dexador for the wire).

(defsystem "nodecode-kame-rotate"
  :description "One chat completion through a rotating pool of API keys: the healthiest key per call, the next key instead of a failed one"
  :license "MIT"
  :version "0.1.0"
  :author "Kame696 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-kame-rotate/test"))))

(defsystem "nodecode-kame-rotate/test"
  :description "KAME rotation tests. Registered into the shared nodecode.test registry; RUN-KAME-ROTATE-TESTS filters by the KAME-ROTATE-CELL- name prefix."
  :license "MIT"
  :author "Kame696 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-kame-rotate" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-kame-rotate-tests)))
