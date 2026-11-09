;;;; nodecode-birkin.asd --- a local Markdown memory vault as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `birkin' section, registers the birkin_memory_* tools and
;;;; /birkin, and puts the vault digest on the :ROUND point.
;;;;
;;;; A Nodecode rewrite of ashmoonori-afk/birkin-mnemosyne's Hermes memory
;;;; provider (see NOTICE).

(defsystem "nodecode-birkin"
  :description "Birkin Mnemosyne: a local Markdown memory vault with lexical recall"
  :license "MIT"
  :version "0.1.0"
  :author "ashmoonori-afk (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-birkin/test"))))

(defsystem "nodecode-birkin/test"
  :description "Birkin tests. Registered into the shared nodecode.test registry; RUN-BIRKIN-TESTS filters by the BIRKIN-CELL- name prefix."
  :license "MIT"
  :author "ashmoonori-afk (original), Nodecode (rewrite)"
  :depends-on ("nodecode-birkin" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-birkin-tests)))
