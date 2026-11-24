;;;; nodecode-hwiki.asd --- a markdown wiki as long-term memory.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `hwiki' section, registers the four wiki_memory_* tools and
;;;; /hwiki, indexes the wiki, and recalls from it before each model request.
;;;;
;;;; A Nodecode rewrite of ottosulin/hwiki's Hermes memory provider (see NOTICE).

(defsystem "nodecode-hwiki"
  :description "A markdown wiki as long-term memory: plain pages you and the agent edit, searched locally"
  :license "MIT"
  :version "0.1.0"
  :author "ottosulin (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hwiki/test"))))

(defsystem "nodecode-hwiki/test"
  :description "hwiki tests. Registered into the shared nodecode.test registry; RUN-HWIKI-TESTS filters by the HWIKI-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "ottosulin (original), Nodecode (rewrite)"
  :depends-on ("nodecode-hwiki" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hwiki-tests)))
