;;;; nodecode-flashdata.asd --- FlashData's Google and YouTube data, as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `flashdata' section, registers the three tools cell.lisp lists
;;;; and puts the plugin's own guidance on the manual as (help :flashdata).
;;;;
;;;; A rewrite of FlashData's Hermes plugin (see NOTICE): the original was a
;;;; declarative MCP client, so this cell speaks the same JSON-RPC to the same
;;;; hosted endpoint and needs neither Hermes nor an MCP runtime of its own.

(defsystem "nodecode-flashdata"
  :description "FlashData: Google web, news, image, shopping, places and video search plus YouTube research, through your own FlashData key"
  :author "rocosen (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "render")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-flashdata/test"))))

(defsystem "nodecode-flashdata/test"
  :description "FlashData tests. Registered into the shared nodecode.test registry; RUN-FLASHDATA-TESTS filters by the FLASHDATA-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-flashdata" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-flashdata-tests)))
