;;;; nodecode-vidiq.asd --- vidIQ's creator intelligence as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `vidiq' section and puts (help :vidiq) on the manual.
;;;;
;;;; Rewritten from vid-io/vidiq-mcp's Hermes plugin (see NOTICE). Every
;;;; dependency rides with nodecode: nlk:http is the one HTTP seam,
;;;; nlk:decode-json the one reader.

(defsystem "nodecode-vidiq"
  :description "vidIQ: live YouTube, Instagram and TikTok creator intelligence, and the workflows that use it"
  :license "Apache-2.0"
  :author "vid-io (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "verbs")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-vidiq/test"))))

(defsystem "nodecode-vidiq/test"
  :description "vidIQ tests. Registered into the shared nodecode.test registry; RUN-VIDIQ-TESTS filters by the VIDIQ-CELL- name prefix."
  :license "Apache-2.0"
  :depends-on ("nodecode-vidiq" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-vidiq-tests)))
