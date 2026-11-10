;;;; nodecode-makernice.asd --- Makernice video and image generation, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `makernice' section and registers the eight makernice_* tools,
;;;; the /makernice command and the (help :makernice) topic.
;;;;
;;;; A Nodecode rewrite of makernice/makernice-plugin (see NOTICE).

(defsystem "nodecode-makernice"
  :description "Makernice: Seedance, Kling, Wan and MiniMax videos and Seedream or Qwen images, priced before every job and capped by what the user approved"
  :author "makernice (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-makernice/test"))))

(defsystem "nodecode-makernice/test"
  :description "Makernice cell tests. Registered into the shared nodecode.test registry; RUN-MAKERNICE-TESTS filters by the MAKERNICE-CELL- name prefix."
  :author "makernice (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-makernice" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-makernice-tests)))
