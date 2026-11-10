;;;; nodecode-minimax-image.asd --- MiniMax's Image-01 models, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `minimax_image' section, registers the minimax_image tool and
;;;; the /minimax-image command, and puts the primer on (help :minimax-image).
;;;;
;;;; Rewritten from rriggs's Hermes Agent plugin of the same name; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to MiniMax,
;;;; cl-base64 builds the data URI a local source image rides as.

(defsystem "nodecode-minimax-image"
  :description "MiniMax image generation: Image-01 models, as the minimax_image tool and /minimax-image"
  :author "rriggs (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-minimax-image/test"))))

(defsystem "nodecode-minimax-image/test"
  :description "minimax-image tests. Registered into the shared nodecode.test registry; RUN-MINIMAX-IMAGE-TESTS filters by the MINIMAX-IMAGE-CELL- name prefix."
  :author "rriggs (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-minimax-image" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-minimax-image-tests)))
