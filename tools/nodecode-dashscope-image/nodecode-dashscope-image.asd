;;;; nodecode-dashscope-image.asd --- Alibaba's Wan image models, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `dashscope' section, registers the dashscope_image tool and the
;;;; /dashscope command, and puts the primer on (help :dashscope-image).
;;;;
;;;; Rewritten from rriggs's Hermes Agent plugin of the same name; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to DashScope,
;;;; cl-base64 builds the data URI a local source image rides as.

(defsystem "nodecode-dashscope-image"
  :description "DashScope (Qwen Cloud) image generation: Wan 2.7 models, as the dashscope_image tool and /dashscope"
  :author "rriggs (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-dashscope-image/test"))))

(defsystem "nodecode-dashscope-image/test"
  :description "dashscope-image tests. Registered into the shared nodecode.test registry; RUN-DASHSCOPE-IMAGE-TESTS filters by the DASHSCOPE-IMAGE-CELL- name prefix."
  :author "rriggs (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-dashscope-image" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-dashscope-image-tests)))
