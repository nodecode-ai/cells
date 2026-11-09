;;;; nodecode-dashscope-video.asd --- video generation on Alibaba Cloud DashScope.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `dashscope-video' section and registers the video_generate
;;;; tool, the /dashscope-video command and the (help :dashscope-video) topic.
;;;;
;;;; A Nodecode rewrite of rriggs/hermes-plugin-dashscope-video (see NOTICE).

(defsystem "nodecode-dashscope-video"
  :description "DashScope (Qwen Cloud) video generation: short clips on Alibaba Cloud's HappyHorse models, from text, an image or a reference image"
  :author "rriggs (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-dashscope-video/test"))))

(defsystem "nodecode-dashscope-video/test"
  :description "DashScope video tests. Registered into the shared nodecode.test registry; RUN-DASHSCOPE-VIDEO-TESTS filters by the DASHSCOPE-VIDEO-CELL- name prefix."
  :author "rriggs (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-dashscope-video" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-dashscope-video-tests)))
