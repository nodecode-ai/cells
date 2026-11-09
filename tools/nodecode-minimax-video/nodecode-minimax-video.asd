;;;; nodecode-minimax-video.asd --- MiniMax's video generation, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `minimax-video' section, registers the generate_video and
;;;; video_status tools and the /video command, and puts the primer on
;;;; (help :minimax-video).
;;;;
;;;; Rewritten from rriggs's Hermes Agent plugin of the same name; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to MiniMax,
;;;; cl-base64 encodes a local image, alexandria writes the downloaded video.

(defsystem "nodecode-minimax-video"
  :description "MiniMax Hailuo video: generate_video, video_status and /video"
  :author "rriggs (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-minimax-video/test"))))

(defsystem "nodecode-minimax-video/test"
  :description "minimax-video tests. Registered into the shared nodecode.test registry; RUN-MINIMAX-VIDEO-TESTS filters by the MINIMAX-VIDEO-CELL- name prefix."
  :author "rriggs (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-minimax-video" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-minimax-video-tests)))
