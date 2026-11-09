;;;; nodecode-rocketchat.asd --- Rocket.Chat rooms, history and messages as ten tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `rocketchat' section, registers the ten tools and puts the
;;;; primer on (help :rocketchat).
;;;;
;;;; Rewritten from HalfbitStudio's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http speaks to the
;;;; instance's REST API v1.

(defsystem "nodecode-rocketchat"
  :description "Rocket.Chat: list rooms, read history and threads, search, post, upload and DM"
  :author "HalfbitStudio (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-rocketchat/test"))))

(defsystem "nodecode-rocketchat/test"
  :description "Rocket.Chat tests. Registered into the shared nodecode.test registry; RUN-ROCKETCHAT-TESTS filters by the ROCKETCHAT-CELL- name prefix."
  :author "HalfbitStudio (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-rocketchat" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-rocketchat-tests)))
