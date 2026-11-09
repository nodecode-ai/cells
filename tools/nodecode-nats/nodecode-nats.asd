;;;; nodecode-nats.asd --- a session's door to a NATS JetStream event bus.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of atbrace/hermes-jetstream. Optional ASDF cell, not part of
;;;; the organism core: the folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/, and the gateway calls START-CELL after
;;;; recovery, which reads the `nats' section and registers nats_publish,
;;;; nats_fetch, /nats and (help :nats). No client library: the NATS wire
;;;; protocol over usocket, which the image already has.

(defsystem "nodecode-nats"
  :description "NATS JetStream: publish an event, pull a batch, from a session"
  :license "MIT"
  :author "atbrace (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-nats/test"))))

(defsystem "nodecode-nats/test"
  :description "NATS tests. Registered into the shared nodecode.test registry; RUN-NATS-TESTS filters by the NATS-CELL- name prefix."
  :license "MIT"
  :author "atbrace (original), Nodecode (rewrite)"
  :depends-on ("nodecode-nats" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-nats-tests)))
