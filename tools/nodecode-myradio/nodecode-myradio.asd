;;;; nodecode-myradio.asd --- the myradio station as the model's deck.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Optional ASDF cell, NOT part of the organism core. The folder loader
;;;; loads it at boot when its directory sits under ~/.nodecode/cells/; the
;;;; gateway calls START-CELL after recovery, which reads the `myradio'
;;;; section, registers twelve tools on the station's own HTTP API and
;;;; /radio, and puts the DJ manual on (help :myradio).
;;;;
;;;; It reaches the station the way the studio does: HTTP on the loopback
;;;; port, the token the station writes in its data folder as
;;;; control.token. Every dependency is already in the serving image.

(defsystem "nodecode-myradio"
  :description "Your myradio station as the model's deck: now playing, the queue, song requests, DJ lines"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "newjordan (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-myradio/test"))))

(defsystem "nodecode-myradio/test"
  :description "myradio tests, against a stubbed station: registered into the shared nodecode.test registry, RUN-MYRADIO-TESTS filters by the MYRADIO-CELL- name prefix."
  :license "Apache-2.0"
  :author "newjordan (original), Nodecode (rewrite)"
  :depends-on ("nodecode-myradio" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-myradio-tests)))
