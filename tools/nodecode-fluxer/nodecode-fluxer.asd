;;;; nodecode-fluxer.asd --- Fluxer chat as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `fluxer' section, registers fluxer_send_message,
;;;; fluxer_read_messages, fluxer_list_channels, fluxer_react and
;;;; fluxer_pin, and puts /fluxer and (help :fluxer) up. Every dependency
;;;; is already in the serving image.

(defsystem "nodecode-fluxer"
  :description "Fluxer chat from Nodecode: post, read, react and pin as the bot"
  :author "elkimek (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fluxer/test"))))

(defsystem "nodecode-fluxer/test"
  :description "Fluxer tests. Registered into the shared nodecode.test registry; RUN-FLUXER-TESTS filters by the FLUXER-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-fluxer" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fluxer-tests)))
