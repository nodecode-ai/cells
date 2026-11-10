;;;; nodecode-omonpakari.asd --- a considerate health journal as seven tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `omonpakari' section, registers the seven tools, puts the
;;;; manual on (help :omonpakari), and adds a short picture of today to the
;;;; standing context before each reply.
;;;;
;;;; Rewritten from tamamo510's Hermes Agent plugin of the same name; see
;;;; NOTICE. Already zipped in the release: nlk:http speaks to OpenWeather,
;;;; and the journal is one JSON file under the organism's home.

(defsystem "nodecode-omonpakari"
  :description "Health journal, barometric pressure, cycle windows and interaction checks as tools"
  :author "tamamo510 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-omonpakari/test"))))

(defsystem "nodecode-omonpakari/test"
  :description "Omonpakari tests, registered into the shared nodecode.test slice runner."
  :author "tamamo510 (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-omonpakari" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-omonpakari-tests)))
