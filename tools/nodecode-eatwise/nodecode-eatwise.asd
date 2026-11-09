;;;; nodecode-eatwise.asd --- a Chinese meal nutrition review, on this machine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `eatwise' section and registers the four tools,
;;;; /eatwise and (help :eatwise).
;;;;
;;;; Everything is local: a table of 467 foods compiled into DATA, the
;;;; Mifflin-St Jeor arithmetic, and one JSON file under the home for the
;;;; profile and the meal history.

(defsystem "nodecode-eatwise"
  :description "Chinese meal nutrition review, daily targets and a local meal history"
  :license "MIT"
  :author "harrylabsj (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell")
               (:file "data"))
  :in-order-to ((test-op (test-op "nodecode-eatwise/test"))))

(defsystem "nodecode-eatwise/test"
  :description "Eatwise tests. Registered into the shared nodecode.test registry; RUN-EATWISE-TESTS filters by the EATWISE-CELL- name prefix."
  :license "MIT"
  :author "harrylabsj (original), Nodecode (rewrite)"
  :depends-on ("nodecode-eatwise" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-eatwise-tests)))
