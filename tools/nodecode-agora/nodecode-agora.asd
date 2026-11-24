;;;; nodecode-agora.asd --- a motion a fact, a vote a tally, an adopted
;;;; conclusion a task.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `agora' section, registers the nine tools, puts /agora
;;;; and the manual on (help :agora).
;;;;
;;;; The ledger is one file under the home: the log is the authority, and
;;;; every verb is a read-modify-write of it under one lock. Every
;;;; dependency is already in the serving image.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting - or symlinking -
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-agora"
  :description "A team that argues a motion in rounds, votes, and turns the adopted conclusion into tasks"
  :license "MIT"
  :version "0.1.0"
  :author "yzy806806 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-agora/test"))))

(defsystem "nodecode-agora/test"
  :description "Agora tests. Registered into the shared nodecode.test registry; RUN-AGORA-TESTS filters by the AGORA-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "yzy806806 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-agora" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-agora-tests)))
