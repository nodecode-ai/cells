;;;; nodecode-dreaming.asd --- a nightly consolidation pass over what the organism keeps.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `dreaming' section, registers the three tools
;;;; (dream_sight, dream_report, dream_reject), puts /dream and the manual on
;;;; (help :dreaming), and starts the one worker that runs the pass at the
;;;; configured hour.
;;;;
;;;; Every dependency is already in the serving image: the pass reads the
;;;; organism's own knowledge cell and writes two files of its own.

(defsystem "nodecode-dreaming"
  :description "Nightly memory consolidation: a fact is kept once it comes back on separate days"
  :license "MIT"
  :version "0.1.0"
  :author "itpartypattaya (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-dreaming/test"))))

(defsystem "nodecode-dreaming/test"
  :description "Dreaming tests. Registered into the shared nodecode.test registry; RUN-DREAMING-TESTS filters by the DREAMING-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "itpartypattaya (original), Nodecode (rewrite)"
  :depends-on ("nodecode-dreaming" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-dreaming-tests)))
