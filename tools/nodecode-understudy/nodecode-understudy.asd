;;;; nodecode-understudy.asd --- audition cheaper models on your own past decisions.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `understudy' section and registers the two tools cell.lisp
;;;; lists and the /understudy command.
;;;;
;;;; A rewrite of stffinfcti's understudy Hermes plugin (see NOTICE): the
;;;; plugin replayed past decisions through Hermes's own model facade; this
;;;; cell replays them through one OpenAI-compatible endpoint over nlk:http.

(defsystem "nodecode-understudy"
  :description "Audition cheaper models on your own past decisions: takeover rate, misses, tokens"
  :author "stffinfcti (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "history")
               (:file "wire")
               (:file "audition")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-understudy/test"))))

(defsystem "nodecode-understudy/test"
  :description "Understudy cell tests. Registered into the shared nodecode.test registry; RUN-UNDERSTUDY-TESTS filters by the UNDERSTUDY-CELL- name prefix."
  :author "stffinfcti (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-understudy" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-understudy-tests)))
