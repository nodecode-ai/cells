;;;; nodecode-slash-router.asd --- a misspelled /command finds the right one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/, and the gateway then calls START-CELL, which
;;;; reads the `slash-router' section, registers /slash-router and puts the
;;;; manual on (help :slash-router). The verbs themselves are Lisp functions
;;;; the model calls through eval. Every dependency is already in the
;;;; serving image.

(defsystem "nodecode-slash-router"
  :description "Send a misspelled or shortened /command to the live command it meant"
  :license "MIT"
  :version "0.1.0"
  :author "raitoxlol (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-slash-router/test"))))

(defsystem "nodecode-slash-router/test"
  :description "Slash-router tests. Registered into the shared nodecode.test registry; RUN-SLASH-ROUTER-TESTS filters by the SLASH-ROUTER-CELL- name prefix."
  :license "MIT"
  :author "raitoxlol (original), Nodecode (rewrite)"
  :depends-on ("nodecode-slash-router" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-slash-router-tests)))
