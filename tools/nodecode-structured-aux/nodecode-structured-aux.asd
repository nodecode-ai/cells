;;;; nodecode-structured-aux.asd --- OpenRouter's decision endpoint, as verbs the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `structured-aux' section, puts (help :structured-aux) on the
;;;; manual and registers /structured-aux.
;;;;
;;;; A rewrite of trajectoire-ai's hermes-structured-aux-models (see NOTICE).

(defsystem "nodecode-structured-aux"
  :description "Bounded OpenRouter decisions: judge a command, pick among tools, digest context"
  :license "MIT"
  :version "0.1.0"
  :author "trajectoire-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "decide")
               (:file "compress")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-structured-aux/test"))))

(defsystem "nodecode-structured-aux/test"
  :description "The structured-aux cell's tests, registered into the shared nodecode.test registry."
  :license "MIT"
  :author "trajectoire-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-structured-aux" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-structured-aux-tests)))
