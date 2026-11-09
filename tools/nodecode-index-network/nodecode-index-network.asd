;;;; nodecode-index-network.asd --- Index Network: signals, communities and introduced matches.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `index-network' section, registers the nine
;;;; index_* tools and /index, and puts the manual on (help :index-network).
;;;; Every dependency is already in the serving image.

(defsystem "nodecode-index-network"
  :description "Index Network: signals, communities and introduced matches, as tools and a /index command"
  :author "indexnetwork (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-index-network/test"))))

(defsystem "nodecode-index-network/test"
  :description "Index Network tests. Registered into the shared nodecode.test registry; RUN-INDEX-NETWORK-TESTS filters by the INDEX-NETWORK-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-index-network" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-index-network-tests)))
