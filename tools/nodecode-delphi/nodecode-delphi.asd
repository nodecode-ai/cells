;;;; nodecode-delphi.asd --- Gensyn Delphi prediction markets, read from Nodecode.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `delphi' section and registers the seven read-only tools,
;;;; /delphi and (help :delphi).
;;;;
;;;; The original is a Hermes Agent plugin of seven read-only tools over the
;;;; Gensyn Delphi testnet and mainnet APIs. This is the same reads over the
;;;; API's own REST routes through the image's own HTTP call -- no Node, no
;;;; SDK, nothing fetched at run time.

(defsystem "nodecode-delphi"
  :description "Gensyn Delphi prediction markets: markets, positions, history, quotes and an exact edge, read-only"
  :license "MIT"
  :version "0.1.0"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "exact")
               (:file "api")
               (:file "rpc")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-delphi/test"))))

(defsystem "nodecode-delphi/test"
  :description "Delphi tests. Registered into the shared nodecode.test registry; RUN-DELPHI-TESTS filters by the DELPHI-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode-delphi" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-delphi-tests)))
