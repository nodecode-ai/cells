;;;; nodecode-onda.asd --- Onda: the Hits of the creators you follow.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `onda' section, puts ten tools up, and registers /onda and the
;;;; manual topic (help :onda). Ported from getondaru/onda's hermes package
;;;; (see NOTICE).

(defsystem "nodecode-onda"
  :description "Onda: the viral Hits of the Instagram, YouTube and TikTok creators you follow, as tools"
  :license "MIT-0"
  :version "0.1.0"
  :author "getondaru (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-onda/test"))))

(defsystem "nodecode-onda/test"
  :description "Onda tests. Registered into the shared nodecode.test registry; RUN-ONDA-TESTS filters by the ONDA-CELL- name prefix."
  :license "MIT-0"
  :author "getondaru (original), Nodecode (rewrite)"
  :depends-on ("nodecode-onda" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-onda-tests)))
