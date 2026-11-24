;;;; nodecode-mock-review.asd --- review boards for UI mockups, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `mock-review' section, registers the five mock_review_*
;;;; tools, puts /mock-review and (help :mock-review) up, and answers
;;;; /mock-review/ on the gateway's own port with the board. Every dependency
;;;; is already in the serving image.

(defsystem "nodecode-mock-review"
  :description "Review boards for UI mockups: decisions marked on a page, read back as one digest"
  :author "apelabs-dev (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "board")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mock-review/test"))))

(defsystem "nodecode-mock-review/test"
  :description "Mock review tests. Registered into the shared nodecode.test registry; RUN-MOCK-REVIEW-TESTS filters by the MOCK-REVIEW-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-mock-review" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mock-review-tests)))
