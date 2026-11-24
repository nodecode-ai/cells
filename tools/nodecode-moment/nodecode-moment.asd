;;;; nodecode-moment.asd --- a session's house files, fingerprinted, and the receipt when one moves.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of noprijbi99-dotcom's moment as a Nodecode cell: the files a
;;;; session stands on are fingerprinted at its first round, re-checked every
;;;; round after, and when one changed the model gets one receipt naming the
;;;; file, what moved and the text as it stands now.

(defsystem "nodecode-moment"
  :description "Reports when a house file changed under a session, and hands the model the new text"
  :license "MIT"
  :version "0.1.0"
  :author "noprijbi99-dotcom (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-moment/test"))))

(defsystem "nodecode-moment/test"
  :description "Moment tests. Registered into the shared nodecode.test registry; RUN-MOMENT-TESTS filters by the MOMENT-CELL- name prefix."
  :license "MIT"
  :author "noprijbi99-dotcom (original), Nodecode (rewrite)"
  :depends-on ("nodecode-moment" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-moment-tests)))
