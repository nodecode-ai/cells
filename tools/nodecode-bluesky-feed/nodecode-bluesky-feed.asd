;;;; nodecode-bluesky-feed.asd --- a public Bluesky profile, its posts, and one watched account.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. The folder loader loads it when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `bluesky-feed' section, registers the three tools
;;;; and /bluesky, and puts the manual on (help :bluesky).

(defsystem "nodecode-bluesky-feed"
  :description "Bluesky: a public profile, its recent posts, and one watched account"
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-bluesky-feed/test"))))

(defsystem "nodecode-bluesky-feed/test"
  :description "Bluesky cell tests. Registered into the shared nodecode.test registry; RUN-BLUESKY-FEED-TESTS filters by the BLUESKY-FEED-CELL- name prefix."
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode-bluesky-feed" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-bluesky-feed-tests)))
