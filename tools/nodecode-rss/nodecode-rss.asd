;;;; nodecode-rss.asd --- your feeds as a tool the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. The folder loader loads it when
;;;; its directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; after recovery, which reads the `rss' section, loads the library and puts
;;;; (help :rss) on the manual.

(defsystem "nodecode-rss"
  :description "Your RSS feeds as a tool: subscribe, refresh, read, star, mute and digest them"
  :license "MIT"
  :author "apoapostolov (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "feed")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-rss/test"))))

(defsystem "nodecode-rss/test"
  :description "RSS cell tests. Registered into the shared nodecode.test registry; RUN-RSS-TESTS filters by the RSS-CELL- name prefix."
  :license "MIT"
  :author "apoapostolov (original), Nodecode (rewrite)"
  :depends-on ("nodecode-rss" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-rss-tests)))
