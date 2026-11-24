;;;; nodecode-nextdns.asd --- NextDNS query logs, blocked queries and analytics, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `nextdns' section, registers the four read-only tools, and puts
;;;; the primer on (help :nextdns) and the /nextdns command in the catalog.
;;;;
;;;; Rewritten from shanelord01's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http is the one seam,
;;;; and every endpoint this cell reads is a GET.

(defsystem "nodecode-nextdns"
  :description "NextDNS: DNS query logs, blocked queries with the matching blocklist, and aggregate analytics"
  :author "shanelord01 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "verbs")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-nextdns/test"))))

(defsystem "nodecode-nextdns/test"
  :description "NextDNS tests. Registered into the shared nodecode.test registry; RUN-NEXTDNS-TESTS filters by the NEXTDNS-CELL- name prefix."
  :author "shanelord01 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-nextdns" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-nextdns-tests)))
