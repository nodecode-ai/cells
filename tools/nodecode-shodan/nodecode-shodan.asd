;;;; nodecode-shodan.asd --- Shodan's host, search, count, DNS, CVE and scan surface, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `shodan' section, registers the twelve tools and puts the
;;;; primer on (help :shodan) and the /shodan command in the catalog.
;;;;
;;;; Rewritten from Adolanium's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http is the one seam,
;;;; and two endpoints answer with no key at all.

(defsystem "nodecode-shodan"
  :description "Shodan: host intel, search, counts, DNS, CVEs and scanning, with a session credit budget"
  :author "Adolanium (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "wire")
               (:file "shape")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-shodan/test"))))

(defsystem "nodecode-shodan/test"
  :description "Shodan tests. Registered into the shared nodecode.test registry; RUN-SHODAN-TESTS filters by the SHODAN-CELL- name prefix."
  :author "Adolanium (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-shodan" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-shodan-tests)))
