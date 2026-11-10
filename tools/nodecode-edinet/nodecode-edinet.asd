;;;; nodecode-edinet.asd --- Japanese corporate filings, as three tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `edinet' section, registers the three tools cell.lisp lists,
;;;; puts (help :edinet) on the manual and registers the /edinet command.
;;;;
;;;; A rewrite of TakeshiTGAL's hermes-plugin-edinet (see NOTICE): the plugin
;;;; was three tools over the EDINET API v2. This cell speaks the same two
;;;; GETs itself over nlk:http, so no Hermes runtime, no manifest and no
;;;; private Hermes modules are needed.

(defsystem "nodecode-edinet"
  :description "Japan's EDINET corporate filings: search by company, securities code or date; read a filing's financials"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "csv")
               (:file "filings")
               (:file "verbs")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-edinet/test"))))

(defsystem "nodecode-edinet/test"
  :description "EDINET cell tests. Registered into the shared nodecode.test registry; RUN-EDINET-TESTS filters by the EDINET-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-edinet" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-edinet-tests)))
