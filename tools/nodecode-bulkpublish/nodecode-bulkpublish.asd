;;;; nodecode-bulkpublish.asd --- fifteen social networks behind one API, as one cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `bulkpublish' section, registers the two tools and /bulkpublish,
;;;; and puts (help :bulkpublish) on the manual.
;;;;
;;;; Ported from bulkpublish's bulkpublish/hermes-bulkpublish (see NOTICE):
;;;; the plugin is a manifest, one MCP server entry and prose skills, so the
;;;; rewrite is an MCP client over streamable HTTP plus the workflow those
;;;; skills taught, in the manual. Every dependency rides with nodecode.

(defsystem "nodecode-bulkpublish"
  :description "Publish, schedule and measure social posts on 15 networks through BulkPublish"
  :license "MIT"
  :version "0.1.0"
  :author "bulkpublish (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-bulkpublish/test"))))

(defsystem "nodecode-bulkpublish/test"
  :description "BulkPublish tests. Registered into the shared nodecode.test registry; RUN-BULKPUBLISH-TESTS filters by the BULKPUBLISH-CELL- name prefix."
  :license "MIT"
  :author "bulkpublish (original), Nodecode (rewrite)"
  :depends-on ("nodecode-bulkpublish" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-bulkpublish-tests)))
