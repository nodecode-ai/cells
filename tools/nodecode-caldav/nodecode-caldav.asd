;;;; nodecode-caldav.asd --- one CalDAV collection, read-only, as two tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `caldav' section, registers the two tools the original plugin
;;;; gave its harness (list_events and read_event), puts /caldav and the
;;;; manual topic on, and hands the model a primer.
;;;;
;;;; The original is a Hermes Agent plugin: Python tools that send one
;;;; calendar-query REPORT to a CalDAV collection and read the multistatus
;;;; that comes back. This cell makes the same request through the image's own
;;;; HTTP call -- no Python, nothing fetched at run time, no dependency of its
;;;; own beyond what the image already loads -- and answers the same fields as
;;;; lines a model reads.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-caldav"
  :description "CalDAV calendar: list and read the events of a collection you run, as two read-only tools"
  :license "MIT"
  :version "0.1.0"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "xml")
               (:file "ics")
               (:file "caldav")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-caldav/test"))))

(defsystem "nodecode-caldav/test"
  :description "CalDAV tests. Registered into the shared nodecode.test registry; RUN-CALDAV-TESTS filters by the CALDAV-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode-caldav" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-caldav-tests)))
