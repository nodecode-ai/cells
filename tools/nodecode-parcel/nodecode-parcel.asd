;;;; nodecode-parcel.asd --- Parcel deliveries through Parcel's external API.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `parcel' section, registers the three tools the original plugin
;;;; gave its harness (parcel_deliveries, parcel_add_delivery,
;;;; parcel_carriers), puts /parcel and the manual topic on, and hands the
;;;; model a primer.
;;;;
;;;; The original is a Hermes Agent plugin: a Python tool that calls Parcel's
;;;; external API. This cell makes the same requests through the image's own
;;;; HTTP call -- no Python, nothing fetched at run time, no dependency of its
;;;; own -- and answers the same fields as JSON text a model reads.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-parcel"
  :description "Parcel: the deliveries on the way, and a tracking number added, through Parcel's external API"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "promptclickrun (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-parcel/test"))))

(defsystem "nodecode-parcel/test"
  :description "Parcel tests. Registered into the shared nodecode.test registry; RUN-PARCEL-TESTS filters by the PARCEL-CELL- name prefix."
  :license "Apache-2.0"
  :version "0.1.0"
  :author "promptclickrun (original), Nodecode (rewrite)"
  :depends-on ("nodecode-parcel" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-parcel-tests)))
