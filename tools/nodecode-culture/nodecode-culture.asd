;;;; nodecode-culture.asd --- business culture for 57 economies, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `culture' section, registers the three tools the original
;;;; plugin gave its harness, puts /culture and the manual topic on, and hands
;;;; the model a primer. A rewrite of pswong661-eng/hermes-global-business-culture
;;;; (see NOTICE): the same bundled research data, reimplemented as Lisp over
;;;; the image's own JSON and section seams, with no Python and nothing
;;;; downloaded at run time.

(defsystem "nodecode-culture"
  :description "Country culture briefs and home-vs-host gap analysis for cross-border business dealings"
  :license "MIT"
  :author "pswong661-eng (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-culture/test"))))

(defsystem "nodecode-culture/test"
  :description "Culture tests. Registered into the shared nodecode.test registry; RUN-CULTURE-TESTS filters by the CULTURE-CELL- name prefix."
  :license "MIT"
  :author "pswong661-eng (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode-culture" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-culture-tests)))
