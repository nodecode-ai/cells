;;;; nodecode-egov-law.asd --- Japanese statutes from e-Gov, as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `egov-law' section, registers the three tools cell.lisp lists,
;;;; the /egov-law command and (help :egov-law).
;;;;
;;;; Rewritten from TakeshiTGAL's hermes-plugin-egov-law; see NOTICE. The
;;;; original was three Python tools over the e-Gov Law API v2 and a skill;
;;;; this cell speaks the same API itself, through nlk:http.

(defsystem "nodecode-egov-law"
  :description "Japanese laws (e-Gov): search a statute, read an article, check amendment dates"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "laws")
               (:file "article")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-egov-law/test"))))

(defsystem "nodecode-egov-law/test"
  :description "egov-law cell tests. Registered into the shared nodecode.test registry; RUN-EGOV-LAW-TESTS filters by the EGOV-LAW-CELL- name prefix."
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-egov-law" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-egov-law-tests)))
