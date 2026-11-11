;;;; nodecode-plugin-vet.asd --- a supply-chain gate for anything you are about to install.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `plugin-vet' section, registers the plugin_vet tool and the
;;;; /plugin-vet command, and puts the audit method on (help :plugin-vet).
;;;;
;;;; Rewritten from PerryLink's dsh-skill-pack-security; see NOTICE. The
;;;; original mounted eight audit skills on the model and ran its checks in
;;;; TypeScript; this cell keeps the checks and writes them in Lisp.

(defsystem "nodecode-plugin-vet"
  :description "Plugin Vet: a five-dimension supply-chain risk card for a repository, npm package or folder"
  :author "PerryLink (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "scan")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-plugin-vet/test"))))

(defsystem "nodecode-plugin-vet/test"
  :description "Plugin Vet tests. Registered into the shared nodecode.test registry; RUN-PLUGIN-VET-TESTS filters by the PLUGIN-VET-CELL- name prefix."
  :author "PerryLink (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :depends-on ("nodecode-plugin-vet" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-plugin-vet-tests)))
