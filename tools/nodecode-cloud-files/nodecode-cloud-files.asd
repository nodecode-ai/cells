;;;; nodecode-cloud-files.asd --- the folders an operator exposes, as file verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cloud-files' section, registers the five tools and puts the
;;;; primer on (help :cloud-files).
;;;;
;;;; Rewritten from electricsheephq's Hermes Agent plugin of the same name; see
;;;; NOTICE. Everything it needs rides with nodecode: no HTTP, no key, the
;;;; filesystem alone.

(defsystem "nodecode-cloud-files"
  :description "Cloud Files: list, search, read, save and create folders in the folders you expose"
  :author "electricsheephq (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" #+unix "sb-posix")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cloud-files/test"))))

(defsystem "nodecode-cloud-files/test"
  :description "Cloud Files tests. Registered into the shared nodecode.test registry; RUN-CLOUD-FILES-TESTS filters by the CLOUD-FILES-CELL- name prefix."
  :author "electricsheephq (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-cloud-files" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cloud-files-tests)))
