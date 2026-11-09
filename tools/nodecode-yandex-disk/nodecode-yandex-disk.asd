;;;; nodecode-yandex-disk.asd --- Yandex Disk as fifteen model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `yandex-disk' section and installs the tools the original
;;;; plugin gave its harness.
;;;;
;;;; The original is a Hermes Agent plugin in Python (see NOTICE): fifteen
;;;; tools over the Yandex Disk REST API v1. This cell makes the same
;;;; requests through the image's own HTTP call -- no Python, nothing fetched
;;;; at run time, no dependency of its own -- and answers the same fields as
;;;; lines a model reads.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-yandex-disk"
  :description "Yandex Disk: browse, read, write, upload, download, share, delete and restore files, with your own OAuth token"
  :license "MIT"
  :version "0.1.0"
  :author "akinfold (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "paths")
               (:file "client")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-yandex-disk/test"))))

(defsystem "nodecode-yandex-disk/test"
  :description "Yandex Disk tests. Registered into the shared nodecode.test registry; RUN-YANDEX-DISK-TESTS filters by the YADISK-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "akinfold (original), Nodecode (rewrite)"
  :depends-on ("nodecode-yandex-disk" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-yandex-disk-tests)))
