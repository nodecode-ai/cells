;;;; nodecode-deja.asd --- memory from the coding sessions already on this machine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `deja' section, registers the three tools and the /deja command,
;;;; hooks the :ROUND point so a session's first request carries deja's digest,
;;;; and puts the manual on (help :deja).
;;;;
;;;; The one thing outside this folder is the deja program itself: a Go binary
;;;; the user installs (brew install deja-vu, or go install
;;;; github.com/vshulcz/deja-vu/cmd/deja@latest), which indexes the session
;;;; transcripts every coding agent on the machine already writes to disk. See
;;;; NOTICE: this folder is a rewrite of vshulcz/deja-vu's Hermes plugin.

(defsystem "nodecode-deja"
  :description "deja: memory from the coding sessions every agent on this machine already wrote"
  :license "MIT"
  :version "0.1.0"
  :author "vshulcz (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-deja/test"))))

(defsystem "nodecode-deja/test"
  :description "deja tests. Registered into the shared nodecode.test registry; RUN-DEJA-TESTS filters by the DEJA-CELL- name prefix."
  :license "MIT"
  :author "vshulcz (original), Nodecode (rewrite)"
  :depends-on ("nodecode-deja" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-deja-tests)))
