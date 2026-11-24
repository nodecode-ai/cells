;;;; nodecode-windows-ops.asd --- Windows and WSL triage for Nodecode: one
;;;; prose skill, read by the index and never loaded.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A knowledge cell, as the cell contract puts it: a LAYER system carrying
;;;; (:knowledge . t) and no component. Nothing here is loaded or run. The
;;;; folder's *.lisp files are read as data by the index -- one prose
;;;; (define-skill windows-ops ...) a file -- and (help 'windows-ops) reads it
;;;; back. references/ beside it is the markdown the skill names: the
;;;; copy-paste commands for each check.
;;;;
;;;; The kernel's package is there before any folder's .asd is read at boot.
;;;; A bare ASDF run -- this cell's own test, which reads this file before
;;;; "nodecode" loads -- needs the name to read, so an empty stand-in package
;;;; is made when it is absent, and :defsystem-depends-on loads the kernel
;;;; before ASDF resolves the class below, whose own definition replaces it.

(eval-when (:load-toplevel :execute)
  (unless (find-package "NODECODE.KERNEL")
    (defpackage "NODECODE.KERNEL" (:use #:cl) (:export #:layer))))

(defsystem "nodecode-windows-ops"
  :description "Windows Ops: Windows and WSL triage for Nodecode -- orphaned processes, argv-to-shell interop, BOM, path and console traps"
  :license "MIT"
  :version "0.1.0"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :class nodecode.kernel:layer
  :depends-on ("nodecode")
  :defsystem-depends-on ("nodecode")
  :properties ((:knowledge . t))
  :components ()
  :in-order-to ((test-op (test-op "nodecode-windows-ops/test"))))

(defsystem "nodecode-windows-ops/test"
  :description "The cell's test slice: the folder reads as a knowledge cell, the skill answers its own prose, and the commands it names sit beside it."
  :license "MIT"
  :version "0.1.0"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :depends-on ("nodecode-windows-ops" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-windows-ops-tests)))
