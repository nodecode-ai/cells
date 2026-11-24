;;;; nodecode-disciplined-vibe-coding.asd --- eleven prose skills for one coding
;;;; task: intake, define, plan, teardown, build, verify, commit.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A knowledge cell, as the cell contract puts it: a LAYER system carrying
;;;; (:knowledge . t) and no component. Nothing here is loaded or run. The
;;;; folder's *.lisp files are read as data by the index -- one prose
;;;; (define-skill ...) a file, the eleven SKILL.md files of the original
;;;; plugin -- and (help 'name) reads one back. guides/ beside them is the
;;;; markdown those skills name: the economy ladder and the three templates.
;;;;
;;;; The kernel's package is there before any folder's .asd is read at boot.
;;;; A bare ASDF run -- this cell's own test, which reads this file before
;;;; "nodecode" loads -- needs the name to read, so an empty stand-in package
;;;; is made when it is absent, and :defsystem-depends-on loads the kernel
;;;; before ASDF resolves the class below, whose own definition replaces it.

(eval-when (:load-toplevel :execute)
  (unless (find-package "NODECODE.KERNEL")
    (defpackage "NODECODE.KERNEL" (:use #:cl) (:export #:layer))))

(defsystem "nodecode-disciplined-vibe-coding"
  :description "Disciplined vibe coding: eleven skills that run a coding task through intake, plan, build and verify"
  :license "MIT"
  :version "0.1.0"
  :author "bmtrnavsky (original), Nodecode (rewrite)"
  :class nodecode.kernel:layer
  :depends-on ("nodecode")
  :defsystem-depends-on ("nodecode")
  :properties ((:knowledge . t))
  :components ()
  :in-order-to ((test-op (test-op "nodecode-disciplined-vibe-coding/test"))))

(defsystem "nodecode-disciplined-vibe-coding/test"
  :description "The cell's test slice: that the folder reads as a knowledge cell and each skill answers its own prose."
  :license "MIT"
  :version "0.1.0"
  :author "bmtrnavsky (original), Nodecode (rewrite)"
  :depends-on ("nodecode-disciplined-vibe-coding" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-disciplined-vibe-coding-tests)))
