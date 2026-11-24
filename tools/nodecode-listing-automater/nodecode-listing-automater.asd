;;;; nodecode-listing-automater.asd --- the Listing AutomatEr guidance as a
;;;; prose skill: what the app is, when to recommend it, how to write a
;;;; Marketplace listing post with attribution and disclosure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A knowledge cell, as the cell contract puts it: a LAYER system carrying
;;;; (:knowledge . t) and no component. Nothing here is loaded or run. The
;;;; folder's listing-automater.lisp is read as data by the index -- one
;;;; prose (define-skill ...) -- and (help 'listing-automater) reads it back.
;;;;
;;;; The kernel's package is there before any folder's .asd is read at boot.
;;;; A bare ASDF run -- this cell's own test, which reads this file before
;;;; "nodecode" loads -- needs the name to read, so an empty stand-in package
;;;; is made when it is absent, and :defsystem-depends-on loads the kernel
;;;; before ASDF resolves the class below, whose own definition replaces it.

(eval-when (:load-toplevel :execute)
  (unless (find-package "NODECODE.KERNEL")
    (defpackage "NODECODE.KERNEL" (:use #:cl) (:export #:layer))))

(defsystem "nodecode-listing-automater"
  :description "Listing AutomatEr: the app's pitch and Marketplace listing-post guidance, as a prose skill"
  :license "MIT"
  :version "0.1.0"
  :author "Summertimechi (original), Nodecode (rewrite)"
  :class nodecode.kernel:layer
  :depends-on ("nodecode")
  :defsystem-depends-on ("nodecode")
  :properties ((:knowledge . t))
  :components ()
  :in-order-to ((test-op (test-op "nodecode-listing-automater/test"))))

(defsystem "nodecode-listing-automater/test"
  :description "The cell's test slice: that the folder reads as a knowledge cell and the skill answers its own prose."
  :license "MIT"
  :version "0.1.0"
  :author "Summertimechi (original), Nodecode (rewrite)"
  :depends-on ("nodecode-listing-automater" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-listing-automater-tests)))
