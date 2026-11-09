;;;; package.lisp --- the NODECODE-DIFF-REVIEW package: the cell's tool and its rules.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; DIFF-REVIEW is a nickname so an eval form reads
;;;; (diff-review:...). SETTING comes from the NLE:DEFINE-CELL declaration in
;;;; cell.lisp, which loads last; diff.lisp and git.lisp read the section
;;;; through it. FAIL and DIFF-REVIEW-ERROR come from NLK:DEFINE-PERIPHERAL,
;;;; and are what a tool says when its call cannot be built at all.

(defpackage #:nodecode-diff-review
  (:use #:cl)
  (:nicknames #:diff-review)
  (:export #:diff-review-error))

(in-package #:nodecode-diff-review)

(nlk:define-peripheral diff-review :not-running t)
