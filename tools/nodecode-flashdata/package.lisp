;;;; package.lisp --- the NODECODE-FLASHDATA package: the cell's verbs and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; FLASHDATA is a nickname, so an eval form reads
;;;; (flashdata:search "..."). SETTING comes from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last; the files between read the
;;;; section through it. FAIL and FLASHDATA-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a verb signals when its call cannot be made.

(defpackage #:nodecode-flashdata
  (:use #:cl)
  (:nicknames #:flashdata)
  (:shadow #:search)
  (:export #:flashdata-error #:flashdata-error-detail
           #:search #:news #:images #:shopping #:places #:videos
           #:youtube-search #:youtube-extended-search #:metadata #:transcript
           #:captions #:trainability #:channel #:suggestions
           #:pricing #:job #:result #:call))

(in-package #:nodecode-flashdata)

(nlk:define-peripheral flashdata :not-running t)
