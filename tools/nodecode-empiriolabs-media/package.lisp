;;;; package.lisp --- the EMPIRIOLABS-MEDIA package: four verbs and one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed EMPIRIOLABS-MEDIA so an eval form reads
;;;; (empiriolabs-media:image "a red fox"). Every exported verb answers a
;;;; string and every refusal is EMPIRIOLABS-MEDIA-ERROR, rendered by the eval
;;;; snippet as "ERROR: ...".
;;;;
;;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;;; loads last; media.lisp reads the section through it. FAIL and the
;;;; condition, and DEFINE-VERB, whose functions refuse while the cell is not
;;;; running, come from NLK:DEFINE-PERIPHERAL here.

(defpackage #:nodecode-empiriolabs-media
  (:use #:cl)
  (:nicknames #:empiriolabs-media)
  (:export
   ;; the verbs, every one a string
   #:image #:video #:speech #:models
   ;; the words
   #:*primer*
   ;; the one condition
   #:empiriolabs-media-error #:empiriolabs-media-error-detail))

(in-package #:nodecode-empiriolabs-media)

(nlk:define-peripheral empiriolabs-media :not-running t)
