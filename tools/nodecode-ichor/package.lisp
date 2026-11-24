;;;; package.lisp --- the ICHOR package: the verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed ICHOR so an eval form reads
;;;; (ichor:status). Every verb answers a string and refuses with
;;;; ICHOR-ERROR, which the eval snippet renders as "ERROR: ICHOR-ERROR: ...".

(defpackage #:nodecode-ichor
  (:use #:cl)
  (:nicknames #:ichor)
  (:export
   ;; the verbs, all string-answering, all callable from eval
   #:status #:review #:keep-pair #:share-pair #:discard-pair
   #:export-pairs #:upload #:forget-all
   ;; data, so a layer can reword it
   #:*primer* #:*statuses* #:*openers* #:*redirects*
   ;; the one condition
   #:ichor-error))

(in-package #:nodecode-ichor)

;;; FAIL and ICHOR-ERROR, which a verb that cannot go on signals, and
;;; DEFINE-VERB, whose functions refuse while the cell is not running, come
;;; from here. SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp,
;;; which loads last.
(nlk:define-peripheral ichor :not-running t)
