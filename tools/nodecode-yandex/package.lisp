;;;; package.lisp --- the YANDEX package: two verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed YANDEX so an eval form reads
;;;; (yandex:search "..."): the same calls the tools make, answering the same
;;;; text, for the operator who would rather type one.

(defpackage #:nodecode-yandex
  (:use #:cl)
  (:nicknames #:yandex)
  ;; SEARCH is one of the two verbs and a name COMMON-LISP owns: shadowed
  ;; here, so a substring test in this cell says CL:SEARCH to be unambiguous.
  (:shadow #:search)
  (:export
   ;; the two verbs, each answering a string
   #:search
   #:answer
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:yandex-error))

(in-package #:nodecode-yandex)

;;; FAIL, YANDEX-ERROR, *YANDEX* and RUNNING-SETTINGS land here; DEFINE-VERB,
;;; whose functions refuse while the cell is not running, comes from it too.
(nlk:define-peripheral yandex :not-running t)
