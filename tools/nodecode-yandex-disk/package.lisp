;;;; package.lisp --- the NODECODE-YANDEX-DISK package, its condition, its settings.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed YADISK, so an eval form reads
;;;; (yadisk:list-path "/Documents"): the same calls the tools make, answering
;;;; the same text, for the operator who would rather type one.

(defpackage #:nodecode-yandex-disk
  (:use #:cl)
  (:nicknames #:yadisk)
  ;; COPY and DELETE are names COMMON-LISP owns and the verbs here are the
  ;; disk's own: shadowed, so a cell test that means the sequence function
  ;; says CL:DELETE.
  (:shadow #:copy #:delete)
  (:export
   ;; read
   #:disk-info
   #:list-path
   #:search-file
   #:read-file
   ;; transfers, in and out
   #:download
   #:upload
   ;; structure
   #:mkdir
   #:write-file
   #:copy
   #:move
   #:delete
   #:publish
   ;; the bin
   #:trash-list
   #:trash-restore
   #:trash-empty
   ;; data, so a layer can reword it
   #:*manual*
   ;; the one condition
   #:yandex-disk-error))

(in-package #:nodecode-yandex-disk)

;;; FAIL, YANDEX-DISK-ERROR, *YANDEX-DISK* and RUNNING-SETTINGS land here.
(nlk:define-peripheral yandex-disk :not-running t)
