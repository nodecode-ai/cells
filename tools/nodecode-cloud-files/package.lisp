;;;; package.lisp --- the CLOUD-FILES package: five file verbs and their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed CLOUD-FILES so an eval form reads
;;;; (cloud-files:list-files ...). The verbs are tools the model calls; a
;;;; person can call the same functions from eval.

(defpackage #:nodecode-cloud-files
  (:use #:cl)
  (:nicknames #:cloud-files)
  (:export #:start-cell
           #:stop-cell
           #:roots-list #:list-files #:search-files #:read-file #:save-file #:make-folder
           #:cloud-files-error #:cloud-files-error-detail))

(in-package #:nodecode-cloud-files)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and CLOUD-FILES-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; verbs answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral cloud-files :not-running t)
