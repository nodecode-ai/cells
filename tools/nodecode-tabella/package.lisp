;;;; package.lisp --- the TABELLA package: documents, three tools, three verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed TABELLA so an eval form reads
;;;; (tabella:convert-file ...). The three verbs are the tools the model
;;;; calls; a person can call the same functions from eval, on a path.

(defpackage #:nodecode-tabella
  (:use #:cl)
  (:nicknames #:tabella)
  (:export #:start-cell
           #:stop-cell
           ;; verbs, one per tool, on a plain path
           #:read-text #:document-meta #:convert-file #:create-file
           ;; the one condition, and the seam a test stubs
           #:tabella-error #:tabella-error-detail
           #:*run-argv* #:*program-present-p*))

(in-package #:nodecode-tabella)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and TABELLA-ERROR, which a document that cannot be read
;;; or written signals, come from here: every refusal starts as one of these,
;;; and the tools answer it as the call's failure, never as an unhandled
;;; error.
(nlk:define-peripheral tabella :not-running t)
