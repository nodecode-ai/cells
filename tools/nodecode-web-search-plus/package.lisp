;;;; package.lisp --- the WEB-SEARCH-PLUS package: two tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed WEB-SEARCH-PLUS so an eval form reads
;;;; (web-search-plus:search "..."). The two verbs are tools the model calls;
;;;; a person can call the same functions from eval.

(defpackage #:nodecode-web-search-plus
  (:use #:cl)
  (:nicknames #:web-search-plus)
  (:shadow #:search)
  (:export #:start-cell
           #:stop-cell
           #:search #:extract
           #:web-search-plus-error #:web-search-plus-error-detail))

(in-package #:nodecode-web-search-plus)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and WEB-SEARCH-PLUS-ERROR, which a request that cannot go
;;; on signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral web-search-plus :not-running t)
