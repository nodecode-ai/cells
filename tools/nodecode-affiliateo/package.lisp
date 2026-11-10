;;;; package.lisp --- the AFFILIATEO package: the server's tools, two verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed AFFILIATEO so an eval form reads
;;;; (affiliateo:call-tool "list_businesses"). The two verbs are tools the
;;;; model calls; a person can call the same functions from eval.

(defpackage #:nodecode-affiliateo
  (:use #:cl)
  (:nicknames #:affiliateo)
  (:export #:start-cell
           #:stop-cell
           #:list-tools
           #:call-tool
           #:affiliateo-error
           #:affiliateo-error-detail))

(in-package #:nodecode-affiliateo)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and AFFILIATEO-ERROR, which a call that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; verbs answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral affiliateo :not-running t)
