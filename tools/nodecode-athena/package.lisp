;;;; package.lisp --- the ATHENA package: seven tools, one command, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed ATHENA so an eval form reads
;;;; (athena:search-store "voice clone"). The verbs are tools the model calls;
;;;; a person can call the same functions from eval.

(defpackage #:nodecode-athena
  (:use #:cl)
  (:nicknames #:athena)
  (:export #:start-cell
           #:stop-cell
           #:search-store #:list-apps #:install-app #:launch-app
           #:stop-app #:read-logs #:store-status
           #:athena-error #:athena-error-detail))

(in-package #:nodecode-athena)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and ATHENA-ERROR, which a verb that cannot go on signals,
;;; come from here: every refusal starts as one of these, and the tools answer
;;; it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral athena :not-running t)
