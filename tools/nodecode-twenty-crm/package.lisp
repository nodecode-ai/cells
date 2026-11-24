;;;; package.lisp --- the TWENTY-CRM package: four tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed TWENTY-CRM so an eval form reads
;;;; (twenty-crm:twenty-schema ...). The four verbs are tools the model calls;
;;;; a person can call the same functions from eval.

(defpackage #:nodecode-twenty-crm
  (:use #:cl)
  (:nicknames #:twenty-crm)
  (:export #:start-cell
           #:stop-cell
           #:describe-workspace #:twenty-rest #:twenty-graphql #:twenty-schema
           #:twenty-crm-error #:twenty-crm-error-detail))

(in-package #:nodecode-twenty-crm)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and TWENTY-CRM-ERROR, which a call that cannot go on
;;; signals, come from here: every refusal starts as one of these, and a tool
;;; answers it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral twenty-crm :not-running t)
