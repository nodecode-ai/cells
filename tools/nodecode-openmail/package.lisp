;;;; package.lisp --- the OPENMAIL package: nine tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed OPENMAIL so an eval form reads
;;;; (openmail:list-inboxes). The nine verbs are tools the model calls; a
;;;; person can call the same functions from eval.

(defpackage #:nodecode-openmail
  (:use #:cl)
  (:nicknames #:openmail)
  (:export #:start-cell
           #:stop-cell
           #:whoami #:send-mail #:reply-to-thread #:list-threads #:read-thread
           #:list-messages #:attachment-text #:list-inboxes #:create-inbox
           #:openmail-error #:openmail-error-detail))

(in-package #:nodecode-openmail)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and OPENMAIL-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral openmail :not-running t)
