;;;; package.lisp --- the NODECODE-LINEAR-DIGEST package: one tool, one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed LINEAR-DIGEST so an eval form reads
;;;; (linear-digest:digest-text). The package, the nickname, the slash command
;;;; and the manual topic all carry the folder's own slug.

(defpackage #:nodecode-linear-digest
  (:use #:cl)
  (:nicknames #:linear-digest)
  (:export #:start-cell
           #:stop-cell
           #:linear-digest
           #:digest-text
           #:run-command
           #:status-text
           #:running-settings
           #:linear-digest-error #:linear-digest-error-detail))

(in-package #:nodecode-linear-digest)

;;; LINEAR-DIGEST-ERROR, its detail reader, FAIL, the settings variable
;;; (*LINEAR-DIGEST*), the not-running refusal and SETTING all come from here;
;;; cell.lisp declares the section that fills them in.
(nlk:define-peripheral linear-digest :not-running t)
