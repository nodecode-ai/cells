;;;; package.lisp --- the NODECODE-TELEGRAM-NOTIFY package: one tool, one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed TELEGRAM-NOTIFY so an eval form reads
;;;; (telegram-notify:notify "...").

(defpackage #:nodecode-telegram-notify
  (:use #:cl)
  (:nicknames #:telegram-notify)
  (:export #:start-cell
           #:stop-cell
           #:send-notification #:notify #:status
           #:running-settings
           #:telegram-notify-error #:telegram-notify-error-detail))

(in-package #:nodecode-telegram-notify)

;;; TELEGRAM-NOTIFY-ERROR, its detail reader, FAIL, the settings variable,
;;; the not-running refusal and SETTING's companion all come from here;
;;; cell.lisp declares the section that fills them in.
(nlk:define-peripheral telegram-notify :not-running t)
