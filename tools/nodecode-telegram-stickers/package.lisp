;;;; package.lisp --- the TELEGRAM-STICKERS package: three tools, the /stickers
;;;; command, their one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed TELEGRAM-STICKERS so an eval form reads
;;;; (telegram-stickers:find-stickers ...). The three verbs are tools the model
;;;; calls; a person can call the same functions from eval.

(defpackage #:nodecode-telegram-stickers
  (:use #:cl)
  (:nicknames #:telegram-stickers)
  (:export #:start-cell
           #:stop-cell
           #:find-stickers #:send-sticker #:sticker-settings #:run-slash
           #:telegram-stickers-error #:telegram-stickers-error-detail))

(in-package #:nodecode-telegram-stickers)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL, TELEGRAM-STICKERS-ERROR and RUNNING-SETTINGS, which a
;;; call that cannot go on signals, come from here: every refusal starts as one
;;; of these, and the tools answer it as the call's failure rather than as an
;;; unhandled error.
(nlk:define-peripheral telegram-stickers :not-running t)
