;;;; package.lisp --- the DISCORD-PRESENCE package: the IPC transport, three tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed DISCORD-PRESENCE so an eval form reads
;;;; (discord-presence:show-presence ...). The three verbs are tools the model
;;;; calls; a person can call the same functions from eval.

(defpackage #:nodecode-discord-presence
  (:use #:cl)
  (:nicknames #:discord-presence)
  (:export #:start-cell
           #:stop-cell
           #:show-presence #:clear-presence #:presence-status
           #:discord-presence-error #:discord-presence-error-detail
           #:*transport* #:*paused*))

(in-package #:nodecode-discord-presence)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and PRESENCE-ERROR, which one call that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral discord-presence :not-running t)
