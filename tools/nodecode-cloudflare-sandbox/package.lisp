;;;; package.lisp --- the CLOUDFLARE-SANDBOX package: the model's command and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed CF-SANDBOX so an eval form reads
;;;; (cf-sandbox:run "uname -a"). It is meant to be used qualified only.

(defpackage #:nodecode-cloudflare-sandbox
  (:use #:cl)
  (:nicknames #:cf-sandbox)
  (:export #:run #:health #:release #:reset #:read-file #:write-file
           #:sandbox-name #:cloudflare-sandbox-error))

(in-package #:nodecode-cloudflare-sandbox)

;;; FAIL, CLOUDFLARE-SANDBOX-ERROR, the settings variable, RUNNING-SETTINGS
;;; and DEFINE-VERB come from here; SETTING comes from the NLE:DEFINE-CELL
;;; declaration in cell.lisp, which loads last, and client.lisp reads the
;;; section through it.
(nlk:define-peripheral cloudflare-sandbox :not-running t)
