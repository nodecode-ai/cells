;;;; package.lisp --- the NODECODE-CODEX-TRULY package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed CODEX-TRULY so an eval form reads
;;;; (codex-truly:status).

(defpackage #:nodecode-codex-truly
  (:use #:cl)
  (:nicknames #:codex-truly)
  (:export #:start-cell
           #:stop-cell
           #:status #:report #:apply-windows #:revert-windows #:probe #:codex-truly
           #:running-settings
           #:codex-truly-error #:codex-truly-error-detail))

(in-package #:nodecode-codex-truly)

;;; CODEX-TRULY-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares the
;;; section that fills them in.
(nlk:define-peripheral codex-truly :not-running t)
