;;;; package.lisp --- the NODECODE-BLUEPRINT package: the six tools and the /blueprint command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed BLUEPRINT so an eval form reads
;;;; (blueprint:status-text). The nickname, the slash command and the manual
;;;; topic are the folder's slug and nothing shorter.

(defpackage #:nodecode-blueprint
  (:use #:cl)
  (:nicknames #:blueprint)
  (:export #:start-cell #:stop-cell
           #:phase-tool #:profile-tool #:research-tool
           #:verdict-tool #:plan-tool #:recheck-tool
           #:run-command #:status-text #:standing-card
           #:running-settings
           #:blueprint-error #:blueprint-error-detail))

(in-package #:nodecode-blueprint)

;;; BLUEPRINT-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares the
;;; section that fills them in.
(nlk:define-peripheral blueprint :not-running t)
