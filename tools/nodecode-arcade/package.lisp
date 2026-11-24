;;;; package.lisp --- the arcade package: the cabinet, the store, the verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed ARCADE so an eval form reads (arcade:board "snake").
;;;; Every refusal is ARCADE-ERROR, rendered by the eval snippet as
;;;; "ERROR: ARCADE-ERROR: <detail>". The settings the cabinet runs on derive
;;;; from the one `arcade' section declaration in cell.lisp (NLE:DEFINE-CELL),
;;;; read with (SETTING :keep-scores).

(defpackage #:nodecode-arcade
  (:use #:cl)
  (:nicknames #:arcade)
  (:export
   ;; the condition and its one helper
   #:arcade-error #:arcade-error-detail #:fail
   ;; the cabinet
   #:+games+ #:game-ids #:game-row #:game-label
   #:start-game #:advance-run #:reset-cabinet
   #:run-game #:run-score #:run-over-p #:run-text
   ;; the scores
   #:normalize-initials #:*store-file* #:store-path #:empty-store
   #:read-store #:write-store #:record-play #:submit-score #:board #:summary
   ;; what the cell contributes
   #:run-games #:run-play #:run-top-scores #:run-stats #:run-slash
   #:start-cell #:stop-cell))

(in-package #:nodecode-arcade)

(nlk:define-peripheral arcade :not-running t)
