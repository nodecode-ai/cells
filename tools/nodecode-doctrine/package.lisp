;;;; package.lisp --- the DOCTRINE package: the verbs, and the prose behind them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed DOCTRINE so an eval form reads
;;;; (doctrine:status-text). Every verb answers a STRING and every refusal is
;;;; DOCTRINE-ERROR, which the eval snippet renders as "ERROR: DOCTRINE-ERROR:
;;;; <detail>". The settings this cell runs on derive from the one `doctrine'
;;;; section declaration in cell.lisp, read with (SETTING :FORM).

(defpackage #:nodecode-doctrine
  (:use #:cl)
  (:nicknames #:doctrine)
  (:export
   ;; the lifecycle the folder loader calls
   #:start-cell #:stop-cell #:setting
   ;; verbs, every one a string
   #:status-text #:show-text #:plugins-text
   ;; data, so a layer can reword it
   #:*footer*
   ;; the one condition
   #:doctrine-error))

(in-package #:nodecode-doctrine)

;;; FAIL and DOCTRINE-ERROR, the *DOCTRINE* settings variable, RUNNING-SETTINGS
;;; and DEFINE-VERB come from here; SETTING comes from the NLE:DEFINE-CELL
;;; declaration in cell.lisp, which loads last.
(nlk:define-peripheral doctrine :not-running t)
