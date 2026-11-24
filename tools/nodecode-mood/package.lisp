;;;; package.lisp --- the MOOD package: four verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MOOD so an eval form reads (mood:show). Every
;;;; exported verb answers a STRING and every refusal is MOOD-ERROR, rendered
;;;; by the eval snippet as "ERROR: MOOD-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `mood' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :QUIET).

(defpackage #:nodecode-mood
  (:use #:cl)
  (:nicknames #:mood)
  (:export
   ;; the loader's two, as every cell exports them
   #:start-cell #:stop-cell
   ;; verbs, every one a string
   #:show #:reset #:set-enabled
   ;; data, so a layer can reword the line
   #:*framing* #:*open-framing*
   ;; the one condition
   #:mood-error))

(in-package #:nodecode-mood)

(nlk:define-peripheral mood :not-running t :lock "mood")
