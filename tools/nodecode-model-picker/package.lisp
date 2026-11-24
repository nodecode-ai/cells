;;;; package.lisp --- the MODEL-PICKER package: one verb, the tiers, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MODEL-PICKER so an eval form reads
;;;; (model-picker:escalate-model ...). The settings this cell runs on derive
;;;; from the one `model-picker' section declaration in cell.lisp
;;;; (NLE:DEFINE-CELL), read with (SETTING :low-model) and its siblings.

(defpackage #:nodecode-model-picker
  (:use #:cl)
  (:nicknames #:model-picker)
  (:export
   ;; the one verb a model calls, and the one an operator may run from eval
   #:escalate-model
   ;; data, so a layer can reword the routing policy without the code
   #:*tier-order* #:*tier-aliases* #:+primer+))

(in-package #:nodecode-model-picker)

(nlk:define-peripheral model-picker :not-running t)
