;;;; package.lisp --- the AGORA package: the verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed AGORA so an eval form reads (agora:raise-motion ...).
;;;; Every exported verb answers a STRING and every refusal is AGORA-ERROR,
;;;; rendered as "ERROR: AGORA-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `agora' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :roles).

(defpackage #:nodecode-agora
  (:use #:cl)
  (:nicknames #:agora)
  (:export
   ;; the verbs, every one a string
   #:raise-motion #:speak #:vote #:close-motion #:get-motion #:list-motions
   #:create-task #:list-tasks #:close-task
   ;; data, so a layer can reword it
   #:*primer* #:*stance-words* #:*decision-words*
   ;; the one condition
   #:agora-error))

(in-package #:nodecode-agora)

(nlk:define-peripheral agora :not-running t :lock "agora")
