;;;; package.lisp --- the NODECODE-PLUGIN-VET package: one verb, one condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed PLUGIN-VET so an eval form reads
;;;; (plugin-vet:vet "owner/repo"). Meant to be used qualified only.

(defpackage #:nodecode-plugin-vet
  (:use #:cl)
  (:nicknames #:plugin-vet)
  (:export #:vet #:plugin-vet-error #:plugin-vet-error-detail))

(in-package #:nodecode-plugin-vet)

;;; PLUGIN-VET-ERROR is the one condition every refusal signals, with a
;;; readable detail; FAIL signals it. *PLUGIN-VET*, RUNNING-SETTINGS and
;;; DEFINE-VERB come from here too: a verb opens with the refusal that a
;;; stopped cell says. SETTING comes from the NLE:DEFINE-CELL declaration in
;;; cell.lisp, which loads last.
(nlk:define-peripheral plugin-vet :not-running t)
