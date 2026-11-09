;;;; package.lisp --- the NODECODE-RSS package: the tool and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it. It is meant to be used qualified only.

(defpackage #:nodecode-rss
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:rss-error))

(in-package #:nodecode-rss)

;;; DEFINE-PERIPHERAL gives this package its condition RSS-ERROR, FAIL for a
;;; refusal in words, the settings variable *RSS* and RUNNING-SETTINGS.
;;; SETTING, which feed.lisp reads, comes from the NLE:DEFINE-CELL
;;; declaration in cell.lisp, which loads last.
(nlk:define-peripheral rss :not-running t)
