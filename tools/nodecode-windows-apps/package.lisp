;;;; package.lisp --- the NODECODE-WINDOWS-APPS package: the sweep and the two tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; WINDOWS-APPS is a nickname, so an eval form reads
;;;; (windows-apps:win-app-table :query "python"). SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; the files
;;;; between read the section through it. FAIL and WINDOWS-APPS-ERROR come
;;;; from NLK:DEFINE-PERIPHERAL, and are what a verb says when its call cannot
;;;; be built at all.
;;;;
;;;; WIN-APP-TABLE and WIN-APP-DETAIL are exported so a person can ask from
;;;; eval what the model asks through the tool of the same name.

(defpackage #:nodecode-windows-apps
  (:use #:cl)
  (:nicknames #:windows-apps)
  (:export #:windows-apps-error
           #:win-app-table #:win-app-detail))

(in-package #:nodecode-windows-apps)

(nlk:define-peripheral windows-apps :not-running t)
