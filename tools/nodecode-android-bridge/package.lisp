;;;; package.lisp --- the NODECODE-ANDROID-BRIDGE package: the phone's verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; ANDROID-BRIDGE is a nickname, so an eval form
;;;; reads (android-bridge:call-relay ...). SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; the file
;;;; before it reads the section through it at start. FAIL and
;;;; ANDROID-BRIDGE-ERROR come from NLK:DEFINE-PERIPHERAL, and are what a
;;;; verb says when its call cannot be built at all.
;;;;
;;;; HANDLE-TOOL is exported so a person can call any one of the fifty-eight
;;;; from eval, by the tool's own name, as the model does.

(defpackage #:nodecode-android-bridge
  (:use #:cl)
  (:nicknames #:android-bridge)
  (:export #:android-bridge-error
           #:handle-tool
           #:call-relay
           #:*http*))
