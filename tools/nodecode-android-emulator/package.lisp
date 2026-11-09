;;;; package.lisp --- the NODECODE-ANDROID-EMULATOR package: the cell's verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; ANDROID-EMULATOR is a nickname, so an eval form
;;;; reads (android-emulator:handle-tap ...). SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; the files
;;;; between read the section through it. FAIL and ANDROID-EMULATOR-ERROR come
;;;; from NLK:DEFINE-PERIPHERAL, and are what a verb says when its call cannot
;;;; be built at all.
;;;;
;;;; The HANDLE- verbs are exported so a person can call any one of the
;;;; eighteen from eval, as the model does through the tool of the same name;
;;;; each takes the record its contract declares.

(defpackage #:nodecode-android-emulator
  (:use #:cl)
  (:nicknames #:android-emulator)
  (:export #:android-emulator-error
           #:handle-status #:handle-shell #:handle-install #:handle-uninstall
           #:handle-screenshot #:handle-tap #:handle-swipe #:handle-type
           #:handle-key #:handle-launch #:handle-packages #:handle-push
           #:handle-pull #:handle-logcat #:handle-gps #:handle-battery
           #:handle-network #:handle-deeplink))

(in-package #:nodecode-android-emulator)

(nlk:define-peripheral android-emulator :not-running t)
