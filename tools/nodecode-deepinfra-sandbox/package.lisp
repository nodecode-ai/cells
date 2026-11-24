;;;; package.lisp --- the NODECODE-DEEPINFRA-SANDBOX package: its verbs and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it; DEEPINFRA-SANDBOX is a nickname, so an eval form reads
;;;; (deepinfra-sandbox:exec "ls -l"). SETTING comes from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last; wire.lisp and tools.lisp read
;;;; the section through it. FAIL, DEEPINFRA-SANDBOX-ERROR and DEFINE-VERB,
;;;; whose functions refuse while the cell is not running, come from
;;;; NLK:DEFINE-PERIPHERAL here.

(defpackage #:nodecode-deepinfra-sandbox
  (:use #:cl)
  (:nicknames #:deepinfra-sandbox)
  (:export #:deepinfra-sandbox-error #:deepinfra-sandbox-error-detail
           #:exec #:sandbox))

(in-package #:nodecode-deepinfra-sandbox)

(nlk:define-peripheral deepinfra-sandbox :not-running t)
