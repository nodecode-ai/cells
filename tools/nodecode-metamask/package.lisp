;;;; package.lisp --- the NODECODE-METAMASK package: the cell's verbs and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; METAMASK is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp and
;;;; tools.lisp read the section through it. FAIL and METAMASK-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a verb signals when its call cannot be made.
;;;; Nothing here holds a key: MetaMask keeps them, and the CLI holds the login.

(defpackage #:nodecode-metamask
  (:use #:cl)
  (:nicknames #:metamask)
  (:export #:metamask-error #:metamask-error-detail
           #:status #:balance #:market #:history #:swap-quote #:swap-execute
           #:transfer #:sign #:requests #:policy))

(in-package #:nodecode-metamask)

(nlk:define-peripheral metamask :not-running t)
