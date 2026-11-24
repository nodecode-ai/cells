;;;; package.lisp --- the ICON-INDEX package: three tools, one command, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed ICON-INDEX so an eval form reads
;;;; (icon-index:icon-search "security"). Every verb answers a string: the
;;;; tools answer the JSON the model reads, the command a line of text.

(defpackage #:nodecode-icon-index
  (:use #:cl)
  (:nicknames #:icon-index)
  (:export
   #:start-cell
   #:stop-cell
   ;; the verbs, the same three the tools dispatch to
   #:icon-search
   #:icon-get
   #:icon-save
   #:icon-index-command
   ;; the data, so a layer can reword or extend it
   #:*catalog*
   #:*icon-license*))

(in-package #:nodecode-icon-index)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and ICON-INDEX-ERROR, which a request that cannot go on
;;; signals, come from here.
(nlk:define-peripheral icon-index :not-running t)
