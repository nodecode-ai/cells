;;;; package.lisp --- one package, and the words the wire uses.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Named after the system: the loader finds START-CELL as the exported
;;;; symbol of package NODECODE-MEMORIA, and every contribution is filed
;;;; under "nodecode-memoria", which is the folder's name.

(defpackage #:nodecode-memoria
  (:use #:cl)
  (:export #:start-cell #:stop-cell))

(in-package #:nodecode-memoria)

;;; NLK:DEFINE-PERIPHERAL gives this package its condition, MEMORIA-ERROR and
;;; FAIL, and -- with :NOT-RUNNING -- *MEMORIA*, RUNNING-SETTINGS and the
;;; refusal every verb answers while the cell is not started.
(nlk:define-peripheral memoria :not-running t)

(defparameter +memory-types+ '("semantic" "profile" "procedural" "working" "episodic" "tool_result")
  "The memory types the service stores, as memoria_store is offered them.")

(defparameter +signals+ '("useful" "irrelevant" "wrong" "outdated")
  "What memoria_feedback may say about one memory.")

(defparameter +default-origin+ "https://api.thememoria.ai"
  "Memoria Cloud's API origin: the free service the original plugin defaults to.")
