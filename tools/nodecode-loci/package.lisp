;;;; package.lisp --- the LOCI package: one condition, the words, the client.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed LOCI so an eval form reads (loci:status-text).
;;;;
;;;; NLK:DEFINE-PERIPHERAL gives this package its condition LOCI-ERROR, FAIL,
;;;; the settings variable *LOCI* and RUNNING-SETTINGS: every refusal names
;;;; what to fix, and a verb called while the cell is stopped says so.

(defpackage #:nodecode-loci
  (:use #:cl)
  (:nicknames #:loci)
  (:export
   ;; the refusal every verb raises
   #:loci-error
   ;; data, so a layer can reword it
   #:+primer+
   ;; one status paragraph, and one tool of the app's called by name
   #:status-text #:call-tool
   ;; the tools the app advertises, newest read
   #:tools))

(in-package #:nodecode-loci)

(nlk:define-peripheral loci :not-running t)
