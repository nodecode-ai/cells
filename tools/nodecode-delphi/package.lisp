;;;; package.lisp --- the DELPHI package: seven read verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed DELPHI so an eval form reads
;;;; (delphi:markets) or (delphi:edge "0.62" "0.45"): the same call the
;;;; model's tools make, answering the same text, for the operator who would
;;;; rather type one.

(defpackage #:nodecode-delphi
  (:use #:cl)
  (:nicknames #:delphi)
  (:export
   ;; the verbs, each answering a string
   #:markets #:market #:positions #:edge #:history #:quote-trade
   ;; data, so a layer can reword it
   #:*primer* #:*creator-notice*
   ;; the one condition
   #:delphi-error))

(in-package #:nodecode-delphi)

(nlk:define-peripheral delphi :not-running t)
