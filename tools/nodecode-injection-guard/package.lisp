;;;; package.lisp --- the INJECTION-GUARD package: two tools, one slash
;;;; command, the :TOOL hook that screens a result, and the condition a
;;;; refusal signals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed INJECTION-GUARD so an eval form
;;;; reads (injection-guard:scan ...). The verb a model calls is two tools
;;;; (injection_guard_scan, injection_guard_stats); a person types
;;;; /injection-guard. The hook is the plugin's own middleware, at the one
;;;; point Nodecode has for it.

(defpackage #:nodecode-injection-guard
  (:use #:cl)
  (:nicknames #:injection-guard)
  (:export #:start-cell
           #:stop-cell
           #:scan #:protected-p #:file-reader-p #:exempt-families
           #:guard-tool-result #:screen #:report-text #:run-slash
           #:injection-guard-error #:injection-guard-error-detail))

(in-package #:nodecode-injection-guard)

;;; SETTING is a function DEFINE-CELL defines in this package in cell.lisp,
;;; which loads last; the layers before it call it, so its type is declared
;;; here rather than discovered as an undefined function.
(declaim (ftype (function (keyword) t) setting))

;;; FAIL and INJECTION-GUARD-ERROR, which a scan that cannot go on signals,
;;; come from here: every refusal starts as one of these, and a tool answers
;;; it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral injection-guard :not-running t)
