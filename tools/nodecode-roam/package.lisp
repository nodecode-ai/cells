;;;; package.lisp --- one package, named after the system.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of vcruz305/hermes-roam (see NOTICE). The loader finds
;;;; START-CELL as the exported symbol of package NODECODE-ROAM; the
;;;; nickname and every registration (the tools, /roam, (help :roam)) are
;;;; the slug itself, so nothing here takes a word another cell needs.

(defpackage #:nodecode-roam
  (:use #:cl)
  (:nicknames #:roam)
  (:export #:start-cell #:stop-cell #:roam-error #:roam-error-detail))
