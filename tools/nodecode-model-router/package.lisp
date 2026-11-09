;;;; package.lisp --- the MODEL-ROUTER package: the same routing the tool does.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MODEL-ROUTER, so an eval form reads
;;;; (model-router:route "summarise this letter"): the same decision the
;;;; model_route tool makes, and the same reasoning, for the operator who
;;;; would rather type one.
;;;;
;;;; Every verb answers a STRING: ROUTE-JSON the result as the JSON the tool
;;;; answers, ROUTE and CATALOG-TEXT as the lines /route prints. A refusal is
;;;; MODEL-ROUTER-ERROR, which the eval snippet and a tool result both render
;;;; as "ERROR: ...".
;;;;
;;;; The settings these run on derive from the one `model-router' section
;;;; declaration in cell.lisp, read with (SETTING :DEFAULT-PREFER).

(defpackage #:nodecode-model-router
  (:use #:cl)
  (:nicknames #:model-router)
  (:export
   ;; the verbs, every one a string
   #:route #:route-json #:catalog-text
   ;; the catalog as data: a plist a row, the same one the tool shows
   #:model-catalog
   ;; data, so a layer can reword or re-point it
   #:*presets* #:*primer*
   ;; the one condition
   #:model-router-error))

(in-package #:nodecode-model-router)

(nlk:define-peripheral model-router :not-running t)
