;;;; package.lisp --- the CLOUDFLARE-WEB-SEARCH package: one verb, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed CLOUDFLARE-WEB-SEARCH so an eval form reads
;;;; (cloudflare-web-search:search "..."): the same call the model's tool
;;;; makes, answering the same text, for the operator who would rather type one.

(defpackage #:nodecode-cloudflare-web-search
  (:use #:cl)
  (:nicknames #:cloudflare-web-search)
  ;; SEARCH is the verb the original's web-search provider answers with, and a
  ;; name COMMON-LISP owns: shadowed here, so a substring test in this cell
  ;; says CL:SEARCH to be unambiguous.
  (:shadow #:search)
  (:export
   ;; the verb, answering a string
   #:search
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:cloudflare-web-search-error))

(in-package #:nodecode-cloudflare-web-search)

(nlk:define-peripheral cloudflare-web-search :not-running t)
