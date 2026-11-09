;;;; package.lisp --- the OSV package: one verb, the word for its failure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed OSV so an eval form reads (osv:lookup "CVE-2024-3651").
;;;; The verb answers a STRING -- the record as the model reads it -- and every
;;;; failure is OSV-ERROR, which the eval snippet renders as
;;;; "ERROR: OSV-ERROR: <detail>".

(defpackage #:nodecode-osv
  (:use #:cl)
  (:nicknames #:osv)
  (:export
   ;; the verb, a string
   #:lookup
   ;; the one condition
   #:osv-error))

(in-package #:nodecode-osv)
