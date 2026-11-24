;;;; package.lisp --- the WEBLY package: the verbs a user reaches, and the words.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, nicknamed WEBLY so an eval form reads (webly:status).
;;;; Every exported verb answers a STRING and every refusal is WEBLY-ERROR,
;;;; rendered by the eval snippet as "ERROR: WEBLY-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `webly' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :timeout).

(defpackage #:nodecode-webly
  (:use #:cl)
  (:nicknames #:webly)
  (:export
   ;; verbs, every one a string
   #:publish #:share #:status #:claim-link
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:webly-error))

(in-package #:nodecode-webly)

(nlk:define-peripheral webly :not-running t)
