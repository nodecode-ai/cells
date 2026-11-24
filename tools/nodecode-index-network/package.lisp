;;;; package.lisp --- the INDEX-NETWORK package: the verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed INDEXNETWORK so an eval form reads
;;;; (indexnetwork:signals "design partner"). Every exported verb answers a
;;;; STRING; every refusal is INDEX-NETWORK-ERROR, rendered to the model as
;;;; "ERROR: INDEX-NETWORK-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `index-network'
;;;; section declared in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :api-key).

(defpackage #:nodecode-index-network
  (:use #:cl)
  (:nicknames #:indexnetwork)
  (:export
   ;; verbs, every one a string
   #:signals #:opportunities #:networks #:docs
   ;; data, so a layer can reword it
   #:*row-chars* #:*line-format*
   ;; the one condition
   #:index-network-error))

(in-package #:nodecode-index-network)

(nlk:define-peripheral index-network :not-running t)
