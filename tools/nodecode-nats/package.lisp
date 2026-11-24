;;;; package.lisp --- the NATS package: three verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed NATS so an eval form reads (nats:publish ...).
;;;; Every exported verb answers a STRING and every refusal is NATS-ERROR,
;;;; rendered by the eval snippet as "ERROR: NATS-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `nats' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :stream).

(defpackage #:nodecode-nats
  (:use #:cl)
  (:nicknames #:nats)
  (:export
   ;; verbs, every one a string
   #:publish #:fetch
   ;; the seam a test binds instead of the wire
   #:*publisher* #:*fetcher* #:*prober*
   ;; the one condition
   #:nats-error))

(in-package #:nodecode-nats)

(nlk:define-peripheral nats :not-running t)
