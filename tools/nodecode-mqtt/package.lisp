;;;; package.lisp --- the MQTT package: three verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MQTT so an eval form reads (mqtt:publish ...).
;;;; Every exported verb answers a STRING and every refusal is MQTT-ERROR,
;;;; rendered by the eval snippet as "ERROR: MQTT-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `mqtt' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :url).

(defpackage #:nodecode-mqtt
  (:use #:cl)
  (:nicknames #:mqtt)
  (:export
   ;; verbs, every one a string
   #:publish #:read-topics
   ;; the seam a test binds instead of the wire
   #:*publisher* #:*reader* #:*prober*
   ;; the one condition
   #:mqtt-error))

(in-package #:nodecode-mqtt)

(nlk:define-peripheral mqtt :not-running t)
