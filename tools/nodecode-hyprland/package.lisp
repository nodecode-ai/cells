;;;; package.lisp --- the HYPRLAND package: one desktop, driven by coordinates.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed HYPRLAND so an eval form reads
;;;; (hyprland:windows). Every verb answers a STRING, and every refusal is
;;;; HYPRLAND-ERROR, rendered by the eval snippet as
;;;; "ERROR: HYPRLAND-ERROR: <detail>".

(defpackage #:nodecode-hyprland
  (:use #:cl)
  (:nicknames #:hyprland)
  (:export
   ;; the verbs, each answering text for eval
   #:status #:windows #:apps #:capture #:click #:drag #:scroll #:type-text
   #:press-key #:focus
   ;; the seams a layer or a test rebinds
   #:*run* #:*which* #:*timeout-seconds* #:*frame* #:*target*
   ;; the one condition
   #:hyprland-error #:hyprland-error-detail))

(in-package #:nodecode-hyprland)

(nlk:define-peripheral hyprland :not-running t)
