;;;; package.lisp --- the PROCGEN-STUDIO package: the verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed PROCGEN-STUDIO so an eval form reads
;;;; (procgen-studio:draw ...). Every verb answers a STRING -- JSON for a
;;;; tool call, the answer's text for /procgen-studio -- and every refusal is
;;;; PROCGEN-STUDIO-ERROR, which the tool dispatch renders as ERROR: with the
;;;; detail the condition carries.
;;;;
;;;; The settings this cell runs on derive from the one `procgen-studio'
;;;; section declaration in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :PRESET) and the rest.

(defpackage #:nodecode-procgen-studio
  (:use #:cl)
  (:nicknames #:procgen-studio)
  (:export
   ;; verbs: a rendered drawing, the contract it was drawn against, and the
   ;; re-render of a stored source. The first three are the tools' bodies.
   #:draw #:contract-text #:reroll #:renders
   ;; the resolved style a verb runs on, and the presets it may name
   #:resolve-style #:preset-names
   ;; data, so a layer can reword it
   #:*presets* #:*palette-roles* #:*rasterize*
   ;; the one condition
   #:procgen-studio-error))

(in-package #:nodecode-procgen-studio)
