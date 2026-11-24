;;;; package.lisp --- the WEKNORA package: four verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed WEKNORA so an eval form reads
;;;; (weknora:search "..."): the same four calls the tools make, and the same
;;;; text they answer, for the operator who would rather type one.
;;;;;;
;;;; Every verb answers a STRING and every refusal is WEKNORA-ERROR, which the
;;;; eval snippet and a tool result both render as "ERROR: ...: <detail>".
;;;;;;
;;;; The settings this cell runs on derive from the one `weknora' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :BASE-URL).

(defpackage #:nodecode-weknora
  (:use #:cl)
  (:nicknames #:weknora)
  ;; SEARCH is the verb the original's workhorse tool is named for, and the one
  ;; name in this package COMMON-LISP owns: shadowed here, and the two places
  ;; this cell reads for a substring say CL:SEARCH to be unambiguous.
  (:shadow #:search)
  (:export
   ;; the four verbs, every one a string
   #:knowledge-bases #:search #:read-document #:ask
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:weknora-error))

(in-package #:nodecode-weknora)

(nlk:define-peripheral weknora :not-running t)
