;;;; package.lisp --- the Doctorpack package, its condition, and its verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The settings this cell runs on are NOT declared here: they derive from
;;;; the one `doctorpack' section declaration in cell.lisp (NLE:DEFINE-CELL),
;;;; read with (SETTING :HOME) and the rest, and *DOCTORPACK* below is the
;;;; variable they land in. Every verb answers a string: the three tools
;;;; answer indented JSON, /doctorpack answers a summary in words.

(defpackage #:nodecode-doctorpack
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:doctorpack-error
           #:audit #:cell-check #:postmortem #:redact))

(in-package #:nodecode-doctorpack)

(nlk:define-peripheral doctorpack :not-running t)
