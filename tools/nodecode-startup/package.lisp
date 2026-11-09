;;;; package.lisp --- the Startup package, its condition, and its verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The settings this cell runs on are NOT declared here: they derive from
;;;; the one `startup' section declaration in cell.lisp (NLE:DEFINE-CELL),
;;;; read with (SETTING :MAX-ROWS) and the rest, and *STARTUP* below is the
;;;; variable they land in. Every verb answers a string.

(defpackage #:nodecode-startup
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:startup-error
           #:startup-table #:startup-detail #:entries #:platform))

(in-package #:nodecode-startup)

(nlk:define-peripheral startup :not-running t)
