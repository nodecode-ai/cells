;;;; package.lisp --- the Fluxer package and its one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed FLUXER so an eval form reads
;;;; (fluxer:send-message "123" "hello"): every exported verb answers a
;;;; STRING and every refusal is FLUXER-ERROR, rendered by the tool or the
;;;; eval snippet as "ERROR: FLUXER-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on are NOT declared here: they derive from
;;;; the one `fluxer' section declaration in cell.lisp (NLE:DEFINE-CELL),
;;;; read with (SETTING :bot-token), and *FLUXER* below is the variable
;;;; they land in.

(defpackage #:nodecode-fluxer
  (:use #:cl)
  (:nicknames #:fluxer)
  (:export
   ;; verbs, every one a string
   #:send-message #:read-messages #:list-channels #:react #:pin-message
   ;; the one condition
   #:fluxer-error))

(in-package #:nodecode-fluxer)

(nlk:define-peripheral fluxer :not-running t)
