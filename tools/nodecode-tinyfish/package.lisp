;;;; package.lisp --- the NODECODE-TINYFISH package and its one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed TINYFISH so an eval form reads
;;;; (tinyfish:search-text "query"): every verb answers a STRING and every
;;;; refusal is TINYFISH-ERROR, rendered by the tool as
;;;; ERROR: TINYFISH-ERROR: <detail>.
;;;;
;;;; The settings this cell runs on are NOT declared here: they derive from
;;;; the one `tinyfish' section declaration in cell.lisp (NLE:DEFINE-CELL),
;;;; read with (SETTING :api-key), and *TINYFISH* below is the variable they
;;;; land in.

(defpackage #:nodecode-tinyfish
  (:use #:cl)
  (:nicknames #:tinyfish)
  (:export
   ;; the verbs, every one a string
   #:search-text #:fetch-text #:agent-text #:profiles-text #:create-profile-text
   #:status-text #:usage-text #:run-command
   ;; the one condition
   #:tinyfish-error))

(in-package #:nodecode-tinyfish)

(nlk:define-peripheral tinyfish :not-running t)

(defparameter +version+ "0.1.0"
  "This cell's version, sent as the client version the API records.")
