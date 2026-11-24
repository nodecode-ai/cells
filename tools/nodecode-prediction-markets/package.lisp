;;;; package.lisp --- the PREDICTION-MARKETS package: six calculators, the
;;;; condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed PREDICTION-MARKETS so an eval form reads
;;;;   (prediction-markets:calculate-ev :market-price 40 :your-probability 55)
;;;; and the same five others the plugin's tools carry. Every verb answers a
;;;; string the model reads; every refusal is PREDICTION-MARKETS-ERROR, which
;;;; the tool dispatch renders as "ERROR: PREDICTION-MARKETS-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `prediction-markets'
;;;; section declared in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :BANKROLL) and (SETTING :DEFAULT-FRACTION).

(defpackage #:nodecode-prediction-markets
  (:use #:cl)
  (:nicknames #:prediction-markets)
  (:export
   ;; the six calculators the plugin shipped, each answering a string
   #:calculate-ev #:kelly-size #:bayes-update
   #:convert-probability #:base-rate-gap #:combo-edge
   ;; the operator's own door, and the table the base rates live in
   #:run-slash #:base-rate-table
   ;; data over code: a layer may reword this
   #:*primer*
   ;; the one condition
   #:prediction-markets-error))

(in-package #:nodecode-prediction-markets)

;;; NLK:DEFINE-PERIPHERAL gives this package PREDICTION-MARKETS-ERROR, FAIL,
;;; SETTING, RUNNING-SETTINGS and the *PREDICTION-MARKETS* the settings live
;;; in.
(nlk:define-peripheral prediction-markets :not-running t)
