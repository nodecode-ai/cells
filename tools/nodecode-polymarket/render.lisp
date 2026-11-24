;;;; render.lisp --- the briefing, in markdown ready to send.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-polymarket)

(defparameter *disclaimer*
  "Informational only, not financial advice. Prices are Polymarket's public data and can be stale or thin."
  "The line every briefing ends with; a layer may reword it.")

(defun brief-markdown (alerts movers signals closing errors)
  "One markdown briefing: alerts, movers, what is worth a look, what closes soon."
  (format nil "# Prediction Market Brief~%~%~a~%~%~a~%~%~a~%~%~a~@[~%~%~{~%_Note: ~a_~}~]~%~%~%_~a_"
          (alert-list "Watchlist alerts" alerts)
          (market-list "Biggest 24h movers" movers)
          (signal-list "Worth a closer look" signals)
          (market-list "Closing soon" closing)
          errors
          *disclaimer*))
