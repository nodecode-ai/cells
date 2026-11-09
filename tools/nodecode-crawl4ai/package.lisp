;;;; package.lisp --- the CRAWL4AI package: the condition, the settings, the seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, used qualified from the outside; the model reaches it through
;;;; the crawl4ai_extract tool cell.lisp registers. A refusal in a body signals
;;;; CRAWL4AI-ERROR, which the engine renders to the model as "ERROR: <detail>".
;;;;
;;;; NLK:DEFINE-PERIPHERAL gives this package CRAWL4AI-ERROR, FAIL, the
;;;; settings variable *CRAWL4AI*, RUNNING-SETTINGS (the refusal a verb gives
;;;; before the cell started) and the REDACT filter that keeps the bearer token
;;;; out of every refusal. The typed settings derive from the one `crawl4ai'
;;;; section declaration in cell.lisp.

(defpackage #:nodecode-crawl4ai
  (:use #:cl)
  (:export #:crawl4ai-error))

(in-package #:nodecode-crawl4ai)

(nlk:define-peripheral crawl4ai :not-running t :filter redact)

