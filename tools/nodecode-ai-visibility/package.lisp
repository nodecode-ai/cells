;;;; package.lisp --- the one package: the verbs and the word for their failure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed AI-VISIBILITY so an eval form reads
;;;; (ai-visibility:check "example.com"). Every verb answers a STRING -- the
;;;; server's own reading, as a person would read it -- and every failure is
;;;; AI-VISIBILITY-ERROR, which the eval snippet renders as
;;;; "ERROR: AI-VISIBILITY-ERROR: <detail>".

(defpackage #:nodecode-ai-visibility
  (:use #:cl)
  (:nicknames #:ai-visibility)
  (:export
   ;; the verbs, strings
   #:check
   #:report
   #:info
   ;; the one condition
   #:ai-visibility-error))

(in-package #:nodecode-ai-visibility)
