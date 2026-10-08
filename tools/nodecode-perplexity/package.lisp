;;;; package.lisp --- the PERPLEXITY package: the model's verb and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed PERPLEXITY so an eval form reads
;;;; (perplexity:search "..."). It is meant to be used qualified only: SEARCH
;;;; shadows its CL namesake (the model's verb). Inside this package, string
;;;; searching spells CL:SEARCH.

(defpackage #:nodecode-perplexity
  (:use #:cl)
  (:nicknames #:perplexity)
  (:shadow #:search)
  (:export #:search #:perplexity-error))

(in-package #:nodecode-perplexity)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp and signin.lisp read the section through it.
;;; FAIL and PERPLEXITY-ERROR, which a search or a sign-in that cannot go on
;;; signals, and DEFINE-VERB, whose functions refuse while the cell is not
;;; running, come from here too.
(nlk:define-peripheral perplexity :not-running t)

(defparameter +provider+ "perplexity"
  "The id omp keeps Perplexity's credentials under: auth.json's
oauth_tokens.perplexity for a sign-in, api_keys.perplexity for a key.")
