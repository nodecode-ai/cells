;;;; package.lisp --- the GENSYN-REE package: three verbs, the doctor, one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed GENSYN-REE so an eval form reads
;;;; (gensyn-ree:validate-receipt "receipt.json"). Every verb answers a
;;;; STRING -- the block the model reads -- and every refusal is
;;;; GENSYN-REE-ERROR, which the eval snippet renders as
;;;; "ERROR: GENSYN-REE-ERROR: <detail>".

(defpackage #:nodecode-gensyn-ree
  (:use #:cl)
  (:nicknames #:gensyn-ree)
  (:export
   ;; the verbs, every one a string
   #:validate-receipt #:verify-receipt #:infer #:doctor
   ;; the one condition
   #:gensyn-ree-error))

(in-package #:nodecode-gensyn-ree)

(nlk:define-peripheral gensyn-ree :not-running t)
