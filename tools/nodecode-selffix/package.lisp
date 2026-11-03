;;;; package.lisp --- the SELFFIX package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed SELFFIX so an eval form reads (selffix:selffix :status).
;;;; Every exported verb answers a STRING, and a refusal is the peripheral's
;;;; condition, which the eval snippet renders as an ERROR line.

(defpackage #:nodecode-selffix
  (:use #:cl)
  (:nicknames #:selffix)
  (:export #:selffix))

(in-package #:nodecode-selffix)

(nlk:define-peripheral selffix :not-running t :lock "selffix")

(defvar *root* nil
  "The folder the loop keeps its ledger, lock, findings and scratch homes
in, or NIL for selffix/ under the organism's home. A test binds it.")

(defun selffix-path (&optional (relative ""))
  "RELATIVE under the loop's folder."
  (if *root*
      (merge-pathnames relative (uiop:ensure-directory-pathname *root*))
      (nlk:home (concatenate 'string "selffix/" relative))))

(defun iso (universal)
  "UNIVERSAL as the event log's ISO-8601 UTC text."
  (nlk:iso-time (- universal nlk:+unix-epoch+)))
