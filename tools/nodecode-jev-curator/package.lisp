;;;; package.lisp --- the NODECODE-JEV-CURATOR package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed CURATOR so an eval form reads
;;;; (curator:plans ...). Every failure the model sees is
;;;; JEV-CURATOR-ERROR, which the eval snippet renders as
;;;; "ERROR: JEV-CURATOR-ERROR: <detail>"; the detail never carries a key.

(defpackage #:nodecode-jev-curator
  (:use #:cl)
  (:nicknames #:curator)
  (:export
   ;; the model's four verbs, answered as text through eval
   #:inventory #:candidates #:judge #:plans #:apply-plan
   ;; the one condition
   #:jev-curator-error))

(in-package #:nodecode-jev-curator)

(nlk:define-peripheral jev-curator :not-running t :filter redact)

;;;; The section and the settings arrive from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last; every other file reads them
;;;; through SETTING.

(defun redact (text)
  "TEXT with every key this cell would send replaced by [redacted]."
  (if (stringp text)
      (uiop:frob-substrings text (ignore-errors (api-keys)) "[redacted]")
      (princ-to-string text)))

(defun nonblank (value)
  "VALUE trimmed when it is a string with something in it, else NIL."
  (and (stringp value)
       (let ((trimmed (nlk:trimmed value)))
         (and (plusp (length trimmed)) trimmed))))

(defun one-line-of (value &optional (limit 400))
  "VALUE as one line of at most LIMIT characters."
  (nlk:clip (nlk:one-line (or value "")) limit :ellipsis "..."))
