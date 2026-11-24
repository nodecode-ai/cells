;;;; package.lisp --- the NODECODE-EFFORTLESS package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed EFFORTLESS so an eval form reads
;;;; (effortless:pick-effort ...). Every failure the model sees is
;;;; EFFORTLESS-ERROR, whose detail never carries a key.

(defpackage #:nodecode-effortless
  (:use #:cl)
  (:nicknames #:effortless)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:pick-effort
   ;; the one condition
   #:effortless-error))

(in-package #:nodecode-effortless)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; judge.lisp and this file's functions read the section through
;;; it.
(nlk:define-peripheral effortless :not-running t :filter redact)

(defun replace-all (needle replacement text)
  "TEXT with every NEEDLE written as REPLACEMENT; NEEDLE empty leaves TEXT."
  (if (or (null needle) (zerop (length needle)) (null text))
      text
      (with-output-to-string (out)
        (loop with start = 0
              for at = (search needle text :start2 start)
              do (cond (at (write-string (subseq text start at) out)
                           (write-string replacement out)
                           (setf start (+ at (length needle))))
                       (t (write-string (subseq text start) out)
                          (return)))))))

(defun redact (text)
  "TEXT with every key this cell would send replaced by [redacted]."
  (dolist (key (list (ignore-errors (setting :typesafe-key))
                      (uiop:getenv "TYPESAFE_API_KEY")
                      (ignore-errors (setting :custom-api-key)))
           text)
    (when (and (stringp key) (plusp (length key)))
      (setf text (replace-all key "[redacted]" text)))))
