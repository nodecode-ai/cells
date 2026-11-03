;;;; cell.lisp --- the planted defect. SPDX-License-Identifier: MIT

(defpackage #:selffix-plant
  (:use #:cl)
  (:export #:start-cell))

(in-package #:selffix-plant)

(defun planted-session-p (session-id)
  "True for a session one of whose inputs says SELFFIX-PLANT."
  (some (lambda (row) (search "SELFFIX-PLANT" (first row)))
        (nlk:events :kind "turn.started" :session-id session-id :as :rows :columns '("payload"))))

(defun planted (next session-id)
  (when (and session-id (planted-session-p session-id))
    (funcall 'rules-section-gone session-id))
  (funcall next session-id))

;; A raw encapsulation, not a cell :hook: the hook runner contains a hook's
;; error, and the defect this plants is one nothing contains.
(defun start-cell (&rest arguments)
  "The loader's entry: plant the defect."
  (declare (ignore arguments))
  (unless (sb-int:encapsulated-p 'nle::read-harness-sections 'selffix-plant)
    (sb-int:encapsulate 'nle::read-harness-sections 'selffix-plant #'planted)))

(start-cell)
