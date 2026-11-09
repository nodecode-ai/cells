;;;; support.lisp --- relationship-memory test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a WECHAT-CELL-
;;;; name prefix; RUN-WECHAT-TESTS runs exactly that slice. Every test gets a
;;;; scratch home, so the memory file lands nowhere near the operator's.
;;;;
;;;; Nothing is stubbed and nothing is dialled: the cell's one I/O is a JSON
;;;; file under the home, and every answer is a string built from it.

(in-package #:nodecode.test)

(define-test-slice "wechat" "WECHAT-CELL-" :start nodecode-wechat-assistant:start-cell)

(defmacro with-wechat ((&rest config) &body body)
  "BODY with the cell started on its defaults, or on the section pairs CONFIG
gives, and stopped on unwind."
  `(with-cell-stop ((wechat-start ,@config))
     ,@body))

(defmacro wechat-answer (form)
  "FORM's first value: what a verb answers, the text and the dialog left."
  `(first (multiple-value-list ,form)))

(defun wechat-memory-raw ()
  "The memory as the cell reads it."
  (nodecode-wechat-assistant::read-memory))

(defun wechat-note-count ()
  "How many notes the memory holds."
  (length (getf (wechat-memory-raw) :notes)))

(defun wechat-add-quiet (name days)
  "A note on NAME dated DAYS days ago, which is what makes them quiet."
  (nodecode-wechat-assistant:note :contact name :text (format nil "last heard from ~a" name)
                                  :kind "met" :at (format nil "-~ad" days)))
