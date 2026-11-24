;;;; support.lisp --- DeepCanary test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core DEFTEST,
;;;; with its hermetic machine-state posture) under a DEEPCANARY-CELL- name
;;;; prefix; RUN-DEEPCANARY-TESTS runs exactly that slice. Every test gets a
;;;; scratch home, so the inbox file lands nowhere near the operator's.
;;;;
;;;; Nothing is stubbed and nothing is dialled: the cell's one I/O is a JSON
;;;; file under the home, and the notice is the organism's own board.

(in-package #:nodecode.test)

(define-test-slice "deepcanary" "DEEPCANARY-CELL-" :start nodecode-deepcanary:start-cell)

(defmacro with-deepcanary ((&rest config) &body body)
  "BODY with the cell started on level C2 and on the section pairs CONFIG
gives -- the first pair for a key winning, as the host's config reader does --
and stopped on unwind."
  `(with-cell-stop ((deepcanary-start "level" "C2" ,@config))
     ,@body))

(defun deepcanary-items (&optional (status "all"))
  "The items the inbox file holds, oldest first, or only those with STATUS."
  (let ((items (getf (nodecode-deepcanary::read-inbox) :items)))
    (if (string= status "all")
        items
        (remove-if-not (lambda (item)
                         (string= status (nodecode-deepcanary::item-field item :status)))
                       items))))

(defun deepcanary-count ()
  "How many items the inbox file holds."
  (length (deepcanary-items)))

(defun deepcanary-raise-one (reason &rest keys)
  "An item on REASON raised on runtime evidence, with the words and keys KEYS
gives."
  (apply #'nodecode-deepcanary:raise reason
         (append keys (list :authority "runtime" :summary "something needs a look"))))
