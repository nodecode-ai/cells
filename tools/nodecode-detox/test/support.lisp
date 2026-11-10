;;;; support.lisp --- detox test runner and the little helpers its tests share.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the core's own registry (DEFTEST) under a
;;;; DETOX-CELL- prefix, and RUN-DETOX-TESTS runs exactly that slice. Nothing
;;;; is stubbed: the hook, the files it writes and the tools that read them
;;;; are the cell's own. Each test gets a store folder of its own, under the
;;;; run's scratch home.

(in-package #:nodecode.test)

(define-test-slice "detox" "DETOX-CELL-" :start nodecode-detox:start-cell)

(defmacro with-detox-home (&body body)
  "BODY over a detox store of its own, one per test, under the run's home."
  `(let ((nlk:*home* (merge-pathnames "detox-tests/" (nlk:home))))
     ,@body))

(defmacro with-detox ((&rest config) &body body)
  "BODY with the cell started on its defaults, or on the CONFIG pairs, and
stopped on unwind, over a store of the test's own."
  `(with-detox-home
     (with-cell-stop ((detox-start ,@config))
       ,@body)))

(defun intercept-text (tool text)
  "TEXT through the started cell's :TOOL hook, as a dispatch of TOOL would."
  (funcall (cell-hook "nodecode-detox" :tool)
           (list :name tool :arguments nil :call-id "call-1")
           (lambda (op) (declare (ignore op)) text)))

(defun report-id (report)
  "The evidence id a compact report names, or NIL."
  (multiple-value-bind (whole groups)
      (cl-ppcre:scan-to-strings "kept as evidence (ev_[0-9a-f]+)" report)
    (declare (ignore whole))
    (and groups (aref groups 0))))

(defun sample-log (&key (lines 400) (needle "the build is green"))
  "A log of LINES lines with an error, a warning and NEEDLE in it."
  (with-output-to-string (out)
    (loop for n from 1 to lines
          do (format out "line ~d: allocating ~d bytes in chunk ~d~%"
                     n (* 16 n) (mod n 7))
             (when (= n 42) (format out "error: cannot find symbol `build'~%"))
             (when (= n 250) (format out "~a~%" needle))
          finally (format out "3 failed, 42 passed, 1 skipped in 1.20s~%"))))

(defun error-log (&key (lines 435))
  "A log of LINES short error lines, which no report can shrink to ten percent."
  (with-output-to-string (out)
    (loop for n from 1 to lines
          do (format out "error: step ~d of the build failed~%" n))))

(defun line-of (text phrase)
  "The 1-based number of the line of TEXT that PHRASE appears on, or NIL."
  (let ((at (position-if (lambda (line) (search phrase line)) (nodecode-detox::text-lines text))))
    (and at (1+ at))))
