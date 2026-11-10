;;;; cell-test.lisp --- the transform, its settings, the command, and the hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The registry is the core's, under this cell's PROGRESS-SQUASH- prefix.
;;;; Every test that needs the advice installed starts the cell and stops it on
;;;; unwind, so nothing is left on SH for the next test. The hook is called
;;;; with a stub NEXT that answers a captured string and a status, which is
;;;; exactly what a shell command would: no command runs.

(in-package #:nodecode.test)

(define-test-slice "progress-squash" "PROGRESS-SQUASH-"
  :start nodecode-progress-squash:start-cell)

(define-cell-lifecycle-tests "progress-squash"
  (:hooks 'nle:sh)
  (:help :progress-squash)
  (:command "progress-squash")
  (:running
   (is (nodecode-progress-squash::setting :footer) "the footer is on by default")
   (is (eql 1 (nodecode-progress-squash::setting :min-frames)) "one overwritten frame is enough to collapse a line"))
  (:refused ("min_frames" 0) ("keep" "epoch:")))

(defun progress-squash-through (output &optional (status 0))
  "OUTPUT through the installed advice on SH, with a stub NEXT answering it:
a captured terminal result and the status the command answered."
  (multiple-value-list
   (funcall (cell-hook "nodecode-progress-squash" 'nle:sh)
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (values output status)))))

(deftest progress-squash-collapses-a-bar (with-cell-stop ((progress-squash-start)))
  (let ((answer (progress-squash-through
                 (format nil "epoch: 1/3~cepoch: 2/3~cepoch: 3/3 done" #\Return #\Return))))
    (is (equal 0 (second answer)) "the status the command answered is untouched")
    (is (equal (format nil "epoch: 3/3 done~c[progress-squash: 2 overwritten progress frames removed]"
                       #\Newline)
               (first answer)) "the frames a terminal overwrote are gone, the footer says how many")))

(deftest progress-squash-leaves-plain-output-alone (with-cell-stop ((progress-squash-start)))
  (let* ((text "remote: Total 3 (delta 1), reused 0 (delta 0)")
         (answer (progress-squash-through text)))
    (is (equal text (first answer)) "no carriage return, not one byte changed")))

(deftest progress-squash-keeps-a-different-line (with-cell-stop ((progress-squash-start)))
  (let ((answer (progress-squash-through
                 (format nil "error: boom~cReceiving objects: 1%~cReceiving objects: 100% done"
                         #\Return #\Return))))
    (is (equal (format nil "error: boom~cReceiving objects: 100% done~c[progress-squash: 1 overwritten progress frame removed]"
                       #\Newline #\Newline)
               (first answer)) "a segment with another stem keeps a line of its own")))

(deftest progress-squash-keeps-line-endings (with-cell-stop ((progress-squash-start)))
  (let* ((text (format nil "a~c~cb~c~c" #\Return #\Newline #\Return #\Newline))
         (answer (progress-squash-through text)))
    (is (equal text (first answer)) "a carriage return that ends a line is an ending, not a redraw")))

(deftest progress-squash-reads-through-erase-codes (with-cell-stop ((progress-squash-start)))
  (let ((answer (progress-squash-through
                 (format nil "x 1%~c[K~cx 100%~c[K" #\Escape #\Return #\Escape))))
    (is (equal (format nil "x 100%~c[K~c[progress-squash: 1 overwritten progress frame removed]"
                       #\Escape #\Newline)
               (first answer)) "the erase-line codes do not stop two frames reading as one line")))

(deftest progress-squash-honours-its-settings (with-cell-stop ((progress-squash-start "footer" nil)))
  (let ((answer (progress-squash-through (format nil "a 1~ca 2" #\Return))))
    (is (equal "a 2" (first answer)) "no footer when it is off")))

(deftest progress-squash-keeps-named-stems (with-cell-stop ((progress-squash-start "keep" (vector "epoch:"))))
  (let* ((text (format nil "epoch: 1~cepoch: 2" #\Return))
         (answer (progress-squash-through text)))
    (is (equal text (first answer)) "a kept stem is left as it was")))

(deftest progress-squash-min-frames-gates-a-redraw (with-cell-stop ((progress-squash-start "min_frames" 2)))
  (let* ((text (format nil "a 1~ca 2" #\Return))
         (answer (progress-squash-through text)))
    (is (equal text (first answer)) "one overwrite is under min_frames 2"))
  (let ((answer (progress-squash-through (format nil "a 1~ca 2~ca 3" #\Return #\Return))))
    (is (equal (format nil "a 3~c[progress-squash: 2 overwritten progress frames removed]" #\Newline)
               (first answer)) "two are enough")))

(deftest progress-squash-command-reports-and-resets (with-cell-stop ((progress-squash-start)))
  (progress-squash-through (format nil "a 1~ca 2~ca 3" #\Return #\Return))
  (let ((report (cell-entry "nodecode-progress-squash" "progress-squash" "")))
    (is (search "1 rewritten" report) "the report counts the rewrite")
    (is (search "2 progress frames dropped" report) "and the frames it dropped"))
  (is (equal "progress-squash: counters cleared"
             (cell-entry "nodecode-progress-squash" "progress-squash" "reset")) "reset answers in words")
  (is (search "0 rewritten" (cell-entry "nodecode-progress-squash" "progress-squash" "")) "and the counters start over"))
