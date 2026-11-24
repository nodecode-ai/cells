;;;; cell-test.lisp --- the fold, the shapes it leaves alone, and the seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here has a network or a child process: the hook is funcalled
;;;; with a NEXT of the test's own, so what is proved is the cell's own rule
;;;; -- which text it folds, which it leaves whole, and that a rewrite travels
;;;; the same path an ordinary tool result does.

(in-package #:nodecode.test)

(defun tokenjuice-output (&optional (lines 60) (width 40))
  "LINES distinct lines of WIDTH characters: the noise a build log makes."
  (format nil "~{~a~^~%~}"
          (loop for i from 1 to lines
                collect (format nil "compile ~3,'0d: ~a" i (make-string (- width 16) :initial-element #\.)))))

(defun tokenjuice-verdict (name arguments text)
  "The installed hook's answer for a call NAME with ARGUMENTS whose body
answered TEXT."
  (let ((op (list :name name :arguments arguments :call-id "c")))
    (funcall (cell-hook "nodecode-tokenjuice" :tool) op
             (lambda (op) (declare (ignore op)) text))))

(defun tokenjuice-sh-arguments (&optional (command "cat build.log"))
  (nlk:json-object "command" command))

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "tokenjuice"
  (:hooks :tool)
  (:command "tokenjuice")
  (:refused ("min_lines" 1))
  (:running (is (= 12 (nodecode-tokenjuice::setting :min-lines)) "the declared defaults are what it runs with")))

;;; --- what it folds ---------------------------------------------------------

(deftest tokenjuice-cell-folds-a-long-terminal-output (with-cell-stop ((tokenjuice-start)))
  (let* ((text (tokenjuice-output 60))
         (answer (tokenjuice-verdict "sh" (tokenjuice-sh-arguments) text)))
    (is (stringp answer))
    (is (< (length answer) (length text)) "the model reads fewer characters")
    (is-carrying answer ("... 40 lines omitted ..." "the middle is counted, not hidden")
      ("compile 001:" "the head is kept")
      ("compile 060:" "and the tail"))
    (is (not (search "compile 030:" answer)) "which is what went")))

(deftest tokenjuice-cell-leaves-alone-what-it-should (with-cell-stop ((tokenjuice-start)))
  (let* ((long (tokenjuice-output 60))
         (text (concatenate 'string long (format nil "~%[exit 1]")))
         (failed (tokenjuice-verdict "sh" (tokenjuice-sh-arguments) text))
         (short (tokenjuice-verdict "sh" (tokenjuice-sh-arguments) "one line"))
         (lisp (tokenjuice-verdict "eval" (nlk:json-object "form" "(reduce #'+ (list 1 2 3))") long))
         (other (tokenjuice-verdict "write-file" (nlk:json-object "path" "x") long)))
    (is (equal text failed) "a failure keeps its whole output")
    (is (equal "one line" short) "a short result is not worth a rewrite")
    (is (equal long lisp) "an eval that is not a shell call is untouched")
    (is (equal long other) "and so is a tool of another name")))

(deftest tokenjuice-cell-compacts-only-when-the-fold-pays (with-cell-stop ((tokenjuice-start "min_saved_chars" 100000)))
  (let* ((long (tokenjuice-output 60))
         (answer (tokenjuice-verdict "sh" (tokenjuice-sh-arguments) long)))
    (is (equal long answer) "a fold that saves too little is no rewrite at all")))

(deftest tokenjuice-cell-recounts-the-header-of-a-text-result (with-cell-stop ((tokenjuice-start)))
  (let* ((body (tokenjuice-output 60))
         (text (format nil "[text: 60 lines, ~:d characters]~%~a" (length body) body))
         (answer (tokenjuice-verdict "eval" (nlk:json-object "form" "(sh \"cat build.log\")") text)))
    (is (stringp answer))
    (is (ppcre:scan "^\\[text: 21 lines, " answer) "the text header is recounted, not left lying")))

(deftest tokenjuice-cell-clamps-a-long-line (with-cell-stop ((tokenjuice-start "min_lines" 2 "head_lines" 2 "tail_lines" 2 "min_saved_chars" 0)))
  (let* ((text (format nil "~a~%~a~%~a~%~a~%~a~%~a"
                        (make-string 600 :initial-element #\x) "b" "c" "d" "e" "f"))
         (answer (tokenjuice-verdict "sh" (tokenjuice-sh-arguments) text)))
    (is (stringp answer))
    (is (< (length answer) 1000) "a line past max_line_chars is folded in its middle")
    (is-carrying answer (" ... " "both ends of the line survive")
      ("... 2 lines omitted ..." "and the middle of the list goes"))))

;;; --- the real dispatch seam ------------------------------------------------

(deftest tokenjuice-cell-rewrites-through-the-tool-dispatch (with-cell-stop ((tokenjuice-start)))
  (with-saved-globals (nle::*tools*)
    (let ((long (tokenjuice-output 60)))
      (nle:register-tool "sh"
                         (list :description "a stub shell"
                               :parameters (nlk:json-object
                                            "type" "object"
                                            "properties" (nlk:json-object
                                                          "command" (nlk:json-object "type" "string"))))
                         (lambda (arguments) (declare (ignore arguments)) long))
      (let ((result (execute-wire-call "call-tj-1" "sh" "{\"command\": \"cat build.log\"}")))
        (is-carrying (content (nle::message-content result))
          ("... 40 lines omitted ..." "the model is handed the folded result"))))))

;;; --- the report ------------------------------------------------------------

(deftest tokenjuice-cell-report-counts-what-it-saved (with-cell-stop ((tokenjuice-start)))
  (tokenjuice-verdict "sh" (tokenjuice-sh-arguments) (tokenjuice-output 60))
  (let ((first (nodecode-tokenjuice:report "" "s1"))
        (reset (nodecode-tokenjuice:report "reset" "s1"))
        (after (nodecode-tokenjuice:report "" "s1")))
    (is-carrying first ("compacted 1 of 1 terminal results" "the call it folded")
      ("head/tail 10/10 lines" "and the counts it runs with"))
    (is-carrying reset ("counts reset" "reset zeroes them"))
    (is-carrying after ("compacted 0 of 0" "and the next report starts over"))))

(deftest tokenjuice-cell-report-refuses-when-not-running ()
  (is (signals-error nodecode-tokenjuice::tokenjuice-error (nodecode-tokenjuice:report "" "s1"))))
