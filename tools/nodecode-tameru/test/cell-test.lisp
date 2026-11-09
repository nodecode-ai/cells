;;;; cell-test.lisp --- the reduction, the store behind tameru_expand, and /tameru.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here has a network, a model call or a child process: the hook is
;;;; funcalled with a NEXT of the test's own, so what is proved is the cell's
;;;; own rule -- which result it shortens, what it keeps, that the exact
;;;; original comes back through the registered tool, and that a section that
;;;; does not fit refuses.

(in-package #:nodecode.test)

;;; --- the fixtures ---------------------------------------------------------

(defun tameru-log (&optional (lines 200))
  "LINES lines of build noise, with one error in the middle and one warning
repeated: a log the model should not pay for whole."
  (format nil "~{~a~^~%~}"
          (loop for i from 1 to lines
                collect (cond ((= i 100) "ERROR: boom: the parser gave up at token 42 (code E1234)")
                              ((> i 180) (format nil "src/parse.lisp:~d: warning: unused variable" i))
                              (t (format nil "compiling unit ~3,'0d: ~a" i
                                         (make-string 40 :initial-element #\.)))))))

(defun tameru-answer (name text)
  "The installed hook's answer for a call to tool NAME whose body answered
TEXT -- the path an ordinary tool result travels."
  (let ((op (list :name name :arguments (nlk:json-object) :call-id "c")))
    (funcall (cell-hook "nodecode-tameru" :tool) op
             (lambda (op) (declare (ignore op)) text))))

(defun tameru-ref (text)
  "The ref a shortened result's header names, or NIL."
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings "ref=([0-9a-f]{16})" text)
    (declare (ignore match))
    (and groups (aref groups 0))))

(defun tameru-call (name &rest pairs)
  "Call the registered tool NAME through its declared contract with PAIRS, as
a tool call does."
  (let* ((entry (nle::find-tool name))
         (schema (second entry)))
    (funcall (third entry) (nlk:contract-decode schema (apply #'nlk:make-json-object pairs)))))

;;; --- lifecycle --------------------------------------------------------------

(define-cell-lifecycle-tests "tameru"
  (:hooks :tool)
  (:help :tameru)
  (:command "tameru")
  (:refused ("min_chars" 1))
  (:running (is (nle::find-tool "tameru_expand") "the expand tool is registered"))
  (:stopped (is (null (nle::find-tool "tameru_expand")) "stop takes the tool back")))

;;; --- what it shortens ---------------------------------------------------------

(deftest tameru-cell-shortens-a-big-result-and-keeps-its-errors (with-cell-stop ((tameru-start)))
  (let* ((text (tameru-log))
         (answer (tameru-answer "sh" text)))
    (is (stringp answer))
    (is (< (length answer) (length text)) "the model reads fewer characters")
    (is-carrying answer ("[tameru: kept" "the header counts what was kept")
      ("ref=" "and names the ref tameru_expand takes")
      ("ERROR: boom" "the one error line survives, though it sat in the middle")
      ("compiling unit 001" "the head is kept")
      ("src/parse.lisp:200:" "and so is the tail"))
    (is (not (search "compiling unit 090" answer)) "which is what the marker line stands for")
    (is (search "warning: unused variable" answer) "a distinct warning is kept once")))

(deftest tameru-cell-collapses-a-repeating-template-into-one-exemplar (with-cell-stop ((tameru-start "min_chars" 200)))
  (let* ((text (format nil "~{~a~^~%~}"
                        (append (loop for i from 1 to 10 collect (format nil "step ~2,'0d: warm" i))
                                (loop for i from 1 to 120
                                      collect (format nil "reading /var/db/item-~d.json" i))
                                (loop for i from 1 to 10 collect (format nil "step ~2,'0d: cool" i)))))
         (answer (tameru-answer "sh" text)))
    (is (stringp answer))
    (is (search "[×120] reading" answer) "one exemplar stands for the hundred and twenty")
    (is (not (search "item-37.json" answer)) "and the copies do not ride along")))

(deftest tameru-cell-leaves-alone-what-it-should (with-cell-stop ((tameru-start)))
  (let* ((big (tameru-log))
         (short "one line")
         (self (tameru-answer "tameru_expand" big))
         (smallest (tameru-answer "sh" (make-string 300 :initial-element #\x))))
    (is (equal "one line" (tameru-answer "sh" short)) "a short result is not worth a rewrite")
    (is (equal big self) "tameru_expand's own answer is never shortened")
    (is (equal smallest (tameru-answer "sh" smallest)) "nor is a result under min_chars")))

(deftest tameru-cell-keeps-a-credential-carrying-result-but-does-not-store-it (with-cell-stop ((tameru-start)))
  (let* ((text (format nil "~a~%api_key = abcd1234efgh5678~%~a"
                        (format nil "~{~a~^~%~}"
                                (loop for i from 1 to 400 collect (format nil "fetching remote object ~3,'0d of the archive" i)))
                        (format nil "~{~a~^~%~}" (loop for i from 1 to 20 collect "done"))))
         (answer (tameru-answer "sh" text)))
    (is (stringp answer))
    (is (search "credential-looking text" answer) "the header says the original was not kept")
    (is (null (tameru-ref answer)) "so no ref is offered")))

;;; --- the original comes back ---------------------------------------------------

(deftest tameru-cell-expand-answers-the-exact-original (with-cell-stop ((tameru-start)))
  (let* ((text (tameru-log))
         (answer (tameru-answer "sh" text))
         (ref (tameru-ref answer)))
    (is (stringp ref) "the header carries a 16-hex ref")
    (let ((original (tameru-call "tameru_expand" "ref" ref)))
      (is (search "compiling unit 090" original) "a line the shortening dropped comes back")
      (is (search "ERROR: boom" original))
      (is-carrying original ("tameru_expand ref=" "the answer names its ref")
        ("of 200" "and how many lines the original has")))))

(deftest tameru-cell-expand-answers-a-line-range-and-a-grep (with-cell-stop ((tameru-start)))
  (let* ((text (tameru-log))
         (ref (tameru-ref (tameru-answer "sh" text)))
         (range (tameru-call "tameru_expand" "ref" ref "start_line" 3 "end_line" 5))
         (grepped (tameru-call "tameru_expand" "ref" ref "grep" "boom")))
    (is-carrying range ("lines 3-5 of 200" "the range is what was asked for")
      ("compiling unit 003" "its first line") ("compiling unit 005" "and its last"))
    (is (not (search "compiling unit 006" range)) "nothing past the range")
    (is-carrying grepped ("match" "the grep says how many lines matched")
      ("ERROR: boom" "and answers the matching line"))
    (is (not (search "compiling unit 003" grepped)) "and only that line")))

(deftest tameru-cell-expand-says-when-a-ref-is-unknown (with-cell-stop ((tameru-start)))
  (let ((unknown (tameru-call "tameru_expand" "ref" "00000000deadbeef"))
        (bad (tameru-call "tameru_expand" "ref" "not-a-ref")))
    (is-carrying unknown ("not found" "a ref this process never kept is said so"))
    (is-carrying bad ("not found" "and so is something that is no ref at all"))))

;;; --- the operator's side ---------------------------------------------------------

(deftest tameru-cell-command-reports-and-expands (with-cell-stop ((tameru-start)))
  (let* ((text (tameru-log))
         (ref (tameru-ref (tameru-answer "sh" text)))
         (report (nodecode-tameru:report "" "s1"))
         (expanded (nodecode-tameru:report (format nil "expand ~a" ref) "s1"))
         (reset (nodecode-tameru:report "reset" "s1"))
         (after (nodecode-tameru:report "" "s1")))
    (is-carrying report ("shortened 1 of 1" "the report counts the one result it shortened")
      ("ref=" "and lists the original it holds"))
    (is-carrying expanded ("compiling unit 090" "expand prints the original here too"))
    (is-carrying reset ("cleared" "reset clears the store"))
    (is-carrying after ("shortened 0 of 0" "and the next report starts over"))))
