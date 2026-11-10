;;;; cell-test.lisp --- lifecycle, the record, mining, compiling, running.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the cell installs its :TOOL hook, its tool, /kata
;;;; and its manual topic and takes every one of them back on stop; a call
;;;; that runs through the hook is recorded with a credential's value masked;
;;;; a window repeated in three sessions is mined once, whole, with the
;;;; argument that varied as a parameter; compiling keeps it in the cell's own
;;;; file and starting again reads it back; a run renders each step's
;;;; arguments and dispatches them in order through the stub, and refuses
;;;; while a parameter is missing. Every dispatch is a stub: nothing here
;;;; calls a real tool, a network or a process.

(in-package #:nodecode.test)

(define-test-slice "kata" "KATA-CELL-" :start nodecode-kata:start-cell)

(define-cell-lifecycle-tests "kata"
  (:config "min_support" 2)
  (:hooks :tool)
  (:help :kata)
  (:command "kata")
  (:running (is (= 2 (nodecode-kata::setting :min-support)) "min_support is read")
            (is (eq t (nodecode-kata::setting :record)) "the record is on by default"))
  (:refused ("min_support" 1)))

;;; --- helpers -------------------------------------------------------------------

(defmacro with-kata ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS, its routines file in the scratch home."
  `(with-temp-store ()
     (let ((nodecode-kata::*routines-file*
             (namestring (nlk:home "kata-test-routines.json"))))
       (with-cell-stop ((kata-start ,@pairs))
         ,@body))))

(defun kata-fire (tool &rest pairs)
  "One dispatch of TOOL with PAIRS, as a turn runs it: through the installed
:TOOL hook, whose NEXT answers TOOL-DONE."
  (funcall (cell-hook "nodecode-kata" :tool)
           (list :name tool :arguments (apply #'nlk:make-json-object pairs) :call-id "call-1")
           (lambda (op) (declare (ignore op)) "TOOL-DONE")))

(defun kata-seed (&rest sessions)
  "One read and one edit per session, each session a path of its own."
  (dolist (session sessions)
    (let ((nlk:*scribe-session-id* session)
          (path (format nil "/work/~a.txt" session)))
      (kata-fire "read_file" "path" path)
      (kata-fire "edit_file" "path" path "old" "alpha" "new" "beta"))))

(defun kata-values (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key table) value))
    table))

(defun kata-call-tool (&rest pairs)
  "PAIRS through the registered tool, its contract's boundary and its body."
  (let ((entry (nle::find-tool "kata")))
    (funcall (third entry)
             (nle::tool-body-arguments entry (apply #'nlk:make-json-object pairs)))))

(defvar *kata-steps* '()
  "The (TOOL . PATH) a stubbed run dispatched, newest first.")

(defun kata-stub ()
  "The step stub: records (TOOL . PATH) onto *KATA-STEPS* and answers STEP-OK."
  (lambda (tool arguments)
    (push (cons tool (gethash "path" arguments)) *kata-steps*)
    "STEP-OK"))

;;; --- the record ------------------------------------------------------------------

(deftest kata-cell-records-a-call-masked (with-kata ())
  (let ((nlk:*scribe-session-id* "s1"))
    (kata-fire "read_file" "path" "/a.txt"
               "api_key" "sk-live-0123456789abcdef0123456789abcdef"))
  (let ((call (first nodecode-kata::*calls*)))
    (is (equal "read_file" (getf call :tool)) "the tool is kept")
    (is (equal "s1" (getf call :session)) "under the session that ran it")
    (is (equal "[set]" (cdr (assoc "api_key" (getf call :arguments) :test #'string=))) "a credential's value is masked, the argument kept")
    (is (equal "/a.txt" (cdr (assoc "path" (getf call :arguments) :test #'string=))) "and an ordinary one is kept")
    (is (search "read_file(" (kata:history)) "history lists it")
    (is (not (search "sk-live" (kata:history))) "and its value is nowhere in history")))

(deftest kata-cell-skips-its-own-tool (with-kata ())
  (kata-fire "kata" "action" "list")
  (is (null nodecode-kata::*calls*) "the cell's own tool is never a step"))

;;; --- mining ----------------------------------------------------------------------

(deftest kata-cell-mines-one-whole-routine (with-kata ())
  (kata-seed "s1" "s2" "s3")
  (let ((candidates (kata:mined)))
    (is (= 1 (length candidates)) "one window repeats, and its fragments are dropped")
    (is (equal '("read_file" "edit_file") (getf (first candidates) :tools)) "the whole routine, in order")
    (is (= 3 (getf (first candidates) :support)) "seen in three sessions")
    (is (equal '("path") (getf (first candidates) :params)) "the argument that varied is a parameter")
    (is (= 8 (length (getf (first candidates) :id))) "the id is the 8 hex of its signature")
    (is (search "read_file" (kata:show (getf (first candidates) :id))) "show renders one step per tool")))

(deftest kata-cell-needs-a-second-session (with-kata ("min_support" 2))
  (kata-seed "s1")
  (is (null (kata:mined)) "one session is not yet a routine")
  (kata-seed "s2")
  (is (= 1 (length (kata:mined))) "a second session is"))

;;; --- compiling and running ---------------------------------------------------------

(deftest kata-cell-compiles-and-runs-a-routine (with-kata ())
  (kata-seed "s1" "s2" "s3")
  (let* ((candidate (first (kata:mined)))
         (name (getf candidate :name)))
    (is (search "kept" (kata:compile-routine (getf candidate :id))) "compile answers what it kept")
    (is (= 1 (length (kata:routines))) "the routine is compiled")
    (let ((*kata-steps* '())
          (nodecode-kata::*dispatch* (kata-stub)))
      (let ((text (kata:run-routine name (kata-values "path" "/now.txt"))))
        (is (search "STEP-OK" text) "every step answers through the stub")
        (is (equal '("read_file" "edit_file") (mapcar #'car (reverse *kata-steps*))) "the steps ran in the routine's order")
        (is (equal '("/now.txt" "/now.txt") (mapcar #'cdr *kata-steps*)) "the parameter rendered into each")))
    (is (equal (list name) (mapcar (lambda (routine) (getf routine :name)) (kata:routines))) "and it is the one listed")
    (is (search "forgot" (kata:forget-routine name)) "forget takes it back")
    (is (null (kata:routines)) "leaving none")))

(deftest kata-cell-run-refuses-without-a-parameter (with-kata ())
  (kata-seed "s1" "s2" "s3")
  (let* ((candidate (first (kata:mined)))
         (name (getf candidate :name)))
    (kata:compile-routine (getf candidate :id))
    (is (search "needs path" (refusal-text kata:kata-error (kata:run-routine name (kata-values)))) "a run with no value for a parameter refuses, naming it")
    (is (search "needs path" (refusal-text kata:kata-error (kata:run-routine name))) "and so does a run with no values at all")))

(deftest kata-cell-tool-answers-through-its-contract (with-kata ())
  (kata-seed "s1" "s2" "s3")
  (let* ((candidate (first (kata:mined)))
         (id (getf candidate :id))
         (name (getf candidate :name)))
    (is (search id (kata-call-tool "action" "list")) "list names the candidate id")
    (is (search "read_file" (kata-call-tool "action" "show" "routine" id)) "show renders its steps")
    (is (search "kept" (kata-call-tool "action" "compile" "routine" id)) "compile keeps it")
    (let ((*kata-steps* '())
          (nodecode-kata::*dispatch* (kata-stub)))
      (is (search "STEP-OK" (kata-call-tool "action" "run" "routine" name
                                             "params" (nlk:make-json-object "path" "/now.txt"))) "run dispatches with the parameters the call carried")
      (is (equal '("/now.txt" "/now.txt") (mapcar #'cdr *kata-steps*)) "both steps got the value the tool call carried"))
    (is (search "forgot" (kata-call-tool "action" "forget" "routine" name)) "forget takes it back")
    (is (search "kata: show needs" (kata-call-tool "action" "show")) "show without a routine says so")))

(deftest kata-cell-contract-refuses-a-call-without-an-action (with-kata ())
  (is (signals-error nlk:contract-refusal (kata:decode-kata-call (nlk:make-json-object))) "action is required by the declaration")
  (is (signals-error nlk:contract-refusal
        (kata:decode-kata-call (nlk:make-json-object "action" "nope"))) "and an action outside the options is refused"))

;;; --- the file, and the command -------------------------------------------------------

(deftest kata-cell-reads-its-routines-back (with-temp-store ()
  (let ((nodecode-kata::*routines-file* (namestring (nlk:home "kata-test-routines.json"))))
    (let ((stop (kata-start)))
      (kata-seed "s1" "s2" "s3")
      (let ((name (getf (first (kata:mined)) :name)))
        (kata:compile-routine (getf (first (kata:mined)) :id))
        (funcall stop)
        (let ((stop2 (kata-start)))
          (unwind-protect
               (progn
                 (is (= 1 (length (kata:routines))) "a start reads the kept routines")
                 (is (equal name (getf (first (kata:routines)) :name)) "under the name they were kept with"))
            (funcall stop2))))))))

(deftest kata-cell-command-answers (with-kata ())
  (is (search "no routine repeats yet" (cell-entry "nodecode-kata" "kata" "")) "an empty /kata lists what there is")
  (kata-seed "s1" "s2" "s3")
  (is (search "read_file" (cell-entry "nodecode-kata" "kata" "list")) "/kata list shows the candidate")
  (let* ((candidate (first (kata:mined)))
         (name (getf candidate :name)))
    (is (search "kept" (cell-entry "nodecode-kata" "kata"
                                   (format nil "compile ~a" (getf candidate :id)))) "and compiles one by id")
    (let ((*kata-steps* '())
          (nodecode-kata::*dispatch* (kata-stub)))
      (is (search "STEP-OK" (cell-entry "nodecode-kata" "kata" (format nil "run ~a path=/now.txt" name))) "and runs it with path= from the line")
      (is (equal '("/now.txt" "/now.txt") (mapcar #'cdr *kata-steps*)) "the value on the line reached both steps")))
  (is (search "empty" (cell-entry "nodecode-kata" "kata" "clear")) "clear empties the record"))
