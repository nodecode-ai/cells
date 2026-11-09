;;;; cell-test.lisp --- the degeneracy guard, offline.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Nothing here reaches a model or a network: the detector is pure, the
;;;; questions are a stub, and the incident files land in the run's own
;;;; scratch home. What is proved is the cell's own material: which payloads
;;;; earn a question, what the guard does with an answer, that an ordinary
;;;; call runs untouched, and that the tool and the command answer.

(in-package #:nodecode.test)

(define-test-slice "degeneracy-guard" "DEGENERACY-GUARD-CELL-"
  :start nodecode-degeneracy-guard:start-cell)

;;; --- the lifecycle every cell owes --------------------------------------------

(define-cell-lifecycle-tests "degeneracy-guard"
  (:config "mode" "ask")
  (:hooks :tool :round)
  (:help :degeneracy-guard)
  (:command "degeneracy")
  (:refused ("ask_seconds" "often")))

;;; --- fixtures -------------------------------------------------------------------

(defparameter +looped+
  "The build is still running and I will check it again."
  "One sentence, whose repetition is a loop and reads as prose.")

(defun loop-text (&optional (lines 20))
  "The same sentence LINES times."
  (format nil "~{~a~^~%~}" (loop repeat lines collect +looped+)))

(defun counting-text (&optional (lines 20))
  "LINES sentences, each one different: repetition's nearest false positive."
  (format nil "~{~a~^~%~}" (loop for n from 1 to lines collect (format nil "Step ~d is done." n))))

(defun table-text (&optional (rows 20))
  "A markdown table of identical zero rows: repetition with a data shape."
  (format nil "| a | b | c |~%~{~a~^~%~}" (loop repeat rows collect "| 0 | 0 | 0 |")))

(defun csv-text (&optional (rows 20))
  "Delimited records whose rows differ: a table wearing a comma."
  (format nil "~{~a~^~%~}"
          (loop for n from 1 to rows collect (format nil "~d,~d,ok,the-row-is-fine" n n))))

(defun dg-call (arguments &optional (tool "eval"))
  "The installed :tool hook's answer for a call to TOOL with ARGUMENTS, NEXT
answering :RAN when the call is let through."
  (funcall (cell-hook "nodecode-degeneracy-guard" :tool)
           (list :name tool :arguments arguments :call-id "c")
           (lambda (op) (declare (ignore op)) :ran)))

(defun dg-arguments (&optional (text "(+ 1 1)"))
  "A tool's arguments carrying TEXT under \"form\", the shape eval's are."
  (nlk:json-object "form" text))

(defun call-with-ask-operator (stub thunk)
  "THUNK with the core's ASK-OPERATOR answered by STUB, put back after."
  (let* ((symbol (intern "ASK-OPERATOR" '#:nodecode.evolved))
         (saved (and (fboundp symbol) (fdefinition symbol))))
    (setf (fdefinition symbol) stub)
    (unwind-protect (funcall thunk)
      (if saved (setf (fdefinition symbol) saved) (fmakunbound symbol)))))

;;; --- the judgements -------------------------------------------------------------

(deftest degeneracy-guard-cell-output-loop-is-caught ()
  (let ((verdict (nodecode-degeneracy-guard:check-text (loop-text 20))))
    (is (getf verdict :triggered) "twenty identical sentences are a loop")
    (is (equal "identical_run" (getf verdict :reason)) "and say which criterion"))
  (let ((verdict (nodecode-degeneracy-guard:check-text (counting-text 20))))
    (is (not (getf verdict :triggered)) "twenty lines that each differ are not a loop")))

(deftest degeneracy-guard-cell-argument-rules ()
  (let* ((verdict (nodecode-degeneracy-guard:judge-argument (loop-text 20)))
         (metrics (getf verdict :metrics)))
    (is (getf verdict :ask) "a repeated prose payload earns a question")
    (is (equal "none" (getf metrics :structure)) "and has no data shape"))
  (let* ((verdict (nodecode-degeneracy-guard:judge-argument (table-text 20)))
         (metrics (getf verdict :metrics)))
    (is (not (getf verdict :ask)) "a table of identical rows is data, not a loop")
    (is (equal "markdown_table" (getf metrics :structure)) "and is recognized as one"))
  (let* ((verdict (nodecode-degeneracy-guard:judge-argument (csv-text 20)))
         (metrics (getf verdict :metrics)))
    (is (equal "delimited" (getf metrics :structure)) "differing CSV rows are delimited records")
    (is (not (getf verdict :ask)) "and are left alone"))
  (let ((verdict (nodecode-degeneracy-guard:judge-argument "the same line the same line")))
    (is (not (getf verdict :ask)) "a short payload cannot carry a loop far enough to matter")))

;;; --- what the guard does with a call ---------------------------------------------

(deftest degeneracy-guard-cell-a-loop-shaped-call-is-refused-with-nobody-there ()
  (with-cell-stop ((degeneracy-guard-start "mode" "ask"))
    (let ((answer (dg-call (dg-arguments (loop-text 20))))
          (files (uiop:directory-files (nodecode-degeneracy-guard::incident-directory))))
      (is (stringp answer) "the call does not run")
      (is (search "refused by nodecode-degeneracy-guard" answer) "the model is told who refused it")
      (is (search "repetition loop" answer) "and what the guard saw")
      (is (plusp (length files)) "and the incident is on disk"))))

(deftest degeneracy-guard-cell-an-ordinary-call-runs ()
  (with-cell-stop ((degeneracy-guard-start "mode" "ask"))
    (is (eq :ran (dg-call (dg-arguments))) "an ordinary call is let through")
    (is (eq :ran (dg-call (dg-arguments (table-text 20)))) "and so is a call carrying a table")
    (let ((answer (dg-call (dg-arguments (loop-text 20)) "a-tool")))
      (is (search "refused" answer) "every tool is judged, not one by name"))))

(deftest degeneracy-guard-cell-an-exempt-tool-is-never-judged ()
  (with-cell-stop ((degeneracy-guard-start "mode" "ask" "allow_tools" (vector "eval")))
    (is (eq :ran (dg-call (dg-arguments (loop-text 20)))) "an exempt tool passes a loop-shaped call")
    (let ((answer (dg-call (dg-arguments (loop-text 20)) "other")))
      (is (search "refused" answer) "and another tool is still judged"))))

(deftest degeneracy-guard-cell-allow-once-runs-and-deny-refuses ()
  (call-with-ask-operator
   (lambda (title &rest keys)
     (declare (ignore keys))
     (is (search "repetition loop" title) "the question says what it saw")
     (values "Allow once" :answered))
   (lambda ()
     (with-cell-stop ((degeneracy-guard-start "mode" "ask"))
       (is (eq :ran (dg-call (dg-arguments (loop-text 20)))) "Allow once runs the call"))))
  (call-with-ask-operator
   (lambda (&rest ignored)
     (declare (ignore ignored))
     (values "Deny" :answered))
   (lambda ()
     (with-cell-stop ((degeneracy-guard-start "mode" "ask"))
       (let ((answer (dg-call (dg-arguments (loop-text 20)))))
         (is (search "chose Deny" answer) "Deny refuses it and says so"))))))

(deftest degeneracy-guard-cell-refuse-and-warn-modes ()
  (with-cell-stop ((degeneracy-guard-start "mode" "refuse"))
    (let ((answer (dg-call (dg-arguments (loop-text 20)))))
      (is (search "refused before it ran" answer) "refuse mode never asks")))
  (with-cell-stop ((degeneracy-guard-start "mode" "warn"))
    (is (eq :ran (dg-call (dg-arguments (loop-text 20)))) "warn mode lets it run")))

;;; --- the tool and the command ----------------------------------------------------

(defun dg-tool (call-id &rest pairs)
  "The degeneracy_check tool's answer to a call whose members are PAIRS."
  (let ((arguments (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key arguments) value))
    (nle::message-content
     (execute-wire-call call-id "degeneracy_check" (nlk:encode-json-object arguments)))))

(deftest degeneracy-guard-cell-the-check-tool-answers ()
  (with-cell-stop ((degeneracy-guard-start))
    (let ((answer (dg-tool "dg-1" "text" (loop-text 20))))
      (is (stringp answer) "the tool answers")
      (is (search "LOOP" answer) "a loop is named as one")
      (is (search "identical_run" answer) "with the criterion that caught it"))
    (is (search "CLEAN" (dg-tool "dg-2" "text" "one short sentence" "kind" "output")) "a clean text says so")
    (is (search "text" (dg-tool "dg-3")) "a call with no text is refused by the contract")))

(deftest degeneracy-guard-cell-the-command-answers ()
  (with-cell-stop ((degeneracy-guard-start "mode" "warn"))
    (is (search "mode warn" (nodecode-degeneracy-guard::run-slash "" "s1")) "the state names the mode")
    (let ((answer (nodecode-degeneracy-guard::run-slash (format nil "check ~a" (loop-text 20)) "s1")))
      (is (search "LOOP" answer) "a text is judged as given"))
    (let ((answer (nodecode-degeneracy-guard::run-slash "check (plus 1 1)" "s1")))
      (is (search "CLEAN" answer) "and a short one passes"))
    (is (search "no incidents yet" (nodecode-degeneracy-guard::run-slash "last" "s1")) "an empty log says so")
    (is (search "not one of its verbs" (nodecode-degeneracy-guard::run-slash "x" "s1")) "an unknown verb is named")))

;;; --- the long-session notice --------------------------------------------------------

(deftest degeneracy-guard-cell-session-notice-is-debounced ()
  (let ((notice (nodecode-degeneracy-guard:session-notice 500 0)))
    (is (stringp notice) "the notice comes at the threshold")
    (is (search "500 model requests" notice) "saying how long the session is")
    (is (search "new session" notice) "and what to do about it"))
  (is (null (nodecode-degeneracy-guard:session-notice 499 0)) "not before it")
  (is (null (nodecode-degeneracy-guard:session-notice 520 500)) "not again at once")
  (is (stringp (nodecode-degeneracy-guard:session-notice 550 500)) "but after another fifty rounds")
  (is (null (nodecode-degeneracy-guard:session-notice 4000 0 :max-rounds 0)) "and never when it is off"))
