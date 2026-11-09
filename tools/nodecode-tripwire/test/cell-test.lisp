;;;; cell-test.lisp --- the record, the drill, the digest, the command, the tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section installs one
;;;; hook, one command, one manual topic and one tool, and stop takes every
;;;; one of them back; a refused call is one row and the call still answers
;;;; the model; the drill stops exactly its one tool; a call that ran is not
;;;; a row, nor is an ignored tool's, and a plain failure is a row only when
;;;; the section asks for it; the command digests what is new once and a
;;;; peek leaves it; and the tool answers through the wire, its one declared
;;;; member required. No network, no provider: every row is fed through the
;;;; installed hook with a log of the test's own.

(in-package #:nodecode.test)

(define-test-slice "tripwire" "TRIPWIRE-CELL-" :start nodecode-tripwire:start-cell)

;;; --- lifecycle ------------------------------------------------------------

(define-cell-lifecycle-tests "tripwire"
  (:hooks :tool)
  (:command "tripwire")
  (:help :tripwire)
  (:running (is (nle::find-tool "tripwire_digest") "the digest tool is registered")
            (is (equal "" (nodecode-tripwire::setting :drill-tool)) "the cell only watches")
            (is (search "events.jsonl" (namestring (nodecode-tripwire::events-path))) "the log is under the home"))
  (:stopped (is (null (nle::find-tool "tripwire_digest")) "stop unregisters the tool"))
  ;; A section of the wrong shape refuses at start: a string where the
  ;; declaration says array, and a number where it says string.
  (:refused ("ignore" "eval")
            ("drill_tool" 42)))

;;; --- helpers --------------------------------------------------------------

(defmacro with-tripwire ((&key drill ignore record-errors) &body body)
  "Run BODY with the cell started on a log of its own in a fresh folder, and
the session it reads stubbed to S1."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir "tripwire")
       (with-cell-stop ((tripwire-start
                         "events_file" (namestring (merge-pathnames "events.jsonl" ,dir))
                         ,@(when drill `("drill_tool" ,drill))
                         ,@(when ignore `("ignore" (vector ,@ignore)))
                         ,@(when record-errors `("record_errors" t))))
         (let ((nodecode-tripwire::*session* (lambda () "s1")))
           ,@body)))))

(defun tripwire-hook (tool text)
  "Feed the installed :TOOL hook one call of TOOL whose result is TEXT, as a
turn's dispatch does, and answer what the call answered."
  (funcall (cell-hook "nodecode-tripwire" :tool)
           (list :name tool :arguments (nlk:json-object) :call-id "c1")
           (lambda (op) (declare (ignore op)) text)))

(defun tripwire-rows ()
  "The rows the log of the running cell holds, oldest first."
  (nodecode-tripwire::digest-rows))

;;; --- how a result is read -------------------------------------------------

(deftest tripwire-cell-reads-how-a-call-was-stopped ()
  (is (equal "hook_refused" (nodecode-tripwire:refusal-how "ERROR: refused by nodecode-guard: rule rm-critical matched a critical path")) "a cell that refused")
  (is (equal "approval_denied" (nodecode-tripwire:refusal-how "the operator was asked and chose Deny; the call did not run")) "an approval denied")
  (is (equal "approval_timeout" (nodecode-tripwire:refusal-how "the operator was asked and did not answer in time")) "an approval nobody answered")
  (is (null (nodecode-tripwire:refusal-how "the build is green")) "a call that ran")
  (is (null (nodecode-tripwire:refusal-how nil)) "and a result with no text at all")
  (let ((event (nodecode-tripwire:refusal-event "eval" (make-string 500 :initial-element #\a) "s1" "hook_refused" 100)))
    (is (equal 160 (length (getf event :why))) "the reason is cut to its first line, 160 characters")
    (is (equal 100 (getf event :ts)) "the row carries the time it was made")
    (is (equal "eval" (getf event :tool)))))

(deftest tripwire-cell-a-digest-spells-out-the-new-and-counts-the-rest ()
  (let* ((rows (loop for i from 1 to 10
                      collect (nodecode-tripwire:refusal-event
                               "eval" (format nil "ERROR: refused by nodecode-guard: rule ~d matched" i)
                               "s1" "hook_refused" 3800000000)))
         (text (nodecode-tripwire:digest-body rows 10)))
    (is (search "TRIPWIRE: 10 refused action(s) since the last check" text) text)
    (is (search "(+2 older; by session: s1 10)" text) text)
    (is (search "2020-06-0" text) "a row carries its time")
    (is (search "each of these was stopped" (string-downcase text)) text))
  (is (search "nothing refused since the last check (0 in the log)"
              (nodecode-tripwire:digest-body nil 0)) "an empty digest says so")
  (is (search "holds no refusals yet" (nodecode-tripwire:digest-body nil 0 :peek t)) "and a peek at an empty log"))

;;; --- one refused call, one row --------------------------------------------

(deftest tripwire-cell-a-refused-call-is-one-row-and-still-answers ()
  (with-tripwire ()
    (let ((answer (tripwire-hook "eval" "ERROR: refused by nodecode-guard: rule rm-critical matched a critical path")))
      (is (equal "ERROR: refused by nodecode-guard: rule rm-critical matched a critical path" answer) "the call answers what it would have answered"))
    (let ((rows (tripwire-rows)))
      (is (= 1 (length rows)) "one refused call is one row")
      (is (equal "eval" (getf (first rows) :tool)))
      (is (equal "hook_refused" (getf (first rows) :how)))
      (is (equal "s1" (getf (first rows) :session)))
      (is (search "rm-critical" (getf (first rows) :why))))
    (tripwire-hook "eval" "the build is green")
    (is (= 1 (length (tripwire-rows))) "a call that ran is not a row")))

(deftest tripwire-cell-a-failed-call-is-a-row-only-when-asked ()
  (with-tripwire ()
    (tripwire-hook "eval" "ERROR: sb-int:simple-file-error: no such file")
    (is (zerop (length (tripwire-rows))) "an ordinary failure is not a trip by default"))
  (with-tripwire (:record-errors t)
    (tripwire-hook "eval" "ERROR: sb-int:simple-file-error: no such file")
    (is (equal "error" (getf (first (tripwire-rows)) :how)) "with record_errors it is")))

(deftest tripwire-cell-an-ignored-tool-is-never-a-trip ()
  (with-tripwire (:ignore ("eval"))
    (tripwire-hook "eval" "ERROR: refused by nodecode-guard: rule rm-critical matched")
    (is (zerop (length (tripwire-rows))) "the ignore list leaves that tool alone")
    (tripwire-hook "sh" "ERROR: refused by nodecode-guard: rule rm-critical matched")
    (is (= 1 (length (tripwire-rows))) "another tool is still recorded")))

;;; --- the drill ------------------------------------------------------------

(deftest tripwire-cell-the-drill-stops-one-tool-and-records-it ()
  (with-tripwire (:drill "eval")
    (let* ((ran nil)
           (answer (funcall (cell-hook "nodecode-tripwire" :tool)
                            (list :name "eval" :arguments (nlk:json-object) :call-id "c1")
                            (lambda (op) (declare (ignore op)) (setf ran t) "ran"))))
      (is (search "refused by tripwire (drill)" answer) "the drill answers a failure the model reads")
      (is (null ran) "and the call never runs"))
    (is (equal "ran" (tripwire-hook "sh" "ran")) "every other tool is untouched")
    (let ((rows (tripwire-rows)))
      (is (= 1 (length rows)) "the drill is one row")
      (is (equal "drill" (getf (first rows) :how))))))

;;; --- the command and the tool ---------------------------------------------

(deftest tripwire-cell-the-command-digests-what-is-new-once ()
  (with-tripwire ()
    (dotimes (i 3)
      (tripwire-hook "eval" "ERROR: refused by nodecode-guard: rule rm-critical matched a critical path"))
    (let ((first (cell-entry "nodecode-tripwire" "tripwire" "")))
      (is (search "TRIPWIRE: 3 refused action(s) since the last check" first) first)
      (is (search "hook_refused" first) first))
    (is (search "nothing refused since the last check (3 in the log)"
                (cell-entry "nodecode-tripwire" "tripwire" "")) "the same rows are not digested twice")
    (is (search "TRIPWIRE: 3 refused action(s) in the log"
                (cell-entry "nodecode-tripwire" "tripwire" "peek")) "a peek reads them again")
    (is (search "nothing refused" (cell-entry "nodecode-tripwire" "tripwire" "")) "and a peek marks nothing read")
    (is (search "events.jsonl" (cell-entry "nodecode-tripwire" "tripwire" "path")) "path says where the log is")
    (is (search "unknown subcommand" (cell-entry "nodecode-tripwire" "tripwire" "wat")))))

(deftest tripwire-cell-the-tool-answers-through-the-wire (with-session-store ("s1"))
  (with-temp-directory (dir "tripwire-tool")
    (with-cell-stop ((tripwire-start
                      "events_file" (namestring (merge-pathnames "events.jsonl" dir))))
      (let ((nodecode-tripwire::*session* (lambda () "s1")))
        (tripwire-hook "eval" "ERROR: refused by nodecode-guard: rule rm-critical matched"))
      (let ((result (execute-wire-call "call-1" "tripwire_digest" "{\"verb\":\"peek\"}")))
        (is (equal "tool" (gethash "role" result)) "the call answers as a tool result")
        (is (search "TRIPWIRE: 1 refused action(s) in the log"
                    (princ-to-string (nle::message-content result)))))
      ;; The one member the contract declares is required: the preflight
      ;; refuses a call without it, and the body never runs.
      (let ((result (execute-wire-call "call-2" "tripwire_digest" "{}")))
        (is (search "verb" (princ-to-string (nle::message-content result))) "a call with no verb is refused")))))
