;;;; cell-test.lisp --- lifecycle, the tool over a stubbed runner, /phone.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here runs phone-harness: *RUN* is the cell's one seam to the
;;;; child, and every test that runs a script binds it to a stub. What is
;;;; proved is the cell's own wiring -- the argv and the environment it
;;;; builds, the bound it passes, what it makes of an exit code, a timeout and
;;;; a runner that is not installed, which image path it reports, that the
;;;; contract refuses a call with no code, and that the approval question
;;;; never starts a script nobody answered for.

(in-package #:nodecode.test)

(define-test-slice "phone-harness" "PHONE-HARNESS-" :start nodecode-phone-harness:start-cell)

;;; --- the stubs ----------------------------------------------------------------

(defvar *runs* '()
  "What the stubbed runner was called with, newest first.")

(defvar *answers* '()
  "What the stubbed runner answers next: (STDOUT STDERR STATUS) each.")

(defun stub-run (argv &key seconds directory environment input)
  "The runner, as the cell calls it: one call recorded, one canned answer."
  (push (list :argv argv :seconds seconds :directory directory
              :environment environment :input input)
        *runs*)
  (destructuring-bind (out err status) (or (pop *answers*) (list "" "" 0))
    (values out err status)))

(defun broken-run (argv &key seconds directory environment input)
  "A runner that is not on PATH: what SPAWN-PROGRAM signals."
  (declare (ignore seconds directory environment input))
  (push (list :argv argv) *runs*)
  (error "spawning ~a: No such file or directory" (first argv)))

(defmacro with-phone-harness ((&rest pairs) &body body)
  "The cell started with PAIRS as its section, its runner stubbed."
  `(let ((*runs* '()) (*answers* '()))
     (with-cell-stop ((phone-harness-start ,@pairs))
       (let ((nodecode-phone-harness::*run* 'stub-run))
         ,@body))))

(defun phone-call (&rest pairs)
  "phone_exec's answer for PAIRS: the call decoded through the declaration
the tool registered, as the dispatch seam does it."
  (let ((entry (or (nle::find-tool "phone_exec")
                    (error "phone_exec is not registered"))))
    (funcall (third entry)
             (nlk:contract-decode (second entry)
                                   (apply #'nlk:make-json-object pairs)))))

(defun answer-json (text)
  "TEXT, the tool's answer, read as JSON."
  (shasht:read-json text))

(defun environment-entry (prefix environment)
  "The child environment entry that starts with PREFIX, or NIL."
  (find-if (lambda (entry) (uiop:string-prefix-p prefix entry)) environment))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "phone-harness"
  (:help :phone)
  (:command "phone")
  (:running (is (nle::find-tool "phone_exec") "the tool is registered")
            (is (equal "phone-harness" (nodecode-phone-harness::setting :command)))
            (is (equal "auto" (nodecode-phone-harness::setting :platform)))
            (is (= 300 (nodecode-phone-harness::setting :timeout-seconds)))
            (is (null (nodecode-phone-harness::setting :ask)) "the question starts off"))
  (:refused ("platform" "windows") ("timeout_seconds" 1)))

;;; --- the tool -----------------------------------------------------------------

(deftest phone-harness-a-script-runs-through-the-runner ()
  (with-phone-harness ()
    (setf *answers* (list (list "battery 82%" "" 0)))
    (let* ((json (answer-json (phone-call "code" "print(screen())")))
           (run (first *runs*))
           (environment (getf run :environment)))
      (is (eq t (gethash "success" json)) "it succeeded")
      (is (= 0 (gethash "exit_code" json)))
      (is (equal "battery 82%" (gethash "output" json)) "what the script printed comes back")
      (is (equal '("phone-harness") (getf run :argv)) "the command line is the argv")
      (is (equal "print(screen())" (getf run :input)) "the script is what the child reads on stdin")
      (is (= 300 (getf run :seconds)) "the original's default bound")
      (is (member "PH_CLIENT=nodecode" environment :test #'equal) "PH_CLIENT names the harness")
      (is (member "PHONE_HARNESS_TELEMETRY=0" environment :test #'equal) "telemetry is off")
      (is (environment-entry "PH_AGENT_WORKSPACE=" environment) "the workspace rides along")
      (is (null (environment-entry "PHONE_HARNESS_PLATFORM=" environment)) "auto names no platform"))))

(deftest phone-harness-a-failed-script-keeps-its-stderr-tail ()
  (with-phone-harness ()
    (setf *answers* (list (list "" (format nil "  Traceback: no phone~%  ") 1)))
    (let ((json (answer-json (phone-call "code" "1/0"))))
      (is (null (gethash "success" json)) "success is false")
      (is (= 1 (gethash "exit_code" json)) "with the exit code")
      (is (equal "Traceback: no phone" (gethash "stderr" json)) "the end of stderr, trimmed"))))

(deftest phone-harness-a-call-bounds-the-script-and-scrubs-the-child ()
  (with-phone-harness ()
    (phone-call "code" "print(1)" "timeout_s" 9999)
    (phone-call "code" "print(1)" "timeout_s" 1)
    (let ((bounds (mapcar (lambda (run) (getf run :seconds)) *runs*)))
      (is (equal '(5 1800) bounds) "each call clamped to the original's 5 and 1800")))
  (with-phone-harness ("platform" "ios" "telemetry" t)
    (phone-call "code" "print(1)")
    (let ((environment (getf (first *runs*) :environment)))
      (is (member "PHONE_HARNESS_PLATFORM=ios" environment :test #'equal))
      (is (member "PHONE_HARNESS_TELEMETRY=1" environment :test #'equal))))
  (with-phone-harness ()
    (let ((name "NODECODE_PHONE_TEST_TOKEN"))
      (setf (uiop:getenv name) "inert")
      (unwind-protect
           (progn
             (phone-call "code" "print(1)")
             (let* ((environment (getf (first *runs*) :environment))
                    (value (environment-entry (concatenate 'string name "=") environment)))
               (is (member name environment :test #'equal) "a credential-suffixed variable is named bare")
               (is (null value) "and its value never reaches the child")))
        (setf (uiop:getenv name) "")))))

(deftest phone-harness-only-an-image-this-call-wrote-is-named ()
  (let* ((png (merge-pathnames (format nil "phone-harness-test-~d.png" (get-universal-time))
                                (uiop:temporary-directory)))
         (gone (merge-pathnames "phone-harness-test-gone.png" (uiop:temporary-directory))))
    (with-open-file (stream png :direction :output :if-exists :supersede)
      (write-string "PNG" stream))
    (unwind-protect
         (with-phone-harness ()
           (setf *answers*
                 (list (list (format nil "saved to ~a; also shot.png and ~a~%"
                                     (uiop:native-namestring png)
                                     (uiop:native-namestring gone))
                             "" 0)))
           (let* ((json (answer-json (phone-call "code" "screenshot()")))
                  (shots (gethash "screenshots" json)))
             (is (= 1 (length shots)) "one screenshot: the absolute path that exists")
             (is (equal (uiop:native-namestring png) (aref shots 0)))
             (is (search "(look " (gethash "note" json)) "the result says how to see it")))
      (ignore-errors (delete-file png)))))

(deftest phone-harness-a-timeout-and-a-missing-runner-are-failures ()
  (with-phone-harness ()
    (setf *answers* (list (list "" "" :timeout)))
    (is-carrying (answer (phone-call "code" "print(1)"))
      ("success\":false") ("timed out after 300s") ("max 1800"))
    (let ((nodecode-phone-harness::*run* 'broken-run))
      (is-carrying (answer (phone-call "code" "print(1)"))
        ("could not start phone-harness") ("uv tool install phone-harness")))))

(deftest phone-harness-the-tool-rests-on-its-declaration ()
  (with-phone-harness ()
    (is (typep (second (nle::find-tool "phone_exec")) 'nlk:contract) "on its declaration")
    (let ((refusal (refusal-text nlk:contract-refusal (phone-call "timeout_s" 5))))
      (is (search "code" refusal) "a call with no code is refused before the body runs"))
    (is (null *runs*) "and nothing ran")
    (is-carrying (answer (phone-call "code" "   "))
      ("No code provided") ("ensure_device()"))
    (is (null *runs*) "nor for a script of blanks")))

(deftest phone-harness-ask-refuses-a-script-nobody-answered-for ()
  (with-phone-harness ("ask" t)
    (is-carrying (answer (phone-call "code" "print(1)"))
      ("nobody answered") ("set the phone-harness section"))
    (is (null *runs*) "and the script did not run")))

;;; --- /phone -------------------------------------------------------------------

(deftest phone-harness-slash-answers-the-state-and-checks-the-runner ()
  (with-phone-harness ()
    (is-carrying (answer (nodecode-phone-harness::run-slash "" nil))
      ("phone_exec") ("phone-harness") ("platform auto") ("ask off") ("/phone check"))
    (is (null *runs*) "the bare command starts nothing")
    (setf *answers* (list (list "phone-harness 0.3.3" "" 0)))
    (is-carrying (answer (nodecode-phone-harness::run-slash "check" nil))
      ("phone-harness 0.3.3") ("--version => 0"))
    (let ((argv (getf (first *runs*) :argv)))
      (is (equal '("phone-harness" "--version") argv) "check runs the runner's --version"))
    (setf *answers* '())
    (let ((nodecode-phone-harness::*run* 'broken-run))
      (is-carrying (answer (nodecode-phone-harness::run-slash "check" nil))
        ("did not start") ("uv tool install phone-harness")))))
