;;;; cell-test.lisp --- camel-guard: the capability table, the classifier's
;;;; contract, and the gate at the real :tool seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials a model: the classifier is a scripted function, the
;;;; trusted message is a durable turn's own input fact, and the call the hook
;;;; sees is the object the core hands it. What is proved is the guard's own
;;;; judgement -- which capability a call needs, what the operator's message
;;;; granted, and what happens when it granted nothing.

(in-package #:nodecode.test)

;;; --- fixtures --------------------------------------------------------------

(defmacro with-cg-classifier ((function) &body body)
  "Run BODY with the guard's classifier answering FUNCTION, put back on unwind."
  `(let ((nodecode-camel-guard::*classifier* ,function))
     ,@body))

(defun cg-plan (&rest allowed)
  "A classifier answer granting ALLOWED and denying nothing, as its JSON text."
  (nlk:encode-json-object
   (nlk:json-object "allowed_capabilities" (coerce allowed 'vector)
                     "denied_capabilities" (vector)
                     "rationale" "the operator asked for it")))

(defmacro with-cg-turn ((session &key (input "hi") (mode "enforce")) &body body)
  "A store holding SESSION, a durable turn opened by INPUT, the cell started in
MODE, and that turn as the one the hook runs for."
  `(with-session-store (,session)
     (with-durable-turn (turn ,session :command-id (concatenate 'string ,session "-cmd")
                               :input ,input :current t)
       (with-cell-stop ((camel-guard-start "mode" ,mode))
         (let ((nle::*live-turn* (nle::%make-live-turn :session-id ,session
                                                        :turn-id (nlk:durable-turn-turn-id turn))))
           ,@body)))))

(defun cg-call (form &key (tool "eval"))
  "The installed hook's answer for FORM called as TOOL, and whether it ran:
=> (values ANSWER RAN-P)."
  (let ((ran nil))
    (values (funcall (cell-hook "nodecode-camel-guard" :tool)
                     (list :name tool :arguments (nlk:json-object "form" form) :call-id "c")
                     (lambda (op) (declare (ignore op)) (setf ran t) :ran))
            ran)))

(defun cg-always-asked (message session)
  "A classifier that records what it was asked and grants nothing."
  (declare (ignore session))
  (push message *asked*)
  (cg-plan))

(defvar *asked* '()
  "The messages the scripted classifier was given, newest first.")

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "camel-guard"
  (:hooks :tool)
  (:command "camel")
  (:help :camel-guard)
  (:running (is (equal "off" (nodecode-camel-guard::setting :mode)) "an unsaid mode is off, as the original's was"))
  (:refused ("mode" "sometimes") ("trace_size" 0)))

;;; --- what a call needs -----------------------------------------------------

(defun cg-caps (form)
  (nodecode-camel-guard::form-capabilities (nlk:json-object "form" form)))

(deftest camel-guard-reads-a-calls-verbs ()
  (is (equal '("command_execution") (cg-caps "(sh \"ls -la\")")) "a shell call runs commands")
  (is (equal '("command_execution") (cg-caps "(uiop:run-program (list \"git\" \"status\"))")) "so does a spawn the form spells out")
  (is (equal '("file_mutation") (cg-caps "(write-file \"x.lisp\" ($ \"body\"))")) "a write mutates files")
  (is (equal '("file_mutation") (cg-caps "(edit \"x\" ($ \"old\") ($ \"new\"))")) "so does an edit")
  (is (equal '("delegation" "command_execution") (cg-caps "(progn (fork \"check it\") (sh \"ls\"))")) "every verb counts, in the order the form first calls one")
  (is (null (cg-caps "(+ 1 2)")) "arithmetic changes nothing")
  (is (null (cg-caps "(help :eval)")) "reading the manual changes nothing"))

(deftest camel-guard-reads-a-payload-or-a-comment-as-no-effect ()
  ;; EVAL's form carries file bodies, comments and test strings beside the
  ;; calls it makes; only the calls are read.
  (is (null (cg-caps "(write-string \"(sh \\\"rm -rf /\\\")\" s)")) "a spawn inside a string is a file's text")
  (is (null (cg-caps (format nil ";; (sh \"ls\") is how you list~%(+ 2 3)"))) "a comment is not a call")
  (is (null (cg-caps "(")) "a form that will not read carries no capability"))

(deftest camel-guard-unknown-tools-need-external-system ()
  (is (equal '("external_system") (nodecode-camel-guard::call-capabilities "mystery" nil '() '())) "a tool the guard does not know is conservative, as the original's default was")
  (is (equal '("browser_interaction") (nodecode-camel-guard::call-capabilities "browser_click" nil '() '())) "a browser tool is browser interaction")
  (is (null (nodecode-camel-guard::call-capabilities "web_search" nil '() '())) "a search changes nothing"))

(deftest camel-guard-exempts-what-the-operator-audited ()
  (is (null (nodecode-camel-guard::call-capabilities "eval" nil '("eval") '())) "read_only_tools exempts a name")
  (is (equal '("scheduled_action")
             (nodecode-camel-guard::call-capabilities "deploy" nil '() '(("deploy" . "scheduled_action")))) "tool_capabilities pins a tool's capability exactly")
  (is (equal '("command_execution")
             (nodecode-camel-guard::call-capabilities
              "eval" (nlk:json-object "form" "(sh \"x\")") '() '(("deploy" . "scheduled_action")))) "an override for another name changes nothing"))

;;; --- the classifier's answer -----------------------------------------------

(deftest camel-guard-holds-the-classifier-to-its-shape ()
  (let ((plan (nodecode-camel-guard::parse-plan
               (cg-plan "command_execution" "file_mutation"))))
    (is (equal '("command_execution" "file_mutation") (getf plan :allowed)) "a good answer is read")
    (is (equal "ok" (getf plan :status)) "and marked classified"))
  (let ((plan (nodecode-camel-guard::parse-plan
               (concatenate 'string "```json" (string #\Newline)
                            (cg-plan "command_execution")
                            (string #\Newline) "```"))))
    (is (equal '("command_execution") (getf plan :allowed)) "a fenced answer is unwrapped"))
  (flet ((refuses (text) (handler-case (progn (nodecode-camel-guard::parse-plan text) nil) (error () t))))
    (is (refuses "not json") "text that is not JSON is refused")
    (is (refuses "{\"allowed_capabilities\": \"x\"}") "an array that is not one is refused")
    (is (refuses "{\"allowed_capabilities\": [\"teleport\"], \"denied_capabilities\": [], \"rationale\": \"r\"}") "a capability the guard does not know is refused")
    (is (refuses (nlk:encode-json-object
                  (nlk:json-object "allowed_capabilities" (vector "command_execution")
                                    "denied_capabilities" (vector "command_execution")
                                    "rationale" 7))) "a rationale that is not text is refused")))

;;; --- the gate at the seam --------------------------------------------------

(deftest camel-guard-passes-a-turn-that-has-read-nothing ()
  (with-cg-turn ("camel-clean" :input "write the report")
    (with-cg-classifier ((lambda (&rest arguments)
                           (declare (ignore arguments))
                           (error "a clean turn needs no classifier")))
      (multiple-value-bind (answer ran) (cg-call "(sh \"make report\")")
        (is (eq :ran answer) "the first call of a turn runs, whatever it does")
        (is ran "the tool really ran")))
    (is (equal "clean_turn" (fifth (first nodecode-camel-guard::*trace*))) "and the trace says why it was not gated")))

(deftest camel-guard-grants-what-the-message-asked-for ()
  (with-cg-turn ("camel-grant" :input "run the test suite")
    (with-cg-classifier ((lambda (message session)
                           (declare (ignore session))
                           (is (equal "run the test suite" message) "the classifier sees the operator's words")
                           (cg-plan "command_execution")))
      (multiple-value-bind (answer ran) (cg-call "(+ 1 2)")
        (declare (ignore answer))
        (is ran "a call that changes nothing runs and taints the turn"))
      (multiple-value-bind (answer ran) (cg-call "(sh \"make test\")")
        (is (eq :ran answer) "command execution was asked for, so it runs")
        (is ran "and the tool ran")))))

(deftest camel-guard-refuses-a-capability-the-message-did-not-give ()
  (with-cg-turn ("camel-refuse" :input "what does this page say?")
    (with-cg-classifier ((lambda (message session)
                           (declare (ignore message session))
                           (cg-plan)))
      (cg-call "(+ 1 2)")
      (multiple-value-bind (answer ran) (cg-call "(sh \"make deploy\")")
        (is (stringp answer) "a changing call after untrusted output is refused")
        (is-carrying answer ("refused by nodecode-camel-guard" "the refusal names who refused")
          ("command execution" "and the capability it needed")
          ("trusted_intent_missing" "and the policy's reason")
          ("ask them to say it" "and what to do instead"))
        (is (not ran) "the tool never ran")))))

(deftest camel-guard-keeps-one-plan-for-the-turn ()
  (setf *asked* '())
  (with-cg-turn ("camel-once" :input "list the files")
    (with-cg-classifier
        ((lambda (message session)
           (declare (ignore session))
           (push message *asked*)
           (cg-plan "command_execution")))
      (cg-call "(+ 1 2)")
      (cg-call "(sh \"ls\")")
      (cg-call "(sh \"ls -la\")")
      (is (= 1 (length *asked*)) "the classifier is asked once for the turn"))))

(deftest camel-guard-a-failed-classifier-authorizes-nothing ()
  (with-cg-turn ("camel-fallback" :input "do the thing")
    (with-cg-classifier ((lambda (&rest arguments)
                           (declare (ignore arguments))
                           (error "the model is down")))
      (cg-call "(+ 1 2)")
      (multiple-value-bind (answer ran) (cg-call "(sh \"ls\")")
        (is (stringp answer) "enforce refuses on a classifier it could not get")
        (is-carrying answer ("fallback_read_only" "saying the plan fell back"))
        (is (not ran) "and the tool did not run"))))
  (with-cg-turn ("camel-fallback-monitor" :input "do the thing" :mode "monitor")
    (with-cg-classifier ((lambda (&rest arguments)
                           (declare (ignore arguments))
                           (error "the model is down")))
      (cg-call "(+ 1 2)")
      (multiple-value-bind (answer ran) (cg-call "(sh \"ls\")")
        (is (eq :ran answer) "monitor runs it anyway")
        (is ran "the tool ran")
        (is (eq :would-block (fourth (first nodecode-camel-guard::*trace*))) "and the decision is kept as what enforce would do")))))

(deftest camel-guard-monitor-refuses-nothing ()
  (with-cg-turn ("camel-monitor" :input "read it to me" :mode "monitor")
    (with-cg-classifier ((lambda (&rest arguments) (declare (ignore arguments)) (cg-plan)))
      (cg-call "(+ 1 2)")
      (multiple-value-bind (answer ran) (cg-call "(write-file \"x\" \"y\")")
        (is (eq :ran answer) "monitor is observation")
        (is ran "the write ran")))))

(deftest camel-guard-a-denied-capability-is-named ()
  (with-cg-turn ("camel-denied" :input "look, but do not touch anything")
    (with-cg-classifier
        ((lambda (&rest arguments)
           (declare (ignore arguments))
           (nlk:encode-json-object
            (nlk:json-object "allowed_capabilities" (vector "file_mutation")
                              "denied_capabilities" (vector "file_mutation")
                              "rationale" "they said not to touch anything"))))
      (cg-call "(+ 1 2)")
      (multiple-value-bind (answer ran) (cg-call "(write-file \"x\" \"y\")")
        (is (stringp answer) "a denied capability is refused")
        (is-carrying answer ("explicitly_denied" "as a denial, not a missing grant"))
        (is (not ran) "and the write did not happen")))))

(deftest camel-guard-off-runs-everything ()
  (with-cell-stop ((camel-guard-start "mode" "off"))
    (with-cg-classifier ((lambda (&rest arguments)
                           (declare (ignore arguments))
                           (error "off asks no one")))
      (multiple-value-bind (answer ran) (cg-call "(sh \"make deploy\")")
        (is (eq :ran answer) "off is the original's default and changes nothing")
        (is ran "the tool ran")))))

;;; --- the command -----------------------------------------------------------

(deftest camel-guard-command-reports-and-moves-the-mode ()
  (with-cg-turn ("camel-command" :input "hello")
    (let ((status (nodecode-camel-guard::run-slash "" "camel-command")))
      (is-carrying status ("mode enforce" "status names the running mode")
        ("/camel" "and how to move it")))
    (let ((answer (nodecode-camel-guard::run-slash "monitor" "camel-command")))
      (is-carrying answer ("mode monitor" "the command moves the mode"))
      (is (eq :monitor nodecode-camel-guard::*mode*) "at once, with no restart"))
    (let ((trace (nodecode-camel-guard::run-slash "trace" "camel-command")))
      (is (stringp trace) "trace answers text"))
    (let ((usage (nodecode-camel-guard::run-slash "sometimes" "camel-command")))
      (is-carrying usage ("/camel [status|off|monitor|enforce|trace]" "an unknown word is usage")))))
