;;;; cell-test.lisp --- the approval round trip against a fake ntfy server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wire and its refusals: which text
;;;; of a call the rules read, which answers count, and every path that ends
;;;; in a denial. The fake server is the seam the cell publishes through, so
;;;; a test reads the very bytes a phone would have been shown.

(in-package #:nodecode.test)

(define-test-slice "ntfy-approval" "NTFY-APPROVAL"
  :start nodecode-ntfy-approval:start-cell
  :defaults (("topic" "nodecode-approval-topic-0123456789")))

;;; --- a fake ntfy server ------------------------------------------------------

(defparameter +fake-answers+
  '(("once" . "Approve once") ("session" . "Approve for session") ("deny" . "Deny"))
  "Which notification button a test's fake phone would tap.")

(defun make-fake-ntfy (&key answer (publish-status 200))
  "A stub for the cell's HTTP seam, answering as ntfy does.
=> (VALUES HANDLER CALLS): HANDLER is the stub; CALLS answers the requests it
saw, oldest first, each a plist of :METHOD :URL :BODY. With ANSWER, the button
with that label is tapped, and its own body comes back on the reply topic."
  (let ((calls '()) (pending nil))
    (values
     (lambda (method url &key headers content timeout)
       (declare (ignore headers timeout))
       (push (list :method method :url url :body content) calls)
       (cond
         ((equal method "POST")
          (let ((body (ignore-errors (shasht:read-json content))))
            (when (and answer (hash-table-p body))
              (let ((wanted (cdr (assoc answer +fake-answers+ :test #'string=)))
                    (actions (gethash "actions" body)))
                (loop for action across (if (vectorp actions) actions (vector))
                      when (equal wanted (gethash "label" action))
                        do (setf pending (gethash "body" action))))))
          (values "{\"id\":\"m1\",\"time\":1700000000}" publish-status))
         ((equal method "GET")
          (values (if pending
                      (format nil "{\"id\":\"r1\",\"event\":\"message\",\"time\":1700000001,\"message\":~a}~%"
                              (shasht:write-json pending nil))
                      "")
                  200))
         (t (values "" 200))))
     (lambda () (reverse calls)))))

(defmacro with-fake-ntfy ((calls &rest keys) &body body)
  "BODY with the cell's HTTP, clock and sleep seams answering a fake ntfy server."
  `(let* ((fake (multiple-value-list (make-fake-ntfy ,@keys)))
          (handler (first fake))
          (,calls (second fake))
          (nodecode-ntfy-approval::*http* handler)
          (nodecode-ntfy-approval::*sleep* (lambda (seconds) (declare (ignore seconds))))
          (nodecode-ntfy-approval::*clock* (let ((now 0)) (lambda () (incf now)))))
     (declare (ignorable ,calls))
     ,@body))

;;; --- running one call through the installed hook --------------------------------

(defun ntfy-verdict (form next &rest strings)
  "The installed hook's answer for FORM, its strings object being STRINGS
(name text ...): what NEXT answered, or the refusal string."
  (funcall (cell-hook "nodecode-ntfy-approval" :tool)
           (list :name "eval"
                 :arguments (if strings
                                (nlk:json-object
                                 "form" form
                                 "strings" (let ((object (nlk:json-object)))
                                             (loop for (name text) on strings by #'cddr
                                                   do (setf (gethash name object) text))
                                             object))
                                (nlk:json-object "form" form))
                 :call-id "c")
           next))

(defun ntfy-ran (form)
  "The hook's answer for FORM when the call it gates answers :ran."
  (ntfy-verdict form (lambda (op) (declare (ignore op)) :ran)))

(defun ntfy-scenario (config form &key strings answer (publish-status 200))
  "Start the cell with CONFIG (section pairs), run FORM through its hook with a
fake ntfy server answering ANSWER, and stop it.
=> (VALUES VERDICT RAN-P CALLS): what the hook answered, whether the call ran,
and what the server saw."
  (let* ((fake (multiple-value-list (make-fake-ntfy :answer answer
                                                     :publish-status publish-status)))
         (calls (second fake))
         (nodecode-ntfy-approval::*http* (first fake))
         (nodecode-ntfy-approval::*sleep* (lambda (seconds) (declare (ignore seconds))))
         (nodecode-ntfy-approval::*clock* (let ((now 0)) (lambda () (incf now))))
         (ran nil)
         (stop (apply #'ntfy-approval-start config))
         (verdict (apply #'ntfy-verdict form
                         (lambda (op) (declare (ignore op)) (setf ran t) :ran)
                         strings)))
    (funcall stop)
    (values verdict ran (funcall calls))))

(defun ntfy-post (calls)
  "The one notification CALLS carries, or NIL."
  (find "POST" calls :key (lambda (call) (getf call :method)) :test #'equal))

(defun ntfy-gets (calls)
  "Every read of the reply topic CALLS carries."
  (remove-if-not (lambda (call)
                   (and (equal "GET" (getf call :method))
                        (search "-reply/json" (getf call :url))))
                 calls))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "ntfy-approval"
  (:hooks :tool)
  (:command "ntfy")
  (:refused ("priority" "loud") ("topic" "short")))

;;; --- the round trip -------------------------------------------------------------

(deftest ntfy-approval-cell-runs-what-the-phone-approves
  (multiple-value-bind (verdict ran calls) (ntfy-scenario '() "(sh \"sudo ls /root\")" :answer "once")
    (is (eq :ran verdict) "Approve once lets the call run")
    (is ran "which ran")
    (let ((post (ntfy-post calls)))
      (is post "one notification was published")
      (is-carrying (body (getf post :body))
        ("nodecode-approval-topic-0123456789" "to the configured topic")
        ("sudo ls /root" "carrying the command as the call wrote it")
        ("Approve once" "with the buttons")
        ("Deny" "including Deny")
        ("No answer means deny" "and the deadline the phone is told")))
    (let ((reads (ntfy-gets calls)))
      (is reads "and the reply topic was read")
      (is (search "-reply/json?poll=1" (getf (first reads) :url)) "the reply topic is read in poll mode"))))

(deftest ntfy-approval-cell-runs-nothing-the-phone-denies
  (multiple-value-bind (verdict ran) (ntfy-scenario '() "(sh \"sudo ls\")" :answer "deny")
    (is-carrying verdict
      ("refused by nodecode-ntfy-approval" "the model is told who refused")
      ("chose Deny" "and that the operator chose Deny")
      ("do not retry" "and must not work around it"))
    (is (not ran) "the call did not run")))

(deftest ntfy-approval-cell-remembers-a-session-approval
  (let ((stop (ntfy-approval-start)))
    (with-fake-ntfy (calls :answer "session")
      (is (eq :ran (ntfy-ran "(sh \"sudo ls\")")) "Approve for session runs the call")
      (let ((asked (length (funcall calls))))
        (is (eq :ran (ntfy-ran "(sh \"sudo id\")")) "and the next call of the same rule runs")
        (is (= asked (length (funcall calls))) "without asking again")))
    (funcall stop)))

(deftest ntfy-approval-cell-denies-on-silence
  (multiple-value-bind (verdict ran)
      (ntfy-scenario (list "timeout_seconds" 10) "(sh \"git push --force origin main\")")
    (is-carrying verdict
      ("no answer on the phone" "no answer is a deny, saying so")
      ("force-push" "naming the rule that asked"))
    (is (not ran) "the call did not run")))

(deftest ntfy-approval-cell-denies-when-ntfy-refuses
  (multiple-value-bind (verdict ran) (ntfy-scenario '() "(sh \"sudo ls\")" :publish-status 500)
    (is-carrying verdict ("HTTP 500" "a server that answers an error denies the call"))
    (is (not ran) "which did not run")))

(deftest ntfy-approval-cell-denies-when-not-set-up
  (multiple-value-bind (verdict ran calls) (ntfy-scenario (list "topic" "") "(sh \"sudo ls\")")
    (is-carrying verdict ("not set up" "a section with no topic denies rather than guesses"))
    (is (null calls) "and nothing was published")
    (is (not ran) "the call did not run")))

;;; --- which calls are read ---------------------------------------------------------

(deftest ntfy-approval-cell-leaves-ordinary-work-alone
  (dolist (form '("(* 6 7)"
                  "(sh \"ls -la\")"
                  "(sh \"git status\")"
                  "(sh \"rm -f build/one-file\")"
                  "(sh \"rg -n sudo src\")"
                  "(sh \"git push origin main\")"
                  "(write-file \"x.lisp\" \"(sh \\\"sudo ls\\\")\")"))
    (multiple-value-bind (verdict ran calls) (ntfy-scenario '() form)
      (is (eq :ran verdict) (format nil "runs: ~a" form))
      (is ran (format nil "which ran: ~a" form))
      (is (null calls) (format nil "without the network: ~a" form)))))

(deftest ntfy-approval-cell-reads-a-command-the-strings-carry
  (multiple-value-bind (verdict ran calls)
      (ntfy-scenario '() "(sh ($ \"s\"))" :strings '("s" "sudo ls") :answer "deny")
    (declare (ignore ran))
    (is (stringp verdict) "a command carried in the strings object is read")
    (is-carrying (body (getf (ntfy-post calls) :body)) ("sudo ls" "and rides the notification"))))

(deftest ntfy-approval-cell-asks-about-the-operators-own-patterns
  (multiple-value-bind (verdict ran calls)
      (ntfy-scenario (list "patterns" (vector "\\bmake\\s+deploy\\b")) "(sh \"make deploy\")" :answer "once")
    (is (eq :ran verdict) "a custom pattern is asked about, and a tap runs the call")
    (is ran "which ran")
    (is (search "custom-1" (getf (ntfy-post calls) :body)) "under its own rule id")))

;;; --- /ntfy --------------------------------------------------------------------------

(deftest ntfy-approval-cell-command-answers-all-three-ways
  (let ((stop (ntfy-approval-start)))
    (is-carrying (text (cell-entry "nodecode-ntfy-approval" "ntfy" "setup"))
      ("ntfy-approval setup" "setup names itself")
      ("topic = nodecode-" "and prints a fresh topic")
      ("/ntfy test" "with the next step"))
    (is-carrying (text (cell-entry "nodecode-ntfy-approval" "ntfy" ""))
      ("on https://ntfy.sh" "no argument reports the state"))
    (is-carrying (text (cell-entry "nodecode-ntfy-approval" "ntfy" "nonsense"))
      ("usage" "an unknown action says how to use it"))
    (with-fake-ntfy (calls :answer "deny")
      (is-carrying (text (cell-entry "nodecode-ntfy-approval" "ntfy" "test"))
        ("Your phone answered: Deny" "test reports the tap")
        ("work" "and that approvals work")))
    (funcall stop)))

(deftest ntfy-approval-cell-test-without-a-topic-says-so
  (let ((stop (ntfy-approval-start "topic" "")))
    (is-carrying (text (cell-entry "nodecode-ntfy-approval" "ntfy" "test"))
      ("No approval came back" "a test with no topic reports no answer")
      ("no topic is set" "and names the missing setting"))
    (funcall stop)))
