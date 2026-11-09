;;;; cell-test.lisp --- lifecycle, the four tools and /deepinfra, against a stubbed API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange is a stubbed NLK:HTTP, so nothing reaches DeepInfra, needs
;;;; a key or touches the network. What is proved is the cell's own wiring:
;;;; the section (a healthy start, a wrong value and a missing key refused),
;;;; the sandbox lifecycle, the four tools as the model calls them -- their
;;;; arguments, their headers and their answer -- and the operator's command.

(in-package #:nodecode.test)

;;; --- the stubbed service --------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *state* "running"
  "The state the stubbed sandbox answers a get with.")

(defvar *gone* nil
  "When true, the stubbed sandbox is not there: a get answers 404.")

(defvar *file* "hello\n"
  "What a read of a file answers.")

(defvar *missing-file* nil
  "When true, a read answers 404.")

(defvar *exec-lines* '("{\"stdout\":\"hi\n\"}" "{\"returncode\":0}")
  "The NDJSON lines a stubbed exec streams.")

(defun df-headers (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (name value) on pairs by #'cddr do (setf (gethash name table) value))
    table))

(defun df-record ()
  (format nil "{\"sandbox_id\":\"sb-test\",\"plan\":\"small\",\"image\":\"base\",\"state\":~s,\"tags\":{}}"
          (or *state* "running")))

(defun df-answer (method url args)
  "What the stubbed sandbox API answers a request => (values BODY STATUS HEADERS)."
  (push (list* method url args) *requests*)
  (cond ((uiop:string-suffix-p url "/exec")
         (values (format nil "~{~a~%~}" *exec-lines*) 200 (df-headers "content-type" "application/x-ndjson")))
        ((uiop:string-suffix-p url "/start")
         (setf *state* "running")
         (values (df-record) 200 (df-headers "content-type" "application/json")))
        ((uiop:string-suffix-p url "/stop")
         (setf *state* "stopped")
         (values (df-record) 200 (df-headers "content-type" "application/json")))
        ((search "/fs/content" url)
         (cond ((eql method :put) (values "" 200 (df-headers)))
               (*missing-file* (values "{\"error\":\"no such file\"}" 404 (df-headers)))
               (t (values (nlk:utf-8-octets *file*) 200 (df-headers "content-type" "text/plain")))))
        ((eql method :delete)
         (values "" 200 (df-headers)))
        ((eql method :post)
         (setf *gone* nil)
         (values "{\"sandbox_id\":\"sb-test\"}" 200 (df-headers "content-type" "application/json")))
        (*gone* (values "{\"error\":\"no such sandbox\"}" 404 (df-headers)))
        (t (values (df-record) 200 (df-headers "content-type" "application/json")))))

(defmacro with-deepinfra-wire (&body body)
  "BODY with NLK:HTTP stubbed, the cell's memory of a sandbox cleared and its
id file a path under the scratch home, so no test reaches DeepInfra or leaves
a file behind."
  `(let ((*requests* '())
         (*state* "running")
         (*gone* nil)
         (*file* "hello\n")
         (*missing-file* nil)
         (*exec-lines* '("{\"stdout\":\"hi\n\"}" "{\"returncode\":0}")))
     (let ((nodecode-deepinfra-sandbox::*state-file* (nlk:home "deepinfra-sandbox-test.id")))
       (ignore-errors (delete-file nodecode-deepinfra-sandbox::*state-file*))
       (let ((nodecode-deepinfra-sandbox::*sandbox-id* nil)
             (nodecode-deepinfra-sandbox::*sandbox-state* nil)
             (nodecode-deepinfra-sandbox::*state-read* t))
         (unwind-protect
              (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                         (df-answer method url args))
                ,@body)
           (ignore-errors (delete-file nodecode-deepinfra-sandbox::*state-file*)))))))

(defun df-asked (&optional (method nil))
  "The stubbed requests, oldest first, of METHOD when it names one."
  (let ((asked (reverse *requests*)))
    (if method (remove method asked :key #'first :test-not #'eql) asked)))

(defun df-asked-to (fragment)
  "The stubbed requests whose URL carries FRAGMENT, oldest first."
  (remove-if-not (lambda (request) (search fragment (second request))) (df-asked)))

(defun df-created ()
  "The requests that make a sandbox."
  (remove-if-not (lambda (request) (uiop:string-suffix-p (second request) "/v1/sandboxes"))
                 (df-asked)))

(defun df-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun df-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun df-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "deepinfra-call" name arguments)))

;;; --- lifecycle ------------------------------------------------------------------

(define-test-slice "deepinfra-sandbox" "DEEPINFRA-CELL-"
  :start nodecode-deepinfra-sandbox:start-cell)

(define-cell-lifecycle-tests "deepinfra-sandbox"
  (:fixture with-deepinfra-wire)
  (:config "api_key" "di-test")
  (:help :deepinfra-sandbox)
  (:command "deepinfra")
  (:running
   (is (nle::find-tool "deepinfra_run") "run is registered")
   (is (nle::find-tool "deepinfra_read") "and read")
   (is (nle::find-tool "deepinfra_write") "and write")
   (is (nle::find-tool "deepinfra_sandbox") "and the sandbox's own lifecycle")
   (is (= 4 (count-if (lambda (entry) (eql 0 (search "deepinfra_" (first entry))))
                       nle::*tools*)) "four of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "deepinfra_run")) "and stop takes every one back")
   (is (null (nle::find-tool "deepinfra_sandbox"))))
  (:refused ("timeout_seconds" 1))
  (:idle deepinfra-sandbox:deepinfra-sandbox-error (deepinfra-sandbox:exec "ls")))

(deftest deepinfra-cell-a-start-with-no-key-refuses-and-installs-nothing (with-deepinfra-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (let ((text (refusal-text nlk:config-refusal (deepinfra-sandbox-start))))
      (is-carrying text "needs a key" "DEEPINFRA_API_KEY")
      (is (null (nle::find-tool "deepinfra_run")) "nothing was installed"))))

(deftest deepinfra-cell-a-start-dials-nothing (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (is (null (df-asked)) "no sandbox is made until something needs one")))

;;; --- running a command ------------------------------------------------------------

(deftest deepinfra-cell-a-run-makes-a-sandbox-then-sends-the-command (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (let ((answer (df-call "deepinfra_run" "{\"command\": \"echo hi\"}")))
      (is (search "hi" answer) "the command's output is the answer")
      (is (search "[exit 0]" answer) "with its exit code")
      (let ((create (first (df-created)))
            (exec (first (df-asked-to "/exec"))))
        (is (equal :post (first create)) "a sandbox is made first")
        (is (equal "Bearer di-test" (df-header exec "authorization")) "the key rides every call, and is the section's")
        (is (equal (list "bash" "-c" "echo hi") (coerce (nlk:json-value (df-body exec) :array "command") 'list)) "the command reaches the service as bash -c, verbatim")
        (is (= 60 (nlk:json-value (df-body exec) :integer "timeout_seconds")) "with the default ceiling")
        (is (null (search "di-test" answer)) "and the key is never in the answer")))))

(deftest deepinfra-cell-a-run-honours-its-own-timeout (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (df-call "deepinfra_run" "{\"command\": \"sleep 5\", \"timeout_seconds\": 900}")
    (is (= 900 (nlk:json-value (df-body (first (df-asked-to "/exec"))) :integer "timeout_seconds")) "the call's own timeout is the one sent")))

(deftest deepinfra-cell-a-stopped-sandbox-is-woken-first (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (setf *state* "stopped"
          nodecode-deepinfra-sandbox::*sandbox-id* "sb-test")
    (let ((answer (df-call "deepinfra_run" "{\"command\": \"ls\"}")))
      (is (search "[exit 0]" answer) answer)
      (is (null (df-created)) "and no second sandbox was made")
      (is (eql 1 (length (df-asked-to "/start"))) "the stopped one was started again")
      (is (equal '("start" "exec")
                 (loop for request in (df-asked)
                       when (equal :post (first request))
                         collect (car (last (uiop:split-string (second request)
                                                               :separator "/"))))) "with the start before the command"))))

(deftest deepinfra-cell-a-sandbox-that-is-gone-is-made-again (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (setf nodecode-deepinfra-sandbox::*sandbox-id* "sb-old")
    (setf *gone* t)
    (let ((answer (df-call "deepinfra_run" "{\"command\": \"ls\"}")))
      (is (search "[exit 0]" answer) "the command ran in a fresh sandbox")
      (is (equal 1 (length (df-created))) "one sandbox was made")
      (is (search "/v1/sandboxes/sb-old" (second (first (df-asked-to "sb-old")))) "the one it held was asked after first"))))

(deftest deepinfra-cell-an-id-file-carries-the-sandbox-across-a-restart (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (let ((nodecode-deepinfra-sandbox::*state-read* nil))
      (ensure-directories-exist nodecode-deepinfra-sandbox::*state-file*)
      (with-open-file (out nodecode-deepinfra-sandbox::*state-file* :direction :output
                                                             :if-exists :supersede
                                                             :if-does-not-exist :create)
        (write-string "sb-test" out))
      (df-call "deepinfra_run" "{\"command\": \"ls\"}")
      (is (null (df-created)) "no sandbox is made when one is remembered")
      (is (eql 1 (length (df-asked-to "sb-test/exec"))) "the remembered one takes the command"))))

(deftest deepinfra-cell-a-stream-with-no-return-code-is-an-error (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (setf *exec-lines* '("{\"stdout\":\"partial\n\"}")
          nodecode-deepinfra-sandbox::*sandbox-id* "sb-test")
    (let ((answer (df-call "deepinfra_run" "{\"command\": \"ls\"}")))
      (is (search "ERROR: " answer) "a failure is the tool result, not a condition up the stack")
      (is (search "partial" answer) "and keeps what did arrive"))))

(deftest deepinfra-cell-the-service-s-error-is-the-models-repair (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (setf *exec-lines* '("{\"error\":\"command timed out after 60s\"}")
          nodecode-deepinfra-sandbox::*sandbox-id* "sb-test")
    (is (search "command timed out after 60s" (df-call "deepinfra_run" "{\"command\": \"ls\"}")) "the service's own words reach the model")))

;;; --- files ------------------------------------------------------------------------

(deftest deepinfra-cell-writing-sends-the-whole-file (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (setf nodecode-deepinfra-sandbox::*sandbox-id* "sb-test")
    (let ((answer (df-call "deepinfra_write"
                           "{\"path\": \"/workspace/a.txt\", \"content\": \"hi there\"}")))
      (let ((put (first (df-asked :put))))
        (is (search "path=/workspace/a.txt" (second put)) "the path rides the query")
        (is (equal "hi there" (nlk:decode-octets (getf (cddr put) :content))) "and the content is the body")
        (is (equal "application/octet-stream" (df-header put "content-type")) "as raw bytes"))
      (is (search "wrote 8 bytes" answer) answer))))

(deftest deepinfra-cell-reading-answers-the-file (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (setf nodecode-deepinfra-sandbox::*sandbox-id* "sb-test"
          *file* "one\ntwo\n")
    (is (equal "one\ntwo\n" (df-call "deepinfra_read" "{\"path\": \"/workspace/a.txt\"}")) "the file's own text is the answer")
    (setf *missing-file* t)
    (let ((answer (df-call "deepinfra_read" "{\"path\": \"/workspace/b.txt\"}")))
      (is (search "there is no file" answer) answer))))

(deftest deepinfra-cell-a-path-outside-workspace-refuses-before-any-call (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (setf nodecode-deepinfra-sandbox::*sandbox-id* "sb-test")
    (let ((answer (df-call "deepinfra_read" "{\"path\": \"/etc/hostname\"}")))
      (is (search "outside /workspace" answer) "the refusal names the rule")
      (is (null (df-asked)) "and nothing was sent"))))

;;; --- the sandbox's own lifecycle ---------------------------------------------------

(deftest deepinfra-cell-status-dials-nothing (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (let ((answer (df-call "deepinfra_sandbox" "{\"action\": \"status\"}")))
      (is (search "none yet" answer) "a home with no sandbox says so")
      (is (null (df-asked)) "without dialling anything"))))

(deftest deepinfra-cell-stop-keeps-the-sandbox-and-delete-removes-it (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (setf nodecode-deepinfra-sandbox::*sandbox-id* "sb-test")
    (let ((answer (df-call "deepinfra_sandbox" "{\"action\": \"stop\"}")))
      (is (search "is stopped" answer) answer)
      (is (eql 1 (length (df-asked-to "/stop"))) "and stops the sandbox, keeping /workspace"))
    (let ((answer (df-call "deepinfra_sandbox" "{\"action\": \"delete\"}")))
      (is (search "deleted" answer) "delete answers too")
      (is (eql 1 (length (df-asked :delete))) "with one delete")
      (is (null (nodecode-deepinfra-sandbox::remembered-id)) "and the home forgets the sandbox"))))

;;; --- the operator's command ---------------------------------------------------------

(deftest deepinfra-cell-the-command-reports-the-sandbox (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (let ((status (nodecode-deepinfra-sandbox::run-command "" nil)))
      (is (search "deepinfra sandbox" status) "the command names the sandbox")
      (is (null (search "di-test" status)) "without ever printing the key")
      (is (null (df-asked)) "and sends nothing"))
    (let ((odd (nodecode-deepinfra-sandbox::run-command "bogus" nil)))
      (is (search "no action called bogus" odd) "an unknown word says what it did not do"))))

(deftest deepinfra-cell-the-command-runs-an-action (with-deepinfra-wire)
  (with-cell-stop ((deepinfra-sandbox-start "api_key" "di-test"))
    (let ((answer (nodecode-deepinfra-sandbox::run-command "status" nil)))
      (is (search "none yet" answer) "status is the same answer the tool gives"))))
