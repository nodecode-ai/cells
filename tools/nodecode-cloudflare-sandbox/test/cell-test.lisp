;;;; cell-test.lisp --- the cell against a stubbed Worker: what it asks, what it answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange goes through NLE:HTTP, so the whole protocol is one stub
;;;; here: nothing is dialled, no container is started, and the assertions are
;;;; on the request the cell built (route, bearer token, JSON body) and on the
;;;; answer it made of the reply.

(in-package #:nodecode.test)

(define-test-slice "cloudflare-sandbox" "CLOUDFLARE-SANDBOX-CELL-"
  :start nodecode-cloudflare-sandbox:start-cell)

;;; --- the bridge these tests answer as -------------------------------------------------

(defparameter +exec-stream+
  (format nil "{\"type\":\"heartbeat\"}~%~
{\"type\":\"started\",\"request_id\":\"~a\"}~%~
{\"type\":\"output\",\"data\":\"hello\"}~%~
{\"type\":\"output\",\"data\":\" world\\n\"}~%~
{\"type\":\"exit\",\"exit_code\":0}~%"
          (make-string 32 :initial-element #\a))
  "One successful command as the Worker streams it.")

(defvar *asked* nil
  "Every exchange the stub answered this test, newest first, as
(METHOD URL HEADERS BODY).")

(defvar *answers* nil
  "Answers a test overrides the stub with: (MATCH TEXT STATUS), first match
wins, MATCH a substring of the URL or a function of it.")

(defmacro with-bridge (&body body)
  "BODY with NLE:HTTP stubbed as the operator's Worker: an exec answers
+EXEC-STREAM+, anything else {} with 200, unless *ANSWERS* says otherwise for
a URL."
  `(let ((*asked* nil) (*answers* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list method url (getf args :headers) (getf args :content)) *asked*)
                                (let ((rule (find-if (lambda (rule)
                                               (let ((match (first rule)))
                                                 (if (functionp match) (funcall match url) (search match url))))
                                             *answers*)))
                                  (if rule
                                      (values (second rule) (third rule))
                                      (if (search "/exec" url)
                                          (values +exec-stream+ 200)
                                          (values "{}" 200)))))
       ,@body)))

(defun answer-with (match text status)
  "Make the stub answer every URL holding MATCH with TEXT and STATUS."
  (push (list match text status) *answers*))

(defun asked-for (fragment)
  "The newest exchange whose URL holds FRAGMENT."
  (find-if (lambda (asked) (search fragment (second asked))) *asked*))

(defun create-asked ()
  "The newest exchange that declared the workspace: a sandbox route with no
suffix, which the exec route is distinguished from."
  (find-if (lambda (asked)
             (and (search "/v1/sandboxes/" (second asked))
                  (not (search "/exec" (second asked)))))
           *asked*))

(defun asked-body (asked)
  "The JSON body ASKED carried, decoded, or NIL."
  (let ((content (fourth asked)))
    (and content (plusp (length content)) (nlk:decode-json content))))

(defun asked-headers (asked)
  "The alist of headers ASKED carried."
  (third asked))

(defun sandbox-call (&rest pairs)
  "The cf_sandbox tool as the engine calls it: the declared contract decoded,
then the body."
  (let ((entry (nle::find-tool "cf_sandbox")))
    (assert entry () "cf_sandbox is not registered")
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defmacro with-sandbox (&body body)
  "BODY with the cell started on a healthy section, and the Worker stubbed."
  `(with-bridge
     (with-cell-stop ((cloudflare-sandbox-start "endpoint" "https://sandbox.example.test"
                                                "token" "cfut-token"))
       ,@body)))

;;; --- lifecycle, one assertion per thing NLE:DEFINE-CELL owns -----------------------------

(define-cell-lifecycle-tests "cloudflare-sandbox"
  (:config "endpoint" "https://sandbox.example.test" "token" "cfut-token")
  (:help :cloudflare-sandbox)
  (:command "cloudflare-sandbox")
  (:running (is (equal "/workspace" (nodecode-cloudflare-sandbox::setting :cwd)) "cwd defaults to /workspace")
            (is (equal "hermes" (nodecode-cloudflare-sandbox::setting :image)) "image defaults to the plugin's")
            (is (stringp (nodecode-cloudflare-sandbox:sandbox-name)) "a workspace is named"))
  (:refused ("endpoint" "http://sandbox.example.test" "token" "cfut-token")
            ("endpoint" "https://sandbox.example.test" "instance" "standard-9" "token" "cfut-token")
            ("endpoint" "https://sandbox.example.test" "timeout_seconds" 0 "token" "cfut-token"))
  (:idle nodecode-cloudflare-sandbox:cloudflare-sandbox-error
         (nodecode-cloudflare-sandbox:run "true")))

;;; --- what the cell asks the Worker -----------------------------------------------------

(deftest cloudflare-sandbox-cell-the-tool-runs-one-command-through-the-bridge ()
  (with-sandbox
    (let ((answer (sandbox-call "command" "printf hello")))
      (is (search "hello world" answer) "the stream's output is the answer")
      (is (search "exit 0" answer) "and the exit status with it"))
    (let* ((exec (asked-for "/exec"))
           (body (asked-body exec)))
      (is exec "one command stream was asked")
      (is (equal "POST" (first exec)))
      (is (search "/v1/sandboxes/" (second exec)))
      (is (equal "Bearer cfut-token"
                 (cdr (assoc "Authorization" (asked-headers exec) :test #'string-equal))))
      (is (search "printf hello" (nlk:json-value body :string "command")))
      (is (search "/workspace" (nlk:json-value body :string "command")))
      (is (equal 120 (nlk:json-value body :integer "timeout")) "the section's ceiling")
      (is (equal 32 (length (nlk:json-value body :string "request_id"))) "a fresh request id"))
    (let* ((create (create-asked))
           (name (subseq (second create) (+ 14 (search "/v1/sandboxes/" (second create))))))
      (is (search "/v1/sandboxes/nc-" (second create)) (format nil "the workspace name is a hash, got ~s" name))
      (is (ppcre:scan "^nc-[0-9a-f]+$" name)
          (format nil "no path of this machine rides the name, got ~s" name))
      (is (<= (length name) 63) "the Worker's identifier width"))))

(deftest cloudflare-sandbox-cell-the-token-env-is-read-at-start ()
  (let ((saved (uiop:getenv "CF_SANDBOX_TOKEN")))
    (unwind-protect
         (with-bridge
           (setf (uiop:getenv "CF_SANDBOX_TOKEN") "cfut-from-env")
           (with-cell-stop ((cloudflare-sandbox-start "endpoint" "https://sandbox.example.test"
                                                      "token_env" "CF_SANDBOX_TOKEN"))
             (sandbox-call "command" "true")
             (is (equal "Bearer cfut-from-env"
                        (cdr (assoc "Authorization" (asked-headers (asked-for "/exec"))
                                    :test #'string-equal))))))
      (if saved
          (setf (uiop:getenv "CF_SANDBOX_TOKEN") saved)
          (sb-posix:unsetenv "CF_SANDBOX_TOKEN")))))

(deftest cloudflare-sandbox-cell-a-workspace-already-declared-is-not-a-failure ()
  (with-bridge
    (answer-with (lambda (url) (and (search "/v1/sandboxes/nc-" url) (not (search "/exec" url))))
                 "{\"error\":{\"code\":\"conflict\"}}" 409)
    (with-cell-stop ((cloudflare-sandbox-start "endpoint" "https://sandbox.example.test"
                                               "token" "cfut-token"))
      (is (search "exit 0" (sandbox-call "command" "true"))))))

(deftest cloudflare-sandbox-cell-refusals-are-words-the-model-can-act-on ()
  (with-bridge
    (with-cell-stop ((cloudflare-sandbox-start "endpoint" "https://sandbox.example.test"
                                               "token" "cfut-token"))
      (answer-with "/exec" "{\"error\":{\"code\":\"unauthorized\"}}" 401)
      (let ((words (refusal-text cf-sandbox:cloudflare-sandbox-error
                     (sandbox-call "command" "true"))))
        (is (search "HTTP 401" words))
        (is (search "unauthorized" words))
        (is (search "already ran" words) "the model is told not to repeat it blind"))
      (setf *answers* nil)
      (answer-with "/exec" (format nil "{\"type\":\"error\",\"code\":\"output_limit\"}~%") 200)
      (let ((words (refusal-text cf-sandbox:cloudflare-sandbox-error
                     (sandbox-call "command" "true"))))
        (is (search "output_limit" words)))
      (setf *answers* nil)
      (answer-with "/exec" (format nil "{\"type\":\"output\",\"data\":\"half\"}~%") 200)
      (let ((words (refusal-text cf-sandbox:cloudflare-sandbox-error
                     (sandbox-call "command" "true"))))
        (is (search "without an exit status" words) "a stream cut short is not a success")))))

(deftest cloudflare-sandbox-cell-the-command-answers-health-and-resets ()
  (with-sandbox
    (let ((status (cell-entry "nodecode-cloudflare-sandbox" "cloudflare-sandbox" "")))
      (is (search "sandbox.example.test" status))
      (is (search "/workspace" status))
      (is (asked-for "/health") "the command asks the Worker health"))
    (let ((status (cell-entry "nodecode-cloudflare-sandbox" "cloudflare-sandbox" "reset")))
      (is (search "clean" status))
      (let ((reset (asked-for "/v1/sandboxes/")))
        (is (equal "DELETE" (first reset)))
        (is (eq t (nlk:json-value (asked-body reset) :boolean "confirm")) "a reset is confirmed")))
    (let ((status (cell-entry "nodecode-cloudflare-sandbox" "cloudflare-sandbox" "release")))
      (is (search "snapshot" status))
      (is (search "/release" (second (asked-for "/release")))))))

(deftest cloudflare-sandbox-cell-an-unreachable-worker-is-a-refusal ()
  (with-bridge
    (with-cell-stop ((cloudflare-sandbox-start "endpoint" "https://sandbox.example.test"
                                               "token" "cfut-token"))
      (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                 (declare (ignore method url args))
                                 (error "connection refused"))
        (let ((words (refusal-text cf-sandbox:cloudflare-sandbox-error
                       (sandbox-call "command" "true"))))
          (is (search "could not be reached" words))
          (is (search "connection refused" words)))))))
