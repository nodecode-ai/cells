;;;; cell-test.lisp --- the ACP dialogue, the tool, the command, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here starts a CLI. *OPEN-AGENT* is bound to a scripted peer that
;;;; answers the handshake the way the Kimi CLI does and asks this side for a
;;;; permission and a file read while a prompt is in flight, so what the client
;;;; sends and what it answers are asserted without a child process.

(in-package #:nodecode.test)

;;; --- a scripted peer -------------------------------------------------------------

(defparameter *kimi-script-log* '()
  "Every message the client sent the scripted peer, newest first.")

(defparameter *kimi-chunks* '("the agent's " "words")
  "The agent's words the scripted turn streams, in order.")

(defparameter *kimi-stop* "end_turn"
  "The stop reason the scripted turn reports beside its words.")

(defun script-result (id result)
  (nlk:json-object "jsonrpc" "2.0" "id" id "result" result))

(defun script-update (text)
  (nlk:json-object "jsonrpc" "2.0" "method" "session/update"
                    "params" (nlk:json-object
                              "sessionId" "s1"
                              "update" (nlk:json-object
                                        "sessionUpdate" "agent_message_chunk"
                                        "content" (nlk:json-object "type" "text" "text" text)))))

(defun script-options ()
  "The config options the CLI offers a session, as the scripted session/new says."
  (vector (nlk:json-object "id" "model" "currentValue" "kimi-code/k3"
                            "options" (vector (nlk:json-object "value" "kimi-code/k3" "name" "k3")
                                              (nlk:json-object "value" "kimi-code/k3-256k"
                                                               "name" "k3-256k")))
          (nlk:json-object "id" "thinking" "currentValue" "medium"
                            "options" (vector (nlk:json-object "value" "low")
                                              (nlk:json-object "value" "medium")
                                              (nlk:json-object "value" "high")))))

(defun kimi-script (request)
  "One scripted Kimi ACP peer: the handshake, one session, one prompt."
  (let ((method (gethash "method" request))
        (id (gethash "id" request)))
    (cond
      ((null method) '())                  ; a reply this side sent: nothing back
      ((equal method "initialize")
       (list (script-result id
                            (nlk:json-object "protocolVersion" 1
                                             "agentInfo" (nlk:json-object "name" "Kimi Code"
                                                                          "version" "0.29.2")
                                             "authMethods" (vector (nlk:json-object "id" "login"
                                                                                    "name" "login"))))))
      ((equal method "authenticate") (list (script-result id (nlk:make-json-object))))
      ((equal method "session/new")
       (list (script-result id (nlk:json-object "sessionId" "s1"
                                                "configOptions" (script-options)))))
      ((equal method "session/set_config_option") (list (script-result id (nlk:make-json-object))))
      ((equal method "session/prompt")
       (append
        (list (nlk:json-object
               "jsonrpc" "2.0" "id" 900 "method" "session/request_permission"
               "params" (nlk:json-object
                         "sessionId" "s1"
                         "options" (vector (nlk:json-object "optionId" "allow-once"
                                                             "kind" "allow_once" "name" "Allow")
                                           (nlk:json-object "optionId" "deny-once"
                                                             "kind" "reject_once" "name" "Reject"))))
              (nlk:json-object
               "jsonrpc" "2.0" "id" 901 "method" "fs/read_text_file"
               "params" (nlk:json-object "sessionId" "s1" "path" "notes.txt")))
        (loop for chunk in *kimi-chunks* collect (script-update chunk))
        (list (script-result id (nlk:json-object "stopReason" *kimi-stop*)))))
      (t '()))))

(defmacro with-scripted-kimi (&body body)
  "Run BODY with the cell's peer scripted by KIMI-SCRIPT."
  `(let ((*kimi-script-log* '())
         (peer (let ((inbox '()))
                 (nodecode-kimi-code-delegate::make-agent
                  :send (lambda (message)
                          (push message *kimi-script-log*)
                          (setf inbox (nconc inbox (kimi-script message)))
                          t)
                  :recv (lambda (timeout)
                          (declare (ignore timeout))
                          (if inbox (pop inbox) :timeout))
                  :close (lambda () nil)))))
     (let ((nodecode-kimi-code-delegate::*open-agent*
             (lambda (settings) (declare (ignore settings)) peer)))
       ,@body)))

(defun script-requests ()
  "The requests the client sent, oldest first."
  (nreverse (remove nil (mapcar (lambda (message) (gethash "method" message))
                                 *kimi-script-log*))))

(defun script-prompt ()
  "The text of the last session/prompt the client sent the peer."
  (let ((request (find "session/prompt" *kimi-script-log* :test #'equal
                       :key (lambda (message) (gethash "method" message)))))
    (gethash "text" (aref (gethash "prompt" (gethash "params" request)) 0))))

(defun script-replies ()
  "The messages this side sent that answer the agent, newest first."
  (remove-if (lambda (message) (gethash "method" message)) *kimi-script-log*))

;;; --- the slice -------------------------------------------------------------------

(define-test-slice "kimi-code-delegate" "KIMI-CODE-DELEGATE-CELL-"
  :start nodecode-kimi-code-delegate:start-cell
  :defaults (("command" "kimi") ("model" "k3") ("thinking" "auto")))

(defun kimi-tool ()
  "The registered kimi_code_delegate function, or NIL."
  (let ((entry (nle::find-tool "kimi_code_delegate")))
    (and entry (coerce (third entry) 'function))))

(defun kimi-arguments (&rest pairs)
  "The tool's arguments, decoded as the contract decodes them."
  (nodecode-kimi-code-delegate::decode-kimi-delegate-arguments
   (apply #'nlk:make-json-object pairs)))

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "kimi-code-delegate"
  (:config "command" "kimi")
  (:help :kimi-code)
  (:command "kimi-code")
  (:running
   (is (functionp (kimi-tool)) "the tool is registered")
   (is (equal "k3" (nodecode-kimi-code-delegate::setting :model)) "the model defaulted")
   (is (equal 300 (nodecode-kimi-code-delegate::setting :timeout-seconds)) "and the ceiling"))
  (:refused ("timeout_seconds" 0) ("startup_timeout_seconds" 0) ("model" "k4"))
  (:idle nodecode-kimi-code-delegate::kimi-code-delegate-error
         (nodecode-kimi-code-delegate::probe)))

;;; --- the tool --------------------------------------------------------------------

(deftest kimi-code-delegate-cell-the-dialogue-is-the-originals ()
  (with-cell-stop ((kimi-code-delegate-start "command" "kimi" "model" "k3-256k"))
    (with-scripted-kimi ()
      (let ((answer (funcall (kimi-tool) (kimi-arguments "prompt" "review this plan"))))
        (is (search "\"ok\":true" answer) "the call answered")
        (is (search "the agent's words" answer) "with the agent's own words")
        (is (search "kimi-code/k3-256k" answer) "and the model it ran on")
        (is (equal '("initialize" "authenticate" "session/new"
                     "session/set_config_option" "session/set_config_option" "session/prompt")
                   (script-requests)) "the handshake is the original's, in order")
        (is (search "READ-ONLY DELEGATION BOUNDARY" (script-prompt)) "the task rides inside the read-only boundary")))))

(deftest kimi-code-delegate-cell-the-client-denies-what-the-agent-asks ()
  (with-cell-stop ((kimi-code-delegate-start "command" "kimi"))
    (with-scripted-kimi ()
      (funcall (kimi-tool) (kimi-arguments "prompt" "review this plan"))
      (let* ((replies (script-replies))
             (denial (find-if (lambda (message) (gethash "result" message)) replies))
             (refusal (find-if (lambda (message) (gethash "error" message)) replies)))
        (is (equal "deny-once"
                   (gethash "optionId" (gethash "outcome" (gethash "result" denial)))) "a permission request picks the agent's own reject option")
        (is (equal -32000 (gethash "code" (gethash "error" refusal))) "a file read is answered with an error, not a file")))))

(deftest kimi-code-delegate-cell-an-empty-answer-is-a-failure ()
  (with-cell-stop ((kimi-code-delegate-start "command" "kimi"))
    (let ((*kimi-chunks* '()))
      (with-scripted-kimi ()
        (let ((answer (funcall (kimi-tool) (kimi-arguments "prompt" "review this plan"))))
          (is (search "\"ok\":false" answer) "no text is not a turn")
          (is (search "returned no text" answer) "and says so"))))))

(deftest kimi-code-delegate-cell-an-argument-the-cli-cannot-honour-is-refused ()
  (with-cell-stop ((kimi-code-delegate-start "command" "kimi"))
    (is (signals-error nodecode-kimi-code-delegate::kimi-code-delegate-error
          (nodecode-kimi-code-delegate::check-model "k4")))
    (is (signals-error nodecode-kimi-code-delegate::kimi-code-delegate-error
          (nodecode-kimi-code-delegate::check-thinking "Auto Mode!")))
    (is (signals-error nodecode-kimi-code-delegate::kimi-code-delegate-error
          (nodecode-kimi-code-delegate::check-prompt "   ")))))

;;; --- what the child inherits -------------------------------------------------------

(deftest kimi-code-delegate-cell-the-child-inherits-only-what-it-needs ()
  (let ((environment (nodecode-kimi-code-delegate::child-environment
                      '("HOME=/kimi-home" "PATH=/usr/bin" "KIMI_CODE_HOME=/kimi"
                        "OPENAI_API_KEY=sk-abc" "GITHUB_TOKEN=abc"))))
    (is (member "HOME=/kimi-home" environment :test #'equal) "HOME, where the login is")
    (is (member "PATH=/usr/bin" environment :test #'equal) "PATH, to find its own tools")
    (is (member "KIMI_CODE_HOME=/kimi" environment :test #'equal) "the CLI's own home")
    (is (not (member "OPENAI_API_KEY=sk-abc" environment :test #'equal)) "a provider key never reaches the CLI")
    (is (not (member "GITHUB_TOKEN=abc" environment :test #'equal)) "nor a token")
    (is (not (search "abc123" (nodecode-kimi-code-delegate::redact "access_token=abc123 done"))) "a credential in child output is redacted")))

;;; --- the command ------------------------------------------------------------------

(deftest kimi-code-delegate-cell-the-doctor-answers-the-handshake ()
  (with-cell-stop ((kimi-code-delegate-start "command" "kimi"))
    (with-scripted-kimi ()
      (let ((shown (nle:slash "/kimi-code doctor")))
        (is (search "Kimi Code ACP: OK (Kimi Code 0.29.2)" shown) "who answered")
        (is (search "kimi-code/k3-256k" shown) "the models it offers")
        (is (search "delegate-only" shown) "and that it is no route")))))
