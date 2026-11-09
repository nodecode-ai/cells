;;;; sidecar.lisp --- the one network call: an admission, on loopback.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The only outbound connection this cell makes is HTTP to the literal
;;;; 127.0.0.1 at the configured port, endpoint /admit-action, carrying the
;;;; envelope and the run's scoped admission credential. No redirect, no
;;;; remote service, no telemetry. A rewrite of writer.py's _admit.

(in-package #:nodecode-deadbolt)

(defparameter +scheme+ "dbw1-"
  "What a scoped admission credential starts with: the engine's own prefix.
A token without it belongs to something else and is not sent.")

(defun http (method url &rest arguments)
  "The cell's one network call, NLK:HTTP; tests stub this name."
  (apply #'nlk:http method url arguments))

(defun env-value (name)
  "NAME's value in the environment, or NIL; tests stub this name."
  (uiop:getenv name))

(defun admission-token ()
  "The run's scoped admission credential: the variable admission_token_env
names, else the one pasted into admission_token; NIL when neither is set."
  (let ((name (setting :admission-token-env))
        (inline (setting :admission-token)))
    (or (and (stringp name) (plusp (length name)) (env-value name))
        (and (stringp inline) (plusp (length inline)) inline))))

(defun admit-url ()
  "The sidecar's admission endpoint on loopback, which is all this cell dials."
  (format nil "http://127.0.0.1:~d/admit-action" (setting :sidecar-port)))

(defun admission-headers (token)
  "What the admission call carries beside the envelope."
  (list (cons "Content-Type" "application/json")
        (cons "X-Deadbolt-Admission" token)))

(defun answer-code (answer)
  "ANSWER's code, cut to the 128 characters the plugin kept."
  (let ((code (gethash "code" answer)))
    (if (and (stringp code) (plusp (length code)))
        (subseq code 0 (min 128 (length code)))
        "unknown")))

(defun admit (text)
  "Ask the sidecar for exact admission of the envelope TEXT this run prepared.
=> (values DECISION CODE), DECISION \"allow\" or \"deny\"."
  (let ((token (admission-token)))
    (unless (and (stringp token) (uiop:string-prefix-p +scheme+ token))
      (return-from admit (values "deny" "missing_admission_credential")))
    (multiple-value-bind (body status)
        (http :post (admit-url)
              :headers (admission-headers token)
              :content text
              :timeout (setting :sidecar-timeout))
      (unless (= status 200)
        (return-from admit (values "deny" "sidecar_rejected")))
      (let ((answer (handler-case (nlk:decode-json body :whole t) (error () nil))))
        (unless (hash-table-p answer)
          (fail "the sidecar answered something that is not a decision"))
        (let ((decision (gethash "decision" answer)))
          (unless (member decision '("allow" "deny") :test #'equal)
            (fail "the sidecar's decision is ~s, not allow or deny" decision))
          (values decision (answer-code answer)))))))
