;;;; setup.lisp --- signing in to a relay with a device code.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's `hermes artifact-relay setup' was a blocking device
;;;; authorization: ask the control plane for a code, print the URL and the
;;;; code, poll every interval until the operator approves, keep the token the
;;;; answer carries in the environment file at 0600, save the service origin
;;;; through the harness's own config command, and publish one artifact to
;;;; prove the pair works. Here the same flow is a worker started by
;;;; /artifact-relay setup: the command answers at once with the URL and the
;;;; code, the worker polls in the background, and the token goes to the
;;;; token file the section names (0600) beside the service origin it learns.

(in-package #:nodecode-artifact-relay)

(defstruct (pending (:copier nil))
  "A device authorization waiting for the operator's approval."
  (device-code "")
  (user-code "")
  (verification-url "")
  (origin nil)
  (interval 5)
  (deadline 0)
  (failures 0))

(defvar *pending* nil
  "The device authorization being polled, or NIL.")

(defvar *worker* nil
  "The thread polling it, or NIL.")

(defun unix-seconds ()
  "Now, in seconds since 1970."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun notice-key ()
  "The key this cell's one standing notice rides under."
  "nodecode-artifact-relay")

(defun say (text &key (level :info))
  (nle:notice text :level level :key (notice-key)))

(defun control-plane-origin ()
  "The control plane a sign-in starts at: the section's control_plane, else
the hosted one the original defaulted to."
  (origin-of (or (secret-text (setting :control-plane)) +control-plane+)
             :label "control_plane"))

(defun percent-encode (text)
  "TEXT with everything but unreserved characters percent-encoded, as the
activation URL quotes its code."
  (with-output-to-string (out)
    (loop for character across text
          do (if (or (alphanumericp character)
                      (member character '(#\- #\. #\_ #\~)))
                 (write-char character out)
                 (format out "%~(~2,'0x~)" (char-code character))))))

(defun verification-url-p (value origin user-code)
  "Whether VALUE is ORIGIN's own /activate/<code> page: nowhere else is the
operator sent to approve a token."
  (let ((uri (and (secret-text value) (ignore-errors (quri:uri value)))))
    (and uri
         (same-origin-p origin uri)
         (string= (or (quri:uri-path uri) "")
                  (format nil "/activate/~a" (percent-encode user-code))))))

(defun device-fields (json)
  "The four things a device authorization answer must carry."
  (let ((device-code (secret-text (json-text json "device_code")))
        (user-code (secret-text (json-text json "user_code")))
        (verification (secret-text (json-text json "verification_uri_complete")))
        (expires-in (nlk:json-value json :number "expires_in")))
    (unless (and device-code user-code verification)
      (fail "the control plane's authorization answer is missing device_code, user_code or verification_uri_complete"))
    (unless (and expires-in (plusp expires-in))
      (fail "the control plane's authorization answer has no positive expires_in"))
    (values device-code user-code verification expires-in)))

(defun begin-setup (&optional service)
  "Ask SERVICE (the section's control_plane unless named) for a device code,
start polling, and answer the words the operator reads."
  (let ((origin (origin-of (or (secret-text service) (setting :control-plane) +control-plane+)
                           :label "the control plane")))
    (multiple-value-bind (json status)
        (call :post (format nil "~a/api/device/authorizations" (origin-text origin))
              :auth nil
              :content "{}"
              :headers (list (cons "content-type" "application/json")
                             (cons "user-agent" "nodecode-artifact-relay/0.1")))
      (let ((json (answer-json json status)))
        (multiple-value-bind (device-code user-code verification expires-in)
            (device-fields json)
          (unless (verification-url-p verification origin user-code)
            (fail "the control plane's activation URL is not its own /activate page: ~s"
                  verification))
          (let ((interval (max 1 (or (nlk:json-value json :number "interval") 5))))
            (setf *pending* (make-pending :device-code device-code
                                           :user-code user-code
                                           :verification-url verification
                                           :origin origin
                                           :interval interval
                                           :deadline (+ (unix-seconds) (ceiling expires-in))))
            (start-poller)
            (format nil "Open ~a~%and enter the code ~a (it expires in ~d minutes).~%This cell is polling for the approval; /artifact-relay status says whether it landed, and /artifact-relay cancel stops waiting."
                    verification user-code (ceiling expires-in 60))))))))

(defun start-poller ()
  (stop-poller)
  (setf *worker* (nlk:worker-start "artifact-relay-setup" #'poll-attempt
                                    :wake (lambda ()
                                            (if *pending* (pending-interval *pending*) 5))
                                    :prime t)))

(defun stop-poller ()
  (when *worker*
    (setf *worker* (nlk:worker-stop *worker*))))

(defun expire-pending (&optional (why "the code expired"))
  (setf *pending* nil)
  (say (format nil "artifact-relay: ~a; run /artifact-relay setup again" why) :level :warning))

(defun poll-attempt ()
  "One poll of the token endpoint. Answering :STOP ends the worker."
  (let ((pending *pending*))
    (cond
      ((null pending) :stop)
      ((>= (unix-seconds) (pending-deadline pending))
       (expire-pending) :stop)
      (t
       (multiple-value-bind (json status)
           (handler-case
               (call :post (format nil "~a/api/device/token" (origin-text (pending-origin pending)))
                     :auth nil
                     :content (nlk:encode-json-object
                               (nlk:make-json-object "device_code" (pending-device-code pending)))
                     :headers (list (cons "content-type" "application/json")
                                    (cons "user-agent" "nodecode-artifact-relay/0.1")))
             (artifact-relay-error (condition)
               (when (zerop (pending-failures pending))
                 (say (format nil "artifact-relay: waiting, but the control plane did not answer: ~a"
                              condition)
                      :level :warning))
               (incf (pending-failures pending))
               (values nil nil)))
         (let ((error (and (hash-table-p json) (json-text json "error"))))
           (cond
             ((equal error "authorization_pending") nil)
             ((equal error "slow_down") (incf (pending-interval pending) 5) nil)
             ((member error '("expired_token" "authorization_expired") :test #'equal)
              (expire-pending) :stop)
             ((member error '("access_denied" "denied") :test #'equal)
              (setf *pending* nil)
              (say "artifact-relay: the sign-in was declined" :level :warning)
              :stop)
             ((or error (not (ok-p status)))
              (setf *pending* nil)
              (say (format nil "artifact-relay: the sign-in failed: ~a" (refusal-line json status))
                   :level :error)
              :stop)
             (t (finish-setup pending json) :stop))))))))

(defun publish-smoke ()
  "One artifact through the pair just signed in, so the operator sees a link
that works. Answers its URL, or the words of the refusal."
  (handler-case
      (multiple-value-bind (json id url)
          (publish :title "Artifact Relay connected"
                   :content "This artifact says the Artifact Relay pair works: a token is kept on this
machine and this link came back from the service.

Publish another with the `artifact_publish` tool."
                   :summary "The link a finished sign-in published"
                   :format "markdown"
                   :expires-days 7)
        (declare (ignore json id))
        url)
    (error (condition) (princ-to-string condition))))

(defun finish-setup (pending json)
  "Keep the pair the approved sign-in answered with, and say where the proof
artifact is."
  (declare (ignore pending))
  (let* ((api-token (secret-text (json-text json "api_token")))
         (service (origin-of (or (secret-text (json-text json "base_url")) "")
                             :label "the service URL the sign-in answered with")))
    (unless api-token
      (fail "the approved sign-in answered without an api_token"))
    (save-service (origin-text service))
    (keep-token api-token)
    (setf *pending* nil)
    (let ((smoke (publish-smoke)))
      (say (format nil "artifact-relay: signed in to ~a; the token is in ~a~%Proof: ~a"
                    (origin-text service) (namestring (token-path)) smoke))
      (format nil "Signed in to ~a. The token is kept in ~a (0600) and the service in ~a.~%~@[Proof artifact: ~a~]"
              (origin-text service) (namestring (token-path)) (namestring (setup-path)) smoke))))

(defun cancel-setup ()
  (let ((waiting (and *pending* t)))
    (setf *pending* nil)
    (stop-poller)
    (if waiting
        "Stopped waiting for the approval; run /artifact-relay setup to start again."
        "Nothing was waiting for an approval.")))

;;; --- what /artifact-relay says ---------------------------------------------------

(define-verb status-line ()
  "What this cell is pointed at, and what it is still missing."
  (let ((service (ignore-errors (base-url-text)))
        (token (ignore-errors (token)))
        (waiting *pending*))
    (with-output-to-string (out)
      (format out "Artifact Relay: ~a~%" (if (and service token) "ready" "not ready yet"))
      (format out "  service: ~a~%" (or service
                                         (format nil "none -- set base_url in the artifact-relay section, or run /artifact-relay setup")))
      (format out "  token:   ~a~%" (if token
                                       (format nil "kept (~a)" (if (secret-text (file-text (token-path)))
                                                                    (namestring (token-path))
                                                                    (format nil "from ~a" (setting :token-env))))
                                       (format nil "none -- run /artifact-relay setup, or set ~a" (or (secret-text (setting :token-env)) +token-variable+))))
      (format out "  tools:   artifact_publish, artifact_read~%")
      (format out "  proof:   ~a~%" (if (probe-file (setup-path)) (namestring (setup-path)) "no sign-in has saved a service yet"))
      (when waiting
        (format out "  waiting: open ~a and enter ~a~%"
                (pending-verification-url waiting) (pending-user-code waiting))))))
