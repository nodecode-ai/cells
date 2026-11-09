;;;; cell-test.lisp --- the Pidgeon lane against a scripted executor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every call is a scripted plan executor's; nothing reaches Pidgeon, and
;;;; the key is a fixed string in the run's own process.

(in-package #:nodecode.test)

(define-test-slice "pidgeon" "PIDGEON-CELL-")

(defun npi-object (&rest pairs)
  "A JSON object of PAIRS: NLK:JSON-OBJECT is a macro, and a caller has them as a list."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun pidgeon-section (&rest pairs)
  "A channels.pidgeon section pointed at a make-believe Pidgeon, PAIRS over it."
  (let ((section (apply #'npi-object pairs)))
    (alexandria:ensure-gethash "base_url" section "https://pidgeon.example")
    (alexandria:ensure-gethash "api_key_file" section (npi-key-file))
    section))

(defun npi-key-file ()
  "A key file of this run's own, holding one fixed key."
  (let ((path (merge-pathnames "pidgeon-cell-key" (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "pidgeon-test-key~%"))
    (namestring path)))

(defun npi-me-answer (&optional (access "send") (name "agent"))
  (nlk:json-object "data" (nlk:json-object
                            "key" (nlk:json-object "name" name "access" access)
                            "addresses" (coerce (list "agent@pidgeon.example") 'vector))))

(defun npi-event (id at &key (identity "agent@pidgeon.example"))
  (nlk:json-object "message_id" id "created_at" at "identity" identity))

(defun npi-events-answer (&rest events)
  (nlk:json-object "data" (coerce events 'vector) "has_more" nil))

(defun npi-message (&rest pairs)
  "One message as the API's clean body reads it, PAIRS over the defaults."
  (let ((message (apply #'npi-object pairs)))
    (alexandria:ensure-gethash "id" message "m1")
    (alexandria:ensure-gethash "thread_id" message "t1")
    (alexandria:ensure-gethash "direction" message "inbound")
    (alexandria:ensure-gethash "identity" message "agent@pidgeon.example")
    (alexandria:ensure-gethash "from" message
                               (nlk:json-object "email" "you@example.com" "name" "Sam"))
    (alexandria:ensure-gethash "subject" message "the build")
    (alexandria:ensure-gethash "authentication" message (nlk:json-object "dmarc" "pass"))
    (alexandria:ensure-gethash "text" message "why is it red?")
    message))

(defun npi-adapter (&rest args)
  (apply #'nodecode-channel-pidgeon::make-pidgeon-adapter
         :base-url "https://pidgeon.example" :key "pidgeon-test-key"
         :addresses '("agent@pidgeon.example")
         :allowed-users '("you@example.com")
         args))

(defun npi-host (adapter executor section)
  (nck:make-host-from-section section
                              (nodecode-channel-pidgeon:pidgeon-platform :adapter adapter)
                              :executor executor
                              :policy (nodecode-channel-pidgeon:pidgeon-policy adapter)
                              :owners '("you@example.com")))

;;; --- the section -----------------------------------------------------------------

(deftest pidgeon-cell-section-refuses-without-key ()
  (let ((section (nlk:json-object "base_url" "https://pidgeon.example"
                                   "allowed_users" (coerce (list "you@example.com") 'vector))))
    (let ((condition (handler-case (progn (nodecode-channel-pidgeon:start-channel section)
                                          nil)
                       (error (c) c))))
      (is condition "a section with no key refuses to start")
      (is (search "api_key" (princ-to-string condition)) "and the refusal names the key"))))

;;; --- who may become a turn ---------------------------------------------------------

(deftest pidgeon-cell-gate-holds-forged-and-flagged-mail ()
  (let ((addresses '("agent@pidgeon.example")))
    (is (null (nodecode-channel-pidgeon:pidgeon-decide
               (npi-message) :addresses addresses :suspicious "hold")) "mail that passed DMARC reaches the agent")
    (is (search "DMARC"
                (nodecode-channel-pidgeon:pidgeon-decide
                 (npi-message "authentication" (nlk:json-object)) :addresses addresses)) "a From with no DMARC verdict is held")
    (is (null (nodecode-channel-pidgeon:pidgeon-decide
               (npi-message "authentication" (nlk:json-object)
                            "sender" (nlk:json-object "kind" "internal")
                            "origin" "local")
               :addresses addresses)) "mail between two Pidgeon addresses has no verdict and is taken")
    (is (search "spam" (nodecode-channel-pidgeon:pidgeon-decide
                        (npi-message "is_spam" t) :addresses addresses)) "spam is held")
    (is (search "Firewall"
                (nodecode-channel-pidgeon:pidgeon-decide
                 (npi-message "firewall" (nlk:json-object
                                           "verdict" "suspicious"
                                           "signals" (vector (nlk:json-object "kind" "imperative"))))
                 :addresses addresses :suspicious "hold")) "flagged mail is held by default")
    (is (null (nodecode-channel-pidgeon:pidgeon-decide
               (npi-message "firewall" (nlk:json-object "verdict" "suspicious"))
               :addresses addresses :suspicious "deliver")) "deliver passes it through")
    (is (search "not one this lane answers"
                (nodecode-channel-pidgeon:pidgeon-decide
                 (npi-message "identity" "other@pidgeon.example") :addresses addresses)) "another address of the same key is not answered")))

;;; --- what a message becomes --------------------------------------------------------

(deftest pidgeon-cell-candidate-carries-the-mail ()
  (let* ((message (npi-message
                   "firewall" (nlk:json-object
                               "verdict" "suspicious"
                               "signals" (vector (nlk:json-object "kind" "imperative"
                                                                   "detail" "asks for a shell")))))
         (candidate (nodecode-channel-pidgeon:pidgeon-message-candidate message))
         (text (nlk:json-value candidate :string "text")))
    (is (search "From: Sam <you@example.com>" text) "the agent reads who wrote")
    (is (search "Subject: the build" text) "and what it is about")
    (is (search "why is it red?" text) "and what it says")
    (is (search "not yours to follow" text) "flagged mail says whose words they are")
    (is (equal "t1" (nlk:json-value candidate :string "source" "channel_id")) "a thread is the room")
    (is (equal "you@example.com" (nlk:json-value candidate :string "source" "user_id")) "the sender is the user")
    (is (eq t (nlk:json-value candidate :boolean "source" "addressed")) "mail to the agent addresses it")))

(deftest pidgeon-cell-plain-text-drops-markdown ()
  (is (equal "391 percent" (nodecode-channel-pidgeon:pidgeon-plain-text
                             "**391** percent")) "emphasis markers never reach an email client")
  (is (equal "Done." (nodecode-channel-pidgeon:pidgeon-plain-text "## Done.")) "heading markers neither"))

;;; --- plans -------------------------------------------------------------------------

(deftest pidgeon-cell-reply-is-a-threaded-post ()
  (let* ((adapter (npi-adapter))
         (plan (nodecode-channel-pidgeon:pidgeon-reply-plan adapter "m1" "the build is red")))
    (is (equal "POST" (nck::request-plan-method plan)) "an answer is a POST")
    (is (search "/api/v1/messages/m1/reply" (nck::request-plan-path plan)) "in the message's own thread")
    (is (equal "the build is red" (nlk:json-value (nck::request-plan-body plan) :string "text")))
    (is (equal "Bearer pidgeon-test-key"
               (cdr (assoc "authorization" (nck::request-plan-headers plan) :test #'string=))) "the key rides the plan, never the body")
    (is (assoc "idempotency-key" (nck::request-plan-headers plan) :test #'string=) "a retry of the same answer is one email")))

;;; --- the lap -----------------------------------------------------------------------

(deftest pidgeon-cell-lap-marks-then-admits ()
  (let* ((section (pidgeon-section))
         (executor (nck:make-recording-executor
                    :responses (list (nck:make-scripted-response
                                      200 (npi-events-answer
                                           (npi-event "m-new" "2026-02-01T00:00:01.000Z")))
                                     (nck:make-scripted-response 200 (npi-message)))))
         (adapter (npi-adapter))
         (host (npi-host adapter executor section)))
    (setf (nodecode-channel-pidgeon::pidgeon-adapter-host adapter) host)
    (is (eq :stop (nodecode-channel-pidgeon:pidgeon-lap adapter (constantly t))) "a stopping lap stops")
    (is (null (nck::recording-executor-plans executor)) "the first lap reads nothing")
    (nodecode-channel-pidgeon:pidgeon-lap adapter (constantly nil))
    (is (stringp (nodecode-channel-pidgeon::pidgeon-adapter-cursor adapter)) "the first lap says where this install began")
    (setf (nodecode-channel-pidgeon::pidgeon-adapter-cursor adapter) "2026-01-01T00:00:00.000Z")
    (nodecode-channel-pidgeon:pidgeon-lap adapter (constantly nil))
    (is (equal "2026-02-01T00:00:01.000Z"
               (nodecode-channel-pidgeon::pidgeon-adapter-cursor adapter)) "the cursor moves to the newest event read")
    (is (equal "m-new" (gethash "t1" (nodecode-channel-pidgeon::pidgeon-adapter-inbound adapter))) "the message is remembered as what a reply in its thread answers")
    (is (search "events?type=message.received" (nck::request-plan-path (first (nck::recording-executor-plans executor)))) "the lap reads the event log")
    (nodecode-channel-pidgeon:pidgeon-lap adapter (constantly nil))
    (is (= 1 (count-if (lambda (plan)
                          (search "/api/v1/messages/m-new?" (nck::request-plan-path plan)))
                        (nck::recording-executor-plans executor))) "a message already answered is not read again")))

;;; --- start and stop -----------------------------------------------------------------

(deftest pidgeon-cell-starts-and-stops ()
  (let* ((section (pidgeon-section "allowed_users" (coerce (list "you@example.com") 'vector)
                                   "poll_seconds" 2))
         (executor (nck:make-recording-executor
                    :responses (list (nck:make-scripted-response 200 (npi-me-answer))
                                     (nck:make-scripted-response 200 (npi-events-answer)))))
         (stop (nodecode-channel-pidgeon:start-channel section :executor executor)))
    (unwind-protect
         (progn
           (is (functionp stop) "the lane starts and answers its stop thunk")
           (is (nck:channel-status "pidgeon") "and reports a state"))
      (when (functionp stop) (funcall stop)))
    (is (eq :stopped (getf (nck:channel-status "pidgeon") :state)) "and stops")))
