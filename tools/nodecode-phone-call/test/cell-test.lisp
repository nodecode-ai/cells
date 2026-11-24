;;;; cell-test.lisp --- lifecycle, the four tools and /phone-call, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Twilio, needs an
;;;; account or touches the network, and the clock is a constant: a call's
;;;; fifteen-minute approval is one binding away.

(in-package #:nodecode.test)

;;; --- fixtures ---------------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defvar *phone-config*
  (list "allowed_numbers" (vector "+14155550123")
        "user_name" "Ada"
        "account_sid" "ACtest"
        "auth_token" "tok"
        "from_number" "+14155550100")
  "A configured section: one number allowed, Twilio set, a name to disclose.")

(defmacro with-phone-wire (&body body)
  "BODY with NLK:HTTP stubbed, the cell's clock held still and its store reset:
a request is logged in *REQUESTS* and answered by *REPLY*, so no test dials."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected phone-call request")))
         (nodecode-phone-call::*clock* (lambda () 1000000)))
     (nodecode-phone-call::reset-store)
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun phone-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun phone-reply (text &optional (status 200))
  "Answer every request with TEXT and STATUS until the next reply."
  (setf *reply* (lambda (method url args)
                  (declare (ignore method url args))
                  (values text status))))

(defun phone-tool (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "phone-call" name arguments)))

(defun phone-prepare (number &key facts record)
  "phone_prepare for NUMBER with a goal, a message and optional FACTS."
  (phone-tool "phone_prepare"
              (format nil "{\"callee_name\": \"Bella Napoli\", \"number\": ~s, \
\"number_source\": \"user\", \"goal\": \"book a table for four on Friday at 8\", \
\"message\": \"I would like a table for four on Friday at eight.\"~@[~a~]~@[~a~]}"
                      number
                      (and facts (format nil ", \"facts_to_share\": ~a" facts))
                      (and record ", \"record\": true"))))

(defun phone-approve (id)
  "(/phone-call approve ID) as the operator types it."
  (nodecode-phone-call::run-command (format nil "approve ~a" id) nil))

(defun phone-command (line)
  "(/phone-call LINE) as the operator types it."
  (nodecode-phone-call::run-command line nil))

;;; --- lifecycle --------------------------------------------------------------------

(define-test-slice "phone-call" "PHONE-CALL-CELL-" :start nodecode-phone-call:start-cell)

(define-cell-lifecycle-tests "phone-call"
  (:fixture with-phone-wire)
  (:config "allowed_numbers" (vector "+14155550123"))
  (:help :phone-call)
  (:command "phone-call")
  (:running
   (is (nle::find-tool "phone_prepare") "phone_prepare is registered")
   (is (nle::find-tool "phone_call") "and phone_call")
   (is (nle::find-tool "phone_status") "and phone_status")
   (is (nle::find-tool "phone_transcript") "and phone_transcript, the original's four")
   (is (= 4 (count-if (lambda (entry) (eql 0 (search "phone_" (first entry)))) nle::*tools*)) "four of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "phone_prepare")) "and stop takes every one back"))
  (:refused ("daily_call_limit" 0)
            ("max_duration_s" 10)))

;;; --- the card, and the numbers it will and will not dial ----------------------------

(deftest phone-call-cell-the-card-says-what-will-happen (with-phone-wire)
  (with-cell-stop ((apply #'phone-call-start *phone-config*))
    (let ((text (phone-prepare "+14155550123" :record t
                               :facts "{\"party_size\": \"4\", \"day\": \"Friday\"}")))
      (is (search "Call brief c1" text) "one card, with its id")
      (is (search "Hi, this is an AI assistant calling on behalf of Ada" text) "the disclosure")
      (is (search "This call may be recorded" text) "and the recording notice")
      (is (search "I would like a table for four" text) "the words the callee will hear")
      (is (search "party_size: 4" text) "and what the call may share")
      (is (search "/phone-call approve c1" text) "and the one command that dials")
      (is (null (phone-asked)) "preparing reaches nothing"))))

(deftest phone-call-cell-a-number-off-the-allowlist-is-refused (with-phone-wire)
  (with-cell-stop ((apply #'phone-call-start *phone-config*))
    (let ((text (phone-prepare "+14155550999")))
      (is (search "ERROR: " text) "the model reads a refusal")
      (is (search "not on the allowlist" text) "and why: the number is not on it")))
  (with-cell-stop ((phone-call-start "user_name" "Ada"))
    (let ((text (phone-prepare "+14155550123")))
      (is (search "dialing is disabled" text) "an unset allowlist allows nothing"))))

(deftest phone-call-cell-an-emergency-number-is-never-dialed (with-phone-wire)
  (with-cell-stop ((phone-call-start "allowed_numbers" (vector "+1911")))
    (let ((text (phone-prepare "911")))
      (is (search "ERROR: " text) "911 is refused")
      (is (search "emergency" text) "as an emergency number"))))

(deftest phone-call-cell-a-premium-number-is-never-dialed (with-phone-wire)
  (with-cell-stop ((phone-call-start "allowed_numbers" (vector "+19005550123")))
    (let ((text (phone-prepare "+19005550123")))
      (is (search "premium" text) "1-900 is premium-rate"))))

;;; --- approval, then one dial ---------------------------------------------------------

(deftest phone-call-cell-no-call-without-an-approval (with-phone-wire)
  (with-cell-stop ((apply #'phone-call-start *phone-config*))
    (phone-prepare "+14155550123")
    (let ((text (phone-tool "phone_call" "{\"call_id\": \"c1\"}")))
      (is (search "ERROR: not approved" text) "the approval is the operator's")
      (is (search "/phone-call approve c1" text) "and the refusal names the command"))
    (is (null (phone-asked)) "nothing was dialed")))

(deftest phone-call-cell-an-approval-dials-once-with-the-disclosure (with-phone-wire)
  (with-cell-stop ((apply #'phone-call-start *phone-config*))
    (phone-prepare "+14155550123")
    (phone-reply "{\"sid\": \"CA123\", \"status\": \"queued\"}")
    (let ((text (phone-approve "c1")))
      (is (search "Ringing" text) "the call is placed")
      (is (search "CA123" text) "with Twilio's sid"))
    (let ((request (first (phone-asked))))
      (is (eq :post (first request)) "one POST")
      (is (search "/Accounts/ACtest/Calls.json" (second request)) "to the Calls resource")
      (let ((body (getf (cddr request) :content)))
        (is (search "To=%2B14155550123" body) "to the approved number")
        (is (search "From=%2B14155550100" body) "from the caller id")
        (is (search "AI%20assistant%20calling%20on%20behalf%20of%20Ada" body) "and the AI disclosure is in the document Twilio speaks")
        (is (search "Say" body) "as a SAY verb")))
    (let ((second (phone-tool "phone_call" "{\"call_id\": \"c1\"}")))
      (is (search "ERROR: not approved" second) "a second call on the same approval is refused")
      (is (search "dialing" second) "and says the call is already on the line"))))

(deftest phone-call-cell-without-a-twilio-account-nothing-is-dialed (with-phone-wire)
  (with-cell-stop ((phone-call-start "allowed_numbers" (vector "+14155550123")))
    (phone-prepare "+14155550123")
    (let ((text (phone-approve "c1")))
      (is (search "Dry run" text) "the call is a dry run")
      (is (search "no Twilio account is set" text) "and says what is missing"))
    (is (null (phone-asked)) "no request left the machine")
    (let ((status (phone-command "status c1")))
      (is (search "completed" status) "the call is over")
      (is (search "dry_run" status) "as a dry run"))))

;;; --- consent ---------------------------------------------------------------------------

(deftest phone-call-cell-a-card-number-waits-for-consent (with-phone-wire)
  (with-cell-stop ((apply #'phone-call-start *phone-config*))
    (let ((card (phone-prepare "+14155550123"
                               :facts "{\"card_number\": \"4111 1111 1111 1111\", \"party_size\": \"4\"}")))
      (is (search "card_number (card, no consent)" card) "the card is withheld on the card")
      (is (null (search "4111 1111 1111 1111" card)) "and its value is not shown"))
    (let ((status (phone-command "status c1")))
      (is (search "Withheld: card_number" status) "the status agrees"))
    (is (search "consent given: card" (phone-command "consent c1 card")) "the operator grants it")
    (let ((status (phone-command "status c1")))
      (is (search "card_number: 4111 1111 1111 1111" status) "and only then is it shared"))))

(deftest phone-call-cell-a-cvv-is-never-shared (with-phone-wire)
  (with-cell-stop ((apply #'phone-call-start *phone-config*))
    (let ((text (phone-prepare "+14155550123" :facts "{\"cvv\": \"123\"}")))
      (is (search "ERROR: " text) "a cvv is refused at prepare")
      (is (search "never shared" text) "with the rule that refuses it"))))

;;; --- quiet hours, the transcript and the log ---------------------------------------------

(deftest phone-call-cell-quiet-hours-hold-a-call (with-phone-wire)
  (let ((nodecode-phone-call::*clock*
          (lambda () (encode-universal-time 0 0 3 1 1 2026 0))))
    (with-cell-stop ((phone-call-start "allowed_numbers" (vector "+14155550123")
                                       "enforce_quiet_hours" t "callee_utc_offset" 0))
      (let ((text (phone-prepare "+14155550123")))
        (is (search "quiet hours" text) "03:00 for the callee is outside the window")
        (is (search "override_quiet_hours" text) "and names the way past it"))
      (let ((text (phone-tool "phone_prepare" "{\"callee_name\": \"Bella Napoli\", \"number\": \"+14155550123\", \"goal\": \"book a table\", \"override_quiet_hours\": true}")))
        (is (search "Call brief" text) "the operator's override lets it through")))))

(deftest phone-call-cell-the-transcript-and-log-answer (with-phone-wire)
  (with-cell-stop ((phone-call-start "allowed_numbers" (vector "+14155550123")
                                     "user_name" "Ada"))
    (phone-prepare "+14155550123")
    (phone-approve "c1")
    (let ((transcript (phone-tool "phone_transcript" "{\"call_id\": \"c1\"}")))
      (is (search "agent: Hi, this is an AI assistant" transcript) "what the call said")
      (is (search "agent: I would like a table for four" transcript) "and the message"))
    (let ((log (phone-command "log")))
      (is (search "c1 · completed" log) "the log lists the call at its state"))
    (is (search "there is no call c9" (phone-command "status c9")) "an unknown id refuses")))
