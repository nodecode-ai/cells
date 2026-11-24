;;;; cell-test.lisp --- the carbonvoice cell against a stubbed API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Carbon Voice request is a stubbed NLK:HTTP and the token a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches Carbon Voice and no secret is read
;;;; from the environment. The tools are called the way a model's call reaches
;;;; them — through the real dispatch, contract and all — so the schema the
;;;; provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "carbonvoice" "CARBONVOICE-CELL-" :start nodecode-carbonvoice:start-cell
  :defaults (("pat_env" "CARBONVOICE_PAT") ("base_url" "https://api.carbonvoice.app")))

;;; --- fixtures --------------------------------------------------------------------

(defparameter *requests* nil
  "Every request the stub answered, oldest first, as (METHOD URL BODY).")

(defmacro with-carbonvoice (&body body)
  "BODY with the token answering, and the request log emptied."
  `(let ((saved *requests*))
     (setf *requests* '())
     (nlk:with-cleanup ((setf *requests* saved))
       (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "CARBONVOICE_PAT") "cv-pat-secret"))
         ,@body))))

(defun stub-answer (url answers)
  "The (TEXT STATUS) the table ANSWERS gives URL, by the first needle it holds."
  (loop for (needle text status) in answers
        when (search needle url) return (values text status)
        finally (return (values "" 404))))

(defmacro with-carbonvoice-wire ((&key answers) &body body)
  "BODY with NLK:HTTP answering ANSWERS — a list of (NEEDLE TEXT STATUS),
the first whose NEEDLE is in the url answering — and every exchange
logged."
  `(with-stubbed-fdefinition (nlk:http (method url &rest options)
                              (push (list method url (getf options :content)) *requests*)
                              (stub-answer url ,answers))
     ,@body))

(defun request-to (needle)
  "The oldest logged request whose url holds NEEDLE."
  (find-if (lambda (request) (search needle (second request))) (reverse *requests*)))

(defun tool-answer (id name arguments)
  "The tool's answer to ARGUMENTS, as the model reads it: the real dispatch."
  (nle::message-content (execute-wire-call id name arguments)))

(defun tool-object (id name arguments)
  (nlk:decode-json (tool-answer id name arguments)))

(defun body-field (request name)
  "NAME's value in a logged request's JSON body, or NIL."
  (let ((body (third request)))
    (and body (nlk:json-value (nlk:decode-json body) :string name))))

;;; --- what the API answers --------------------------------------------------------------

(defparameter +whoami+
  "{\"user\": {\"user_guid\": \"agent-1\", \"created_by\": \"owner-1\", \"first_name\": \"Node\", \"last_name\": \"Code\"}}")

(defparameter +recent+
  (format nil "[~a,~a,~a]"
          "{\"message_id\": \"m-1\", \"channel_ids\": [\"ch-1\"], \"creator_id\": \"agent-1\", \"created_at\": \"2026-01-02T03:04:05Z\", \"text_models\": [{\"type\": \"transcript\", \"timecodes\": [{\"t\": \"hello\"}, {\"t\": \"there\"}]}]}"
          "{\"message_id\": \"m-2\", \"channel_ids\": [\"ch-1\"], \"creator_id\": \"person-1\", \"created_at\": \"2026-01-02T03:05:05Z\", \"parent_message_id\": \"m-1\", \"text_models\": [{\"type\": \"transcript\", \"timecodes\": [{\"t\": \"can\"}, {\"t\": \"you\"}, {\"t\": \"deploy\"}]}]}"
          "{\"message_id\": \"m-3\", \"channel_ids\": [\"ch-2\"], \"creator_id\": \"person-2\", \"created_at\": \"2026-01-02T03:06:05Z\", \"text_models\": []}"))

(defparameter +inbox+
  (list (list "/whoami" +whoami+ 200)
        (list "/v3/messages/recent" +recent+ 200)))

(defparameter +nothing+
  (list))

(defparameter +a-send+
  (list (list "/v5/messages/text" "{\"id\": \"m-9\"}" 201)))

(defparameter +two-messages+
  (list (list "/v5/messages/m-1"
              "{\"id\": \"m-1\", \"conversation_id\": \"ch-1\", \"creator_id\": \"person-1\", \"transcript\": \"ship it friday\"}"
              200)
        (list "/v5/messages/m-2"
              "{\"id\": \"m-2\", \"conversation_id\": \"ch-1\", \"transcript\": \"\"}"
              200)))

(defparameter +a-channel+
  (list (list "/channel/ch-1"
              "{\"name\": \"Launch\", \"type\": \"namedConversation\", \"json_collaborators\": [{\"user_guid\": \"person-1\", \"first_name\": \"Ada\", \"last_name\": \"L\"}, {\"user_guid\": \"person-2\", \"display_name\": \"Bo\"}]}"
              200)))

(defparameter +expired+
  (list (list "/whoami" "{\"message\": \"token expired\"}" 401)
        (list "/v3/messages/recent" "{\"message\": \"token expired\"}" 401)))

;;; --- the lifecycle, from the one declaration --------------------------------------

(define-cell-lifecycle-tests "carbonvoice"
  (:config)
  (:help :carbonvoice)
  (:command "carbonvoice")
  (:fixture with-carbonvoice)
  (:refused ("since_minutes" 0) ("limit" 0) ("timeout" 0) ("base_url" 5)))

(deftest carbonvoice-cell-without-a-token-refuses ()
  ;; The one thing no verb can work without: the section names a variable, and
  ;; a variable that is unset stops the start before anything installs.
  (with-stubbed-fdefinition (nle::credential-env (name)
                              (declare (ignore name))
                              nil)
    (let ((condition (signals-error nlk:config-refusal (carbonvoice-start))))
      (is (search "CARBONVOICE_PAT" (princ-to-string condition)) "it names the variable")
      (is (null (nle::find-tool "carbonvoice_recent")) "and installs nothing"))))

;;; --- carbonvoice_recent ------------------------------------------------------------

(deftest carbonvoice-cell-the-tool-reads-what-came-in (with-carbonvoice)
  (with-cell-stop ((carbonvoice-start))
    (with-carbonvoice-wire (:answers +inbox+)
      (let* ((answer (tool-object "cv-1" "carbonvoice_recent"
                                  "{\"since_minutes\": 30, \"limit\": 5}"))
             (messages (nlk:json-array answer "messages")))
        (is (= 2 (or (nlk:json-value answer :integer "count") -1)) "the agent's own is left out")
        (is (= 2 (length messages)))
        (is (equal "m-2" (nlk:json-value (aref messages 0) :string "id")))
        (is (equal "can you deploy" (nlk:json-value (aref messages 0) :string "transcript")) "the v3 timecodes")
        (is (equal "m-1" (nlk:json-value (aref messages 0) :string "reply_to")) "the thread it answers")
        (is (equal "ch-1" (nlk:json-value (aref messages 0) :string "conversation_id")))
        (is (eq t (nlk:json-value (aref messages 1) :boolean "pending")) "not transcribed yet")
        (let* ((request (request-to "/v3/messages/recent"))
               (body (nlk:decode-json (third request))))
          (is (eq :post (first request)) "the recent endpoint is a POST")
          (is (eql 5 (nlk:json-value body :integer "limit")) "the call's own limit rides it")
          (is (eq t (nlk:json-value body :boolean "use_last_updated")) "filtered by updated_at")
          (is (search "T" (or (body-field request "date") "")) "and since the window's own ISO stamp"))))))

(deftest carbonvoice-cell-recent-reads-one-conversation (with-carbonvoice)
  (with-cell-stop ((carbonvoice-start))
    (with-carbonvoice-wire (:answers +inbox+)
      (let ((answer (tool-object "cv-2" "carbonvoice_recent" "{\"conversation\": \"ch-2\"}")))
        (is (= 1 (or (nlk:json-value answer :integer "count") -1)))
        (is (equal "m-3" (nlk:json-value (aref (nlk:json-array answer "messages") 0) :string "id")))))))

;;; --- carbonvoice_send ----------------------------------------------------------------

(deftest carbonvoice-cell-the-tool-sends-a-threaded-reply (with-carbonvoice)
  (with-cell-stop ((carbonvoice-start))
    (with-carbonvoice-wire (:answers +a-send+)
      (let ((answer (tool-object "cv-3" "carbonvoice_send"
                                 "{\"conversation_id\": \"ch-1\", \"text\": \"on it\", \"reply_to\": \"m-2\"}")))
        (is (eq t (nlk:json-value answer :boolean "sent")))
        (is (equal "m-9" (nlk:json-value answer :string "message_id")))
        (let ((request (request-to "/v5/messages/text")))
          (is (eq :post (first request)))
          (is (equal "ch-1" (body-field request "conversation_id")))
          (is (equal "on it" (body-field request "transcript")))
          (is (equal "m-2" (body-field request "reply_to_message_id")) "threaded under the message it answers")
          (is (= 36 (length (or (body-field request "idempotency_key") ""))) "one send carries one key"))))))

(deftest carbonvoice-cell-a-send-without-a-conversation-is-refused-before-the-body (with-carbonvoice)
  (with-cell-stop ((carbonvoice-start))
    (with-carbonvoice-wire (:answers +nothing+)
      (let ((answer (tool-answer "cv-4" "carbonvoice_send" "{\"text\": \"on it\"}")))
        (is (search "preflight" answer) "the contract refuses it before the body")
        (is (search "conversation_id" answer))
        (is (null *requests*) "and nothing left the machine")))))

(deftest carbonvoice-cell-a-send-past-the-ceiling-is-refused (with-carbonvoice)
  (with-cell-stop ((carbonvoice-start))
    (with-carbonvoice-wire (:answers +nothing+)
      (let ((answer (tool-answer "cv-5" "carbonvoice_send"
                                  (nlk:encode-json-object
                                   (nlk:json-object "conversation_id" "ch-1"
                                                    "text" (make-string 8001 :initial-element #\a))))))
        (is (search "8000" answer) (subseq answer 0 (min 160 (length answer))))
        (is (null *requests*) "and nothing was sent")))))

;;; --- carbonvoice_message --------------------------------------------------------------

(deftest carbonvoice-cell-one-message-answers-its-transcript (with-carbonvoice)
  (with-cell-stop ((carbonvoice-start))
    (with-carbonvoice-wire (:answers +two-messages+)
      (let ((answer (tool-object "cv-6" "carbonvoice_message" "{\"message_id\": \"m-1\"}")))
        (is (equal "ship it friday" (nlk:json-value answer :string "transcript")))
        (is (eq nil (nlk:json-value answer :boolean "pending")))
        (is (eq :get (first (request-to "/v5/messages/m-1")))))
      (let ((pending (tool-object "cv-7" "carbonvoice_message" "{\"message_id\": \"m-2\"}")))
        (is (eq t (nlk:json-value pending :boolean "pending")) "an untranscribed memo says so")
        (is (search "transcribing" (or (nlk:json-value pending :string "note") "")))))))

;;; --- carbonvoice_channel -------------------------------------------------------------

(deftest carbonvoice-cell-a-channel-names-its-people (with-carbonvoice)
  (with-cell-stop ((carbonvoice-start))
    (with-carbonvoice-wire (:answers +a-channel+)
      (let* ((answer (tool-object "cv-8" "carbonvoice_channel" "{\"conversation_id\": \"ch-1\"}"))
             (people (nlk:json-array answer "people")))
        (is (equal "Launch" (nlk:json-value answer :string "name")))
        (is (equal "namedConversation" (nlk:json-value answer :string "kind")))
        (is (= 2 (length people)))
        (is (equal "Ada L" (nlk:json-value (aref people 0) :string "name")))
        (is (equal "Bo" (nlk:json-value (aref people 1) :string "name")))))))

;;; --- API errors ----------------------------------------------------------------------

(deftest carbonvoice-cell-a-refused-request-says-why (with-carbonvoice)
  (with-cell-stop ((carbonvoice-start))
    (with-carbonvoice-wire (:answers +expired+)
      (let ((answer (tool-answer "cv-9" "carbonvoice_recent" "{}")))
        (is (search "ERROR:" answer) "a refused call is a failed result, not a success")
        (is (search "401" answer) (subseq answer 0 (min 160 (length answer))))
        (is (search "token expired" answer) "in Carbon Voice's own words")))))

;;; --- the command -----------------------------------------------------------------------

(deftest carbonvoice-cell-the-command-answers-through-stubbed-io (with-carbonvoice)
  (with-cell-stop ((carbonvoice-start))
    (with-carbonvoice-wire (:answers +inbox+)
      (let ((line (cell-entry "nodecode-carbonvoice" "carbonvoice" "")))
        (is (search "CARBONVOICE_PAT" line) "status names the variable the token is read from")
        (is (search "api.carbonvoice.app" line) "and the endpoint")
        (is (search "agent-1" line) (subseq line 0 (min 200 (length line)))))
      (let ((lines (cell-entry "nodecode-carbonvoice" "carbonvoice" "recent 30")))
        (is (search "can you deploy" lines) "recent lists what came in")
        (is (search "still transcribing" lines) "and says which message has no transcript yet"))
      (is (search "Usage" (cell-entry "nodecode-carbonvoice" "carbonvoice" "wobble")) "it says the verbs"))))

;;; --- a verb with nothing started ---------------------------------------------------------

(deftest carbonvoice-cell-a-verb-refuses-while-not-running (with-carbonvoice)
  (setf nodecode-carbonvoice::*carbonvoice* nil)
  (with-carbonvoice-wire (:answers +nothing+)
    (let ((answer (carbonvoice:recent)))
      (is (search "not running" answer) "the refusal says the cell is not on")
      (is (null *requests*) "and nothing left the machine"))))
