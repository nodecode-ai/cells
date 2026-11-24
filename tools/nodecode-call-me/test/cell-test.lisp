;;;; cell-test.lisp --- the call-me cell against a stubbed service.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP and every session file a scratch one
;;;; under the temporary directory: nothing reaches /call-me, a real phone or
;;;; the operator's home. The tools are called the way a model's call reaches
;;;; them — through the real dispatch, contract and all — so the schema the
;;;; provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "call-me" "CALL-ME-CELL-" :start nodecode-call-me:start-cell
  :defaults (("allowed_users" (vector "5550001111"))
             ("names" "5550001111=Sam")))

;;; --- fixtures --------------------------------------------------------------------

(defparameter *requests* nil
  "Every request the stub answered, oldest first, as (URL CONTENT).")

(defun scratch-session-file ()
  "A session file of this test's own, under the temporary directory."
  (merge-pathnames (format nil "call-me-test-~a.json" (random 1000000))
                   (uiop:temporary-directory)))

(defmacro with-call-me (&body body)
  "BODY with a session file of its own that no earlier test wrote, and the
request log emptied."
  `(let ((saved *requests*)
         (file (scratch-session-file)))
     (setf *requests* '())
     (nlk:with-cleanup ((setf *requests* saved)
                        (ignore-errors (delete-file file)))
       (with-stubbed-fdefinition (nodecode-call-me::state-file () file)
         ,@body))))

(defmacro with-call-me-wire ((&key
                                (sesion "{\"session_token\":\"tok-1\",\"session_number\":\"7000000001\"}")
                                (text "{\"message_id\":\"msg-1\",\"delivered\":true}")
                                (call "{\"status\":\"completed\",\"transcript\":\"Yes, go ahead\"}")
                                (status 200))
                              &body body)
  "BODY with NLK:HTTP answering by path: a session, a text or a call."
  `(with-stubbed-fdefinition (nlk:http (method target &rest options)
                              (push (list target (getf options :content)) *requests*)
                              (values (cond ((search "/sessions" target) ,sesion)
                                            ((search "/messages" target) ,text)
                                            ((search "/calls" target) ,call)
                                            (t (progn method "{}")))
                                      ,status))
     ,@body))

(defun tool-answer (id name arguments)
  "The tool's answer to ARGUMENTS, as the model reads it: the real dispatch."
  (nle::message-content (execute-wire-call id name arguments)))

(defun answer-object (id name arguments)
  (nlk:decode-json (tool-answer id name arguments)))

(defun request-body (request field)
  "FIELD's value in REQUEST's JSON body, or NIL."
  (let ((object (ignore-errors (nlk:decode-json (second request)))))
    (and (hash-table-p object) (nlk:json-value object :string field))))

(defun requests-to (path)
  (remove-if-not (lambda (request) (search path (first request) :from-end t)) *requests*))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "call-me"
  (:config)
  (:help :call-me)
  (:command "callme")
  (:fixture with-call-me)
  (:refused ("timeout" 0)))

;;; --- the session -------------------------------------------------------------------

(deftest call-me-cell-mints-one-session-and-keeps-it ()
  ;; The agent's number is the session's, so it is minted once and read off
  ;; the file afterwards: two texts are one session and two messages.
  (with-call-me
    (with-cell-stop ((call-me-start))
      (with-call-me-wire ()
        (tool-answer "c1" "callme_text" "{\"message\":\"one\"}")
        (tool-answer "c2" "callme_text" "{\"message\":\"two\"}")
        (is (= 1 (length (requests-to "/sessions"))) "one session for the whole run")
        (is (= 2 (length (requests-to "/messages"))) "and one message each")
        (is (equal "Nodecode" (request-body (first (requests-to "/sessions")) "label")) "the session carries the section's label")))))

;;; --- callme_text ------------------------------------------------------------------

(deftest call-me-cell-texts-an-allowed-number ()
  (with-call-me
    (with-cell-stop ((call-me-start))
      (with-call-me-wire ()
        (let ((object (answer-object "t1" "callme_text" "{\"message\":\"the build is green\"}")))
          (is (eq t (nlk:json-value object :boolean "sent")) "the text went")
          (is (equal "msg-1" (nlk:json-value object :string "message_id")) "and has the service's id")
          (is (equal "Sam" (nlk:json-value object :string "to")) "the answer names who it reached"))
        (let ((sent (first (requests-to "/messages"))))
          (is (search "callmemcp.com/aiphone/messages" (first sent)) "the service's one host")
          (is (equal "5550001111" (request-body sent "to")) "the allowed number, bare digits")
          (is (equal "tok-1" (request-body sent "session_token")) "the session rides the body")
          (is (equal "the build is green" (request-body sent "body")) "and the message does"))))))

(deftest call-me-cell-a-name-resolves-and-a-stranger-is-refused ()
  (with-call-me
    (with-cell-stop ((call-me-start))
      (with-call-me-wire ()
        (tool-answer "t2" "callme_text" "{\"message\":\"hi\",\"to\":\"Sam\"}")
        (is (equal "5550001111" (request-body (first (requests-to "/messages")) "to")) "a name reaches its number")
        (let ((before (length *requests*)))
          (let ((answer (tool-answer "t3" "callme_text" "{\"message\":\"hi\",\"to\":\"5559990000\"}")))
            (is (search "ERROR:" answer) "a refusal is a failed call")
            (is (search "not an allowed" answer) "in the cell's own words"))
          (is (= before (length *requests*)) "and nothing left the machine"))))))

;;; --- callme_call --------------------------------------------------------------------

(deftest call-me-cell-rings-and-reads-back-the-answer ()
  (with-call-me
    (with-cell-stop ((call-me-start "call_wait" 45))
      (with-call-me-wire ()
        (let ((object (answer-object "r1" "callme_call" "{\"text\":\"Deploy now?\"}")))
          (is (equal "completed" (nlk:json-value object :string "status")) "the call finished")
          (is (equal "Yes, go ahead" (nlk:json-value object :string "transcript")) "and the spoken answer came back as text"))
        (let ((call (first (requests-to "/calls"))))
          (is (equal "Deploy now?" (request-body call "text")) "the text is what gets spoken")
          (is (= 45 (nlk:json-value (nlk:decode-json (second call)) :integer "timeout_s")) "the section's call_wait is the wait"))))))

(deftest call-me-cell-a-missed-call-says-so ()
  (with-call-me
    (with-cell-stop ((call-me-start))
      (with-call-me-wire (:call "{\"status\":\"missed\",\"transcript\":\"\"}")
        (let ((object (answer-object "r2" "callme_call" "{\"text\":\"Deploy now?\",\"wait_seconds\":30}")))
          (is (equal "missed" (nlk:json-value object :string "status")) "nobody answered")
          (is (search "Nobody answered" (nlk:json-value object :string "hint")) "the model is told to text instead"))))))

;;; --- a required setting ---------------------------------------------------------------

(deftest call-me-cell-without-a-number-refuses ()
  ;; The one thing no verb can work without: the section lists nobody, so the
  ;; start refuses by name and installs nothing.
  (with-call-me
    (let ((condition (signals-error nlk:config-refusal (call-me-start "allowed_users" (vector)))))
      (is (search "allowed_users" (princ-to-string condition)) "the refusal names the key")
      (is (null (nle::find-tool "callme_call")) "and installs nothing")
      (is (null (nle::find-tool "callme_text")) "of either tool"))))

;;; --- the command ----------------------------------------------------------------------

(deftest call-me-cell-the-command-answers-through-stubbed-io ()
  (with-call-me
    (with-cell-stop ((call-me-start))
      (let ((line (cell-entry "nodecode-call-me" "callme" "")))
        (is (search "Sam" line) "status names who is reachable")
        (is (search "no number yet" line) "and says the session is not minted")
        (is (null *requests*) "status sends nothing"))
      (with-call-me-wire ()
        (is (search "Texted Sam" (cell-entry "nodecode-call-me" "callme" "text Sam the build is green")) "the command texts too")
        (is (equal "the build is green" (request-body (first (requests-to "/messages")) "body")) "everything after the name is the message"))
      (is (search "Usage" (cell-entry "nodecode-call-me" "callme" "wobble")) "an unknown verb says what the verbs are"))))

;;; --- a verb with nothing started --------------------------------------------------------

(deftest call-me-cell-a-verb-refuses-while-not-running ()
  (with-call-me
    (setf nodecode-call-me::*call-me* nil)
    (with-call-me-wire ()
      (let ((answer (nodecode-call-me:make-call (nodecode-call-me::decode-spoken-call (nlk:make-json-object "text" "hello")))))
        (is (search "not running" answer) "the refusal says the cell is not on")
        (is (null *requests*) "and nothing left the machine")))))
