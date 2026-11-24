;;;; cell-test.lisp --- the cell's wiring, and every exchange it makes, stubbed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The run has no network, so *REQUEST* -- the one function the cell
;;;; exchanges HTTP through -- is bound to a table of canned answers, and what
;;;; the cell asked is read back off *ASKED*: the method, the URL, the JSON
;;;; body and the headers. Nothing above that seam is stubbed: the tools, the
;;;; contracts, the section's settings and the command are the shipped ones.

(in-package #:nodecode.test)

(define-test-slice "pocket" "POCKET-CELL-" :start nodecode-pocket:start-cell
  :defaults (("api_token_env" "POCKET_TEST_TOKEN")))

;;; --- the stub ---------------------------------------------------------------

(defparameter *token-env* "POCKET_TEST_TOKEN"
  "The variable the section's default names in tests, never a real key's name.")

(defparameter *asked* nil
  "Every exchange the stub took, newest first: (METHOD URL BODY HEADERS).")

(defun token-here ()
  "Put a synthetic key where the section's default says the real one lives."
  (setf (uiop:getenv *token-env*) "ntf_test_key"))

(defun no-token ()
  "Take it away again, so a call has to refuse."
  (setf (uiop:getenv *token-env*) ""))

(defun stub-request (table)
  "A *REQUEST* stand-in: TABLE is a list of (NEEDLE BODY STATUS), and the
first whose NEEDLE is in the URL answers it."
  (lambda (method url body headers timeout)
    (declare (ignore timeout))
    (push (list method url body headers) *asked*)
    (let ((row (find-if (lambda (row) (search (first row) url)) table)))
      (unless row
        (error "the test stubs no answer for ~a ~a" method url))
      (values (second row) (third row) nil))))

(defmacro with-stub (rows &body body)
  "BODY with every exchange answered from ROWS, a form answering a list of
(NEEDLE JSON STATUS), and *ASKED* recording what was asked."
  `(let ((*asked* nil)
         (nodecode-pocket::*request* (stub-request ,rows)))
     ,@body))

(defun asked (n)
  "The N-th exchange the stub took, 0 the newest, or NIL."
  (nth n *asked*))

(defun asked-method (&optional (n 0)) (first (asked n)))
(defun asked-url (&optional (n 0)) (second (asked n)))
(defun asked-body (&optional (n 0)) (third (asked n)))

(defun asked-header (name &optional (n 0))
  "The header NAME the N-th exchange carried, case-insensitively."
  (cdr (assoc name (fourth (asked n)) :test #'string-equal)))

(defun asked-json (&optional (n 0))
  "The N-th exchange's body, decoded as the wire decodes a body."
  (nlk:decode-json (asked-body n)))

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "pocket"
  (:help :pocket)
  (:command "pocket")
  (:config "api_token_env" "POCKET_TEST_TOKEN")
  (:running
   (is (string= "https://notify.belweave.ai/api/agent" (nodecode-pocket::setting :base-url)) "the fixed Notify agent API is the default origin") (is (string= "Nodecode" (nodecode-pocket::setting :project)))
   (is (= 15 (nodecode-pocket::setting :timeout-seconds))))
  (:refused ("timeout_seconds" 0))
  (:idle pocket:pocket-error (pocket:notify "hello")))

;;; --- sending -----------------------------------------------------------------

(deftest pocket-cell-sends-an-update ()
  (token-here)
  (with-cell-stop ((pocket-start "project" "Release"))
    (with-stub (list (list "/notifications" "{\"notification\":{\"id\":\"ntf_1\",\"title\":\"Release\"},\"accepted\":1}" 201))
      (let ((receipt (pocket:notify "the build is green" :title "Release")))
        (is (search "ntf_1" receipt) "the receipt names what Notify took")
        (is (equal :post (asked-method)))
        (is (string= "https://notify.belweave.ai/api/agent/notifications" (asked-url)))
        (is (string= "Bearer ntf_test_key" (asked-header "authorization")) "the key rides the header, never the body") (is (not (search "ntf_test_key" (asked-body))) "and never the message")
        (is (string= "Release" (nlk:json-value (asked-json) :text "title")))
        (is (string= "the build is green" (nlk:json-value (asked-json) :text "body")))
        (is (string= "Release" (nlk:json-value (asked-json) :text "project")) "the section's project groups the update") (is (search "ntf_1" (first nodecode-pocket::*sent*)) "the receipt is kept for /pocket"))))) 
(deftest pocket-cell-the-update-is-the-shape-notify-takes ()
  (token-here)
  (with-cell-stop ((pocket-start "project" "Nodecode"))
    (with-stub (list (list "/notifications" "{\"notification\":{\"id\":\"ntf_2\"},\"accepted\":2}" 201))
      (pocket:notify "one line" :title "Build" :summary "green" :url "https://example.invalid/x"
                     :idempotency-key "release-2.4.1")
      (is (string= "release-2.4.1" (asked-header "idempotency-key")) "a retry of one intended update reuses its key") (is (string= "application/json" (asked-header "content-type")))
      (is (string= "https://example.invalid/x" (nlk:json-value (asked-json) :text "url")))
      (is (string= "green" (nlk:json-value (asked-json) :text "summary")))
      (is (null (nlk:json-value (asked-json) :text "deviceIds")) "no member the declaration does not carry is invented")))) 
(deftest pocket-cell-a-body-longer-than-notify-takes-is-refused ()
  (token-here)
  (with-cell-stop ((pocket-start))
    (let ((long (make-string 8001 :initial-element #\a)))
      (is (search "8000" (refusal-text pocket:pocket-error (pocket:notify long))))
      (is (search "80" (refusal-text pocket:pocket-error
                                      (pocket:notify "hi" :title (make-string 81
                                                                           :initial-element #\b))))))))

(deftest pocket-cell-a-repeated-key-in-the-text-is-refused ()
  (token-here)
  (with-cell-stop ((pocket-start))
    (with-stub '()
      (is (search "repeats the Notify key"
                  (refusal-text pocket:pocket-error
                                (pocket:notify "the key is ntf_test_key"))))
      (is (null *asked*) "nothing went out"))))

;;; --- asking ------------------------------------------------------------------

(deftest pocket-cell-asks-and-reads-the-answer ()
  (token-here)
  (with-cell-stop ((pocket-start))
    (with-stub (list (list "/wait" "{\"interaction\":{\"id\":\"int_1\",\"status\":\"approved\",\"response\":\"approve\"},\"timedOut\":false}" 200)
                      (list "/interactions" "{\"interaction\":{\"id\":\"int_1\",\"status\":\"pending\"},\"accepted\":1}" 201))
      (let ((answer (pocket:ask "Run the release?" :title "Release" :kind "approval" :wait 20)))
        (is (search "approved" answer) "the decision comes back as the call's answer")
        (is (search "int_1" answer) "and names the interaction it decided")
        (is (equal :post (asked-method 1)) "the question is created first")
        (is (string= "https://notify.belweave.ai/api/agent/interactions" (asked-url 1)))
        (is (string= "approval" (nlk:json-value (asked-json 1) :text "kind")))
        (is (string= "notification" (nlk:json-value (asked-json 1) :text "presentation")))
        (is (= 900 (nlk:json-value (asked-json 1) :integer "expiresInSeconds")))
        (is (equal :get (asked-method)))
        (is (search "int_1/wait?timeout=20" (asked-url)) "one wait, inside Notify's own ceiling") (is (search "int_1" (first nodecode-pocket::*sent*)) "the question is kept for /pocket"))))) 
(deftest pocket-cell-a-slow-question-is-polled-rather-than-held ()
  (token-here)
  (with-cell-stop ((pocket-start))
    (with-stub (list (list "/interactions/int_2" "{\"interaction\":{\"id\":\"int_2\",\"status\":\"replied\",\"response\":\"ship it after lunch\"}}" 200)
                      (list "/interactions" "{\"interaction\":{\"id\":\"int_2\",\"status\":\"pending\"}}" 201))
      (let ((pending (pocket:ask "When should this ship?" :title "Ship" :kind "reply")))
        (is (search "still pending" pending))
        (is (search "int_2" pending) "the pending answer names the id to poll")
        (is (string= "reply" (nlk:json-value (asked-json) :text "kind")))
        (let ((settled (pocket:answer "int_2")))
          (is (search "replied" settled))
          (is (search "ship it after lunch" settled) "the person's words come back whole, as task input")))))) 
(deftest pocket-cell-an-id-that-is-not-one-is-refused ()
  (token-here)
  (with-cell-stop ((pocket-start))
    (with-stub '()
      (is (search "not a Notify interaction id"
                  (refusal-text pocket:pocket-error (pocket:answer "../notifications"))))
      (is (null *asked*) "a path is never built from an unread id"))))

;;; --- the section, the command, the missing key -------------------------------

(deftest pocket-cell-refuses-without-a-key-and-says-how-to-set-one ()
  (unwind-protect
       (progn
         (no-token)
         (with-cell-stop ((pocket-start))
           (with-stub '()
             (let ((refusal (refusal-text pocket:pocket-error (pocket:notify "hello"))))
               (is (search *token-env* refusal) "the refusal names the variable")
               (is (search "notify.belweave.ai" refusal) "and where a key comes from"))
             (is (null *asked*) "nothing left the machine"))))
    (token-here)))

(deftest pocket-cell-the-command-answers-status-and-sends-what-is-typed ()
  (token-here)
  (with-cell-stop ((pocket-start "project" "Release"))
    (multiple-value-bind (text dialog) (cell-entry "nodecode-pocket" "pocket" "")
      (is (search "https://notify.belweave.ai/api/agent" text) "status says where it sends")
      (is (search "project: Release" text))
      (is (string= "Pocket" (nlk:json-value dialog :string "title")) "and a panel offers to send one")) (with-stub (list (list "/notifications" "{\"notification\":{\"id\":\"ntf_3\"},\"accepted\":1}" 201))
      (let ((answer (cell-entry "nodecode-pocket" "pocket" "the release is out")))
        (is (search "ntf_3" answer))
        (is (string= "the release is out" (nlk:json-value (asked-json) :text "body")))
        (is (string= "Nodecode" (nlk:json-value (asked-json) :text "title")))))
    (with-stub (list (list "/notifications" "{\"notification\":{\"id\":\"ntf_4\"}}" 201))
      (is (search "ntf_4" (cell-entry "nodecode-pocket" "pocket" "test")) "the test update is one command away")))) 