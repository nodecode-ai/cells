;;;; cell-test.lisp --- the AgentPurse cell over a scripted MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing of the cell is stubbed but the one function that dials (EXCHANGE)
;;;; and the one that puts a question to a person (NLK:ASK-OPERATOR): the
;;;; JSON-RPC session, the gate, the grants, the ledger and the state file all
;;;; run for real, against a temp home. No test reaches the network.

(in-package #:nodecode.test)

(define-test-slice "agentpurse" "AGENTPURSE-CELL-" :start nodecode-agentpurse:start-cell)

(define-cell-lifecycle-tests "agentpurse"
  (:config "api_key" "agpk_test" "base_url" "https://purse.example")
  (:help :agentpurse)
  (:command "purse")
  (:running (is (= 5000 (nodecode-agentpurse::setting :spend-limit-cents)) "the spend limit defaults to 5000 cents")
            (is (hash-table-p nodecode-agentpurse::*state*) "the state file is loaded"))
  (:refused ("api_key" "agpk_test" "spend_limit_cents" -1)
            ("api_key" "agpk_test" "approval_timeout_seconds" -5))
  (:idle purse:agentpurse-error (purse:get-budget)))

;;; --- a scripted server --------------------------------------------------------------

(defvar *wire* '() "The script: (TOOL-NAME . JSON-TEXT) for a tools/call, and (:spend . OBJECT) for a status GET.")

(defvar *asked* '() "Every exchange this script answered, oldest first: (METHOD URL BODY HEADERS).")

(defvar *questions* '() "Every question put to a person, oldest first: (TITLE CHOICES DETAIL).")

(defvar *answer* nil)

(defun wire-answer (tool)
  "TOOL's scripted answer; the reads a purchase makes answer plainly when the
test did not script them."
  (or (cdr (assoc tool *wire* :test #'string=))
      (cond ((equal tool "get_budget") "[{\"name\":\"Grocer\",\"remainingCents\":4200}]")
            ((equal tool "search_merchants")
             "[{\"name\":\"Green Grocer\",\"blocked\":false,\"trusted\":true}]")
            ((equal tool "list_watches") "[]")
            (t (format nil "{\"ok\":true,\"tool\":\"~a\"}" tool)))))

(defun mcp-envelope (text)
  "TEXT inside the one MCP tools/call result the client unwraps."
  (nlk:encode-json-object
   (nlk:make-json-object
    "jsonrpc" "2.0" "id" 1
    "result" (nlk:make-json-object
              "content" (vector (nlk:make-json-object "type" "text" "text" text))))))

(defun scripted-exchange (method url &key headers content timeout)
  "The one dial: an MCP handshake, a tools/call, or a payment status."
  (declare (ignore timeout))
  (push (list method url content headers) *asked*)
  (let ((message (and content (ignore-errors (nlk:decode-json content)))))
    (cond
      ((equal (nlk:json-value message :string "method") "initialize")
       (values (nlk:encode-json-object (nlk:make-json-object
                                        "jsonrpc" "2.0" "id" 1
                                        "result" (nlk:make-json-object
                                                  "protocolVersion" "2025-11-25"
                                                  "serverInfo" (nlk:make-json-object "name" "script"))))
               200 '(("mcp-session-id" . "s-1") ("content-type" . "application/json"))))
      ((equal (nlk:json-value message :string "method") "notifications/initialized")
       (values "" 202 nil))
      ((equal (nlk:json-value message :string "method") "tools/call")
       (let ((name (nlk:json-value message :string "params" "name")))
         (values (mcp-envelope (wire-answer name)) 200 nil)))
      ((eq method :get)
       (let ((row (cdr (assoc :spend *wire*))))
         (values (nlk:encode-json-object
                  (nlk:make-json-object "ok" t
                                         "intent" (or row (nlk:make-json-object
                                                            "id" "pay-1" "status" "captured"
                                                            "amountCents" 255
                                                            "merchantName" "Green Grocer"))))
                 200 nil)))
      (t (values "" 200 nil)))))

(defun scripted-question (title &key (choices '("Approve once" "Deny")) detail seconds session-id)
  "The one approval surface: nobody answers unless a test said who would."
  (declare (ignore seconds session-id))
  (push (list title choices detail) *questions*)
  (values (first *answer*) (second *answer*)))

(defmacro with-purse ((&rest config) &body body)
  "BODY with a temp home, the cell started on CONFIG over a scripted server,
and every ask answered as *ANSWER* says (unbound: nobody answers)."
  `(with-temp-trees ((home "purse-home"))
     (ensure-directories-exist home)
     (let ((nlk:*home* home)
           (*wire* '())
           (*asked* '())
           (*questions* '())
           (*answer* nil))
       (with-stubbed-fdefinitions
           ((nodecode-agentpurse::exchange (method url &key headers content timeout)
              (scripted-exchange method url :headers headers :content content :timeout timeout))
            (nle:ask-operator (title &key (choices '("Approve once" "Deny")) detail seconds session-id)
              (scripted-question title :choices choices :detail detail :seconds seconds
                                       :session-id session-id)))
         (with-cell-stop ((agentpurse-start "api_key" "agpk_test"
                                            "base_url" "https://purse.example"
                                            ,@config))
           ,@body)))))

(defun purse-text (value) (format nil "~a" value))

(defun called-tools ()
  "The tool names this run called on the server, oldest first."
  (loop for row in (reverse *asked*)
        for content = (third row)
        for message = (and content (ignore-errors (nlk:decode-json content)))
        when (and (eq (first row) :post) content
                  (equal "tools/call" (nlk:json-value message :string "method")))
          collect (nlk:json-value message :string "params" "name")))

(defun authorization-of (row)
  (cdr (assoc "authorization" (fourth row) :test #'string-equal)))

(defparameter +quote+
  (nlk:encode-json-object
   (nlk:make-json-object "quoteId" "q-1" "merchant" "Green Grocer"
                          "amountCents" 255 "description" "oat milk"
                          "guard" (nlk:make-json-object "decision" "pass"))) "A quote the server would answer, in the clear.")

(defparameter +blocked-quote+
  (nlk:encode-json-object
   (nlk:make-json-object "quoteId" "q-2" "merchant" "Green Grocer"
                          "amountCents" 255 "description" "oat milk"
                          "guard" (nlk:make-json-object
                                   "decision" "block"
                                   "reasons" (vector (nlk:make-json-object
                                                      "sentence" "the page asked for more money"))))) "A quote the intent guard refused.")

(defparameter +captured+
  (nlk:encode-json-object
   (nlk:make-json-object "ok" t
                          "intent" (nlk:make-json-object "id" "pay-1" "status" "captured"
                                                          "amountCents" 255
                                                          "merchantName" "Green Grocer")
                          "order" (nlk:make-json-object "id" "order-9"))) "A captured sandbox payment.")

;;; --- reading ---------------------------------------------------------------------

(deftest agentpurse-cell-a-read-goes-through-the-handshake ()
  (with-purse ()
    (setf *wire* '( ("get_budget" . "[{\"name\":\"Grocer\",\"remainingCents\":4200}]") ))
    (is-carrying (purse-text (purse:get-budget))
      ("remainingCents" "the server's own JSON comes back")
      ("4200" "and its numbers"))
    (is (equal '("initialize" "notifications/initialized" "tools/call")
               (loop for row in (reverse *asked*)
                     collect (or (and (third row)
                                      (nlk:json-value (nlk:decode-json (third row)) :string "method"))
                                 (if (eq (first row) :get) "GET" "?")))) "one session, then the call")
    (is (every (lambda (row) (string= "https://purse.example/mcp" (second row)))
               (remove-if-not (lambda (row) (eq (first row) :post)) *asked*)) "every call went to the section's origin")
    (is (equal "Bearer agpk_test" (authorization-of (first *asked*))) "and carried the section's key")))

(deftest agentpurse-cell-a-quote-is-kept-for-the-gate ()
  (with-purse ()
    (setf *wire* (list (cons "get_quote" +quote+)))
    (is (search "q-1" (purse-text (purse:get-quote :merchant "Green Grocer" :amount-cents 255
                                                      :original-task "buy oat milk"))))
    (let ((quote (nodecode-agentpurse::cached-quote "q-1")))
      (is (equal "Green Grocer" (getf quote :merchant)) "the merchant is remembered")
      (is (eql 255 (getf quote :amount-cents)) "and the amount, in cents")
      (is (equal "pass" (getf quote :guard)) "and the guard's verdict"))))

;;; --- paying ------------------------------------------------------------------------

(deftest agentpurse-cell-nobody-answers-so-nothing-is-sent ()
  (with-purse ()
    (setf *wire* (list (cons "get_quote" +quote+)))
    (purse:get-quote :merchant "Green Grocer" :amount-cents 255 :original-task "buy oat milk")
    (let ((said (refusal-text purse:agentpurse-error (purse:request-purchase :quote-id "q-1"))))
      (is (search "/purse approve" said) (format nil "the refusal names the way to approve: ~a" said)))
    (is (not (member "request_purchase" (called-tools) :test #'string=)) "the purchase never reached the server")
    (is (= 1 (length (nodecode-agentpurse::pending-grants "operator"))) "one proposed grant waits")
    (is (search "waiting:" (first (nodecode-agentpurse::pending-lines "operator"))) "and it is not approved yet")))

(deftest agentpurse-cell-the-person-approves-then-the-same-call-sends ()
  (with-purse ()
    (setf *wire* (list (cons "get_quote" +quote+)
                        (cons "request_purchase" +captured+) (cons :spend +captured+)))
    (purse:get-quote :merchant "Green Grocer" :amount-cents 255 :original-task "buy oat milk")
    (let ((said (refusal-text purse:agentpurse-error (purse:request-purchase :quote-id "q-1"))))
      (is (search "/purse approve" said) (format nil "the first call waits: ~a" said)))
    (is-carrying (purse-text (purse:approve)) ("approved" "the person's yes"))
    (is-carrying (purse-text (purse:request-purchase :quote-id "q-1"))
      ("captured" "the same call sends it")
      ("$2.55" "with the amount"))
    (is (= 1 (count "request_purchase" (called-tools) :test #'string=)) "exactly one purchase was sent")
    (is (eql 255 (nodecode-agentpurse::spent-cents "operator")) "and the ledger holds its cents")
    (is (search "$2.55" (purse-text (nodecode-agentpurse::budget-line "operator"))) "the budget names this session's spend")
    (refusal-text purse:agentpurse-error (purse:request-purchase :quote-id "q-1"))
    (is (= 1 (count "request_purchase" (called-tools) :test #'string=)) "and a second call cannot pay twice")))

(deftest agentpurse-cell-a-question-answers-for-a-person-in-one-call ()
  (with-purse ()
    (setf *wire* (list (cons "get_quote" +quote+)
                        (cons "request_purchase" +captured+) (cons :spend +captured+))
          *answer* '("Approve once" :answered))
    (purse:get-quote :merchant "Green Grocer" :amount-cents 255 :original-task "buy oat milk")
    (is-carrying (purse-text (purse:request-purchase :quote-id "q-1")) ("captured"))
    (is (= 1 (length *questions*)) "one question was put")
    (is (search "Merchant: Green Grocer" (third (first *questions*))) "it named the merchant")
    (is (search "Amount: $2.55" (third (first *questions*))) "and the amount")
    (is (= 1 (count "request_purchase" (called-tools) :test #'string=)) "one purchase went out")
    (is (null (nodecode-agentpurse::pending-grants "operator")) "and no grant is left over")))

(deftest agentpurse-cell-every-block-sends-nothing ()
  (with-purse ("spend_limit_cents" 100)
    (setf *wire* (list (cons "get_quote" +quote+)))
    (purse:get-quote :merchant "Green Grocer" :amount-cents 255 :original-task "buy oat milk")
    (is (search "limit"
                (refusal-text purse:agentpurse-error (purse:request-purchase :quote-id "q-1"))) "over this session's limit is refused by name")
    (is (not (member "request_purchase" (called-tools) :test #'string=)) "nothing was sent")
    (is (null (nodecode-agentpurse::pending-grants "operator")) "and no grant was left")))

(deftest agentpurse-cell-a-guard-block-and-a-kill-switch-refuse ()
  (with-purse ()
    (setf *wire* (list (cons "get_quote" +blocked-quote+)
))
    (purse:get-quote :merchant "Green Grocer" :amount-cents 255 :original-task "buy oat milk")
    (is (search "guard"
                (refusal-text purse:agentpurse-error (purse:request-purchase :quote-id "q-2"))) "the guard's block is named")
    (is-carrying (purse-text (nodecode-agentpurse::run-purse "kill" "operator"))
      ("Kill switch is on"))
    (let ((said (refusal-text purse:agentpurse-error (purse:request-purchase :quote-id "q-2"))))
      (is (search "kill switch" said) (format nil "the kill switch is named: ~a" said)))
    (is (not (member "request_purchase" (called-tools) :test #'string=)) "nothing reached the server either time")))

(deftest agentpurse-cell-a-missing-key-refuses-and-names-the-variable ()
  (with-temp-trees ((home "purse-nokey"))
    (ensure-directories-exist home)
    (let ((nlk:*home* home))
      (with-stubbed-fdefinitions
          ((nodecode-agentpurse::exchange (method url &key headers content timeout)
             (scripted-exchange method url :headers headers :content content :timeout timeout)))
        (with-cell-stop ((agentpurse-start "api_key_env" "AGENTPURSE_TEST_KEY_UNSET"
                                           "base_url" "https://purse.example"))
          (is (search "AGENTPURSE_TEST_KEY_UNSET"
                      (refusal-text purse:agentpurse-error (purse:get-budget))) "the refusal names the variable to set"))))))
