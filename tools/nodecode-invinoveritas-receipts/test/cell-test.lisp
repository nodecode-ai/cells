;;;; cell-test.lisp --- the cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every call to invinoveritas is a stubbed NLK:HTTP, every key a stubbed
;;;; NLE::CREDENTIAL-ENV, and the ledger a scratch home's own file: nothing
;;;; reaches the service, the environment or the operator's files. What is
;;;; proved is the canonical JSON the hashes are taken over, the body and the
;;;; headers a receipt is asked for with, what the ledger keeps, that a
;;;; receipt opens only to the choice it committed to, and each refusal.

(in-package #:nodecode.test)

(define-test-slice "invinoveritas-receipts" "INVINOVERITAS-RECEIPTS-CELL-" :start nodecode-invinoveritas-receipts:start-cell)

(define-cell-lifecycle-tests "invinoveritas-receipts"
  (:help :invinoveritas-receipts)
  (:command "invinoveritas-receipts")
  (:running (is (equal nodecode-invinoveritas-receipts::+primer+ (nle:help :invinoveritas-receipts)))
            (is (equal "https://api.babyblueviper.com"
                       (nodecode-invinoveritas-receipts::setting :api-url)) "the service's own base by default"))
  (:refused ("timeout_seconds" 0) ("listed" 0))
  (:idle nodecode-invinoveritas-receipts:invinoveritas-receipts-error (nodecode-invinoveritas-receipts:issue "approve" "x")))

;;; --- fixtures ------------------------------------------------------------

(defvar *asked* nil
  "What the stubbed endpoint was sent, newest first: (METHOD URL HEADERS BODY).")

(defmacro with-endpoint ((&key (status 200) (answer "{}") (key "k-1")) &body body)
  "BODY with NLK:HTTP answering ANSWER at STATUS and NLE::CREDENTIAL-ENV
holding KEY under the cell's variable; ANSWER is a form, with CONTENT bound
to the request body."
  `(with-stubbed-fdefinition (nlk:http (method url &rest args)
                              (let ((content (getf args :content)))
                                (declare (ignorable content))
                                (push (list method url (getf args :headers) content) *asked*)
                                (values ,answer ,status)))
     (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and ,key (string= name "INVINOVERITAS_API_KEY") ,key))
       ,@body)))

(defun invinoveritas-receipts-reset ()
  "An empty ledger and an empty log, whatever an earlier test left."
  (uiop:delete-file-if-exists (nodecode-invinoveritas-receipts::store-path))
  (setf *asked* nil))

(defun invinoveritas-receipts-header (headers name)
  "HEADERS, an alist, as it carries NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun invinoveritas-receipts-answer (content &key (salt "salt-1") (id "ev-1"))
  "The service's answer to the request BODY CONTENT: an event, a receipt whose
two commitments are taken over the salt and what the body committed to, and
the salt revealed to this machine alone."
  (let* ((body (nlk:decode-json content))
         (sha (nlk:json-value body :string "context_sha256"))
         (choice (nlk:json-value body :string "choice")))
    (nlk:encode-json-object
     (nodecode-invinoveritas-receipts::jobject
      "event" (nodecode-invinoveritas-receipts::jobject "id" id "issued_at" "2026-01-01T00:00:00Z"
                                           "type" "decision")
      "receipt" (nodecode-invinoveritas-receipts::jobject
                 "choice_commitment" (nodecode-invinoveritas-receipts::commitment salt choice)
                 "context_commitment" (nodecode-invinoveritas-receipts::commitment salt sha))
      "reveal" (nodecode-invinoveritas-receipts::jobject "salt" salt)))))

(defmacro with-receipts ((&key (api "https://api.example.test")) &body body)
  "BODY with the cell started on API and an empty ledger."
  `(with-cell-stop ((invinoveritas-receipts-start "api_url" ,api))
     (invinoveritas-receipts-reset)
     ,@body))

;;; --- the hash ------------------------------------------------------------

(deftest invinoveritas-receipts-cell-canonical-json-is-sorted-and-compact ()
  (let ((object (nodecode-invinoveritas-receipts::jobject
                 "b" "two"
                 "a" (coerce '("x" "y") 'vector)
                 "c" (nodecode-invinoveritas-receipts::jobject "z" 1 "n" :true))))
    (is (equal "{\"a\":[\"x\",\"y\"],\"b\":\"two\",\"c\":{\"n\":true,\"z\":1}}"
               (nodecode-invinoveritas-receipts::canonical-text object)) "keys sorted, no space, booleans as JSON"))
  (is (equal "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
             (nodecode-invinoveritas-receipts::digest "abc")) "64 hex characters, as the service publishes")
  (is (= 64 (length (nodecode-invinoveritas-receipts::new-nonce))) "a nonce is 32 bytes of hex"))

;;; --- asking for a receipt ------------------------------------------------

(deftest invinoveritas-receipts-cell-issues-a-receipt-and-keeps-the-salt ()
  (with-receipts ()
    (with-endpoint (:answer (invinoveritas-receipts-answer content))
      (let ((answer (nodecode-invinoveritas-receipts:issue "approve" "rm -rf the build"
                                             :decider "guard" :reference "call-7")))
        (is (search "ev-1" answer) "the answer names the receipt")
        (is (search "https://api.example.test/verify-proof" answer) "and where it is checked"))
      (destructuring-bind (method url headers content) (first *asked*)
        (is (eq :post method))
        (is (equal "https://api.example.test/decision-receipt" url))
        (is (equal "Bearer k-1" (invinoveritas-receipts-header headers "authorization")))
        (is (equal "application/json" (invinoveritas-receipts-header headers "content-type")))
        (let ((body (nlk:decode-json content)))
          (is (equal "approve" (nlk:json-value body :string "choice")))
          (is (equal "guard" (nlk:json-value body :string "decider" "model")))
          (is (equal "call-7" (nlk:json-value body :string "decider" "request_id")))
          (is (equal "nodecode" (nlk:json-value body :string "decider" "provider")))
          (is (equal '("approve" "deny")
                     (coerce (gethash "options" body) 'list)) "the options it offered")
          (is (= 64 (length (nlk:json-value body :string "context_sha256"))))
          (is (not (search "rm -rf the build" content)) "the context itself is not sent")))
      (let ((record (nodecode-invinoveritas-receipts::kept-record "ev-1")))
        (is (equal "salt-1" (nodecode-invinoveritas-receipts::text-field record "salt")))
        (is (equal "approve" (nodecode-invinoveritas-receipts::text-field record "choice")))
        (is (equal "rm -rf the build"
                   (nlk:json-value record :string "context" "subject")) "the context stays here")
        (is (equal "call-7" (nlk:json-value record :string "context" "reference")))
        (is (search "verify-proof" (nodecode-invinoveritas-receipts::text-field record "verify")))))))

(deftest invinoveritas-receipts-cell-a-model-decides-under-its-own-name ()
  (with-receipts ()
    (with-endpoint (:answer (invinoveritas-receipts-answer content))
      (nodecode-invinoveritas-receipts:issue "deny" "deploy on a friday" :decider "guardian")
      (let ((body (nlk:decode-json (fourth (first *asked*)))))
        (is (equal "guardian" (nlk:json-value body :string "decider" "model")))
        (is (equal "deny" (nlk:json-value body :string "choice")))))))

;;; --- refusals ------------------------------------------------------------

(deftest invinoveritas-receipts-cell-without-a-key-names-the-setting-and-sends-nothing ()
  (with-receipts ()
    (with-endpoint (:key nil)
      (let ((answer (nodecode-invinoveritas-receipts::issue-tool
                     (nodecode-invinoveritas-receipts::decode-issue-receipt
                      (nodecode-invinoveritas-receipts::jobject "choice" "approve" "context" "the deploy")))))
        (is (search "ERROR: INVINOVERITAS-RECEIPTS-ERROR" answer))
        (is (search "INVINOVERITAS_API_KEY" answer) "the variable to set")
        (is (search "api_key_env" answer) "the setting that names it"))
      (is (null *asked*) "no call without a key"))))

(deftest invinoveritas-receipts-cell-a-choice-outside-the-options-refuses ()
  (with-receipts ()
    (with-endpoint (:answer (invinoveritas-receipts-answer content))
      (signals-error nodecode-invinoveritas-receipts:invinoveritas-receipts-error
        (nodecode-invinoveritas-receipts:issue "maybe" "the deploy"))
      (is (null *asked*) "nothing is asked for an option the question never offered"))))

(deftest invinoveritas-receipts-cell-a-refused-status-says-what-came-back ()
  (with-receipts ()
    (with-endpoint (:status 402 :answer "{\"error\":\"payment required\"}")
      (let ((answer (nodecode-invinoveritas-receipts::issue-tool
                     (nodecode-invinoveritas-receipts::decode-issue-receipt
                      (nodecode-invinoveritas-receipts::jobject "choice" "approve" "context" "the deploy")))))
        (is (search "HTTP 402" answer))
        (is (search "payment required" answer))))))

;;; --- verifying and opening -------------------------------------------------

(deftest invinoveritas-receipts-cell-verify-asks-the-free-endpoint-with-the-event ()
  (with-receipts ()
    (with-endpoint (:answer (invinoveritas-receipts-answer content))
      (nodecode-invinoveritas-receipts:issue "approve" "the deploy")
      (setf *asked* nil)
      (with-endpoint (:key nil :answer "{\"valid\":true,\"reason\":\"signature matches\"}")
        (let ((answer (nodecode-invinoveritas-receipts:verify "ev-1")))
          (is (search "signature valid" answer))
          (is (search "signature matches" answer)))
        (destructuring-bind (method url headers content) (first *asked*)
          (is (eq :post method))
          (is (equal "https://api.example.test/verify-proof" url))
          (is (null (invinoveritas-receipts-header headers "authorization")) "free and unauthenticated")
          (is (equal "ev-1" (nlk:json-value (nlk:decode-json content) :string "event" "id"))))))))

(deftest invinoveritas-receipts-cell-a-kept-receipt-opens-to-what-was-decided ()
  (with-receipts ()
    (with-endpoint (:answer (invinoveritas-receipts-answer content))
      (nodecode-invinoveritas-receipts:issue "approve" "the deploy"))
    (setf *asked* nil)
    (let ((answer (nodecode-invinoveritas-receipts:check "ev-1")))
      (is (search "opens as \"approve\"" answer))
      (is (search "context hash: holds" answer))
      (is (search "choice commitment: holds" answer))
      (is (search "context commitment: holds" answer)))
    (is (null *asked*) "opening asks the network nothing")))

(deftest invinoveritas-receipts-cell-a-tampered-choice-does-not-open ()
  (with-receipts ()
    (with-endpoint (:answer (invinoveritas-receipts-answer content))
      (nodecode-invinoveritas-receipts:issue "approve" "the deploy"))
    (let ((record (nodecode-invinoveritas-receipts::kept-record "ev-1")))
      (setf (gethash "choice" record) "deny")
      (with-open-file (out (nodecode-invinoveritas-receipts::store-path) :direction :output
                                                           :if-exists :supersede
                                                           :external-format :utf-8)
        (write-line (nlk:encode-json-object record) out)))
    (let ((answer (nodecode-invinoveritas-receipts:check "ev-1")))
      (is (search "opens as \"deny\"" answer))
      (is (search "choice commitment: does NOT hold" answer) "it still commits to its choice"))))

;;; --- the command -----------------------------------------------------------

(deftest invinoveritas-receipts-cell-the-command-lists-and-shows-what-is-kept ()
  (with-receipts ()
    (is (search "no receipt is kept" (nodecode-invinoveritas-receipts::run-slash "" nil)))
    (with-endpoint (:answer (invinoveritas-receipts-answer content))
      (nodecode-invinoveritas-receipts:issue "approve" "the deploy"))
    (let ((listing (nodecode-invinoveritas-receipts::run-slash "" nil)))
      (is (search "1 receipt kept" listing))
      (is (search "ev-1" listing))
      (is (search "approve" listing)))
    (let ((shown (nodecode-invinoveritas-receipts::run-slash "ev-1" nil)))
      (is (search "chose: approve" shown))
      (is (search "receipts:check" shown) "it says how to open it"))))
