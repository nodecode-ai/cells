;;;; cell-test.lisp --- the hostyourai cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every service endpoint a stubbed
;;;; DEX:POST or NLK:HTTP-FETCH: nothing touches the network, the environment
;;;; or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "hostyourai" "HOSTYOURAI-CELL-" :start nodecode-hostyourai:start-cell)

(define-cell-lifecycle-tests "hostyourai"
  (:config "base_url" "https://relay.example/v1")
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body 'nle::walk-provider-stream)
  (:command "hostyourai")
  (:running (is (equal "https://relay.example/v1" (nlk:json-value (nle::models-catalog-table) :string "hostyourai" "api")) "the catalog row carries the section's base"))
  (:refused ("base_url" 5) ("task" "nope") ("transport" "carrier-pigeon")))

;;; --- fixtures --------------------------------------------------------------------

(defun hya-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defvar *hya-message* "x"
  "The text the round's one user message carries; a test that wants auto to
have something to read binds it longer.")

(defmacro with-hya-round ((url headers body) model config &body forms)
  "FORMS with the cell started on CONFIG (section pairs) and one chat round
for MODEL captured: URL, HEADERS and BODY (the decoded request) as dex:post
saw them."
  `(with-cell-stop ((hostyourai-start ,@config))
     (let ((nle::*provider* "hostyourai") (nle::*model* ,model) (nle::*api-key* "sk-test")
           (nle::*endpoint* nil) (,url nil) (,headers nil) (,body nil))
       (declare (ignorable ,url ,headers ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values (make-truncated-sse-stream
                     "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                     "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                     "[DONE]")
                    200))
         (nle::call-provider-streaming (user-context *hya-message*)))
       ,@forms)))

;;; --- the catalog and the key --------------------------------------------------------

(deftest hostyourai-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((hostyourai-start))
    (let* ((catalog (nle::models-catalog-table))
           (row (nlk:json-value catalog :object "hostyourai")))
      (is (equal "HostYourAI" (nlk:json-value row :string "name")))
      (is (equal "https://hostyourai.com/api/v1" (nlk:json-value row :string "api")))
      (is (gethash "hyai/auto" (nlk:json-value row :object "models")) "the router is the one model listed")
      (is (equal "openai-completions" (nle::configured-provider-lane "hostyourai")) "the chat lane drives it")
      (is (equal "https://hostyourai.com/api/v1/chat/completions"
                 (nle::lane-endpoint "hostyourai" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "hostyourai")))))

(deftest hostyourai-cell-reads-hostyourai-api-key ()
  (with-cell-stop ((hostyourai-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "HOSTYOURAI_API_KEY") "sk-hyai"))
        (let ((credential (nle::resolve-provider-credential "hostyourai" :auth-path auth :probe t)))
          (is (equal "sk-hyai" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential)))))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "HOSTYOURAI_API_KEY") "sk-hyai"))
        (is (not (equal "sk-hyai"
                        (nle:credential-key (nle::resolve-provider-credential "anthropic" :auth-path auth :probe t)))))))))

(deftest hostyourai-cell-never-sends-another-familys-key ()
  (with-cell-stop ((hostyourai-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "hostyourai" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

;;; --- the hint on the wire -------------------------------------------------------------

(deftest hostyourai-cell-sends-the-task-and-policy-in-the-body ()
  (with-hya-round (url headers body) "hyai/auto" ("task" "code" "policy" "speed")
    (is (equal "https://hostyourai.com/api/v1/chat/completions" url))
    (is (equal "hyai/auto" (nlk:json-value body :text "model")))
    (is (equal "code" (nlk:json-value body :text "hyai" "task")))
    (is (equal "speed" (nlk:json-value body :text "hyai" "policy")))))

(deftest hostyourai-cell-sends-no-hint-until-one-is-set ()
  (with-hya-round (url headers body) "hyai/auto" ()
    (is (null (nlk:json-value body :object "hyai")) "router on both words is no hint at all")))

(deftest hostyourai-cell-sends-them-as-headers-when-the-section-says-so ()
  (with-hya-round (url headers body) "hyai/auto" ("task" "code" "policy" "speed" "transport" "headers")
    (is (equal "code" (hya-header headers "X-HYAI-Task")))
    (is (equal "speed" (hya-header headers "X-HYAI-Policy")))
    (is (null (nlk:json-value body :object "hyai")) "the body carries no hint then")))

(deftest hostyourai-cell-auto-asks-system-one ()
  (let ((*hya-message* "please write me a lisp function that reads a file line by line")
        (asked nil))
    (with-stubbed-fdefinition (nle:http-fetch (url &rest args)
                               (setf asked (list url (getf args :content) (getf args :headers)))
                               (values "{\"answers\":{\"task\":{\"choice\":\"code\",\"confidence\":0.93}}}" 200))
      (with-hya-round (url headers body) "hyai/auto" ("task" "auto")
        (is (equal "code" (nlk:json-value body :text "hyai" "task")) "the answered task rides the round")
        (is (equal "https://hostyourai.com/api/decision/v1/systemone" (first asked)))
        (is (search "reads a file line by line" (second asked)) "the classifier reads the message")
        (is (equal "Bearer sk-test" (hya-header (third asked) "authorization")))))))

(deftest hostyourai-cell-auto-falls-back-to-chat ()
  (let ((*hya-message* "please write me a lisp function that reads a file line by line"))
    (with-stubbed-fdefinition (nle:http-fetch (url &rest args)
                               (declare (ignore url args))
                               (values "{\"answers\":{\"task\":{\"choice\":\"code\",\"confidence\":0.1}}}" 200))
      (with-hya-round (url headers body) "hyai/auto" ("task" "auto")
        (is (equal "chat" (nlk:json-value body :text "hyai" "task")) "a low-confidence answer is chat")))))

(deftest hostyourai-cell-leaves-other-providers-alone ()
  (with-cell-stop ((hostyourai-start "task" "code"))
    (let ((nle::*provider* "openai-completions") (nle::*model* "gpt-4o") (nle::*api-key* "sk")
          (nle::*endpoint* nil) (headers nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (declare (ignore asked))
           (setf headers (getf args :headers) body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (null (nlk:json-value body :object "hyai")))
      (is (null (hya-header headers "X-HYAI-Task"))))))

;;; --- the command ------------------------------------------------------------------------

(deftest hostyourai-cell-command-sets-and-clears ()
  (with-cell-stop ((hostyourai-start))
    (is (search "not set" (cell-entry "nodecode-hostyourai" "hostyourai" "")) "bare /hostyourai says what is set")
    (is (search "Usage: /hostyourai" (cell-entry "nodecode-hostyourai" "hostyourai" "")))
    (is (search "task=code" (cell-entry "nodecode-hostyourai" "hostyourai" "task code")))
    (is (search "policy=speed" (cell-entry "nodecode-hostyourai" "hostyourai" "code speed")) "a task and a policy in one shorthand")
    (is (search "task=code policy=speed" (cell-entry "nodecode-hostyourai" "hostyourai" "status")))
    (is (search "cleared" (cell-entry "nodecode-hostyourai" "hostyourai" "off")))
    (is (search "not set" (cell-entry "nodecode-hostyourai" "hostyourai" "status")) "and the hint is gone")
    (is (search "Unknown task" (cell-entry "nodecode-hostyourai" "hostyourai" "task nope")))
    (is (search "Unknown policy" (cell-entry "nodecode-hostyourai" "hostyourai" "policy nope")))))

(deftest hostyourai-cell-usage-reads-the-balance ()
  (with-cell-stop ((hostyourai-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*api-key* nil))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "HOSTYOURAI_API_KEY") "sk-hyai"))
          (with-stubbed-fdefinition (nle:http-fetch (url &rest args)
                                     (declare (ignore args))
                                     (is (equal "https://hostyourai.com/api/v1/balance" url))
                                     (values "{\"balance\":1.23,\"spend_month\":0.5,\"auto_topup\":false,\"unlimited\":false}" 200))
            (let ((answer (cell-entry "nodecode-hostyourai" "hostyourai" "usage")))
              (is (search "Balance: €1.23" answer))
              (is (search "Spent this month: €0.50" answer))
              (is (search "Auto top-up: off" answer)))))))))

(deftest hostyourai-cell-usage-without-a-key-says-so ()
  (with-cell-stop ((hostyourai-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*api-key* nil))
        (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
          (is (search "no HOSTYOURAI_API_KEY set"
                      (cell-entry "nodecode-hostyourai" "hostyourai" "usage"))))))))
