;;;; cell-test.lisp --- the sabi cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every wire is a stubbed dex:post and every session a stubbed
;;;; attribution: nothing touches the network, the environment or the
;;;; operator's files.

(in-package #:nodecode.test)

(define-test-slice "sabi" "SABI-CELL-" :start nodecode-sabi:start-cell)

(define-cell-lifecycle-tests "sabi"
  (:hooks 'nle::models-catalog-table :credential 'nle::walk-provider-stream)
  (:refused ("base_url" 5) ("context_tokens" 0)))

(defun header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-sabi-round ((url headers body) &body forms)
  "FORMS with the cell started and one chat round for the sabi alias
captured: URL, HEADERS and BODY (the decoded request) as dex:post saw them."
  `(with-cell-stop ((sabi-start))
     (let ((nle::*provider* "sabi") (nle::*model* nodecode-sabi::+alias+)
           (nle::*api-key* "public") (nle::*endpoint* nil)
           (,url nil) (,headers nil) (,body nil))
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
         (with-stubbed-fdefinition (nodecode-sabi::session-attribution () "sess-01H:2")
           (nle::call-provider-streaming (user-context))))
       ,@forms)))

(deftest sabi-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((sabi-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "sabi")))
      (is (equal "Sabi" (nlk:json-value row :string "name")))
      (is (equal "http://127.0.0.1:8787/v1" (nlk:json-value row :string "api")))
      (is (gethash "sabi-code" (nlk:json-value row :object "models")) "the alias is listed")
      (is (equal "openai-completions" (nle::configured-provider-lane "sabi")) "the chat lane drives it")
      (is (equal "http://127.0.0.1:8787/v1/chat/completions"
                 (nle::lane-endpoint "sabi" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "sabi")))))

(deftest sabi-cell-base-follows-the-section ()
  (with-cell-stop ((sabi-start "base_url" "http://127.0.0.1:9911/v1"))
    (is (equal "http://127.0.0.1:9911/v1"
               (nlk:json-value (nle::models-catalog-table) :string "sabi" "api")))))

(deftest sabi-cell-asks-for-no-key-and-sends-no-other-familys ()
  (with-cell-stop ((sabi-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "sabi" :auth-path auth :probe t)))
          (is (eq :public (nle:credential-source credential)))
          (is (not (equal "sk-openai" (nle:credential-key credential)))))))))

(deftest sabi-cell-sends-the-attribution-headers ()
  (with-sabi-round (url headers body)
    (is (equal "http://127.0.0.1:8787/v1/chat/completions" url))
    (is (equal "sabi-code" (nlk:json-value body :string "model")))
    (is (equal "nodecode" (header headers "X-Sabi-Client")))
    (is (equal "sess-01H:2" (header headers "X-Sabi-Session")))
    (is (null (search "sk-openai" (or (header headers "authorization") "")))
        "no other provider's key reaches the proxy")))

(deftest sabi-cell-replaces-a-stale-attribution-header ()
  (with-cell-stop ((sabi-start))
    (with-stubbed-fdefinitions ((nodecode-sabi::session-attribution () nil)
                                (nodecode-sabi::ours-p (&rest args) (declare (ignore args)) t))
      (let ((seen nil))
        (nodecode-sabi::walk
         (lambda (fold &rest args)
           (declare (ignore fold))
           (setf seen (getf args :headers))
           nil)
         nil :config nil
         :headers '(("X-Sabi-Client" . "another-client") ("Accept" . "text/event-stream")))
        (is (equal "nodecode" (header seen "X-Sabi-Client")))
        (is (= 1 (count "x-sabi-client" seen :key #'car :test #'string-equal))
            "one copy of a header this cell owns")
        (is (equal "text/event-stream" (header seen "Accept")) "the lane's own headers stay")))))

(deftest sabi-cell-omits-an-impure-session-id ()
  (with-cell-stop ((sabi-start))
    (with-stubbed-fdefinition (nodecode-sabi::session-attribution () "two words")
      (is (null (assoc "X-Sabi-Session" (nodecode-sabi::attribution-headers)
                        :test #'string-equal))))))

(deftest sabi-cell-leaves-other-providers-alone ()
  (with-cell-stop ((sabi-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "gpt-x") (nle::*api-key* "k")
          (nle::*endpoint* nil) (headers nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (is (stringp asked))
           (setf headers (getf args :headers) body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (equal "gpt-x" (nlk:json-value body :string "model")))
      (is (null (header headers "X-Sabi-Client"))))))
