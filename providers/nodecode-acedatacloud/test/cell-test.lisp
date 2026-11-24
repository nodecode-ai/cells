;;;; cell-test.lisp --- the acedatacloud cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every listing a stubbed
;;;; NLE::HTTP-FETCH, every round a stubbed dex:post: nothing touches the
;;;; network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "acedatacloud" "ACEDATACLOUD-CELL-" :start nodecode-acedatacloud:start-cell)

(define-cell-lifecycle-tests "acedatacloud"
  (:hooks 'nle::models-catalog-table :credential 'nle::list-provider-models)
  (:refused ("base_url" 5)))

;;; --- fixtures -------------------------------------------------------------------

(defun ace-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-ace-round ((url headers body) model &body forms)
  "FORMS with the cell started and one chat round for acedatacloud MODEL
captured: URL, HEADERS and BODY (the decoded request) as dex:post saw them."
  `(with-cell-stop ((acedatacloud-start))
     (let ((nle::*provider* "acedatacloud") (nle::*model* ,model) (nle::*api-key* "sk-test")
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
         (nle::call-provider-streaming (user-context)))
       ,@forms)))

(defun listing-body (&rest ids)
  "A /models answer carrying IDS, as the vendor's shared catalog answers."
  (format nil "{\"data\":[~{~a~^,~}]}"
          (loop for id in ids collect (format nil "{\"id\":\"~a\"}" id))))

;;; --- the catalog and the wire ---------------------------------------------------------

(deftest acedatacloud-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((acedatacloud-start))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "acedatacloud"))
           (models (nlk:json-value row :object "models")))
      (is (equal "Ace Data Cloud" (nlk:json-value row :string "name")))
      (is (equal "https://api.acedata.cloud/openai" (nlk:json-value row :string "api")))
      (is (equalp #("ACEDATACLOUD_API_KEY") (nlk:json-value row :array "env")))
      (is (gethash "gpt-4.1-mini" models) "the verified models are listed")
      (is (gethash "gpt-4.1" models))
      (is (null (nle::catalog-model-price (gethash "gpt-4.1" models))) "no price is invented")
      (is (nle::catalog-model-tool-call-p (gethash "gpt-4.1-mini" models)) "it takes tool calls")
      (is (equal "openai-completions" (nle::configured-provider-lane "acedatacloud")) "the chat lane drives it")
      (is (equal "https://api.acedata.cloud/openai/chat/completions"
                 (nle::lane-endpoint "acedatacloud" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "acedatacloud")))))

(deftest acedatacloud-cell-base-follows-the-section ()
  (with-cell-stop ((acedatacloud-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "acedatacloud" "api")))))

(deftest acedatacloud-cell-posts-a-chat-round-to-its-address ()
  (with-ace-round (url headers body) "gpt-4.1-mini"
    (is (equal "https://api.acedata.cloud/openai/chat/completions" url))
    (is (equal "gpt-4.1-mini" (nlk:json-value body :string "model")))
    (is (equal "Bearer sk-test" (ace-header headers "authorization")))))

;;; --- the credential --------------------------------------------------------------------

(deftest acedatacloud-cell-reads-its-variable-and-no-other ()
  (with-cell-stop ((acedatacloud-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "ACEDATACLOUD_API_KEY") "ace-key"))
        (let ((credential (nle::resolve-provider-credential "acedatacloud" :auth-path auth :probe t)))
          (is (equal "ace-key" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential)))))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "acedatacloud" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))) "another family's key is never sent")
          (is (eq :public (nle:credential-source credential))))))))

(deftest acedatacloud-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((acedatacloud-start))
    (with-temp-auth (auth "{\"api_keys\":{\"acedatacloud\":{\"provider\":\"acedatacloud\",\"key\":\"ace-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "ACEDATACLOUD_API_KEY") "ace-env"))
        (is (equal "ace-saved"
                   (nle:credential-key
                    (nle::resolve-provider-credential "acedatacloud" :auth-path auth :probe t))))))))

;;; --- the listing -----------------------------------------------------------------------

(deftest acedatacloud-cell-narrows-the-listing-to-the-two-ids ()
  (with-cell-stop ((acedatacloud-start))
    (with-temp-file (nle::*provider-models-cache-path*)
      (let ((asked '()))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "ACEDATACLOUD_API_KEY") "ace-key"))
          (with-stubbed-fdefinition
              (nle::http-fetch (url &rest args)
               (push url asked)
               (values (listing-body "gpt-image-2" "gpt-4.1" "claude-sonnet-5-5" "gpt-4.1-mini") 200))
            (multiple-value-bind (rows reason) (nle::list-provider-models "acedatacloud")
              (is (equal '("gpt-4.1-mini" "gpt-4.1") (mapcar (lambda (row) (getf row :id)) rows)) "the picker offers the two verified ids, in their order")
              (is (null reason)))
            (is (equal '("https://api.acedata.cloud/openai/models") asked) "one listing, of the section's base")))))))

(deftest acedatacloud-cell-answers-the-two-ids-when-the-catalog-is-unreadable ()
  (with-cell-stop ((acedatacloud-start))
    (with-temp-file (nle::*provider-models-cache-path*)
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "ACEDATACLOUD_API_KEY") "ace-key"))
        (with-stubbed-fdefinition (nle::http-fetch (url &rest args)
                                   (values nil "HTTP 401 missing key"))
          (multiple-value-bind (rows reason) (nle::list-provider-models "acedatacloud" :key "wrong")
            (is (null rows) "a refused key answers no rows")
            (is (search "HTTP 401" reason) "and the endpoint's reason, which /connect reads as refused")))
        (with-stubbed-fdefinition (nle::http-fetch (url &rest args)
                                   (values nil "connection refused" nil))
          (multiple-value-bind (rows reason) (nle::list-provider-models "acedatacloud")
            (is (equal '("gpt-4.1-mini" "gpt-4.1") (mapcar (lambda (row) (getf row :id)) rows)) "an unreachable catalog still offers the two verified ids")
            (is (search "connection refused" reason) "and says they went unchecked")))))))
