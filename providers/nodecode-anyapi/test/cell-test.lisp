;;;; cell-test.lisp --- the anyapi cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every read a stubbed
;;;; NLE:HTTP-FETCH: nothing touches the network, the environment or the
;;;; operator's files.

(in-package #:nodecode.test)

(define-test-slice "anyapi" "ANYAPI-CELL-" :start nodecode-anyapi:start-cell)

(define-cell-lifecycle-tests "anyapi"
  (:hooks 'nle::models-catalog-table :credential 'nle::list-provider-models)
  (:refused ("timeout_seconds" 0)))

(defparameter +catalog+
  (concatenate 'string
               "{\"data\":["
               "{\"id\":\"anthropic/claude-sonnet-4.6\",\"name\":\"Claude Sonnet 4.6\","
               "\"tags\":[\"function_calling\",\"vision\"],\"context_length\":200000},"
               "{\"id\":\"openai/gpt-image-1\",\"tags\":[\"images\"]},"
               "{\"id\":\"google/gemini-3-flash-preview\",\"tags\":[\"chat_completions:function_calling\"]},"
               "{\"id\":\"anthropic/claude-sonnet-4.6\",\"tags\":[\"function_calling\"]}]}")
  "An AnyAPI /v1/models answer: two tool-capable routes, one without the
tag, and a repeat of an id already listed.")

(defun row-ids (rows)
  "The ids ROWS carry, in order."
  (mapcar (lambda (row) (getf row :id)) rows))

(defmacro with-anyapi-fetch ((urls body) &body forms)
  "FORMS with NLE:HTTP-FETCH answered BODY for every URL, each call's (URL
HEADERS) pushed on URLS."
  `(with-stubbed-fdefinition (nle:http-fetch (url &rest args)
                               (push (list url (getf args :headers)) ,urls)
                               (values ,body 200))
     ,@forms))

(deftest anyapi-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((anyapi-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "anyapi")))
      (is (equal "AnyAPI.ai" (nlk:json-value row :string "name")))
      (is (equal "https://api.anyapi.ai/v1" (nlk:json-value row :string "api")))
      (is (gethash "anthropic/claude-sonnet-4.6" (nlk:json-value row :object "models")) "the fallback roster is listed")
      (is (equal "openai-completions" (nle::configured-provider-lane "anyapi")) "the chat lane drives it")
      (is (equal "https://api.anyapi.ai/v1/chat/completions"
                 (nle::lane-endpoint "anyapi" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "anyapi")))))

(deftest anyapi-cell-base-follows-the-section ()
  (with-cell-stop ((anyapi-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "anyapi" "api")))))

(deftest anyapi-cell-reads-anyapi-api-key ()
  (with-cell-stop ((anyapi-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "ANYAPI_API_KEY") "sk-anyapi"))
        (let ((credential (nle::resolve-provider-credential "anyapi" :auth-path auth :probe t)))
          (is (equal "sk-anyapi" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential)))))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "anyapi" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))) "never the chat family's own key")
          (is (eq :public (nle:credential-source credential))))))))

(deftest anyapi-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((anyapi-start))
    (with-temp-auth (auth "{\"api_keys\":{\"anyapi\":{\"provider\":\"anyapi\",\"key\":\"sk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "ANYAPI_API_KEY") "sk-env"))
        (is (equal "sk-saved"
                   (nle:credential-key (nle::resolve-provider-credential
                                        "anyapi" :auth-path auth :probe t))))))))

(deftest anyapi-cell-lists-only-the-tool-capable-routes ()
  (with-cell-stop ((anyapi-start))
    (let ((urls '()))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "ANYAPI_API_KEY") "sk-anyapi"))
        (with-anyapi-fetch (urls +catalog+)
          (multiple-value-bind (rows error) (nle::list-provider-models "anyapi")
            (is (null error))
            (is (equal '("anthropic/claude-sonnet-4.6" "google/gemini-3-flash-preview")
                       (row-ids rows)) "the route without the tag is left out, the repeat once")
            (is (equal "Claude Sonnet 4.6" (getf (first rows) :display)))
            (is (equal 200000 (getf (first rows) :context-window)))
            (let ((asked (first urls)))
              (is (equal "https://api.anyapi.ai/v1/models" (first asked)))
              (is (equal "Bearer sk-anyapi"
                         (cdr (assoc "authorization" (second asked) :test #'string-equal))) "the read carries the operator's key"))))))))

(deftest anyapi-cell-falls-back-to-the-whole-listing ()
  (with-cell-stop ((anyapi-start))
    (let ((urls '()))
      (with-anyapi-fetch (urls "{\"data\":[{\"id\":\"someone/untagged-chat\"},{\"id\":\"anthropic/claude-sonnet-4.6\"}]}")
        (multiple-value-bind (rows error) (nle::list-provider-models "anyapi")
          (is (null error))
          (is (equal '("anthropic/claude-sonnet-4.6" "someone/untagged-chat") (row-ids rows)) "a catalog with no tool-capable route leaves the core's whole listing")
          (is (plusp (length urls))))))))

(deftest anyapi-cell-falls-back-when-the-catalog-cannot-be-read ()
  (with-cell-stop ((anyapi-start))
    (let ((urls '()))
      (with-stubbed-fdefinition (nle:http-fetch (url &rest args)
                                   (declare (ignore url args))
                                   (push t urls)
                                   (values "not the catalog" 200))
        (multiple-value-bind (rows error) (nle::list-provider-models "anyapi")
          (is (null rows) "an unreadable answer is no rows, never a failure to unwind")
          (is (stringp error) "what the core's own read of it says stands")
          (is (plusp (length urls))))))))

(deftest anyapi-cell-a-relay-base-is-not-interpreted ()
  (with-cell-stop ((anyapi-start))
    (let ((urls '()))
      (with-anyapi-fetch (urls +catalog+)
        (multiple-value-bind (rows error)
            (nle::list-provider-models "anyapi" :base "https://relay.example/v1")
          (is (null error))
          (is (equal 3 (length rows)) "a base this section does not serve is a relay's catalog: the core's own, unfiltered")
          (is (equal '("https://relay.example/v1/models")
                     (remove-duplicates (mapcar #'first urls) :test #'equal)) "one read, the core's, with the base the caller named"))))))

(deftest anyapi-cell-tool-capable-option-off-lists-everything ()
  (with-cell-stop ((anyapi-start "tool_capable" nil))
    (let ((urls '()))
      (with-anyapi-fetch (urls +catalog+)
        (multiple-value-bind (rows error) (nle::list-provider-models "anyapi")
          (is (null error))
          (is (equal 3 (length rows)) "every route, tool-capable or not")
          (is (plusp (length urls))))))))

(deftest anyapi-cell-leaves-other-providers-alone ()
  (with-cell-stop ((anyapi-start))
    (let ((urls '()))
      (with-anyapi-fetch (urls "{\"data\":[{\"id\":\"someone/untagged-chat\"},{\"id\":\"anthropic/claude-sonnet-4.6\"}]}")
        (multiple-value-bind (rows error)
            (nle::list-provider-models "anthropic" :base "https://api.anthropic.com/v1")
          (is (null error) error)
          (is (equal '("anthropic/claude-sonnet-4.6" "someone/untagged-chat") (row-ids rows)) "another lane's listing is its own, sorted by the core"))))))
