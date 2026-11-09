;;;; cell-test.lisp --- the impossibl cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every read a stubbed
;;;; NLE:HTTP-FETCH: nothing touches the network, the environment or the
;;;; operator's files.

(in-package #:nodecode.test)

(define-test-slice "impossibl" "IMPOSSIBL-CELL-" :start nodecode-impossibl:start-cell)

(define-cell-lifecycle-tests "impossibl"
  (:hooks 'nle::models-catalog-table :credential 'nle::list-provider-models)
  (:refused ("timeout_seconds" 0)))

(defparameter +catalog+
  (concatenate 'string
               "{\"data\":["
               "{\"id\":\"anthropic/claude-sonnet-5\",\"name\":\"Claude Sonnet 5\","
               "\"endpoints\":[\"/v1/chat/completions\"],\"context_length\":200000},"
               "{\"id\":\"openai/gpt-image-2\",\"endpoints\":[\"/v1/images/generations\"]},"
               "{\"id\":\"convai/laya\",\"endpoints\":[\"/v1/chat/completions\"],"
               "\"supports_tools\":false},"
               "{\"id\":\"openai/gpt-5.5\",\"endpoints\":[\"/v1/chat/completions\"],"
               "\"supports_tools\":true},"
               "{\"id\":\"plain/entry\"},"
               "{\"id\":\"anthropic/claude-sonnet-5\",\"endpoints\":[\"/v1/chat/completions\"]}]}")
  "An Impossibl /v1/models answer: two chat models that take tools, an image
model, a chat model that declines tools, an entry naming no endpoints at all,
and a repeat of an id already listed.")

(defun row-ids (rows)
  "The ids ROWS carry, in order."
  (mapcar (lambda (row) (getf row :id)) rows))

(defmacro with-impossibl-fetch ((urls body) &body forms)
  "FORMS with NLE:HTTP-FETCH answered BODY for every URL, each call's (URL
HEADERS) pushed on URLS."
  `(with-stubbed-fdefinition (nle:http-fetch (url &rest args)
                               (push (list url (getf args :headers)) ,urls)
                               (values ,body 200))
     ,@forms))

(deftest impossibl-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((impossibl-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "impossibl")))
      (is (equal "Impossibl" (nlk:json-value row :string "name")))
      (is (equal "https://api.impossibl.com/v1" (nlk:json-value row :string "api")))
      (is (gethash "anthropic/claude-sonnet-5" (nlk:json-value row :object "models")) "the fallback roster is listed")
      (is (equal "openai-completions" (nle::configured-provider-lane "impossibl")) "the chat lane drives it")
      (is (equal "https://api.impossibl.com/v1/chat/completions"
                 (nle::lane-endpoint "impossibl" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "impossibl")))))

(deftest impossibl-cell-base-follows-the-section ()
  (with-cell-stop ((impossibl-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1" (nlk:json-value (nle::models-catalog-table) :string "impossibl" "api")))))

(deftest impossibl-cell-reads-impossibl-api-key ()
  (with-cell-stop ((impossibl-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "IMPOSSIBL_API_KEY") "sk-impossibl"))
        (let ((credential (nle::resolve-provider-credential "impossibl" :auth-path auth :probe t)))
          (is (equal "sk-impossibl" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential)))))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "impossibl" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))) "never the chat family's own key")
          (is (eq :public (nle:credential-source credential))))))))

(deftest impossibl-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((impossibl-start))
    (with-temp-auth (auth "{\"api_keys\":{\"impossibl\":{\"provider\":\"impossibl\",\"key\":\"sk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "IMPOSSIBL_API_KEY") "sk-env"))
        (is (equal "sk-saved"
                   (nle:credential-key (nle::resolve-provider-credential
                                        "impossibl" :auth-path auth :probe t))))))))

(deftest impossibl-cell-lists-only-the-chat-routes-that-take-tools ()
  (with-cell-stop ((impossibl-start))
    (let ((urls '()))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "IMPOSSIBL_API_KEY") "sk-impossibl"))
        (with-impossibl-fetch (urls +catalog+)
          (multiple-value-bind (rows error) (nle::list-provider-models "impossibl")
            (is (null error))
            (is (equal '("anthropic/claude-sonnet-5" "openai/gpt-5.5" "plain/entry")
                       (row-ids rows)) "the image model and the tool-declining one are left out, the repeat once")
            (is (equal "Claude Sonnet 5" (getf (first rows) :display)))
            (is (equal 200000 (getf (first rows) :context-window)))
            (let ((asked (first urls)))
              (is (equal "https://api.impossibl.com/v1/models" (first asked)))
              (is (equal "Bearer sk-impossibl"
                         (cdr (assoc "authorization" (second asked) :test #'string-equal))) "the read carries the operator's key"))))))))

(deftest impossibl-cell-falls-back-to-the-whole-listing ()
  (with-cell-stop ((impossibl-start))
    (let ((urls '()))
      (with-impossibl-fetch (urls "{\"data\":[{\"id\":\"openai/gpt-image-2\",\"endpoints\":[\"/v1/images/generations\"]}]}")
        (multiple-value-bind (rows error) (nle::list-provider-models "impossibl")
          (is (null error))
          (is (plusp (length rows)) "a catalogue with no chat model leaves the core's whole listing")
          (is (plusp (length urls))))))))

(deftest impossibl-cell-falls-back-when-the-catalog-cannot-be-read ()
  (with-cell-stop ((impossibl-start))
    (let ((urls '()))
      (with-stubbed-fdefinition (nle:http-fetch (url &rest args)
                                   (declare (ignore url args))
                                   (push t urls)
                                   (values "not the catalog" 200))
        (multiple-value-bind (rows error) (nle::list-provider-models "impossibl")
          (is (null rows) "an unreadable answer is no rows, never a failure to unwind")
          (is (stringp error) "what the core's own read of it says stands")
          (is (plusp (length urls))))))))

(deftest impossibl-cell-a-relay-base-is-not-interpreted ()
  (with-cell-stop ((impossibl-start))
    (let ((urls '()))
      (with-impossibl-fetch (urls +catalog+)
        (multiple-value-bind (rows error)
            (nle::list-provider-models "impossibl" :base "https://relay.example/v1")
          (is (null error))
          (is (member "openai/gpt-image-2" (row-ids rows) :test #'equal) "a base this section does not serve is a relay's catalogue: the core's own, unfiltered")
          (is (equal '("https://relay.example/v1/models")
                     (remove-duplicates (mapcar #'first urls) :test #'equal)) "one read, the core's, with the base the caller named"))))))

(deftest impossibl-cell-chat-only-option-off-lists-everything ()
  (with-cell-stop ((impossibl-start "chat_only" nil))
    (let ((urls '()))
      (with-impossibl-fetch (urls +catalog+)
        (multiple-value-bind (rows error) (nle::list-provider-models "impossibl")
          (is (null error))
          (is (member "convai/laya" (row-ids rows) :test #'equal) "the model that declines tools is listed")
          (is (plusp (length urls))))))))

(deftest impossibl-cell-leaves-other-providers-alone ()
  (with-cell-stop ((impossibl-start))
    (let ((urls '()))
      (with-impossibl-fetch (urls "{\"data\":[{\"id\":\"openai/gpt-image-2\"},{\"id\":\"anthropic/claude-sonnet-5\"}]}")
        (multiple-value-bind (rows error)
            (nle::list-provider-models "anthropic" :base "https://api.anthropic.com/v1")
          (is (null error) error)
          (is (equal '("anthropic/claude-sonnet-5" "openai/gpt-image-2") (row-ids rows)) "another lane's listing is its own, sorted by the core"))))))
