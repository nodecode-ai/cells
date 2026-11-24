;;;; cell-test.lisp --- the hopper cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every list is a temp file, every catalog a fixture file, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, and the one wire to OpenRouter a
;;;; stubbed dex:post. Nothing leaves the machine.

(in-package #:nodecode.test)

(define-test-slice "hopper" "HOPPER-CELL-" :start nodecode-hopper:start-cell)

(define-cell-lifecycle-tests "hopper"
  (:hooks 'nle::models-catalog-table 'nle::list-provider-models :credential)
  (:command "hopper")
  (:help :hopper)
  (:refused ("base_url" 5) ("models_file" 7)))

(defparameter *hopper-catalog*
  "{\"openrouter\":{\"id\":\"openrouter\",\"name\":\"OpenRouter\",\"npm\":\"@openrouter/ai-sdk-provider\",\"api\":\"https://openrouter.ai/api/v1\",\"env\":[\"OPENROUTER_API_KEY\"],\"models\":{\"acme/known\":{\"id\":\"acme/known\",\"name\":\"Known\",\"tool_call\":true,\"limit\":{\"context\":64000,\"output\":8000}},\"acme/not-listed\":{\"id\":\"acme/not-listed\",\"name\":\"Not Listed\",\"tool_call\":true}}}}"
  "A models.dev document holding OpenRouter as models.dev publishes it: one
model the list names, one it does not.")

(defparameter *two-models*
  "acme/known
# a comment, and a blank line under it

acme/unknown:free
"
  "A list file naming one model the catalog knows and one it does not.")

(defun hopper-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun hopper-stream ()
  "One whole chat answer, `ok'."
  (make-truncated-sse-stream
   "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
   "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
   "[DONE]"))

(defmacro with-hopper-list ((file contents) &body body)
  "BODY with FILE a fresh temp list holding CONTENTS and the cell started on it."
  `(with-temp-file (,file :type "txt" :contents ,contents)
     (with-cell-stop ((hopper-start "models_file" ,file))
       ,@body)))

;;; --- the catalog, the lane, the address --------------------------------------------

(deftest hopper-cell-puts-its-row-in-the-catalog ()
  (with-catalog-fixture (catalog *hopper-catalog*)
    (with-hopper-list (file *two-models*)
      (let* ((table (nle::models-catalog-table))
             (row (nlk:json-value table :object "hopper"))
             (models (nlk:json-value row :object "models")))
        (is (equal "Hopper" (nlk:json-value row :text "name")))
        (is (equal "https://openrouter.ai/api/v1" (nlk:json-value row :text "api")))
        (is (gethash "acme/known" models) "the list's own first id is offered")
        (is (gethash "acme/unknown:free" models) "and its second, unknown to every source")
        (is (not (gethash "acme/not-listed" models)) "a model the list does not name is not")
        (is (equal "Known" (nle::catalog-model-name (gethash "acme/known" models))))
        (is (eql 64000 (nle::catalog-model-context (gethash "acme/known" models))) "a model models.dev publishes keeps its window")
        (is (null (nle::catalog-model-context (gethash "acme/unknown:free" models))) "an unknown one keeps an unknown window, never a guess")
        (is (nle::catalog-model-turn-p (gethash "acme/unknown:free" models)) "an unknown id still runs a turn")
        (is (gethash "openrouter" table) "models.dev's own rows are kept")
        (is (equal "openai-completions" (nle::configured-provider-lane "hopper")) "the chat lane drives it")
        (is (equal "https://openrouter.ai/api/v1/chat/completions"
                   (nle::lane-endpoint "hopper" "openai-completions"))))
      (funcall stop)
      (setf stop nil)
      (is (null (nlk:json-value (nle::models-catalog-table) :object "hopper")) "the stop takes the row back"))))

(deftest hopper-cell-lists-only-the-ids-the-file-names ()
  (with-catalog-fixture (catalog *hopper-catalog*)
    (with-hopper-list (file *two-models*)
      (multiple-value-bind (rows error) (nle::list-provider-models "hopper")
        (is (null error))
        (is (equal '("acme/known" "acme/unknown:free") (mapcar (lambda (row) (getf row :id)) rows)) "the listing is the list, never OpenRouter's roster")
        (is (equal "Known" (getf (first rows) :display)))
        (is (eql 64000 (getf (first rows) :context-window)))))))

(deftest hopper-cell-a-comment-only-list-serves-nothing ()
  (with-catalog-fixture (catalog *hopper-catalog*)
    (with-hopper-list (file "# nothing yet
")
      (is (zerop (hash-table-count (nlk:json-value (nle::models-catalog-table) :object "hopper" "models"))) "an empty list is a row with no models, not the whole roster")
      (multiple-value-bind (rows error) (nle::list-provider-models "hopper")
        (is (and (null rows) (null error))))
      (is (search "no models" (cell-entry "nodecode-hopper" "hopper" "")) "and /hopper says how to add one"))))

(deftest hopper-cell-answers-its-own-listing-without-asking ()
  (with-catalog-fixture (catalog *hopper-catalog*)
    (with-hopper-list (file "acme/known")
      (is (equal '("acme/known")
                 (mapcar (lambda (row) (getf row :id))
                         (nodecode-hopper::listing (lambda (&rest args)
                                                     (declare (ignore args))
                                                     (error "hopper asked the core for its own listing"))
                                                   "hopper")))))))

(deftest hopper-cell-leaves-another-provider-alone ()
  (let ((asked nil))
    (is (equal "why"
               (nth-value 1 (nodecode-hopper::listing
                             (lambda (&rest args) (setf asked args) (values '((:id "x")) "why"))
                             "openrouter" :key "sk-or-test"))))
    (is (equal '("openrouter" :key "sk-or-test") asked) "next asked, as it came")))

(deftest hopper-cell-checks-a-key-where-it-is-spent ()
  (with-catalog-fixture (catalog *hopper-catalog*)
    (with-hopper-list (file "acme/known")
      (let ((url nil) (headers nil))
        (with-stubbed-fdefinition (nle::http-fetch (asked &rest args)
                                   (setf url asked headers (getf args :headers))
                                   (values "{\"data\":[{\"id\":\"openrouter/auto\"}]}" 200))
          (multiple-value-bind (rows error) (nle::list-provider-models "hopper" :key "sk-or-test")
            (is (null rows) "the key check keeps none of OpenRouter's rows")
            (is (null error) "a listing that answered is a key that works")))
        (is (equal "https://openrouter.ai/api/v1/models" url) "asked at OpenRouter, where the key is spent")
        (is (equal "Bearer sk-or-test" (hopper-header headers "authorization"))))
      (with-stubbed-fdefinition (nle::http-fetch (&rest args)
                                 (or (null args) (values nil "HTTP 401 invalid_api_key")))
        (is (search "401" (nth-value 1 (nle::list-provider-models "hopper" :key "sk-or-bad"))) "a refused key is a refusal, not a pass")))))

;;; --- the key -------------------------------------------------------------------------

(deftest hopper-cell-reads-openrouter-api-key ()
  (with-temp-auth (auth "{}")
    (with-hopper-list (file "acme/known")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENROUTER_API_KEY") "sk-or-env"))
        (let ((credential (nle::resolve-provider-credential "hopper" :auth-path auth :probe t)))
          (is (equal "sk-or-env" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest hopper-cell-never-sends-another-familys-key ()
  (with-temp-auth (auth "{}")
    (with-hopper-list (file "acme/known")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "hopper" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest hopper-cell-saved-key-outranks-the-variable ()
  (with-temp-auth (auth "{\"api_keys\":{\"hopper\":{\"provider\":\"hopper\",\"key\":\"sk-or-saved\"}}}")
    (with-hopper-list (file "acme/known")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENROUTER_API_KEY") "sk-or-env"))
        (is (equal "sk-or-saved"
                   (nle:credential-key (nle::resolve-provider-credential "hopper" :auth-path auth :probe t))))))))

;;; --- one round -----------------------------------------------------------------------

(deftest hopper-cell-sends-the-id-as-the-file-writes-it ()
  (with-catalog-fixture (catalog *hopper-catalog*)
    (with-hopper-list (file "acme/unknown:free")
      (let ((nle::*provider* "hopper") (nle::*model* "acme/unknown:free") (nle::*api-key* "sk-or-test")
            (nle::*endpoint* nil) (nle::*reasoning-effort* nil) (nle::*max-completion-tokens* nil)
            (nle::*model-capability-memo* nil)
            (url nil) (headers nil) (body nil))
        (with-stubbed-fdefinition (dex:post (asked &rest args)
                                   (setf url asked headers (getf args :headers)
                                         body (nlk:decode-json (getf args :content)))
                                   (values (hopper-stream) 200))
          (nle::call-provider-streaming (user-context)))
        (is (equal "https://openrouter.ai/api/v1/chat/completions" url))
        (is (equal "Bearer sk-or-test" (hopper-header headers "authorization")))
        (is (equal "acme/unknown:free" (nlk:json-value body :string "model")) "the id rides the wire exactly as the line wrote it")))))

;;; --- /hopper ---------------------------------------------------------------------------

(deftest hopper-cell-adds-removes-and-resets ()
  (with-temp-file (file :type "txt")
    (with-cell-stop ((hopper-start "models_file" file))
      (is (search "inclusionai/ling-3.0-flash-vl:free"
                  (cell-entry "nodecode-hopper" "hopper" "list")) "a list written for the first time holds the original's default model")
      (is (search "Added 1 model: acme/known"
                  (cell-entry "nodecode-hopper" "hopper" "add acme/known")))
      (is (equal '("inclusionai/ling-3.0-flash-vl:free" "acme/known") (nodecode-hopper::read-models file)) "the file holds the list")
      (is (search "Removed 1 model: acme/known"
                  (cell-entry "nodecode-hopper" "hopper" "remove acme/known")))
      (is (equal '("inclusionai/ling-3.0-flash-vl:free") (nodecode-hopper::read-models file)))
      (is (search "not an OpenRouter model id"
                  (cell-entry "nodecode-hopper" "hopper" "add no-slash")) "an id with no provider is refused, and nothing is written")
      (is (equal '("inclusionai/ling-3.0-flash-vl:free") (nodecode-hopper::read-models file)))
      (is (equal (namestring (pathname file))
                 (cell-entry "nodecode-hopper" "hopper" "file")))
      (is (search "default" (cell-entry "nodecode-hopper" "hopper" "reset")))
      (is (search "not one of its choices"
                  (cell-entry "nodecode-hopper" "hopper" "frobnicate"))))))

(deftest hopper-cell-a-changed-list-reaches-the-picker ()
  (with-catalog-fixture (catalog *hopper-catalog*)
    (with-hopper-list (file "acme/known")
      (is (gethash "acme/known" (nlk:json-value (nle::models-catalog-table) :object "hopper" "models")))
      (cell-entry "nodecode-hopper" "hopper" "add acme/unknown:free")
      (let ((models (nlk:json-value (nle::models-catalog-table) :object "hopper" "models")))
        (is (gethash "acme/unknown:free" models) "the new id is in the catalog at once")
        (is (= 2 (hash-table-count models)))))))
