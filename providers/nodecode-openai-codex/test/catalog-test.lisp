;;;; catalog-test.lisp --- the account's own model catalogue, asked of the Codex backend.
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The stored sign-ins, HTTP listings and round responses here are fixtures;
;;;; the credential resolver and the listing and catalogue hooks are the host's.
(in-package #:nodecode.test)

(defun oc-catalogue-row (slug &key (visibility "list") (context 400000) (minimum "0.150.0")
                                   (efforts '("low" "medium" "high")) (inputs '("text" "image")))
  (nlk:json-object "slug" slug "display_name" (string-upcase slug) "visibility" visibility
                   "context_window" context "minimal_client_version" minimum
                   "supported_reasoning_levels"
                   (map 'vector (lambda (effort) (nlk:json-object "effort" effort)) efforts)
                   "input_modalities" (coerce inputs 'vector)
                   "base_instructions" "a long text no reader of the catalogue asks for"))

(defun oc-catalogue (&rest rows)
  (nlk:encode-json-object (nlk:json-object "models" (coerce rows 'vector))))

(defun oc-live-catalogue ()
  "A backend answer: a model no source file names, one bundled model, one hidden."
  (oc-catalogue (oc-catalogue-row "gpt-7-nova" :minimum "0.200.0")
                (oc-catalogue-row "gpt-6.1-sol" :context 500000 :minimum "0.153.0")
                (oc-catalogue-row "codex-auto-review" :visibility "hide")))

(defun oc-row-ids (rows) (sort (mapcar (lambda (row) (getf row :id)) rows) #'string<))

(defun oc-catalog-provider (provider)
  (nlk:json-value (nle::models-catalog-table) :object provider))

(defun oc-catalog-entry (provider model)
  (gethash model (nlk:json-value (oc-catalog-provider provider) :object "models")))

(defmacro with-native-catalogue ((&key (store '(oc-store)) base) &body body)
  "Start with a synthetic native sign-in; no catalogue is read from the user's disk."
  `(let ((nle::*provider-models-cache-path* nil))
     (with-temp-auth (auth ,store)
       (let ((nle::*auth-file-path* auth) (nle::*provider* "openai-codex") (nle::*model* "gpt-6.1-sol")
             (nle::*api-key* nil) (nle::*endpoint* nil))
         (with-cell-stop ((openai-codex-start ,@(when base `("base_url" ,base))))
           ,@body)))))

(defun oc-resolve-other-account ()
  "Resolve a turn on the same provider after its native sign-in has changed."
  (let* ((entry (nlk:json-object "provider" "openai-codex" "account_id" "acct-other"
                                 "access_token" "second-synthetic-access" "refresh_token" "rt-other"
                                 "expires_at" (+ (oc-now) 3600)))
         (store (nlk:json-object "oauth_tokens" (nlk:json-object "openai-codex" entry))))
    (with-temp-auth (auth (nlk:encode-json-object store))
      (let ((nle::*auth-file-path* auth))
        (nle::snapshot-effective-provider-config "openai-codex" "gpt-6.1-sol")))))

(defmacro with-oc-listing ((requests &optional (answer '(values (oc-live-catalogue) 200))) &body body)
  "Run BODY with every listing GET answered by ANSWER and recorded in REQUESTS."
  `(with-stubbed-fdefinition (nodecode-openai-codex::catalogue-get (url &rest keys)
                              (push (list url (getf keys :headers)) ,requests)
                              ,answer)
     ,@body))

(deftest openai-codex-cell-catalogue-lists-what-the-account-is-served ()
  ;; Given a native sign-in, when the host asks for its listing, then the
  ;; rows are the backend's: a model no source file names is offered, a hidden
  ;; one is not, and the request went to the fixed Codex backend as this login.
  (with-native-catalogue ()
    (let ((requests '()))
      (with-oc-listing (requests)
        (multiple-value-bind (rows reason) (nle::list-provider-models "openai-codex")
          (is (null reason))
          (is (equal '("gpt-6.1-sol" "gpt-7-nova") (oc-row-ids rows)))
          (let ((nova (find "gpt-7-nova" rows :key (lambda (row) (getf row :id)) :test #'equal)))
            (is (equal 400000 (getf nova :context-window)))
            (is (equal "chat" (getf nova :kind))))))
      (is (= 1 (length requests)))
      (destructuring-bind (url headers) (first requests)
        (is (eql 0 (search "https://chatgpt.com/backend-api/codex/models?client_version=" url)))
        (is (equal (format nil "Bearer ~a" +oc-access+) (oc-header headers "authorization")))
        (is (equal "acct-7" (oc-header headers "chatgpt-account-id")))))))

(deftest openai-codex-cell-catalogue-feeds-the-host-catalogue ()
  ;; The picker's metadata comes from MODELS-CATALOG-TABLE: the listed account's
  ;; row carries the live models instead of the bundled fallback and says so.
  (with-native-catalogue ()
    (let ((requests '()))
      (is (equal "bundled" (nlk:json-value (oc-catalog-provider "openai-codex") :text "catalog_source")))
      (with-oc-listing (requests) (nle::list-provider-models "openai-codex"))
      (let ((nova (oc-catalog-entry "openai-codex" "gpt-7-nova")))
        (is nova "a model the backend lists is in the catalog without a source edit")
        (is (nle::catalog-model-turn-p nova))
        (is (equal 400000 (nle::catalog-model-context nova)))
        (is (equal '("low" "medium" "high") (nle::catalog-model-efforts nova))))
      (let ((window (nle::catalog-model-context (oc-catalog-entry "openai-codex" "gpt-6.1-sol"))))
        (is (equal 500000 window) "the account's window, not the bundled one"))
      (is (null (oc-catalog-entry "openai-codex" "codex-auto-review")) "a hidden model is not offered")
      (is (null (oc-catalog-entry "openai-codex" "gpt-5.6-luna")) "a model the account is not served is not invented")
      (is (not (nle::catalog-model-turn-p (oc-catalog-entry "openai-codex" "gpt-image-2"))))
      (is (equal "live" (nlk:json-value (oc-catalog-provider "openai-codex") :text "catalog_source")))
      (is (integerp (nlk:json-value (oc-catalog-provider "openai-codex") :integer "catalog_fetched_at")))
      (is (search "catalogue" (nodecode-openai-codex::run-command "status" nil))))))

(deftest openai-codex-cell-catalogue-failures-are-reasons-and-never-live ()
  ;; Offline, refused, malformed and empty answers are each a reason; the
  ;; catalog stays the bundled fallback, labelled as such, with why.
  (with-native-catalogue ()
    (dolist (answer (list (lambda () (values nil "HTTP 503"))
                          (lambda () (values "{" 200))
                          (lambda () (values "{\"models\":[]}" 200))
                          (lambda () (values "{\"models\":[{\"display_name\":\"nameless\"}]}" 200))
                          (lambda () (values "[1]" 200))))
      (let ((requests '()))
        (with-oc-listing (requests (funcall answer))
          (multiple-value-bind (rows reason) (nle::list-provider-models "openai-codex")
            (is (null rows))
            (is (stringp reason))))
        (is (equal "bundled" (nlk:json-value (oc-catalog-provider "openai-codex") :text "catalog_source")))
        (is (stringp (nlk:json-value (oc-catalog-provider "openai-codex") :text "catalog_error")))
        (is (oc-catalog-entry "openai-codex" "gpt-6.1-sol"))))))

(deftest openai-codex-cell-catalogue-keeps-the-last-answer-through-a-failure ()
  ;; A failed refresh is not an empty catalogue: the last answer of the same
  ;; account stays, beside the reason the refresh failed.
  (with-native-catalogue ()
    (let ((requests '()))
      (with-oc-listing (requests) (nle::list-provider-models "openai-codex"))
      (with-oc-listing (requests (values nil "HTTP 503"))
        (multiple-value-bind (rows reason) (nle::list-provider-models "openai-codex")
          (is (null rows))
          (is (equal "HTTP 503" reason))))
      (is (oc-catalog-entry "openai-codex" "gpt-7-nova"))
      (is (equal "live" (nlk:json-value (oc-catalog-provider "openai-codex") :text "catalog_source")))
      (is (equal "HTTP 503" (nlk:json-value (oc-catalog-provider "openai-codex") :text "catalog_error"))))))

(deftest openai-codex-cell-catalogue-does-not-outlive-its-account ()
  ;; When the slot is signed into another account, the next credential the
  ;; slot resolves drops the old account's catalogue and the host's listing.
  (with-native-catalogue ()
    (let ((requests '()) (dropped '()))
      (with-oc-listing (requests) (nle::list-provider-models "openai-codex"))
      (is (oc-catalog-entry "openai-codex" "gpt-7-nova"))
      (with-stubbed-fdefinition (nle::drop-provider-models-cache (ids) (push ids dropped))
        (oc-resolve-other-account)
        (is (null (oc-catalog-entry "openai-codex" "gpt-7-nova")))
        (is (equal "bundled" (nlk:json-value (oc-catalog-provider "openai-codex") :text "catalog_source")))
        (is (equal '(("openai-codex")) dropped))))))

(deftest openai-codex-cell-catalogue-drops-an-answer-that-lands-after-a-switch ()
  ;; A listing asked as one account and answered after the slot moved to
  ;; another is discarded: it is a reason, and nothing of it is kept.
  (with-native-catalogue ()
    (let ((requests '()))
      (with-stubbed-fdefinition (nle::drop-provider-models-cache (ids) ids)
        (with-oc-listing (requests
                          (progn
                            (oc-resolve-other-account)
                            (values (oc-live-catalogue) 200)))
          (multiple-value-bind (rows reason) (nle::list-provider-models "openai-codex")
            (is (null rows))
            (is (stringp reason)))))
      (is (null (oc-catalog-entry "openai-codex" "gpt-7-nova"))))))

(deftest openai-codex-cell-catalogue-round-names-the-release-its-model-needs ()
  ;; The backend serves a model only to a client new enough for it. A round on
  ;; a model the catalogue says needs a newer release names that release; a
  ;; round on any other model names the release this client was ported from.
  (with-native-catalogue ()
    (let ((requests '()) (versions '()))
      (with-oc-listing (requests) (nle::list-provider-models "openai-codex"))
      (with-stubbed-fdefinition
          (dex:post (url &rest keys)
            (push (oc-header (getf keys :headers) "version") versions)
            (values (apply #'make-truncated-sse-stream +oc-stream+) 200))
        (dolist (model '("gpt-7-nova" "gpt-6.1-sol"))
          (let ((nle::*model* model))
            (is (equal "ok" (gethash "content" (nle::call-responses-streaming (user-context "hi"))))))))
      (is (equal '("0.200.0" "0.159.0") (nreverse versions))))))

(deftest openai-codex-cell-catalogue-is-asked-once-by-a-round-on-an-unknown-model ()
  ;; A cold start on a model the bundled list does not know: the round asks the
  ;; catalogue first, once, so it can name the release the model needs.
  (with-native-catalogue ()
    (let ((requests '()) (versions '()))
      (with-oc-listing (requests)
        (with-stubbed-fdefinition
            (dex:post (url &rest keys)
              (push (oc-header (getf keys :headers) "version") versions)
              (values (apply #'make-truncated-sse-stream +oc-stream+) 200))
          (let ((nle::*model* "gpt-7-nova"))
            (dotimes (round 2)
              (is (equal "ok" (gethash "content" (nle::call-responses-streaming (user-context "hi")))))))))
      (is (= 1 (length requests)))
      (is (equal '("0.200.0" "0.200.0") versions)))))

(deftest openai-codex-cell-catalogue-is-refetched-when-the-backend-says-it-moved ()
  ;; A round's x-models-etag that differs from the last one seen means the
  ;; backend's catalogue changed: the host's listing is dropped, once, so the
  ;; picker's next open asks again.
  (with-native-catalogue ()
    (let ((requests '()) (dropped '()) (etags (list "etag-1" "etag-1" "etag-2" "etag-2")))
      (with-oc-listing (requests) (nle::list-provider-models "openai-codex"))
      (with-stubbed-fdefinitions
          ((nle::drop-provider-models-cache (ids) (push ids dropped))
           (dex:post (url &rest keys)
             (let ((answer (make-hash-table :test 'equal)))
               (setf (gethash "x-models-etag" answer) (pop etags))
               (values (apply #'make-truncated-sse-stream +oc-stream+) 200 answer))))
        (dotimes (round 4)
          (is (equal "ok" (gethash "content" (nle::call-responses-streaming (user-context "hi")))))))
      (is (equal '(("openai-codex")) dropped)))))

(deftest openai-codex-cell-catalogue-survives-a-restart-without-naming-the-account ()
  ;; Stop and start with a cache directory: the catalogue is read back for the
  ;; cold start without a request, the file names neither token nor account,
  ;; and it is still dropped when the slot turns out to be another account.
  (with-native-catalogue ()
    (with-temp-directory (dir "codex-catalogue")
      (let ((nle::*provider-models-cache-path* (namestring (merge-pathnames "listing.json" dir)))
            (file (merge-pathnames "openai-codex-catalogue.json" dir))
            (requests '()))
        (with-oc-listing (requests) (nle::list-provider-models "openai-codex"))
        (funcall stop)
        (is (null (oc-catalog-provider "openai-codex")))
        (setf stop (openai-codex-start))
        (is (oc-catalog-entry "openai-codex" "gpt-7-nova"))
        (is (equal "live" (nlk:json-value (oc-catalog-provider "openai-codex") :text "catalog_source")))
        (is (= 1 (length requests)))
        (let ((text (uiop:read-file-string file)))
          (is (search "gpt-7-nova" text))
          (is (not (search "acct-7" text)))
          (is (not (search +oc-access+ text)))
          (is (not (search "a long text" text))))
        (with-stubbed-fdefinition (nle::drop-provider-models-cache (ids) ids)
          (oc-resolve-other-account))
        (is (null (oc-catalog-entry "openai-codex" "gpt-7-nova")))
        (is (not (search "gpt-7-nova" (uiop:read-file-string file))))))))

(deftest openai-codex-cell-catalogue-without-a-cache-directory-restarts-bundled ()
  (with-native-catalogue ()
    (let ((requests '()) (nle::*provider-models-cache-path* nil))
      (with-oc-listing (requests) (nle::list-provider-models "openai-codex"))
      (funcall stop)
      (setf stop (openai-codex-start))
      (is (equal "bundled" (nlk:json-value (oc-catalog-provider "openai-codex") :text "catalog_source"))))))

(deftest openai-codex-cell-catalogue-without-a-sign-in-asks-nobody ()
  (with-native-catalogue (:store "{}")
    (let ((requests '()))
      (with-stubbed-fdefinition (nle::credential-env (name) nil)
        (with-oc-listing (requests)
          (multiple-value-bind (rows reason) (nle::list-provider-models "openai-codex")
            (is (null rows))
            (is (stringp reason)))))
      (is (null requests)))))

(deftest openai-codex-cell-catalogue-refuses-an-untrusted-listing-before-resolving-credentials ()
  ;; Neither a caller's :BASE nor the cell's configured chat relay authorizes
  ;; fetching a listing there with the subscription token, even when they match.
  (with-native-catalogue (:base "https://untrusted.invalid/backend-api")
    (let ((resolved nil) (requests '()))
      (with-stubbed-fdefinition (nle::resolve-provider-credential (&rest args)
                                 (setf resolved t)
                                 (error "unexpected credential resolution"))
        (with-oc-listing (requests)
          (dolist (keys '(nil (:base "https://untrusted.invalid/backend-api/codex")))
            (multiple-value-bind (rows reason) (apply #'nle::list-provider-models "openai-codex" keys)
              (is (null rows))
              (is (stringp reason))))))
      (is (not resolved))
      (is (null requests)))))

(deftest openai-codex-cell-catalogue-does-not-follow-a-redirect ()
  (let ((calls 0))
    (with-stubbed-fdefinition (dex:get (url &rest options)
                               (declare (ignore url))
                               (incf calls)
                               (is (eql 0 (getf options :max-redirects)))
                               (values "" 302))
      (multiple-value-bind (body reason)
          (nodecode-openai-codex::catalogue-get "https://chatgpt.com/backend-api/codex/models" :timeout 5)
        (is (null body))
        (is (equal "HTTP 302" reason))))
    (is (= 1 calls))))
