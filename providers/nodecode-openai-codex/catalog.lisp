;;;; catalog.lisp --- the account's own model catalogue, asked of the Codex backend.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Which models a ChatGPT subscription is served is the backend's word, per
;;;; account and per client release: GET <codex base>/models?client_version=V
;;;; answers {"models": [...]}, each row naming the oldest client release it is
;;;; served to (minimal_client_version). The official client asks the same
;;;; address (codex-rs/codex-api endpoint/models.rs) and oh-my-pi does too
;;;; (packages/catalog/src/discovery/codex.ts).
;;;;
;;;; One catalogue, this cell's, per provider id and account:
;;;;
;;;;   LIST-PROVIDER-MODELS   the host's listing of a Codex provider is this
;;;;                          GET, as the login the provider resolves, only
;;;;                          ever at the backend that login is served at
;;;;   MODELS-CATALOG-TABLE   the provider's row carries the listed models with
;;;;                          what the backend says of each, and says whether
;;;;                          it is the account's answer ("live") or the
;;;;                          bundled fallback ("bundled"), and why
;;;;   a round                names the release its model needs, so a model
;;;;                          newer than the release this client was ported
;;;;                          from is served without a source edit
;;;;
;;;; The host owns when: its listing cache, its TTL, its picker's background
;;;; refresh and /models refresh ask this cell; the cell starts no thread and
;;;; no timer. What it keeps is the last answer per provider, under a digest of
;;;; the account that was asked, never the account or a token.

(in-package #:nodecode-openai-codex)

(defparameter +listing-client-version+ "99.0.0"
  "The release a listing says it is when the section names none: newer than
every release, so the answer is every model the account is served and each
row's minimal_client_version says what a round on it must name.")

(defstruct (snapshot (:copier nil))
  "One provider's last catalogue answer."
  ;; a digest of the provider id and the account asked, never the account
  (scope nil)
  ;; the client_version the listing was asked with
  (version nil)
  (fetched-at nil)
  ;; the answer's rows in models.json's shape, plus visibility and min_version
  (rows #())
  ;; the newest x-models-etag a round of this provider was answered with
  (round-etag nil))

(defvar *catalogue-lock* (bt2:make-lock :name "openai-codex catalogue"))
(defvar *snapshots* (make-hash-table :test 'equal) "Provider id -> SNAPSHOT.")
(defvar *listing-errors* (make-hash-table :test 'equal)
  "Provider id -> why its last listing was not answered.")
(defvar *scopes* (make-hash-table :test 'equal)
  "Provider id -> (DIGEST). Cell identity is the account generation.")
(defvar *bootstraps* (make-hash-table :test 'equal)
  "Provider id -> when a round last asked the catalogue for want of one.")
(defvar *catalogue-generation* 0
  "Moves whenever what CATALOG-ROW would answer moves.")

;;; --- versions ----------------------------------------------------------------------

(defun version-parts (text)
  "TEXT, a dotted release, as integers; NIL when it is none."
  (and (stringp text) (cl-ppcre:scan "\\A[0-9]{1,6}(?:\\.[0-9]{1,6}){0,3}\\z" text)
       (mapcar #'parse-integer (cl-ppcre:split "\\." text))))

(defun version< (a b &aux (a (version-parts a)) (b (version-parts b)))
  "Whether release A is older than release B; false when either is no release."
  (and a b
       (loop for x = (or (pop a) 0) for y = (or (pop b) 0)
             do (cond ((< x y) (return t)) ((> x y) (return nil))
                      ((and (null a) (null b)) (return nil))))))

(defun listing-version (&aux (configured (setting :client-version)))
  "The client_version a listing is asked with."
  (if (version-parts configured) configured +listing-client-version+))

;;; --- whose catalogue ---------------------------------------------------------------

(defparameter +listing-base+ "https://chatgpt.com/backend-api/codex")

(defun credential-scope (credential)
  "Use the stable account id across grant rotation. Opaque keys receive their
own scope instead of all sharing one; only a digest ever lives in the cache."
  (or (cdr (assoc "chatgpt-account-id" (getf (getf credential :attributes) :headers) :test #'string-equal))
      (nlk::sha256-text (nle:credential-key credential))))

(defun scope-digest (provider scope)
  (nlk::sha256-text (format nil "~a|~a" provider scope)))

(defun snapshot-file ()
  "Where the catalogue is kept for a cold start: beside the host's listing
cache, and nowhere when the host keeps none."
  (and nle::*provider-models-cache-path*
       (merge-pathnames "openai-codex-catalogue.json" nle::*provider-models-cache-path*)))

(defun save-snapshots (&aux (file (snapshot-file)))
  "Write every snapshot whole, in one rename."
  (when file
    (let ((providers (make-hash-table :test 'equal)))
      (bt2:with-lock-held (*catalogue-lock*)
        (maphash (lambda (provider snapshot)
                   (setf (gethash provider providers)
                         (nlk:json-object "scope" (snapshot-scope snapshot)
                                          "client_version" (snapshot-version snapshot)
                                          "fetched_at" (snapshot-fetched-at snapshot)
                                          "models" (snapshot-rows snapshot))))
                 *snapshots*)
        (ignore-errors
         (nlk:write-file-atomically
          file (nlk:encode-json-object (nlk:json-object "version" 1 "providers" providers))
          :mode #o600))))))

(defun saved-scope ()
  "Which native account owns the persisted snapshot? Read the local sign-in
only; no grant refresh or network call at startup. Fail closed if unknown."
  (when (equal +listing-base+ (codex-base (setting :base-url)))
    (let* ((entry (stored-entry (nle::read-auth-file nle::*auth-file-path*)))
           (key (or (nlk:json-value entry :text "access_token") (some #'nle::credential-env +env+))))
      (when key
        (scope-digest +provider+ (or (token-profile key) (nlk:json-value entry :text "account_id")
                                     (nlk::sha256-text key)))))))

(defun load-snapshots (&aux (file (snapshot-file)))
  "Restore the native account's snapshot only when its current scope agrees."
  (when (and file (probe-file file))
    (let* ((entry (ignore-errors (nlk:json-value (nlk:decode-json (uiop:read-file-string file))
                                                 :object "providers" +provider+)))
           (scope (nlk:json-value entry :text "scope"))
           (version (nlk:json-value entry :text "client_version"))
           (fetched-at (nlk:json-value entry :integer "fetched_at"))
           (rows (nlk:json-value entry :array "models")))
      (when (and scope version fetched-at rows (every #'hash-table-p rows)
                 (equal scope (ignore-errors (saved-scope))))
        (bt2:with-lock-held (*catalogue-lock*)
          (setf (gethash +provider+ *snapshots*)
                (make-snapshot :scope scope :version version :fetched-at fetched-at :rows rows))
          (incf *catalogue-generation*))))))

(defun forget-catalogue (provider &aux dropped)
  "Drop PROVIDER's catalogue and the host's listing of it."
  (bt2:with-lock-held (*catalogue-lock*)
    (setf dropped (remhash provider *snapshots*))
    (remhash provider *scopes*)
    (remhash provider *listing-errors*)
    (remhash provider *bootstraps*)
    (incf *catalogue-generation*))
  (when dropped (save-snapshots))
  (ignore-errors (nle::drop-provider-models-cache (list provider))))

(defun note-account (provider scope &aux (digest (scope-digest provider scope)) owner stale)
  "Return the resolved digest and its unique generation object. A switch away
and back, logout, or a cell stop invalidates the earlier request's object."
  (bt2:with-lock-held (*catalogue-lock*)
    (let ((known (gethash provider *scopes*)) (snapshot (gethash provider *snapshots*)))
      (setf owner (if (equal (car known) digest) known
                      (setf (gethash provider *scopes*) (list digest))))
      (when (and snapshot (not (equal (snapshot-scope snapshot) digest)))
        (setf stale (remhash provider *snapshots*))
        (remhash provider *listing-errors*)
        (remhash provider *bootstraps*)
        (incf *catalogue-generation*))))
  (when stale
    (save-snapshots)
    (ignore-errors (nle::drop-provider-models-cache (list provider))))
  (values digest owner))

(defun clear-catalogue ()
  "Forget everything in memory; the file stays for the next start."
  (bt2:with-lock-held (*catalogue-lock*)
    (clrhash *snapshots*) (clrhash *listing-errors*) (clrhash *scopes*) (clrhash *bootstraps*)
    (incf *catalogue-generation*)))

(defun current-snapshot (provider)
  "PROVIDER's catalogue, when it was asked as the release a listing asks as now."
  (let ((snapshot (bt2:with-lock-held (*catalogue-lock*) (gethash provider *snapshots*))))
    (and snapshot (equal (snapshot-version snapshot) (listing-version)) snapshot)))

;;; --- the answer --------------------------------------------------------------------

(defun bundled-row (id)
  (find id +models+ :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

(defun live-row (model &aux (id (nlk:json-value model :text "slug")))
  "One row of the backend's answer in models.json's shape, or NIL when it names
no model. What the backend does not say (the output cap, a price) is the
bundled row's when there is one, else left unsaid."
  (when (and id (plusp (length id)) (< (length id) 200)
             (notany (lambda (ch) (or (< (char-code ch) 33) (= (char-code ch) 127))) id))
    (let ((bundled (bundled-row id))
          (efforts (loop for level across (or (nlk:json-value model :array "supported_reasoning_levels") #())
                         for effort = (nlk:json-value level :text "effort")
                         when effort collect effort)))
      (nlk:json-object
       "id" id
       "name" (or (nlk:json-value model :text "display_name") id)
       :opt "context" (nlk:json-value model :integer "context_window")
       :opt "output" (nlk:json-value bundled :integer "output")
       :when efforts "reasoning" t
       "efforts" (coerce efforts 'vector)
       "input" (let ((inputs (remove-if-not #'stringp (or (nlk:json-value model :array "input_modalities") #()))))
                 (if (plusp (length inputs)) inputs #("text")))
       :opt "cost" (nlk:json-value bundled :object "cost")
       "visibility" (or (nlk:json-value model :text "visibility") "none")
       :opt "min_version" (let ((minimum (nlk:json-value model :text "minimal_client_version")))
                            (and (version-parts minimum) minimum))))))

(defun listed-row-p (row)
  "Whether the backend offers ROW for picking (the official client's show_in_picker)."
  (and (equal "list" (nlk:json-value row :text "visibility"))
       (not (member (nlk:json-value row :text "id") +image-models+ :test #'equal))))

(defun parse-catalogue (text)
  "The rows of the answer TEXT, or NIL when it is no catalogue."
  (let* ((answer (ignore-errors (nlk:decode-json text)))
         (models (and (hash-table-p answer) (nlk:json-value answer :array "models")))
         (rows (remove nil (map 'list (lambda (model) (and (hash-table-p model) (live-row model)))
                                (or models #())))))
    (and (some #'listed-row-p rows)
         (coerce (remove-duplicates rows :key (lambda (row) (nlk:json-value row :text "id"))
                                         :test #'equal :from-end t)
                 'vector))))

(defun listing-rows (snapshot)
  "SNAPSHOT as the host's listing rows: the models offered for a turn."
  (loop for row across (snapshot-rows snapshot)
        when (listed-row-p row)
          collect (list :id (nlk:json-value row :text "id")
                        :display (nlk:json-value row :text "name")
                        :context-window (nlk:json-value row :integer "context")
                        :kind "chat")))

(defun listing-failed (provider reason)
  "(values NIL REASON), REASON kept beside whatever catalogue PROVIDER has."
  (bt2:with-lock-held (*catalogue-lock*)
    (setf (gethash provider *listing-errors*) reason)
    (incf *catalogue-generation*))
  (values nil reason))

(defun listing-error (provider)
  "Why PROVIDER's last listing was not answered, or NIL."
  (bt2:with-lock-held (*catalogue-lock*) (gethash provider *listing-errors*)))

(defun listing (next provider &rest keys &key key (base nil base-p) &allow-other-keys)
  "The signed-in provider's own model listing; every other provider stays with NEXT."
  (cond ((not (equal provider +provider+)) (apply next provider keys))
        (key (values nil "openai-codex: listing uses the signed-in account, not an explicit key"))
        (t (codex-listing provider (if base-p base (nle::provider-base-url provider))))))

(defun catalogue-get (url &key headers timeout proxy)
  "The host's generic fetch follows redirects; a subscription bearer must not.
Never return a transport condition or a response body as an error message."
  (handler-case
      (sb-sys:with-deadline (:seconds timeout)
        (multiple-value-bind (body status)
            (dex:get url :headers (nle::with-user-agent headers) :max-redirects 0
                         :connect-timeout timeout :read-timeout timeout :keep-alive nil
                         :use-connection-pool nil :proxy (nlk:proxy-for url proxy))
          (if (eql status 200) (values (body-string body) status)
              (values nil (format nil "HTTP ~d" status)))))
    (dex:http-request-failed (condition) (values nil (format nil "HTTP ~d" (dex:response-status condition))))
    ((or error sb-sys:deadline-timeout) () (values nil "Codex listing unavailable"))))

(defun codex-listing (provider base)
  "(values ROWS ERROR), LIST-PROVIDER-MODELS for a Codex provider: the models
its account is served, asked of the Codex backend as the login PROVIDER
resolves; never signals."
  ;; Never include a credential or transport condition in a diagnostic.
  (handler-case (ask-catalogue provider base)
    (error (condition)
      (listing-failed provider (format nil "openai-codex: the model listing failed (~(~a~))"
                                       (type-of condition))))))

(defun ask-catalogue (provider base)
  "CODEX-LISTING's exchange."
  ;; The address is checked before any login is read: a base that is not the
  ;; backend this login is served at gets no request and no credential.
  (unless (and (stringp base) (equal (string-right-trim "/" base) +listing-base+))
    (return-from ask-catalogue
      (values nil "openai-codex: a model listing is asked only of the Codex backend its sign-in is served at")))
  (let ((credential (handler-case (nle::resolve-provider-credential provider :endpoint +listing-base+)
                      (nle:credential-error ()
                        (return-from ask-catalogue
                          (listing-failed provider "openai-codex: no usable sign-in for listing"))))))
    (when (equal "public" (nle:credential-key credential))
      (return-from ask-catalogue (listing-failed provider "openai-codex: not signed in")))
    (multiple-value-bind (digest owner) (note-account provider (credential-scope credential))
      (let ((version (listing-version)))
        (multiple-value-bind (body status)
            (catalogue-get (format nil "~a/models?client_version=~a" +listing-base+ version)
                             :headers `(("authorization" . ,(format nil "Bearer ~a" (nle:credential-key credential)))
                                        ,@(getf (getf credential :attributes) :headers)
                                        ("originator" . ,(setting :originator))
                                        ("version" . ,version)
                                        ("accept" . "application/json"))
                             :timeout nle::*provider-models-fetch-timeout-seconds*
                             :proxy (nle::trimmed-config-string (nle::configured-provider-entry provider) "proxy"))
          (let ((rows (and (stringp body) (parse-catalogue body))))
            (cond ((not (stringp body))
                   (listing-failed provider (if (stringp status) status "no answer")))
                  ((null rows)
                   (listing-failed provider (format nil "the Codex backend listed no model for client_version ~a" version)))
                  (t
                   (let ((snapshot (make-snapshot :scope digest :version version
                                                  :fetched-at (get-universal-time) :rows rows)))
                     ;; An answer that lands after the slot moved to another
                     ;; account is that other account's to ask, not this one's
                     ;; to keep.
                     (if (bt2:with-lock-held (*catalogue-lock*)
                           (when (eq owner (gethash provider *scopes*))
                             (setf (gethash provider *snapshots*) snapshot)
                             (remhash provider *listing-errors*)
                             (incf *catalogue-generation*)))
                         (progn (save-snapshots) (values (listing-rows snapshot) nil))
                         (values nil "openai-codex: the account changed while its models were listed")))))))))))

;;; --- the catalog's row -------------------------------------------------------------

(defun catalog-row (&optional prior (provider +provider+))
  "PROVIDER as a models.dev provider: this cell's lane, the section's base,
the token variable, and its models: the ones its account's catalogue lists
beside the bundled image models, else, while the account has not answered,
the bundled models over PRIOR's. catalog_source says which."
  (let ((models (make-hash-table :test 'equal))
        (snapshot (current-snapshot provider))
        (failure (listing-error provider)))
    (flet ((add (row) (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row))))
      (cond (snapshot
             (loop for row across +models+
                   when (member (nlk:json-value row :string "id") +image-models+ :test #'equal)
                     do (add row))
             (loop for row across (snapshot-rows snapshot)
                   when (listed-row-p row) do (add row)))
            (t
             (alexandria:when-let (published (nlk:json-value prior :object "models"))
               (maphash (lambda (id model) (setf (gethash id models) model)) published))
             (map nil #'add +models+))))
    (nlk:json-object "name" "ChatGPT Plus/Pro (Codex Subscription)"
                     "npm" +key+
                     "api" (codex-base (setting :base-url))
                     "env" (coerce +env+ 'vector)
                     "models" models
                     "catalog_source" (if snapshot "live" "bundled")
                     :opt "catalog_fetched_at" (and snapshot (snapshot-fetched-at snapshot))
                     :opt "catalog_error" failure)))

;;; --- what a round reads of it ------------------------------------------------------

(defun catalogue-model (provider model &aux (snapshot (current-snapshot provider)))
  "MODEL's row in PROVIDER's catalogue, or NIL."
  (and snapshot (stringp model)
       (find model (snapshot-rows snapshot)
             :key (lambda (row) (nlk:json-value row :text "id")) :test #'equal)))

(defun round-version (config)
  "The release a round under CONFIG says it is: the one this client was
ported from, or the newer one the catalogue says its model needs."
  (let ((minimum (nlk:json-value (catalogue-model (nle::effective-provider-config-provider config)
                                                  (nle::effective-provider-config-model config))
                                 :text "min_version")))
    (if (version< +client-version+ minimum) minimum +client-version+)))

(defun ensure-catalogue (config)
  "Before a round on a model neither the catalogue nor the bundled list knows:
ask the catalogue, at most once a minute, so the round can name the release
the model needs."
  (let ((provider (nle::effective-provider-config-provider config))
        (model (nle::effective-provider-config-model config)))
    (when (and (stringp provider) (stringp model)
               (null (catalogue-model provider model))
               (null (bundled-row model))
               (bt2:with-lock-held (*catalogue-lock*)
                 (let ((asked (gethash provider *bootstraps*)) (now (get-universal-time)))
                   (when (or (null asked) (> (- now asked) 60))
                     (setf (gethash provider *bootstraps*) now)))))
      (ignore-errors (codex-listing provider (nle::provider-base-url provider))))))

(defun note-models-etag (provider etag &aux moved)
  "A round of PROVIDER was answered with the models ETAG: one that differs
from the last seen means the backend's catalogue moved, so the host's listing
is dropped and its next open asks again."
  (when (and (stringp provider) (stringp etag))
    (bt2:with-lock-held (*catalogue-lock*)
      (alexandria:when-let (snapshot (gethash provider *snapshots*))
        (setf moved (and (snapshot-round-etag snapshot)
                         (not (equal etag (snapshot-round-etag snapshot))))
              (snapshot-round-etag snapshot) etag)))
    (when moved (ignore-errors (nle::drop-provider-models-cache (list provider))))))

(defun catalogue-status ()
  "What /openai-codex status says of the catalogue."
  (let ((lines '()) (now (get-universal-time)))
    (bt2:with-lock-held (*catalogue-lock*)
      (maphash (lambda (provider snapshot)
                 (push (format nil "catalogue ~a: the account's own listing, ~d model~:p, asked ~d minute~:p ago as client ~a~@[; the last refresh failed: ~a~]"
                               provider (count-if #'listed-row-p (snapshot-rows snapshot))
                               (floor (- now (snapshot-fetched-at snapshot)) 60)
                               (snapshot-version snapshot) (gethash provider *listing-errors*))
                       lines))
               *snapshots*)
      (maphash (lambda (provider reason)
                 (unless (gethash provider *snapshots*)
                   (push (format nil "catalogue ~a: the bundled fallback, not the account's listing: ~a" provider reason)
                         lines)))
               *listing-errors*))
    (format nil "~{~a~^~%~}"
            (or (sort lines #'string<)
                ;; nothing asked yet, or asked and refused before any login was read
                (list "catalogue: the bundled fallback; the account's own listing is asked when /models opens")))))
