;;;; cell.lisp --- the cell: GitHub Copilot among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Seven hooks, each declining for every provider but github-copilot, and one
;;;; slash command:
;;;;
;;;;   MODELS-CATALOG-TABLE    the catalog carries Copilot's row: this
;;;;                           section's base and omp's bundled models over
;;;;                           whatever models.dev published, so /connect
;;;;                           offers it and /models lists its models
;;;;   RESOLVE-MODEL-LANE      each model rides the wire Copilot serves it on
;;;;                           (Messages, Responses or chat), unless the
;;;;                           operator's config pins one
;;;;   LANE-ENDPOINT           a Messages model is served under /v1
;;;;   :CREDENTIAL             the GitHub token the sign-in kept (refreshed
;;;;                           first when it is due), else COPILOT_GITHUB_TOKEN;
;;;;                           either moves the round to the account's own host
;;;;   ANTHROPIC-REQUEST-BODY  no Anthropic beta: Copilot refuses them
;;;;   WALK-PROVIDER-STREAM    the bearer, the Copilot CLI's identity, who
;;;;                           started the request and whether it carries an
;;;;                           image; a refused chat identity is tried once
;;;;                           more as the CLI, and what worked is remembered
;;;;   LIST-PROVIDER-MODELS    /models asks the account's host for its chat
;;;;                           models, with the Copilot CLI's identity
;;;;   /github-copilot         login [ENTERPRISE-DOMAIN], logout, status
;;;;
;;;; Config, a sibling top-level key:
;;;;   "github-copilot": {"base_url": "https://api.githubcopilot.com"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-github-copilot)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a github-copilot round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

;;; --- the catalog ---------------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Copilot's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Copilot's row, made once per
catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

;;; --- which wire, at which address ------------------------------------------------

(defun operator-pinned-p (model)
  "Whether the operator's config names the wire MODEL rides at Copilot."
  (or (nle::trimmed-config-string (nle::configured-model-entry +provider+ model) "sdk")
      (nle::trimmed-config-string (nle::configured-provider-entry +provider+) "sdk")))

(defun lane (next provider model)
  "RESOLVE-MODEL-LANE advice: a Copilot model rides the wire Copilot serves it on."
  (or (and (equal provider +provider+) (stringp model) (not (operator-pinned-p model))
           (model-lane model))
      (funcall next provider model)))

(defun endpoint (next provider lane)
  "LANE-ENDPOINT advice: Copilot serves Messages under /v1, where the
Anthropic SDK omp drives appends it."
  (let ((base (and (equal provider +provider+) (nle::provider-base-url provider))))
    (if (and base (equal "anthropic" (or lane (nle::resolve-model-lane provider nle::*model*))))
        (concatenate 'string (string-right-trim "/" base) "/v1/messages")
        (funcall next provider lane))))

(defun rebased (endpoint &key enterprise api-endpoint)
  "ENDPOINT moved to the account's own host, or NIL when it stays where it is."
  (let ((configured (and endpoint (nle::provider-base-url +provider+))))
    (when configured
      (let ((prefix (string-right-trim "/" configured))
            (base (account-base configured :enterprise enterprise :api-endpoint api-endpoint)))
        (when (and (uiop:string-prefix-p prefix endpoint) (not (equal base prefix)))
          (concatenate 'string base (subseq endpoint (length prefix))))))))

;;; --- the credential ----------------------------------------------------------------

(defvar *env-endpoints* (make-hash-table :test #'equal :synchronized t)
  "A GitHub token's digest -> the plan host GitHub named for it, or :NONE.")

(defun env-endpoint (token)
  "The plan host of the environment's TOKEN, asked of GitHub once per process."
  (let* ((digest (nlk:sha256-text token))
         (known (gethash digest *env-endpoints*)))
    (if known
        (and (stringp known) known)
        (let ((found (discover-api-endpoint token)))
          (setf (gethash digest *env-endpoints*) (or found :none))
          found))))

(defun credential (op next)
  "The :CREDENTIAL answer for github-copilot: the signed-in GitHub token, else
COPILOT_GITHUB_TOKEN's, else nothing to send (no other provider's key is
lent to Copilot)."
  ;; A round names the address it would dial; a question of where the
  ;; credential comes from (/connect's listing) names none, and is answered
  ;; without a refresh, a write or the network.
  (if (not (equal (getf op :provider) +provider+))
      (funcall next op)
      (let ((endpoint (getf op :endpoint))
            (entry (stored-entry (getf op :auth))))
        (cond
          (entry
           (let* ((entry (if endpoint (fresh-entry entry (getf op :auth-path)) entry))
                  (enterprise (nlk:json-value entry :text "enterprise_url")))
             (nle:make-credential
              (nlk:json-value entry :text "access_token") :oauth
              (append (alexandria:when-let
                          (moved (rebased endpoint :enterprise enterprise
                                                   :api-endpoint (nlk:json-value entry :text "api_endpoint")))
                        (list :endpoint moved))
                      (and enterprise (list :copilot-enterprise enterprise))))))
          ((env-key)
           (let ((token (env-key)))
             (nle:make-credential
              token :env
              (alexandria:when-let (moved (and endpoint (rebased endpoint :api-endpoint (env-endpoint token))))
                (list :endpoint moved)))))
          (t (nle:make-credential "public" :public))))))

;;; --- the request -----------------------------------------------------------------------

(defun strip-ttl (body)
  "BODY with every cache marker's ttl taken off: the one-hour TTL needs a beta
Copilot refuses, and the default marker needs none."
  (labels ((walk (value)
             (typecase value
               (hash-table
                (alexandria:when-let (marker (nlk:json-value value :object "cache_control"))
                  (remhash "ttl" marker))
                (maphash (lambda (key inner) (declare (ignore key)) (walk inner)) value))
               ((and vector (not string)) (map nil #'walk value)))))
    (walk body)
    body))

(defun anthropic-body (next context)
  "ANTHROPIC-REQUEST-BODY advice: a Copilot round asks for no Anthropic beta."
  ;; omp: "The GitHub Copilot Anthropic proxy doesn't accept Anthropic beta
  ;; features. Forward only caller-supplied betas", and Nodecode supplies none.
  (if (ours-p (nle::compiled-turn-context-provider-config context))
      (let ((answer (multiple-value-list (funcall next context))))
        (values-list (list* (strip-ttl (first answer)) nil (cddr answer))))
      (funcall next context)))

(defun last-item (body)
  "The newest message or input item of the request BODY, whichever wire it is."
  (let ((items (or (nlk:json-value body :array "messages") (nlk:json-value body :array "input"))))
    (and items (plusp (length items)) (aref items (1- (length items))))))

(defun initiator (body)
  "Who started the request BODY: `user' for the operator's own message,
`agent' for a round the loop sends after a tool ran (inferCopilotInitiator).
Copilot bills a premium request for the first only."
  (let* ((last (last-item body))
         (role (nlk:json-value last :string "role")))
    (cond ((not (hash-table-p last)) "user")
          ((equal role "user")
           (let ((content (nlk:json-value last :array "content")))
             (if (and content (plusp (length content))
                      (equal "tool_result" (nlk:json-value (aref content (1- (length content))) :string "type")))
                 "agent"
                 "user")))
          ((or role (nlk:json-value last :string "type")) "agent")
          (t "user"))))

(defun vision-p (body)
  "Whether any message of the request BODY carries an image."
  (labels ((image-p (value)
             (typecase value
               (hash-table (or (member (nlk:json-value value :string "type") '("image" "image_url" "input_image")
                                       :test #'equal)
                               (some #'image-p (coerce (nlk:json-array value "content") 'list))))
               (t nil))))
    (some #'image-p (coerce (or (nlk:json-value body :array "messages")
                                (nlk:json-value body :array "input")
                                #())
                            'list))))

(defun round-headers (lane-headers lane token initiator vision integration-id)
  "The headers of one Copilot round on LANE: the lane's own, less its key and
betas, then the bearer, the Copilot CLI identity under INTEGRATION-ID, the
INITIATOR and, when VISION, the vision flag (buildCopilotDynamicHeaders)."
  (append (remove-if (lambda (pair)
                       (member (car pair) '("authorization" "x-api-key" "anthropic-beta") :test #'string-equal))
                     lane-headers)
          (when (equal lane "anthropic")
            '(("Accept" . "text/event-stream")
              ("Anthropic-Dangerous-Direct-Browser-Access" . "true")))
          `(("Authorization" . ,(format nil "Bearer ~a" token)))
          (remove "Copilot-Integration-Id" (api-headers) :key #'car :test #'string-equal)
          `(("Copilot-Integration-Id" . ,integration-id)
            ("X-Initiator" . ,initiator)
            ("X-Interaction-Type" . ,(format nil "conversation-~a" initiator)))
          (when vision '(("Copilot-Vision-Request" . "true")))))

;;; The identity that last cleared Copilot's client gate, per credential and
;;; host: a Business organization that refuses the chat identity pays the
;;; retry once, not on every round. Process-local, as omp's is.
(defvar *working-identity* (make-hash-table :test #'equal :synchronized t)
  "A credential-and-host key -> the integration id that last worked there.")

(defun identity-key (token endpoint)
  "The key *WORKING-IDENTITY* files TOKEN's identity at ENDPOINT's host under."
  (format nil "~a ~a" (nlk:short-digest token 16)
          (ignore-errors (string-downcase (quri:uri-host (quri:uri endpoint))))))

(defun identity-refused-p (condition)
  "Whether CONDITION is Copilot refusing the client identity before anything
streamed: a 403, or a 400 whose code is model_not_supported."
  (and (eq :request (nle::provider-error-scope condition))
       (identity-denied-p (nle::provider-error-status condition)
                          (or (nle::provider-error-evidence-body condition)
                              (nle::provider-error-detail condition)))))

(defun walk (next fold &rest keys &key config headers request-json endpoint &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a Copilot round carries Copilot's headers and
goes to the account's host."
  (if (not (ours-p config))
      (apply next fold keys)
      (let ((key (nle::effective-provider-config-api-key config)))
        (when (member key '(nil "" "public") :test #'equal)
          (error 'nle::provider-config-error
                 :status 401
                 :detail "GitHub Copilot is not signed in: run /github-copilot login, or set COPILOT_GITHUB_TOKEN to a GitHub token"))
        (multiple-value-bind (token envelope-enterprise envelope-endpoint) (parse-api-key key)
          (let* ((lane (nle::effective-provider-config-lane config))
                 (enterprise (or envelope-enterprise (nle::credential-attribute config :copilot-enterprise)))
                 ;; the walk's own default when the lane named none: the
                 ;; credential's address, else the one the config froze
                 (endpoint (or endpoint (nle::credential-attribute config :endpoint)
                               (nle::effective-provider-config-endpoint config)))
                 (endpoint (or (and (or envelope-enterprise envelope-endpoint)
                                    (rebased endpoint :enterprise envelope-enterprise :api-endpoint envelope-endpoint))
                               endpoint))
                 (body (ignore-errors (nlk:decode-json request-json)))
                 (initiator (initiator body))
                 (vision (vision-p body))
                 (pinned (pinned-integration-id))
                 (memo (identity-key token endpoint))
                 (cached (normalize-integration-id (gethash memo *working-identity*)))
                 (first-id (or pinned cached (if enterprise +cli-integration-id+ +chat-integration-id+)))
                 (rest (alexandria:remove-from-plist keys :headers :endpoint)))
            (flet ((send (integration-id)
                     (apply next fold
                            :headers (round-headers headers lane token initiator vision integration-id)
                            :endpoint endpoint
                            rest)))
              ;; wrapFetchForCopilotFallback: a refused chat identity retries
              ;; once as the CLI; a remembered CLI identity that is refused
              ;; retries once as chat. Never a pin, never a loop.
              (handler-case (send first-id)
                (nle::provider-error (refusal)
                  (let ((retry (and (not pinned) (identity-refused-p refusal)
                                    (cond ((equal first-id +chat-integration-id+) +cli-integration-id+)
                                          ((equal cached +cli-integration-id+) +chat-integration-id+)))))
                    (unless retry (error refusal))
                    (handler-case
                        (multiple-value-prog1 (send retry)
                          (setf (gethash memo *working-identity*) retry))
                      (nle::provider-error (again)
                        (remhash memo *working-identity*)
                        (error again))))))))))))

;;; --- the listing ------------------------------------------------------------------------

(defun discovery-headers (token)
  "The headers of a model listing (COPILOT_DISCOVERY_HEADERS): the CLI
identity, which unlocks enterprise and experimental models, as the user."
  `(("Authorization" . ,(format nil "Bearer ~a" token))
    ,@(api-headers)
    ("X-Initiator" . "user")))

(defun listing-rows (text)
  "The chat models the Copilot listing TEXT names, as listing rows
(:id :display :context-window); its embedding models are left out."
  (loop for entry across (nlk:json-array (json-of text) "data")
        for id = (nlk:json-value entry :text "id")
        for type = (nlk:json-value entry :string "capabilities" "type")
        when (and id (or (null type) (equal type "chat")))
          collect (list :id id
                        :display (nlk:json-value entry :text "name")
                        :context-window (or (nlk:json-value entry :integer "capabilities" "limits" "max_context_window_tokens")
                                            (nlk:json-value entry :integer "capabilities" "limits" "max_prompt_tokens")))))

(defun listing (next provider &rest keys &key key base &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: Copilot's listing is asked at the account's
host with Copilot's headers. => (values ROWS ERROR), never a signal."
  (if (not (equal provider +provider+))
      (apply next provider keys)
      (let* ((configured (string-right-trim "/" (or base (nle::provider-base-url provider) +base+)))
             (asked (format nil "~a/models" configured))
             (credential (if key
                             (nle:make-credential key :config)
                             (ignore-errors (nle::resolve-provider-credential provider :endpoint asked))))
             (token (parse-api-key (nle:credential-key credential))))
        (if (member token '(nil "" "public") :test #'equal)
            (values nil "not signed in: /github-copilot login")
            (multiple-value-bind (text status)
                (handler-case (exchange :get (or (getf (nle:credential-attributes credential) :endpoint) asked)
                                        :headers (discovery-headers token)
                                        :timeout nle::*provider-models-fetch-timeout-seconds*)
                  (error (e) (values nil (princ-to-string e))))
              (cond ((not (integerp status)) (values nil (or status "no answer")))
                    ((not (ok-p status)) (values nil (format nil "HTTP ~d" status)))
                    (t (let ((rows (listing-rows text)))
                         (if rows
                             (values (sort rows #'string-lessp :key (lambda (row) (getf row :id))) nil)
                             (values nil "empty listing"))))))))))

;;; --- /github-copilot ------------------------------------------------------------------

(defun status ()
  "Where the Copilot credential comes from, in a line."
  (let* ((auth (ignore-errors (nle::read-auth-file nle::*auth-file-path*)))
         (entry (stored-entry auth)))
    (cond (entry
           (format nil "github-copilot: signed in~@[ to ~a~]~@[, served at ~a~]~:[~;; a sign-in is in progress~]"
                   (nlk:json-value entry :text "enterprise_url")
                   (nlk:json-value entry :text "api_endpoint")
                   *flow*))
          ((env-key)
           (format nil "github-copilot: using the GitHub token in COPILOT_GITHUB_TOKEN~:[~;; a sign-in is in progress~]" *flow*))
          (*flow* "github-copilot: a sign-in is in progress")
          (t "github-copilot: not signed in; /github-copilot login signs in at github.com, /github-copilot login DOMAIN at a GitHub Enterprise instance"))))

(defun logout ()
  "Forget the kept sign-in."
  (cancel-flow)
  (save-entry nle::*auth-file-path* nil)
  (nle:notice nil :key +key+)
  "github-copilot: signed out")

(defun run-slash (args session-id)
  "/github-copilot login [ENTERPRISE-DOMAIN] | logout | status"
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (nlk:trimmed (or args "")) :separator '(#\Space #\Tab))
                        :test #'string=))
         (verb (string-downcase (or (first words) "status"))))
    (cond ((equal verb "login") (login (second words) nle::*auth-file-path*))
          ((equal verb "logout") (logout))
          ((equal verb "status") (status))
          (t "usage: /github-copilot login [ENTERPRISE-DOMAIN] | logout | status"))))

(defun complete-slash (text session-id)
  "The verbs /github-copilot takes, those that start with TEXT."
  (declare (ignore session-id))
  (loop for verb in '("login" "logout" "status")
        when (uiop:string-prefix-p (string-downcase (nlk:trimmed (or text ""))) verb)
          collect (list :name verb :value verb)))

(defun start ()
  "Forget the last catalog; on the way down, end a sign-in in progress and
clear what it said."
  (forget-catalog)
  (nle:on-stop #'forget-catalog)
  (nle:on-stop (lambda () (cancel-flow) (nle:notice nil :key +key+))))

(nle:define-cell github-copilot
  (:section ("github-copilot")
    (:guide "sign in with /github-copilot login (login DOMAIN for GitHub Enterprise), or set COPILOT_GITHUB_TOKEN to a GitHub token; base_url is where Copilot is served")
    ("base_url" :string :default +base+
     :doc "the Copilot API base: chat appends /chat/completions, Responses /responses, Messages /v1/messages"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::resolve-model-lane #'lane)
  (:hook 'nle::lane-endpoint #'endpoint)
  (:hook 'nle::anthropic-request-body #'anthropic-body)
  (:hook 'nle::walk-provider-stream #'walk)
  (:hook 'nle::list-provider-models #'listing)
  (:command "github-copilot" 'run-slash
   :description "Sign in to GitHub Copilot, or out"
   :argument-hint "login [ENTERPRISE-DOMAIN] | logout | status"
   :session nil
   :complete 'complete-slash))
