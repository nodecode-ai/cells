;;;; cell.lisp --- the cell: Cloud Code Assist as a lane of its own.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No lane of the organism speaks Cloud Code Assist, so the cell registers
;;;; one, google-gemini-cli, whose stream is the organism's own Gemini fold
;;;; (CALL-GOOGLE-STREAMING) with the request and the events that differ put
;;;; right around it (wire.lisp). Five hooks, each declining for everything
;;;; but this lane's rounds, and one slash command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries the provider's row: this
;;;;                          cell's lane package, this section's base and
;;;;                          omp's bundled models, so /models lists them and
;;;;                          a turn resolves this lane
;;;;   :CREDENTIAL            the kept Google sign-in, refreshed first when it
;;;;                          is due, its project beside it; no other
;;;;                          provider's key is lent to Cloud Code Assist
;;;;   GOOGLE-REQUEST-BODY    the round's body is the Cloud Code Assist
;;;;                          request, built from the history the way omp
;;;;                          builds its own
;;;;   WALK-PROVIDER-STREAM   the round goes to v1internal:streamGenerateContent
;;;;                          with the bearer and the Gemini CLI's identity,
;;;;                          and its events are unwrapped for the fold
;;;;   LIST-PROVIDER-MODELS   /models lists the bundled models, asking no one
;;;;   /google-gemini-cli     login, code ADDRESS, logout, status
;;;;
;;;; Config, a sibling top-level key:
;;;;   "google-gemini-cli": {"base_url": "https://cloudcode-pa.googleapis.com"}
;;;; A vetoed section ("enabled": false) installs nothing: no lane, no hook.

(in-package #:nodecode-google-gemini-cli)

(defun ours-p (config)
  "Whether the frozen provider CONFIG rides this cell's lane."
  (and config (equal (nle::effective-provider-config-lane config) +provider+)))

;;; --- the catalog ---------------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
the provider's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with the provider's row, made once
per catalog the core reads."
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

;;; --- the credential ----------------------------------------------------------------

(defun credential (op next)
  "The :CREDENTIAL answer for google-gemini-cli: the kept sign-in's token,
with its project, else nothing to send."
  ;; A round names the address it would dial; a question of where the
  ;; credential comes from (/connect's listing) names none, and is answered
  ;; without a refresh, a write or the network.
  (if (not (equal (getf op :provider) +provider+))
      (funcall next op)
      (alexandria:if-let (entry (stored-entry (getf op :auth)))
        (let* ((path (getf op :auth-path))
               (entry (if (getf op :endpoint)
                          (handler-case (fresh-entry entry path)
                            (signin-failed (refusal)
                              ;; stands until a sign-in succeeds: the model
                              ;; keeps seeing why its rounds cannot go
                              (nle:notice (format nil "~a: the sign-in could not be refreshed (~a); run /~a login"
                                                  +provider+ refusal +provider+)
                                          :level :warning :key +key+)
                              (nle::credential-fail "~a; run /~a login" refusal +provider+)))
                          entry)))
          (nle:make-credential (nlk:json-value entry :text "access_token") :oauth
                               (list :project-id (nlk:json-value entry :text "project_id")
                                     :email (nlk:json-value entry :text "email")
                                     :auth-path path)))
        (nle:make-credential "public" :public))))

(defun round-login (config)
  "(values TOKEN PROJECT EMAIL) a round on CONFIG sends: the sign-in's token,
refreshed when a long turn outlived the one the turn froze, or a key /connect
saved in omp's structured form {token, projectId}."
  (let ((key (nle::effective-provider-config-api-key config))
        (project (nle::credential-attribute config :project-id))
        (path (nle::credential-attribute config :auth-path)))
    (if project
        (let ((entry (and path (ignore-errors
                                (let ((stored (stored-entry (nle::read-auth-file path))))
                                  (and stored (fresh-entry stored path)))))))
          (if entry
              (values (nlk:json-value entry :text "access_token")
                      (or (nlk:json-value entry :text "project_id") project)
                      (nlk:json-value entry :text "email"))
              (values key project (nle::credential-attribute config :email))))
        (let ((parsed (and (stringp key) (uiop:string-prefix-p "{" (nlk:trimmed key))
                           (ignore-errors (nlk:decode-json key)))))
          (values (nlk:json-value parsed :text "token")
                  (or (nlk:json-value parsed :text "projectId") (nlk:json-value parsed :text "project_id"))
                  (nlk:json-value parsed :text "email"))))))

;;; --- the lane -------------------------------------------------------------------------

(defun round-headers (round)
  "The headers of ROUND's request: the bearer and the Gemini CLI's identity."
  `(("Authorization" . ,(format nil "Bearer ~a" (round-token round)))
    ("Accept" . "text/event-stream")
    ,@(cli-headers (round-model round))))

(defun cca-request (context round)
  "The Cloud Code Assist request of the compiled CONTEXT (buildRequest)."
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (facts (round-facts round))
         (tools (nle::compiled-turn-context-tools context))
         (choice (nle::effective-provider-config-tool-choice config))
         (system (nle::compiled-turn-context-system-prompt context))
         (sampling (not (and (nth-value 1 (gethash "sampling" (nlk:json-value facts :object "compat")))
                             (not (compat facts "sampling"))))))
    (multiple-value-bind (model thinking max-tokens)
        (thinking-plan facts (nle::effective-provider-config-reasoning-effort config)
                       (or (nle::effective-max-output-tokens context) (nlk:json-value facts :integer "output")))
      (let* ((generation (nlk:json-object
                          :opt "temperature" (and sampling (nle::effective-provider-config-temperature config))
                          :opt "maxOutputTokens" max-tokens
                          :opt "topP" (and sampling (nle::effective-provider-config-top-p config))
                          :opt "thinkingConfig" thinking))
             (mode (and tools (stringp choice)
                        (cdr (assoc choice '(("none" . "NONE") ("required" . "ANY") ("any" . "ANY"))
                                    :test #'string-equal))))
             (request (nlk:json-object
                       "contents" (cca-contents (nle::request-messages context) facts
                                                (round-provider round) (round-model round))
                       :when (plusp (length system)) "systemInstruction"
                       (nlk:json-object "parts" (vector (nlk:json-object "text" system)))
                       :when tools "tools" (cca-tools tools facts)
                       :when mode "toolConfig"
                       (nlk:json-object "functionCallingConfig" (nlk:json-object "mode" mode))
                       :when (plusp (hash-table-count generation)) "generationConfig" generation)))
        (nlk:json-object "project" (round-project round)
                         "model" model
                         "request" request)))))

(defun body (next context)
  "GOOGLE-REQUEST-BODY advice: this lane's round sends the Cloud Code Assist request."
  (let ((round *round*))
    (if (and round (ours-p (nle::compiled-turn-context-provider-config context)))
        (cca-request context round)
        (funcall next context))))

(defun walk (next fold &rest keys &key config &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: this lane's round goes to Cloud Code Assist
with its own headers, and its events reach the Gemini fold unwrapped."
  (let ((round *round*))
    (if (and round (ours-p config))
        (apply next (ready-fold fold round)
               :endpoint (format nil "~a/v1internal:streamGenerateContent?alt=sse"
                                 (string-right-trim "/" (nle::effective-provider-config-endpoint config)))
               :headers (round-headers round)
               (alexandria:remove-from-plist keys :endpoint :headers))
        (apply next fold keys))))

(defun stream-round (context &key (on-part nle::*turn-part-fn*))
  "The lane's stream: the Gemini fold over a Cloud Code Assist round, which
must end on a finish reason. => (values MESSAGE USAGE FINISH-REASON REQUEST-JSON)"
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (model (nle::effective-provider-config-model config)))
    (multiple-value-bind (token project email) (round-login config)
      (unless (and token project)
        (error 'nle::provider-config-error
               :status 401
               :detail (format nil "Google Cloud Code Assist is not signed in: run /~a login" +provider+)))
      (let ((*round* (make-round :provider (nle::effective-provider-config-provider config)
                                 :model model :facts (model-facts model)
                                 :token token :project project :email email
                                 :tool-names (map 'list (lambda (wrapper)
                                                          (nlk:json-value wrapper :string "function" "name"))
                                                  (nle::compiled-turn-context-tools context)))))
        (handler-bind ((nle::provider-error
                         (lambda (refusal)
                           (alexandria:when-let (url (validation-url (or (nle::provider-error-evidence-body refusal)
                                                                         (nle::provider-error-detail refusal))))
                             (error 'nle::provider-error
                                    :status (nle::provider-error-status refusal)
                                    :scope (nle::provider-error-scope refusal)
                                    :detail (validation-message url "retry your request" email))))))
          (multiple-value-bind (message usage finish-reason request-json)
              (nle::call-google-streaming context :on-part on-part)
            (unless (round-finished *round*)
              (error 'nle::provider-stream-incomplete
                     :detail "Cloud Code Assist stream ended without a finish reason (connection dropped or response truncated)"))
            (values (remember-signatures message *round*) usage finish-reason request-json)))))))

(defun register-lane ()
  "The google-gemini-cli lane: the Gemini fold over Cloud Code Assist."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                            :stream-symbol 'stream-round
                            :family :google
                            :reasoning-carry :text
                            :default-endpoint +base+
                            :path ""
                            :npm "nodecode-google-gemini-cli")))

(defun unregister-lane ()
  "Take the google-gemini-cli lane back out."
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes* :key #'nle::provider-lane-name :test #'equal)))

;;; --- the listing ------------------------------------------------------------------------

(defun listed-p (model-id)
  "Whether MODEL-ID is a chat model the listing offers: an image model takes no tools."
  (let ((facts (model-facts model-id)))
    (or (not (nth-value 1 (gethash "tools" facts))) (nlk:json-value facts :boolean "tools"))))

(defun listing (next provider &rest keys)
  "LIST-PROVIDER-MODELS advice: the provider's listing is its bundled chat
models, asked of no one; Cloud Code Assist publishes no /models listing, and
omp's own discovery of the account's roster is not carried here (README, Gaps).
Asked with a KEY, which is /connect checking one, it says the key was not
checked: the credential is a sign-in, and a key no listing can ask about must
never read as one that works."
  (if (equal provider +provider+)
      (values (loop for row across +models+
                    for id = (nlk:json-value row :string "id")
                    when (listed-p id)
                      collect (list :id id
                                    :display (nlk:json-value row :string "name")
                                    :context-window (nlk:json-value row :integer "context")))
              (and (getf keys :key)
                   (format nil "~a signs in with Google, so a key is not checked: run /~a login"
                           +provider+ +provider+)))
      (apply next provider keys)))

;;; --- /google-gemini-cli -------------------------------------------------------------

(defun status ()
  "Whether a sign-in is kept, in a line."
  (let ((entry (stored-entry (ignore-errors (nle::read-auth-file nle::*auth-file-path*)))))
    (cond (entry
           (format nil "~a: signed in~@[ as ~a~], project ~a~:[~;; a sign-in is in progress~]"
                   +provider+ (nlk:json-value entry :text "email")
                   (nlk:json-value entry :text "project_id") *flow*))
          (*flow* (format nil "~a: a sign-in is in progress" +provider+))
          (t (format nil "~a: not signed in; /~a login signs in with Google" +provider+ +provider+)))))

(defun logout ()
  "Forget the kept sign-in."
  (cancel-flow)
  (save-entry nle::*auth-file-path* nil)
  (nle:notice nil :key +key+)
  (format nil "~a: signed out" +provider+))

(defun run-slash (args session-id)
  "/google-gemini-cli login | code ADDRESS | logout | status"
  (declare (ignore session-id))
  (let* ((trimmed (nlk:trimmed (or args "")))
         (space (position-if (lambda (char) (member char '(#\Space #\Tab))) trimmed))
         (verb (string-downcase (if (plusp (length trimmed)) (subseq trimmed 0 space) "status")))
         (rest (if space (nlk:trimmed (subseq trimmed space)) "")))
    (cond ((equal verb "login") (login nle::*auth-file-path*))
          ((equal verb "code") (paste rest))
          ((equal verb "logout") (logout))
          ((equal verb "status") (status))
          (t (format nil "usage: /~a login | code ADDRESS | logout | status" +provider+)))))

(defun complete-slash (text session-id)
  "The verbs /google-gemini-cli takes, those that start with TEXT."
  (declare (ignore session-id))
  (loop for verb in '("login" "code" "logout" "status")
        when (uiop:string-prefix-p (string-downcase (nlk:trimmed (or text ""))) verb)
          collect (list :name verb :value verb)))

(defun start ()
  "Register the lane; on the way down, take it out, end a sign-in in progress
and clear what it said."
  (forget-catalog)
  (register-lane)
  (nle:on-stop #'unregister-lane)
  (nle:on-stop #'forget-catalog)
  (nle:on-stop (lambda () (cancel-flow) (nle:notice nil :key +key+))))

(nle:define-cell google-gemini-cli
  (:section ("google-gemini-cli")
    (:guide "sign in with /google-gemini-cli login (a Google account; GOOGLE_CLOUD_PROJECT names the project of a paid tier); base_url is where Cloud Code Assist is served")
    ("base_url" :string :default +base+
     :doc "the Cloud Code Assist base the lane appends /v1internal:streamGenerateContent to"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::google-request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk)
  (:hook 'nle::list-provider-models #'listing)
  (:command "google-gemini-cli" 'run-slash
   :description "Sign in to Google Cloud Code Assist (Gemini CLI), or out"
   :argument-hint "login | code ADDRESS | logout | status"
   :session nil
   :complete 'complete-slash))
