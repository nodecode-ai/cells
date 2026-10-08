;;;; cell.lisp --- the cell: Antigravity's Cloud Code Assist as a lane of its own.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No lane of the organism speaks Cloud Code Assist, so the cell registers
;;;; one, google-antigravity, whose stream is the organism's own Gemini fold
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
;;;;                          provider's key is lent to Antigravity
;;;;   GOOGLE-REQUEST-BODY    the round's body is Antigravity's request: the
;;;;                          history as omp converts it, inside the envelope
;;;;                          the real client sends (a session, a step, labels)
;;;;   WALK-PROVIDER-STREAM   the round goes to v1internal:streamGenerateContent
;;;;                          with the bearer and Antigravity's user agent, and
;;;;                          its events are unwrapped for the fold
;;;;   LIST-PROVIDER-MODELS   /models lists the bundled models, asking no one
;;;;   /google-antigravity    login, code ADDRESS, logout, status
;;;;
;;;; A round that fails at the daily host before anything streamed is tried at
;;;; the sandbox host, and the host that answered is tried first next time.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "google-antigravity": {"base_url": "https://daily-cloudcode-pa.googleapis.com",
;;;;                          "endpoint_mode": "auto"}
;;;; A vetoed section ("enabled": false) installs nothing: no lane, no hook.

(in-package #:nodecode-google-antigravity)

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
  "The :CREDENTIAL answer for google-antigravity: the kept sign-in's token,
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
  "The headers of ROUND's request: the bearer, Antigravity's user agent, and
for a thinking Claude model the interleaved-thinking beta."
  (let ((facts (round-facts round)))
    `(("Authorization" . ,(format nil "Bearer ~a" (round-token round)))
      ("Accept" . "text/event-stream")
      ("User-Agent" . ,(client-user-agent))
      ,@(when (and (compat facts "claude_thinking_beta")
                   (equal "anthropic" (nlk:json-value facts :string "class"))
                   (nlk:json-value facts :boolean "reasoning"))
          '(("anthropic-beta" . "interleaved-thinking-2025-05-14"))))))

;;; --- the conversation's identity -------------------------------------------------------
;;; The real client names each request: a session id (a signed decimal), an
;;; agent and a trajectory (UUIDs), a step that counts up, and the id of the
;;; last answer. omp keeps them per conversation; here they are kept per
;;; session, in this process.

(defstruct (session (:copier nil))
  agent trajectory id (step 1) last-execution last-good)

(defvar *sessions* (make-hash-table :test #'equal :synchronized t)
  "A session id -> its Antigravity identity.")

(defparameter +sessions-limit+ 512
  "The most sessions whose identity is kept; past it the table starts over.")

(defun session-of (session-id)
  "The Antigravity identity of SESSION-ID, made the first time; NIL for none."
  (when session-id
    (or (gethash session-id *sessions*)
        (progn (when (>= (hash-table-count *sessions*) +sessions-limit+) (clrhash *sessions*))
               (setf (gethash session-id *sessions*) (make-session))))))

(defparameter +int63+ (1- (ash 1 63)))

(defun random-int63 (below)
  "A random integer under BELOW from eight bytes of entropy."
  (loop for value = (logand +int63+ (reduce (lambda (sum byte) (+ (ash sum 8) byte)) (nlk:random-bytes 8)
                                            :initial-value 0))
        when (< value below) return value))

(defun random-session-id ()
  "A signed-decimal session id, as the real client mints one."
  (format nil "-~d" (random-int63 9000000000000000000)))

(defun derived-session-id (text)
  "The signed-decimal session id TEXT hashes to: the first eight bytes of its SHA-256."
  (format nil "-~d" (logand +int63+ (parse-integer (subseq (nlk:sha256-text text) 7 23) :radix 16))))

(defun uuid ()
  "A random version-4 UUID."
  (let ((bytes (nlk:random-bytes 16)))
    (setf (aref bytes 6) (logior #x40 (logand #x0f (aref bytes 6)))
          (aref bytes 8) (logior #x80 (logand #x3f (aref bytes 8))))
    (let ((hex (format nil "~(~{~2,'0x~}~)" (coerce bytes 'list))))
      (format nil "~a-~a-~a-~a-~a" (subseq hex 0 8) (subseq hex 8 12) (subseq hex 12 16)
              (subseq hex 16 20) (subseq hex 20 32)))))

(defun first-user-text (context)
  "The text of the conversation's first user message, or NIL."
  (loop for message across (coerce (nle::compiled-turn-context-messages context) 'vector)
        when (equal "user" (nlk:json-value message :string "role"))
          do (let ((content (gethash "content" message)))
               (return (if (stringp content)
                           content
                           (loop for part in (nle::message-content-parts content)
                                 when (equal "text" (nle::content-part-type part))
                                   return (nlk:json-value part :string "text")))))))

(defun envelope (round context wire-model)
  "(values SESSION-ID REQUEST-ID LABELS) of ROUND's request, its session's
step advanced (buildAntigravityRequestEnvelope)."
  (let ((session (round-session round)))
    (when session
      (setf (session-agent session) (or (session-agent session) (uuid))
            (session-trajectory session) (or (session-trajectory session) (uuid))
            (session-id session) (or (session-id session) (random-session-id)))
      (incf (session-step session)))
    (let* ((facts (round-facts round))
           (agent (if session (session-agent session) (uuid)))
           (trajectory (if session (session-trajectory session) (uuid)))
           (session-id (if session
                           (session-id session)
                           (let ((text (first-user-text context)))
                             (if (blank-p text) (random-session-id) (derived-session-id text)))))
           (step (if session (session-step session) 2))
           (label (or (nlk:json-value facts :text "compat" "usage_label")
                      (if (equal "anthropic" (nlk:json-value facts :string "class")) "true" "false"))))
      (values session-id
              (format nil "agent/~a/~d/~a/~d" agent
                      (* 1000 (unix-now))
                      trajectory step)
              (nlk:json-object :opt "last_execution_id" (and session (session-last-execution session))
                               "last_step_index" (princ-to-string (1- step))
                               :opt "model_enum" (values (wire-profile wire-model))
                               "trajectory_id" trajectory
                               "used_claude" label
                               "used_claude_conservative" label)))))

;;; --- the request ------------------------------------------------------------------------

(defun cca-request (context round)
  "Antigravity's request of the compiled CONTEXT (buildRequest, its
Antigravity arm)."
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (facts (round-facts round))
         (claude (equal "anthropic" (nlk:json-value facts :string "class")))
         (tools (nle::compiled-turn-context-tools context))
         (choice (nle::effective-provider-config-tool-choice config))
         (system (nle::compiled-turn-context-system-prompt context))
         (sampling (not (and (nth-value 1 (gethash "sampling" (nlk:json-value facts :object "compat")))
                             (not (compat facts "sampling"))))))
    (multiple-value-bind (model thinking max-tokens)
        (thinking-plan facts (nle::effective-provider-config-reasoning-effort config)
                       (or (nle::effective-max-output-tokens context) (nlk:json-value facts :integer "output")))
      (let* ((contents (cca-contents (nle::request-messages context) facts
                                     (round-provider round) (round-model round)))
             ;; the real client's fixed output cap for the requested model
             (max-tokens (or (nth-value 1 (wire-profile model)) max-tokens))
             (generation (nlk:json-object
                          :opt "temperature" (and sampling (nle::effective-provider-config-temperature config))
                          :opt "maxOutputTokens" max-tokens
                          :opt "topP" (and sampling (nle::effective-provider-config-top-p config))
                          :opt "thinkingConfig" thinking))
             (mode (and tools (stringp choice)
                        (cdr (assoc choice '(("none" . "NONE") ("required" . "ANY") ("any" . "ANY"))
                                    :test #'string-equal))))
             ;; Antigravity's default tool mode is VALIDATED, and Claude always runs under it
             (tool-mode (cond ((and claude (compat facts "claude_tool_mode")) "VALIDATED")
                              (mode mode)
                              (tools "VALIDATED"))))
        ;; Gemini routes drop the tool config, so a forced call is restated
        (when (and tools (equal mode "ANY") (not claude))
          (setf contents (concatenate 'vector contents
                                      (vector (nlk:json-object "role" "user"
                                                               "parts" (vector (nlk:json-object "text" +forced-tool-directive+)))))))
        (multiple-value-bind (session-id request-id labels) (envelope round context model)
          (nlk:json-object
           "project" (round-project round)
           "requestId" request-id
           "request" (nlk:json-object
                      "contents" contents
                      :when (plusp (length system)) "systemInstruction"
                      (nlk:json-object "role" "user" "parts" (vector (nlk:json-object "text" system)))
                      :when tools "tools" (cca-tools tools facts)
                      :when tool-mode "toolConfig"
                      (nlk:json-object "functionCallingConfig" (nlk:json-object "mode" tool-mode))
                      "labels" labels
                      :when (plusp (hash-table-count generation)) "generationConfig" generation
                      "sessionId" session-id)
           "model" model
           "userAgent" "antigravity"
           "requestType" "agent"))))))

(defun body (next context)
  "GOOGLE-REQUEST-BODY advice: this lane's round sends Antigravity's request,
built once for every host the round tries."
  (let ((round *round*))
    (if (and round (ours-p (nle::compiled-turn-context-provider-config context)))
        (or (round-request round) (setf (round-request round) (cca-request context round)))
        (funcall next context))))

(defun walk (next fold &rest keys &key config &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: this lane's round goes to the host it is
trying, with its own headers, and its events reach the Gemini fold unwrapped."
  (let ((round *round*))
    (if (and round (ours-p config))
        (apply next (ready-fold fold round)
               :endpoint (format nil "~a/v1internal:streamGenerateContent?alt=sse" (round-endpoint round))
               :headers (round-headers round)
               (alexandria:remove-from-plist keys :endpoint :headers))
        (apply next fold keys))))

(defun endpoints (config session)
  "The hosts a round on CONFIG tries, in order: the section's mode, else the
configured base, or both Antigravity hosts, the last that answered first."
  (let ((mode (setting :endpoint-mode))
        (base (string-right-trim "/" (nle::effective-provider-config-endpoint config))))
    (cond ((equal mode "sandbox") (when session (setf (session-last-good session) nil)) (list +sandbox+))
          ((equal mode "production") (when session (setf (session-last-good session) nil)) (list +base+))
          ((not (member base (list +base+ +sandbox+) :test #'equal))
           (when session (setf (session-last-good session) nil))
           (list base))
          (t (let ((good (and session (session-last-good session))))
               (if good
                   (cons good (remove good (list +base+ +sandbox+) :test #'equal))
                   (list +base+ +sandbox+)))))))

(defun transient-p (refusal)
  "Whether REFUSAL, before anything streamed, is one another host may not
repeat: no answer at all, a timeout, a rate limit or a server failure."
  (and (eq :request (nle::provider-error-scope refusal))
       (let ((status (nle::provider-error-status refusal)))
         (or (null status) (eql status 408) (eql status 429) (and (integerp status) (>= status 500))))))

(defun stream-round (context &key (on-part nle::*turn-part-fn*))
  "The lane's stream: the Gemini fold over an Antigravity round, tried at
each host in turn while a host fails before answering, which must end on a
finish reason. => (values MESSAGE USAGE FINISH-REASON REQUEST-JSON)"
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (model (nle::effective-provider-config-model config))
         (turn (nle:turn))
         (session (session-of (and turn (getf turn :session-id)))))
    (multiple-value-bind (token project email) (round-login config)
      (unless (and token project)
        (error 'nle::provider-config-error
               :status 401
               :detail (format nil "Antigravity is not signed in: run /~a login" +provider+)))
      ;; the backend gates newer models on the client version
      (ensure-version)
      (let ((*round* (make-round :provider (nle::effective-provider-config-provider config)
                                 :model model :facts (model-facts model)
                                 :token token :project project :email email :session session
                                 :tool-names (map 'list (lambda (wrapper)
                                                          (nlk:json-value wrapper :string "function" "name"))
                                                  (nle::compiled-turn-context-tools context)))))
        (multiple-value-bind (message usage finish-reason request-json)
            (loop for (endpoint . more) on (endpoints config session)
                  do (setf (round-endpoint *round*) endpoint)
                     (handler-case
                         (handler-bind ((nle::provider-error
                                          (lambda (refusal)
                                            (alexandria:when-let (url (validation-url (or (nle::provider-error-evidence-body refusal)
                                                                                          (nle::provider-error-detail refusal))))
                                              (error 'nle::provider-error
                                                     :status (nle::provider-error-status refusal)
                                                     :scope (nle::provider-error-scope refusal)
                                                     :detail (validation-message url "retry your request" email))))))
                           (return (nle::call-google-streaming context :on-part on-part)))
                       (nle::provider-error (refusal)
                         (unless (and more (transient-p refusal))
                           (error refusal)))))
          (unless (round-finished *round*)
            (error 'nle::provider-stream-incomplete
                   :detail "Cloud Code Assist stream ended without a finish reason (connection dropped or response truncated)"))
          (when session
            (when (equal (setting :endpoint-mode) "auto")
              (setf (session-last-good session) (round-endpoint *round*)))
            (setf (session-last-execution session) (round-response-id *round*)))
          (values (remember-signatures message *round*) usage finish-reason request-json))))))

(defun register-lane ()
  "The google-antigravity lane: the Gemini fold over Antigravity's Cloud Code Assist."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                            :stream-symbol 'stream-round
                            :family :google
                            :reasoning-carry :text
                            :default-endpoint +base+
                            :path ""
                            :npm "nodecode-google-antigravity")))

(defun unregister-lane ()
  "Take the google-antigravity lane back out."
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

;;; --- /google-antigravity -------------------------------------------------------------

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
  "/google-antigravity login | code ADDRESS | logout | status"
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
  "The verbs /google-antigravity takes, those that start with TEXT."
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

(nle:define-cell google-antigravity
  (:section ("google-antigravity")
    (:guide "sign in with /google-antigravity login (a Google account on Antigravity's free tier); base_url is where Antigravity's Cloud Code Assist is served, endpoint_mode which of its hosts a round uses")
    ("base_url" :string :default +base+
     :doc "the Cloud Code Assist base the lane appends /v1internal:streamGenerateContent to")
    ("endpoint_mode" :choice :options '("auto" "production" "sandbox") :default "auto"
     :doc "auto tries the daily host, then the sandbox host, the last that answered first; production and sandbox use one"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::google-request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk)
  (:hook 'nle::list-provider-models #'listing)
  (:command "google-antigravity" 'run-slash
   :description "Sign in to Antigravity, or out"
   :argument-hint "login | code ADDRESS | logout | status"
   :session nil
   :complete 'complete-slash))
