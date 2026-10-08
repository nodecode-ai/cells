;;;; provider.lisp --- what the Codex backend is: its address, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; providers/openai-codex.kdl, catalog/src/wire/codex.ts (the headers and
;;;; the token's claims), ai/src/providers/openai-codex-responses.ts and
;;;; openai-codex/request-transformer.ts (the request, its identity, the
;;;; stream's terminal events), and the bundled rows of catalog/src/
;;;; models.json, which models.json in this folder carries
;;;; (tools/omp-models.py wrote it).
;;;;
;;;; A ChatGPT Plus/Pro subscription is served at
;;;; https://chatgpt.com/backend-api/codex/responses, not at the OpenAI API.
;;;; The wire is the Responses API's, with the differences omp's codex
;;;; transport makes and codex-rs makes before it:
;;;;
;;;;   - the bearer is the sign-in's access token, and the ChatGPT workspace
;;;;     it draws on rides as chatgpt-account-id (and a region-pinned
;;;;     workspace's residency as x-openai-internal-codex-residency)
;;;;   - every request names its client: originator, version (the Codex CLI
;;;;     release the backend gates models on), OpenAI-Beta, and a routing hint
;;;;   - every request carries the conversation's identity: session, thread,
;;;;     window and turn, as headers and as client_metadata
;;;;   - the backend refuses an output cap and every sampling control, and
;;;;     keeps nothing (store false), so an input item never names an item
;;;;     id it would have to look up; encrypted reasoning is always asked for
;;;;   - the stream may end on response.done, and a response.failed is a
;;;;     failure, not a finish
;;;;
;;;; This is the same wire the nodecode-openai-codex cell carries, carried
;;;; again here so this cell serves the sign-in it makes on its own.

(in-package #:nodecode-openai-codex-device)

(defparameter +base+ "https://chatgpt.com/backend-api"
  "Where the Codex backend is served: the base omp names. The Responses
endpoint is <base>/codex/responses.")

(defparameter +env+ '("OPENAI_CODEX_OAUTH_TOKEN")
  "The environment variable omp reads a Codex access token from, when no
sign-in is saved.")

(defparameter +client-version+ "0.159.0"
  "The Codex CLI release this client says it is (omp's CODEX_CLIENT_VERSION):
the backend gates which models it serves on it.")

(defparameter +auth-claim+ "https://api.openai.com/auth"
  "The JWT claim namespace that carries the ChatGPT account, plan and residency.")

(defparameter +profile-claim+ "https://api.openai.com/profile"
  "The JWT claim namespace that carries the account's email.")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-openai-codex-device" "models.json")))
  "omp's bundled openai-codex rows, read when this file loads: a vector of objects.")

(defparameter +image-models+ '("gpt-image-2")
  "Rows omp bundles as image models (`kind image', no tools): listed, never
offered for a turn.")

(defun catalog-model (row &aux (id (nlk:json-value row :string "id"))
                               (image-p (member id +image-models+ :test #'equal)))
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields)."
  (flet ((value (type key) (nlk:json-value row type key)))
    (let ((cost (value :object "cost")))
      (nle::make-catalog-model
       (value :string "name")
       (value :integer "context")
       (value :integer "output")
       (or (value :array "input") #("text"))
       (if image-p #("image") #("text"))
       (sort (remove-if-not #'nle::effort-rank (coerce (or (value :array "efforts") #()) 'list))
             #'< :key #'nle::effort-rank)
       (value :boolean "reasoning")
       nil
       (not image-p)
       ;; CATALOG-PRICE's shape; omp prices a subscription model at its API
       ;; list price, so a round reads as what it would have cost
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun codex-base (base)
  "The base the lane appends /responses to, out of BASE: omp's
resolveCodexResponsesUrl, which accepts the backend's root, its /codex, or
the whole /codex/responses."
  (let ((base (string-right-trim "/" (or base +base+))))
    (cond ((uiop:string-suffix-p base "/codex/responses")
           (subseq base 0 (- (length base) (length "/responses"))))
          ((uiop:string-suffix-p base "/codex") base)
          (t (concatenate 'string base "/codex")))))

(defun catalog-row (&optional prior)
  "openai-codex as a models.dev provider: this cell's lane, the section's
base, the token variable, and the bundled models over PRIOR's."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "ChatGPT Plus/Pro (Codex Subscription)"
                     "npm" +key+
                     "api" (codex-base (setting :base-url))
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun make-lane ()
  "The Codex lane: the Responses fold (CALL-RESPONSES-STREAMING), under this
cell's name and a family of its own."
  ;; The family is what the :CREDENTIAL point tells a hook about the lane; an
  ;; openai-family hook (nodecode-codex-auth) answers OpenAI API rounds and
  ;; leaves this one to this cell.
  (nle::make-provider-lane :name +lane+
                           :stream-symbol 'nle::call-responses-streaming
                           :family :openai-codex
                           :reasoning-carry :text
                           :default-endpoint (concatenate 'string (codex-base (setting :base-url))
                                                          "/responses")
                           :path "/responses"
                           :npm +key+))

;;; --- the token's claims ---------------------------------------------------------

(defun jwt-claims (token)
  "The payload of the JWT TOKEN as a JSON object, or NIL for anything else."
  ;; Unverified, as omp reads it: the claims say whose token it is, the
  ;; backend decides whether it is good.
  (let* ((parts (and (stringp token) (uiop:split-string token :separator ".")))
         (text (and (= (length parts) 3) (second parts))))
    (when (plusp (length text))
      (let ((claims (ignore-errors
                     (nlk:decode-json
                      (cl-base64:base64-string-to-usb8-array
                       ;; base64url, unpadded: cl-base64's URI alphabet pads with dots
                       (concatenate 'string (substitute #\- #\+ (substitute #\_ #\/ text))
                                    (make-string (mod (- (length text)) 4) :initial-element #\.))
                       :uri t)))))
        (and (hash-table-p claims) claims)))))

(defun token-profile (access &optional id-token)
  "(values ACCOUNT-ID EMAIL PLAN) the ACCESS token's claims name, else the
ID-TOKEN's: omp's getTokenProfile."
  (let ((access (jwt-claims access))
        (id (jwt-claims id-token)))
    (flet ((claim (namespace key)
             (or (nlk:json-value access :text namespace key)
                 (nlk:json-value id :text namespace key))))
      (values (claim +auth-claim+ "chatgpt_account_id")
              (let ((email (claim +profile-claim+ "email")))
                (and email (string-downcase (string-trim " " email))))
              (let ((plan (claim +auth-claim+ "chatgpt_plan_type")))
                (and plan (string-downcase (string-trim " " plan))))))))

(defun token-residency (access)
  "The data residency a region-pinned workspace's ACCESS token names, or NIL:
omp's getCodexResidency."
  (let ((claims (jwt-claims access)))
    (some (lambda (key)
            (let ((value (nlk:json-value claims :string +auth-claim+ key)))
              (and value (plusp (length (string-trim " " value))) (string-trim " " value))))
          '("chatgpt_data_residency" "chatgpt_compute_residency"))))

(defun token-expiry (access)
  "The epoch second the JWT ACCESS token's exp claim names, or NIL."
  (let ((exp (nlk:json-value (jwt-claims access) :number "exp")))
    (and exp (floor exp))))

;;; --- a round's identity ------------------------------------------------------------
;;; codex-rs names every request's place in a conversation: the session, a
;;; thread and a window that live as long as the session does here, and a
;;; turn that starts with each operator turn. omp keeps them in its provider
;;; session state; this cell keeps them per session id, in this process.

(defun uuid ()
  "A random UUID, version 4."
  (let ((bytes (nlk:random-bytes 16)))
    (setf (aref bytes 6) (logior #x40 (logand (aref bytes 6) #x0f))
          (aref bytes 8) (logior #x80 (logand (aref bytes 8) #x3f)))
    (flet ((hex (start end) (format nil "~(~{~2,'0x~}~)" (coerce (subseq bytes start end) 'list))))
      (format nil "~a-~a-~a-~a-~a" (hex 0 4) (hex 4 6) (hex 6 8) (hex 8 10) (hex 10 16)))))

(defun unix-seconds ()
  "Now, in seconds since 1970."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defstruct (thread (:copier nil))
  "The Codex identity one session's rounds carry."
  (session (uuid))
  (id (uuid))
  (window (uuid))
  ;; the Nodecode turn the Codex turn below was opened for
  (turn-key nil)
  (turn nil)
  (started nil)
  ;; the backend's sticky-routing token for the running turn, once it gave one
  (turn-state nil)
  ;; the backend's models etag, the newest it gave
  (models-etag nil))

(defvar *threads* (make-hash-table :test 'equal :synchronized t)
  "Session id -> its THREAD; \"\" for rounds outside any turn.")

(defvar *installation* (uuid)
  "The installation id a sign-in that saved none is sent under: this process's.")

(defun round-thread (&aux (turn (nle:turn)) (session (getf turn :session-id))
                          (key (getf turn :turn-id)))
  "(values THREAD SESSION-ID): the identity of the round this thread runs,
a new Codex turn opened when Nodecode's turn moved; SESSION-ID is NIL outside
a turn."
  (let ((thread (or (gethash (or session "") *threads*)
                    (progn
                      ;; a bound, not an eviction policy: identities are cheap to remake
                      (when (> (hash-table-count *threads*) 1024) (clrhash *threads*))
                      (setf (gethash (or session "") *threads*)
                            (if session (make-thread :session session) (make-thread)))))))
    (unless (and (equal key (thread-turn-key thread)) (thread-turn thread))
      (setf (thread-turn-key thread) key
            (thread-turn thread) (uuid)
            (thread-started thread) (* 1000 (unix-seconds))
            (thread-turn-state thread) nil))
    (values thread session)))

(defun installation-id (config)
  "The installation id the round's credential carries, else this process's."
  (or (nle::credential-attribute config :installation-id) *installation*))

(defun turn-metadata (thread installation)
  "x-codex-turn-metadata: the round's identity as JSON text (omp's
createCodexRequestMetadata, request_kind turn)."
  (nlk:encode-json-object
   (nlk:json-object "installation_id" installation
                    "session_id" (thread-session thread)
                    "thread_id" (thread-id thread)
                    "turn_id" (thread-turn thread)
                    "window_id" (thread-window thread)
                    "request_kind" "turn"
                    :opt "turn_started_at_unix_ms" (thread-started thread))))

(defun client-metadata (thread installation)
  "The body's client_metadata: the identity flat, and the turn metadata whole."
  (nlk:json-object "x-codex-installation-id" installation
                   "session_id" (thread-session thread)
                   "thread_id" (thread-id thread)
                   "x-codex-window-id" (thread-window thread)
                   "turn_id" (thread-turn thread)
                   "x-codex-turn-metadata" (turn-metadata thread installation)))

;;; --- the body ----------------------------------------------------------------------

(defvar *stripped* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A history item's verbatim, as the core's memo keeps it, -> the verbatim
this lane sends in its place: the same item without its id.")

(defun strip-id (item)
  "ITEM, a decoded input item, without its item id (omp's filterInput: the
backend stores nothing, so an id is a reference to nothing); ITEM itself when
it carries none. A computer call keeps its id, as there."
  (if (and (hash-table-p item)
           (nth-value 1 (gethash "id" item))
           (not (equal "computer_call" (gethash "type" item))))
      (let ((copy (nlk:copy-json-object item)))
        (remhash "id" copy)
        copy)
      item))

(defparameter +id-key+ (sb-ext:string-to-octets "\"id\":" :external-format :utf-8)
  "The bytes an encoded object's id member opens with. Inside a string a
quote is escaped, so these bytes can only be a key.")

(defun stripped-verbatim (verbatim)
  "What this lane sends for the history item VERBATIM: decoded and stripped
once, then remembered. An item whose bytes carry no id key is itself."
  (or (gethash verbatim *stripped*)
      (setf (gethash verbatim *stripped*)
            (let ((octets (nlk:json-verbatim-octets verbatim)))
              (if (not (search +id-key+ octets))
                  verbatim
                  (let* ((item (ignore-errors (nlk:decode-json octets)))
                         (stripped (strip-id item)))
                    (if (eq stripped item)
                        verbatim
                        (nlk:json-verbatim (nlk:encode-json-octets stripped)))))))))

(defun codex-input (input memo)
  "INPUT, the Responses lane's input array, with every item id taken off."
  ;; The core remembers each history item's encoding (JSON-MEMO), and other
  ;; lanes send the item with its id: a fresh item is noted as the core built
  ;; it, before this lane swaps in its copy, so the memo stays every lane's
  ;; and this lane pays one decode per item, once.
  (map 'simple-vector
       (lambda (item)
         (cond ((nlk:json-verbatim-p item) (stripped-verbatim item))
               ((and (hash-table-p item) (not (eq item (strip-id item))))
                (stripped-verbatim (nlk:json-memo-note memo item (nlk:encode-json-octets item))))
               (t item)))
       (or input #())))

(defun shape-body (body context &aux (config (nle::compiled-turn-context-provider-config context)))
  "BODY, the Responses request the core built for CONTEXT, as the Codex
backend takes it (omp's transformRequestBody)."
  (multiple-value-bind (thread) (round-thread)
    (let ((installation (installation-id config))
          (effort (nle::effective-provider-config-reasoning-effort config)))
      ;; the backend refuses a caller's output cap and every sampling control
      (dolist (key '("max_output_tokens" "max_completion_tokens" "temperature" "top_p"))
        (remhash key body))
      ;; an effort turned off is said, not left to the backend's default
      (when (and (stringp effort) (member effort '("off" "none") :test #'string-equal))
        (setf (gethash "reasoning" body) (nlk:json-object "effort" "none")))
      (setf (gethash "store" body) :false
            (gethash "stream" body) t
            (gethash "include" body) (vector "reasoning.encrypted_content")
            (gethash "input" body) (codex-input (gethash "input" body)
                                                (nle::compiled-turn-context-memo context))
            (gethash "client_metadata" body) (client-metadata thread installation))
      body)))

;;; --- the request's headers and the stream ------------------------------------------

(defun round-headers (config)
  "The headers a Codex round carries beside its bearer (omp's
createCodexHeaders); the account and residency ride on the credential."
  (multiple-value-bind (thread session) (round-thread)
    (let ((installation (installation-id config)))
      `(("OpenAI-Beta" . "responses=experimental")
        ("originator" . ,(setting :originator))
        ("version" . ,+client-version+)
        ("x-codex-routing-hint" . ,(format nil "model=~a" (nle::effective-provider-config-model config)))
        ("accept" . "text/event-stream")
        ,@(when session
            `(("session_id" . ,session)
              ("conversation_id" . ,session)
              ("x-client-request-id" . ,session)))
        ("session-id" . ,(thread-session thread))
        ("thread-id" . ,(thread-id thread))
        ("x-codex-window-id" . ,(thread-window thread))
        ("x-codex-turn-metadata" . ,(turn-metadata thread installation))
        ,@(alexandria:when-let (state (thread-turn-state thread))
            `(("x-codex-turn-state" . ,state)))
        ,@(alexandria:when-let (etag (thread-models-etag thread))
            `(("x-models-etag" . ,etag)))))))

(defun note-response (next endpoint request-json session response-headers)
  "NOTE-BODY-WIRE advice: what a Codex round's response headers tell the next
request of its turn (omp's updateCodexSessionMetadataFromHeaders): the
backend's sticky-routing token, the first one a turn is given, and its
models etag. NOTE-BODY-WIRE is the one function the core hands a round's
response headers to; only the Codex backend sends these two."
  (multiple-value-prog1 (funcall next endpoint request-json session response-headers)
    (let ((state (nlk:json-value response-headers :text "x-codex-turn-state"))
          (etag (nlk:json-value response-headers :text "x-models-etag")))
      (when (or state etag)
        (let ((thread (round-thread)))
          (when (and state (null (thread-turn-state thread)))
            (setf (thread-turn-state thread) state))
          (when etag
            (setf (thread-models-etag thread) etag)))))))

(defun failure-frame (frame)
  "The error frame a response.failed FRAME means: its error's code and words,
for the lane's terminal error to classify (omp's createCodexProviderStreamError)."
  (let* ((failure (or (nlk:json-value frame :object "response" "error")
                      (nlk:json-value frame :object "error")))
         (code (or (nlk:json-value failure :string "code") (nlk:json-value failure :string "type")))
         (message (or (nlk:json-value failure :string "message")
                      (nlk:json-value frame :string "response" "message")
                      "")))
    (nlk:json-object "type" "error"
                     :opt "code" code
                     "message" (format nil "Codex response failed: ~a~@[ (code=~a)~]"
                                       (if (plusp (length message)) message "no reason given") code))))

(defun codex-fold (fold)
  "FOLD, handed the Codex stream as the Responses lane reads it."
  ;; response.done ends a response as response.completed does; a
  ;; response.failed fails it; response.metadata carries the backend's
  ;; sticky-routing token for the turn, which the next request of the turn
  ;; sends back (omp's updateCodexSessionMetadataFromHeaders).
  (lambda (frame finish record)
    (let ((type (nlk:json-value frame :string "type")))
      (cond ((equal type "response.done")
             (setf (gethash "type" frame) "response.completed"))
            ((equal type "response.failed")
             (setf frame (failure-frame frame)))
            ((equal type "response.metadata")
             (alexandria:when-let (state (some (lambda (key)
                                                 (nlk:json-value frame :text "headers" key))
                                               '("x-codex-turn-state" "X-Codex-Turn-State")))
               (let ((thread (round-thread)))
                 (unless (thread-turn-state thread)
                   (setf (thread-turn-state thread) state)))))))
    (funcall fold frame finish record)))
