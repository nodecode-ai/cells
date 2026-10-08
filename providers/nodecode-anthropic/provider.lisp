;;;; provider.lisp --- the anthropic provider on a Claude Pro/Max subscription.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; providers/anthropic.kdl and the bundled rows of catalog/src/models.json
;;;; (models.json in this folder, tools/omp-models.py wrote it),
;;;; ai/src/providers/anthropic.ts (buildAnthropicHeaders,
;;;; buildClaudeCodeBetas, buildAnthropicSystemBlocks, the billing header and
;;;; its cch attestation, the tool names), claude-code-fingerprint.ts and
;;;; anthropic-identity.ts (the metadata user id), catalog/src/utils.ts
;;;; (isAnthropicOAuthToken).
;;;;
;;;; The core serves `anthropic' on its own lane with a key: x-api-key, the
;;;; Messages body it builds. This file leaves that alone. A subscription
;;;; token (a sign-in this cell saved, or any key that is one, sk-ant-oat...)
;;;; is accepted by Anthropic only on a request shaped as Claude Code shapes
;;;; its own, and omp shapes it so:
;;;;
;;;;   - Authorization: Bearer, never x-api-key, and the CLI's client
;;;;     headers: its User-Agent, x-app cli, the Stainless SDK headers, the
;;;;     session id, and the Claude Code beta set (oauth-2025-04-20 among it)
;;;;   - /v1/messages?beta=true
;;;;   - the system prompt opens with a billing header (the CLI's version
;;;;     and a fingerprint of the first user message, and a cch attestation:
;;;;     the low 20 bits of the body's XXH64) and Claude Code's one-line
;;;;     identity, ahead of the prompt Nodecode wrote
;;;;   - every tool name carries the `_' prefix the CLI isolates custom
;;;;     tools with (taken back off on the answer), and a metadata user id
;;;;     names the device, the session and the account
;;;;   - cache marks live an hour, Claude Code's own policy for a seat

(in-package #:nodecode-anthropic)

(defparameter +claude-code-version+ "2.1.280"
  "The Claude Code release the requests say they come from (omp's DEFAULT_CLAUDE_CODE_VERSION).")

(defparameter +sdk-version+ "0.112.1"
  "The @anthropic-ai/sdk version that Claude Code release bundles.")

(defparameter +identity+ "You are Claude Code, Anthropic's official CLI for Claude."
  "The identity block Claude Code's runtime puts at the head of the system prompt.")

(defparameter +tool-prefix+ "_"
  "What a tool's name carries on the wire of a subscription round.")

(defparameter +builtin-tools+ '("web_search" "code_execution" "text_editor" "computer")
  "Anthropic's own tool names, which keep their names.")

(defparameter +billing-prefix+ "x-anthropic-billing-header:")

(defparameter +cch-seed+ #x4d659218e32a3268
  "The XXH64 seed of the billing header's cch attestation.")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-anthropic" "models.json")))
  "omp's bundled anthropic rows, read when this file loads: a vector of objects.")

(defun catalog-model (row)
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields)."
  (flet ((value (type key) (nlk:json-value row type key)))
    (let ((cost (value :object "cost")))
      (nle::make-catalog-model
       (value :string "name")
       (value :integer "context")
       (value :integer "output")
       (or (value :array "input") #("text"))
       #("text")
       (sort (remove-if-not #'nle::effort-rank (coerce (or (value :array "efforts") #()) 'list))
             #'< :key #'nle::effort-rank)
       (value :boolean "reasoning")
       nil
       t
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (prior)
  "The anthropic row with omp's bundled models added where PRIOR, the row
models.dev published, lacks them. Nothing models.dev says is replaced, and
no base is added: the core's own lane keeps its own address."
  (let ((row (nlk:copy-json-object (and (hash-table-p prior) prior)))
        (models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for entry across +models+
          for id = (nlk:json-value entry :string "id")
          unless (gethash id models)
            do (setf (gethash id models) (catalog-model entry)))
    (unless (gethash "name" row) (setf (gethash "name" row) "Anthropic"))
    (unless (gethash "npm" row) (setf (gethash "npm" row) "@ai-sdk/anthropic"))
    (unless (gethash "env" row) (setf (gethash "env" row) (vector "ANTHROPIC_API_KEY")))
    (setf (gethash "models" row) models)
    row))

;;; --- XXH64 -------------------------------------------------------------------------
;;; The billing header's cch is XXH64 over the request body, which neither the
;;; core nor SBCL carries: this is the reference algorithm (xxhash.com, the
;;; XXH64 specification), 64-bit wrapping arithmetic throughout.

(defconstant +p1+ #x9E3779B185EBCA87)
(defconstant +p2+ #xC2B2AE3D27D4EB4F)
(defconstant +p3+ #x165667B19E3779F9)
(defconstant +p4+ #x85EBCA77C2B2AE63)
(defconstant +p5+ #x27D4EB2F165667C5)

(deftype u64 () '(unsigned-byte 64))

(declaim (inline wrap rotl xxh-round read64 read32))

(defun wrap (x) (ldb (byte 64 0) x))

(defun rotl (x r)
  (declare (type u64 x) (type (integer 1 63) r))
  (wrap (logior (ash x r) (ash x (- r 64)))))

(defun xxh-round (acc input)
  (declare (type u64 acc input))
  (wrap (* (rotl (wrap (+ acc (wrap (* input +p2+)))) 31) +p1+)))

(defun read64 (octets at)
  (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type fixnum at))
  (let ((value 0))
    (declare (type u64 value))
    (loop for i from 7 downto 0
          do (setf value (logior (ash (ldb (byte 56 0) value) 8) (aref octets (+ at i)))))
    value))

(defun read32 (octets at)
  (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type fixnum at))
  (logior (aref octets at) (ash (aref octets (+ at 1)) 8)
          (ash (aref octets (+ at 2)) 16) (ash (aref octets (+ at 3)) 24)))

(defun xxh64 (octets &optional (seed 0))
  "The XXH64 hash of OCTETS under SEED."
  (let* ((octets (coerce octets '(simple-array (unsigned-byte 8) (*))))
         (length (length octets))
         (at 0)
         (hash 0))
    (declare (type u64 hash) (type fixnum at length))
    (flet ((merge-round (acc value)
             (wrap (+ (wrap (* (logxor acc (xxh-round 0 value)) +p1+)) +p4+))))
      (if (>= length 32)
          (let ((v1 (wrap (+ seed +p1+ +p2+))) (v2 (wrap (+ seed +p2+)))
                (v3 (wrap seed)) (v4 (wrap (- seed +p1+))))
            (declare (type u64 v1 v2 v3 v4))
            (loop while (<= (+ at 32) length)
                  do (setf v1 (xxh-round v1 (read64 octets at))
                           v2 (xxh-round v2 (read64 octets (+ at 8)))
                           v3 (xxh-round v3 (read64 octets (+ at 16)))
                           v4 (xxh-round v4 (read64 octets (+ at 24))))
                     (incf at 32))
            (setf hash (wrap (+ (rotl v1 1) (rotl v2 7) (rotl v3 12) (rotl v4 18))))
            (dolist (v (list v1 v2 v3 v4))
              (setf hash (merge-round hash v))))
          (setf hash (wrap (+ seed +p5+))))
      (setf hash (wrap (+ hash length)))
      (loop while (<= (+ at 8) length)
            do (setf hash (logxor hash (xxh-round 0 (read64 octets at)))
                     hash (wrap (+ (wrap (* (rotl hash 27) +p1+)) +p4+)))
               (incf at 8))
      (when (<= (+ at 4) length)
        (setf hash (logxor hash (wrap (* (read32 octets at) +p1+)))
              hash (wrap (+ (wrap (* (rotl hash 23) +p2+)) +p3+)))
        (incf at 4))
      (loop while (< at length)
            do (setf hash (logxor hash (wrap (* (aref octets at) +p5+)))
                     hash (wrap (* (rotl hash 11) +p1+)))
               (incf at))
      (setf hash (logxor hash (ash hash -33))
            hash (wrap (* hash +p2+))
            hash (logxor hash (ash hash -29))
            hash (wrap (* hash +p3+))
            hash (logxor hash (ash hash -32)))
      hash)))

;;; --- whose round it is -----------------------------------------------------------

(defun subscription-p (config)
  "Whether CONFIG, a frozen provider config on the anthropic lane, sends a
subscription token: one this cell answered, or a key that is one."
  (and config
       (equal (nle::effective-provider-config-lane config) "anthropic")
       (or (nle::credential-attribute config :subscription)
           (search "sk-ant-oat" (nle::effective-provider-config-api-key config)))
       t))

(defun uuid ()
  "A random UUID, version 4, lower case."
  (let ((bytes (nlk:random-bytes 16)))
    (setf (aref bytes 6) (logior #x40 (logand (aref bytes 6) #x0f))
          (aref bytes 8) (logior #x80 (logand (aref bytes 8) #x3f)))
    (flet ((hex (start end) (format nil "~(~{~2,'0x~}~)" (coerce (subseq bytes start end) 'list))))
      (format nil "~a-~a-~a-~a-~a" (hex 0 4) (hex 4 6) (hex 6 8) (hex 8 10) (hex 10 16)))))

(defvar *installation* (uuid)
  "The installation id a request is sent under when its credential carries none: this process's.")

(defvar *session* (uuid)
  "The session id a request outside any turn is sent under: this process's.")

(defun round-session ()
  "The session id this round runs in."
  (or (getf (nle:turn) :session-id) *session*))

(defun sha256-hex (text)
  "The lowercase hex SHA-256 of TEXT's UTF-8 bytes."
  (subseq (nlk:sha256-text text) 7))

(defun device-id (installation account)
  "omp's deriveClaudeDeviceId: a stable device id for INSTALLATION, and ACCOUNT when known."
  (if (plusp (length account))
      (sha256-hex (format nil "omp-claude-device-id-v2~c~a~c~a" (code-char 0) installation (code-char 0) account))
      (sha256-hex (format nil "omp-claude-device-id-v1:~a" installation))))

(defun metadata-user-id (config)
  "The metadata user id a subscription round carries: device, session, account, as JSON text."
  (let ((account (nle::credential-attribute config :account-id)))
    (nlk:encode-json-object
     (nlk:json-object "device_id" (device-id (or (nle::credential-attribute config :installation-id)
                                                 *installation*)
                                             account)
                      "session_id" (round-session)
                      :opt "account_uuid" account))))

;;; --- the body ----------------------------------------------------------------------

(defun first-user-text (messages)
  "The text of the first user turn of the wire MESSAGES that says something:
the billing fingerprint's seed (omp's extractClaudeCodeFirstWireUserMessageText)."
  (loop for message across (or messages #())
        when (equal "user" (nlk:json-value message :string "role"))
          do (let ((content (gethash "content" message)))
               (if (stringp content)
                   (when (plusp (length (string-trim " " content))) (return content))
                   (let ((blocks (coerce (or content #()) 'list)))
                     (alexandria:when-let
                         (text (find-if (lambda (block)
                                          (and (equal "text" (nlk:json-value block :string "type"))
                                               (plusp (length (string-trim " " (or (nlk:json-value block :string "text") ""))))))
                                        blocks))
                       (return (nlk:json-value text :string "text")))
                     (when (find "image" blocks :key (lambda (block) (nlk:json-value block :string "type"))
                                                :test #'equal)
                       (return "")))))
        finally (return "")))

(defun billing-header (first-text)
  "The billing header Claude Code puts first in its system prompt, its cch a
placeholder until the body is whole (omp's createClaudeBillingHeader)."
  (let* ((seed (coerce (loop for i in '(4 7 20)
                             collect (if (< i (length first-text)) (char first-text i) #\0))
                       'string))
         (suffix (subseq (sha256-hex (format nil "59cf53e54c78~a~a" seed +claude-code-version+)) 0 3)))
    (format nil "~a cc_version=~a.~a; cc_entrypoint=cli; cch=00000;"
            +billing-prefix+ +claude-code-version+ suffix)))

(defun wire-tool-name (name)
  "NAME as a subscription round's wire carries it (omp's applyClaudeToolPrefix)."
  (if (or (null name) (member name +builtin-tools+ :test #'string-equal))
      name
      (concatenate 'string +tool-prefix+ name)))

(defun shape-body (body config)
  "BODY, the Messages request the core built, as a subscription round sends it."
  (let ((system (coerce (or (gethash "system" body) #()) 'list)))
    ;; The identity block takes no cache mark of its own, as omp's does: the
    ;; core's mark on the last system block covers it, and a fifth mark would
    ;; be one past the four a request may carry.
    (setf (gethash "system" body)
          (coerce (list* (nlk:json-object "type" "text"
                                          "text" (billing-header
                                                  (first-user-text (gethash "messages" body))))
                         (nlk:json-object "type" "text" "text" +identity+)
                         system)
                  'vector))
    (setf (gethash "tools" body)
          (map 'vector (lambda (tool)
                         (nlk:copy-json-object tool "name" (wire-tool-name (nlk:json-value tool :string "name"))))
               (or (gethash "tools" body) #())))
    (loop for message across (or (gethash "messages" body) #())
          do (loop for block across (or (nlk:json-value message :array "content") #())
                   when (equal "tool_use" (nlk:json-value block :string "type"))
                     do (setf (gethash "name" block) (wire-tool-name (gethash "name" block)))))
    (alexandria:when-let (choice (nlk:json-value body :object "tool_choice"))
      (when (gethash "name" choice)
        (setf (gethash "tool_choice" body)
              (nlk:copy-json-object choice "name" (wire-tool-name (gethash "name" choice))))))
    (setf (gethash "metadata" body) (nlk:json-object "user_id" (metadata-user-id config)))
    body))

(defun patch-cch (octets)
  "OCTETS, the request body, with the billing header's cch placeholder
replaced by its attestation: the low 20 bits of the XXH64 of the body as it
stands with the placeholder in it (omp's patchCch). OCTETS itself when no
billing header opens the system prompt."
  (let* ((octets (coerce octets '(simple-array (unsigned-byte 8) (*))))
         (marker (sb-ext:string-to-octets
                  (format nil "\"system\":[{\"type\":\"text\",\"text\":\"~a" +billing-prefix+)
                  :external-format :utf-8))
         (placeholder (sb-ext:string-to-octets "cch=00000" :external-format :utf-8))
         (at (search marker octets))
         (from (and at (+ at (length marker))))
         (spot (and from (search placeholder octets :start2 from))))
    (if (and spot (<= (- spot from) 150))
        (let ((cch (format nil "~(~5,'0x~)" (ldb (byte 20 0) (xxh64 octets +cch-seed+))))
              (patched (copy-seq octets)))
          (replace patched (sb-ext:string-to-octets cch :external-format :latin-1) :start1 (+ spot 4))
          patched)
        octets)))

;;; --- the request's headers -----------------------------------------------------------

(defparameter +agent-betas+
  '("claude-code-20250219" "oauth-2025-04-20" "interleaved-thinking-2025-05-14"
    "thinking-token-count-2026-05-13" "context-management-2025-06-27"
    "prompt-caching-scope-2026-01-05" "mid-conversation-system-2026-04-07")
  "Claude Code's beta set for an agent request: one with tools or thinking.")

(defparameter +utility-betas+
  '("oauth-2025-04-20" "interleaved-thinking-2025-05-14" "thinking-token-count-2026-05-13"
    "context-management-2025-06-27" "prompt-caching-scope-2026-01-05" "structured-outputs-2025-12-15")
  "Claude Code's beta set for any other request.")

(defun betas (body)
  "The anthropic-beta list a subscription round of BODY sends (omp's buildClaudeCodeBetas)."
  ;; context-1m is never asked for: a seat has no long-context credit, and a
  ;; natively 1M model serves its window without it.
  (let ((thinking-p (let ((type (nlk:json-value body :string "thinking" "type")))
                      (and type (string/= type "disabled"))))
        (tools-p (plusp (length (or (nlk:json-value body :array "tools") #())))))
    (if (or tools-p thinking-p)
        (append +agent-betas+ (when thinking-p '("effort-2025-11-24")) '("fallback-credit-2026-06-01"))
        +utility-betas+)))

(defun stainless-os ()
  "The platform as the Stainless SDK names it."
  (case (uiop:operating-system)
    (:macosx "MacOS")
    (:windows "Windows")
    (:linux "Linux")
    (t (format nil "Other::~(~a~)" (uiop:operating-system)))))

(defun stainless-arch ()
  "The architecture as the Stainless SDK names it."
  (let ((machine (string-downcase (or (machine-type) ""))))
    (cond ((search "arm64" machine) "arm64")
          ((search "aarch64" machine) "arm64")
          ((or (search "x86-64" machine) (search "x86_64" machine) (search "amd64" machine)) "x64")
          ((search "x86" machine) "x86")
          (t (format nil "other::~a" machine)))))

(defparameter +replaced-headers+
  '("x-api-key" "authorization" "anthropic-version" "anthropic-beta" "user-agent" "accept")
  "The lane's headers a subscription round sends its own of.")

(defun round-headers (config betas lane-headers)
  "The headers of a subscription round asking for BETAS: the lane's but its
key and version, and Claude Code's own (omp's buildAnthropicHeaders, OAuth
branch)."
  (append (remove-if (lambda (pair) (member (car pair) +replaced-headers+ :test #'string-equal))
                     lane-headers)
          `(("Accept" . "application/json")
            ("User-Agent" . ,(format nil "claude-cli/~a (external, cli)" +claude-code-version+))
            ("X-Claude-Code-Session-Id" . ,(round-session))
            ("X-Stainless-Arch" . ,(stainless-arch))
            ("X-Stainless-Lang" . "js")
            ("X-Stainless-OS" . ,(stainless-os))
            ("X-Stainless-Package-Version" . ,+sdk-version+)
            ("X-Stainless-Retry-Count" . "0")
            ("X-Stainless-Runtime" . "node")
            ("X-Stainless-Runtime-Version" . "v26.3.0")
            ("X-Stainless-Timeout" . "600")
            ("anthropic-beta" . ,(format nil "~{~a~^,~}" betas))
            ("anthropic-dangerous-direct-browser-access" . "true")
            ("anthropic-version" . "2023-06-01")
            ("Authorization" . ,(format nil "Bearer ~a" (nle::effective-provider-config-api-key config)))
            ("x-app" . "cli"))))

(defun beta-endpoint (endpoint)
  "ENDPOINT with Claude Code's ?beta=true."
  (if (search "beta=true" endpoint)
      endpoint
      (format nil "~a~:[?~;&~]beta=true" endpoint (find #\? endpoint))))

(defun unprefixed-fold (fold)
  "FOLD, handed every frame with a tool call's name back under Nodecode's
own (omp's stripClaudeToolPrefix)."
  (lambda (frame finish record)
    (let ((block (and (equal "content_block_start" (nlk:json-value frame :string "type"))
                      (nlk:json-value frame :object "content_block"))))
      (when (equal "tool_use" (nlk:json-value block :string "type"))
        (let ((name (nlk:json-value block :string "name")))
          (when (and name (uiop:string-prefix-p +tool-prefix+ name))
            (setf (gethash "name" block) (subseq name (length +tool-prefix+)))))))
    (funcall fold frame finish record)))
