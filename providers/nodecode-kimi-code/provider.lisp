;;;; provider.lisp --- what Kimi Code is: its address, its client identity, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; providers/kimi-code.kdl (the base, the key variable, the thinking
;;;; formats), packages/ai/src/providers/kimi.ts and openai-anthropic-shim.ts
;;;; (which wire a model rides and where), packages/ai/src/registry/oauth/
;;;; kimi.ts (the client fingerprint every request carries), the Anthropic
;;;; request rules of packages/ai/src/providers/anthropic.ts that apply to a
;;;; non-Anthropic host, and the bundled rows of catalog/src/models.json,
;;;; which models.json in this folder carries (tools/omp-models.py wrote it).
;;;;
;;;; Kimi Code serves two wires at https://api.kimi.com/coding/v1, an
;;;; OpenAI-compatible /chat/completions and an Anthropic-compatible
;;;; /messages. Every model omp bundles is declared on the Anthropic one
;;;; (compat.kimiApiFormat "anthropic"; only a live discovery answer naming no
;;;; protocol moves a model to the chat wire, and this cell lists no live
;;;; models), so a Kimi Code round is a Messages request, sent the way omp's
;;;; Anthropic client sends one to a host that is not Anthropic's: the
;;;; credential as a bearer, no x-api-key, and the Kimi CLI's fingerprint
;;;; headers on top.

(in-package #:nodecode-kimi-code)

(defparameter +base+ "https://api.kimi.com/coding/v1"
  "Where Kimi Code is served: the base the Messages lane appends /messages to.")

(defparameter +env+ '("KIMI_API_KEY")
  "The environment variables a Kimi Code key is read from, in order.")

(defparameter +client-version+ "18.8.3"
  "The version the fingerprint headers carry: omp sends its own package
version after KimiCLI/, and this is the version of the omp the cell ports.")

;;; --- the client fingerprint -----------------------------------------------------
;;; getKimiCommonHeaders: the platform, the version, and four facts about this
;;; machine, computed once per process. The device id is a random id kept in
;;; a file of the home so the same machine stays the same device across runs;
;;; a home it cannot be written to costs only that persistence.

(defvar *fingerprint* nil
  "The fingerprint headers, once computed.")

(defun sanitized (value &optional (fallback "unknown"))
  "VALUE with every character outside printable ASCII taken out and trimmed,
or FALLBACK when nothing is left: a header value must be plain ASCII."
  (let ((clean (string-trim " " (remove-if-not (lambda (c) (<= 32 (char-code c) 126))
                                               (or value "")))))
    (if (plusp (length clean)) clean fallback)))

(defun device-model ()
  "The machine as Node's os module names it: platform, kernel release, arch."
  (let ((label (case (uiop:operating-system)
                 (:macosx "macOS")
                 (:windows "Windows")
                 (:linux "Linux")
                 (t (string-downcase (uiop:operating-system)))))
        ;; Node's os.arch(): x64 and arm64, which uiop spells the same way
        (arch (string-downcase (uiop:architecture))))
    (format nil "~{~a~^ ~}" (remove "" (list label (software-version) arch) :test #'equal))))

(defun os-version ()
  "Node's os.version(): the kernel's version string (uname -v)."
  (or (ignore-errors (uiop:read-file-line "/proc/sys/kernel/version"))
      (ignore-errors (uiop:run-program '("uname" "-v") :output '(:string :stripped t)))
      (software-version)))

(defun device-id ()
  "This install's Kimi device id: the one kept in the home's kimi-device-id
file, else a fresh random one, written there for next time."
  (let ((path (nlk:home "kimi-device-id")))
    (or (let ((kept (ignore-errors (nlk:trimmed (uiop:read-file-string path)))))
          (and (plusp (length kept)) kept))
        (let ((id (let ((state (make-random-state t)))
                    (format nil "~(~{~2,'0x~}~)" (loop repeat 16 collect (random 256 state))))))
          (ignore-errors
           (nlk:write-file-atomically path (format nil "~a~%" id) :mode #o600 :directory-mode #o700))
          id))))

(defun client-headers ()
  "The Kimi CLI's identity, which every Kimi Code request carries, the
sign-in's included."
  (or *fingerprint*
      (setf *fingerprint*
            `(("User-Agent" . ,(format nil "KimiCLI/~a" +client-version+))
              ("X-Msh-Platform" . "kimi_cli")
              ("X-Msh-Version" . ,+client-version+)
              ("X-Msh-Device-Name" . ,(sanitized (machine-instance)))
              ("X-Msh-Device-Model" . ,(sanitized (device-model)))
              ("X-Msh-Os-Version" . ,(sanitized (os-version)))
              ("X-Msh-Device-Id" . ,(sanitized (device-id)))))))

(defun session-id ()
  "The session the round on this thread belongs to, or NIL outside a turn."
  (let ((turn nle::*current-durable-turn*))
    (and turn (nlk:durable-turn-session-id turn))))

(defun request-headers (headers key)
  "HEADERS, the Messages lane's own, as omp sends them to a host that is not
Anthropic's: the credential KEY as a bearer instead of x-api-key, the two
headers its shared set adds, the session as omp's Anthropic client names it
on every Messages request, and the Kimi fingerprint."
  (append (remove "x-api-key" headers :key #'car :test #'string-equal)
          `(("Authorization" . ,(format nil "Bearer ~a" key))
            ("x-app" . "cli")
            ("anthropic-dangerous-direct-browser-access" . "true"))
          (alexandria:when-let (session (session-id))
            `(("X-Claude-Code-Session-Id" . ,session)))
          (client-headers)))

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-kimi-code" "models.json")))
  "omp's bundled Kimi Code rows, read when this file loads: a vector of objects.")

(defparameter +adaptive-models+ '("k3" "k3-256k" "kimi-for-coding")
  "The models whose thinking format is Kimi's own (compat.thinkingFormat
\"kimi\"): omp asks them for adaptive thinking on the Messages wire, an
effort and no budget. The others think on a budget.")

(defparameter +effort-required-models+ '("k3" "k3-256k")
  "The models whose endpoint refuses a request with thinking off
(thinking.requiresEffort): an off request asks for the lowest effort.")

(defparameter +keep-thinking-beta+ "context-management-2025-06-27"
  "The beta the context_management field rides under.")

(defparameter +interleaved-beta+ "interleaved-thinking-2025-05-14"
  "The beta omp's Anthropic client sends with every request to a model that
does not declare its own thinking display, which no Kimi model does.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (find model-id +models+ :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

(defun lowest-effort (model-id)
  "The weakest rung MODEL-ID's row offers, or NIL."
  (first (sort (remove-if-not #'nle::effort-rank
                              (coerce (or (nlk:json-value (model-row model-id) :array "efforts") #()) 'list))
               #'< :key #'nle::effort-rank)))

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
       ;; CATALOG-PRICE's shape; a row priced at nothing is a plan's, not a price
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "Kimi Code as a models.dev provider: the Messages lane's package, this
section's base, the key variable, and the bundled models over PRIOR's (the
row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Kimi Code"
                     "npm" "@ai-sdk/anthropic"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defparameter +listing-headers+ '(("User-Agent" . "KimiCLI/1.0") ("X-Msh-Platform" . "kimi_cli"))
  "The headers omp's discovery asks the model listing with: a fixed client
identity, not the fingerprint a round carries.")

(defun list-models (key base)
  "(values ROWS ERROR): Kimi Code's model listing at BASE, asked with KEY the
way omp's discovery asks it, or NIL and why."
  (if (or (null key) (equal key "public"))
      (values nil "not signed in")
      (multiple-value-bind (body status)
          (nle::http-fetch (format nil "~a/models" (string-right-trim "/" base))
                           :headers `(("Authorization" . ,(format nil "Bearer ~a" key))
                                      ,@+listing-headers+)
                           :timeout nle::*provider-models-fetch-timeout-seconds*)
        (if (stringp status)
            (values nil status)
            (nle::parse-provider-listing (nlk:body-text body))))))

;;; --- the Messages body -------------------------------------------------------

(defun thinking-body (body model effort betas)
  "BODY, the Messages lane's request for MODEL at EFFORT (NIL for off), set
the way omp sets it for Kimi Code; => the BETAS the request carries."
  ;; A Kimi-format model thinks adaptively: {type: adaptive} and the effort in
  ;; output_config, minimal spoken as low. Off on such a model is no thinking
  ;; field and the lowest effort, except where the endpoint refuses off, which
  ;; asks for the lowest rung instead. Any thinking request then keeps every
  ;; replayed thinking block (clear_thinking keep all): without it the
  ;; Anthropic-compatible backend strips the thinking the history replays,
  ;; and the model loses its reasoning chain across rounds. Every request
  ;; carries the interleaved-thinking beta, as omp's client sends it.
  (when (member model +adaptive-models+ :test #'equal)
    (let ((effort (or effort
                      (and (member model +effort-required-models+ :test #'equal)
                           (lowest-effort model)))))
      (remhash "thinking" body)
      (remhash "output_config" body)
      (cond (effort
             (setf (gethash "thinking" body) (nlk:json-object "type" "adaptive")
                   (gethash "output_config" body)
                   (nlk:json-object "effort" (if (string-equal effort "minimal") "low" effort)))
             (remhash "temperature" body)
             (remhash "top_p" body))
            (t (setf (gethash "output_config" body) (nlk:json-object "effort" "low"))))))
  (let ((betas (adjoin +interleaved-beta+ betas :test #'equal)))
    (if (member (nlk:json-value body :string "thinking" "type") '("adaptive" "enabled") :test #'equal)
        (progn
          (setf (gethash "context_management" body)
                (nlk:json-object "edits" (vector (nlk:json-object "type" "clear_thinking_20251015"
                                                                  "keep" "all"))))
          (adjoin +keep-thinking-beta+ betas :test #'equal))
        betas)))
