;;;; cell.lisp --- the cell: LLM API among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four hooks, each declining for every provider but llmapi, one command and
;;;; one manual topic:
;;;;
;;;;   MODELS-CATALOG-TABLE    the catalog carries LLM API's row: the chat
;;;;                            lane's package, this section's base, the key
;;;;                            variable, and the models the gateway's own
;;;;                            catalog names, so /models lists them and a turn
;;;;                            resolves its address the way it does any
;;;;                            catalog provider's
;;;;   :CREDENTIAL              LLMAPI_API_KEY, else no key: never the chat
;;;;                            family's default variable
;;;;   RESOLVE-MODEL-LANE       a Claude model rides the Anthropic Messages
;;;;                            lane, a GPT-5-or-later model the Responses
;;;;                            lane, everything else Chat Completions
;;;;   WALK-PROVIDER-STREAM     every request names this app to the gateway
;;;;
;;;;   /llmapi usage | models | refresh | status
;;;;
;;;; Config, a sibling top-level key:
;;;;   "llmapi": {"base_url": "https://api.llmapi.ai/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and LLM API is
;;;; then a provider no catalog row names.

(in-package #:nodecode-llmapi)

(defparameter *help*
  (format nil "llmapi: LLM API, one key for Claude, GPT, Gemini, Kimi, DeepSeek and more~%~
make a key at ~a and save it with /connect or set LLMAPI_API_KEY.~%~
A model is sent on the wire it works best on: claude-* on the Anthropic Messages API, ~
gpt-5 and later on the Responses API, everything else on Chat Completions, all at the ~
one base_url (default https://api.llmapi.ai/v1). The picker, context windows, image ~
input, reasoning levels and list prices come from the gateway's public catalog ~
(GET /v1/models, no key needed), read at start and hourly.~%~
/llmapi usage reads the account's credit balance; /llmapi models says what the catalog ~
holds; /llmapi refresh reads it again."
          +key-page+)
  "(help :llmapi) answers this: what the cell is, and what the command does.")

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an llmapi round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* nil
  "(CATALOG STAMP . MERGED): the last root the core answered, the catalog read
the row was built from, and that catalog with LLM API's row in it.")

(defun catalog (next)
  "MODELS-CATALOG-TABLE advice: the catalog with LLM API's row, made once per
catalog the core reads and once more when the gateway's model list changes."
  (let ((root (funcall next)) (stamp (feed-stamp)) (memo *catalog*))
    (if (and memo (eq (first memo) root) (eql (second memo) stamp))
        (third memo)
        (let ((merged (make-hash-table :test 'equal)))
          (when (hash-table-p root)
            (maphash (lambda (id provider) (setf (gethash id merged) provider)) root))
          (setf (gethash +provider+ merged)
                (catalog-row (and (hash-table-p root) (gethash +provider+ root))))
          (setf *catalog* (list root stamp merged))
          merged))))

(defun credential (op next)
  "The :CREDENTIAL answer for llmapi: LLMAPI_API_KEY, else none. A key
/connect saved in auth.json answers before this point."
  ;; Never NEXT for llmapi: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to the
  ;; gateway, which is another account's key.
  (if (equal (getf op :provider) +provider+)
      (let ((key (env-key)))
        (if key
            (nle:make-credential key :env)
            (nle:make-credential "public" :public)))
      (funcall next op)))

(defun lane (next provider model)
  "RESOLVE-MODEL-LANE advice: at LLM API the model's own name picks the wire,
so one key serves Claude on Messages and GPT-5 on Responses."
  (if (equal provider +provider+)
      (lane-for model)
      (funcall next provider model)))

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: an llmapi round names this app to the gateway,
on whichever wire it rides."
  (if (ours-p config)
      (apply next fold :headers (append headers +attribution+)
             (alexandria:remove-from-plist keys :headers))
      (apply next fold keys)))

(defun words (text)
  "TEXT split on white space, lower-cased: a command's words."
  (mapcar #'string-downcase
          (uiop:split-string (or text "") :separator '(#\Space #\Tab))))

(defun status-text ()
  "What /llmapi typed alone says: the address, whether a key is set, and what
the catalog holds."
  (let ((models (catalog-models))
        (stamp (feed-stamp)))
    (format nil "LLM API at ~a~%Key: ~a~%Models: ~a~@[, read ~a~]~%~a"
            (setting :base-url)
            (if (credential-key) "set" (format nil "none yet - make one at ~a" +key-page+))
            (if stamp (format nil "~d in the picker" (length models)) "the featured list, until the catalog is read")
            (and stamp (format-timestamp stamp))
            "Wire: claude-* Messages, gpt-5+ Responses, everything else Chat Completions")))

(defun format-timestamp (universal)
  "UNIVERSAL as `2026-01-31 12:00', so a line reads without a locale."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal)
    (declare (ignore second))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute)))

(defun usage-text ()
  "What /llmapi usage says: the credit balance, billing mode and cap."
  (let ((key (credential-key)))
    (if (null key)
        (format nil "LLM API: no key yet - make one at ~a, save it with /connect or set LLMAPI_API_KEY."
                +key-page+)
        (let ((payload (fetch-credits (setting :base-url) key +usage-timeout+)))
          (if payload
              (format nil "~{~a~^~%~}" (usage-lines payload))
              "LLM API: the balance could not be read - the gateway did not answer, or the key was refused.")))))

(defun models-text ()
  "What /llmapi models says: how many models the picker offers, and from where."
  (let ((stamp (feed-stamp))
        (models (catalog-models)))
    (if stamp
        (format nil "LLM API: ~d models in the picker, read ~a."
                (length models) (format-timestamp stamp))
        (format nil "LLM API: ~d featured models; the catalog has not been read yet - /llmapi refresh."
                (length models)))))

(defun refresh-text ()
  "What /llmapi refresh says: the catalog as it now stands."
  (multiple-value-bind (models read-p) (refresh-feed t)
    (if read-p
        (format nil "LLM API: read ~d models, ~d in the picker."
                (length models) (length (catalog-models)))
        "LLM API: the catalog did not answer, so the list already held stands.")))

(defun run-command (args session-id)
  "One /llmapi invocation."
  (declare (ignore session-id))
  (let ((verb (or (first (words args)) "")))
    (cond ((or (string= verb "") (string= verb "status")) (status-text))
          ((member verb '("usage" "credits") :test #'string=) (usage-text))
          ((string= verb "models") (models-text))
          ((string= verb "refresh") (refresh-text))
          (t (format nil "LLM API: /llmapi knows usage, models, refresh and status; not ~a." verb)))))

(defun start ()
  "Start clean, read the catalog once, and keep it an hour at a time."
  (setf *catalog* nil)
  (nle:on-stop (lambda () (setf *catalog* nil)))
  (nle:on-stop #'forget-feed)
  (ignore-errors (refresh-feed))
  (let ((worker (nlk:worker-start "llmapi-models" #'refresh-feed
                                  :wake +refresh-seconds+)))
    (nle:on-stop (lambda () (nlk:worker-stop worker)))))

(nle:define-cell llmapi
  (:section ("llmapi")
    (:guide "make a key at the dashboard; save it with /connect or set LLMAPI_API_KEY; base_url is where the gateway is served; /llmapi usage reads the balance; /llmapi refresh reads the catalog again")
    ("base_url" :string :default +base+
     :doc "the LLM API base each lane appends its path to: /chat/completions, /messages or /responses"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::resolve-model-lane #'lane)
  (:hook 'nle::walk-provider-stream #'walk)
  (:help :llmapi "LLM API: one key for Claude, GPT, Gemini, Kimi and more, each on the wire it works best on" *help*)
  (:command "llmapi" #'run-command
            :argument-hint "usage | models | refresh | status"
            :description "LLM API: the account's credits, and the models it serves"))
