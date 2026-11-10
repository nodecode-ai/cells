;;;; provider.lisp --- what NetMind is: its address, its key, its roster, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from NetMind-AI/netmind-hermes-provider (see NOTICE):
;;;; __init__.py's OUTPUT_LIMITS table (each model's answer cap), its
;;;; fallback_models tuple (the roster, in its own order) and its
;;;; model_capabilities object (window, vision, tools, reasoning), and
;;;; plugin.yaml's NETMIND_API_KEY.
;;;;
;;;; NetMind is an OpenAI-compatible inference service: one base, a bearer
;;;; key, the chat completions wire. Its Hermes plugin declares no model
;;;; listing and no health check, so the roster below is the one it bundles
;;;; rather than one asked of the service. Four models there take no tool
;;;; calls, and a turn always offers tools, so /models leaves those four to a
;;;; model pinned in providers.netmind.models by hand.

(in-package #:nodecode-netmind)

(defparameter +base+ "https://api.netmind.ai/inference-api/v1"
  "Where NetMind is served: the base the chat lane appends /chat/completions to.")

(defparameter +key-page+ "https://www.netmind.ai/"
  "NetMind's own page, where a key is made.")

(defparameter +models+
  '(("openai/gpt-6-astra" 1000000 128000 t t nil)
    ("openai/gpt-6-sol" 1000000 128000 t t nil)
    ("openai/gpt-6-luna" 1000000 128000 t t nil)
    ("openai/gpt-6.1-sol" 1000000 128000 t t nil)
    ("openai/gpt-5.6-sol" 1000000 128000 t t t)
    ("openai/gpt-5.6-terra" 1000000 128000 t t t)
    ("openai/gpt-5.6-luna" 1000000 128000 t t t)
    ("openai/gpt-5.5" 1000000 128000 t t t)
    ("openai/gpt-5.4" 1000000 128000 t t t)
    ("openai/gpt-5.4-mini" 400000 128000 t t t)
    ("anthropic/claude-opus-5-5" 1000000 128000 t t t)
    ("anthropic/claude-sonnet-5-5" 1000000 128000 t t t)
    ("anthropic/claude-haiku-5-5" 1000000 128000 t t t)
    ("anthropic/claude-fable-5-1" 1000000 128000 t t t)
    ("anthropic/claude-sonnet-5" 1000000 128000 t t t)
    ("anthropic/claude-haiku-4-5" 200000 64000 t t t)
    ("deepseek-ai/DeepSeek-V4-Pro" 1000000 131072 t nil t)
    ("deepseek-ai/DeepSeek-V4.1-Flash" 1000000 384000 t nil t)
    ("deepseek-ai/DeepSeek-V4-Flash-Vision-Exp" 1000000 384000 t t t)
    ("Qwen/Qwen3.8-Max" 1000000 131072 t t t)
    ("Qwen/Qwen3.8-Flash" 1000000 131072 t t t)
    ("Qwen/Qwen3.7-Plus" 1000000 65536 t t t)
    ("zai-org/GLM-5.3" 1000000 131072 t nil t)
    ("zai-org/GLM-5.3-Flash" 1000000 131072 t t t)
    ("zai-org/GLM-5.3-FlashX" 1000000 131072 t t t)
    ("google/gemini-3.8-flash" 1000000 65536 t t t)
    ("moonshotai/Kimi-K3" 1000000 131072 t t t)
    ("minimax/minimax-m3" 1000000 128000 t t t)
    ("x-ai/grok-4.7" 500000 450000 t t t)
    ("XiaomiMiMo/MiMo-V2.5-Pro" 1000000 131072 t nil t)
    ("XiaomiMiMo/MiMo-V2.5" 1000000 131072 t nil t))
  "NetMind's roster, as the plugin bundles it: (ID CONTEXT OUTPUT REASONING
VISION TOOLS), in its own order, the window and the answer cap in tokens.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (find model-id +models+ :key #'first :test #'equal))

(defun catalog-model (row)
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields)."
  (destructuring-bind (id context output reasoning vision tools) row
    (nle::make-catalog-model id
                             context
                             output
                             (if vision #("text" "image") #("text"))
                             #("text")
                             ;; the plugin declares no reasoning ladder, only that a model reasons
                             nil
                             reasoning
                             ;; and no release date and no price
                             nil
                             tools
                             nil)))

;;; --- the address ------------------------------------------------------------

(defun loopback-host-p (host)
  "Whether HOST is one of this machine's own loopback names or addresses."
  (let ((host (string-downcase (or host ""))))
    (or (string= host "localhost")
        (string= host "::1")
        (string= host "[::1]")
        (string= host "0:0:0:0:0:0:0:1")
        (uiop:string-prefix-p "127." host))))

(defun refuse (control &rest arguments)
  "Signal a config refusal saying CONTROL, formatted with ARGUMENTS."
  (error 'nlk:config-refusal :detail (apply #'format nil control arguments)))

(defun split-address (text)
  "TEXT as (SCHEME AUTHORITY PATH) when it is scheme://authority[/path], else NIL."
  (let* ((mark (search "://" text))
         (scheme (and mark (plusp mark) (string-downcase (subseq text 0 mark))))
         (rest (and scheme (subseq text (+ mark 3))))
         (slash (and rest (position #\/ rest)))
         (authority (and rest (subseq rest 0 (or slash (length rest)))))
         (path (and rest (if slash (subseq rest slash) ""))))
    (and scheme (plusp (length authority)) (list scheme authority path))))

(defun validate-base-url (value)
  "VALUE as the base this cell sends a key to, right-trimmed of slashes;
signals a refusal naming what is wrong with it."
  (let ((text (string-trim " " (or value ""))))
    (when (zerop (length text))
      (refuse "netmind: base_url is empty; it is ~a unless your service is elsewhere" +base+))
    (when (find-if (lambda (character)
                     (or (<= (char-code character) 32) (= (char-code character) 127)))
                   text)
      (refuse "netmind: base_url must hold no spaces, control characters or line breaks"))
    (when (or (find #\\ text) (find #\? text) (find #\# text))
      (refuse "netmind: base_url may carry no query, fragment or backslash; the address alone, like ~a"
              +base+))
    (let* ((parts (split-address text))
           (scheme (first parts))
           (authority (second parts)))
      (unless scheme
        (refuse "netmind: base_url must be an address like ~a" +base+))
      (unless (member scheme '("https" "http") :test #'string=)
        (refuse "netmind: base_url must use https; http is allowed only on this machine's loopback"))
      (when (or (find #\@ authority) (find #\% authority))
        (refuse "netmind: base_url names no usable host; no credentials or escapes belong in it"))
      (let* ((colon (position #\: authority))
             (host (if colon (subseq authority 0 colon) authority))
             (port (and colon (subseq authority (1+ colon)))))
        (when (zerop (length host))
          (refuse "netmind: base_url names no host; an address like ~a" +base+))
        (when (and port
                   (or (zerop (length port))
                       (not (every #'digit-char-p port))
                       (not (<= 1 (parse-integer port) 65535))))
          (refuse "netmind: base_url's port must be a number from 1 to 65535"))
        (when (and (string= scheme "http") (not (loopback-host-p host)))
          (refuse "netmind: base_url over http must be on this machine's loopback; ~a is remote, so it needs https"
                  host)))
      (string-right-trim "/" text))))

(defparameter *base* nil
  "This start's validated base: the address the catalog row names and a turn dials.")

(defun gateway ()
  "This section's base, right-trimmed: the address a turn dials."
  (or *base* (string-right-trim "/" (or (setting :base-url) +base+))))

(defun check-config ()
  "Refuse a section whose base this cell would not send a key to."
  (setf *base* (validate-base-url (setting :base-url))))

;;; --- the catalog row ---------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
NetMind's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with NetMind's row, made once per
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

(defun catalog-row (&optional prior)
  "NetMind as a models.dev provider: the chat lane's package, this section's
base, the key variable, and the bundled roster over whatever PRIOR (models.dev's
own row, when it publishes one) lists."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row in +models+
          do (setf (gethash (first row) models) (catalog-model row)))
    (nlk:json-object "name" "NetMind"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (gateway)
                     "env" (vector +env+)
                     "models" models)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

;;; --- the key ------------------------------------------------------------------

(defun credential ()
  "The key this image would send, or NIL: the one /connect saved, then
NETMIND_API_KEY, and never another lane's."
  (let ((credential (ignore-errors (nle::resolve-provider-credential +provider+ :probe t))))
    (and credential
         (not (eq :public (nle:credential-source credential)))
         (nle:credential-key credential)
         credential)))

(defun key-state ()
  "Where a NetMind key comes from, in words: never the key itself."
  (let ((credential (credential)))
    (cond ((null credential)
           (format nil "no key yet: save one with /connect, NetMind, or set ~a" +env+))
          ((eq :env (nle:credential-source credential)) (format nil "a key read from ~a" +env+))
          (t "a key saved in auth.json"))))

;;; --- the command's answer ----------------------------------------------------

(defun model-line (row)
  "ROW as /netmind shows it: its id, its window and answer cap, and whether
it takes tool calls and images."
  (destructuring-bind (id context output reasoning vision tools) row
    (declare (ignore reasoning))
    (format nil "  ~a (~:d context, ~:d answer tokens~:[; no tool calls~;~]~:[; text only~;~])"
            id context output tools vision)))

(defun models-report ()
  "The /netmind answer: the service, where its key comes from, and its roster."
  (format nil "NetMind at ~a bundles ~d models. ~a.~%~{~a~^~%~}"
          (gateway) (length +models+) (key-state)
          (mapcar #'model-line +models+)))

(defun key-report ()
  "The /netmind key answer: the service, the key's source, and where to make one."
  (format nil "NetMind at ~a: ~a. Make a key at ~a, save it with /connect, NetMind, or set ~a."
          (gateway) (key-state) +key-page+ +env+))

(defun run-command (args session-id)
  "The /netmind command: the models NetMind serves, and where its key comes from."
  (declare (ignore session-id))
  (let ((args (string-downcase (string-trim " " (or args "")))))
    (cond ((or (string= args "") (string= args "models")) (models-report))
          ((string= args "key") (key-report))
          (t (concatenate 'string
                          "usage: /netmind -- the models NetMind serves"
                          (format nil "~%/netmind key -- where the key comes from"))))))

;;; --- what the model reads ----------------------------------------------------

(defparameter +manual+
  "NetMind is an OpenAI-compatible inference service: a turn on provider netmind goes to the section's base over the chat completions wire, with a key made at https://www.netmind.ai/. Save a key with /connect, NetMind, or set NETMIND_API_KEY. The roster is the one the service's Hermes plugin bundles, each model with the context window it accepts and the tokens it answers; /netmind lists it, /netmind key says where the key comes from, and /models is the picker the turn's model is set from. A turn always offers tools, so the four gpt-6 models declared without tool calls are not offered by /models: pin one in providers.netmind.models to run on it. base_url names where NetMind is served, https unless it is on this machine's loopback."
  "What (help :netmind) answers, and the line every request carries.")
