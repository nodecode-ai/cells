;;;; provider.lisp --- what Cursor is: its address, its identity, its models, how a model is named on the wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/cursor.kdl and providers/cursor.kdl, catalog/src/wire/cursor.ts (the
;;;; client headers and the RPC paths), the wire-model resolution of
;;;; ai/src/providers/cursor.ts (resolveCursorWireModel, resolveCursorMaxMode),
;;;; catalog/src/model-thinking.ts (resolveWireModelId), the cursor rows of
;;;; catalog/src/compat/rules/taxonomy/_collapse.kdl as taxonomy.ts reads them
;;;; (collapseVariantId), runtime/behavior.kdl (the fixed model parameters),
;;;; and the bundled rows of catalog/src/models.json: models.json in this
;;;; folder carries what the catalog reads (tools/omp-models.py wrote it),
;;;; wire.json what the wire reads (tools/wire-rows.py here wrote it).
;;;;
;;;; Cursor serves Claude, GPT, Gemini, Grok, Kimi and its own Composer models
;;;; to a Cursor account through its agent service at https://api2.cursor.sh.
;;;; The service runs the agent loop itself and asks the client to run the
;;;; tools (agent.lisp, wire.lisp). Requests carry the Cursor agent CLI's
;;;; identity; the service gates protocol features on the client version.

(in-package #:nodecode-cursor)

(defparameter +base+ "https://api2.cursor.sh"
  "Where Cursor's agent service and account API are served.")

(defparameter +client-version+ "cli-2026.09.02-c22c1a3"
  "The released Cursor agent CLI build whose protocol this client mirrors.")

(defparameter +run-sse-path+ "/agent.v1.AgentService/RunSSE"
  "The server-streaming half of a run over HTTP/1.1.")

(defparameter +bidi-append-path+ "/aiserver.v1.BidiService/BidiAppend"
  "The unary call each client message of a run rides over HTTP/1.1.")

(defparameter +usable-models-path+ "/agent.v1.AgentService/GetUsableModels"
  "The account's runnable model roster.")

(defparameter +env+ '("CURSOR_ACCESS_TOKEN" "CURSOR_API_KEY")
  "The environment variables a Cursor access token is read from, in order:
the provider's own, then the one omp's discovery reads.")

(defun client-headers (token &key (content-type "application/proto"))
  "The headers every Cursor RPC carries (cursorClientHeaders)."
  `(("content-type" . ,content-type)
    ("authorization" . ,(format nil "Bearer ~a" token))
    ("x-cursor-client-type" . "cli")
    ("x-cursor-client-version" . ,+client-version+)
    ("x-ghost-mode" . "true")))

;;; --- the models ----------------------------------------------------------------

(defun bundled-rows (name)
  "The rows of the file NAME in this folder: a vector of objects."
  (nlk:decode-json (uiop:read-file-string (asdf:system-relative-pathname "nodecode-cursor" name))))

(defparameter +models+ (bundled-rows "models.json")
  "omp's bundled Cursor rows as the catalog reads them, read when this file loads.")

(defparameter +wire-rows+ (bundled-rows "wire.json")
  "omp's bundled Cursor rows as the wire reads them: the request id, the
effort routing, the max-mode markers, the identity.")

(defun find-row (rows model-id)
  "The row of ROWS whose id is MODEL-ID, or NIL."
  (find model-id rows :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

(defvar *discovered* (make-hash-table :test #'equal :synchronized t)
  "Model id -> the max-mode marker the account's listing gave it, for a model
the bundled rows do not carry.")

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
  "Cursor as a models.dev provider: this cell's lane package, this section's
base, the token variables, and the bundled models over PRIOR's (the row
models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Cursor (Claude, GPT, etc.)"
                     "npm" "nodecode-cursor"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-token ()
  "The first token one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

;;; --- the model a round names on the wire ------------------------------------------

(defparameter +thinking-efforts+ '("minimal" "low" "medium" "high" "xhigh" "max")
  "omp's THINKING_EFFORTS, weakest first.")

(defparameter +effort-suffixes+
  ;; (SUFFIX EFFORT THINKING-P EXCEPT-BARE-PREFIX), _collapse.kdl's
  ;; thinking-suffix and effort-suffix rows
  '(("-thinking" nil t nil)
    ("-extra-high" "xhigh" nil nil)
    ("-none" "off" nil nil)
    ("-minimal" "minimal" nil nil)
    ("-medium" "medium" nil nil)
    ("-xhigh" "xhigh" nil nil)
    ("-high" "high" nil nil)
    ("-low" "low" nil nil)
    ("-max" "max" nil "qwen"))
  "The variant suffixes omp's taxonomy collapses a sibling id by.")

(defparameter +lane-suffixes+ '("-fast")
  "Cursor's parallel SKU lanes: they stay in the logical id while the effort
token before them selects the sibling.")

(defun bare-of (id)
  "ID after its last slash."
  (subseq id (1+ (or (position #\/ id :from-end t) -1))))

(defun suffix-rule (lower &key effort-only)
  "The longest +EFFORT-SUFFIXES+ rule LOWER ends with, honouring each rule's
bare-prefix exception; EFFORT-ONLY skips the thinking rule."
  (let ((bare (bare-of lower)) (winner nil))
    (dolist (rule +effort-suffixes+ winner)
      (destructuring-bind (suffix effort thinking except) rule
        (declare (ignore thinking))
        (when (and (uiop:string-suffix-p lower suffix)
                   (not (and except (uiop:string-prefix-p except bare)))
                   (or effort (not effort-only))
                   (or (null winner) (> (length suffix) (length (first winner)))))
          (setf winner rule))))))

(defun collapse-variant-id (model)
  "(values LOGICAL-ID EFFORT THINKING-P) of the Cursor wire id MODEL
(collapseVariantId): the effort tier its suffix names, its lane kept."
  (let* ((lower (string-downcase model))
         (rule (suffix-rule lower)))
    (if rule
        (values (subseq model 0 (- (length model) (length (first rule)))) (second rule) (third rule))
        (dolist (lane +lane-suffixes+ (values model nil nil))
          (when (uiop:string-suffix-p lower lane)
            (let* ((trimmed (subseq lower 0 (- (length lower) (length lane))))
                   (effort-rule (suffix-rule trimmed :effort-only t)))
              (when effort-rule
                (let ((base (subseq model 0 (- (length trimmed) (length (first effort-rule))))))
                  (when (and (plusp (length base)) (not (uiop:string-suffix-p base "/")))
                    (return (values (concatenate 'string base (subseq model (length trimmed)))
                                    (second effort-rule) nil)))))))))))

(defun max-mode-wire-id-p (wire-id)
  "Whether WIRE-ID's effort tier is one Cursor serves in max mode
(isCursorMaxModeWireId)."
  (and (member (nth-value 1 (collapse-variant-id wire-id)) '("xhigh" "max") :test #'equal) t))

(defun model-class (model-id)
  "The lineage class of MODEL-ID: its bundled identity, else what its id says."
  (or (nlk:json-value (find-row +wire-rows+ model-id) :string "class")
      (let ((id (string-downcase (bare-of model-id))))
        (cond ((ppcre:scan "^(?:gpt-\\d|o\\d)" id) "openai")
              ((ppcre:scan "^claude" id) "anthropic")
              ((ppcre:scan "kimi|^k\\d" id) "kimi")
              ((ppcre:scan "grok" id) "xai")
              ((ppcre:scan "^gemini" id) "gemini")))))

(defun k3-p (model-id)
  "Whether MODEL-ID is a Kimi K3 model, whose thinking replays (family k3)."
  (let ((family (nlk:json-value (find-row +wire-rows+ model-id) :string "family")))
    (if family
        (equal family "k3")
        (and model-id (ppcre:scan "(?i)(?:^|[-/])k3(?:$|-)" model-id) t))))

(defparameter +fixed-parameters+ '(("composer-2.5" ("fast" . "false")))
  "Per exact wire id, the requestedModel parameters runtime/behavior.kdl pins:
a bare composer-2.5 resolves to the Fast tier server-side unless told.")

(defun model-reasoning-p (model-id)
  "Whether MODEL-ID thinks, as its bundled row says; an unknown model may."
  (let ((row (find-row +models+ model-id)))
    (if row (nlk:json-value row :boolean "reasoning") t)))

(defun model-efforts (model-id)
  "The efforts MODEL-ID's bundled row declares, or NIL."
  (coerce (or (nlk:json-value (find-row +models+ model-id) :array "efforts") #()) 'list))

(defun round-effort (model-id effort)
  "The effort a round on MODEL-ID asks for, or NIL for thinking off: omp
sends none for a model that does not think, and refuses an effort the model
does not take (requireSupportedEffort)."
  (when (and effort (not (equal effort "off")) (model-reasoning-p model-id))
    (let ((efforts (model-efforts model-id)))
      (when (and efforts (not (member effort efforts :test #'equal)))
        (error 'nle::provider-config-error
               :detail (format nil "Thinking effort ~a is not supported by ~a/~a. Supported efforts: ~{~a~^, ~}"
                               effort +provider+ model-id efforts)))
      effort)))

(defun wire-model-id (model-id effort)
  "The wire id a round on MODEL-ID at EFFORT (NIL: off) is routed to
(resolveWireModelId): the row's effort routing, else its request id, else
the id itself."
  (let ((row (find-row +wire-rows+ model-id)))
    (or (nlk:json-value row :string "routing" (or effort "off"))
        (nlk:json-value row :string "request")
        model-id)))

(defun row-max-mode (row model-id)
  "The row's cursorMaxMode: :TRUE, :FALSE, or NIL when it says nothing."
  (multiple-value-bind (value present) (and row (gethash "max_mode" row))
    (cond (present (if value :true :false))
          (t (multiple-value-bind (discovered known) (gethash model-id *discovered*)
               (and known (if discovered :true :false)))))))

(defun resolve-max-mode (model-id wire-id)
  "Whether the round sends WIRE-ID in max mode (resolveCursorMaxMode): the
routed member's own marker, else the row's, else what the id's tier says."
  (let* ((row (find-row +wire-rows+ model-id))
         (routes (nlk:json-value row :object "max_routes"))
         (routing (nlk:json-value row :object "routing"))
         (marker (row-max-mode row model-id)))
    (flet ((own-marker (id) (if marker (eq marker :true) (max-mode-wire-id-p id))))
      (multiple-value-bind (discovered present) (and routes (gethash wire-id routes))
        (cond
          (present (and discovered t))
          ((or (null routing) (equal wire-id model-id)) (own-marker wire-id))
          (t (let ((routes-own-id (equal (gethash "off" routing) model-id))
                   (inferred-max (and (stringp (gethash "off" routing))
                                      (max-mode-wire-id-p (gethash "off" routing)))))
               (dolist (effort +thinking-efforts+)
                 (let ((target (gethash effort routing)))
                   (when (equal target model-id) (setf routes-own-id t))
                   (when (and (stringp target) (max-mode-wire-id-p target)) (setf inferred-max t))))
               (cond (routes-own-id (own-marker wire-id))
                     ((and (eq marker :true) (not inferred-max)) t)
                     (t (max-mode-wire-id-p wire-id))))))))))

(defun resolve-wire-model (model-id wire-id &optional (mode :normalized))
  "(values MODEL-ID DETAILS-ID PARAMETERS MAX-MODE) the request names for
WIRE-ID (resolveCursorWireModel): PARAMETERS a list of (ID . VALUE).

Cursor validates the two ids apart: modelDetails keeps the account-usable
sibling slug, requestedModel the base id with its parameters. MODE
:DISCOVERED sends the wire id as it is, the retry after the service did
not know the normalized pair."
  (let ((max-mode (resolve-max-mode model-id wire-id)))
    (if (eq mode :discovered)
        (values wire-id wire-id '() max-mode)
        (multiple-value-bind (logical effort) (collapse-variant-id wire-id)
          (let ((fixed (cdr (assoc wire-id +fixed-parameters+ :test #'equal))))
            (cond ((and effort (plusp (length logical)) (equal "openai" (model-class logical))
                        (or (equal effort "off") (member effort +thinking-efforts+ :test #'equal)))
                   (values logical wire-id
                           (if (equal effort "off") '() (list (cons "reasoning" effort)))
                           max-mode))
                  (fixed (values wire-id wire-id fixed max-mode))
                  (t (values wire-id wire-id '() max-mode))))))))

(defun tool-schema-projection-p (model-id)
  "Whether MODEL-ID's advertised tool schemas must be projected onto what
Cursor's MCP catalog accepts (requiresCursorToolSchemaProjection)."
  (or (nlk:json-value (find-row +wire-rows+ model-id) :boolean "projection")
      ;; the provider rule: class anthropic, family fable
      (and (ppcre:scan "(?i)fable" (or model-id "")) t)))
