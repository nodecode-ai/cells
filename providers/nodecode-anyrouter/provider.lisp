;;;; provider.lisp --- what AnyRouter is: its address, its key, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from AnyRouterHQ/hermes-provider-anyrouter (see NOTICE):
;;;; __init__.py's profile, after-install.md and plugin.yaml.
;;;;
;;;; AnyRouter speaks the OpenAI chat wire at https://anyrouter.dev/api/v1
;;;; with a bearer key made at anyrouter.dev, and routes a `vendor/model'
;;;; slug to whichever upstream serves it. What the original adds:
;;;;
;;;;   - the model list is AnyRouter's own catalog, GET /v1/models, cut down
;;;;     to the routes that take tool calls (a `capabilities' list naming
;;;;     function-calling or tools, or supported_parameters naming tools);
;;;;     /anyrouter reads it on demand and puts it in /models, and the
;;;;     original's fallback rows stand in until it is read
;;;;   - the catalog prices a route in `input_per_1m' / `output_per_1m', which
;;;;     is the per-million shape Nodecode's catalog carries, so a round on a
;;;;     priced route is costed here rather than by a reader of its own
;;;;   - the session id rides as session_id, so related requests group in
;;;;     AnyRouter's Request Logs dashboard
;;;;   - routing preferences (only, ignore, order, sort, allow_fallbacks,
;;;;     max_price) ride as the body's `provider'
;;;;   - reasoning rides as AnyRouter's own `reasoning' object, which
;;;;     AnyRouter translates to each upstream's thinking control
;;;;   - app-attribution headers are off unless the section asks for them;
;;;;     when it does, they name Nodecode

(in-package #:nodecode-anyrouter)

(defparameter +base+ "https://anyrouter.dev/api/v1"
  "Where AnyRouter is served: the base the chat lane appends /chat/completions to.")

(defparameter +env+ '("ANYROUTER_API_KEY")
  "The environment variables a key is read from, in order.")

(defparameter +key-page+ "https://anyrouter.dev"
  "Where a key is made.")

(defparameter +app-url+ "https://nodecode.ai"
  "The app AnyRouter attributes a request to: its HTTP-Referer, when the
section asks for attribution.")

(defparameter +app-name+ "Nodecode"
  "The app's title in AnyRouter's rankings: X-AnyRouter-Title.")

(defparameter +efforts+ '("minimal" "low" "medium" "high")
  "The rungs a reasoning route is offered: AnyRouter takes the OpenAI-style
effort and translates it to each upstream's thinking control.")

(defparameter +agentic-capabilities+ '("function-calling" "tools")
  "The capability markers in AnyRouter's catalog that mean a route takes tool
calls; Hermes' own set, since a route that takes none is no use to an agent.")

(defparameter +models+
  '(("anyrouter/hermes" . nil)
    ("anyrouter/auto" . nil)
    ("x-ai/grok-4.7" . nil)
    ("moonshotai/kimi-k3" . nil)
    ("deepseek/deepseek-v4-flash" . nil)
    ("google/gemini-3.5-flash" . nil)
    ("z-ai/glm-5.3-flash" . nil)
    ("minimax/m3" . nil))
  "The routes the original's profile lists as its fallback, shown until
/anyrouter reads the live catalog: (ID . CONTEXT), the context NIL where the
original names none. The two anyrouter/* meta-models auto-route across the
upstreams; the rest are direct vendor routes.")

;;; --- the models ----------------------------------------------------------------

(defun row-model (id name &key context price reasoning (vision t))
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields). A
route takes images unless the catalog says it takes text alone: AnyRouter
relays multimodal content to whichever upstream serves the model, and the
original declares the whole provider vision-capable."
  (nle::make-catalog-model name context nil (if vision #("text" "image") #("text"))
                            #("text") (and reasoning +efforts+) reasoning nil t price))

(defun rate (pricing)
  "PRICING, a catalog entry's pricing block, as CATALOG-PRICE: US dollars per
million tokens (INPUT OUTPUT NIL NIL), or NIL when it names no usable pair."
  (when (hash-table-p pricing)
    (flet ((per-million (key &aux (value (nlk:json-value pricing :number key)))
             (and value (>= value 0) value)))
      (let ((input (per-million "input_per_1m"))
            (output (per-million "output_per_1m")))
        (and input output (list input output nil nil))))))

(defun entry-id (entry)
  "ENTRY's route id, or NIL."
  (or (nlk:json-value entry :text "model_id")
      (nlk:json-value entry :text "id")))

(defun marker-p (entry key wanted)
  "Whether ENTRY's KEY array names one of WANTED."
  (let ((markers (nlk:json-value entry :array key)))
    (and markers
         (some (lambda (marker) (and (stringp marker) (member marker wanted :test #'string-equal)))
               (coerce markers 'list)) t)))

(defun agentic-p (entry)
  "Whether ENTRY, one of the catalog's rows, is a route a turn can take: it
advertises tool calls, by capability marker or by supported parameter."
  (or (marker-p entry "capabilities" +agentic-capabilities+)
      (marker-p entry "supported_parameters" '("tools"))))

(defun reasoning-p (entry)
  "Whether ENTRY, one of the catalog's rows, reasons."
  (or (marker-p entry "capabilities" '("reasoning" "thinking"))
      (marker-p entry "supported_parameters" '("reasoning_effort" "reasoning"))))

(defun entry-context (entry)
  "ENTRY's context window, or NIL where it publishes none."
  (let ((window (or (nlk:json-value entry :integer "context_length")
                    (nlk:json-value entry :integer "context_window")
                    (nlk:json-value entry :integer "limit" "context"))))
    (and window (plusp window) window)))

(defun catalog-model (entry)
  "ENTRY, one catalog row, as the catalog keeps a model. A route is marked
vision-capable whatever its markers say, as the original declares the whole
provider: AnyRouter relays the image to whichever upstream serves it."
  (let ((id (entry-id entry)))
    (row-model id (or (nlk:json-value entry :text "name") id)
               :context (entry-context entry)
               :price (rate (nlk:json-value entry :object "pricing"))
               :reasoning (reasoning-p entry))))

(defvar *live* nil
  "The tool-capable routes /anyrouter last read from AnyRouter, as its catalog
entries; NIL before the command runs.")

(defun catalog-row (&optional prior)
  "AnyRouter as a models.dev provider: its name, the chat lane's package, this
section's base, the key variable, and the routes /anyrouter read over the
original's fallback rows, over PRIOR's (when models.dev published an
anyrouter row of its own)."
  (let ((models (make-hash-table :test 'equal)))
    (when (hash-table-p prior)
      (let ((published (nlk:json-value prior :object "models")))
        (when (hash-table-p published)
          (maphash (lambda (id model) (setf (gethash id models) model)) published))))
    (dolist (row +models+)
      (setf (gethash (car row) models)
            (row-model (car row) (car row) :context (cdr row) :reasoning t :vision t)))
    (dolist (entry *live*)
      (setf (gethash (entry-id entry) models) (catalog-model entry)))
    (nlk:json-object "name" "AnyRouter"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defvar *catalog* nil
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
AnyRouter's row in it; NIL before the first read.")

(defun catalog (next &aux (base (funcall next)))
  "MODELS-CATALOG-TABLE advice: the catalog with AnyRouter's row, made once per
catalog the core reads and again after /anyrouter read a new route list."
  (if (and *catalog* (eq (car *catalog*) base))
      (cdr *catalog*)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog and the route list /anyrouter read, so a restart
with another base builds both again."
  (setf *catalog* nil *live* nil))

;;; --- the key -------------------------------------------------------------------

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun credential (op next)
  "The :CREDENTIAL answer for anyrouter: ANYROUTER_API_KEY, else none. A key
/connect saved in auth.json answers before this point."
  ;; Never NEXT for anyrouter: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to AnyRouter.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

;;; --- the request ------------------------------------------------------------------

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an anyrouter round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defun reasons-p (model)
  "Whether the catalog says MODEL on AnyRouter reasons."
  (let ((capability (ignore-errors (nle::resolve-model-capability model +provider+))))
    (and capability (nle::model-capability-reasoning-p capability))))

(defun wiring-p ()
  "Whether this section asks for the app-attribution headers."
  (setting :attribution))

(defun attribution-headers ()
  "The headers an attributed request carries, naming Nodecode."
  `(("HTTP-Referer" . ,+app-url+)
    ("X-AnyRouter-Title" . ,+app-name+)
    ("X-AnyRouter-Source" . "cli-agent")
    ("X-AnyRouter-Categories" . "cli-agent")))

(defun word-p (choice)
  "CHOICE among the section's on/off/default words."
  (string-downcase (or choice "default")))

(defun provider-preferences ()
  "AnyRouter's routing-preference object from this section, or NIL when the
section names none."
  (let ((only (coerce (or (setting :only) '()) 'vector))
        (ignore (coerce (or (setting :ignore) '()) 'vector))
        (order (coerce (or (setting :order) '()) 'vector))
        (sort (setting :sort))
        (fallbacks (word-p (setting :allow-fallbacks)))
        (max-price (setting :max-price)))
    (when (or (plusp (length only)) (plusp (length ignore)) (plusp (length order))
              (not (equal sort "default")) (not (equal fallbacks "default")) max-price)
      (nlk:json-object :when (plusp (length only)) "only" only
                       :when (plusp (length ignore)) "ignore" ignore
                       :when (plusp (length order)) "order" order
                       :when (not (equal sort "default")) "sort" sort
                       :when (not (equal fallbacks "default"))
                       "allow_fallbacks" (equal fallbacks "on")
                       :when max-price "max_price" max-price))))

(defun shape-body (body context config)
  "BODY, the chat request the core built for the AnyRouter round CONFIG, as the
original sends it: AnyRouter's own reasoning object, the session id, the
routing preferences."
  (let* ((model (nle::effective-provider-config-model config))
         (selected (nle::effective-provider-config-reasoning-effort config))
         (asked (gethash "reasoning_effort" body)))
    ;; The chat lane's key is the OpenAI spelling; AnyRouter takes its own
    ;; object, and translates the effort for each upstream itself.
    (remhash "reasoning_effort" body)
    (cond (asked
           (setf (gethash "reasoning" body) (nlk:json-object "effort" asked)))
          ((and (stringp selected) (string-equal selected "off") (reasons-p model))
           (setf (gethash "reasoning" body) (nlk:json-object "enabled" nil))))
    (alexandria:when-let (session (nle::compiled-turn-context-session-id context))
      (setf (gethash "session_id" body) session))
    (alexandria:when-let (preferences (provider-preferences))
      (setf (gethash "provider" body) preferences))
    body))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: an anyrouter round is shaped as the original sends it."
  (when (and (ours-p config) (hash-table-p body))
    (shape-body body context config))
  body)

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: an anyrouter round asks for attribution only
when the section does."
  (if (and (ours-p config) (wiring-p))
      (apply next fold :headers (append headers (attribution-headers))
             (alexandria:remove-from-plist keys :headers))
      (apply next fold keys)))

;;; --- the catalog command --------------------------------------------------------------

(defun base ()
  "The base this section names, without a trailing slash."
  (string-right-trim "/" (setting :base-url)))

(defun catalog-key ()
  "The key the catalog fetch carries, or NIL: the one /connect saved, else
ANYROUTER_API_KEY. An authenticated listing also carries the caller's own
@preset routes, so it is worth sending."
  (handler-case
      (let ((key (nle:credential-key (nle::resolve-provider-credential +provider+))))
        (and key (not (equal key "public")) key))
    (error () nil)))

(defun fetch-models (&optional (timeout 15))
  "GET <base>/models: the tool-capable routes this key may use, how many rows
the catalog held, and NIL; or NIL, zero and the reason it could not be read."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get (format nil "~a/models" (base))
                    :headers (append '(("accept" . "application/json"))
                                     (when (wiring-p) (attribution-headers))
                                     (alexandria:when-let ((key (catalog-key)))
                                       `(("authorization" . ,(format nil "Bearer ~a" key)))))
                    :timeout timeout)
        (if (and (integerp status) (= status 200))
            (let* ((document (handler-case (nlk:decode-json body) (error () nil)))
                   (data (if (hash-table-p document)
                             (nlk:json-value document :array "data")
                             document))
                   (entries (and data (coerce data 'list)))
                   (routes (remove-if-not #'agentic-p entries)))
              (if (null entries)
                  (values nil 0 "an unexpected catalog shape")
                  (values routes (length entries) nil)))
            (values nil 0 (format nil "HTTP ~a" status))))
    (error (condition)
      (values nil 0 (princ-to-string condition)))))

(defun price-text (entry)
  "ENTRY's published rates as a short line, or NIL."
  (let ((price (rate (nlk:json-value entry :object "pricing"))))
    (when price
      (format nil "  $~a in / $~a out per 1M" (first price) (second price)))))

(defun run-command (args session-id)
  "The /anyrouter command: read AnyRouter's catalog and list the routes that
take tool calls, so /models offers what this key routes; ARGS narrows the list
by substring."
  (declare (ignore session-id))
  (multiple-value-bind (routes total why) (fetch-models)
    (cond
      ((null routes)
       (format nil "AnyRouter: the catalog at ~a did not answer (~a)." (base) why))
      (t
       (setf *catalog* nil)
       (setf *live* routes)
       (let* ((needle (string-downcase (or args "")))
              (shown (remove-if-not
                      (lambda (entry)
                        (search needle (string-downcase (entry-id entry))))
                      routes)))
         (if (null shown)
             (format nil "AnyRouter: ~a route~:p take tool calls, of ~a read; none matches ~s."
                     (length routes) total args)
             (with-output-to-string (out)
               (format out "AnyRouter: ~a of ~a catalog route~:p take tool calls, ~a listed; /models offers them now:~%"
                       (length routes) total (length shown))
               (dolist (entry (subseq shown 0 (min 25 (length shown))))
                 (format out "  ~a~@[~a~]~%" (entry-id entry) (price-text entry)))
               (when (> (length shown) 25)
                 (format out "  ... and ~a more~%" (- (length shown) 25))))))))))
