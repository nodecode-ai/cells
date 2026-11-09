;;;; provider.lisp --- what Chutes is: its address, its key, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from TheStreamCode/hermes-chutes-provider (see NOTICE):
;;;; hermes_chutes_provider/__init__.py and plugin.yaml.
;;;;
;;;; Chutes speaks the OpenAI chat wire at https://llm.chutes.ai/v1 with a
;;;; bearer key made at chutes.ai, and serves open models on decentralised
;;;; GPUs. Nothing sets it apart from a plain OpenAI-compatible endpoint
;;;; except its catalog, and that is the whole of what the original adds:
;;;;
;;;;   - the model list is Chutes' own catalog, GET <base>/models, cut down
;;;;     to the entries whose supported_features name `tools' (case-blind, so
;;;;     `Tools' counts): a chute that takes no tool call is no use to an
;;;;     agent, so /models offers none of them. /chutes reads it on demand and
;;;;     puts it in /models, and the original's two meta-models stand in until
;;;;     it is read
;;;;   - the base is the section's base_url, else CHUTES_BASE_URL, else
;;;;     https://llm.chutes.ai/v1, which is the original's own pair of
;;;;     variables

(in-package #:nodecode-chutes)

(defparameter +base+ "https://llm.chutes.ai/v1"
  "Where Chutes is served: the base the chat lane appends /chat/completions to.")

(defparameter +env+ '("CHUTES_API_KEY")
  "The environment variables a Chutes key is read from, in order.")

(defparameter +base-env+ "CHUTES_BASE_URL"
  "The variable the base is read from when the section names none.")

(defparameter +key-page+ "https://chutes.ai/app/api"
  "Where a key is made: the Chutes API page.")

(defparameter +models+
  '(("default:latency" . "the chute that answers soonest")
    ("default:throughput" . "the chute that answers fastest"))
  "The meta-models the original's profile lists as its fallback, shown until
/chutes reads the live catalog: (ID . WHAT IT ROUTES AMONG). Chutes picks the
chute itself from the request.")

(defparameter +agentic-feature+ "tools"
  "The supported_features marker that means a chute takes tool calls; Hermes'
own, since a chute that takes none is no use to an agent.")

;;; --- the models ----------------------------------------------------------------

(defun present (value)
  "VALUE trimmed when it is a non-empty string, else NIL."
  (and (stringp value)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
         (and (plusp (length trimmed)) trimmed))))

(defun base ()
  "The base a round and the catalog read dial: the section's base_url, else
CHUTES_BASE_URL, else the original's own address."
  (string-right-trim "/"
                     (or (present (setting :base-url))
                         (present (nle::credential-env +base-env+))
                         +base+)))

(defun features (entry)
  "ENTRY's supported_features as a list of downcased names, or NIL where it
publishes none."
  (let ((named (nlk:json-value entry :array "supported_features")))
    (and named
         (loop for feature across named
               when (stringp feature) collect (string-downcase feature)))))

(defun feature-p (entry name)
  "Whether ENTRY's supported_features name NAME."
  (and (member name (features entry) :test #'equal) t))

(defun agentic-p (entry)
  "Whether ENTRY, one of the catalog's rows, is a model a turn can take: it
advertises tool calls in supported_features."
  (feature-p entry +agentic-feature+))

(defun entry-context (entry)
  "ENTRY's context window, or NIL where it publishes none."
  (let ((window (or (nlk:json-value entry :integer "context_length")
                    (nlk:json-value entry :integer "max_model_len")
                    (nlk:json-value entry :integer "limit" "context"))))
    (and window (plusp window) window)))

(defun catalog-model (entry)
  "ENTRY, one catalog row, as the catalog keeps a model. Vision and reasoning
are set where the row's own features name them. No price: the original
publishes none, so a round is costed by models.dev's row when it has one."
  (nle::make-catalog-model (nlk:json-value entry :text "id")
                            (entry-context entry)
                            nil
                            (if (feature-p entry "vision") #("text" "image") #("text"))
                            #("text")
                            nil
                            (feature-p entry "reasoning")
                            nil
                            t
                            nil))

(defun fallback-model (id)
  "The meta-model ID as the catalog keeps a model: no window, no price."
  (nle::make-catalog-model id nil nil #("text") #("text") nil nil nil t nil))

(defvar *live* nil
  "The tool-capable entries /chutes last read from Chutes, as its catalog
entries; NIL before the command runs.")

(defun catalog-row (&optional prior)
  "Chutes as a models.dev provider: its name, the chat lane's package, this
section's base, the key variable, and the tool-capable entries /chutes read
over the original's meta-models, over PRIOR's (when models.dev published a
chutes row of its own)."
  (let ((models (make-hash-table :test 'equal)))
    (when (hash-table-p prior)
      (let ((published (nlk:json-value prior :object "models")))
        (when (hash-table-p published)
          (maphash (lambda (id model) (setf (gethash id models) model)) published))))
    (dolist (row +models+)
      (setf (gethash (car row) models) (fallback-model (car row))))
    (dolist (entry *live*)
      (alexandria:when-let ((id (nlk:json-value entry :text "id")))
        (setf (gethash id models) (catalog-model entry))))
    (nlk:json-object "name" "Chutes"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (base)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defvar *catalog* nil
  "(CORE ADDRESS . MERGED): the last catalog the core answered, the base this
section named when it was made, and that catalog with Chutes' row in it; NIL
before the first read.")

(defun catalog (next &aux (base (funcall next)) (address (base)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Chutes' row, made once per
catalog the core reads at one base, and again after /chutes read a new list."
  (if (and memo (eq (first memo) base) (equal (second memo) address))
      (third memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (list base address merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog and the list /chutes read, so a restart with
another base builds both again."
  (setf *catalog* nil *live* nil))

;;; --- the key -------------------------------------------------------------------

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun credential (op next)
  "The :CREDENTIAL answer for chutes: CHUTES_API_KEY, else none. A key
/connect saved in auth.json answers before this point."
  ;; Never NEXT for chutes: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to Chutes.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

;;; --- the catalog command --------------------------------------------------------------

(defun catalog-key ()
  "The key the catalog read carries, or NIL: the one /connect saved, else
CHUTES_API_KEY. Chutes answers an unauthenticated listing too, so a failure to
find one is not a failure to read."
  (handler-case
      (let ((key (nle:credential-key (nle::resolve-provider-credential +provider+))))
        (and key (not (equal key "public")) key))
    (error () nil)))

(defun fetch-models (&optional (timeout 15))
  "GET <base>/models: the entries that take tool calls, how many rows the
catalog held, and NIL; or NIL, zero and the reason it could not be read."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get (format nil "~a/models" (base))
                    :headers (append '(("accept" . "application/json"))
                                     (alexandria:when-let ((key (catalog-key)))
                                       `(("authorization" . ,(format nil "Bearer ~a" key)))))
                    :timeout timeout)
        (if (and (integerp status) (= status 200))
            (let* ((document (handler-case (nlk:decode-json body) (error () nil)))
                   (data (if (hash-table-p document)
                             (nlk:json-value document :array "data")
                             document))
                   (entries (and data (coerce data 'list))))
              (if (null data)
                  (values nil 0 "an unexpected catalog shape")
                  (values (remove-if-not #'agentic-p entries) (length entries) nil)))
            (values nil 0 (format nil "HTTP ~a" status))))
    (error (condition)
      (values nil 0 (princ-to-string condition)))))

(defun window-text (entry)
  "ENTRY's context window as a short line, or NIL."
  (alexandria:when-let ((window (entry-context entry)))
    (format nil "  ~a tokens" window)))

(defun run-command (args session-id)
  "The /chutes command: read Chutes' catalog and list the models that take
tool calls, so /models offers what this key may use; ARGS narrows the list by
substring."
  (declare (ignore session-id))
  (multiple-value-bind (entries total why) (fetch-models)
    (setf *catalog* nil
          *live* entries)
    (cond
      (why
       (format nil "Chutes: the catalog at ~a did not answer (~a)." (base) why))
      ((null entries)
       (format nil "Chutes: the catalog at ~a names ~a model~:p, none of them ~
                    take tool calls." (base) total))
      (t
       (let* ((needle (string-downcase (or args "")))
              (shown (remove-if-not
                      (lambda (entry)
                        (search needle (string-downcase (nlk:json-value entry :text "id"))))
                      entries)))
         (if (null shown)
             (format nil "Chutes: ~a model~:p take tool calls of ~a read; none matches ~s."
                     (length entries) total args)
             (with-output-to-string (out)
               (format out "Chutes: ~a of ~a catalog model~:p take tool calls, ~a listed; /models offers them now:~%"
                       (length entries) total (length shown))
               (dolist (entry (subseq shown 0 (min 25 (length shown))))
                 (format out "  ~a~@[~a~]~%"
                         (nlk:json-value entry :text "id") (window-text entry)))
               (when (> (length shown) 25)
                 (format out "  ... and ~a more~%" (- (length shown) 25))))))))))
