;;;; provider.lisp --- what BeLive AI Hub is: its address, its key, its catalog, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of BeLive Technology's Hermes provider plugin (see NOTICE). The
;;;; plugin's one idea, kept here: the hub is a gateway in front of many
;;;; upstream models, so the /model picker should list only the routes a turn
;;;; can actually run on. The plugin reads the hub's model list with the
;;;; workspace key and keeps the entries whose catalog says text out, tools in,
;;;; and a ready Chat Completions route that takes tool calls; the model id
;;;; keeps the hub's exact author/slug form.
;;;;
;;;; Two tiers, as in the original: the live catalog when it answers, and the
;;;; plugin's own fallback_models when it does not. An empty answer is an
;;;; answer too, and empties the picker rather than falling back.

(in-package #:nodecode-belive-aihub)

(defparameter +base+ "https://tokenhub-api.belive.sg/api/v1"
  "Where BeLive AI Hub is served: the base the chat lane appends /chat/completions to.")

(defparameter +staging+ "https://api-stg-tokenhub.belive.sg/api/v1"
  "The hub's staging base. Discovery accepts it too, as the original does, so a
base_url pointed at staging lists staging's models.")

(defparameter +api-key-env+ "BELIVE_TOKENHUB_API_KEY"
  "The environment variable a BeLive workspace key is read from; /connect's saved key answers first.")

(defparameter +timeout+ 8
  "Seconds one catalog fetch may take, connect included: the original's ceiling.")

(defparameter +max-catalog-bytes+ (* 4 1024 1024)
  "The largest catalog the original reads; a bigger answer counts as no answer.")

(defparameter +fallback-ids+
  '("qwen/qwen3.8-flash" "z-ai/glm-5.3-flash" "deepseek/deepseek-v4.1-flash-1017")
  "The picker when the live catalog is unreachable: the original plugin's own
fallback_models, in its order, the first being the model it names as its vision
and auxiliary default.")

(defparameter +vision-ids+ '("qwen/qwen3.8-flash")
  "The curated models the original designates for images: the picker offers
these for one, and every other curated row takes text.")

;;; --- the catalog -------------------------------------------------------------

(defun official-base-p (base)
  "Whether BASE is one of the hub's own HTTPS bases, a trailing slash aside."
  (and (stringp base) (member (string-right-trim "/" base) (list +base+ +staging+) :test #'equal)))

(defun catalog-url (base)
  "The model list of the gateway at BASE."
  (concatenate 'string (string-right-trim "/" base) "/models"))

(defun marker-p (value marker)
  "Whether VALUE, a decoded JSON array of strings, carries MARKER."
  (and (vectorp value)
       (some (lambda (item) (and (stringp item) (string-equal item marker))) value)))

(defun spaced-p (string)
  "Whether STRING carries whitespace, which no model id does."
  (some (lambda (character) (member character '(#\Space #\Tab #\Newline #\Return #\Page)))
        string))

(defun chat-route-p (endpoint)
  "Whether ENDPOINT is a ready Chat Completions route that takes tool calls."
  (and (hash-table-p endpoint)
       (equal "chat_completions" (nlk:json-value endpoint :string "route"))
       (equal "ready" (nlk:json-value endpoint :string "status"))
       (marker-p (nlk:json-value endpoint :array "supported_parameters") "tools")))

(defun eligible-p (entry)
  "Whether ENTRY, one catalog row, is a route a turn can run on: text out,
tool calls in, and a ready Chat Completions endpoint that takes them."
  (and (hash-table-p entry)
       (let ((id (nlk:json-value entry :string "id"))
             (endpoints (nlk:json-value entry :array "endpoints")))
         (and (stringp id) (find #\/ id) (not (spaced-p id))
              (marker-p (nlk:json-value entry :array "architecture" "output_modalities") "text")
              (marker-p (nlk:json-value entry :array "supported_parameters") "tools")
              (vectorp endpoints)
              (some #'chat-route-p endpoints)))))

(defun catalog-rows (payload)
  "=> (VALUES ROWS VALID-P): the eligible rows of catalog PAYLOAD, in catalog
order, each a plist; and whether PAYLOAD was a catalog at all (a `data' array,
an empty one included)."
  (let ((entries (and (hash-table-p payload) (nlk:json-value payload :array "data"))))
    (if (not (vectorp entries))
        (values nil nil)
        (let ((rows '()) (seen (make-hash-table :test #'equal)))
          (loop for entry across entries
                do (let ((id (and (eligible-p entry) (nlk:json-value entry :string "id"))))
                     (when (and id (not (gethash id seen)))
                       (setf (gethash id seen) t)
                       (push (list :id id
                                   :name (or (nlk:json-value entry :string "name") id)
                                   :context (nlk:json-value entry :integer "context_length")
                                   :vision (marker-p (nlk:json-value entry :array
                                                                    "architecture" "input_modalities")
                                                      "image"))
                             rows))))
          (values (nreverse rows) t)))))

(defun fetch-rows (base &aux (answer nil) (status nil))
  "One fetch of BASE's model list => (values ROWS LANDED-P). LANDED-P when the
gateway answered a catalog, an empty one included; nothing at all when it did
not — a refused status, a transport error, an oversized body, or a body that is
no catalog."
  (handler-case
      (multiple-value-setq (answer status)
        (nlk:http :get (catalog-url base)
                  :headers (let ((key (nle::credential-env +api-key-env+)))
                             (append (when key
                                       (list (cons "authorization" (format nil "Bearer ~a" key))))
                                     (list (cons "accept" "application/json"))))
                  :timeout +timeout+
                  :connect-timeout (min 5 +timeout+)))
    ((or error sb-sys:deadline-timeout) () (return-from fetch-rows nil)))
  (when (and (integerp status) (< status 300)
             (stringp answer) (<= (length answer) +max-catalog-bytes+))
    (multiple-value-bind (rows valid)
        (handler-case (catalog-rows (nlk:decode-json answer))
          (error () (values nil nil)))
      (when valid (values rows t)))))

(defvar *live-rows* nil
  "The live catalog once it has landed, as (T . ROWS): an empty catalog lands
too, as (T), and empties the picker the way the original keeps a successful
empty answer empty. NIL while the catalog is unfetched or unreachable, which is
what lets the curated list answer.")

(defvar *catalog* (cons nil nil)
  "(CATALOG . MERGED): the last catalog the core answered, and that catalog with
BeLive AI Hub's row in it.")

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base or other models builds a new one."
  (setf *catalog* (cons nil nil)))

(defun refresh (base)
  "One step of the catalog worker: fetch BASE's model list when BASE is one of
the hub's own, keep its rows, and stop the worker once they have landed.
Answers NIL to be woken again after the worker's wait, so an unreachable
catalog is retried rather than given up on."
  (when (official-base-p base)
    (multiple-value-bind (rows landed) (fetch-rows base)
      (when landed
        (setf *live-rows* (cons t rows))
        (forget-catalog)
        (nle:notice (if rows
                        (format nil "belive-aihub: ~d chat models from the live catalog" (length rows))
                        "belive-aihub: the live catalog lists no chat model"))
        :stop))))

;;; --- the provider row --------------------------------------------------------

(defun catalog-model (row)
  "ROW, a plist from the live catalog or the curated list, as a CATALOG-MODEL."
  (destructuring-bind (&key id name context vision) row
    (declare (ignore id))
    ;; every row here is tool-capable by construction, and a turn answers in
    ;; text: an image goes in only where the row says the model reads one
    (nle::make-catalog-model name context nil
                              (if vision #("text" "image") #("text"))
                              #("text")
                              nil nil nil t nil)))

(defun fallback-rows ()
  "The curated rows: the original's fallback_models, its vision default among them."
  (loop for id in +fallback-ids+
        collect (list :id id :name id :context nil
                      :vision (and (member id +vision-ids+ :test #'equal) t))))

(defun catalog-row (&optional prior)
  "BeLive AI Hub as a models.dev provider: the chat lane's package, this
section's base, the key variable, and its models over PRIOR's (the row
models.dev itself published, when it did)."
  (let ((models (make-hash-table :test #'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (dolist (row (if *live-rows* (cdr *live-rows*) (fallback-rows)))
      (setf (gethash (getf row :id) models) (catalog-model row)))
    (nlk:json-object "name" "BeLive AI Hub"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (vector +api-key-env+)
                     "models" models)))

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with BeLive AI Hub's row, made once
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
