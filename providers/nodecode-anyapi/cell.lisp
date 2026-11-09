;;;; cell.lisp --- the cell: AnyAPI.ai among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but anyapi:
;;;;
;;;;   MODELS-CATALOG-TABLE    the catalog carries AnyAPI's row: the chat
;;;;                            lane's package, this section's base, the key
;;;;                            variable and the fallback roster, so /connect
;;;;                            offers it and a turn resolves its lane and
;;;;                            address the way any catalog provider's does
;;;;   :CREDENTIAL              a key from ANYAPI_API_KEY and no other
;;;;                            variable; a key /connect saved in auth.json
;;;;                            answers before this point does
;;;;   LIST-PROVIDER-MODELS    the lane's listing is the routes the catalog
;;;;                            tags as tool-capable, the unfiltered listing
;;;;                            when that read fails or names none
;;;;
;;;; Config, a sibling top-level key:
;;;;   "anyapi": {"base_url": "https://api.anyapi.ai/v1", "tool_capable": true}
;;;; A vetoed section ("enabled": false) installs nothing, and AnyAPI is then
;;;; whatever models.dev alone makes of it (nothing).

(in-package #:nodecode-anyapi)

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
AnyAPI's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with AnyAPI's row, made once per
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

;;; --- the listing ---------------------------------------------------------------

(defvar *tool-capable-rows* nil
  "The tool-capable rows the last catalog read named: a catalog that answered
once is not asked again until a restart, which is the plugin's own cache.")

(defun served-p (provider)
  "Whether PROVIDER is the lane this cell serves."
  (and (stringp provider) (string-equal provider +provider+)))

(defun tool-capable-p (entry)
  "Whether ENTRY, one route of the catalog, advertises tool calling."
  (loop for tag across (or (nlk:json-value entry :array "tags") #())
        thereis (and (stringp tag) (member tag +tool-tags+ :test #'equal))))

(defun catalog-rows (body)
  "The rows BODY, an AnyAPI /v1/models answer, carries for the routes it
tags as tool-capable: (:id :display :context-window) each, in the catalog's
order and each id once; NIL for a body that is not the catalog or names none
such."
  ;; A row's display is the catalog's name when it says more than the id: the
  ;; picker's own ids are long, and a name that repeats one is noise.
  (let ((catalog (ignore-errors (nlk:decode-json body)))
        (seen (make-hash-table :test 'equal))
        (rows '()))
    (loop for entry across (or (nlk:json-value catalog :array "data") #())
          for id = (nlk:json-value entry :text "id")
          for name = (nlk:json-value entry :text "name")
          for window = (or (nlk:json-value entry :integer "context_length")
                           (nlk:json-value entry :integer "max_model_len"))
          when (and id (tool-capable-p entry) (not (gethash id seen)))
            do (setf (gethash id seen) t)
               (push (list :id id
                           :display (and name (string/= name id) name)
                           :context-window (and window (plusp window) window))
                     rows))
    (nreverse rows)))

(defun models-url ()
  "The catalog this cell reads: the section's base with /models on it."
  (uiop:strcat (string-right-trim "/" (setting :base-url)) "/models"))

(defun fetch-rows (&key api-key)
  "The rows AnyAPI's catalog names for its tool-capable routes, or NIL when
the read fails or names none."
  ;; HTTP-FETCH is the one bounded GET whose failures, a deadline's included,
  ;; are values: the listing worker never unwinds through here.
  (let ((headers (append '(("accept" . "application/json"))
                          (and api-key
                               (list (cons "authorization" (format nil "Bearer ~a" api-key)))))))
    (multiple-value-bind (body)
        (nle:http-fetch (models-url) :headers headers :timeout (setting :timeout-seconds))
      (and (stringp body) (catalog-rows body)))))

(defun listing-key (key)
  "The key the catalog read carries: KEY as the caller passed it, else
ANYAPI_API_KEY, else a key /connect saved -- never the placeholder a keyless
lane resolves."
  (or key
      (env-key)
      (let ((saved (ignore-errors
                     (nle:credential-key (nle::resolve-provider-credential +provider+)))))
        (and saved (string/= saved "public") saved))))

(defun tool-capable-rows (key)
  "The tool-capable rows the catalog names, kept once a read answers."
  (or *tool-capable-rows*
      (setf *tool-capable-rows* (fetch-rows :api-key key))))

(defun filtering-p (base)
  "Whether this listing is one this cell may interpret: the base the section
serves, and tool_capable on. A base the operator pointed somewhere else is a
relay's catalog, not AnyAPI's, so the core's own reading stands."
  (and (setting :tool-capable)
       (let ((base (string-right-trim "/" (or base ""))))
         (or (zerop (length base))
             (equal base (string-right-trim "/" (setting :base-url)))))))

(defun list-models (next provider &rest keys &key base &allow-other-keys)
  "(values ROWS ERROR): the served lane's listing is the routes the catalog
tags as tool-capable; any other lane's, and a catalog that answered none, is
NEXT's own."
  (if (and (served-p provider) (filtering-p base))
      (alexandria:if-let (rows (tool-capable-rows (listing-key (getf keys :key))))
        (values rows nil)
        (apply next provider keys))
      (apply next provider keys)))

(defun forget-catalog ()
  "Drop the merged catalog and the rows read, so a restart with another base
or another key asks again."
  (setf *catalog* (cons nil nil) *tool-capable-rows* nil))

(nle:define-cell anyapi
  (:section ("anyapi")
    (:guide "make a key at https://anyapi.ai; save it with /connect or set ANYAPI_API_KEY; base_url is where AnyAPI is served; tool_capable lists only the routes it tags as taking tool calls")
    ("base_url" :string :default +base+
     :doc "the AnyAPI API base the chat lane appends /chat/completions to")
    ("tool_capable" :boolean :default t
     :doc "list only the routes AnyAPI tags as tool-capable, the ones an agent turn can run on")
    ("timeout_seconds" :integer :default 8 :min 1
     :doc "one catalog read's ceiling, in seconds"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::list-provider-models #'list-models))
