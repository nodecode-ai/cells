;;;; cell.lisp --- the cell: Command Code on a lane of its own.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No lane of the organism speaks Command Code's /alpha/generate, so the cell
;;;; registers one, commandcode, whose stream is STREAM-ROUND (wire.lisp).
;;;; Three hooks, each declining for every provider but commandcode:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Command Code's row: this
;;;;                          cell's lane package, this section's base and the
;;;;                          CLI's model snapshot, so /connect offers it,
;;;;                          /models lists its models and a turn resolves this
;;;;                          lane
;;;;   :CREDENTIAL            the key COMMANDCODE_CLI_TOKEN holds, else the one
;;;;                          the Command Code CLI already stored (that is the
;;;;                          point of the original), else none; a key /connect
;;;;                          saved in auth.json answers before this point does
;;;;   LIST-PROVIDER-MODELS   /models asks GET /provider/v1/models?view=client,
;;;;                          the account's own catalog
;;;;
;;;; Config, a sibling top-level key:
;;;;   "commandcode": {"base_url": "https://api.commandcode.ai"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-commandcode)

;;; --- the catalog ---------------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Command Code's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Command Code's row, made once
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

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

;;; --- the credential ----------------------------------------------------------------

(defun credential (op next)
  "The :CREDENTIAL answer for commandcode: the environment's key, else the
CLI's stored one, else none."
  ;; Never NEXT for commandcode: the ladder behind this point falls back to
  ;; the chat family's default variable, and would send OPENAI_API_KEY here.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (or (configured-key) (nle::provider-env-key +provider+)))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

;;; --- the listing ----------------------------------------------------------------------

(defun listing-row (item)
  "One catalog ITEM as the listing keeps a model: its id, its display name and
its window."
  (let* ((id (nlk:json-value item :text "id"))
         (name (nlk:json-value item :text "name"))
         (bundled (and id (model-row id))))
    (when id
      (list :id id
            :display (or (and name (string/= name id) name)
                         (nlk:json-value bundled :text "name")
                         id)
            :context-window (or (nlk:json-value item :integer "contextWindow")
                                (nlk:json-value item :integer "contextLength")
                                (nlk:json-value bundled :integer "context"))))))

(defun list-models (base key)
  "(values ROWS ERROR): the models GET <BASE>/provider/v1/models?view=client
lists for KEY, hidden and locked entries dropped (transport's fetch_models
reads the same listing)."
  (let ((url (format nil "~a/provider/v1/models?view=client" (normalized-base base))))
    (multiple-value-bind (body status)
        (nle::http-fetch url :headers `(("accept" . "application/json")
                                        ("authorization" . ,(format nil "Bearer ~a" key)))
                              :timeout 10)
      (if (null body)
          (values nil status)
          (let ((parsed (ignore-errors (nlk:decode-json body))))
            (if (not (hash-table-p parsed))
                (values nil "unparseable listing")
                (let ((rows '()))
                  (loop for item across (or (nlk:json-array parsed "data")
                                            (nlk:json-array parsed "models"))
                        when (and (hash-table-p item)
                                  (not (eq t (nlk:json-value item :boolean "hidden"))))
                          do (let ((row (listing-row item)))
                               (when row (push row rows))))
                  (values (sort rows #'string< :key (lambda (row) (getf row :id))) nil))))))))

(defun listing (next provider &rest keys &key key base &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: Command Code lists its account catalog."
  (if (equal provider +provider+)
      (let ((key (or key (configured-key)
                     (ignore-errors (nle:credential-key (nle::resolve-provider-credential provider))))))
        (if (or (null key) (equal key "public") (zerop (length key)))
            (values nil (format nil "no key: Command Code lists its models to a signed-in account (~a)"
                                +key-page+))
            (list-models (or base (nle::provider-base-url provider) (setting :base-url)) key)))
      (apply next provider keys)))

;;; --- the lane -------------------------------------------------------------------------

(defun register-lane ()
  "The commandcode lane: /alpha/generate under this provider's name."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                            :stream-symbol 'stream-round
                            ;; the family only names the listing's auth header
                            ;; (a bearer) and the env ladder's default key,
                            ;; which the credential hook never falls to
                            :family :openai-completions
                            :reasoning-carry :reasoning
                            :default-endpoint +base+
                            :path ""
                            :npm +npm+)))

(defun unregister-lane ()
  "Take the commandcode lane back out."
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes* :key #'nle::provider-lane-name :test #'equal)))

(defun start ()
  "Register the lane; on the way down take it out and drop the merged catalog."
  (forget-catalog)
  (register-lane)
  (nle:on-stop #'unregister-lane)
  (nle:on-stop #'forget-catalog))

(nle:define-cell commandcode
  (:section ("commandcode")
    (:guide "sign in with the Command Code CLI, or save a key with /connect; base_url is where Command Code is served")
    ("base_url" :string :default +base+
     :doc "the Command Code base the lane appends /alpha/generate to")
    ("cli_auth_file" :string :default +default-cli-auth-file+
     :doc "the file the Command Code CLI keeps its sign-in in (read only)")
    ("read_cli_login" :boolean :default t
     :doc "import the CLI's stored key when nothing else answered"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::list-provider-models #'listing))
