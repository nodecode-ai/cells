;;;; cell.lisp --- the cell: EmpirioLabs AI among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four hooks, each declining for every provider but empiriolabs:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries EmpirioLabs AI's row: the
;;;;                          chat lane's package, this section's base, the
;;;;                          catalog's own models (context window, image
;;;;                          input, reasoning, price) over whatever
;;;;                          models.dev published, and the two ids the
;;;;                          original offers when its catalog cannot be read
;;;;   LIST-PROVIDER-MODELS   the listing is the service's catalog: chat
;;;;                          models that take tool calls, as the original
;;;;                          filtered it, and the models' details kept for
;;;;                          the row above; asked with a key, which only
;;;;                          /connect's check does, it asks with that key
;;;;   :CREDENTIAL             a key from EMPIRIOLABS_API_KEY, and no other
;;;;                          variable; a key /connect saved in auth.json
;;;;                          answers before this point does
;;;;   REQUEST-BODY            a round asks for reasoning_effort only when
;;;;                          the model declares one, at the nearest level
;;;;                          not dearer than the one chosen
;;;;
;;;; Config, a sibling top-level key:
;;;;   "empiriolabs": {"base_url": "https://api.empiriolabs.ai/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and EmpirioLabs AI
;;;; is then whatever models.dev alone makes of it.

(in-package #:nodecode-empiriolabs)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an empiriolabs round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *records* '()
  "The catalog's chat models, from the last listing this cell made: plists
(:id :display :context :vision :reasoning :efforts :aliases :input :output).")

(defvar *generation* 0
  "Bumped whenever *RECORDS* changes, so the catalog row is rebuilt from the
details the listing learned.")

(defvar *catalog* (list nil nil nil)
  "(BASE GENERATION . MERGED): the last catalog the core answered, the
generation it was built from, and that catalog with this row in it.")

(defun remember (entries)
  "Keep ENTRIES as this cell's catalog, for the row and for rounds."
  (setf *records* entries)
  (incf *generation*))

(defun forget-catalog ()
  "Drop the kept catalog and the merged one, so a restart with another base
builds a new row."
  (setf *catalog* (list nil nil nil))
  (setf *records* '())
  (incf *generation*))

(defun record-for (model)
  "The kept catalog entry MODEL names, by id or by one of its aliases, or NIL
(the original's _find_row)."
  (or (find model *records* :key (lambda (entry) (getf entry :id)) :test #'equal)
      (find model *records* :key (lambda (entry) (getf entry :aliases))
            :test (lambda (wanted aliases) (member wanted aliases :test #'equal)))))

(defun find-record (model)
  "RECORD-FOR, with the empty model answering NIL."
  (and (stringp model) (plusp (length model)) (record-for model)))

(defun catalog-row (&optional prior)
  "EmpirioLabs AI as a models.dev provider: the chat lane's package, this
section's base, the key variable, the catalog's own models over PRIOR's (the
row models.dev itself published, when it did), and the two fallback ids."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for id in +fallback-ids+
          do (setf (gethash id models) (fallback-model id)))
    (loop for entry in *records*
          do (setf (gethash (getf entry :id) models) (catalog-model entry)))
    (nlk:json-object "name" "EmpirioLabs AI"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with EmpirioLabs AI's row, made
once per catalog the core reads and per catalog this cell has learned."
  (if (and (car memo) (eq (car memo) base) (eql (second memo) *generation*))
      (cddr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (list base *generation* merged))
        merged)))

;;; --- the listing ----------------------------------------------------------------

(defun catalog-headers (key)
  "What a catalog GET carries: the JSON accept, this cell's user agent, and
KEY as a bearer when one is known."
  (append '(("Accept" . "application/json"))
          `(("User-Agent" . ,+user-agent+))
          (when (and (stringp key) (present key) (not (equal key "public")))
            `(("Authorization" . ,(format nil "Bearer ~a" key))))))

(defun active-key ()
  "The key a listing asks with: the one /connect saved, else
EMPIRIOLABS_API_KEY, else NIL."
  (ignore-errors (nle::credential-key (nle::resolve-provider-credential +provider+ :probe t))))

(defun fetch-records (key base)
  "(values RECORDS ERROR): GET BASE/models with KEY and read the chat models
out of it, or say why not in words (the original's _fetch_rows, never a
signal)."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get (uiop:strcat (string-right-trim "/" base) +models-path+)
                    :headers (catalog-headers key) :timeout +timeout+)
        (cond ((not (and (integerp status) (<= 200 status 299)))
               (values nil (format nil "HTTP ~a" status)))
              ((not (stringp body)) (values nil "the catalog answered with no body"))
              (t (let ((payload (ignore-errors (nlk:decode-json body))))
                   (if payload
                       (parse-catalog payload)
                       (values nil "unparseable catalog"))))))
    (error (condition) (values nil (princ-to-string condition)))))

(defun listing (next provider &rest keys &key key base &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: EmpirioLabs AI's listing is its catalog,
narrowed to the chat models that take tool calls. Asked with KEY, which only
/connect's key check does, it asks with that key, so a refused key reads
refused."
  (if (equal provider +provider+)
      (multiple-value-bind (entries error)
          (fetch-records (or key (active-key)) (or base (nle::provider-base-url provider) (base-url)))
        (if error
            (values nil error)
            (progn (remember entries)
                   (values (mapcar #'listing-row entries) nil))))
      (apply next provider keys)))

;;; --- the key and the round --------------------------------------------------------

(defun credential (op next)
  "The :CREDENTIAL answer for empiriolabs: the key EMPIRIOLABS_API_KEY
holds, else none."
  ;; Never NEXT for this provider: the ladder behind this point falls back to
  ;; the chat family's default variable, and would send OPENAI_API_KEY to
  ;; EmpirioLabs AI.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a round asks for the effort the model declares, at
the level chosen or the nearest one below it, and for nothing when the model
declares none."
  ;; The lane put the chosen rung in the body already; what the catalog adds
  ;; is whether this model takes it, and at which of its own levels.
  (when (and (ours-p config) (hash-table-p body))
    (alexandria:when-let ((requested (nlk:json-value body :string "reasoning_effort")))
      (let* ((entry (find-record (nle::effective-provider-config-model config)))
             (chosen (and entry (clamp-effort requested (getf entry :efforts)))))
        (if chosen
            (setf (gethash "reasoning_effort" body) chosen)
            (remhash "reasoning_effort" body)))))
  body)

(nle:define-cell empiriolabs
  (:section ("empiriolabs")
    (:guide "make a key at https://platform.empiriolabs.ai/dashboard/api-keys; save it with /connect or set EMPIRIOLABS_API_KEY; base_url is where the service is served")
    ("base_url" :string :default +base+
     :doc "the EmpirioLabs AI API base the chat lane appends /chat/completions to"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::list-provider-models #'listing)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body))
