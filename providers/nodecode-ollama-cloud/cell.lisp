;;;; cell.lisp --- the cell: Ollama Cloud on a lane of its own.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No lane of the organism speaks Ollama's /api/chat, so the cell registers
;;;; one, ollama-cloud, whose stream is STREAM-ROUND (wire.lisp). Three hooks,
;;;; each declining for every provider but ollama-cloud:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Ollama Cloud's row: this
;;;;                          cell's lane package, this section's base (the
;;;;                          bare host, not models.dev's /v1) and omp's
;;;;                          bundled models over whatever models.dev
;;;;                          published, so /connect offers it, /models lists
;;;;                          its models and a turn resolves this lane
;;;;   :CREDENTIAL            a key from OLLAMA_CLOUD_API_KEY, the variable omp
;;;;                          reads (and the core's own name for it), and no
;;;;                          other variable; a key /connect saved in auth.json
;;;;                          answers before this point does
;;;;   LIST-PROVIDER-MODELS   /models asks GET /api/tags, as omp's discovery
;;;;                          does, not /v1/models
;;;;
;;;; An operator who pins providers.ollama-cloud.sdk to openai-completions
;;;; (base_url https://ollama.com/v1) keeps the core's chat lane: the config's
;;;; pin outranks the catalog's package, and the key still comes from here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "ollama-cloud": {"base_url": "https://ollama.com"}
;;;; A vetoed section ("enabled": false) installs nothing: no lane, no hook,
;;;; and Ollama Cloud is then whatever models.dev alone makes of it.

(in-package #:nodecode-ollama-cloud)

;;; --- the catalog ---------------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Ollama Cloud's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Ollama Cloud's row, made once
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
  "The :CREDENTIAL answer for ollama-cloud: the key OLLAMA_CLOUD_API_KEY
holds, else none."
  ;; Never NEXT for ollama-cloud: the ladder behind this point falls back to
  ;; the chat family's default variable, and would send OPENAI_API_KEY to
  ;; Ollama.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (or (env-key) (nle::provider-env-key +provider+)))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

;;; --- the listing ----------------------------------------------------------------------

(defun listing (next provider &rest keys &key key base &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: Ollama Cloud lists GET /api/tags."
  (if (equal provider +provider+)
      (let ((key (or key (ignore-errors (nle:credential-key (nle::resolve-provider-credential provider))))))
        (if (or (null key) (equal key "public"))
            (values nil "no key: Ollama Cloud lists its models only to a key")
            (list-models (or base (nle::provider-base-url provider) +base+) key)))
      (apply next provider keys)))

;;; --- the lane -------------------------------------------------------------------------

(defun register-lane ()
  "The ollama-cloud lane: Ollama's /api/chat under this provider's name."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                            :stream-symbol 'stream-round
                            ;; the family only names the listing's auth header
                            ;; (a bearer) and the env ladder's default key,
                            ;; which the credential hook never falls to
                            :family :openai-completions
                            ;; a cut attempt's thinking rides back as
                            ;; reasoning_content, which the request drops:
                            ;; Ollama Cloud refuses `thinking' in history
                            :reasoning-carry :reasoning
                            :default-endpoint +base+
                            :path ""
                            :npm +npm+)))

(defun unregister-lane ()
  "Take the ollama-cloud lane back out."
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes* :key #'nle::provider-lane-name :test #'equal)))

(defun start ()
  "Register the lane; on the way down take it out and drop the merged catalog."
  (forget-catalog)
  (register-lane)
  (nle:on-stop #'unregister-lane)
  (nle:on-stop #'forget-catalog))

(nle:define-cell ollama-cloud
  (:section ("ollama-cloud")
    (:guide "make a key at https://ollama.com/settings/keys; save it with /connect or set OLLAMA_CLOUD_API_KEY; base_url is where Ollama's native API is served (the lane appends /api/chat)")
    ("base_url" :string :default +base+
     :doc "the Ollama base the lane appends /api/chat to (a trailing /api is dropped)"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::list-provider-models #'listing))
