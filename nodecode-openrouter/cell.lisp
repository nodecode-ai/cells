;;;; cell.lisp --- the cell: OpenRouter's sign-in and request quirks among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four hooks, each declining for every provider but openrouter, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog's openrouter row is models.dev's,
;;;;                          with this section's base and omp's bundled models
;;;;                          over models.dev's, so /models lists both
;;;;   :CREDENTIAL            OPENROUTER_API_KEY, else no key: never the chat
;;;;                          family's default variable; a key the sign-in or
;;;;                          /connect saved answers before this point does
;;;;   REQUEST-BODY           the routed wire id, OpenRouter's reasoning
;;;;                          object, no output cap the operator did not set,
;;;;                          the routing preferences
;;;;   WALK-PROVIDER-STREAM   every request carries the attribution headers
;;;;
;;;;   /openrouter login | code TEXT | logout | status        (signin.lisp)
;;;;
;;;; The key the sign-in obtains is saved where /connect saves one
;;;; (api_keys.openrouter), so the core's own store tier answers it.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "openrouter": {"base_url": "https://openrouter.ai/api/v1",
;;;;                  "variant": "nitro", "only": ["anthropic"], "order": []}
;;;; A vetoed section ("enabled": false) installs nothing, and OpenRouter is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-openrouter)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an openrouter round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
OpenRouter's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with OpenRouter's row, made once
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

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: an openrouter round is shaped as omp sends it."
  (when (and (ours-p config) (hash-table-p body))
    (shape-body body config))
  body)

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: an openrouter round names the app it comes from."
  (if (ours-p config)
      (apply next fold :headers (append headers (attribution-headers))
             (alexandria:remove-from-plist keys :headers))
      (apply next fold keys)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(defun start ()
  "Start clean, and leave no sign-in listening behind a stop."
  (forget-catalog)
  (nle:on-stop #'forget-catalog)
  (nle:on-stop #'cancel-flow))

(nle:define-cell openrouter
  (:section ("openrouter")
    (:guide "sign in with /openrouter login, or make a key at https://openrouter.ai/settings/keys and save it with /connect or OPENROUTER_API_KEY; variant routes every model (nitro, floor, online, exacto); only and order are provider slugs")
    ("base_url" :string :default +base+
     :doc "the OpenRouter API base the chat lane appends /chat/completions to")
    ("variant" :choice :options +variants+ :default "default"
     :doc "the routing variant appended to a model id that names none: nitro (throughput), floor (price), online (web search), exacto (curated providers)")
    ("only" :list :doc "the upstream providers OpenRouter may route to, and no other")
    ("order" :list :doc "the upstream providers OpenRouter tries first, in order"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk)
  (:command "openrouter" #'run-command
            :description "OpenRouter: sign in from the browser for a key"
            :argument-hint "login | code <address, code or key> | logout | status"))
