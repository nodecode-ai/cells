;;;; cell.lisp --- the cell: Kimi Code among the organism's providers, and its sign-in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Five hooks, each declining for every provider but kimi-code, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE    the catalog carries Kimi Code's row: the
;;;;                           Messages lane's package, this section's base,
;;;;                           and omp's bundled models over whatever
;;;;                           models.dev published, so /models lists them and
;;;;                           a turn resolves the anthropic lane and
;;;;                           https://api.kimi.com/coding/v1/messages
;;;;   :CREDENTIAL             the sign-in's token from auth.json, refreshed
;;;;                           first when it is about to expire, else a key
;;;;                           from KIMI_API_KEY or KIMI_CODE_API_KEY; a key
;;;;                           /connect saved answers before this point does.
;;;;                           It never falls through to the lane's family
;;;;                           default, which would send ANTHROPIC_API_KEY to
;;;;                           Kimi
;;;;   LIST-PROVIDER-MODELS    the live listing asked as omp's discovery asks
;;;;                           it: a bearer and Kimi's fixed client headers
;;;;   ANTHROPIC-REQUEST-BODY  thinking as omp asks Kimi for it: adaptive on
;;;;                           Kimi's own models, and every replayed thinking
;;;;                           block kept
;;;;   WALK-PROVIDER-STREAM    the credential as a bearer, and the Kimi CLI's
;;;;                           fingerprint headers
;;;;   /kimi-code              login (a device code), logout, status
;;;;
;;;; Config, a sibling top-level key:
;;;;   "kimi-code": {"base_url": "https://api.kimi.com/coding/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and Kimi Code is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-kimi-code)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a kimi-code round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Kimi Code's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Kimi Code's row, made once
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

(defun credential (op next)
  "The :CREDENTIAL answer for kimi-code: the sign-in's token, else a key from
the environment, else the keyless placeholder."
  (if (equal (getf op :provider) +provider+)
      (or (token-credential op)
          (alexandria:when-let (key (or (env-key) (nle::provider-env-key +provider+)))
            (nle:make-credential key :env))
          (nle:make-credential "public" :public))
      (funcall next op)))

(defun listing (next provider &rest keys &key key base &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: Kimi Code's listing, asked as omp asks it."
  (if (equal provider +provider+)
      (list-models (or key (ignore-errors (nle:credential-key (nle::resolve-provider-credential provider))))
                   (or base (setting :base-url)))
      (apply next provider keys)))

(defun body (next context)
  "ANTHROPIC-REQUEST-BODY advice: a kimi-code round thinks as omp asks Kimi to."
  (let* ((answer (multiple-value-list (funcall next context)))
         (body (first answer))
         (config (nle::compiled-turn-context-provider-config context)))
    (when (and (ours-p config) (hash-table-p body))
      (setf (second answer)
            (thinking-body body (nle::effective-provider-config-model config)
                           (let ((effort (nle::effective-provider-config-reasoning-effort config)))
                             (and (stringp effort) (not (string-equal effort "off")) effort))
                           (second answer))))
    (values-list answer)))

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a kimi-code round carries its credential as a
bearer and the Kimi client headers."
  (if (ours-p config)
      (apply next fold :headers (request-headers headers (nle::effective-provider-config-api-key config))
             (alexandria:remove-from-plist keys :headers))
      (apply next fold keys)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(defun start ()
  "Begin from a fresh catalog, and stop a sign-in in progress with the cell."
  (forget-catalog)
  (nle:on-stop #'forget-catalog)
  (nle:on-stop #'cancel-flow))

(nle:define-cell kimi-code
  (:section ("kimi-code")
    (:guide "sign in with /kimi-code login (a device code: open the address, enter the code), or set KIMI_API_KEY; base_url is where Kimi Code is served")
    ("base_url" :string :default +base+
     :doc "the Kimi Code API base the Messages lane appends /messages to"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::list-provider-models #'listing)
  (:hook 'nle::anthropic-request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk)
  (:command "kimi-code" 'run-command
            :description "Sign in to Kimi Code with a device code"
            :argument-hint "login | logout | status"
            :session nil
            :complete 'complete-command))
