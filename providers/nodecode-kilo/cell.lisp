;;;; cell.lisp --- the cell: Kilo Gateway among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but kilo, and one slash
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Kilo Gateway's row: the chat
;;;;                          lane's package, this section's base, and omp's
;;;;                          bundled models over whatever models.dev
;;;;                          published, so /connect offers it, /models lists
;;;;                          its models and a turn resolves its lane and
;;;;                          address the way it does any catalog provider's
;;;;   :CREDENTIAL            the token /kilo login saved, while omp's year
;;;;                          has not run out; else a key from KILO_API_KEY;
;;;;                          a key /connect saved in auth.json answers before
;;;;                          this point does
;;;;   REQUEST-BODY           a Qwen model is asked to think the Qwen way
;;;;   /kilo                  login, logout, status
;;;;
;;;; Config, a sibling top-level key:
;;;;   "kilo": {"base_url": "https://api.kilo.ai/api/gateway"}
;;;; A vetoed section ("enabled": false) installs nothing, and Kilo is then
;;;; whatever models.dev alone makes of it.

(in-package #:nodecode-kilo)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a kilo round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Kilo Gateway's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Kilo Gateway's row, made once
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

(defparameter +expired+ "kilo: the sign-in has expired; sign in again with /kilo login"
  "What a round says when the only credential is a token past its year.")

(defun credential (op next)
  "The :CREDENTIAL answer for kilo: the saved sign-in while it holds, else
the key KILO_API_KEY holds, else none."
  ;; Never NEXT for kilo: the ladder behind this point falls back to the chat
  ;; family's default variable, and would send OPENAI_API_KEY to Kilo. The
  ;; token has no refresh: a round that finds it expired and no key beside it
  ;; is refused with what to do, and the notice stands until a sign-in clears
  ;; it. A probe (no endpoint) reads the store as it is.
  (if (equal (getf op :provider) +provider+)
      (let ((entry (stored-entry (getf op :auth)))
            (key (env-key)))
        (flet ((signed-in () (nle:make-credential (nlk:json-value entry :text "access_token") :oauth)))
          (cond ((and entry (not (expired-p entry))) (signed-in))
                (key (nle:make-credential key :env))
                ((and entry (getf op :endpoint))
                 (nle:notice +expired+ :level :warning :key +key+)
                 (error 'nle:credential-error :detail +expired+))
                (entry (signed-in))
                (t (nle:make-credential "public" :public)))))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a kilo round asks a Qwen model to think in its dialect."
  (when (and (ours-p config) (hash-table-p body))
    (shape-thinking body
                    (nle::effective-provider-config-model config)
                    (nle::effective-provider-config-reasoning-effort config)))
  body)

(defun start ()
  "On stop, drop the merged catalog, end a waiting sign-in and clear what the
cell said."
  (setf *catalog* (cons nil nil))
  (nle:on-stop (lambda ()
                 (setf *catalog* (cons nil nil))
                 (cancel-login)
                 (nle:notice nil :key +key+))))

(nle:define-cell kilo
  (:section ("kilo")
    (:guide "sign in with /kilo login, then approve the code it shows on the page it names; or save a key with /connect or set KILO_API_KEY; base_url is where Kilo Gateway is served")
    ("base_url" :string :default +base+
     :doc "the Kilo Gateway base the chat lane appends /chat/completions to"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body)
  (:command "kilo" 'run-command
            :description "Kilo Gateway sign-in with a device code: login, logout, status"
            :argument-hint "login | logout | status"
            :session nil))
