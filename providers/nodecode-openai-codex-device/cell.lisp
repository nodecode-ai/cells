;;;; cell.lisp --- the cell: a ChatGPT subscription signed in with a device code.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A lane and five hooks, each declining for every provider and lane but
;;;; this one's, and one slash command:
;;;;
;;;;   the openai-codex-device lane   the Responses fold under this cell's name
;;;;                                  and a family of its own (provider.lisp)
;;;;   MODELS-CATALOG-TABLE     the catalog carries openai-codex: this lane,
;;;;                            the Codex backend's base, omp's bundled models
;;;;   :CREDENTIAL              the saved sign-in's access token, refreshed
;;;;                            first when it is about to expire, carrying the
;;;;                            account and residency headers; else
;;;;                            OPENAI_CODEX_OAUTH_TOKEN; else nothing
;;;;   RESPONSES-REQUEST-BODY   a round on this lane is the Codex request
;;;;   WALK-PROVIDER-STREAM     ... with the Codex headers, and its stream read
;;;;                            the Codex way
;;;;   NOTE-BODY-WIRE           the response headers' sticky-routing token and
;;;;                            models etag, sent back on the turn's next round
;;;;   /openai-codex-device     login, logout, status
;;;;
;;;; The sign-in is kept as openai-codex, as omp keeps it, and this cell
;;;; serves the openai-codex provider itself, so it works alone on a machine
;;;; with no browser.
;;;;
;;;; Beside nodecode-openai-codex, which signs in to the same subscription with
;;;; a browser and serves the same provider: both read and refresh the one
;;;; oauth_tokens.openai-codex entry, under a lock each and the same rule, and
;;;; whichever hook runs first answers the credential (the other never sees
;;;; the provider asked for). Each registers a lane of its own name and shapes
;;;; only the rounds on it. The provider resolves to the lane named
;;;; openai-codex while that cell runs, and to this one otherwise, so a round
;;;; is shaped once, by one of them.
;;;;
;;;; Beside nodecode-codex-auth: this lane's family is :openai-codex, so that
;;;; cell's openai-family rule never answers for it.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "openai-codex-device": {"base_url": "https://chatgpt.com/backend-api",
;;;;                           "originator": "codex_cli_rs"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-openai-codex-device)

(defun ours-p (config)
  "Whether the frozen provider CONFIG rides this cell's lane."
  (and config (equal (nle::effective-provider-config-lane config) +lane+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
openai-codex's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with openai-codex's row, made once
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

(defun entry-credential (entry source)
  "The credential the access token of ENTRY (a stored entry, or an object
holding only access_token) sends, from SOURCE."
  (let* ((access (nlk:json-value entry :text "access_token"))
         (account (or (token-profile access) (nlk:json-value entry :text "account_id")))
         (residency (token-residency access)))
    (nle:make-credential access source
                         (list :headers (append (when account `(("chatgpt-account-id" . ,account)))
                                                (when residency
                                                  `(("x-openai-internal-codex-residency" . ,residency))))
                               :installation-id (nlk:json-value entry :text "installation_id")))))

(defun credential (op next)
  "The :CREDENTIAL answer for openai-codex: the saved sign-in, refreshed when
due, else the token OPENAI_CODEX_OAUTH_TOKEN holds; else a probe reads as no
credential and a round is refused with what to do."
  ;; Never NEXT for this provider: the ladder's environment rung knows the
  ;; four core families, not this lane's. A refresh is a round's business: a
  ;; probe (no endpoint) reads the store as it is, without the network.
  (if (equal (getf op :provider) +provider+)
      (let ((entry (stored-entry (getf op :auth))))
        (cond (entry
               (entry-credential (if (getf op :endpoint)
                                     (fresh-entry (getf op :auth-path) entry)
                                     entry)
                                 :oauth))
              ((some #'nle::credential-env +env+)
               (entry-credential (nlk:json-object "access_token" (some #'nle::credential-env +env+)) :env))
              ((getf op :endpoint)
               (error 'nle:credential-error
                      :detail "openai-codex: not signed in; sign in with /openai-codex-device login"))
              (t (nle:make-credential "public" :public))))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "RESPONSES-REQUEST-BODY advice: a round on this lane is the Codex request."
  (when (and (ours-p config) (hash-table-p body))
    (shape-body body context))
  body)

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a round on this lane carries the Codex
headers, and its stream is read the Codex way."
  (if (ours-p config)
      (apply next (codex-fold fold) :headers (append headers (round-headers config))
             (alexandria:remove-from-plist keys :headers))
      (apply next fold keys)))

(defun start ()
  "Register the lane; on stop take it out, drop the merged catalog, end a
waiting sign-in and clear what the cell said."
  (setf *catalog* (cons nil nil))
  (let ((lane (make-lane)))
    (nle::register-provider-lane lane)
    (nle:on-stop (lambda ()
                   (setf nle::*provider-lanes* (remove lane nle::*provider-lanes*))
                   (setf *catalog* (cons nil nil))
                   (cancel-login)
                   (nle:notice nil :key +key+)))))

(nle:define-cell openai-codex-device
  (:section ("openai-codex-device")
    (:guide "sign in with /openai-codex-device login, then type the code it shows at auth.openai.com/codex/device on any device; base_url is where the Codex backend is served; originator is the client name every request carries")
    ("base_url" :string :default +base+
     :doc "the Codex backend: its root, its /codex, or its /codex/responses")
    ("originator" :string :default "codex_cli_rs"
     :doc "the client name every request carries"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::responses-request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk)
  (:hook 'nle::note-body-wire #'note-response)
  (:command "openai-codex-device" 'run-command
            :description "ChatGPT Plus/Pro sign-in with a device code: login, logout, status"
            :argument-hint "login | logout | status"
            :session nil))
