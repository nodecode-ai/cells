;;;; cell.lisp --- the cell: a Claude Pro/Max sign-in for the anthropic provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four hooks and one slash command. Every hook declines for every round
;;;; that is not a subscription round, so the core's own anthropic path, a key
;;;; sent as x-api-key, is what it was:
;;;;
;;;;   MODELS-CATALOG-TABLE     the anthropic row gains omp's bundled models
;;;;                            models.dev does not list; nothing it lists
;;;;                            changes, and no base is added
;;;;   :CREDENTIAL              the saved sign-in, refreshed first when it is
;;;;                            about to expire, when no key answered: not the
;;;;                            config, not auth.json's api_keys, not the
;;;;                            environment (ANTHROPIC_API_KEY,
;;;;                            ANTHROPIC_AUTH_TOKEN)
;;;;   ANTHROPIC-REQUEST-BODY   a subscription round is Claude Code's request:
;;;;                            the billing header and identity first in the
;;;;                            system prompt, tool names prefixed, a metadata
;;;;                            user id, hour-long cache marks
;;;;   WALK-PROVIDER-STREAM     ... sent with a bearer and Claude Code's headers
;;;;                            to /v1/messages?beta=true, its cch attested,
;;;;                            and its tool calls read back under their names
;;;;   /anthropic               login, code CODE, logout, status
;;;;
;;;; A subscription round is one on the anthropic lane whose credential is
;;;; this cell's, or whose key is a subscription token (sk-ant-oat...), as
;;;; omp tells them apart.
;;;;
;;;; Config, a sibling top-level key; it has nothing to set:
;;;;   "anthropic": {}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-anthropic)

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
the anthropic row filled in.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with omp's anthropic models where
models.dev has none, made once per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun entry-credential (entry)
  "The credential the sign-in ENTRY sends."
  (nle:make-credential (nlk:json-value entry :text "access_token") :oauth
                       (list :subscription t
                             :account-id (nlk:json-value entry :text "account_id")
                             :installation-id (nlk:json-value entry :text "installation_id"))))

(defun credential (op next)
  "The :CREDENTIAL answer for anthropic: the saved sign-in, refreshed when
due, unless the environment holds a key, which keeps winning."
  ;; The point runs after the config and api_keys tiers and before the
  ;; environment's: a key there is checked here, so a sign-in never shadows
  ;; it. A refresh is a round's business: a probe (no endpoint) reads the
  ;; store as it is, without the network.
  (let ((entry (and (equal (getf op :provider) +provider+)
                    (not (nle::env-credential +provider+ :anthropic))
                    (stored-entry (getf op :auth)))))
    (if entry
        (entry-credential (if (getf op :endpoint)
                              (fresh-entry (getf op :auth-path) entry)
                              entry))
        (funcall next op))))

(defvar *betas* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A subscription round's frozen config -> the beta list its body asked for:
how the body hook tells the walk what it built.")

(defun body (next context &aux (config (nle::compiled-turn-context-provider-config context)))
  "ANTHROPIC-REQUEST-BODY advice: a subscription round's body is Claude Code's."
  (if (subscription-p config)
      ;; Claude Code's cache policy for a seat: marks that live an hour
      (let* ((nle::*anthropic-cache-breakpoints* '(:static "1h" :conversation "1h"))
             (answer (multiple-value-list (funcall next context)))
             (body (first answer)))
        (when (hash-table-p body)
          (shape-body body config)
          (setf (gethash config *betas*) (betas body)))
        (values-list answer))
      (funcall next context)))

(defun walk (next fold &rest keys &key config headers request-json &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a subscription round goes out as Claude Code
sends it, and its tool calls come back under their own names."
  (if (subscription-p config)
      (apply next (unprefixed-fold fold)
             :headers (round-headers config (gethash config *betas* +utility-betas+) headers)
             :request-json (patch-cch (nle::request-octets request-json))
             :endpoint (beta-endpoint (or (getf keys :endpoint)
                                          (nle::credential-attribute config :endpoint)
                                          (nle::effective-provider-config-endpoint config)))
             (alexandria:remove-from-plist keys :headers :request-json :endpoint))
      (apply next fold keys)))

(defun start ()
  "On stop, drop the merged catalog, end a waiting sign-in and clear what the cell said."
  (setf *catalog* (cons nil nil))
  (nle:on-stop (lambda ()
                 (setf *catalog* (cons nil nil))
                 (cancel-login)
                 (nle:notice nil :key +key+))))

(nle:define-cell anthropic
  (:section ("anthropic")
    (:guide "sign in with /anthropic login (a browser), or paste the code claude.ai shows with /anthropic code; a key (/connect, ANTHROPIC_API_KEY) still outranks the sign-in"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::anthropic-request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk)
  (:command "anthropic" 'run-command
            :description "Claude Pro/Max sign-in: login, code CODE, logout, status"
            :argument-hint "login | code CODE | logout | status"
            :session nil))
