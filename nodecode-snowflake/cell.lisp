;;;; cell.lisp --- the cell: Snowflake Cortex among the organism's providers, and its sign-in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Six hooks, each declining for every provider but snowflake, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE     the catalog carries Snowflake's row: omp's
;;;;                            bundled models, so /connect offers it and
;;;;                            /models lists them
;;;;   LIST-PROVIDER-MODELS     the listing is the roster: omp never fetches
;;;;                            this credential-scoped one
;;;;   RESOLVE-MODEL-LANE       Claude rides the Messages wire, GPT the chat
;;;;                            wire, unless the operator's config pins one
;;;;   :CREDENTIAL              the sign-in's token, refreshed first when it is
;;;;                            about to expire and carrying its account; else
;;;;                            a PAT from SNOWFLAKE_PAT. A PAT /connect saved
;;;;                            in auth.json answers before this point does
;;;;   REQUEST-BODY             a chat round's cap as max_completion_tokens,
;;;;                            and no cache key
;;;;   WALK-PROVIDER-STREAM     the round goes to the Cortex REST API on its
;;;;                            account, the token as its bearer
;;;;   /snowflake               login [ACCOUNT], code ADDRESS, logout, status
;;;;
;;;; Config, a sibling top-level key:
;;;;   "snowflake": {"account": "myorg-myaccount"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-snowflake)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a snowflake round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Snowflake's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Snowflake's row, made once
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

(defun listing (next provider &rest keys &key key &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: Snowflake's listing is its roster. Asked with
KEY, which only /connect's key check does, it says why the PAT was not
checked."
  ;; The roster asks Snowflake nothing, and the check reads a NIL second
  ;; value as a PAT Snowflake took: any PAT read `works'.
  (if (equal provider +provider+)
      (values (listing-rows)
              (and key "Snowflake's model list is scoped to the credential and never fetched, so the first turn tries the token"))
      (apply next provider keys)))

(defun pinned-p (provider model)
  "Whether the operator's config pins the wire PROVIDER's MODEL rides."
  (or (nle::trimmed-config-string (nle::configured-model-entry provider model) "sdk")
      (nle::trimmed-config-string (nle::configured-provider-entry provider) "sdk")))

(defun model-lane-advice (next provider model)
  "RESOLVE-MODEL-LANE advice: a Snowflake model rides its row's wire."
  (if (and (equal provider +provider+) (not (pinned-p provider model)))
      (model-lane model)
      (funcall next provider model)))

(defun entry-credential (entry)
  "The credential the sign-in ENTRY sends: its access token, carrying its account."
  (nle:make-credential (nlk:json-value entry :text "access_token") :oauth
                       (list :account-url (nlk:json-value entry :string "account_url"))))

(defun credential (op next)
  "The :CREDENTIAL answer for snowflake: the sign-in, refreshed when due,
else the PAT SNOWFLAKE_PAT holds, else none."
  ;; Never NEXT for this provider: the ladder behind this point falls back to
  ;; the Messages family's default variable, and would send ANTHROPIC_API_KEY
  ;; to Snowflake. A refresh is a round's business: a probe (no endpoint)
  ;; reads the store as it is, without the network.
  (if (equal (getf op :provider) +provider+)
      (let ((entry (stored-entry (getf op :auth))))
        (cond (entry (entry-credential (if (getf op :endpoint)
                                           (fresh-entry (getf op :auth-path) entry)
                                           entry)))
              ((env-key) (nle:make-credential (env-key) :env))
              (t (nle:make-credential "public" :public))))
      (funcall next op)))

(defun chat (next context &aux (body (funcall next context))
                               (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: Cortex's chat wire refuses max_tokens (a hard
error), so the cap rides as max_completion_tokens; omp sends no cache key to
a host that is not OpenAI's own."
  (when (and (ours-p config) (hash-table-p body))
    (multiple-value-bind (cap present) (gethash "max_tokens" body)
      (when present
        (remhash "max_tokens" body)
        (setf (gethash "max_completion_tokens" body) cap)))
    (remhash "prompt_cache_key" body))
  body)

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a Snowflake round goes to the Cortex REST API
on its account, its token as the bearer."
  (if (ours-p config)
      (multiple-value-bind (token base) (round-route config)
        (apply next fold
               :headers (round-headers headers token)
               :endpoint (round-endpoint base (nle::effective-provider-config-lane config))
               (alexandria:remove-from-plist keys :headers :endpoint)))
      (apply next fold keys)))

(defun start ()
  "Begin from a fresh catalog; on stop drop it, end a waiting sign-in and
clear what the cell said."
  (setf *catalog* (cons nil nil))
  (nle:on-stop (lambda ()
                 (setf *catalog* (cons nil nil))
                 (cancel-login)
                 (nle:notice nil :key +key+))))

(nle:define-cell snowflake
  (:section ("snowflake")
    (:guide "sign in with /snowflake login ACCOUNT (a browser; ACCOUNT is orgname-accountname or the account URL), or save a programmatic access token with /connect or SNOWFLAKE_PAT and name the account here or in SNOWFLAKE_ACCOUNT")
    ("account" :string
     :doc "the account identifier (orgname-accountname) or URL a PAT is sent to, and /snowflake login signs in to by default (else SNOWFLAKE_ACCOUNT)"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::list-provider-models #'listing)
  (:hook 'nle::resolve-model-lane #'model-lane-advice)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'chat)
  (:hook 'nle::walk-provider-stream #'walk)
  (:command "snowflake" 'run-command
            :description "Snowflake Cortex sign-in: login [ACCOUNT], code ADDRESS, logout, status"
            :argument-hint "login [ACCOUNT] | code ADDRESS | logout | status"
            :session nil
            :complete 'complete-command))
