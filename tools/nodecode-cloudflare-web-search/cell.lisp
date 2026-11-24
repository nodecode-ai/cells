;;;; cell.lisp --- the primer, the section, the command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original registered one web-search backend with its harness: one
;;;; search call, its own token out of the secret store, and the account,
;;;; gateway, provider and BYOK alias out of its plugin settings. A cell's
;;;; tools are called by name, so the same call arrives here as the
;;;; cloudflare_search tool, the /cloudflare-web-search command and
;;;; (cloudflare-web-search:search "..."). The primer is what the original's
;;;; setup schema said, written out for the model.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "cloudflare-web-search": {"api_token": "...",
;;;;                             "api_token_env": "CLOUDFLARE_WEB_SEARCH_API_TOKEN",
;;;;                             "account_id": "...", "gateway_id": "default",
;;;;                             "provider": "ceramic", "byok_alias": "",
;;;;                             "base_url": "https://api.cloudflare.com/client/v4",
;;;;                             "max_results": 5, "timeout_seconds": 30}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-cloudflare-web-search)

;;; --- the manual ------------------------------------------------------------------

(defparameter *primer*
  "Cloudflare Web Search is available through the nodecode-cloudflare-web-search cell:
one model tool, cloudflare_search, the same call as (cloudflare-web-search:search
\"query\") from eval, and /cloudflare-web-search <query> from the keyboard. It searches
the live web through the operator's own Cloudflare account, which routes the query to
Ceramic, Exa or Linkup on its AI Gateway.
  cloudflare_search  query, then max_results (1 to 10) and provider (ceramic, exa or
                     linkup) when the question needs them. Answers numbered results,
                     one block each: title, url, description, then the route and the
                     gateway's request id.
The section's max_results and provider answer a call that names neither. Every uncached
search is billed to the gateway's credits, or to a provider key stored there; a provider
alias in the section pins the key to use. It searches only: it does not read a page.
Doctrine: search to discover, read the blocks whose url answers, and cite the urls in
your answer. A second search is for a new question, not the same one reworded. An
ERROR: line names what went wrong in Cloudflare's own words - when it says the token
was refused or the account is wrong, tell the operator to check the
cloudflare-web-search section rather than retrying."
  "What (help :cloudflare-web-search) answers while the cell runs, and the one line
the model reads in every request: the original's provider, in this cell's terms.")

;;; --- reading the section ---------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES with the token, the account and the gateway resolved:
the inline secret first, then the variable the section names, then the
variable the original read (CLOUDFLARE_WEB_SEARCH_API_TOKEN,
CLOUDFLARE_ACCOUNT_ID, CLOUDFLARE_GATEWAY_ID)."
  (declare (ignore table))
  (let ((settings (copy-list values))
        (named (nonblank (getf values :api-token-env))))
    (setf (getf settings :api-token)
          (or (nonblank (getf values :api-token))
              (and named (nle::credential-env named))
              (nle::credential-env +token-env+))
          (getf settings :account-id)
          (or (nonblank (getf values :account-id))
              (nle::credential-env +account-env+))
          (getf settings :gateway-id)
          (or (nonblank (getf values :gateway-id))
              (nle::credential-env +gateway-env+)
              "default")
          (getf settings :byok-alias)
          (nonblank (getf values :byok-alias)))
    settings))

(defun check-config ()
  "Refuse a section that could not search, naming the member: the original's
one unavailable state, made a refusal the operator reads and repairs."
  (let ((problem (settings-problem (current-settings))))
    (when problem
      (nlk:config-error "~a" problem))))

;;; --- the operator's command --------------------------------------------------------

(defun status-text ()
  "What this cell is connected to, without ever printing the token."
  (let* ((settings (current-settings))
         (problem (settings-problem settings)))
    (if problem
        (format nil "cloudflare-web-search: not ready: ~a~%Set it in /settings under ~
cloudflare-web-search, then (restart-cells)." problem)
        (format nil "cloudflare-web-search: ~a~%token set, provider ~a, gateway ~a; the ~
cloudflare_search tool asks for ~d results unless a call names its own count, timeout ~ds.~%
Search from here with /cloudflare-web-search <query>."
                (endpoint settings)
                (getf settings :provider)
                (getf settings :gateway-id)
                (getf settings :max-results)
                (getf settings :timeout-seconds)))))

(defun run-slash (args session-id)
  "/cloudflare-web-search QUERY: the operator's own search, answered as the
model's tool is; with no query, what the cell is connected to."
  (declare (ignore session-id))
  (let ((query (nonblank args)))
    (if query
        (search query)
        (status-text))))

;;; --- the declaration ----------------------------------------------------------------

(nle:define-cell cloudflare-web-search
  (:section ("cloudflare-web-search")
    (:guide "make an API token with Account Workers AI Read and Account AI Gateway Read at https://developers.cloudflare.com/web-search/how-to-use/, then paste it into api_token (or export CLOUDFLARE_WEB_SEARCH_API_TOKEN and name it in api_token_env); account_id is the 32-character Cloudflare account id (or export CLOUDFLARE_ACCOUNT_ID); gateway_id names the AI Gateway that routes and bills the search; provider picks ceramic, exa or linkup, and byok_alias pins a provider key stored on the gateway; next step: /cloudflare-web-search shows the connection")
    ("api_token" :secret
     :doc "the Cloudflare API token, written here; left empty, the variable api_token_env names is read")
    ("api_token_env" :env :default "CLOUDFLARE_WEB_SEARCH_API_TOKEN"
     :doc "the environment variable holding the token, read when api_token is empty")
    ("account_id" :string
     :doc "the 32-character Cloudflare account id (else CLOUDFLARE_ACCOUNT_ID)")
    ("gateway_id" :string :default "default"
     :doc "the AI Gateway the search is routed, logged and billed through (else CLOUDFLARE_GATEWAY_ID)")
    ("provider" :choice :options '("ceramic" "exa" "linkup") :default "ceramic"
     :doc "which search provider routes a call that names none")
    ("byok_alias" :string
     :doc "the alias of a provider key stored on the gateway, when you bring your own; empty uses the gateway's credits")
    ("base_url" :string :default +api-root+
     :doc "Cloudflare's API root; change it only for a proxy of your own")
    ("max_results" :integer :default 5 :min 1
     :doc "how many results a call that names none asks for (at most 10)")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "seconds one search may take"))
  (:settings #'read-settings)
  (:tool "cloudflare_search" (nlk:contract 'search-call) #'search-tool)
  (:command "cloudflare-web-search" 'run-slash
            :catches 'cloudflare-web-search-error
            :description "Search the web through Cloudflare Web Search"
            :argument-hint "QUERY")
  (:help :cloudflare-web-search
         "cloudflare-web-search: web search through Cloudflare Web Search (Ceramic, Exa, Linkup) - the cloudflare_search tool, and (cloudflare-web-search:search \"...\")"
         *primer*)
  (:start (lambda () (check-config))))
