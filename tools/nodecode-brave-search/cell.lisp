;;;; cell.lisp --- the primer, the section, the tool, /brave, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a sibling top-level key next to `websearch':
;;;;   "brave-search": {"api_key": "...", "api_key_env": "BRAVE_API_KEY",
;;;;                    "count": 10, "safesearch": "moderate",
;;;;                    "country": "us", "search_lang": "en", "freshness": "pw",
;;;;                    "timeout_seconds": 20}
;;;; The key is the one required member, pasted or named by a variable -- as
;;;; the original read BRAVE_API_KEY from the environment -- and a section
;;;; naming no key refuses the start, the way the original offered no tool.

(in-package #:nodecode-brave-search)

;;; --- the manual ---------------------------------------------------------------
;;; (help :brave) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter *primer*
  "Brave Search is available through the nodecode-brave-search cell: one model tool,
brave_web_search, and the same call in the BRAVE package from eval. Both answer the
same lines and use the operator's own Brave Search key.
  (brave:search \"query\" &key count country search-lang freshness safesearch)
      Ranked web results, one block each: a numbered title, its url, who published it
      and how old Brave says it is, then the description. count is 1 to 20 and the
      section's count answers when the call names none. country is a code such as
      \"us\"; search-lang one such as \"en\"; freshness a window - \"pd\" a day, \"pw\"
      a week, \"pm\" a month, \"py\" a year; safesearch \"off\", \"moderate\" or
      \"strict\".
Doctrine: search to discover, then use the urls you got - this cell searches, it does
not read a page. Read the results before searching again: when a block already answers,
stop, and cite the urls in your answer. A second search is for a new question, not the
same one reworded. ERROR: BRAVE-SEARCH-ERROR names the problem; when it says the key was
refused, tell the operator to check brave-search.api_key. The key is free-tier metered:
one call asks for what the question needs, not twenty results every time."
  "What (help :brave) answers while the cell runs.")

;;; --- reading the section --------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, with the key read here, once: the inline secret,
else the variable api_key_env names, else a refusal."
  (declare (ignore table))
  (let* ((key (nonblank (getf values :api-key)))
         (env (nonblank (getf values :api-key-env)))
         (plist (copy-list values)))
    (setf (getf plist :api-key)
          (or key
              (and env (nonblank (nle::credential-env env)))
              (nlk:config-error
               "no key: paste a Brave Search subscription token into api_key, or name the ~
variable holding it in api_key_env~@[ (~a is unset or empty)~]"
               env)))
    plist))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell brave-search
  (:section ("brave-search")
    (:guide "make a subscription token at api-dashboard.search.brave.com/app/keys, then paste it into api_key or name the variable holding it in api_key_env; count is what a call that names none asks for, and safesearch, country, search_lang and freshness are the defaults such a call gets")
    ("api_key" :secret
               :doc "the Brave Search subscription token, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "BRAVE_API_KEY"
                   :doc "the environment variable holding the token, read when api_key is empty")
    ("count" :integer :default 10 :min 1
             :doc "how many results a call that names none asks for (at most 20; more is clamped)")
    ("safesearch" :choice :options +safe-search-modes+ :default "moderate"
                  :doc "the safe-search mode a call that names none asks for")
    ("country" :string
               :doc "a country code such as us, for a call that names none; empty leaves it to Brave")
    ("search_lang" :string
                   :doc "a result language code such as en, for a call that names none; empty leaves it to Brave")
    ("freshness" :string
                 :doc "a recency window (pd, pw, pm, py) for a call that names none; empty leaves it to Brave")
    ("timeout_seconds" :integer :default 20 :min 1
                       :doc "seconds one search may take"))
  (:settings #'read-settings)
  (:tool "brave_web_search" (nlk:contract 'web-search-request) #'web-search-tool)
  (:command "brave" 'run-slash
            :catches 'brave-search-error
            :description "Search the web with your own Brave Search key"
            :argument-hint "QUERY")
  (:help :brave
   "brave: web search through Brave Search with your own key - the brave_web_search tool, and (brave:search \"...\")"
   *primer*))
