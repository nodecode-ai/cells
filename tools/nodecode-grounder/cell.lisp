;;;; cell.lisp --- the primer, the section, the tool, /grounder, START-CELL.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Config, a sibling top-level key next to `websearch':
;;;;   "grounder": {"api_key": "...", "api_key_env": "GROUNDER_API_KEY",
;;;;                "base_url": "https://api.grounder.net",
;;;;                "max_results": 5, "description_chars": 800,
;;;;                "timeout_seconds": 45}
;;;; The key is the one required member, pasted or named by a variable -- as
;;;; the original read GROUNDER_API_KEY from Hermes's profile -- and a section
;;;; naming no key refuses the start, the way the original answered no result
;;;; without one.

(in-package #:nodecode-grounder)

;;; --- the manual ---------------------------------------------------------------
;;; (help :grounder) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter *primer*
  "Grounder is available through the nodecode-grounder cell: one model tool,
grounder_search, and the same call in the GROUNDER package from eval. Both answer
the same lines and spend the operator's own Grounder account, 50 searches a day free.
  (grounder:search \"query\" &key limit)
      Licensed web results, one block each: a numbered title, its url, then a
      description -- the passages Grounder chose as the ones that answer the query,
      at most 800 characters. One search reaches the providers the query needs
      (Exa, Brave, Tavily, Perplexity and more, plus live scores, prices, odds and
      flights) and answers one deduplicated list, so fewer results than asked for is
      expected and is never padded with unlicensed ones. limit is 1 to 20 and the
      section's max_results answers when a call names none.
Doctrine: search to discover, then use the urls you got -- this cell searches, it
does not read a page. Read the results before searching again: when a block already
answers, stop, and cite the urls in your answer. A second search is for a new
question, not the same one reworded. ERROR: GROUNDER-ERROR names the problem; when
it says the key was rejected, tell the operator to check grounder.api_key."
  "What (help :grounder) answers while the cell runs.")

;;; --- reading the section --------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, with the key read here, once: the pasted secret, else the
variable api_key_env names, else a refusal."
  (declare (ignore table))
  (let* ((key (nonblank (getf values :api-key)))
         (env (nonblank (getf values :api-key-env)))
         (plist (copy-list values)))
    (setf (getf plist :api-key)
          (or key
              (and env (nonblank (nle::credential-env env)))
              (nlk:config-error
               "no key: make a Grounder search key at grounder.net/account/keys and paste it into ~
api_key, or name the variable holding it in api_key_env~@[ (~a is unset or empty)~]"
               env)))
    plist))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell grounder
  (:section ("grounder")
    (:guide "make a search key at grounder.net/account/keys, then paste it into api_key or name the variable holding it in api_key_env; max_results is what a call that names none asks for, and description_chars is how much of each result's text Grounder is asked to choose")
    ("api_key" :secret
               :doc "the Grounder search key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "GROUNDER_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("base_url" :string :default "https://api.grounder.net"
                :doc "Grounder's API origin; /v1/search is appended to it")
    ("max_results" :integer :default 5 :min 1
                   :doc "how many results a call that names none asks for (at most 20; more is clamped)")
    ("description_chars" :integer :default 800 :min 1
                         :doc "how much of each result's text Grounder is asked to fit, and what a row carries")
    ("timeout_seconds" :integer :default 45 :min 1
                       :doc "seconds one search may take, across every provider it reaches"))
  (:settings #'read-settings)
  (:tool "grounder_search" (nlk:contract 'search-request) #'search-tool)
  (:command "grounder" 'run-slash
            :catches 'grounder-error
            :description "Search the web through Grounder"
            :argument-hint "QUERY")
  (:help :grounder
   "grounder: licensed web search through Grounder - the grounder_search tool, and (grounder:search \"...\")"
   *primer*))
