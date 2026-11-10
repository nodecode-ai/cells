;;;; cell.lisp --- the primer, the section, the tool, /cloro, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a sibling top-level key next to `websearch':
;;;;   "cloro": {"api_key": "...", "api_key_env": "CLORO_API_KEY",
;;;;             "country": "US", "count": 5, "timeout_seconds": 120}
;;;; The key is the one required member, pasted or named by a variable -- as
;;;; the original read CLORO_API_KEY from the environment -- and a section
;;;; naming no key refuses the start, the way the original offered no backend
;;;; without a key.

(in-package #:nodecode-cloro)

;;; --- the manual ---------------------------------------------------------------
;;; (help :cloro) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter *primer*
  "cloro search is available through the nodecode-cloro cell: one model tool,
cloro_search, and the same call in the CLORO package from eval. Both answer the
same lines and spend the operator's own cloro credits.
  (cloro:search \"query\" &key count country)
      Google's organic results, one block each: a numbered title, its url, then
      the snippet. count is 1 to 20 and the section's count answers when the call
      names none; country is the 2-letter ISO 3166-1 code the results are located
      in, such as \"US\" or \"DE\", and the section's country answers when the call
      names none.
Doctrine: search to discover, then use the urls you got - this cell searches, it
does not read a page. Read the results before searching again: when a block already
answers, stop, and cite the urls in your answer. A second search is for a new
question, not the same one reworded. ERROR: CLORO-ERROR names the problem;
when it says the key was refused, tell the operator to check cloro.api_key. Each
search spends credits on the operator's cloro account, and cloro opens the live
results page while it answers, so a call takes seconds: ask for what the question
needs, not twenty results every time."
  "What (help :cloro) answers while the cell runs.")

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
               "no key: paste a cloro API key into api_key, or name the ~
variable holding it in api_key_env~@[ (~a is unset or empty)~]"
               env)))
    plist))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell cloro
  (:section ("cloro")
    (:guide "make an API key at dashboard.cloro.dev/api-keys, then paste it into api_key or name the variable holding it in api_key_env; country is the 2-letter code the results are located in, and count is what a call that names none asks for")
    ("api_key" :secret
               :doc "the cloro API key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "CLORO_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("country" :string :default "US"
               :doc "a 2-letter ISO 3166-1 code such as US or DE, the location of the results a call that names none gets")
    ("count" :integer :default 5 :min 1
             :doc "how many results a call that names none asks for (at most 20; more is clamped)")
    ("timeout_seconds" :integer :default 120 :min 1
                       :doc "seconds one search may take; cloro loads the live results page, so it is slow"))
  (:settings #'read-settings)
  (:tool "cloro_search" (nlk:contract 'cloro-search) #'cloro-search-tool)
  (:command "cloro" 'run-slash
            :catches 'cloro-error
            :description "Search Google through cloro with your own key"
            :argument-hint "QUERY")
  (:help :cloro
   "cloro: live Google results through cloro with your own key - the cloro_search tool, and (cloro:search \"...\")"
   *primer*))
