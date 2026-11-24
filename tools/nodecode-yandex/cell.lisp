;;;; cell.lisp --- the primer, the section, the two tools, /yandex, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a sibling top-level key next to `websearch':
;;;;   "yandex": {"api_key": "...", "api_key_env": "YANDEX_API_KEY",
;;;;              "folder_id": "...", "folder_id_env": "YANDEX_FOLDER_ID",
;;;;              "search_type": "SEARCH_TYPE_RU",
;;;;              "base_url": "https://searchapi.api.cloud.yandex.net",
;;;;              "max_results": 5, "timeout_seconds": 30}
;;;; The key and the folder are the two members no call can work without,
;;;; pasted or named by a variable -- as the original read YANDEX_API_KEY and
;;;; YANDEX_FOLDER_ID from the environment -- and a section naming neither
;;;; refuses the start, the way the original offered no tool without them.

(in-package #:nodecode-yandex)

;;; --- the manual ---------------------------------------------------------------
;;; (help :yandex) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter *primer*
  "Yandex search is available through the nodecode-yandex cell: two model tools,
and the same calls in the YANDEX package from eval. Both spend the operator's own
Yandex Cloud API key, which Yandex bills to one folder: a call is not free, so ask
once and read the answer.
  (yandex:search \"query\" &key n page)       classic web results
  (yandex:answer \"question\" &key sites)      one grounded answer with its sources
      The tools are yandex_web_search (query, n) and yandex_generative_search
      (query, sites: up to five domains to keep the sources inside). n is 1 to 100,
      and the section's max_results answers a call that names none; page is zero-based.
      A yandex_web_search answer is one block a result: a numbered title, its url and
      domain, then the snippet Yandex's passages give. A yandex_generative_search
      answer is the answer text, then the sources it cites (title, url, the text
      Yandex used), then the queries Yandex ran and any spelling correction.
Use the web search to discover pages, then read the ones you need; use the
generative search when a question wants a direct factual answer rather than a list
of links. Yandex is strongest for Russian-language queries. Cite the urls you used,
and read an answer before asking again: a second call is a new question, not the
same one reworded. ERROR: YANDEX-ERROR names the problem; when it names the key or
the folder, tell the operator to check the yandex section."
  "What (help :yandex) answers while the cell runs.")

;;; --- reading the section --------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, with the two credentials read here, once: the inline
value, else the variable its *_env names, else a refusal naming what is missing."
  (declare (ignore table))
  (let* ((plist (copy-list values))
         (key-env (nonblank (getf values :api-key-env)))
         (folder-env (nonblank (getf values :folder-id-env)))
         (key (or (nonblank (getf values :api-key))
                  (and key-env (nonblank (nle::credential-env key-env)))))
         (folder (or (nonblank (getf values :folder-id))
                     (and folder-env (nonblank (nle::credential-env folder-env))))))
    (unless key
      (nlk:config-error "no API key: paste a Yandex Cloud API key into api_key, or name the ~
variable holding it in api_key_env~@[ (~a is unset or empty)~]"
                        key-env))
    (unless folder
      (nlk:config-error "no folder id: paste the Yandex Cloud folder the key bills into ~
folder_id, or name the variable holding it in folder_id_env~@[ (~a is unset or empty)~]"
                        folder-env))
    (setf (getf plist :api-key) key
          (getf plist :folder-id) folder)
    plist))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell yandex
  (:section ("yandex")
    (:guide "make a Yandex Cloud API key, and note the folder it bills to, then paste the key into api_key and the folder id into folder_id, or name the variables holding them in api_key_env and folder_id_env; the Search API is a paid service billed to that folder; search_type picks the market a query runs against, SEARCH_TYPE_RU by default")
    ("api_key" :secret
               :doc "the Yandex Cloud API key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "YANDEX_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("folder_id" :string
                 :doc "the Yandex Cloud folder id the requests are billed to, written here; left empty, the variable folder_id_env names is read")
    ("folder_id_env" :env :default "YANDEX_FOLDER_ID"
                     :doc "the environment variable holding the folder id, read when folder_id is empty")
    ("search_type" :choice :options +search-types+ :default "SEARCH_TYPE_RU"
                   :doc "the market a query runs against")
    ("base_url" :string :default "https://searchapi.api.cloud.yandex.net"
                :doc "where the Yandex Search API answers; change it only for a gateway of your own")
    ("max_results" :integer :default 5 :min 1
                   :doc "how many results a search that names none asks for (at most 100; more is clamped)")
    ("timeout_seconds" :integer :default 30 :min 1
                       :doc "seconds one request may take"))
  (:settings #'read-settings)
  (:tool "yandex_web_search" (nlk:contract 'web-search-request) #'web-search-tool)
  (:tool "yandex_generative_search" (nlk:contract 'generative-search-request) #'generative-search-tool)
  (:command "yandex" 'run-slash
            :description "Search the web with your own Yandex Search API key"
            :argument-hint "QUERY")
  (:help :yandex
   "yandex: web search and grounded answers through the Yandex Search API, with your own key - the yandex_web_search and yandex_generative_search tools, and (yandex:search \"...\")"
   *primer*))
