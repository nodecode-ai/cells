;;;; cell.lisp --- the primer, the section, the /machine-library command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was a web search provider: Hermes asked it for web_search and
;;;; web_extract results. A cell registers tools of its own, so the two verbs
;;;; are the two tools tools.lisp declares, and the plugin's README guidance
;;;; --- cite the page, extract what a search found, other urls fail one by
;;;; one --- is +PRIMER+, which the model reads with (help :machine-library)
;;;; and whose one line rides every request. Nothing hooks the provider path:
;;;; no model is served here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "machine-library": {"api_key": "...", "api_key_env": "MACHINELIBRARY_API_KEY",
;;;;                       "base_url": "https://api.machinelibrary.ai",
;;;;                       "site_url": "https://machinelibrary.ai",
;;;;                       "timeout_seconds": 60, "max_results": 5,
;;;;                       "snippet_chars": 600, "max_chars": 12000}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-machine-library)

(defparameter +primer+
  "Machine Library is available through the nodecode-machine-library cell: research search
over papers, books, patents, standards and Wikipedia, and the full text of the documents it
holds. Every call spends the account's prepaid credits.
  machine_library_search   find documents. query, then limit (at most 20). Answers numbered
                   sources - title, the public page url, the byline (kind, authors,
                   publisher, year), the best-matching passage, and the identifier to cite
                   (doi://, iso://, isbn://, arxiv://).
  machine_library_extract  read documents already known. urls, as https://machinelibrary.ai/r/...
                   pages, or source links the library resolves - doi.org and DOI, arXiv,
                   ISBN, standard identifiers. Answers each document's title, page,
                   identifier and text; a url the library does not hold fails on its own
                   line, so pair it with a general page fetch for the open web.
How to work: search to discover, read the few documents that matter, and cite the page url or
the source identifier in your answer - a book is isbn://..., a standard iso://..., a paper
usually doi://.... A search that answers nothing proves no absence. A 401 means the key is
wrong, a 402 that the balance is spent, a 429 that the rate limit is on: stop and tell the
operator rather than retrying.
The operator runs a search from the keyboard with /machine-library <query>."
  "What (help :machine-library) answers while the cell runs: the plugin's guidance in one topic.")

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's values, with the key
resolved out of its variable, and the endpoints' own defaults."
  (declare (ignore table))
  (list :api-key (or (nonempty (getf values :api-key))
                      (let ((variable (nonempty (getf values :api-key-env))))
                        (and variable (nonempty (uiop:getenv variable))))
                      (nonempty (uiop:getenv "MACHINELIBRARY_API_KEY")))
        :api-key-env (getf values :api-key-env)
        :base-url (or (nonempty (getf values :base-url)) +base-url+)
        :site-url (or (nonempty (getf values :site-url)) +site-url+)
        :timeout-seconds (getf values :timeout-seconds)
        :max-results (getf values :max-results)
        :snippet-chars (getf values :snippet-chars)
        :max-chars (getf values :max-chars)))

;;; --- the operator's command --------------------------------------------------

(defun status-text ()
  "What this cell is connected to, without ever printing the key."
  (format nil "machine-library: ~a, key ~a; ~d sources and ~d characters at most; ~d tools (~{~a~^, ~})~%\
Search from here with /machine-library <query>."
          (root)
          (if (api-key) "set" "MISSING")
          (setting :max-results)
          (setting :max-chars)
          (count-if (lambda (entry) (eql 0 (search "machine_library_" (first entry)))) nle::*tools*)
          '("machine_library_search" "machine_library_extract")))

(defun run-command (args session-id)
  "One /machine-library invocation: with no argument, what the cell is
connected to; with one, a search the operator reads."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (tool-search (decode-search-call (nlk:make-json-object "query" query)))
        (status-text))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell machine-library
  (:section ("machine-library")
    (:guide "make a key at machinelibrary.ai/keys and paste it in api_key, or export MACHINELIBRARY_API_KEY (or name the variable in api_key_env); base_url only for a proxy of your own; max_results and max_chars bound one answer's size; next step: /machine-library shows the connection")
    ("api_key" :secret
     :doc "the Machine Library API key (searches spend its prepaid credits)")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (MACHINELIBRARY_API_KEY is read either way)")
    ("base_url" :string
     :doc "the API address: https://api.machinelibrary.ai unless you go through a proxy of your own")
    ("site_url" :string
     :doc "the public site the cited pages live on: https://machinelibrary.ai")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "one search or document fetch's ceiling")
    ("max_results" :integer :default 5 :min 1
     :doc "how many sources a search answers with unless the call names its own count, at most 20")
    ("snippet_chars" :integer :default 600 :min 100
     :doc "how much of a matching passage one search result carries")
    ("max_chars" :integer :default 12000 :min 500
     :doc "how much of one document's text an extract carries"))
  (:settings 'read-settings)
  (:help :machine-library
         "machine-library: research search and document text, with citable sources - (help :machine-library)"
         +primer+)
  (:command "machine-library" 'run-command
            :description "Machine Library: the connection, or a research search"
            :argument-hint "QUERY"
            :session nil)
  (:tool "machine_library_search" (nlk:contract 'search-call) #'tool-search)
  (:tool "machine_library_extract" (nlk:contract 'extract-call) #'tool-extract)
  (:start (lambda ()
            (check-config))))
