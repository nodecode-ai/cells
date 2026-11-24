;;;; cell.lisp --- the section, the five tools, /corpus, START-CELL.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Config, a sibling top-level key:
;;;;   "corpus": {"enabled": true,
;;;;              "endpoint": "https://corpuslaw.us/api/mcp",  ; required
;;;;              "api_key_env": "CORPUS_API_KEY",
;;;;              "jurisdiction": "",
;;;;              "timeout_seconds": 60}
;;;; A key is optional and jurisdiction is a convenience; the endpoint is the
;;;; one thing the cell cannot run without. A vetoed section
;;;; ("enabled": false) installs nothing.

(in-package #:nodecode-corpus)

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's, with the endpoint checked
here so a wrong or missing address refuses the start and not the first search."
  (declare (ignore table))
  (let ((endpoint (nlk:trimmed (or (getf values :endpoint) ""))))
    (unless (and (or (uiop:string-prefix-p "https://" endpoint)
                     (uiop:string-prefix-p "http://" endpoint))
                 (ignore-errors (quri:uri endpoint)))
      (nlk:config-error
       "corpus.endpoint is ~s: give the hosted MCP server's address as an http:// or ~
https:// URL -- https://corpuslaw.us/api/mcp unless you point the cell elsewhere"
       (getf values :endpoint)))
    (setf (getf values :endpoint) endpoint
          (getf values :api-key) (nlk:trimmed (or (getf values :api-key) ""))
          (getf values :api-key-env) (nlk:trimmed (or (getf values :api-key-env) ""))
          (getf values :jurisdiction) (nlk:trimmed (or (getf values :jurisdiction) "")))
    values))

(defun start ()
  "A start holds no session: the first call claims one, and a stop drops it so
the next start never reuses a stale id."
  (setf *session-id* nil *stateless-p* nil *rpc-counter* 0)
  (nle:on-stop (lambda () (setf *session-id* nil *stateless-p* nil))))

(nle:define-cell corpus
  (:section ("corpus")
    (:guide "endpoint is required and is Corpus's hosted MCP address, https://corpuslaw.us/api/mcp; no key is needed, since the anonymous floor answers and formation is free, while a free key from corpuslaw.us/settings, named by api_key_env or pasted as api_key, raises the research limit; jurisdiction is what law_search assumes when a call names none; timeout_seconds bounds one search or lookup")
    ("endpoint" :string :required t
                :doc "the hosted MCP server's address -- https://corpuslaw.us/api/mcp unless you run one")
    ("api_key" :secret
               :doc "a Corpus key written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "CORPUS_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("jurisdiction" :string :default ""
                    :doc "the state or city law_search assumes when a call names none, e.g. Austin, TX")
    ("timeout_seconds" :integer :default 60 :min 5
                        :doc "seconds one search or lookup may take"))
  (:settings #'read-settings)
  (:tool "law_search" (nlk:contract 'law-search-request) 'law-search-tool)
  (:tool "law_get_node" (nlk:contract 'law-node-request) 'law-node-tool)
  (:tool "law_coverage"
         (no-arguments "Which jurisdictions Corpus indexes. Ask before answering about one you are unsure of, and say so when it is not covered.")
         'law-coverage-tool)
  (:tool "corpus_account"
         (no-arguments "The Corpus credit balance and the limits on this key, free. Quote a limit or a price only from what this answers.")
         'account-tool)
  (:tool "corpus_call" (nlk:contract 'corpus-call-request) 'corpus-call-tool)
  (:command "corpus" 'run-slash
            :catches 'corpus-error
            :description "Search current US law through Corpus"
            :argument-hint "QUERY")
  (:help :corpus "corpus: current US law with verbatim citations, and a formation intake - (corpus:search \"...\")" *primer*)
  (:start #'start))
