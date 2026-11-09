;;;; cell.lisp --- the section, the tools, /parallel, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a sibling top-level key next to `websearch':
;;;;   "parallel-search": {"enabled": true,
;;;;                       "endpoint": "https://search.parallel.ai/mcp",
;;;;                       "max_results": 5,
;;;;                       "timeout_seconds": 60}
;;;; Nothing is required and nothing is a secret: the endpoint is anonymous,
;;;; and a vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-parallel-search)

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's, with the endpoint
checked here so a wrong address refuses the start and not the first search."
  (declare (ignore table))
  (let ((endpoint (nlk:trimmed (or (getf values :endpoint) ""))))
    (unless (and (or (uiop:string-prefix-p "https://" endpoint)
                     (uiop:string-prefix-p "http://" endpoint))
                 (ignore-errors (quri:uri endpoint)))
      (nlk:config-error
       "parallel-search.endpoint is ~s: give the MCP server's address as an http:// or ~
https:// URL -- https://search.parallel.ai/mcp unless you run your own"
       (getf values :endpoint)))
    (setf (getf values :endpoint) endpoint)
    values))

(defun start ()
  "A start holds no session: the first call claims one, and a stop drops it
so the next start never reuses a stale id."
  (setf *session-id* nil *stateless-p* nil *rpc-counter* 0)
  (nle:on-stop (lambda () (setf *session-id* nil *stateless-p* nil))))

(nle:define-cell parallel-search
  (:section ("parallel-search")
    (:guide "no key and nothing required: the anonymous free tier answers, rate limited; endpoint is the MCP address, max_results is what a search asks for when the call names none, timeout_seconds bounds one search or fetch")
    ("endpoint" :string :default "https://search.parallel.ai/mcp"
                :doc "the MCP server's address; Parallel's own unless you run one")
    ("max_results" :integer :default 5 :min 1
                   :doc "how many results a search asks for when the call names none")
    ("timeout_seconds" :integer :default 60 :min 5
                        :doc "seconds one search or fetch may take"))
  (:settings #'read-settings)
  (:tool "web_search" (nlk:contract 'web-search-request) 'web-search-tool)
  (:tool "web_fetch" (nlk:contract 'web-fetch-request) 'web-fetch-tool)
  (:command "parallel" 'run-slash
            :catches 'parallel-search-error
            :description "Search the web through Parallel's free server"
            :argument-hint "QUERY")
  (:help :parallel "parallel: keyless web search and page fetch - (parallel:search \"...\")" *primer*)
  (:start #'start))
