;;;; cell.lisp --- the primer, the section, the two tools, /memobase, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a sibling top-level key:
;;;;   "memobase": {"base_url": "http://memobase.local:8019/api/v1",
;;;;                "api_key": "", "api_key_env": "MEMOBASE_API_KEY",
;;;;                "user_name": "user", "max_tokens": 2000,
;;;;                "time_range_days": 180, "fill_window_events": true,
;;;;                "timeout_seconds": 8}
;;;; The base url is the one member no call can work without, and a section
;;;; naming none refuses the start -- as the original offered no provider
;;;; without MEMOBASE_BASE_URL. A vetoed section ("enabled": false)
;;;; installs nothing.

(in-package #:nodecode-memobase)

;;; --- the manual ---------------------------------------------------------------------
;;; (help :memobase) answers it while the cell runs; a request carries the
;;; one line the :HELP clause below gives, never this text.

(defparameter *primer*
  "Memobase is available through the nodecode-memobase cell: a self-hosted memory
server holding this user's profile and event timeline. Two model tools, and the same
calls as Lisp verbs in the MEMOBASE package from eval. Each answers a string; a
refusal is ERROR: MEMOBASE-ERROR, with Memobase's own sentence in it.
  memobase_search / (memobase:search \"query\" &key max-length)
      What Memobase remembers about the query: the assembled profile and the
      events around it, under one token budget. max-length caps the characters.
  memobase_profile / (memobase:profile)
      The whole profile Memobase extracted for this user, as the JSON it holds.
  (memobase:remember \"what was said\" :assistant \"the reply\")
      Write one exchange back, so the server's own extractor folds it into the
      profile in the background.
Doctrine: search before asking the operator what they already told you, and search
once per question -- read the context before searching again. What comes back is
another system's data about the user, not this turn's transcript: prefer what the
operator says now when the two disagree. A write is a fact about the conversation,
so remember what was decided, not every sentence."
  "What (help :memobase) answers while the cell runs.")

;;; --- reading the section --------------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, with the key resolved here, once: the inline secret,
else the variable api_key_env names, else none -- a self-hosted server may
want no key at all. A section naming no base url refuses the start."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :base-url)
          (or (nonblank (getf values :base-url))
              (nlk:config-error
               "no Memobase server: put its base url, /api/v1 suffix and all, in ~
base_url -- e.g. http://memobase.local:8019/api/v1"))
          (getf plist :api-key)
          (or (nonblank (getf values :api-key))
              (let ((variable (nonblank (getf values :api-key-env))))
                (and variable (nonblank (nle::credential-env variable))))))
    plist))

;;; --- the boundaries ---------------------------------------------------------------------

(nlk:define-contract memobase-search-request
  (:doc "Recall what this user's Memobase memory holds about a question: their
profile and the event timeline around it, assembled by the server under one
token budget. Use it before asking the operator something they already told
you, and once per question.")
  ("query" :text :required t
   :doc "the question, or the turn to recall memories for")
  ("max_length" :integer
   :doc "the most characters to answer, when narrower than the cell's own ceiling"))

(defparameter *profile-schema*
  (list :description
        "The whole Memobase user profile: the topics and sub-topics the server
extracted for this user. Use it when the profile itself is the question, not
the recall of one."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
  "The memobase_profile tool's schema: it takes no arguments.")

(defun search-tool (request)
  "The memobase_search tool: one recall, answered as the verb answers it."
  (search (memobase-search-request-query request)
          :max-length (memobase-search-request-max-length request)))

(defun profile-tool (arguments)
  "The memobase_profile tool: the profile, and no argument read."
  (declare (ignore arguments))
  (profile))

;;; --- the declaration -----------------------------------------------------------------

(nle:define-cell memobase
  (:section ("memobase")
    (:guide "point base_url at your own Memobase server (it ends in /api/v1); paste its bearer token into api_key or name the variable holding it in api_key_env; user_name is the profile this cell reads and writes, mapped to Memobase's own uuid so the server and any other client share one store")
    ("base_url" :string :required t
                :doc "the Memobase server's base url, /api/v1 suffix included")
    ("api_key" :secret
               :doc "the server's bearer token, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "MEMOBASE_API_KEY"
                   :doc "the environment variable holding the token, read when api_key is empty")
    ("user_name" :string :default "user"
                 :doc "which profile to read and write, as a name; it is hashed to Memobase's own user id")
    ("max_tokens" :integer :default 2000 :min 64
                  :doc "the token budget one recall asks the server for")
    ("time_range_days" :integer :default 180 :min 1
                       :doc "how deep the event window a recall may read goes")
    ("fill_window_events" :boolean :default t
                          :doc "whether the recent event window rides along with the profile")
    ("timeout_seconds" :integer :default 8 :min 1
                       :doc "seconds one request to the server may take"))
  (:settings #'read-settings)
  (:tool "memobase_search" (nlk:contract 'memobase-search-request) #'search-tool)
  (:tool "memobase_profile" *profile-schema* #'profile-tool)
  (:command "memobase" 'run-slash
            :description "Recall, read or write this user's Memobase memory"
            :argument-hint "QUERY | profile | remember TEXT")
  (:help :memobase
   "memobase: recall and write a self-hosted Memobase profile and event timeline - the memobase_search and memobase_profile tools, and (memobase:search \"...\")"
   *primer*))
