;;;; cell.lisp --- the primer, the section, the five tools, /tinyfish, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin gives a Hermes agent four things over TinyFish's
;;;; API: Search and Fetch behind the harness's own web tools, a cloud
;;;; browser, a goal-based web agent (tinyfish_agent) and saved website
;;;; sign-ins (the tinyfish_*_profile tools). Nodecode already has web
;;;; search and fetch of its own, so this cell keeps the parts that are
;;;; TinyFish's: the same search and fetch as verbs of their own, the web
;;;; agent, and the profiles it runs with.
;;;;
;;;; Five tools, the original's names kept where they mean the same thing:
;;;;   tinyfish_search          web results for a query (free)
;;;;   tinyfish_fetch           one or more pages as markdown (free)
;;;;   tinyfish_agent           a goal completed on a real site (credits)
;;;;   tinyfish_list_profiles   the saved sign-ins a run can use (free)
;;;;   tinyfish_create_profile  a new profile, and the link to sign in (free
;;;;                            until it is opened)
;;;; /tinyfish reports the key, the hosts and the credits left, and lists
;;;; the profiles. Nothing hooks the provider path: no model is served here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "tinyfish": {"api_key": "...", "search_url": ..., "max_results": 5,
;;;;                "agent_timeout_seconds": 300, "max_chars": 12000}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-tinyfish)

(defvar *last-error* nil
  "Why the last call that failed failed, for /tinyfish; NIL once one answers.")

(defparameter +primer+
  "TinyFish is a hosted web service: its Search and Fetch are free, and its web agent drives a
real browser on a site -- clicking, filling forms, reading several pages -- and answers the
result. These are the cell's tools:
  tinyfish_search          web results for one query: title, url and snippet each.
  tinyfish_fetch           the text of one or more pages, as markdown, with the format named.
  tinyfish_agent           a plain-language goal carried out on one URL, e.g. url
                           https://shop.example/search, goal 'Return the first 5 products and
                           prices as JSON'. A run spends TinyFish credits and can take minutes.
  tinyfish_list_profiles   the saved website sign-ins (Browser Context Profiles) a run can use.
  tinyfish_create_profile  a new profile for one site; it answers a link the operator opens to
                           sign in themselves -- never ask for a password.
Use search and fetch for ordinary web reading; reach for tinyfish_agent only for a site that
needs clicks, forms or a sign-in. Before a run that buys, posts, sends or changes an account,
ask the operator. A failed call answers ERROR: TINYFISH-ERROR with what the API said; a key
that is missing or refused says so. The operator's /tinyfish shows the key, the hosts and the
credits left."
  "What (help :tinyfish) answers while the cell runs.")

;;; --- the boundaries -----------------------------------------------------------

(nlk:define-contract search-request
  (:doc "Search the web with TinyFish. Answers the results a query returns, each with a title, a url and a snippet.")
  ("query" :text :required t :doc "what to search for")
  ("limit" :integer :doc "how many results to list; the section's max_results when absent"))

(nlk:define-contract fetch-request
  (:doc "Read one or more web pages with TinyFish Fetch: the page's own text, without the markup around it.")
  ("urls" :array :of :string :required t :doc "the pages to read, in order")
  ("format" :text :slot fetch-format :options '("markdown" "text" "json")
            :doc "markdown unless the caller needs plain text or the document tree"))

(nlk:define-contract agent-request
  (:doc "Run a TinyFish web agent on one site to complete a goal: navigate, click, fill forms and read across pages. Use it for multi-step site tasks that search and fetch cannot do. It spends TinyFish credits and can run for minutes.")
  ("url" :text :required t :doc "the page to start on")
  ("goal" :text :required t :doc "the task in plain language, including the data and shape to return")
  ("browser_profile" :text :options '("lite" "stealth")
                     :doc "stealth for a site with bot protection; lite otherwise")
  ("proxy_country_code" :text :doc "route the browser through this country, as a two-letter code (US, GB, DE)")
  ("profile_id" :text :doc "run signed in with this saved profile, from tinyfish_list_profiles"))

(nlk:define-contract create-profile-request
  (:doc "Create a TinyFish Browser Context Profile: a saved website sign-in that a later run reuses. It answers the link the operator opens to sign in to the site themselves.")
  ("name" :text :required t :doc "a name for it, e.g. 'Shop account', 1-100 characters")
  ("site" :text :doc "the site to sign in to, as a domain such as shop.example")
  ("set_as_default" :boolean :doc "make it the profile a run picks when none is named")
  ("proxy_country_code" :text :doc "a two-letter country code, for a site that blocks the sign-in page"))

;;; --- rendering what the API answered ------------------------------------------

(defun json-text (value)
  "VALUE as text: a string as itself, anything else as JSON."
  (typecase value
    (null "")
    (string value)
    (t (or (ignore-errors (nlk:encode-json-object value))
           (format nil "~a" value)))))

(defun field (object key)
  "KEY's value in the JSON OBJECT, or NIL when OBJECT is not an object."
  (and (hash-table-p object) (gethash key object)))

(defun array-field (object key)
  "KEY's array in the JSON OBJECT as a list, or NIL."
  (let ((value (field object key)))
    (when (vectorp value) (coerce value 'list))))

(defun string-field (object key)
  (let ((value (field object key)))
    (and (stringp value) value)))

(defun cut (text limit)
  "TEXT, cut at LIMIT characters with the number left out named."
  (if (<= (length text) limit)
      text
      (format nil "~a~%... [~d more characters]" (subseq text 0 limit) (- (length text) limit))))

(defun search-text (query &optional limit)
  "The web results QUERY returns, numbered; one sentence when there are none."
  (guarded
   (lambda ()
     (let* ((n (if (and limit (integerp limit) (plusp limit)) limit (setting :max-results)))
            (results (array-field (search-json query) "results")))
       (if (null results)
           (format nil "TinyFish Search found nothing for ~s." query)
           (with-output-to-string (out)
             (loop for item in (subseq results 0 (min n (length results)))
                   for index from 1
                   do (format out "~d. ~a~%   ~a~@[~%   ~a~]~%"
                              index
                              (or (string-field item "title") (string-field item "url") "")
                              (or (string-field item "url") "")
                              (string-field item "snippet")))))))))

(defun fetch-text (urls &optional format)
  "The pages URLS name, as text; a page that failed says so."
  (guarded
   (lambda ()
     (let* ((value (fetch-json (coerce urls 'list) :format (or format "markdown")))
            (results (array-field value "results"))
            (errors (array-field value "errors"))
            (text (with-output-to-string (out)
                    (dolist (item results)
                      (format out "## ~a~%~@[~a~%~]~a~%~%"
                              (or (string-field item "url") "")
                              (string-field item "title")
                              (json-text (field item "text"))))
                    (dolist (item errors)
                      (format out "## ~a~%ERROR: ~a~%~%"
                              (or (string-field item "url") "")
                              (or (string-field item "error") "fetch failed"))))))
       (if (plusp (length text))
           (cut text (setting :max-chars))
           (format nil "TinyFish Fetch returned nothing for ~{~a~^, ~}." urls))))))

(defun agent-text (url goal &key browser-profile proxy-country profile-id)
  "Start a run on URL for GOAL, wait for it, and answer what it found."
  (guarded
   (lambda ()
     (let* ((started (start-run url goal
                                 :browser-profile browser-profile
                                 :proxy-country proxy-country
                                 :profile-id profile-id))
            (run-id (string-field started "run_id")))
       (unless (and run-id (plusp (length run-id)))
         (fail "TinyFish Agent did not return a run_id"))
       (multiple-value-bind (run finished) (await-run run-id)
         (let ((text (with-output-to-string (out)
                       (format out "TinyFish Agent run ~a: ~a~%" run-id (run-status run))
                       (let ((error (field run "error")))
                         (when error (format out "ERROR: ~a~%" (json-text error))))
                       (let ((result (field run "result")))
                         (when result (format out "~a~%" (json-text result))))
                       (unless finished
                         (format out "Still running after ~d seconds; its run_id is ~a, ask about it again later.~%"
                                 (setting :agent-timeout-seconds) run-id)))))
           (cut text (setting :max-chars))))))))

(defun profiles-text ()
  "The saved sign-ins, one per line, with the sites each covers."
  (guarded
   (lambda ()
     (let ((profiles (array-field (profiles-json) "profiles")))
       (if (null profiles)
           "No TinyFish profiles yet: create one with tinyfish_create_profile, then sign in at the link it answers."
           (with-output-to-string (out)
             (dolist (profile profiles)
               (format out "- ~a (~a)~@[ [default]~]~@[ proxy ~a~]~%"
                       (or (string-field profile "name") (string-field profile "id") "")
                       (or (string-field profile "id") "")
                       (eq t (field profile "is_default"))
                       (string-field profile "proxy_country_code"))
               (let ((sites (array-field profile "signed_in_sites")))
                 (when sites
                   (format out "    signed in to: ~{~a~^, ~}~%"
                           (loop for site in sites
                                 collect (or (string-field site "site") "?"))))))))))))

(defun create-profile-text (name &key site set-as-default proxy-country)
  "Create a profile and answer the link the operator signs in at."
  (guarded
   (lambda ()
     (let ((fields (nlk:json-object "name" name)))
       (when (and proxy-country (plusp (length proxy-country))
                  (not (member (string-downcase proxy-country) '("none" "off" "direct")
                               :test #'equal)))
         (setf (gethash "proxy_country_code" fields) (string-upcase proxy-country)))
       (when (eq set-as-default t)
         (setf (gethash "set_as_default" fields) t))
       (let* ((profile (create-profile-json fields))
              (id (string-field profile "id")))
         (unless (and id (plusp (length id)))
           (fail "TinyFish Profiles did not return a profile id"))
         (format nil "Created profile ~a (~a).~%Sign in at ~a~%Open that link, sign in to the site and click Save profile; then run tinyfish_agent with profile_id ~a.~@[~%Proxy country: ~a.~]"
                 (or (string-field profile "name") name) id
                 (setup-url id site) id
                 (string-field profile "proxy_country_code")))))))

;;; --- what the model calls -----------------------------------------------------

(defun guarded (body)
  "Run BODY, answering its string; a refusal or a failed dial becomes the
answer ERROR: ..., so the model reads what went wrong and the transcript
draws the call failed."
  (handler-case
      (prog1 (funcall body) (setf *last-error* nil))
    (tinyfish-error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: TINYFISH-ERROR: ~a" condition)))
    (error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: TINYFISH-ERROR: ~a" condition)))))

(defun search-tool (call)
  (search-text (search-request-query call) (search-request-limit call)))

(defun fetch-tool (call)
  (fetch-text (fetch-request-urls call) (fetch-request-fetch-format call)))

(defun agent-tool (call)
  (agent-text (agent-request-url call) (agent-request-goal call)
              :browser-profile (agent-request-browser-profile call)
              :proxy-country (agent-request-proxy-country-code call)
              :profile-id (agent-request-profile-id call)))

(defun list-profiles-tool (arguments)
  (declare (ignore arguments))
  (profiles-text))

(defun create-profile-tool (call)
  (create-profile-text (create-profile-request-name call)
                       :site (create-profile-request-site call)
                       :set-as-default (create-profile-request-set-as-default call)
                       :proxy-country (create-profile-request-proxy-country-code call)))

;;; --- the operator's command ---------------------------------------------------

(defun wallet-text ()
  "The credits left, or one sentence when the account has no wallet yet."
  (let ((wallet (wallet-json)))
    (if wallet
        (format nil "~a ~a (as of ~a)~%"
                (or (string-field wallet "available_balance") "unavailable")
                (or (string-field wallet "currency") "")
                (or (string-field wallet "as_of") "unknown"))
        (format nil "no wallet for this account yet (legacy billing); see ~a~%" +key-url+))))

(defun status-text ()
  (with-output-to-string (out)
    (format out "TinyFish~%  API key: ~a~%  search: ~a~%  fetch:  ~a~%  agent:  ~a~%"
            (if (configured-p) "set"
                (format nil "missing: set tinyfish.api_key in /settings, or export ~a" +key-env+))
            (base :search) (base :fetch) (base :agent))
    (when (configured-p)
      (format out "  credits: ~a"
              (handler-case (wallet-text)
                (tinyfish-error (condition) (format nil "unavailable (~a)~%" condition))
                (error (condition) (format nil "unavailable (~a)~%" condition)))))
    (format out "Search and Fetch are free; a tinyfish_agent run spends credits.~%")))

(defun usage-text ()
  (guarded (lambda () (format nil "TinyFish credits~%  ~a" (wallet-text)))))

(defun run-command (args session-id)
  "One /tinyfish invocation: a status (the default), the credits, or the
saved profiles."
  (declare (ignore session-id))
  (let ((word (string-downcase (nlk:trimmed (or args "")))))
    (cond ((member word '("" "status") :test #'string=) (status-text))
          ((member word '("usage" "credits" "wallet") :test #'string=) (usage-text))
          ((member word '("profiles" "profile") :test #'string=) (profiles-text))
          (t (format nil "/tinyfish takes nothing for a status, or one of: usage, profiles; it read ~s"
                     (or args ""))))))

;;; --- the declaration ----------------------------------------------------------

(nle:define-cell tinyfish
  (:section ("tinyfish")
    (:guide "paste a TinyFish API key (free at agent.tinyfish.ai/api-keys) as api_key, or export TINYFISH_API_KEY; nothing else is needed to search and fetch, and a tinyfish_agent run spends the account's credits")
    ("api_key" :string
     :doc "TinyFish API key; TINYFISH_API_KEY is read when this is empty")
    ("search_url" :string :default "https://api.search.tinyfish.ai"
     :doc "the Search host; a proxy may name another")
    ("fetch_url" :string :default "https://api.fetch.tinyfish.ai"
     :doc "the Fetch host")
    ("agent_url" :string :default "https://agent.tinyfish.ai"
     :doc "the web agent and profiles host, which is also where a key is made")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one API call's ceiling")
    ("agent_timeout_seconds" :integer :default 300 :min 1
     :doc "how long tinyfish_agent waits on one run before answering its run_id")
    ("poll_seconds" :integer :default 3 :min 0
     :doc "how often it asks a running agent for its state")
    ("max_results" :integer :default 5 :min 1
     :doc "results one search lists when its call names no limit")
    ("max_chars" :integer :default 12000 :min 200
     :doc "how much of one fetched page, or one run's result, a tool answers"))
  (:help :tinyfish
         "tinyfish: search the web, fetch a page and run a goal on a real site through TinyFish - (help :tinyfish)"
         +primer+)
  (:tool "tinyfish_search" (nlk:contract 'search-request) #'search-tool)
  (:tool "tinyfish_fetch" (nlk:contract 'fetch-request) #'fetch-tool)
  (:tool "tinyfish_agent" (nlk:contract 'agent-request) #'agent-tool)
  (:tool "tinyfish_list_profiles" (no-arguments "List the saved TinyFish website sign-ins (Browser Context Profiles) a run can use, with the sites each covers.") #'list-profiles-tool)
  (:tool "tinyfish_create_profile" (nlk:contract 'create-profile-request) #'create-profile-tool)
  (:command "tinyfish" 'run-command
            :description "TinyFish: the key, the hosts and the credits left, or the saved sign-ins"
            :argument-hint "status | usage | profiles"
            :session nil))
