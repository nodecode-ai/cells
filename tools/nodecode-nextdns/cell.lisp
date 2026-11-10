;;;; cell.lisp --- the manual, the four boundaries, the section, the one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :nextdns) answers the primer below, and every request's help
;;;; section carries one line naming it. A person drives the same verbs from
;;;; eval in the NEXTDNS package, and /nextdns answers query totals in chat.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "nextdns": {"api_key_env": "NEXTDNS_API_KEY", "profile_id": "abc123"}

(in-package #:nodecode-nextdns)

;;; --- the manual ------------------------------------------------------------------

(defparameter +manual*
  "NextDNS is available through the nodecode-nextdns cell: the read-only side of a
NextDNS account, four tools a person can also call from eval in the nextdns package.
  nextdns_logs      {search, status, device, from, to, limit, cursor, profile}
      The DNS query log: timestamp, domain, status, the device that asked, the
      blocklists that matched and the protocol. For 'what did this device look
      up', 'did this domain resolve', 'what happened around time T'. raw is true
      by default, so every query is included, not only the navigational ones.
  nextdns_blocked   {search, device, from, to, limit, profile}
      The same log filtered to blocked queries, with the blocklist that matched
      each one: 'was this blocked, and by what'. Use it when something does not
      work and DNS blocking is a suspect.
  nextdns_analytics {metric, device, from, to, limit, profile}
      Aggregate counts, far cheaper than paging the log for a volume question:
      status, domains, reasons, ips, devices, protocols, queryTypes,
      ipVersions, dnssec, encryption, destinations.
  nextdns_profile   {profile}
      The profile's name, whether logging is enabled, its retention and storage
      region, and the active blocklists. With no profile configured it lists the
      profiles the key can see.
Every endpoint is a GET and nothing here changes the profile. An empty log is
usually logging switched off, or a retention shorter than the window asked
about: read nextdns_profile first. Devices that query NextDNS directly are
named; anything behind a router-level DNS setting reports as __UNIDENTIFIED__.
A device argument takes the name the log shows (it is resolved to the id the
API filters on) or an id. limit is raised to 10 and capped at 1000, and one
answer carries at most the section's max_rows rows. /nextdns answers the
status totals for the configured profile.
(help :nextdns) repeats this while the cell runs."
  "What (help :nextdns) answers while the cell runs.")

(defparameter +statuses+ '("default" "blocked" "allowed" "error")
  "The outcomes a log query filters on, as the API names them.")

(defparameter +metrics+ '("status" "domains" "reasons" "ips" "devices" "protocols"
                          "queryTypes" "ipVersions" "dnssec" "encryption" "destinations")
  "What the analytics endpoint aggregates.")

;;; --- the four boundaries ---------------------------------------------------------

(nlk:define-contract nextdns-logs
  (:doc "Query one NextDNS profile's DNS log. Use it for 'was this domain
resolved', 'what did device X look up', or 'what happened around time T'.
Returns the timestamp, domain, status, the device that asked, the blocklists
that matched and the protocol of each query. Devices that query NextDNS
directly are named; anything behind a router-level DNS setting reports as
unidentified. Use nextdns_blocked when the question is specifically about
blocking. Read-only: nothing is changed.")
  ("profile" :string :default ""
   :doc "the NextDNS profile id; omit for the section's profile_id")
  ("search" :string :default "" :slot search-term
   :doc "match domain names, e.g. plex.tv")
  ("status" :text :options '("default" "blocked" "allowed" "error")
   :doc "filter by the query's outcome")
  ("device" :string :default ""
   :doc "a device name, or __UNIDENTIFIED__ for unattributed queries")
  ("from" :string :default ""
   :doc "start of the window: ISO-8601, or a relative window like -1h")
  ("to" :string :default ""
   :doc "end of the window, ISO-8601")
  ("limit" :integer
   :doc "rows to return; raised to 10, capped at 1000")
  ("cursor" :string :default ""
   :doc "the cursor a previous answer returned, to read the next page")
  ("raw" :boolean :default t
   :doc "include every query rather than only navigational ones (default true)"))

(nlk:define-contract nextdns-blocked
  (:doc "Recently blocked DNS queries of one NextDNS profile, with the
blocklist that matched each one. Use it when something is not working and DNS
blocking is a suspect, or to answer 'why was this domain blocked'. It takes
the same filters as nextdns_logs. Read-only.")
  ("profile" :string :default ""
   :doc "the NextDNS profile id; omit for the section's profile_id")
  ("search" :string :default "" :slot search-term
   :doc "match domain names")
  ("device" :string :default ""
   :doc "a device name, or __UNIDENTIFIED__")
  ("from" :string :default ""
   :doc "start of the window")
  ("to" :string :default ""
   :doc "end of the window")
  ("limit" :integer
   :doc "rows to return; raised to 10, capped at 1000"))

(nlk:define-contract nextdns-analytics
  (:doc "Aggregate counts from one NextDNS profile rather than individual
queries. Use it for 'top blocked domains', 'which blocklist blocks the most',
'which devices are noisiest', or overall query and block totals. Far cheaper
than paging through the log for a volume question. Read-only.")
  ("profile" :string :default ""
   :doc "the NextDNS profile id; omit for the section's profile_id")
  ("metric" :text :options '("status" "domains" "reasons" "ips" "devices" "protocols"
                             "queryTypes" "ipVersions" "dnssec" "encryption" "destinations")
   :default "status"
   :doc "what to aggregate (default status)")
  ("device" :string :default ""
   :doc "limit the counts to one device")
  ("from" :string :default ""
   :doc "start of the window")
  ("to" :string :default ""
   :doc "end of the window")
  ("limit" :integer
   :doc "rows; raised to 10, capped at 500"))

(nlk:define-contract nextdns-profile
  (:doc "One NextDNS profile's settings: its name, whether logging is enabled,
its retention, its log storage region, and its active blocklists. Check it
first when a log query comes back empty, because logging can be off or the
retention shorter than the window asked about. With no profile configured it
lists the profiles the API key can see. Read-only.")
  ("profile" :string :default ""
   :doc "the NextDNS profile id; omit for the section's profile_id"))

;;; --- the section, resolved once ---------------------------------------------------

(defun read-settings (values table)
  "VALUES as the settings this cell runs on, with the key resolved once: the
environment variable api_key_env names first, a literal api_key second.

A start with no key refuses HERE rather than at the first tool call, so the
cells list, /settings and the model all say the one thing to fix."
  (declare (ignore table))
  (let* ((env (nlk:trimmed (or (getf values :api-key-env) "")))
         (literal (nlk:trimmed (or (getf values :api-key) "")))
         (from-env (and (plusp (length env)) (nle::credential-env env)))
         (key (or from-env (and (plusp (length literal)) literal))))
    (unless key
      (error 'nlk:config-refusal
             :detail (format nil "nextdns: no API key: put your NextDNS API key (my.nextdns.io/account, at the bottom of the page) in the environment variable ~a, or write it in api_key"
                             (if (plusp (length env)) env "NEXTDNS_API_KEY"))))
    (list* :api-key key
           :profile-id (nlk:trimmed (or (getf values :profile-id) ""))
           :base-url (string-right-trim "/" (nlk:trimmed (or (getf values :base-url) +base+)))
           values)))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell nextdns
  (:section ("nextdns")
    (:guide "put a NextDNS API key in the environment variable api_key_env names (NEXTDNS_API_KEY) and set profile_id to the id in your DNS-over-HTTPS URL; next step: ask what was blocked")
    ("api_key_env" :env :default "NEXTDNS_API_KEY"
     :doc "the environment variable holding the NextDNS API key (my.nextdns.io/account, at the bottom)")
    ("api_key" :secret
     :doc "a key written here instead of named in api_key_env")
    ("profile_id" :string :default ""
     :doc "the default profile id: the path segment of your DNS-over-HTTPS URL, dns.nextdns.io/<id>")
    ("base_url" :string :default "https://api.nextdns.io"
     :doc "the API's base; change it only for a proxy in front of the same API")
    ("user_agent" :string :default "nodecode-nextdns/0.1 (+https://nodecode.ai)"
     :doc "what the cell calls itself at NextDNS; a default library user agent is refused with a 403")
    ("timeout_seconds" :integer :default 20 :min 1
     :doc "one request's ceiling in seconds")
    ("max_rows" :integer :default 200 :min 10
     :doc "the most log rows one answer carries"))
  (:settings #'read-settings)
  (:help :nextdns
         "nextdns: DNS query logs, blocked queries with the matching blocklist, and analytics"
         +manual*)
  (:command "nextdns" 'run-command
            :description "Today's DNS query totals from NextDNS"
            :argument-hint "[PROFILE]")
  (:tool "nextdns_logs" (nlk:contract 'nextdns-logs) #'logs)
  (:tool "nextdns_blocked" (nlk:contract 'nextdns-blocked) #'blocked)
  (:tool "nextdns_analytics" (nlk:contract 'nextdns-analytics) #'analytics)
  (:tool "nextdns_profile" (nlk:contract 'nextdns-profile) #'profile-settings)
  (:start (lambda ()
            (reset-devices)
            (nle:on-stop (lambda () (setf *devices* nil))))))
