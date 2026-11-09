;;;; contracts.lisp --- the twelve tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-shodan)

(nlk:define-contract host-lookup
  (:doc "Full host intel from Shodan: open ports, products, versions, TLS certificates, hostnames, organisation, ASN, location, tags and known CVEs. Accepts an IP, a hostname, or a comma-separated list of up to 16. With no API key it answers from InternetDB, the free keyless dataset (ports, CPEs and CVE ids only). A free call: host lookups cost no query credits.")
  ("ip" :text :required t
   :doc "an IP (1.1.1.1), a hostname (one.one.one.one), or a comma-separated list of either")
  ("history" :boolean
   :doc "ask for the banner history as well; slower, and each entry is kept whole")
  ("verbosity" :string :default ""
   :doc "\"summary\" (the default), \"detail\" (adds certificates and headers) or \"raw\" (Shodan's own JSON, capped)"))

(nlk:define-contract host-search
  (:doc "Search Shodan's index and return matching hosts, one line of detail each. COSTS 1 QUERY CREDIT per filtered query and 1 more per extra page. Before searching, ask shodan_count with the same query and facets: it is free and answers most questions about scale and shape.")
  ("query" :text :required t
   :doc "the Shodan query, e.g. \"port:22 product:OpenSSH country:DE\"")
  ("page" :integer :default 1
   :doc "which page of results, from 1; each extra page costs another credit")
  ("limit" :integer :default 25
   :doc "how many hosts to return, 1 to 100")
  ("facets" :string :default ""
   :doc "optional facet summary, e.g. \"country:5,org:5\"")
  ("verbosity" :string :default ""
   :doc "\"summary\" (the default), \"detail\" or \"raw\""))

(nlk:define-contract host-count
  (:doc "How many hosts match a query, with optional facet breakdowns. FREE: /shodan/host/count costs no query credits. Use it first, and for any question about scale (\"how many exposed RDP servers are there?\") rather than shodan_search.")
  ("query" :text :required t
   :doc "the Shodan query, the same syntax shodan_search takes")
  ("facets" :string :default ""
   :doc "breakdowns to include, e.g. \"country:5,org:5,port:10,version:5\""))

(nlk:define-contract dns-lookup
  (:doc "Shodan's DNS data. mode=\"domain\" lists the subdomains and records Shodan has seen for a domain and COSTS 1 QUERY CREDIT; mode=\"resolve\" and \"reverse\" are free forward and reverse lookups.")
  ("mode" :text :default "domain"
   :doc "\"domain\" (subdomain and record enumeration, costs 1 credit), \"resolve\" (hostnames to IPs, free) or \"reverse\" (IPs to hostnames, free)")
  ("target" :text :required t
   :doc "the domain for mode=domain, or a comma-separated list of hostnames or IPs otherwise")
  ("page" :integer :default 1
   :doc "mode=domain: which page of records")
  ("record_type" :string :default ""
   :doc "mode=domain: only records of one type (A, AAAA, MX, NS, TXT, CNAME, SOA)"))

(nlk:define-contract cve-lookup
  (:doc "Vulnerability data from Shodan's CVEDB: CVSS, EPSS, CISA KEV status, ransomware campaigns, affected CPEs and references. FREE and needs NO API KEY. action=\"get\" one CVE by id; action=\"search\" CVEs by product or CPE; action=\"cpes\" the CPE names for a product.")
  ("action" :text :default "get"
   :doc "\"get\", \"search\" or \"cpes\"")
  ("cve" :string :default ""
   :doc "action=get: the CVE id, e.g. CVE-2021-44228")
  ("product" :string :default ""
   :doc "action=search or action=cpes: a product name, e.g. \"apache http_server\"")
  ("cpe23" :string :default ""
   :doc "action=search: an exact CPE 2.3 name instead of a product")
  ("kev_only" :boolean
   :doc "action=search: only vulnerabilities in CISA's known-exploited list")
  ("sort_by_epss" :boolean
   :doc "action=search: sort by EPSS (the probability of exploitation) rather than publication date")
  ("limit" :integer :default 20
   :doc "how many CVEs to return, 1 to 100"))

(nlk:define-contract account-info
  (:doc "The Shodan account behind the configured key: plan, query and scan credits left, monitored IPs, and the session ledger this cell keeps. FREE. Use it when a lookup refused for credits or plan, or when the model needs to know how much budget is left.")
  ("include_myip" :boolean
   :doc "also ask Shodan for the public IP this key is used from"))

(nlk:define-contract meta-info
  (:doc "Shodan's own reference data, FREE. action=\"validate_query\" checks a query's syntax before spending a credit on it; the others list the filters, facets, crawled ports and protocols a query or a scan may name.")
  ("action" :text :default "validate_query"
   :doc "\"validate_query\", \"filters\", \"facets\", \"ports\" or \"protocols\"")
  ("query" :string :default ""
   :doc "action=validate_query: the query to check"))

(nlk:define-contract scan-manage
  (:doc "On-demand scanning of addresses with Shodan, which spends SCAN credits (one per IP). OFF by default: the operator must set scan_enabled and usually a scan_allowlist beside it. action=\"submit\" starts a scan, \"status\" polls one, \"list\" shows recent ones. A scan is asynchronous: poll status, then read results with shodan_host once it reports DONE.")
  ("action" :text :default "status"
   :doc "\"submit\", \"status\" or \"list\"")
  ("scan_id" :string :default ""
   :doc "action=status: the id action=submit answered")
  ("targets" :string :default ""
   :doc "action=submit: a comma-separated list of IPs or CIDR ranges, inside the operator's scan_allowlist"))

(nlk:define-contract alert-manage
  (:doc "Shodan network monitors (alerts) on addresses the account watches: list them, read one, create one, delete one, and turn its triggers on or off. A new monitor fires nothing until a trigger is enabled. Changes outlive the conversation, so they cost the account nothing but are not free of consequence.")
  ("action" :text :default "list"
   :doc "\"list\", \"info\", \"create\", \"delete\", \"triggers\", \"enable_trigger\" or \"disable_trigger\"")
  ("alert_id" :string :default ""
   :doc "the monitor's id, for info, delete, enable_trigger and disable_trigger")
  ("name" :string :default ""
   :doc "action=create: a name for the monitor")
  ("ips" :string :default ""
   :doc "action=create: a comma-separated list of IPs or CIDR ranges to watch")
  ("trigger" :string :default ""
   :doc "action=enable_trigger or disable_trigger: the trigger's name, e.g. new_service")
  ("expires" :integer
   :doc "action=create: days until the monitor expires"))

(nlk:define-contract exploit-search
  (:doc "Search the exploits Shodan knows for a product, a CVE or a platform (exploits.shodan.io). Splunk and Hermes both count: this is where you find whether a vulnerable version has a public exploit. Cached, so a repeat costs nothing.")
  ("query" :text :required t
   :doc "what to look for, e.g. \"CVE-2021-44228\", \"apache\", \"windows smb\"")
  ("page" :integer :default 1
   :doc "which page of results")
  ("limit" :integer :default 15
   :doc "how many exploits to return, 1 to 50")
  ("facets" :string :default ""
   :doc "optional facet summary, e.g. \"platform:5,type:5\"")
  ("count_only" :boolean
   :doc "answer the total and the facets only, without the exploit rows"))

(nlk:define-contract query-directory
  (:doc "Shodan's community query directory: query strings other people saved, which are a good starting point for a dork you cannot phrase. FREE, and each answer says how to take it further. action=\"search\" finds saved queries by text, \"list\" browses them by votes, \"tags\" the popular tags.")
  ("action" :text :default "search"
   :doc "\"search\", \"list\" or \"tags\"")
  ("query" :string :default ""
   :doc "action=search: text to find in saved queries")
  ("page" :integer :default 1
   :doc "which page of saved queries")
  ("sort" :string :slot sort-order :default "votes"
   :doc "action=list: \"votes\" or \"timestamp\""))

(nlk:define-contract trend-history
  (:doc "How many hosts matched a query, month by month, from Shodan Trends. FREE. Use it for \"is this still growing?\" questions, not for a current total: shodan_count answers that.")
  ("query" :text :required t
   :doc "the Shodan query whose history to read")
  ("facets" :string :default ""
   :doc "optional facet summary, e.g. \"country:5\""))
