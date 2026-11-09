;;;; cell.lisp --- the twelve Shodan tools, /shodan, and the credit ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :shodan) answers the primer below, and every request's help
;;;; section carries one line naming it. A person drives the same verbs from
;;;; eval in the SHODAN package, and the /shodan command answers in chat.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "shodan": {"api_key_env": "SHODAN_API_KEY", "verbosity": "summary"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Rewritten from Adolanium's Hermes Agent plugin (see NOTICE): the twelve
;;;; tools and their argument schemas, the response shaping that keeps a host
;;;; inside the context window, the free-count-first budget discipline, the
;;;; scan and alert tools and their guards, and the slash command's dispatch.

(in-package #:nodecode-shodan)

;;; --- the manual -----------------------------------------------------------------

(defparameter *manual*
  "Shodan is available through the nodecode-shodan cell: twelve tools, which a person can
also call from eval in the shodan package.
  shodan_host      {ip}
      Full host intel: ports, products, versions, certificates, hostnames, org, ASN,
      geo, tags and CVEs. Takes an IP, a hostname or a comma list. FREE, and works
      with no API key (then it answers from InternetDB: ports, CPEs and CVE ids).
  shodan_count     {query, facets}
      How many hosts match, with facet breakdowns. FREE, no credits at all. Ask this
      BEFORE anything that spends: use facets country:10,org:10,port:10,version:5.
  shodan_search    {query, page, limit, facets}
      Matching hosts, one line each. COSTS 1 QUERY CREDIT when the query has a filter
      (a colon), plus 1 per page past the first.
  shodan_dns       {mode, target, page, record_type}
      mode=\"domain\" enumerates subdomains and records (1 credit); \"resolve\" and
      \"reverse\" are free hostname/IP conversions.
  shodan_cve       {action, cve, product, cpe23, kev_only, sort_by_epss, limit}
      CVEDB: CVSS, EPSS, CISA KEV, ransomware, CPEs. FREE, no key needed.
  shodan_account   {}
      Plan, credits left, and this session's ledger. FREE.
  shodan_meta      {action, query}
      FREE: validate a query's syntax (\"validate_query\"), or list filters, facets,
      crawled ports and protocols.
  shodan_scan      {action, scan_id, targets}
      On-demand scans. OFF unless the operator set scan_enabled; spends SCAN credits,
      one per IP. Asynchronous: submit, poll status, then shodan_host the targets.
  shodan_alert     {action, alert_id, name, ips, trigger, expires}
      Network monitors on the account: list, info, create, delete, triggers,
      enable_trigger, disable_trigger.
  shodan_exploits  {query, page, limit, facets, count_only}
      Exploit search (exploits.shodan.io). Cached.
  shodan_query     {action, query, page, sort}
      Shodan's community directory of saved query strings.
  shodan_trends    {query, facets}
      Match counts month by month for a query.
Most Shodan questions have a free answer: shodan_count takes the same query syntax
and the same facets as shodan_search and costs nothing, and host lookups are free.
Count first, search only for the hosts you actually need to see. Every credit-costing
answer carries the session ledger, and a call that would cross the budget refuses
instead of spending."
  "What (help :shodan) answers while the cell runs.")

;;; --- answers and refusals ---------------------------------------------------------

(defmacro answering (&body body)
  "BODY's text; every refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (shodan-error (condition)
       (nle:failure (format nil "ERROR: ~a" (shodan-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Shodan: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun reply (object)
  "OBJECT as the tool's text, fitted to the configured character budget."
  (nlk:encode-json-object (fit object (setting :max-result-chars))))

(defun verbosity-of (asked)
  "The per-call verbosity when it is one of the three, else the configured one."
  (let ((asked (string-downcase (nlk:trimmed (or asked "")))))
    (if (member asked '("summary" "detail" "raw") :test #'string=)
        asked
        (setting :verbosity))))

(defun split-list (text &key (limit 64))
  "TEXT's comma-separated items, trimmed and non-empty."
  (let ((items (loop for item in (uiop:split-string (or text "") :separator ",")
                      for clean = (nlk:trimmed item)
                      when (plusp (length clean)) collect clean)))
    (subseq items 0 (min limit (length items)))))

(defun looks-like-ip (text)
  (let ((text (nlk:trimmed (or text ""))))
    (and (plusp (length text))
         (every (lambda (character) (or (digit-char-p character) (find character ".:")))
                text)
         (or (find #\. text) (find #\: text)))))

(defun search-cost (query page)
  "Shodan's rule, mirrored: a filter (a colon) costs one credit, plus one per
page past the first; a bare keyword search on page one is free."
  (let ((filtered (find #\: (or query "") )))
    (+ (if filtered 1 0) (max 0 (1- page)))))

;;; --- shodan_host -------------------------------------------------------------------

(defun internetdb-answer (ip)
  "The free dataset's answer for IP, or NIL when it has no record."
  (multiple-value-bind (json status) (get-json (format nil "/~a" ip) :base +internetdb+ :cacheable t)
    (when (and (integerp status) (< status 300) (hash-table-p json))
      (let ((shaped (shape-host json (verbosity-of ""))))
        (put shaped "source" "internetdb")
        (put shaped "note"
             "From the free keyless InternetDB dataset: ports, CPEs, tags and CVE ids only, refreshed weekly. Set api_key_env for banners, products, versions and certificates.")
        shaped))))

(defun shodan-host-answer (ip verbosity history)
  "IP's host record from the REST API, or the keyless one when the account
cannot serve it. => (values SHAPED SOURCE)."
  (if (api-key)
      (multiple-value-bind (json status)
          (get-json (format nil "/shodan/host/~a" ip)
                    :params (when history (list (cons "history" "True")))
                    :cacheable (not history))
        (cond ((and (integerp status) (< status 300) (hash-table-p json))
               (values (shape-host json verbosity) "shodan"))
              ((eql status 404) (values nil "shodan"))
              ((member status '(401 402 403))
               (if (setting :internetdb-fallback)
                   (values (internetdb-answer ip) "internetdb")
                   (fail "Shodan answered ~a for ~a: ~a" status ip (refusal-text status json nil))))
              (t (fail "Shodan answered ~a for ~a: ~a" status ip (refusal-text status json nil)))))
      (values (internetdb-answer ip) "internetdb")))

(defun resolve-hostnames (names)
  "Shodan's free resolver, hostname -> address, for the names that are not
addresses. A partial map on partial failure."
  (let ((table (make-hash-table :test 'equal)))
    (when (and names (api-key))
      (multiple-value-bind (json status)
          (get-json "/dns/resolve" :params (list (cons "hostnames" (format nil "~{~a~^,~}" names)))
                    :cacheable t)
        (when (and (integerp status) (< status 300) (hash-table-p json))
          (loop for name being the hash-keys of json using (hash-value address)
                when (stringp address)
                  do (setf (gethash name table) address)))))
    table))

(defun host (call)
  "The shodan_host tool: host intel, shaped, with the keyless fallback."
  (answering
    (let* ((targets (split-list (host-lookup-ip call) :limit 16))
           (verbosity (verbosity-of (host-lookup-verbosity call)))
           (history (host-lookup-history call)))
      (unless targets
        (return-from host (nle:failure "ERROR: no IP or hostname given.")))
      (let* ((resolved (resolve-hostnames (remove-if #'looks-like-ip targets)))
             (queue '()) (unresolved '())
             (hosts '()) (not-found '()) (sources '()))
        (dolist (target targets)
          (cond ((looks-like-ip target) (push (cons target nil) queue))
                (t (let ((address (gethash target resolved)))
                     (if address
                         (push (cons address target) queue)
                         (push target unresolved))))))
        (setf queue (nreverse queue) unresolved (nreverse unresolved))
        (dolist (pair queue)
          (multiple-value-bind (shaped source)
              (shodan-host-answer (car pair) verbosity history)
            (push source sources)
            (cond (shaped
                   (when (cdr pair) (put shaped "queried_as" (cdr pair)))
                   (push shaped hosts))
                  (t (push (or (cdr pair) (car pair)) not-found)))))
        (let ((payload (nlk:json-object)))
          (if (and (= 1 (length hosts)) (null not-found) (null unresolved))
              (setf (gethash "host" payload) (first hosts))
              (setf (gethash "hosts" payload) (vec (nreverse hosts))
                    (gethash "found" payload) (length hosts)))
          (when not-found
            (setf (gethash "no_data_for" payload) (vec (nreverse not-found))
                  (gethash "note" payload)
                  "Shodan has no record for these. That usually means no exposed services were seen in the last scan window, not that the host is offline."))
          (when unresolved
            (setf (gethash "unresolved_hostnames" payload) (vec unresolved)))
          (when (every (lambda (source) (equal source "internetdb")) sources)
            (setf (gethash "degraded" payload)
                  "Answered from the free keyless InternetDB dataset: ports, CPEs, tags and CVE ids only. Set an API key for banners, products, versions and certificates."))
          (reply payload))))))

;;; --- shodan_search, shodan_count ----------------------------------------------------

(defun search-hosts (call)
  "The shodan_search tool: matching hosts, one line each, for a credit."
  (answering
    (let* ((query (nlk:trimmed (host-search-query call)))
           (page (max 1 (or (host-search-page call) 1)))
           (limit (max 1 (min 100 (or (host-search-limit call) 25))))
           (facets (nlk:trimmed (host-search-facets call)))
           (verbosity (verbosity-of (host-search-verbosity call))))
      (unless (plusp (length query))
        (return-from search-hosts (nle:failure "ERROR: no query given.")))
      (let ((cost (search-cost query page)))
        (spend cost :query)
        (multiple-value-bind (json status)
            (get-json "/shodan/host/search"
                      :params (append (list (cons "query" query) (cons "page" page))
                                      (when (plusp (length facets)) (list (cons "facets" facets)))
                                      (list (cons "minify" (if (string-equal verbosity "summary") "True" "False")))))
          (let ((raw (answer json status "/shodan/host/search")))
            (when (and (vectorp (field raw "matches")) (> (length (field raw "matches")) limit))
              (setf (gethash "matches" raw)
                    (subseq (field raw "matches") 0 limit)))
            (let ((shaped (shape-search raw :query query :page page
                                            :verbosity verbosity :total (num-of raw "total"))))
              (when (< (* page limit) (or (num-of raw "total") 0))
                (put shaped "pagination_hint"
                     "More results exist. Each additional page costs another query credit. If you only need aggregates, use shodan_count with facets instead, which is free."))
              (put shaped "credits" (ledger-object))
              (reply shaped))))))))

(defun count-hosts (call)
  "The shodan_count tool: totals and facets, free."
  (answering
    (let* ((query (nlk:trimmed (host-count-query call)))
           (facets (nlk:trimmed (host-count-facets call))))
      (unless (plusp (length query))
        (return-from count-hosts (nle:failure "ERROR: no query given.")))
      (multiple-value-bind (json status)
          (get-json "/shodan/host/count"
                    :params (append (list (cons "query" query))
                                    (when (plusp (length facets)) (list (cons "facets" facets)))))
        (let ((raw (answer json status "/shodan/host/count")))
          (reply (clean-object
                  `(("query" . ,query)
                    ("total" . ,(num-of raw "total"))
                    ("facets" . ,(shape-facets (field raw "facets")))
                    ("cost" . ,(format nil "free. this lookup spent ~a of the session's ~a query credits"
                                       (ledger-queries *ledger*) (setting :query-credits)))))))))))

;;; --- shodan_dns ----------------------------------------------------------------------

(defun dns (call)
  "The shodan_dns tool: domain enumeration (1 credit), free forward and reverse."
  (answering
    (let* ((mode (string-downcase (nlk:trimmed (or (dns-lookup-mode call) "domain"))))
           (target (nlk:trimmed (dns-lookup-target call))))
      (unless (plusp (length target))
        (return-from dns (nle:failure "ERROR: no target given.")))
      (cond
        ((string= mode "resolve")
         (multiple-value-bind (json status)
             (get-json "/dns/resolve" :params (list (cons "hostnames" (format nil "~{~a~^,~}" (split-list target))))
                       :cacheable t)
           (reply (clean-object `(("mode" . "resolve")
                                  ("resolved" . ,(answer json status "/dns/resolve"))
                                  ("cost" . "free"))))))
        ((string= mode "reverse")
         (multiple-value-bind (json status)
             (get-json "/dns/reverse" :params (list (cons "ips" (format nil "~{~a~^,~}" (split-list target))))
                       :cacheable t)
           (reply (clean-object `(("mode" . "reverse")
                                  ("hostnames" . ,(answer json status "/dns/reverse"))
                                  ("cost" . "free"))))))
        ((string= mode "domain")
         (spend 1 :query)
         (let ((page (max 1 (or (dns-lookup-page call) 1)))
               (record-type (string-upcase (nlk:trimmed (dns-lookup-record-type call)))))
           (multiple-value-bind (json status)
               (get-json (format nil "/dns/domain/~a" (pct-encode target))
                         :params (append (list (cons "page" page))
                                         (when (plusp (length record-type))
                                           (list (cons "type" record-type)))))
             (let ((raw (answer json status "/dns/domain")))
               (reply (clean-object
                       `(("mode" . "domain")
                         ("domain" . ,(or (text-of raw "domain") target))
                         ("tags" . ,(vec (array-of raw "tags")))
                         ("subdomain_count" . ,(length (array-of raw "subdomains")))
                         ("subdomains" . ,(vec (array-of raw "subdomains")))
                         ("records" . ,(vec (loop for row in (array-of raw "data")
                                                   when (hash-table-p row)
                                                     collect (clean-object
                                                              `(("subdomain" . ,(text-of row "subdomain"))
                                                                ("type" . ,(text-of row "type"))
                                                                ("value" . ,(text-of row "value"))
                                                                ("last_seen" . ,(text-of row "last_seen")))))))
                         ("more_pages" . ,(and (field raw "more") t))
                         ("credits" . ,(ledger-object)))))))))
        (t (nle:failure (format nil "ERROR: unknown mode ~s. Use \"domain\", \"resolve\" or \"reverse\"." mode)))))))

;;; --- shodan_cve, keyless ---------------------------------------------------------------

(defun shape-cve (raw)
  (clean-object
   `(("cve" . ,(text-of raw "cve_id"))
     ("summary" . ,(clip (text-of raw "summary") 400))
     ("cvss" . ,(num-of raw "cvss"))
     ("cvss_version" . ,(text-of raw "cvss_version"))
     ("cvss_v3" . ,(num-of raw "cvss_v3"))
     ("cvss_v4" . ,(num-of raw "cvss_v4"))
     ("epss" . ,(num-of raw "epss"))
     ("epss_percentile" . ,(num-of raw "ranking_epss"))
     ("known_exploited" . ,(field raw "kev"))
     ("ransomware_campaign" . ,(text-of raw "ransomware_campaign"))
     ("recommended_action" . ,(text-of raw "propose_action"))
     ("published" . ,(text-of raw "published_time"))
     ("affected_cpes" . ,(vec (subseq (array-of raw "cpes") 0 (min 20 (length (array-of raw "cpes"))))))
     ("references" . ,(vec (subseq (array-of raw "references") 0 (min 8 (length (array-of raw "references")))))))))

(defun cve (call)
  "The shodan_cve tool: CVEDB, no key needed."
  (answering
    (let* ((action (string-downcase (nlk:trimmed (or (cve-lookup-action call) "get"))))
           (limit (max 1 (min 100 (or (cve-lookup-limit call) 20)))))
      (cond
        ((string= action "get")
         (let ((id (string-upcase (nlk:trimmed (cve-lookup-cve call)))))
           (unless (plusp (length id))
             (return-from cve (nle:failure "ERROR: action=\"get\" needs a cve id.")))
           (multiple-value-bind (json status)
               (get-json (format nil "/cve/~a" (pct-encode id)) :base +cvedb+ :cacheable t)
             (reply (clean-object `(("source" . "cvedb")
                                    ("cve" . ,(shape-cve (answer json status "/cve")))))))))
        ((string= action "cpes")
         (let ((product (nlk:trimmed (cve-lookup-product call))))
           (unless (plusp (length product))
             (return-from cve (nle:failure "ERROR: action=\"cpes\" needs a product name.")))
           (multiple-value-bind (json status)
               (get-json "/cpes" :base +cvedb+ :params (list (cons "product" product)) :cacheable t)
             (let ((rows (array-of (answer json status "/cpes") "cpes")))
               (reply (clean-object `(("source" . "cvedb")
                                      ("product" . ,product)
                                      ("cpes" . ,(vec (subseq rows 0 (min limit (length rows))))))))))))
        ((string= action "search")
         (let ((product (nlk:trimmed (cve-lookup-product call)))
               (cpe23 (nlk:trimmed (cve-lookup-cpe23 call))))
           (when (and (not (plusp (length product))) (not (plusp (length cpe23))))
             (return-from cve (nle:failure "ERROR: action=\"search\" needs either product or cpe23.")))
           (multiple-value-bind (json status)
               (get-json "/cves" :base +cvedb+
                         :params (append (list (cons "limit" limit))
                                         (if (plusp (length cpe23))
                                             (list (cons "cpe23" cpe23))
                                             (list (cons "product" product)))
                                         (when (cve-lookup-kev-only call) (list (cons "is_kev" "true")))
                                         (when (cve-lookup-sort-by-epss call) (list (cons "sort_by_epss" "true"))))
                         :cacheable t)
             (let* ((raw (answer json status "/cves"))
                    (rows (or (array-of raw "cves") (and (vectorp raw) (coerce raw 'list))))
                    (shaped (loop for row in (subseq rows 0 (min limit (length rows)))
                                   when (hash-table-p row) collect (shape-cve row))))
               (reply (clean-object `(("source" . "cvedb")
                                      ("count" . ,(length shaped))
                                      ("cves" . ,(vec shaped)))))))))
        (t (nle:failure (format nil "ERROR: unknown action ~s. Use \"get\", \"search\" or \"cpes\"." action)))))))

;;; --- shodan_account, shodan_meta ---------------------------------------------------------

(defun account (call)
  "The shodan_account tool: plan, credits and the session ledger."
  (answering
    (let ((payload (nlk:json-object)))
      (setf (gethash "api_key_configured" payload) (and (api-key) t)
            (gethash "api_key_env" payload) (setting :api-key-env)
            (gethash "verbosity" payload) (setting :verbosity)
            (gethash "scanning_enabled" payload) (and (setting :scan-enabled) t)
            (gethash "session_budget" payload) (ledger-object))
      (unless (api-key)
        (setf (gethash "note" payload)
              "No API key configured, so only the keyless endpoints work: shodan_cve, and shodan_host through the InternetDB fallback.")
        (return-from account (reply payload)))
      (multiple-value-bind (json status) (get-json "/api-info" :cacheable t)
        (let* ((raw (answer json status "/api-info"))
               (limits (field raw "usage_limits"))
               (info (nlk:json-object)))
          (setf (gethash "plan" info) (text-of raw "plan")
                (gethash "query_credits" info) (num-of raw "query_credits")
                (gethash "scan_credits" info) (num-of raw "scan_credits")
                (gethash "monitored_ips" info) (num-of raw "monitored_ips")
                (gethash "monthly_query_credits" info) (num-of limits "query_credits")
                (gethash "monthly_scan_credits" info) (num-of limits "scan_credits")
                (gethash "monitored_ip_allowance" info) (num-of limits "monitored_ips"))
          (multiple-value-bind (profile status) (get-json "/account/profile" :cacheable t)
            (when (and (integerp status) (< status 300) (hash-table-p profile))
              (setf (gethash "display_name" info) (text-of profile "display_name")
                    (gethash "member" info) (field profile "member")
                    (gethash "created" info) (text-of profile "created"))))
          (when (account-info-include-myip call)
            (multiple-value-bind (myip status) (get-json "/tools/myip" :cacheable t)
              (when (and (integerp status) (< status 300))
                (setf (gethash "my_public_ip" payload) (if (hash-table-p myip)
                                                            (or (text-of myip "ip") myip)
                                                            myip)))))
          (setf (gethash "account" payload) info
                (gethash "cache_entries" payload) (and *cache* (hash-table-count *cache*)))
          (reply payload))))))

(defun meta (call)
  "The shodan_meta tool: validate a query, or list Shodan's own reference data."
  (answering
    (let ((action (string-downcase (nlk:trimmed (or (meta-info-action call) "validate_query")))))
      (cond
        ((string= action "validate_query")
         (let ((query (nlk:trimmed (meta-info-query call))))
           (unless (plusp (length query))
             (return-from meta (nle:failure "ERROR: action=\"validate_query\" needs a query.")))
           (multiple-value-bind (json status)
               (get-json "/shodan/host/search/tokens" :params (list (cons "query" query)))
             (let* ((raw (answer json status "/shodan/host/search/tokens"))
                    (errors (array-of raw "errors")))
               (reply (clean-object
                       `(("query" . ,query)
                         ("valid" . ,(null errors))
                         ("errors" . ,(vec errors))
                         ("filters_used" . ,(field raw "filters"))
                         ("free_text" . ,(text-of raw "string"))
                         ("cost" . "free. validating does not run the search")
                         ("note" . ,(if errors
                                        nil
                                        "Valid syntax does not mean the plan can run it: \"vuln:\" needs Small Business or above and \"tag:\" needs Corporate or above.")))))))))
        (t
         (let ((path (cdr (assoc action '(("filters" . "/shodan/host/search/filters")
                                          ("facets" . "/shodan/host/search/facets")
                                          ("ports" . "/shodan/ports")
                                          ("protocols" . "/shodan/protocols"))
                                  :test #'string=))))
           (unless path
             (return-from meta (nle:failure (format nil "ERROR: unknown action ~s. Use validate_query, filters, facets, ports or protocols." action))))
           (multiple-value-bind (json status) (get-json path :cacheable t)
             (let ((raw (answer json status path)))
               (cond
                 ((string= action "protocols")
                  (let ((names (sort (if (hash-table-p raw)
                                         (loop for key being the hash-keys of raw collect key)
                                         (coerce raw 'list))
                                     #'string<)))
                    (reply (nlk:json-object "action" action "count" (length names)
                                             "protocols" (vec names)))))
                 ((string= action "ports")
                  (let ((ports (sort (if (vectorp raw) (coerce raw 'list) raw) #'<)))
                    (reply (nlk:json-object "action" action "count" (length ports)
                                             "note" "Shodan crawls these ports; anything else will not be in the index."
                                             "ports" (vec ports)))))
                 (t (let ((object (nlk:json-object "action" action
                                                    "count" (length (coerce raw 'list)))))
                      (setf (gethash action object)
                            (vec (sort (coerce raw 'list) #'string<)))
                      (reply object))))))))))))

;;; --- shodan_scan, shodan_alert -------------------------------------------------------

(defun cidr-size (target)
  "One scan credit per address: a CIDR's own size, else one."
  (let ((slash (position #\/ target)))
    (if slash
        (let ((bits (ignore-errors (parse-integer target :start (1+ slash)))))
          (if (and bits (<= 0 bits 32)) (expt 2 (- 32 bits)) 1))
        1)))

(defun in-allowlist-p (target allowlist)
  "Whether TARGET is inside one of the operator's permitted ranges. An empty
allowlist permits everything: scan_enabled is the real gate."
  (or (null allowlist)
      (some (lambda (entry)
              (or (string= entry target)
                  (let ((slash (position #\/ target)))
                    (and slash
                         (let ((network (ignore-errors (parse-integer target :start (1+ slash))))
                               (address (subseq target 0 slash)))
                           (and network
                                (uiop:string-prefix-p address entry)
                                (let ((entry-network
                                        (ignore-errors
                                         (parse-integer entry :start
                                                        (1+ (or (position #\/ entry) -1))))))
                                  (and entry-network
                                       (>= network entry-network)))))))))
            allowlist)))

(defun scan (call)
  "The shodan_scan tool: submit, status and list, with the operator's gates."
  (answering
    (let ((action (string-downcase (nlk:trimmed (or (scan-manage-action call) "list")))))
      (cond
        ((string= action "list")
         (multiple-value-bind (json status) (get-json "/shodan/scans")
           (let ((raw (answer json status "/shodan/scans")))
             (reply (nlk:json-object
                     "action" "list" "total" (or (num-of raw "total") 0)
                     "scans" (vec (loop for row in (array-of raw "matches")
                                         when (hash-table-p row)
                                           collect (clean-object
                                                    `(("id" . ,(text-of row "id"))
                                                      ("status" . ,(text-of row "status"))
                                                      ("created" . ,(text-of row "created"))
                                                      ("size" . ,(num-of row "size"))
                                                      ("credits_left" . ,(num-of row "credits_left")))))))))))
        ((string= action "status")
         (let ((id (nlk:trimmed (scan-manage-scan-id call))))
           (unless (plusp (length id))
             (return-from scan (nle:failure "ERROR: action=\"status\" needs a scan_id.")))
           (multiple-value-bind (json status)
               (get-json (format nil "/shodan/scan/~a" (pct-encode id)))
             (let* ((raw (answer json status "/shodan/scan"))
                    (state (text-of raw "status")))
               (reply (nlk:json-object
                       "action" "status" "scan_id" id "status" state
                       "created" (text-of raw "created")
                       "next_step" (if (and state (string-equal state "DONE"))
                                       "Scan finished. Read the results with shodan_host on the targets you submitted."
                                       "Still running. Scans take minutes: check again later rather than resubmitting.")))))))
        ((string= action "submit")
         (unless (setting :scan-enabled)
           (fail "on-demand scanning is off. Set \"scan_enabled\": true (and usually \"scan_allowlist\") in the shodan section, then (restart-cells \"nodecode-shodan\")."))
         (let* ((targets (split-list (scan-manage-targets call)))
                (allowlist (setting :scan-allowlist)))
           (unless targets
             (return-from scan (nle:failure "ERROR: action=\"submit\" needs targets.")))
           (let ((blocked (remove-if (lambda (target) (in-allowlist-p target allowlist)) targets)))
             (when blocked
               (fail "these targets are outside the configured scan_allowlist: ~{~a~^, ~}. The allowlist is ~{~a~^, ~}."
                     blocked allowlist))
             (spend (reduce #'+ targets :key #'cidr-size) :scan)
             (multiple-value-bind (json status)
                 (send-json :post "/shodan/scan" :params (list (cons "ips" (format nil "~{~a~^,~}" targets))))
               (let ((raw (answer json status "/shodan/scan")))
                 (reply (nlk:json-object
                         "action" "submit" "scan_id" (text-of raw "id")
                         "hosts_queued" (num-of raw "count")
                         "credits_left" (num-of raw "credits_left")
                         "targets" (vec targets)
                         "next_step" "Scanning is asynchronous. Poll with shodan_scan(action=\"status\", scan_id=...) and read results with shodan_host once it reports DONE."
                         "credits" (ledger-object))))))))
        (t (nle:failure (format nil "ERROR: unknown action ~s. Use \"submit\", \"status\" or \"list\"." action)))))))

(defun shape-alert (alert)
  (clean-object
   `(("id" . ,(text-of alert "id"))
     ("name" . ,(text-of alert "name"))
     ("created" . ,(text-of alert "created"))
     ("expires" . ,(or (text-of alert "expiration") (text-of alert "expires")))
     ("size" . ,(num-of alert "size"))
     ("watching" . ,(vec (array-of (field alert "filters") "ip")))
     ("triggers" . ,(vec (sort (if (hash-table-p (field alert "triggers"))
                                   (loop for key being the hash-keys of (field alert "triggers") collect key)
                                   (array-of alert "triggers"))
                               #'string<))))))

(defun alert-rows (raw)
  "Shodan answers /shodan/alert/info with an array; every other read here
wraps it in an object under a key. Both shapes read the same way."
  (if (vectorp raw) (coerce raw 'list) (array-of raw "matches")))

(defun alerts (call)
  "The shodan_alert tool: the account's network monitors."
  (answering
    (let ((action (string-downcase (nlk:trimmed (or (alert-manage-action call) "list"))))
          (id (nlk:trimmed (alert-manage-alert-id call))))
      (cond
        ((string= action "list")
         (multiple-value-bind (json status) (get-json "/shodan/alert/info")
           (let* ((raw (answer json status "/shodan/alert/info"))
                  (rows (loop for row in (alert-rows raw)
                              when (hash-table-p row) collect (shape-alert row))))
             (reply (nlk:json-object "action" "list" "count" (length rows) "alerts" (vec rows))))))
        ((string= action "triggers")
         (multiple-value-bind (json status) (get-json "/shodan/alert/triggers" :cacheable t)
           (let* ((raw (answer json status "/shodan/alert/triggers"))
                  (rows (loop for row in (alert-rows raw)
                              when (hash-table-p row)
                                collect (clean-object `(("name" . ,(text-of row "name"))
                                                        ("description" . ,(clip (text-of row "description") 160)))))))
             (reply (nlk:json-object "action" "triggers" "triggers" (vec rows))))))
        ((string= action "info")
         (unless (plusp (length id))
           (return-from alerts (nle:failure "ERROR: action=\"info\" needs an alert_id.")))
         (multiple-value-bind (json status)
             (get-json (format nil "/shodan/alert/~a/info" (pct-encode id)))
           (reply (nlk:json-object "action" "info"
                                   "alert" (shape-alert (answer json status "/shodan/alert/info"))))))
        ((string= action "create")
         (let ((name (nlk:trimmed (alert-manage-name call)))
               (ips (split-list (alert-manage-ips call)))
               (expires (alert-manage-expires call)))
           (when (or (not (plusp (length name))) (null ips))
             (return-from alerts (nle:failure "ERROR: action=\"create\" needs both name and ips.")))
           (let ((body (nlk:json-object "name" name)))
             (setf (gethash "filters" body) (nlk:json-object "ip" (vec ips)))
             (when (and (integerp expires) (plusp expires))
               (setf (gethash "expires" body) expires))
             (multiple-value-bind (json status) (send-json :post "/shodan/alert" :json body)
               (reply (nlk:json-object
                       "action" "create"
                       "alert" (shape-alert (answer json status "/shodan/alert"))
                       "next_step" "The monitor exists but has no triggers yet, so it will not fire. Attach one with shodan_alert(action=\"enable_trigger\", alert_id=..., trigger=\"new_service\")."))))))
        ((string= action "delete")
         (unless (plusp (length id))
           (return-from alerts (nle:failure "ERROR: action=\"delete\" needs an alert_id.")))
         (multiple-value-bind (json status) (send-json :delete (format nil "/shodan/alert/~a" (pct-encode id)))
           (declare (ignore json))
           (if (and (integerp status) (< status 300))
               (reply (nlk:json-object "action" "delete" "alert_id" id "deleted" t))
               (fail "Shodan answered ~a deleting monitor ~a." status id))))
        ((member action '("enable_trigger" "disable_trigger") :test #'string=)
         (let ((trigger (nlk:trimmed (alert-manage-trigger call))))
           (when (or (not (plusp (length id))) (not (plusp (length trigger))))
             (return-from alerts (nle:failure (format nil "ERROR: action=~s needs both alert_id and trigger." action))))
           (multiple-value-bind (json status)
               (send-json (if (string= action "enable_trigger") :put :delete)
                          (format nil "/shodan/alert/~a/trigger/~a" (pct-encode id) (pct-encode trigger)))
             (declare (ignore json))
             (if (and (integerp status) (< status 300))
                 (reply (nlk:json-object "action" action "alert_id" id "trigger" trigger "applied" t))
                 (fail "Shodan answered ~a on monitor ~a trigger ~a." status id trigger)))))
        (t (nle:failure (format nil "ERROR: unknown action ~s." action)))))))

;;; --- shodan_exploits, shodan_query, shodan_trends ---------------------------------------

(defun exploits (call)
  "The shodan_exploits tool: whether a vulnerable version has a public exploit."
  (answering
    (let* ((query (nlk:trimmed (exploit-search-query call)))
           (facets (nlk:trimmed (exploit-search-facets call))))
      (unless (plusp (length query))
        (return-from exploits (nle:failure "ERROR: no query given.")))
      (let ((params (append (list (cons "query" query))
                            (when (plusp (length facets)) (list (cons "facets" facets))))))
        (if (exploit-search-count-only call)
            (multiple-value-bind (json status) (get-json "/count" :base +exploits+ :params params :cacheable t)
              (let ((raw (answer json status "/count")))
                (reply (clean-object `(("query" . ,query)
                                       ("total" . ,(num-of raw "total"))
                                       ("facets" . ,(shape-facets (field raw "facets")))
                                       ("source" . "exploits"))))))
            (let ((limit (max 1 (min 50 (or (exploit-search-limit call) 15)))))
              (multiple-value-bind (json status)
                  (get-json "/search" :base +exploits+
                            :params (append params (list (cons "page" (max 1 (or (exploit-search-page call) 1)))))
                            :cacheable t)
                (let* ((raw (answer json status "/search"))
                       (matches (array-of raw "matches"))
                       (rows (loop for match in (subseq matches 0 (min limit (length matches)))
                                   when (hash-table-p match)
                                     collect (let ((description (text-of match "description")))
                                               (clean-object
                                                `(("id" . ,(text-of match "_id"))
                                                  ("source" . ,(text-of match "source"))
                                                  ("type" . ,(text-of match "type"))
                                                  ("platform" . ,(text-of match "platform"))
                                                  ("title" . ,(or (text-of match "title") (clip description 120)))
                                                  ("description" . ,(clip description 300))
                                                  ("cve" . ,(vec (array-of match "cve")))
                                                  ("author" . ,(vec (array-of match "author")))
                                                  ("date" . ,(text-of match "date"))
                                                  ("port" . ,(num-of match "port"))))))))
                  (reply (clean-object `(("query" . ,query)
                                         ("total" . ,(num-of raw "total"))
                                         ("returned" . ,(length rows))
                                         ("facets" . ,(shape-facets (field raw "facets")))
                                         ("exploits" . ,(vec rows))
                                         ("source" . "exploits"))))))))))))

(defun directory-answer (raw action page)
  (let ((rows (loop for row in (array-of raw "matches")
                    when (hash-table-p row)
                      collect (clean-object `(("title" . ,(text-of row "title"))
                                              ("query" . ,(text-of row "query"))
                                              ("description" . ,(clip (text-of row "description") 200))
                                              ("votes" . ,(num-of row "votes"))
                                              ("tags" . ,(vec (array-of row "tags"))))))))
    (nlk:json-object
     "action" action "total" (num-of raw "total") "page" page
     "saved_queries" (vec rows)
     "next_step" "These are query strings other people wrote. Validate one with shodan_meta(action=\"validate_query\") and count it with shodan_count before spending a credit on a search.")))

(defun query-directory (call)
  "The shodan_query tool: the community's saved query strings."
  (answering
    (let ((action (string-downcase (nlk:trimmed (or (query-directory-action call) "search"))))
          (page (max 1 (or (query-directory-page call) 1))))
      (cond
        ((string= action "tags")
         (multiple-value-bind (json status)
             (get-json "/shodan/query/tags" :params (list (cons "size" 40)) :cacheable t)
           (let* ((raw (answer json status "/shodan/query/tags"))
                  (tags (loop for row in (array-of raw "matches")
                              when (hash-table-p row)
                                collect (clean-object `(("tag" . ,(text-of row "value"))
                                                        ("count" . ,(num-of row "count")))))))
             (reply (nlk:json-object "action" "tags" "tags" (vec tags))))))
        ((string= action "search")
         (let ((query (nlk:trimmed (query-directory-query call))))
           (unless (plusp (length query))
             (return-from query-directory (nle:failure "ERROR: action=\"search\" needs a query.")))
           (multiple-value-bind (json status)
               (get-json "/shodan/query/search" :params (list (cons "query" query) (cons "page" page)) :cacheable t)
             (reply (directory-answer (answer json status "/shodan/query/search") "search" page)))))
        ((string= action "list")
         (let ((sort (string-downcase (nlk:trimmed (or (query-directory-sort-order call) "votes")))))
           (multiple-value-bind (json status)
               (get-json "/shodan/query"
                         :params (list (cons "page" page)
                                       (cons "sort" (if (member sort '("votes" "timestamp") :test #'string=) sort "votes"))
                                       (cons "order" "desc"))
                         :cacheable t)
             (reply (directory-answer (answer json status "/shodan/query") "list" page)))))
        (t (nle:failure (format nil "ERROR: unknown action ~s. Use \"search\", \"list\" or \"tags\"." action)))))))

(defun trends (call)
  "The shodan_trends tool: how many hosts matched a query, month by month."
  (answering
    (let* ((query (nlk:trimmed (trend-history-query call)))
           (facets (nlk:trimmed (trend-history-facets call))))
      (unless (plusp (length query))
        (return-from trends (nle:failure "ERROR: no query given.")))
      (multiple-value-bind (json status)
          (get-json "/api/v1/search" :base +trends+
                    :params (append (list (cons "query" query))
                                    (when (plusp (length facets)) (list (cons "facets" facets))))
                    :cacheable t)
        (let ((raw (answer json status "/api/v1/search")))
          (reply (clean-object
                  `(("query" . ,query)
                    ("total" . ,(num-of raw "total"))
                    ("months" . ,(vec (loop for row in (array-of raw "matches")
                                             when (hash-table-p row)
                                               collect (clean-object `(("month" . ,(text-of row "month"))
                                                                       ("count" . ,(num-of row "count")))))))
                    ("facets" . ,(shape-facets (field raw "facets")))
                    ("source" . "trends")))))))))

;;; --- /shodan, the command ------------------------------------------------------------

(defparameter +usage+
  "**/shodan**: internet exposure lookup
```
/shodan 1.1.1.1              host intel for an IP or hostname (free)
/shodan apache country:DE    free match count, with breakdowns
/shodan info                 plan and remaining credits
/shodan budget               this session's credit ledger
```
Searches that return individual hosts cost credits, so ask the agent for those
rather than through this command.")

(defun decode-text (text)
  "TEXT as JSON when it is JSON, else as it is."
  (or (and (stringp text) (ignore-errors (nlk:decode-json text))) text))

(defun tool-call (function contract &rest pairs)
  "FUNCTION run on an empty call, or on PAIRS, as a model's call reaches it."
  (funcall function (nlk:contract-decode (nlk:contract contract) (apply #'nlk:make-json-object pairs))))

(defun command-host (target)
  "The /shodan command's host answer, as markdown."
  (let* ((object (decode-text (tool-call #'host 'host-lookup "ip" target))))
    (if (stringp object)
        object
        (let ((record (or (field object "host") (first (array-of object "hosts")))))
          (if (null record)
              "No data for that host. Shodan saw no exposed services in the last scan window."
              (let ((services (array-of record "services"))
                    (vulns (array-of record "vulns"))
                    (hostnames (array-of record "hostnames")))
                (with-output-to-string (out)
                  (format out "**~a**~@[ · ~a~]~@[ · ~a~]~@[ · ~a~]~%"
                          (field record "ip") (field record "org") (field record "asn")
                          (let ((place (remove nil (list (field record "city") (field record "country")))))
                            (and place (format nil "~{~a~^, ~}" place))))
                  (when (plusp (length hostnames))
                    (format out "hostnames: ~{~a~^, ~}~%"
                            (subseq hostnames 0 (min 6 (length hostnames)))))
                  (when (plusp (length (array-of record "tags")))
                    (format out "tags: ~{~a~^, ~}~%" (array-of record "tags")))
                  (when (plusp (length services))
                    (format out "~%**~a exposed services**~%" (length services))
                    (dolist (service (subseq services 0 (min 12 (length services))))
                      (format out "  ~a/~a  ~a~%"
                              (field service "port") (or (field service "transport") "tcp")
                              (or (field service "product") (field service "module")
                                  (field service "title") "")))
                    (when (> (length services) 12)
                      (format out "  ... and ~a more~%" (- (length services) 12))))
                  (when (and (null services) (plusp (length (array-of record "open_ports"))))
                    (format out "open ports: ~{~a~^, ~}~%" (array-of record "open_ports")))
                  (when (plusp (length vulns))
                    (format out "~%**~a known CVEs**, worst first~%" (length vulns))
                    (dolist (vuln (subseq vulns 0 (min 8 (length vulns))))
                      (format out "  ~a~@[ (CVSS ~a)~]~%" (field vuln "cve") (field vuln "cvss"))))
                  (when (field object "degraded")
                    (format out "~%~a~%" (field object "degraded"))))))))))

(defun command-count (query)
  "The /shodan command's free count, as markdown. Never a credit-spending search."
  (let* ((object (decode-text (tool-call #'count-hosts 'host-count
                                          "query" query
                                          "facets" "country:8,org:8,port:8")))
         (total (and (hash-table-p object) (field object "total"))))
    (if (not (hash-table-p object))
        object
        (with-output-to-string (out)
          (format out "**~:d matches** for `~a`  (free lookup)~%" (or total 0) query)
          (let ((facets (field object "facets")))
            (when (hash-table-p facets)
              (loop for name being the hash-keys of facets using (hash-value rows)
                    do (format out "~%**~a**~%" name)
                       (loop for row across rows
                             do (format out "  ~a: ~:d~%" (field row "value")
                                        (or (field row "count") 0))))))))))

(defun command-account ()
  "The /shodan info answer, as markdown."
  (let* ((object (decode-text (tool-call #'account 'account-info)))
         (account (and (hash-table-p object) (field object "account")))
         (budget (and (hash-table-p object) (field object "session_budget"))))
    (if (null account)
        (or (and (hash-table-p object) (field object "note"))
            "No API key configured. Set api_key_env and restart the cell.")
        (format nil "**Shodan** plan: ~a~%query credits: ~a left~%scan credits: ~a left~%monitored IPs: ~a of ~a~%session budget: ~a of ~a query credits remaining"
                (or (field account "plan") "unknown")
                (field account "query_credits") (field account "scan_credits")
                (field account "monitored_ips") (field account "monitored_ip_allowance")
                (field budget "query_credits_remaining") (field budget "query_credits_budget")))))

(defun command-budget ()
  "The /shodan budget answer, as markdown."
  (let ((ledger (ledger-object)))
    (with-output-to-string (out)
      (format out "**Shodan session budget**~%```~%")
      (loop for key being the hash-keys of ledger
            do (format out "  ~a: ~a~%" key (gethash key ledger)))
      (format out "```~%"))))

(defun run-command (args session-id)
  "The /shodan command: a bare target is a host, anything else a free count.
Deliberately forgiving, as the original was: /shodan 1.1.1.1 and
/shodan apache country:DE both mean the obvious thing, and neither spends a
credit."
  (declare (ignore session-id))
  (handler-case
      (let* ((argument (nlk:trimmed (or args "")))
             (lowered (string-downcase argument)))
        (cond
          ((or (zerop (length argument)) (member lowered '("help" "-h" "--help" "?") :test #'string=))
           +usage+)
          ((member lowered '("info" "account" "credits") :test #'string=) (command-account))
          ((string= lowered "budget") (command-budget))
          ((and (not (find #\Space argument)) (not (find #\: argument))
                (or (looks-like-ip argument) (find #\. argument)))
           (command-host argument))
          (t (command-count argument))))
    (error (condition)
      (format nil "Shodan command failed: ~a" (nlk:one-line (princ-to-string condition))))))

;;; --- the section, and the one declaration ----------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, plus the key: the environment variable api_key_env
names first, a literal api_key second. No key at all is fine — the keyless
endpoints still answer."
  (declare (ignore table))
  (let* ((env (nlk:trimmed (or (getf values :api-key-env) "")))
         (literal (nlk:trimmed (or (getf values :api-key) "")))
         (from-env (and (plusp (length env))
                        (let ((value (nle::credential-env env)))
                          (and value (plusp (length (nlk:trimmed value)))
                               (nlk:trimmed value))))))
    (list* :api-key (or from-env (and (plusp (length literal)) literal))
           values)))

(defun note-session (op next)
  "A :ROUND point hook: a conversation that is not the ledger's starts at zero,
which is what the original's session-boundary hooks did."
  (let ((session (getf op :session-id)))
    (when (and session (not (equal session *session*)))
      (reset-ledger session)))
  (funcall next op))

(nle:define-cell shodan
  (:section ("shodan")
    (:guide "paste a Shodan API key into the environment variable api_key_env names (SHODAN_API_KEY) — a keyless install still answers hosts and CVEs; next step: ask for a host or a count")
    ("api_key_env" :env :default "SHODAN_API_KEY"
     :doc "the environment variable holding the Shodan API key (Account, then API Key)")
    ("api_key" :secret
     :doc "a key written here instead of named in api_key_env; the environment variable wins")
    ("verbosity" :choice :options '("summary" "detail" "raw") :default "summary"
     :doc "how much of a host to answer: summary, detail (certificates and headers) or raw")
    ("max_result_chars" :integer :default 24000 :min 2000
     :doc "the character budget of one tool answer; a longer one drops its per-service rows and says so")
    ("timeout" :integer :default 30 :min 1
     :doc "how long one Shodan request may take, in seconds")
    ("retries" :integer :default 2 :min 0
     :doc "how many more times a 429 or a 5xx is asked")
    ("rate_limit" :integer :default 1 :min 0
     :doc "requests per second; Shodan documents 1, and the cell paces ahead of it")
    ("cache_ttl" :integer :default 900 :min 0
     :doc "seconds a repeat GET is answered from memory; 0 disables the cache")
    ("internetdb_fallback" :boolean :default t
     :doc "answer a host lookup from InternetDB when there is no key, or when the plan refuses")
    ("query_credits" :integer :default 50 :min 0
     :doc "query credits this cell may spend in one conversation")
    ("scan_credits" :integer :default 25 :min 0
     :doc "scan credits this cell may spend in one conversation")
    ("scan_enabled" :boolean :default nil
     :doc "allow on-demand scanning; off unless the operator says otherwise")
    ("scan_allowlist" :list
     :doc "IPs or CIDR ranges scanning may target; empty means any target, once scan_enabled is true"))
  (:settings #'read-settings)
  (:help :shodan
         "shodan: host intel, free counts, DNS, CVEs, exploits and scans from Shodan's API"
         *manual*)
  (:hook :round #'note-session)
  (:command "shodan" 'run-command
            :description "Shodan lookup, free count, credits and budget"
            :argument-hint "<ip | query | info | budget>")
  (:tool "shodan_host" (nlk:contract 'host-lookup) #'host)
  (:tool "shodan_search" (nlk:contract 'host-search) #'search-hosts)
  (:tool "shodan_count" (nlk:contract 'host-count) #'count-hosts)
  (:tool "shodan_dns" (nlk:contract 'dns-lookup) #'dns)
  (:tool "shodan_cve" (nlk:contract 'cve-lookup) #'cve)
  (:tool "shodan_account" (nlk:contract 'account-info) #'account)
  (:tool "shodan_meta" (nlk:contract 'meta-info) #'meta)
  (:tool "shodan_scan" (nlk:contract 'scan-manage) #'scan)
  (:tool "shodan_alert" (nlk:contract 'alert-manage) #'alerts)
  (:tool "shodan_exploits" (nlk:contract 'exploit-search) #'exploits)
  (:tool "shodan_query" (nlk:contract 'query-directory) #'query-directory)
  (:tool "shodan_trends" (nlk:contract 'trend-history) #'trends)
  (:start (lambda ()
            (reset-ledger))))
