;;;; panels.lisp --- the panels beside the map, and the passive recon lookups.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Space weather (NOAA SWPC), world headlines (five RSS feeds plus GDELT's
;;;; article search), markets (CoinGecko, and Twelve Data when the section
;;;; names a key), World Bank country facts, and the recon kinds: dns, rdap,
;;;; ip, github, cve (with CISA KEV), mac and subdomains. Every one is a
;;;; public keyless endpoint, read through the cache SOURCES.LISP keeps.

(in-package #:nodecode-osint-map)

;;; --- space weather -----------------------------------------------------------------

(defparameter +swpc-kp+ "https://services.swpc.noaa.gov/products/noaa-planetary-k-index.json")

(defun row-number (row index)
  "ROW's INDEXth member when it is a number, or spells one."
  (and (vectorp row) (< -1 index (length row)) (numeric (aref row index))))

(defun parse-kp (raw)
  "The 3-hourly planetary Kp product as (UNIVERSAL-TIME . KP) rows, header row skipped."
  (let* ((rows (if (vectorp raw) raw #()))
         (header (and (plusp (length rows))
                      (let ((first (aref rows 0)))
                        (and (vectorp first)
                             (map 'list (lambda (cell) (and (stringp cell) (string-downcase cell))) first)))))
         (time-at (and header (position "time_tag" header :test #'equal)))
         (kp-at (and header (position "kp" header :test #'equal))))
    (if (and time-at kp-at)
        (loop for index from 1 below (length rows)
              for row = (aref rows index)
              for time = (and (vectorp row) (iso-time (clean (and (< time-at (length row)) (aref row time-at)) 30)))
              for kp = (and (vectorp row) (row-number row kp-at))
              when (and time kp) collect (cons time kp))
        (loop for row across rows
              for time = (iso-time (str row "time_tag"))
              for kp = (numeric (apply #'nlk:json-value row :any '("kp")))
              when (and time kp) collect (cons time kp)))))

(defun space-weather ()
  "The planetary Kp index now, and the last readings."
  (let ((rows (cached "swpc:kp" (lambda () (parse-kp (fetch-json +swpc-kp+))) 300)))
    (if (null rows)
        (fail "NOAA SWPC did not answer with a Kp index")
        (let ((recent (last rows (min 8 (length rows)))))
          (format nil "Space weather (NOAA SWPC): Kp now ~a (last ~a readings: ~{~a~^, ~})."
                  (fmt (cdr (car (last rows))))
                  (length recent)
                  (mapcar (lambda (row) (fmt (cdr row))) recent))))))

;;; --- headlines -----------------------------------------------------------------------

(defparameter +feeds+
  '(("BBC World" . "https://feeds.bbci.co.uk/news/world/rss.xml")
    ("Al Jazeera" . "https://www.aljazeera.com/xml/rss/all.xml")
    ("DW" . "https://rss.dw.com/xml/rss-en-world")
    ("The Guardian" . "https://www.theguardian.com/world/rss")
    ("France 24" . "https://www.france24.com/en/rss"))
  "The world services the headlines come from.")

(defparameter +gdelt-doc+ "https://api.gdeltproject.org/api/v2/doc/doc")

(defun tag-body (xml from to tag)
  "TAG's inner text inside XML between FROM and TO, without any CDATA wrapper."
  (let* ((open (search (format nil "<~a" tag) xml :start2 from :end2 to))
         (body (and open (position #\> xml :start open :end to)))
         (close (and body (search (format nil "</~a>" tag) xml :start2 body :end2 to))))
    (when (and body close)
      (let ((text (subseq xml (1+ body) close)))
        (string-trim '(#\Space #\Tab #\Newline #\Return)
                     (cond ((search "<![CDATA[" text)
                            (let ((start (+ (search "<![CDATA[" text) 9))
                                  (end (search "]]>" text)))
                              (subseq text start (or end (length text)))))
                           (t text)))))))

(defun parse-feed (xml source)
  "XML's items as entries: title, link and date, in feed order."
  (let ((out '()) (from 0))
    (loop for start = (search "<item" xml :start2 from)
          while start
          do (let* ((end (or (search "</item>" xml :start2 start) (length xml)))
                    (title (strip-tags (tag-body xml start end "title") 200))
                    (link (clean (tag-body xml start end "link") 500))
                    (date (tag-body xml start end "pubDate")))
               (when (and title link (search "http" link))
                 (push (make-entry :layer "news" :title title :subtitle source
                                   :severity 0.3 :time (rfc822-time date) :url link)
                       out))
               (setf from (max (1+ start) end))))
    (nreverse out)))

(defun parse-gdelt-articles (raw source)
  (loop for article across (arr raw "articles")
        for title = (strip-tags (str article "title") 200)
        for url = (clean (str article "url") 500)
        for seen = (clean (str article "seendate") 20)
        when (and title url (search "http" url))
          collect (make-entry :layer "news" :title title
                              :subtitle (or (clean (str article "domain") 40) source)
                              :severity 0.3
                              :time (and (and seen (> (length seen) 14))
                                         (iso-time (format nil "~a-~a-~aT~a:~a:~aZ"
                                                            (subseq seen 0 4) (subseq seen 4 6) (subseq seen 6 8)
                                                            (subseq seen 9 11) (subseq seen 11 13) (subseq seen 13 15))))
                              :url url)))

(defun url-encode (text)
  "TEXT with the characters a query string reserves escaped."
  (with-output-to-string (out)
    (loop for char across text
          do (if (or (alphanumericp char) (find char "-_.~"))
                 (write-char char out)
                 (format out "%~2,'0X" (char-code char))))))

(defun rss-feed (name url)
  "NAME's headlines, or NIL when the feed does not answer."
  (handler-case
      (parse-feed (fetch-text url :accept "application/rss+xml, application/xml, text/xml") name)
    (osint-map-error () nil)))

(defun gdelt-feed (query limit)
  "GDELT's article search for QUERY, or NIL when it does not answer."
  (handler-case
      (parse-gdelt-articles
       (fetch-json (format nil "~a?query=~a&mode=artlist&format=json&maxrecords=~d&sort=datedesc"
                           +gdelt-doc+ (url-encode query) (min 50 (max 1 limit))))
       "GDELT")
    (osint-map-error () nil)))

(defun headlines (&key query (limit nil))
  "World headlines: every feed that answered, plus GDELT's article search when QUERY is given."
  (let* ((limit (or limit (item-limit)))
         (query (or query ""))
         (feeds (loop for (name . url) in +feeds+
                      for items = (cached (format nil "feed:~a" name)
                                          (lambda () (rss-feed name url)) 300)
                      when items collect items))
         (searched (and (plusp (length query))
                        (cached (format nil "gdelt:~(~a~)" query)
                                (lambda () (gdelt-feed query limit)) 300))))
    (unless (or feeds searched)
      (fail "none of the news feeds answered"))
    (let ((all (append (apply #'append feeds) searched)))
      (when (plusp (length query))
        (setf all (remove-if-not (lambda (entry)
                                   (search (string-downcase query) (string-downcase (entry-title entry))))
                                 all)))
      (subseq (sort all #'> :key (lambda (entry) (or (entry-time entry) 0)))
              0 (min limit (length all))))))

;;; --- markets --------------------------------------------------------------------------

(defparameter +coingecko+ "https://api.coingecko.com/api/v3/simple/price")
(defparameter +twelvedata+ "https://api.twelvedata.com/quote")
(defparameter +twelvedata-symbols+ '("SPY" "XOM" "LMT" "GLD" "USO"))

(defun twelvedata-key ()
  "The optional Twelve Data key, out of the environment variable the section names."
  (let ((variable (setting :twelvedata-api-key)))
    (and variable (plusp (length variable)) (uiop:getenv variable))))

(defun market-quote (symbol key)
  (handler-case
      (cached (format nil "markets:~a" symbol) 300
              (lambda () (fetch-json (format nil "~a?symbol=~a&apikey=~a" +twelvedata+ symbol key))))
    (osint-map-error () nil)))

(defun markets ()
  "Crypto always; equities and commodities when a Twelve Data key answers."
  (let ((crypto (cached "markets:crypto" 300
                        (lambda ()
                          (fetch-json (format nil "~a?ids=bitcoin,ethereum&vs_currencies=usd"
                                              +coingecko+))))))
    (with-output-to-string (out)
      (format out "Markets:")
      (loop for (id label) in '(("bitcoin" "BTC") ("ethereum" "ETH"))
            for price = (num crypto id "usd")
            when price do (format out "~%  ~a $~,0f" label price))
      (let ((key (twelvedata-key)))
        (if (null key)
            (format out "~%  (add a Twelve Data key to the section for equities and commodities)")
            (dolist (symbol +twelvedata-symbols+)
              (let ((price (num (market-quote symbol key) "close")))
                (when price (format out "~%  ~a $~a" symbol (fmt price 2))))))))))

;;; --- country facts ---------------------------------------------------------------------

(defparameter +worldbank+ "https://api.worldbank.org/v2/country")

(defun world-bank-country (code)
  "World Bank facts for a two-letter CODE: (NAME CAPITAL REGION INCOME), or NIL."
  (let* ((raw (cached (format nil "wb:country:~a" code) 3600
                       (lambda () (fetch-json (format nil "~a/~a?format=json" +worldbank+ code)))))
         (rows (and (vectorp raw) (> (length raw) 1) (aref raw 1)))
         (facts (and (vectorp rows) (plusp (length rows)) (aref rows 0))))
    (when (and facts (str facts "name"))
      (list (str facts "name")
            (str facts "capitalCity")
            (str (obj facts "region") "value")
            (str (obj facts "incomeLevel") "value")))))

(defun world-bank-indicator (code indicator)
  "CODE's latest INDICATOR as (VALUE . YEAR), or NIL."
  (let* ((raw (cached (format nil "wb:~a:~a" indicator code) 3600
                       (lambda ()
                         (fetch-json (format nil "~a/~a/indicator/~a?format=json&per_page=1"
                                             +worldbank+ code indicator)))))
         (rows (and (vectorp raw) (> (length raw) 1) (aref raw 1)))
         (row (and (vectorp rows) (plusp (length rows)) (aref rows 0))))
    (when row
      (let ((value (num row "value")))
        (when value (cons value (str row "date")))))))

(defun country-entries (&optional (name nil))
  "Every fetched layer's items, matching NAME when one is given."
  (let ((all (loop for (id . items) in (layers-fetch)
                   when (listp items) nconc items)))
    (declare (ignore id))
    (sort (if name
              (remove-if-not (lambda (entry)
                               (and (entry-country entry)
                                    (search (string-downcase name) (string-downcase (entry-country entry)))))
                             all)
              all)
          #'> :key #'entry-severity)))

(defun country-brief (code)
  "One country: World Bank facts, the map items located there, and its headlines."
  (let* ((code (string-upcase (string-trim '(#\Space) (if (stringp code) code ""))))
         (facts (world-bank-country code)))
    (unless facts
      (fail "the World Bank has no country ~a; use an ISO 3166 alpha-2 code like UA or JP" code))
    (destructuring-bind (name capital region income) facts
      (let ((population (world-bank-indicator code "SP.POP.TOTL"))
            (gdp (world-bank-indicator code "NY.GDP.PCAP.CD")))
        (with-output-to-string (out)
          (format out "~a (~a) — World Bank" name code)
          (when capital (format out "~%  Capital: ~a" capital))
          (when region (format out "~%  Region: ~a" region))
          (when income (format out "~%  Income group: ~a" income))
          (when population (format out "~%  Population: ~:d (~a)" (round (car population)) (cdr population)))
          (when gdp (format out "~%  GDP per capita: ~,0f USD (~a)" (car gdp) (cdr gdp)))
          (let ((events (country-entries name)))
            (format out "~%~%Current map events located in ~a: ~d" name (length events))
            (dolist (entry (subseq events 0 (min 10 (length events))))
              (format out "~%~a" (entry-line entry))))
          (let ((news (handler-case (headlines :query name :limit 5) (osint-map-error () nil))))
            (format out "~%~%Headlines mentioning ~a: ~d" name (length news))
            (dolist (entry news)
              (format out "~%  - ~a (~a) ~a" (entry-title entry)
                      (or (entry-subtitle entry) "") (or (entry-url entry) "")))))))))

;;; --- recon ------------------------------------------------------------------------------

(defparameter +doh+ "https://dns.google/resolve")
(defparameter +rdap+ "https://rdap.org")
(defparameter +crtsh+ "https://crt.sh/")
(defparameter +circl+ "https://cve.circl.lu/api/cve/")
(defparameter +kev+ "https://www.cisa.gov/sites/default/files/feeds/known_exploited_vulnerabilities.json"
  "CISA's known-exploited vulnerabilities feed.")
(defparameter +ipwho+ "https://ipwho.is/")
(defparameter +github+ "https://api.github.com/users/")
(defparameter +macvendors+ "https://api.macvendors.com/")

(defparameter +recon-kinds+ '("dns" "rdap" "subdomains" "cve" "ip" "github" "mac")
  "The lookups this cell answers.")

(defun domain-target (target)
  "TARGET as a domain name, or a refusal naming what it wanted."
  (let* ((trimmed (string-downcase (string-trim '(#\Space) (if (stringp target) target ""))))
         (trimmed (if (search "://" trimmed) (url-host trimmed) trimmed))
         (trimmed (string-right-trim "." trimmed)))
    (if (and (find #\. trimmed)
             (every (lambda (c) (or (alphanumericp c) (find c ".-"))) trimmed))
        trimmed
        (fail "give a domain name like example.com (no scheme, path or port)"))))

(defun recon-dns (target)
  (let ((domain (domain-target target)))
    (with-output-to-string (out)
      (format out "dns lookup for ~a" domain)
      (dolist (type '("A" "AAAA" "MX" "NS" "TXT"))
        (let ((answers (arr (fetch-json (format nil "~a?name=~a&type=~a" +doh+ domain type)) "Answer")))
          (if (zerop (length answers))
              (format out "~%  ~a: none" type)
              (loop for answer across answers
                    do (format out "~%  ~a: ~a" type (or (str answer "data") "?"))))))
      (format out "~%Sources: Google Public DNS"))))

(defun recon-rdap (target)
  (let* ((domain (domain-target target))
         (raw (fetch-json (format nil "~a/domain/~a" +rdap+ domain))))
    (with-output-to-string (out)
      (format out "rdap lookup for ~a" domain)
      (when (str raw "handle") (format out "~%  Handle: ~a" (str raw "handle")))
      (loop for event across (arr raw "events")
            for action = (str event "eventAction")
            for date = (str event "eventDate")
            when (and action date) do (format out "~%  ~a: ~a" action date))
      (loop for entity across (arr raw "entities")
            for roles = (arr entity "roles")
            for handle = (str entity "handle")
            when handle do (format out "~%  ~@[~a: ~]~a" (clean (at roles 0) 30) handle))
      (format out "~%Sources: rdap.org"))))

(defun recon-ip (target)
  (let* ((address (string-trim '(#\Space) (if (stringp target) target "")))
         (raw (fetch-json (format nil "~a~a" +ipwho+ (url-encode address)))))
    (unless (nlk:json-value raw :boolean "success")
      (fail "ipwho.is could not place ~a" address))
    (with-output-to-string (out)
      (format out "ip lookup for ~a" address)
      (loop for (label . key) in '(("Country" . "country") ("Region" . "region") ("City" . "city"))
            do (let ((value (str raw key)))
                 (when value (format out "~%  ~a: ~a" label value))))
      (let ((organisation (str raw "connection" "org"))
            (asn (str raw "connection" "asn")))
        (when organisation (format out "~%  Organisation: ~a" organisation))
        (when asn (format out "~%  ASN: ~a" asn)))
      (format out "~%Sources: ipwho.is"))))

(defun recon-github (target)
  (let* ((name (string-trim '(#\Space) (if (stringp target) target "")))
         (raw (fetch-json (format nil "~a~a" +github+ (url-encode name))
                          :accept "application/vnd.github+json")))
    (unless (str raw "login")
      (fail "GitHub knows no user or organisation called ~a" name))
    (with-output-to-string (out)
      (format out "github lookup for ~a" (str raw "login"))
      (loop for (label . key) in '(("Name" . "name") ("Company" . "company") ("Location" . "location")
                                   ("Repositories" . "public_repos") ("Followers" . "followers")
                                   ("Created" . "created_at"))
            do (let ((value (nlk:json-value raw :any key)))
                 (when value (format out "~%  ~a: ~a" label value))))
      (format out "~%Sources: GitHub"))))

(defun kev-ids ()
  "Every CVE id on the CISA known-exploited list, fetched once and kept."
  (cached "kev" 21600
          (lambda ()
            (loop for entry across (arr (fetch-json +kev+) "vulnerabilities")
                  for id = (str entry "cveID")
                  when id collect id))))

(defun recon-cve (target)
  (let* ((id (string-upcase (string-trim '(#\Space) (if (stringp target) target ""))))
         (raw (and (> (length id) 8) (string= "CVE-" id :end2 4)
                   (fetch-json (format nil "~a~a" +circl+ id)))))
    (unless raw
      (fail "CVE ids look like CVE-2024-3094"))
    (unless (str raw "summary")
      (fail "CIRCL has no record of ~a" id))
    (let ((kev (handler-case (kev-ids) (osint-map-error () nil))))
      (with-output-to-string (out)
        (format out "cve lookup for ~a" id)
        (format out "~%  Summary: ~a" (str raw "summary"))
        (let ((score (num raw "cvss")))
          (when score (format out "~%  CVSS: ~a" (fmt score 1))))
        (format out "~%  CISA KEV: ~a" (if (and kev (member id kev :test #'string=)) "yes" "no"))
        (format out "~%Sources: CIRCL CVE, CISA KEV")))))

(defun recon-mac (target)
  (let* ((digits (string-upcase (remove-if (lambda (c) (find c ":-. ")) (if (stringp target) target ""))))
         (oui (and (>= (length digits) 6) (subseq digits 0 6))))
    (unless oui (fail "give a MAC address like 00:1A:2B:3C:4D:5E, or its first half"))
    (let ((vendor (string-trim '(#\Space #\Newline)
                                (handler-case (fetch-text (format nil "~a~a" +macvendors+ oui) :accept "text/plain")
                                  (osint-map-error () "")))))
      (if (zerop (length vendor))
          (format nil "mac lookup for ~a~%  Vendor: unknown~%Sources: MacVendors" oui)
          (format nil "mac lookup for ~a~%  Vendor: ~a~%Sources: MacVendors" oui vendor)))))

(defun recon-subdomains (target)
  (let* ((domain (domain-target target))
         (raw (cached (format nil "crtsh:~a" domain) 900
                      (lambda () (fetch-json (format nil "~aq=%%25.~a&output=json" +crtsh+ domain)))))
         (names '()))
    (loop for row across (if (vectorp raw) raw #())
          for value = (str row "name_value")
          when value
            do (dolist (one (uiop:split-string value :separator '(#\Newline)))
                 (let ((one (string-downcase (string-trim '(#\Space) one))))
                   (when (and (plusp (length one)) (not (member one names :test #'equal)))
                     (push one names)))))
    (when (null names) (fail "crt.sh lists no certificate for ~a" domain))
    (let ((names (nreverse names)))
      (format nil "subdomains lookup for ~a~%  ~d names~%  ~a~%Sources: crt.sh"
              domain (length names) (format nil "~{~a~^, ~}" (subseq names 0 (min 40 (length names))))))))

(defun recon-run (kind target)
  "One passive lookup of KIND on TARGET."
  (let ((kind (string-downcase (string-trim '(#\Space) (if (stringp kind) kind "")))))
    (cond ((equal kind "dns") (recon-dns target))
          ((equal kind "rdap") (recon-rdap target))
          ((equal kind "ip") (recon-ip target))
          ((equal kind "github") (recon-github target))
          ((equal kind "cve") (recon-cve target))
          ((equal kind "mac") (recon-mac target))
          ((equal kind "subdomains") (recon-subdomains target))
          (t (fail "unknown recon kind ~s; use one of ~{~a~^, ~}" kind +recon-kinds+)))))
