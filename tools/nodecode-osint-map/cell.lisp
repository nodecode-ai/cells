;;;; cell.lisp --- the five tools, the primer, the section, /osint, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :osint-map) answers the primer below, and every session's help
;;;; section carries one line naming it. The operator reads a brief with
;;;; /osint. Nothing here needs a key: every source is public.

(in-package #:nodecode-osint-map)

;;; --- the primer -------------------------------------------------------------------

(defparameter +primer+
  "OSINT Map reads public situational-awareness feeds and answers from them: one globe's worth of
sources, no key. Call a verb through eval; each answers a plain-text brief the model can quote,
with the sources named. Every verb is also a tool the model may call directly.
  (osint:situation)                    counts and the worst three items per layer, the Kp index
                                        and the top headlines. The first call fetches five feeds
                                        and can take ~20 s.
  (osint:events &key layer country lat lon radius limit)
                                        current items, most severe first. layer is one of
                                        quakes, natural, disasters, weather, aircraft. country is
                                        a code (UA) or a name (Ukraine). lat and lon take a point
                                        and radius is its circle in km (default 500, max 5000).
  (osint:country \"BR\")                 World Bank facts, the events located there, headlines.
  (osint:news &key query limit)         BBC, Al Jazeera, DW, The Guardian, France 24, plus GDELT's
                                        article search when query is given.
  (osint:recon \"dns\" \"example.com\")   one passive lookup: dns, rdap, subdomains, cve, ip,
                                        github or mac. Private and local addresses are not probed.
Layers come from USGS, NASA EONET, GDACS, the US National Weather Service and adsb.lol; incidents
and aircraft are machine-coded or volunteer-fed, so say where data is thin rather than asserting.
ERROR: OSINT-MAP-ERROR names what failed; a source that is down says so in its own line."
  "What (help :osint-map) answers while the cell runs.")

;;; --- arguments --------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: osint-map: ~a" condition)))))

(defun argument (args name)
  (let ((value (and (hash-table-p args) (gethash name args))))
    (if (eq value :null) nil value)))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (argument args name)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
        (string-trim '(#\Space) value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun argument-words (args name)
  "ARGS' NAME as a string, or NIL when it is absent or blank."
  (let ((value (argument args name)))
    (and (stringp value) (plusp (length (string-trim '(#\Space) value)))
         (string-trim '(#\Space) value))))

(defun argument-number (args name &optional (low most-negative-double-float) (high most-positive-double-float))
  (let ((value (argument args name)))
    (when (and (realp value) (<= low value high)) (coerce value 'double-float))))

(defun argument-integer (args name &optional (low 1) (high 100) (default nil))
  (let ((value (argument args name)))
    (if (and (integerp value) (<= low value high)) value default)))

(defun distance-km (from-lat from-lon lat lon)
  "The great-circle distance between two points, in km."
  (let* ((radians (lambda (degrees) (* (/ pi 180.0) degrees)))
         (dlat (- (funcall radians lat) (funcall radians from-lat)))
         (dlon (- (funcall radians lon) (funcall radians from-lon)))
         (a (+ (expt (sin (/ dlat 2)) 2)
               (* (cos (funcall radians from-lat)) (cos (funcall radians lat))
                  (expt (sin (/ dlon 2)) 2)))))
    (* 2 6371.0 (atan (sqrt a) (sqrt (- 1 a))))))

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

;;; --- the verbs ---------------------------------------------------------------------

(defun situation ()
  "What is happening now, layer by layer, with the Kp index and the top headlines."
  (answering
    (let ((results (layers-fetch)))
      (with-output-to-string (out)
        (format out "OSINT situation at ~a" (stamp (get-universal-time)))
        (loop for (id . items) in results
              for label = (second (assoc id +layers+ :test #'equal))
              do (format out "~%~a: ~a" label
                         (if (listp items)
                             (format nil "~d items" (length items))
                             (format nil "unavailable (~a)" items)))
                 (when (listp items)
                   (dolist (entry (subseq items 0 (min 3 (length items))))
                     (format out "~%  ~a" (entry-line entry)))))
        (format out "~%~a" (handler-case (space-weather)
                             (osint-map-error (condition)
                               (format nil "Space weather unavailable: ~a" condition))))
        (let ((news (handler-case (headlines :limit 5) (osint-map-error () nil))))
          (format out "~%Top headlines:")
          (if news
              (dolist (entry news)
                (format out "~%  - ~a (~a, ~a) ~a" (entry-title entry) (entry-subtitle entry)
                        (or (stamp (entry-time entry)) "time unknown") (or (entry-url entry) "")))
              (format out "~%  (not available)")))))))

(defun events (&key layer country lat lon (radius 500) (limit nil))
  "Current map items, most severe first, filtered by LAYER, COUNTRY, or a point and RADIUS."
  (answering
    (let* ((ids (if layer
                    (progn (unless (member layer (layer-ids) :test #'equal)
                             (fail "unknown layer ~s; use one of ~{~a~^, ~}" layer (layer-ids)))
                           (list layer))
                    (layer-ids)))
           (radius (min (or radius 500) 5000))
           (limit (or limit (item-limit))))
      (when (and (or lat lon) (not (and lat lon)))
        (fail "give both lat and lon, or neither"))
      (let ((name (and country (or (ignore-errors (first (world-bank-country (string-upcase country))))
                                   (list country)))))
        (let ((found (loop for (id . items) in (layers-fetch ids)
                           when (listp items)
                             nconc (remove-if-not
                                    (lambda (entry)
                                      (and (or (null name)
                                               (and (entry-country entry)
                                                    (search (string-downcase (first name))
                                                            (string-downcase (entry-country entry)))))
                                           (or (null lat)
                                               (and (entry-lat entry) (entry-lon entry)
                                                    (<= (distance-km lat lon (entry-lat entry) (entry-lon entry))
                                                        radius)))))
                                    items))))
          (with-output-to-string (out)
            (format out "~d items~@[ in ~a~]~@[ within ~a km of ~,2f,~,2f~]"
                    (length found)
                    (and name (first name))
                    (and lat radius)
                    (or lat 0.0) (or lon 0.0))
            (format out ":")
            (if found
                (dolist (entry (subseq found 0 (min limit (length found))))
                  (format out "~%~a" (entry-line entry)))
                (format out "~%  - nothing matches right now"))))))))

(defun country (code)
  "A country's World Bank facts, the map items located there, and its headlines."
  (answering (country-brief (or code (fail "code is required")))))

(defun news (&key query (limit nil))
  "World headlines: five world services, plus GDELT's article search when QUERY is given."
  (answering
    (let ((items (headlines :query (or query "") :limit (or limit (item-limit)))))
      (with-output-to-string (out)
        (format out "~d headlines~@[ matching ~s~]:" (length items) (and query (plusp (length query)) query))
        (dolist (entry items)
          (format out "~%  - ~a (~a, ~a) ~a" (entry-title entry) (entry-subtitle entry)
                  (or (stamp (entry-time entry)) "time unknown") (or (entry-url entry) "")))))))

(defun recon (kind target)
  "One passive lookup on a public TARGET: dns, rdap, subdomains, cve, ip, github or mac."
  (answering (recon-run (or kind (fail "kind is required")) (or target (fail "target is required")))))

;;; --- the tools ------------------------------------------------------------------------

(defparameter *situation-schema*
  (tool-schema
   "A current world situation brief from public sources: item counts and the most severe items per layer (earthquakes, natural events, disaster alerts, US severe weather, military aircraft), the planetary Kp index and the top headlines. Takes no arguments; the first call can take about 20 seconds."
   '()))

(defparameter *events-schema*
  (tool-schema
   "List current map items, most severe first, filtered by layer, country and/or a point and radius."
   '(("layer" "string" "One layer: quakes, natural, disasters, weather or aircraft. Omit for every layer." nil)
     ("country" "string" "A country code like UA or a name like Ukraine." nil)
     ("lat" "number" "Latitude of the centre point (needs lon)." nil)
     ("lon" "number" "Longitude of the centre point (needs lat)." nil)
     ("radius_km" "number" "Radius around lat/lon in km (default 500, max 5000)." nil)
     ("limit" "integer" "Maximum items (at most 100)." nil))))

(defparameter *country-schema*
  (tool-schema
   "Country brief: World Bank facts (capital, region, income, population, GDP per capita), current map items located in the country, and matching headlines."
   '(("code" "string" "ISO 3166 alpha-2 code, like BR." t))))

(defparameter *recon-schema*
  (tool-schema
   "Passive recon lookup on a public target: dns (records), rdap (registration), subdomains (certificate transparency), cve (score and CISA KEV status), ip (geo and ASN), github (user or organisation), mac (vendor). Private and local addresses are not probed."
   '(("kind" "string" "One of dns, rdap, subdomains, cve, ip, github, mac." t)
     ("target" "string" "Domain, IP, CVE id, GitHub name or MAC, matching kind." t))))

(defparameter *news-schema*
  (tool-schema
   "Search recent world headlines: BBC World, Al Jazeera, DW, The Guardian and France 24, plus GDELT's article search when a query is given. GDELT results are recent article listings, not editorially curated."
   '(("query" "string" "Words to find in the headlines; empty for the latest." nil)
     ("limit" "integer" "Maximum headlines (at most 50)." nil))))

(defun tool-situation (args)
  (declare (ignore args))
  (situation))

(defun tool-events (args)
  (answering
    (events :layer (argument-words args "layer")
            :country (argument-words args "country")
            :lat (argument-number args "lat" -90 90)
            :lon (argument-number args "lon" -180 180)
            :radius (argument-number args "radius_km" 1 5000)
            :limit (argument-integer args "limit" 1 100 nil))))

(defun tool-country (args)
  (answering (country (argument-text args "code"))))

(defun tool-recon (args)
  (answering (recon (argument-text args "kind") (argument-text args "target"))))

(defun tool-news (args)
  (answering
    (news :query (or (argument-words args "query") "")
          :limit (argument-integer args "limit" 1 50 nil))))

;;; --- the command ----------------------------------------------------------------------

(defun run-command (text session-id)
  "One /osint invocation: no argument the situation brief, an argument the headlines it matches."
  (declare (ignore session-id))
  (let ((query (and (stringp text) (string-trim '(#\Space) text))))
    (answering
      (if (and query (plusp (length query)))
          (news :query query :limit 8)
          (situation)))))

;;; --- the cell -------------------------------------------------------------------------

(defun start ()
  "The cache starts empty; stopping drops it, and every verb refuses again."
  (check-config)
  (setf *cache* (make-hash-table :test #'equal))
  (nle:on-stop (lambda () (setf *cache* nil))))

(defun check-config ()
  "Refuse a section that names a market source it has no key for."
  (when (and (equal (setting :markets) "twelvedata")
             (null (twelvedata-key)))
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "osint-map: the twelvedata market source needs a key: set "
                                 "\"twelvedata_api_key\" in the osint-map section to the name of "
                                 "an environment variable holding a free key, or use "
                                 "\"markets\": \"crypto\""))))

(nle:define-cell osint-map
  (:section ("osint-map")
    (:guide "no key is needed: every source is public; markets is crypto, or twelvedata with \"twelvedata_api_key\" naming an environment variable holding a free key; next step: /osint writes a brief")
    ("markets" :choice :options '("crypto" "twelvedata") :default "crypto"
     :doc "crypto for CoinGecko alone, twelvedata to add equities and commodities")
    ("twelvedata_api_key" :env
     :doc "the environment variable holding an optional Twelve Data key")
    ("user_agent" :string :default "nodecode-osint-map/0.1 (+https://nodecode.ai)"
     :doc "what every source sees the request as")
    ("timeout_seconds" :integer :default 15 :min 1
     :doc "one source's ceiling")
    ("max_items" :integer :default 20 :min 1
     :doc "how many items a verb lists by default")
    ("cache_seconds" :integer :default 60 :min 0
     :doc "how long one source's answer is reused"))
  (:start #'start)
  (:help :osint-map "osint: public situational feeds and passive recon, with no key - (osint:situation), (osint:events), (osint:country \"BR\"), (osint:news), (osint:recon \"dns\" \"example.com\")" +primer+)
  (:command "osint" 'run-command
            :description "A world situation brief, or headlines matching what you typed"
            :argument-hint "QUERY")
  (:tool "osint_situation" *situation-schema* 'tool-situation)
  (:tool "osint_events" *events-schema* 'tool-events)
  (:tool "osint_country" *country-schema* 'tool-country)
  (:tool "osint_recon" *recon-schema* 'tool-recon)
  (:tool "osint_news" *news-schema* 'tool-news))
