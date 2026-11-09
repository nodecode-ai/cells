;;;; cell-test.lisp --- the OSINT Map cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every source is a stubbed NLK:HTTP: nothing here reaches USGS, GDELT, the
;;;; World Bank or the network at all.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (URL METHOD ARGS), newest first.")

(defvar *answers* '()
  "((FRAGMENT . BODY) ...): the body a URL containing FRAGMENT answers with.")

(defparameter +quakes-json+
  "{\"type\":\"FeatureCollection\",\"features\":[{\"id\":\"q1\",\"properties\":{\"mag\":6.1,\"place\":\"10 km NE of Somewhere, Japan\",\"time\":1731067200000,\"url\":\"https://example.org/quake\"},\"geometry\":{\"coordinates\":[140.1,35.6,42.0]}}]}")

(defparameter +eonet-json+ "{\"events\":[]}")
(defparameter +gdacs-json+ "{\"features\":[]}")
(defparameter +nws-json+ "{\"features\":[]}")
(defparameter +adsb-json+ "{\"ac\":[]}")

(defparameter +kp-json+ "[[\"time_tag\",\"Kp\"],[\"2024-11-08T09:00:00\",\"3.0\"],[\"2024-11-08T12:00:00\",\"4.7\"]]")

(defparameter +feed-xml+
  "<rss><channel><item><title>World headline one</title><link>https://example.org/a</link><pubDate>Fri, 08 Nov 2024 12:00:00 GMT</pubDate></item></channel></rss>")

(defparameter +gdelt-json+
  "{\"articles\":[{\"title\":\"A searched headline\",\"url\":\"https://example.org/b\",\"domain\":\"example.org\",\"seendate\":\"20241108T120000Z\"}]}")

(defparameter +country-json+
  "[{\"page\":1},[{\"name\":\"Brazil\",\"capitalCity\":\"Brasilia\",\"region\":{\"value\":\"Latin America\"},\"incomeLevel\":{\"value\":\"Upper middle income\"}}]]")

(defparameter +indicator-json+ "[{\"page\":1},[{\"value\":213421037,\"date\":\"2023\"}]]")

(defparameter +dns-json+
  "{\"Status\":0,\"Answer\":[{\"name\":\"example.com.\",\"type\":1,\"data\":\"93.184.216.34\"}]}")

(defun osint-body-for (url)
  (cdr (assoc-if (lambda (fragment) (search fragment url)) *answers*)))

(defmacro with-osint-wire (&body body)
  "BODY with NLK:HTTP stubbed: each request is logged, each URL answered from *ANSWERS*."
  `(let ((*requests* '())
         (*answers* (list (cons "earthquake.usgs.gov" +quakes-json+)
                          (cons "eonet" +eonet-json+)
                          (cons "gdacs" +gdacs-json+)
                          (cons "api.weather.gov" +nws-json+)
                          (cons "adsb.lol" +adsb-json+)
                          (cons "swpc.noaa.gov" +kp-json+)
                          (cons "feeds.bbci.co.uk" +feed-xml+)
                          (cons "gdeltproject.org" +gdelt-json+)
                          (cons "worldbank.org/v2/country/BR/indicator" +indicator-json+)
                          (cons "worldbank.org" +country-json+)
                          (cons "dns.google" +dns-json+))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list url method args) *requests*)
                                (values (or (osint-body-for url) "") 200))
       ,@body)))

(defun osint-asked () "The stubbed requests, oldest first." (reverse *requests*))

(defun osint-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ----------------------------------------------------------------------

(define-test-slice "osint-map" "OSINT-MAP-CELL-" :start nodecode-osint-map:start-cell)

(define-cell-lifecycle-tests "osint-map"
  (:fixture with-osint-wire)
  (:help :osint-map)
  (:command "osint")
  (:running (is (nle::find-tool "osint_situation") "osint_situation is registered")
            (is (nle::find-tool "osint_events") "osint_events")
            (is (nle::find-tool "osint_country") "osint_country")
            (is (nle::find-tool "osint_recon") "osint_recon")
            (is (nle::find-tool "osint_news") "osint_news")
            (is (equal "crypto" (nodecode-osint-map::setting :markets)) "the default market source")
            (is (= 15 (nodecode-osint-map::setting :timeout-seconds)) "the default timeout"))
  (:stopped (is (null (nle::find-tool "osint_situation")) "stop unregisters the tools")
            (is (null nodecode-osint-map::*cache*) "and the cache is gone"))
  (:refused ("timeout_seconds" 0) ("max_items" 0) ("markets" "twelvedata")))

;;; --- the situation brief --------------------------------------------------------------

(deftest osint-map-cell-situation-reads-every-layer ()
  (with-osint-wire
    (with-cell-stop ((osint-map-start))
      (let ((answer (osint-tool "osint_situation" (nlk:json-object))))
        (is (search "OSINT situation at" answer) answer)
        (is (search "Earthquakes (USGS): 1 items" answer) answer)
        (is (search "Japan" answer) answer)
        (is (search "Kp now 4.7" answer) answer)
        (is (search "World headline one" answer) answer))
      (is (search "earthquake.usgs.gov" (first (first (osint-asked)))) "the layers were fetched"))))

;;; --- events --------------------------------------------------------------------------------

(deftest osint-map-cell-events-answers-one-layer ()
  (with-osint-wire
    (with-cell-stop ((osint-map-start))
      (let ((answer (osint-tool "osint_events" (nlk:json-object "layer" "quakes" "limit" 5))))
        (is (search "1 items:" answer) answer)
        (is (search "[Japan]" answer) answer)
        (is (search "Depth 42 km" answer) answer))
      (is (= 1 (length (osint-asked))) "one layer, one request")
      (let ((answer (osint-tool "osint_events" (nlk:json-object "layer" "volcanoes"))))
        (is (search "unknown layer" answer) answer)))))

;;; --- news ------------------------------------------------------------------------------------

(deftest osint-map-cell-news-merges-the-feeds ()
  (with-osint-wire
    (with-cell-stop ((osint-map-start))
      (let ((answer (osint-tool "osint_news" (nlk:json-object "limit" 3))))
        (is (search "1 headlines:" answer) answer)
        (is (search "World headline one" answer) answer)
        (is (search "2024-11-08 12:00 UTC" answer) answer))
      (let ((answer (osint-tool "osint_news" (nlk:json-object "query" "searched"))))
        (is (search "A searched headline" answer) answer)))))

;;; --- country -----------------------------------------------------------------------------------

(deftest osint-map-cell-country-briefs-from-the-world-bank ()
  (with-osint-wire
    (with-cell-stop ((osint-map-start))
      (let ((answer (osint-tool "osint_country" (nlk:json-object "code" "BR"))))
        (is (search "Brazil (BR)" answer) answer)
        (is (search "Capital: Brasilia" answer) answer)
        (is (search "Population: 213,421,037" answer) answer)
        (is (search "Current map events located in Brazil: 0" answer) answer)))))

;;; --- recon -------------------------------------------------------------------------------------

(deftest osint-map-cell-recon-answers-one-lookup ()
  (with-osint-wire
    (with-cell-stop ((osint-map-start))
      (let ((answer (osint-tool "osint_recon" (nlk:json-object "kind" "dns" "target" "example.com"))))
        (is (search "dns lookup for example.com" answer) answer)
        (is (search "A: 93.184.216.34" answer) answer)
        (is (search "Google Public DNS" answer) answer))
      (is (search "name=example.com" (first (first (osint-asked)))) "the domain rode the query"))))

(deftest osint-map-cell-a-missing-argument-is-a-repair ()
  (with-osint-wire
    (with-cell-stop ((osint-map-start))
      (let ((answer (osint-tool "osint_recon" (nlk:json-object "kind" "dns"))))
        (is (search "target is required" answer) answer))
      (let ((answer (osint-tool "osint_country" (nlk:json-object))))
        (is (search "code is required" answer) answer)))))

;;; --- /osint ------------------------------------------------------------------------------------

(deftest osint-map-cell-the-command-briefs-or-searches ()
  (with-osint-wire
    (with-cell-stop ((osint-map-start))
      (let ((brief (nodecode-osint-map::run-command "" nil)))
        (is (search "OSINT situation at" brief) brief))
      (let ((found (nodecode-osint-map::run-command "searched" nil)))
        (is (search "A searched headline" found) found)))))
