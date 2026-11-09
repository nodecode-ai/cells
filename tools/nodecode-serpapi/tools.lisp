;;;; tools.lisp --- the eight tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin registered seven tools and one web-search provider on one
;;;; endpoint, https://serpapi.com/search, with the engine as a parameter.
;;;; Here every tool is one NLK:DEFINE-CONTRACT, so the shape the model is
;;;; shown and the shape the body reads are one declaration, and an argument
;;;; the declaration does not carry has no reader. The provider's own search
;;;; (engine google_light) is the eighth tool, serpapi_search, so the same
;;;; search the provider served is one the model can call by name.
;;;;
;;;; Each tool answers text: SerpApi's own Markdown by default, or the
;;;; structured response when the call says output "json".

(in-package #:nodecode-serpapi)

;;; --- reading what the call carries -----------------------------------------------

(defun output-json-p (output)
  (equal output "json"))

(defun limit-of (value)
  "VALUE clamped to 1..20; the section's max_results when the call named none."
  (max 1 (min (or value (setting :max-results) 5) 20)))

(defun code (value name)
  "VALUE as a two-letter market code, lowercased, or NIL."
  (let ((text (string-downcase (string-trim " " (or value "")))))
    (cond ((zerop (length text)) nil)
          ((and (= (length text) 2) (every #'alpha-char-p text)) text)
          (t (fail "~a must be a two-letter country code" name)))))

(defun language (value)
  "VALUE as a language code (en, fil, en-gb, es-419), lowercased, or NIL."
  (let* ((text (string-downcase (string-trim " " (or value ""))))
         (dash (position #\- text))
         (head (if dash (subseq text 0 dash) text))
         (tail (and dash (subseq text (1+ dash)))))
    (cond ((zerop (length text)) nil)
          ((and (<= 2 (length head) 3)
                (every #'alpha-char-p head)
                (or (null tail)
                    (and (= (length tail) 2) (every #'alpha-char-p tail))
                    (and (= (length tail) 3) (every #'digit-char-p tail))))
           text)
          (t (fail "language must use a code such as 'en', 'fil', 'en-gb', or 'es-419'")))))

(defun currency (value)
  "VALUE as a three-letter currency code, uppercased, or NIL."
  (let ((text (string-upcase (string-trim " " (or value "")))))
    (cond ((zerop (length text)) nil)
          ((and (= (length text) 3) (every #'alpha-char-p text)) text)
          (t (fail "currency must be a three-letter code")))))

(defun date-shape-p (text)
  (and (= (length text) 10)
       (loop for index from 0 below 10
             for character = (char text index)
             always (if (member index '(4 7))
                        (char= character #\-)
                        (digit-char-p character)))
       (<= 1 (parse-integer text :start 5 :end 7) 12)
       (<= 1 (parse-integer text :start 8 :end 10) 31)))

(defun date-value (value name &key required)
  "VALUE as a YYYY-MM-DD date, or NIL."
  (let ((text (string-trim " " (or value ""))))
    (cond ((zerop (length text))
           (when required (fail "~a is required" name))
           nil)
          ((date-shape-p text) text)
          (t (fail "~a must use YYYY-MM-DD" name)))))

(defun travel-ids (value name &key required)
  "VALUE as comma-separated airport codes or /m/ or /g/ KGMIDs, or NIL."
  (let ((text (string-trim " " (or value ""))))
    (cond ((zerop (length text))
           (when required (fail "~a is required" name))
           nil)
          (t (let ((identifiers (mapcar (lambda (item) (string-trim " " item))
                                         (uiop:split-string text :separator '(",")))))
               (dolist (identifier identifiers)
                 (unless (or (and (= (length identifier) 3)
                                   (every #'alpha-char-p identifier)
                                   (every #'upper-case-p identifier))
                             (and (> (length identifier) 3)
                                  (member (subseq identifier 0 3) '("/m/" "/g/") :test #'string=)))
                   (fail "~a must contain uppercase three-letter airport codes or /m/ or /g/ KGMIDs"
                         name)))
               (format nil "~{~a~^,~}" identifiers))))))

(defun passengers (adults children &key infants-in-seat infants-on-lap)
  "The four passenger counts as request parameters, bounded and consistent."
  (let ((adults (or adults 1))
        (children (or children 0))
        (seats (or infants-in-seat 0))
        (laps (or infants-on-lap 0)))
    (unless (<= 1 adults 9) (fail "adults must be between 1 and 9"))
    (unless (<= 0 children 9) (fail "children must be between 0 and 9"))
    (unless (<= 0 seats 9) (fail "infants_in_seat must be between 0 and 9"))
    (unless (<= 0 laps 9) (fail "infants_on_lap must be between 0 and 9"))
    (when (> (+ adults children seats laps) 9)
      (fail "total number of passengers must not exceed 9"))
    (when (> laps adults)
      (fail "infants_on_lap must not exceed the number of adults"))
    (list (cons "adults" adults) (cons "children" children)
          (cons "infants_in_seat" seats) (cons "infants_on_lap" laps))))

(defun children-ages (ages children)
  "AGES — the contract's array — as the comma-separated ages SerpApi wants."
  (cond ((null ages)
         (when (plusp (or children 0))
           (fail "children_ages must contain one age per child"))
         nil)
        (t (unless (= (length ages) (or children 0))
             (fail "children_ages must contain one age per child"))
           (format nil "~{~d~^,~}"
                   (map 'list (lambda (age)
                                (if (and (integerp age) (<= 1 age 17))
                                    age
                                    (fail "children_ages must contain ages from 1 to 17")))
                        ages)))))

(defun bolt (&rest pairs)
  "A request's parameters from PAIRS (NAME VALUE ...), leaving out every NIL."
  (loop for (name value) on pairs by #'cddr
        when value append (list (cons name value))))

;;; --- reading what the service answers -------------------------------------------

(defun rows (body key)
  "BODY's KEY array as a list of objects, dropping anything that is not one."
  (let ((value (nlk:json-value body :array key)))
    (and value (remove-if-not #'hash-table-p (coerce value 'list)))))

(defun value-of (object key)
  "KEY's value when it is a scalar a result row can carry, else NIL."
  (let ((value (nlk:json-value object :any key)))
    (cond ((or (null value) (eq value :null)) nil)
          ((stringp value) (and (plusp (length value)) value))
          ((or (integerp value) (floatp value) (eq value t)) value)
          (t nil))))

(defun text-of (object &rest path)
  "PATH's leaf as text: a string, a number as written, or an object's name."
  (let ((value (apply #'nlk:json-value object :any path)))
    (cond ((stringp value) (and (plusp (length value)) value))
          ((hash-table-p value) (nlk:json-value value :text "name"))
          ((or (integerp value) (floatp value)) (princ-to-string value))
          (t nil))))

(defun pick (object names)
  "OBJECT's NAMES as a JSON-object pair list, every missing or empty one left out."
  (loop for name in names
        for value = (value-of object name)
        when value append (list name value)))

(defun results-json (engine query items)
  "The compact JSON answer: what the engine found, one object per result."
  (nlk:encode-json-object
   (apply #'nlk:make-json-object
          (list "success" t
                "engine" engine
                "query" (or query "")
                "results_count" (length items)
                "results" (coerce items 'vector)))))

(defun item-object (row position names)
  (apply #'nlk:make-json-object
         (append (list "position" (or (value-of row "position") position))
                 (pick row names))))

;;; --- serpapi_search ----------------------------------------------------------

(nlk:define-contract search-call
  (:doc "Search the web through SerpApi (Google, light rendering). Answers the organic
results — title, url, snippet — for a query. Use serpapi_news_search for recent reporting and
serpapi_maps_search for places.")
  ("query" :text :required t :doc "the search query")
  ("location" :string
   :doc "the search origin, such as 'New York, NY'; a city-level location changes the ranking")
  ("language" :string :doc "a language code such as en, fil, en-gb or es-419")
  ("country" :string :doc "a two-letter market country code, such as us or in")
  ("limit" :integer
   :doc "how many results to keep, 1 to 20; the section's max_results when the call names none")
  ("output" :string :options '("md" "json")
   :doc "md by default, which a model reads; json only when structured data is needed"))

(defparameter +search-fields+ '("title" "link" "snippet" "displayed_link" "source" "date"))

(defun tool-search (call)
  "serpapi_search: google_light, the engine the plugin's web provider used."
  (let* ((query (search-call-query call))
         (limit (limit-of (search-call-limit call)))
         (params (bolt "q" query
                       "location" (nonempty (search-call-location call))
                       "hl" (language (search-call-language call))
                       "gl" (code (search-call-country call) "country"))))
    (if (output-json-p (search-call-output call))
        (let ((found (rows (call-json "google_light" params) "organic_results")))
          (results-json "google_light" query
                        (loop for row in (subseq found 0 (min limit (length found)))
                              for index from 1
                              collect (item-object row index +search-fields+))))
        (limit-table (call-markdown "google_light" params) "Organic Results" limit))))

;;; --- serpapi_maps_search -----------------------------------------------------

(nlk:define-contract maps-call
  (:doc "Search Google Maps for places, local businesses, restaurants and services. Use it when
the user wants physical places, ratings, addresses, opening status or coordinates. Give a location
or a latitude and longitude, never both.")
  ("query" :text :required t :doc "the place or business search, such as 'coffee shops'")
  ("location" :string :doc "the search origin, such as 'New York, NY' or '560001'")
  ("latitude" :number :doc "a precise search-origin latitude; needs longitude")
  ("longitude" :number :doc "a precise search-origin longitude; needs latitude")
  ("zoom" :integer :doc "the map zoom when a location or coordinates are given; 14 by default")
  ("nearby" :boolean :doc "prioritize results close to the search origin")
  ("language" :string :doc "a language code such as en, fil, en-gb or es-419")
  ("country" :string :doc "a two-letter market country code, such as us or in")
  ("limit" :integer :doc "how many places to keep, 1 to 20; 5 when the call names none")
  ("output" :string :options '("md" "json") :doc "md by default; json for structured rows"))

(defparameter +maps-fields+
  '("title" "type" "address" "rating" "reviews" "price" "open_state" "phone" "website" "place_id"))

(defun maps-item (row position)
  (let* ((coordinates (nlk:json-value row :any "gps_coordinates"))
         (pairs (append (list "position" (or (value-of row "position") position))
                        (pick row +maps-fields+)
                        (when (hash-table-p coordinates)
                          (list "coordinates"
                                (nlk:make-json-object
                                 "latitude" (nlk:json-value coordinates :any "latitude")
                                 "longitude" (nlk:json-value coordinates :any "longitude")))))))
    (apply #'nlk:make-json-object pairs)))

(defun tool-maps (call)
  "serpapi_maps_search: google_maps."
  (let* ((query (maps-call-query call))
         (output (maps-call-output call))
         (limit (limit-of (maps-call-limit call)))
         (location (nonempty (maps-call-location call)))
         (latitude (maps-call-latitude call))
         (longitude (maps-call-longitude call)))
    (when (and (or latitude longitude) (not (and latitude longitude)))
      (fail "latitude and longitude must be provided together"))
    (when (and location (or latitude longitude))
      (fail "use either location or latitude/longitude, not both"))
    (when (and (maps-call-nearby call) (null location) (null latitude))
      (fail "nearby requires a location or latitude/longitude"))
    (let* ((zoom (and (or location latitude) (or (maps-call-zoom call) 14)))
           (params (bolt "q" query
                         "type" "search"
                         "hl" (language (maps-call-language call))
                         "gl" (code (maps-call-country call) "country")
                         "location" location
                         "lat" latitude
                         "lon" longitude
                         "z" zoom
                         "nearby" (flag (maps-call-nearby call)))))
      (if (output-json-p output)
          (let ((found (rows (call-json "google_maps" params) "local_results")))
            (results-json "google_maps" query
                          (loop for row in (subseq found 0 (min limit (length found)))
                                for index from 1
                                collect (maps-item row index))))
          (limit-table (call-markdown "google_maps" params) "Local Results" limit)))))

;;; --- serpapi_news_search -----------------------------------------------------

(nlk:define-contract news-call
  (:doc "Search recent Google News results: current events, breaking news and recent reporting.
Answers title, publication, date and snippet for each story.")
  ("query" :text :required t :doc "the news search query")
  ("location" :string :doc "a city-level origin for geographically relevant news")
  ("language" :string :doc "a language code such as en, fil, en-gb or es-419")
  ("country" :string :doc "a two-letter market country code, such as us or in")
  ("limit" :integer :doc "how many stories to keep, 1 to 20; 5 when the call names none")
  ("output" :string :options '("md" "json") :doc "md by default; json for structured rows"))

(defun news-item (row position)
  (apply #'nlk:make-json-object
         (append (list "position" (or (value-of row "position") position))
                 (loop for field in '("title" "link" "date" "snippet")
                       for value = (text-of row field)
                       when value append (list field value))
                 (let ((source (text-of row "source")))
                   (when source (list "source" source))))))

(defun tool-news (call)
  "serpapi_news_search: google_news_light."
  (let* ((query (news-call-query call))
         (limit (limit-of (news-call-limit call)))
         (params (bolt "q" query
                       "location" (nonempty (news-call-location call))
                       "hl" (language (news-call-language call))
                       "gl" (code (news-call-country call) "country"))))
    (if (output-json-p (news-call-output call))
        (let ((found (rows (call-json "google_news_light" params) "news_results")))
          (results-json "google_news_light" query
                        (loop for row in (subseq found 0 (min limit (length found)))
                              for index from 1
                              collect (news-item row index))))
        (limit-table (call-markdown "google_news_light" params) "News Results" limit))))

;;; --- serpapi_shopping_search --------------------------------------------------

(nlk:define-contract shopping-call
  (:doc "Search Google Shopping for products, merchants, prices, ratings and delivery. Use it for
shopping and product comparison; name a country or location for local prices.")
  ("query" :text :required t :doc "the product search query")
  ("location" :string :doc "a city-level origin for local prices and availability")
  ("language" :string :doc "a language code such as en, fil, en-gb or es-419")
  ("country" :string :doc "a two-letter market country code, such as us or in")
  ("minimum_price" :number :doc "the lowest product price, in the market's currency")
  ("maximum_price" :number :doc "the highest product price, in the market's currency")
  ("sort" :string :slot sort-order :options '("relevance" "price_low_to_high" "price_high_to_low")
   :doc "relevance by default, or by price")
  ("free_shipping" :boolean :doc "only products that ship free")
  ("on_sale" :boolean :doc "only products currently on sale")
  ("limit" :integer :doc "how many products to keep, 1 to 20; 5 when the call names none")
  ("output" :string :options '("md" "json") :doc "md by default; json for structured rows"))

(defparameter +shopping-fields+
  '("title" "source" "price" "extracted_price" "old_price" "rating" "reviews" "delivery"
    "snippet" "product_id"))

(defun shopping-item (row position)
  (apply #'nlk:make-json-object
         (append (list "position" (or (value-of row "position") position))
                 (pick row +shopping-fields+)
                 (let ((link (or (value-of row "link") (value-of row "product_link"))))
                   (when link (list "link" link))))))

(defun tool-shopping (call)
  "serpapi_shopping_search: google_shopping_light."
  (let* ((query (shopping-call-query call))
         (output (shopping-call-output call))
         (limit (limit-of (shopping-call-limit call)))
         (minimum (shopping-call-minimum-price call))
         (maximum (shopping-call-maximum-price call)))
    (when (and minimum maximum (> minimum maximum))
      (fail "minimum_price must not exceed maximum_price"))
    (let* ((sort-by (cond ((equal (shopping-call-sort-order call) "price_low_to_high") "1")
                           ((equal (shopping-call-sort-order call) "price_high_to_low") "2")))
           (params (bolt "q" query
                         "location" (nonempty (shopping-call-location call))
                         "hl" (language (shopping-call-language call))
                         "gl" (code (shopping-call-country call) "country")
                         "min_price" minimum
                         "max_price" maximum
                         "sort_by" sort-by
                         "free_shipping" (flag (shopping-call-free-shipping call))
                         "on_sale" (flag (shopping-call-on-sale call)))))
      (if (output-json-p output)
          (let ((found (rows (call-json "google_shopping_light" params) "shopping_results")))
            (results-json "google_shopping_light" query
                          (loop for row in (subseq found 0 (min limit (length found)))
                                for index from 1
                                collect (shopping-item row index))))
          (limit-table (call-markdown "google_shopping_light" params) "Shopping Results" limit)))))

;;; --- serpapi_hotels_search ---------------------------------------------------

(nlk:define-contract hotels-call
  (:doc "Search Google Hotels for hotels, vacation rentals, nightly rates, ratings, amenities and
booking options. Check-in and check-out dates are required; give one age per child when children
travel.")
  ("query" :text :required t :doc "the destination, neighborhood, landmark or property name")
  ("check_in_date" :text :required t :doc "the check-in date, YYYY-MM-DD")
  ("check_out_date" :text :required t :doc "the check-out date, YYYY-MM-DD, after check_in_date")
  ("adults" :integer :doc "how many adults, 1 to 9; 2 by default")
  ("children" :integer :doc "how many children, 0 to 9")
  ("children_ages" :array :of :integer :doc "one age from 1 to 17 per child")
  ("currency" :string :doc "a three-letter currency code, such as USD or EUR")
  ("language" :string :doc "a language code such as en, fil, en-gb or es-419")
  ("country" :string :doc "a two-letter market country code, such as us or in")
  ("minimum_price" :number :doc "the lowest nightly price, in the selected currency")
  ("maximum_price" :number :doc "the highest nightly price, in the selected currency")
  ("hotel_class" :integer :options '(2 3 4 5) :doc "the star class; not with vacation_rentals")
  ("sort" :string :slot sort-order :options '("relevance" "lowest_price" "highest_rating" "most_reviewed")
   :doc "relevance by default")
  ("vacation_rentals" :boolean :doc "search vacation rentals instead of hotels")
  ("output" :string :options '("md" "json") :doc "md by default; json for the whole response"))

(defun tool-hotels (call)
  "serpapi_hotels_search: google_hotels."
  (let* ((query (hotels-call-query call))
         (check-in (date-value (hotels-call-check-in-date call) "check_in_date" :required t))
         (check-out (date-value (hotels-call-check-out-date call) "check_out_date" :required t))
         (minimum (hotels-call-minimum-price call))
         (maximum (hotels-call-maximum-price call))
         (vacation (hotels-call-vacation-rentals call))
         (children (or (hotels-call-children call) 0)))
    (when (string<= check-out check-in)
      (fail "check_out_date must be after check_in_date"))
    (when (and minimum (minusp minimum)) (fail "minimum_price must be at least 0"))
    (when (and maximum (minusp maximum)) (fail "maximum_price must be at least 0"))
    (when (and minimum maximum (> minimum maximum))
      (fail "minimum_price must not exceed maximum_price"))
    (when (and vacation (hotels-call-hotel-class call))
      (fail "hotel_class cannot be used with vacation_rentals"))
    (let* ((sort-by (cond ((equal (hotels-call-sort-order call) "lowest_price") "3")
                           ((equal (hotels-call-sort-order call) "highest_rating") "8")
                           ((equal (hotels-call-sort-order call) "most_reviewed") "13")))
           (params (append (bolt "q" query
                                 "check_in_date" check-in
                                 "check_out_date" check-out
                                 "children_ages" (children-ages (hotels-call-children-ages call)
                                                                 children)
                                 "currency" (currency (hotels-call-currency call))
                                 "hl" (language (hotels-call-language call))
                                 "gl" (code (hotels-call-country call) "country")
                                 "min_price" minimum
                                 "max_price" maximum
                                 "sort_by" sort-by
                                 "hotel_class" (hotels-call-hotel-class call)
                                 "vacation_rentals" (flag vacation))
                           (list (cons "adults" (or (hotels-call-adults call) 2))
                                 (cons "children" children)))))
      (if (output-json-p (hotels-call-output call))
          (nlk:encode-json-object (call-json "google_hotels" params))
          (call-markdown "google_hotels" params)))))

;;; --- serpapi_flights_search --------------------------------------------------

(nlk:define-contract flights-call
  (:doc "Search Google Flights for one-way and round-trip fares. Airport ids are three-letter
airport codes such as LHR or /m/ or /g/ location KGMIDs; city codes such as LON are unsupported.
For a round trip, reuse the route, the dates and a returned departure_token to see return
choices.")
  ("departure_id" :text :required t :doc "the departure airport code or location KGMID")
  ("arrival_id" :text :required t :doc "the arrival airport code or location KGMID")
  ("outbound_date" :text :required t :doc "the outbound date, YYYY-MM-DD")
  ("return_date" :string :doc "the return date, YYYY-MM-DD; a round trip needs it")
  ("trip_type" :string :options '("one_way" "round_trip")
   :doc "one_way or round_trip; inferred from return_date")
  ("travel_class" :string :options '("economy" "premium_economy" "business" "first")
   :doc "the cabin; economy by default")
  ("adults" :integer :doc "how many adults, 1 to 9")
  ("children" :integer :doc "how many children, 0 to 9")
  ("infants_in_seat" :integer :doc "how many infants in their own seats")
  ("infants_on_lap" :integer :doc "how many lap infants; not more than the adults")
  ("currency" :string :doc "a three-letter currency code, such as USD or EUR")
  ("language" :string :doc "a language code such as en, fil, en-gb or es-419")
  ("country" :string :doc "a two-letter market country code, such as us or in")
  ("stops" :string :options '("any" "nonstop" "one_or_fewer" "two_or_fewer")
   :doc "the most stops allowed; any by default")
  ("sort" :string :slot sort-order
   :options '("top_flights" "price" "departure_time" "arrival_time" "duration" "emissions")
   :doc "top_flights by default")
  ("maximum_price" :number :doc "the highest ticket price, in the selected currency")
  ("deep_search" :boolean :doc "match Google Flights more closely at the cost of latency")
  ("departure_token" :string
   :doc "the token of a chosen outbound itinerary; with the original round-trip dates")
  ("output" :string :options '("md" "json") :doc "md by default; json for the whole response"))

(defun travel-class (value)
  (cond ((equal value "premium_economy") "2")
        ((equal value "business") "3")
        ((equal value "first") "4")
        (t "1")))

(defun stops-code (value)
  (cond ((equal value "nonstop") "1")
        ((equal value "one_or_fewer") "2")
        ((equal value "two_or_fewer") "3")
        (t "0")))

(defun tool-flights (call)
  "serpapi_flights_search: google_flights."
  (let* ((departure (travel-ids (flights-call-departure-id call) "departure_id" :required t))
         (arrival (travel-ids (flights-call-arrival-id call) "arrival_id" :required t))
         (outbound (date-value (flights-call-outbound-date call) "outbound_date" :required t))
         (return (date-value (flights-call-return-date call) "return_date"))
         (token (nonempty (flights-call-departure-token call)))
         (trip (or (nonempty (flights-call-trip-type call))
                   (if return "round_trip" "one_way")))
         (maximum (flights-call-maximum-price call)))
    (when (and (equal trip "round_trip") (null return))
      (fail "return_date is required for a round trip"))
    (when (and (equal trip "one_way") return)
      (fail "return_date cannot be used for a one-way trip"))
    (when (and token (not (and (equal trip "round_trip") return)))
      (fail "departure_token requires a round trip with return_date"))
    (when (and return (string< return outbound))
      (fail "return_date must not be before outbound_date"))
    (when (and maximum (minusp maximum)) (fail "maximum_price must be at least 0"))
    (let* ((sort-by (cond ((equal (flights-call-sort-order call) "price") "2")
                           ((equal (flights-call-sort-order call) "departure_time") "3")
                           ((equal (flights-call-sort-order call) "arrival_time") "4")
                           ((equal (flights-call-sort-order call) "duration") "5")
                           ((equal (flights-call-sort-order call) "emissions") "6")
                           (t "1")))
           (params (append (bolt "departure_id" departure
                                 "arrival_id" arrival
                                 "outbound_date" outbound
                                 "return_date" return
                                 "type" (if (equal trip "round_trip") "1" "2")
                                 "travel_class" (travel-class (flights-call-travel-class call))
                                 "currency" (currency (flights-call-currency call))
                                 "hl" (language (flights-call-language call))
                                 "gl" (code (flights-call-country call) "country")
                                 "stops" (stops-code (flights-call-stops call))
                                 "sort_by" sort-by
                                 "max_price" maximum
                                 "deep_search" (flag (flights-call-deep-search call))
                                 "departure_token" token)
                           (passengers (flights-call-adults call)
                                       (flights-call-children call)
                                       :infants-in-seat (flights-call-infants-in-seat call)
                                       :infants-on-lap (flights-call-infants-on-lap call)))))
      (if (output-json-p (flights-call-output call))
          (nlk:encode-json-object (call-json "google_flights" params))
          (call-markdown "google_flights" params)))))

;;; --- serpapi_travel_explore_search -------------------------------------------

(nlk:define-contract travel-call
  (:doc "Explore destinations and flexible trip prices through Google Travel Explore. Use it when
the traveler knows where they leave from but wants destination or date ideas; use
serpapi_flights_search for a fixed route and dates. Give fixed dates or a month, not both.")
  ("departure_id" :text :required t :doc "the departure airport code or city KGMID")
  ("arrival_id" :string :doc "an arrival airport code or city KGMID")
  ("arrival_area_id" :string :doc "a region or country /m/ or /g/ KGMID; not with arrival_id")
  ("outbound_date" :string :doc "a fixed outbound date, YYYY-MM-DD")
  ("return_date" :string :doc "a fixed return date, YYYY-MM-DD")
  ("trip_type" :string :options '("one_way" "round_trip") :doc "inferred from the dates given")
  ("month" :integer :doc "a flexible travel month from 1 to 12; 0 for the next six months")
  ("travel_duration" :string :options '("weekend" "one_week" "two_weeks")
   :doc "the trip length for a flexible-date search; one_week by default")
  ("travel_class" :string :options '("economy" "premium_economy" "business" "first")
   :doc "the cabin; economy by default")
  ("adults" :integer :doc "how many adults, 1 to 9")
  ("children" :integer :doc "how many children, 0 to 9")
  ("infants_in_seat" :integer :doc "how many infants in their own seats")
  ("infants_on_lap" :integer :doc "how many lap infants; not more than the adults")
  ("currency" :string :doc "a three-letter currency code, such as USD or EUR")
  ("language" :string :doc "a language code such as en, fil, en-gb or es-419")
  ("country" :string :doc "a two-letter market country code, such as us or in")
  ("stops" :string :options '("any" "nonstop" "one_or_fewer" "two_or_fewer")
   :doc "the most stops allowed; any by default")
  ("maximum_price" :number :doc "the highest flight price, in the selected currency")
  ("output" :string :options '("md" "json") :doc "md by default; json for the whole response"))

(defun tool-travel (call)
  "serpapi_travel_explore_search: google_travel_explore."
  (let* ((departure (travel-ids (travel-call-departure-id call) "departure_id" :required t))
         (arrival (travel-ids (travel-call-arrival-id call) "arrival_id"))
         (area (nonempty (travel-call-arrival-area-id call)))
         (outbound (date-value (travel-call-outbound-date call) "outbound_date"))
         (return (date-value (travel-call-return-date call) "return_date"))
         (month (travel-call-month call))
         (maximum (travel-call-maximum-price call)))
    (when (and arrival area) (fail "use either arrival_id or arrival_area_id, not both"))
    (when (and area (not (member (subseq area 0 (min 3 (length area))) '("/m/" "/g/")
                                 :test #'string=)))
      (fail "arrival_area_id must be a /m/ or /g/ region or country KGMID"))
    (when (and return (null outbound))
      (fail "outbound_date is required when return_date is provided"))
    (when (and return (string< return outbound))
      (fail "return_date must not be before outbound_date"))
    (when (and month (not (<= 0 month 12))) (fail "month must be between 0 and 12"))
    (when (and outbound month (not (zerop month)))
      (fail "use fixed dates or month, not both"))
    (when (and maximum (minusp maximum)) (fail "maximum_price must be at least 0"))
    (let* ((trip (or (nonempty (travel-call-trip-type call))
                      (if (or return (null outbound)) "round_trip" "one_way")))
           (duration (cond ((equal (travel-call-travel-duration call) "weekend") "1")
                           ((equal (travel-call-travel-duration call) "two_weeks") "3")
                           (t "2")))
           (params (append (bolt "departure_id" departure
                                 "arrival_id" arrival
                                 "arrival_area_id" area
                                 "outbound_date" outbound
                                 "return_date" return
                                 "type" (if (equal trip "round_trip") "1" "2")
                                 "month" month
                                 "travel_duration" (and (null outbound) duration)
                                 "travel_class" (travel-class (travel-call-travel-class call))
                                 "currency" (currency (travel-call-currency call))
                                 "hl" (language (travel-call-language call))
                                 "gl" (code (travel-call-country call) "country")
                                 "stops" (stops-code (travel-call-stops call))
                                 "max_price" maximum)
                           (passengers (travel-call-adults call)
                                       (travel-call-children call)
                                       :infants-in-seat (travel-call-infants-in-seat call)
                                       :infants-on-lap (travel-call-infants-on-lap call)))))
      (if (output-json-p (travel-call-output call))
          (nlk:encode-json-object (call-json "google_travel_explore" params))
          (call-markdown "google_travel_explore" params)))))

;;; --- serpapi_extract ---------------------------------------------------------

(nlk:define-contract extract-call
  (:doc "Scrape a public page, map a site's urls, or crawl several pages through SerpApi. Compact
Markdown by default; set compact_mode to false for the full structured JSON. Crawl costs one
search credit per successfully extracted page, so bound it with max_pages. It does not run
JavaScript and does not read a private address.")
  ("url" :text :required t :doc "the absolute http or https url of the page or the starting page")
  ("mode" :string :options '("scrape" "map" "crawl") :default "scrape"
   :doc "scrape one page, map the site's urls, or crawl the site's pages")
  ("compact_mode" :boolean :default t
   :doc "true answers Markdown or a url list; false the full structured Extract response")
  ("include_tags" :string :doc "comma-separated CSS selectors to keep, such as 'article,main'")
  ("exclude_tags" :string :doc "comma-separated CSS selectors to remove, such as 'nav,footer'")
  ("max_pages" :integer :doc "crawl only: the page ceiling, 1 to 100, the start page included")
  ("max_depth" :integer :doc "crawl only: the link depth, 0 to 3; 0 is the start page alone")
  ("include_subdomains" :boolean :doc "map or crawl only: allow other subdomains of the site")
  ("include_sitemap" :boolean :doc "map only: read /sitemap.xml and /robots.txt; on by default"))

(defun normalize-url (value)
  "VALUE as the absolute, credential-free http or https url the request carries."
  (let ((text (string-trim " " (or value ""))))
    (unless (plusp (length text)) (fail "serpapi_extract: url must not be empty"))
    (let ((uri (handler-case (quri:uri text) (error () nil))))
      (unless (and uri
                   (member (quri:uri-scheme uri) '("http" "https") :test #'string-equal)
                   (nonempty (quri:uri-host uri))
                   (null (quri:uri-userinfo uri))
                   (notany (lambda (character) (member character '(#\Space #\Tab #\Newline #\Return)))
                           text))
        (fail "serpapi_extract: url must be an absolute http or https URL without credentials"))
      (quri:render-uri uri))))

(defun extract-params (call url mode)
  (let ((pages (extract-call-max-pages call))
        (depth (extract-call-max-depth call))
        (subdomains (extract-call-include-subdomains call))
        (sitemap (extract-call-include-sitemap call)))
    (when (and pages (not (equal mode "crawl")))
      (fail "max_pages is only available with mode='crawl'"))
    (when (and depth (not (equal mode "crawl")))
      (fail "max_depth is only available with mode='crawl'"))
    (when (and subdomains (not (member mode '("map" "crawl") :test #'equal)))
      (fail "include_subdomains is only available with mode='map' or 'crawl'"))
    (when (and sitemap (not (equal mode "map")))
      (fail "include_sitemap is only available with mode='map'"))
    (when (and pages (not (<= 1 pages 100)))
      (fail "max_pages must be from 1 to 100"))
    (when (and depth (not (<= 0 depth 3)))
      (fail "max_depth must be from 0 to 3"))
    (bolt "url" url
          "mode" mode
          "max_pages" pages
          "max_depth" depth
          "include_subdomains" (flag subdomains)
          "include_sitemap" (flag sitemap)
          "include_tags" (nonempty (extract-call-include-tags call))
          "exclude_tags" (nonempty (extract-call-exclude-tags call)))))

(defun page-text (page &optional (max-chars nil))
  "PAGE's own Markdown, or its Error when it carries one."
  (let ((error (text-of page "error")))
    (if error
        (format nil "Error: ~a" error)
        (let ((markdown (nlk:json-value page :text "markdown")))
          (if (and markdown (plusp (length (nlk:trimmed markdown))))
              (if max-chars (clip (string-trim '(#\Newline) markdown) max-chars)
                  (string-trim '(#\Newline) markdown))
              "Error: SerpApi returned no page content for this URL")))))

(defun map-text (body)
  (let ((urls (nlk:json-value body :array "urls")))
    (if (and urls (plusp (length urls)))
        (format nil "~{~a~^~%~}"
                (loop for entry across urls
                      for index from 1
                      when (hash-table-p entry)
                        collect (let ((error (text-of entry "error"))
                                      (url (text-of entry "url"))
                                      (title (text-of entry "title")))
                                  (cond (error (format nil "- URL ~d: Error: ~a" index error))
                                        (title (format nil "- ~a - ~a" url title))
                                        (t (format nil "- ~a" (or url "")))))))
        "No URLs found.")))

(defun crawl-text (body max-chars)
  (let ((pages (nlk:json-value body :array "pages")))
    (if (and pages (plusp (length pages)))
        (format nil "~{~a~^~%~%~}"
                (loop for page across pages
                      when (hash-table-p page)
                        collect (format nil "## ~a~%~%~a"
                                        (or (text-of page "url") "page")
                                        (page-text page max-chars))))
        "No pages extracted.")))

(defun routed-text (body engine url)
  (format nil "## ~a results~%~%~a~%~%```json~%~a~%```"
          engine url
          (let ((rest (nlk:make-json-object)))
            (maphash (lambda (key value)
                       (unless (member key '("search_metadata" "search_parameters"
                                             "serpapi_pagination")
                                       :test #'string=)
                         (setf (gethash key rest) value)))
                     body)
            (nlk:encode-json-object rest))))

(defun extract-text (body mode url max-chars)
  (let ((engine (nlk:json-value body :text "search_parameters" "engine")))
    (cond ((and engine (not (equal engine "extract"))) (routed-text body engine url))
          ((equal mode "map") (map-text body))
          ((equal mode "crawl") (crawl-text body max-chars))
          (t (let ((markdown (nlk:json-value body :text "markdown")))
               (if (and markdown (plusp (length (nlk:trimmed markdown))))
                   (string-trim '(#\Newline) markdown)
                   (fail "serpapi_extract: SerpApi returned no page content for this URL")))))))

(defun tool-extract (call)
  "serpapi_extract: engine extract, whose scrape, map and crawl modes share one tool."
  (let* ((url (normalize-url (extract-call-url call)))
         (mode (or (nonempty (extract-call-mode call)) "scrape"))
         (params (extract-params call url mode))
         (body (call-json "extract" params :timeout (setting :extract-timeout-seconds))))
    (if (extract-call-compact-mode call)
        (extract-text body mode url (setting :max-chars))
        (nlk:encode-json-object body))))
