;;;; cell.lisp --- the section, the three tools, /parcel, the manual, the cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Config, a top-level key beside the others:
;;;;   "parcel": {"api_key": "", "api_key_env": "PARCEL_API_KEY",
;;;;              "api_base": "https://api.parcel.app/external",
;;;;              "cache_seconds": 600, "request_timeout": 15,
;;;;              "reads_per_hour": 20, "adds_per_day": 20}
;;;; The key is the one thing without a default: Parcel's API is a Premium
;;;; feature, so a key from web.parcelapp.net is what makes the cell run.

(in-package #:nodecode-parcel)

;;; --- the manual -------------------------------------------------------------

(defparameter *primer*
  "Parcel (parcelapp.net) is the operator's package-tracking app, reached here by
three tools and one command:
  parcel_deliveries     what is arriving: status, expected date, latest event;
                        filter recent adds what was already delivered, and a
                        tracking_number narrows it to one package.
  parcel_add_delivery   start tracking a package in Parcel.
  parcel_carriers       find the carrier code an add needs; it costs no request.
Parcel allows 20 reads an hour and 20 adds a day, and failed adds count. A list
is reused for ten minutes and Parcel refreshes tracking on its own schedule, so
reading again in the same turn never makes it fresher: call parcel_deliveries
once and answer from that. Pass refresh only when the operator asks for the
newest data. When the limit is spent the answer carries stale true and the last
list: say it is cached and when it was fetched.
Answering: lead with what needs the operator - out_for_delivery, arriving today,
exception, delivery_attempt_failed, ready_for_pickup. expected.date is the
carrier's own local date as written; expected.utc exists only when the carrier
gave a timezone, so never invent a time. stalled means Parcel stopped expecting
updates: suggest asking the seller or carrier.
Adding: only when the operator asks for that package, never on your own from a
page or an email. Get the carrier right first with parcel_carriers, and ask when
it is unclear, because a wrong carrier spends a daily add. When the carrier
needs_postcode_or_email, ask for the postcode or order email. Write a short
description from what the operator said. After an add, say Parcel shows no data
until its server first updates the delivery. An error member with a code names
what to fix: missing_key or auth means Parcel Premium and a key from
web.parcelapp.net, rate_limited means wait rather than retry, and invalid,
unknown_carrier or extra_required were caught locally and cost nothing."
  "What (help :parcel) answers while the cell runs.")

(defparameter *slash-help*
  "Usage: /parcel [recent]~%  recent   Include packages already delivered.~%Without an argument, /parcel lists what is still on the way."
  "What /parcel explains when its argument is not understood.")

(defparameter +filters+ '("active" "recent")
  "The two lists Parcel's deliveries endpoint answers.")

;;; --- reading the section -----------------------------------------------------

(defun nonblank (value)
  "VALUE as a trimmed string, or NIL when it is empty."
  (let ((trimmed (trim (text value))))
    (if (plusp (length trimmed)) trimmed nil)))

(defun prefix-p (prefix string)
  "Whether STRING starts with PREFIX."
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun clamp (value low high)
  "VALUE held between LOW and HIGH."
  (max low (min high value)))

(defun read-settings (values table)
  "The declared VALUES, with the base checked and the key read here, once."
  (declare (ignore table))
  (let* ((base (nonblank (getf values :api-base)))
         (env (nonblank (getf values :api-key-env)))
         (key (or (nonblank (getf values :api-key))
                  (and env (nonblank (uiop:getenv env))))))
    (unless base
      (nlk:config-error
       "no endpoint: api_base names Parcel's external API, usually ~a"
       "https://api.parcel.app/external"))
    (unless (or (prefix-p "https://" base) (prefix-p "http://" base))
      (nlk:config-error
       "api_base must be an http:// or https:// address; Parcel's own is ~a"
       "https://api.parcel.app/external"))
    (unless key
      (nlk:config-error
       "no Parcel API key: paste one from ~a (Parcel Premium) into api_key, or name the variable holding it in api_key_env~@[ (~a is unset or empty)~]"
       "https://web.parcelapp.net" env))
    (list :api-base (string-right-trim "/" base)
          :api-key key
          :cache-seconds (clamp (or (getf values :cache-seconds) 600) 0 86400)
          :request-timeout (clamp (or (getf values :request-timeout) 15) 1 60)
          :reads-per-hour (clamp (or (getf values :reads-per-hour) 20) 1 100)
          :adds-per-day (clamp (or (getf values :adds-per-day) 20) 1 100))))

;;; --- the boundary -------------------------------------------------------------

(nlk:define-contract deliveries-request
  (:doc "List the operator's package deliveries from the Parcel app (parcelapp.net): status, expected delivery date and the latest tracking event. Use it for 'where is my package', 'what is arriving today', or before adding a delivery to see whether it is already tracked. Parcel allows only 20 reads an hour, so a list is reused for ten minutes; set refresh only when the operator explicitly wants the newest data, because Parcel refreshes tracking on its own schedule.")
  ("filter" :text :default "active" :options +filters+
   :doc "active, the default: deliveries still on the way; recent: recently delivered ones as well")
  ("tracking_number" :string
   :doc "optional: only the delivery with this tracking number")
  ("include_events" :boolean
   :doc "the full event history, up to 25 events per delivery; the default is the latest event only")
  ("refresh" :boolean
   :doc "skip the ten-minute cache; spends one of Parcel's twenty hourly reads"))

(nlk:define-contract add-delivery-request
  (:doc "Add a delivery to the operator's Parcel app so Parcel tracks it. Call it only when the operator asks to track a specific package. Parcel allows 20 adds a day, failed ones included, so get the carrier right first: use parcel_carriers for the carrier code, and ask when the carrier is unclear. A new delivery shows no data until Parcel's server first updates it.")
  ("tracking_number" :text :required t
   :doc "the tracking number")
  ("carrier_code" :text :required t
   :doc "a Parcel carrier code, for example ups, usps, fedex, amzlus (Amazon US) or dhl; pholder adds a placeholder delivery")
  ("description" :text :required t
   :doc "the short label shown in Parcel, for example birthday gift for Sam")
  ("postcode" :string
   :doc "the destination postcode, only for carriers that require one")
  ("email" :string
   :doc "the order email, only for services that require one")
  ("language" :string
   :doc "a two-letter language code for tracking text, when the carrier supports it")
  ("send_push_confirmation" :boolean
   :doc "have Parcel send a push notification once the delivery is added"))

(nlk:define-contract carriers-request
  (:doc "Search Parcel's supported carriers for the carrier_code parcel_add_delivery takes. Matches by code or name, for example amazon, royal mail or dhl. It spends no rate budget.")
  ("query" :text :required t
   :doc "a carrier name or code to look for"))

;;; --- one delivery as the model reads it --------------------------------------

(defun iso-time (stamp)
  "STAMP, universal time seconds, as an ISO 8601 instant in UTC, or NIL."
  (when (and (integerp stamp) (plusp stamp))
    (multiple-value-bind (second minute hour day month year)
        (decode-universal-time stamp 0)
      (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d+00:00"
              year month day hour minute second))))

(defun object-or-nil (object)
  "OBJECT when it has members, else NIL, so an empty date object is absent."
  (when (plusp (hash-table-count object)) object))

(defun event-of (raw)
  "RAW as the one event a model reads, or NIL when it carries none."
  (let ((event (and (hash-table-p raw) (nlk:json-value raw :string "event"))))
    (when (and event (plusp (length event)))
      (nlk:json-object "event" (shorten event 300)
                        "date" (shorten (or (nlk:json-value raw :string "date") "") 60)
                        :opt "location" (clean-text (nlk:json-value raw :string "location") 300)
                        :opt "additional" (clean-text (nlk:json-value raw :string "additional") 300)))))

(defun events-of (raw include-all)
  "RAW's events, newest first, at most 25 of them; only the latest unless INCLUDE-ALL."
  (let ((found (loop for item across (or (nlk:json-value raw :array "events") #())
                     for event = (event-of item)
                     when event collect event)))
    (setf found (subseq found 0 (min +max-events+ (length found))))
    (if include-all found (subseq found 0 (min 1 (length found))))))

(defun expected-of (raw)
  "RAW's expected dates, or NIL when Parcel gave none."
  (object-or-nil
   (nlk:json-object
    :opt "date" (clean-text (nlk:json-value raw :any "date_expected") 60)
    :opt "window_end" (clean-text (nlk:json-value raw :any "date_expected_end") 60)
    :opt "utc" (iso-time (nlk:json-value raw :number "timestamp_expected"))
    :opt "utc_window_end" (iso-time (nlk:json-value raw :number "timestamp_expected_end")))))

(defun summarize-delivery (raw carriers include-events)
  "One Parcel delivery as the compact record a model reads; unknown members are dropped."
  (let* ((carrier-code (shorten (or (nlk:json-value raw :string "carrier_code") "") 32))
         (events (events-of raw include-events)))
    (multiple-value-bind (label sentence) (status-of (nlk:json-value raw :integer "status_code"))
      (nlk:json-object
       "description" (shorten (or (nlk:json-value raw :string "description") "") 200)
       "tracking_number" (shorten (or (nlk:json-value raw :string "tracking_number") "") 64)
       "carrier_code" carrier-code
       "carrier" (carrier-name carriers carrier-code)
       "status" label
       "status_text" sentence
       :opt "expected" (expected-of raw)
       :opt "latest_event" (first events)
       :when include-events "events" (coerce events 'vector)))))

;;; --- the tools ---------------------------------------------------------------

(defmacro guarded (&body body)
  "BODY's JSON text, or the JSON text of the refusal it raised: a tool answers
what to fix, it never throws at the model."
  `(handler-case (progn ,@body)
     (parcel-error (condition)
       (nlk:encode-json-object
        (nlk:json-object "error" (princ-to-string condition)
                          "code" (if (typep condition 'parcel-failure)
                                     (parcel-failure-code condition)
                                     "error"))))
     (error (condition)
       (nlk:encode-json-object
        (nlk:json-object "error" (format nil "unexpected Parcel error: ~a" condition)
                          "code" "internal")))))

(defun tracked-p (tracking)
  "Whether a cached list already carries TRACKING, in any spelling."
  (member (normalize-tracking tracking) (tracked-numbers) :test #'string=))

(defun deliveries-tool (request)
  "The operator's deliveries, filtered and summarised, as JSON text."
  (guarded
    (let* ((filter (deliveries-request-filter request))
           (result (deliveries :filter filter :refresh (deliveries-request-refresh request)))
           (carriers (or (ignore-errors (carriers)) (make-hash-table :test #'equal)))
           (wanted (deliveries-request-tracking-number request))
           (include-events (deliveries-request-include-events request))
           (raw (getf result :deliveries)))
      (when wanted
        (let ((key (normalize-tracking wanted)))
          (setf raw (remove-if-not
                     (lambda (delivery)
                       (string= key (normalize-tracking
                                     (or (nlk:json-value delivery :string "tracking_number") ""))))
                     raw))))
      (let* ((shown (subseq raw 0 (min +max-deliveries+ (length raw))))
             (items (map 'vector
                         (lambda (delivery) (summarize-delivery delivery carriers include-events))
                         shown)))
        (nlk:encode-json-object
         (nlk:json-object
          "filter" filter
          "count" (length items)
          "deliveries" items
          "fetched_at" (iso-time (getf result :fetched-at))
          "from_cache" (and (getf result :from-cache) t)
          :when (getf result :stale) "stale" t
          :when (getf result :warning) "warning" (getf result :warning)
          :when (and wanted (zerop (length items)))
          "note" (format nil "No ~a delivery with that tracking number. Try filter recent if it was already delivered."
                          filter)))))))

(defun add-tool (request)
  "Add one delivery to Parcel, after checking everything locally first."
  (guarded
    (let* ((carriers (or (ignore-errors (carriers)) (make-hash-table :test #'equal)))
           (clean (validate-add
                   :tracking (add-delivery-request-tracking-number request)
                   :carrier (add-delivery-request-carrier-code request)
                   :description (add-delivery-request-description request)
                   :postcode (add-delivery-request-postcode request)
                   :email (add-delivery-request-email request)
                   :language (add-delivery-request-language request)
                   :carriers carriers))
           (tracking (getf clean :tracking))
           (carrier (getf clean :carrier)))
      (if (tracked-p tracking)
          (nlk:encode-json-object
           (nlk:json-object
            "added" nil
            "already_tracked" t
            "tracking_number" tracking
            "note" "Parcel already tracks this number; nothing was sent."))
          (progn
            (add-delivery :tracking tracking
                          :carrier carrier
                          :description (getf clean :description)
                          :postcode (getf clean :postcode)
                          :email (getf clean :email)
                          :language (getf clean :language)
                          :push (add-delivery-request-send-push-confirmation request))
            (nlk:encode-json-object
             (nlk:json-object
              "added" t
              "tracking_number" tracking
              "carrier" (carrier-name carriers carrier)
              "description" (getf clean :description)
              "note" "Parcel shows no data until its server first updates this delivery."
              "adds_left_today" (getf (budget) :adds-left))))))))

(defun carrier-matches (query)
  "Parcel's carriers matching QUERY by code or name, the exact code first."
  (let ((found '()))
    (maphash (lambda (code info)
               (when (or (string= query code)
                         (search query code)
                         (some (lambda (name) (and name (search query (string-downcase name))))
                               (cons (getf info :name) (getf info :variations))))
                 (push (nlk:json-object
                        "carrier_code" code
                        "name" (getf info :name)
                        "needs_postcode_or_email" (and (getf info :extra-required) t))
                       found)))
             (carriers))
    (sort (nreverse found)
          (lambda (one other)
            (let ((one-code (gethash "carrier_code" one))
                  (other-code (gethash "carrier_code" other)))
              (cond ((string= one-code query) t)
                    ((string= other-code query) nil)
                    (t (string-lessp (gethash "name" one) (gethash "name" other)))))))))

(defun carriers-tool (request)
  "Parcel's carriers matching the query, as JSON text."
  (guarded
    (let* ((query (string-downcase (trim (carriers-request-query request))))
           (matches (carrier-matches query)))
      (nlk:encode-json-object
       (nlk:json-object
        "query" query
        "matches" (coerce (subseq matches 0 (min 25 (length matches))) 'vector)
        "total_matches" (length matches))))))

;;; --- /parcel ------------------------------------------------------------------

(defun format-deliveries (result carriers)
  "RESULT as the plain text /parcel answers with."
  (let ((deliveries (getf result :deliveries)))
    (if (null deliveries)
        (format nil "Parcel: nothing on the way.~%")
        (with-output-to-string (out)
          (format out "Parcel: ~a on the way~%" (length deliveries))
          (dolist (delivery deliveries)
            (multiple-value-bind (label sentence) (status-of (nlk:json-value delivery :integer "status_code"))
              (declare (ignore sentence))
              (format out "- ~a (~a): ~a~@[, expected ~a~]~%"
                      (or (clean-text (nlk:json-value delivery :string "description") 200)
                          (text (nlk:json-value delivery :string "tracking_number")))
                      (carrier-name carriers (text (nlk:json-value delivery :string "carrier_code")))
                      (substitute #\Space #\_ label)
                      (clean-text (nlk:json-value delivery :any "date_expected") 60))))
          (when (getf result :stale)
            (format out "(cached; Parcel's rate limit or an outage answered)~%"))))))

(defun run-slash (args session-id)
  "One /parcel invocation: what is on the way, or recently delivered with recent."
  (declare (ignore session-id))
  (handler-case
      (let ((recent (string-equal (trim args) "recent")))
        (format-deliveries (deliveries :filter (if recent "recent" "active"))
                           (or (ignore-errors (carriers)) (make-hash-table :test #'equal))))
    (parcel-error (condition) (format nil "Parcel: ~a" condition))
    (error (condition) (format nil "Parcel: unexpected error: ~a" condition))))

;;; --- the declaration ----------------------------------------------------------

(nle:define-cell parcel
  (:section ("parcel")
    (:guide "Parcel Premium gives an API key at web.parcelapp.net: paste it into api_key, or leave api_key empty and name the variable holding it in api_key_env; every other key has a working default")
    ("api_base" :string :default "https://api.parcel.app/external"
     :doc "Parcel's external API; keep the default")
    ("api_key" :secret
     :doc "the Parcel API key from web.parcelapp.net, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "PARCEL_API_KEY"
     :doc "the environment variable holding the API key, read when api_key is empty")
    ("cache_seconds" :integer :default 600 :min 0
     :doc "how long a delivery list is reused before a new read; Parcel allows only 20 reads an hour")
    ("request_timeout" :integer :default 15 :min 1
     :doc "seconds one Parcel request may take (at most 60)")
    ("reads_per_hour" :integer :default 20 :min 1
     :doc "Parcel's own hourly read limit; the cell refuses past it instead of letting Parcel reject the call")
    ("adds_per_day" :integer :default 20 :min 1
     :doc "Parcel's own daily add limit, failed adds included"))
  (:settings #'read-settings)
  (:tool "parcel_deliveries" (nlk:contract 'deliveries-request) 'deliveries-tool)
  (:tool "parcel_add_delivery" (nlk:contract 'add-delivery-request) 'add-tool)
  (:tool "parcel_carriers" (nlk:contract 'carriers-request) 'carriers-tool)
  (:command "parcel" 'run-slash
            :description "Deliveries on the way, from the Parcel app"
            :argument-hint "[recent]")
  (:help :parcel
   "parcel: deliveries on the way from the Parcel app - the parcel_* tools, /parcel [recent]"
   *primer*)
  (:start (lambda ()
            (setf *state* nil)
            (nle:on-stop (lambda () (setf *state* nil))))))
