;;;; tools.lisp --- what a model call carries, and the Bento call it becomes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's schemas.py, tools_read.py, tools_write.py and
;;;; tools_send.py in Lisp. The same fifteen calls, the same endpoints and
;;;; the same bodies; the boundary is a contract, so the schema a model reads
;;;; and the record the body reads cannot disagree. A read is answered as
;;;; Bento's own JSON under a line saying it is data; every write is TIERED:
;;;; T0/T1 goes, T2/T3 previews first and needs its confirm_token.

(in-package #:nodecode-bento)

;;; --- what a call carries -----------------------------------------------------------

(nlk:define-contract subscriber-lookup
  (:doc "Look up ONE Bento subscriber by email or uuid. Returns fields, tags and
unsubscribed_at (null means currently subscribed). Read-only.")
  ("email" :text :doc "the subscriber's email address")
  ("uuid" :text :doc "a subscriber uuid, when the email is not known"))

(nlk:define-contract subscriber-import
  (:doc "Import or upsert 1-1000 subscribers with names, tags and custom fields. Does
NOT trigger Flows. Async: it can take five minutes to appear. Preview with dry_run first.")
  ("subscribers" :array :required t
   :doc "one object an address: email, first_name, last_name, tags, remove_tags, fields, utm_*, created_at, lifetime_value_currency, lifetime_value_amount")
  ("dry_run" :boolean :default t
   :doc "preview only: answer the exact request and a confirm_token, send nothing")
  ("confirm_token" :text :doc "the token a dry run of these IDENTICAL parameters answered"))

(nlk:define-contract subscriber-tag
  (:doc "Add or remove ONE tag on ONE subscriber (an async command: Bento answers a
queued count, not the updated person). Does not trigger automations.")
  ("email" :text :required t :doc "the subscriber's email address")
  ("action" :text :required t :options '("add" "remove") :doc "add or remove")
  ("tag" :text :required t :doc "the tag name, with no comma"))

(nlk:define-contract tag-definition
  (:doc "Create a tag DEFINITION; it is not applied to anyone. Refuses a name that
already exists. List the tags first.")
  ("name" :text :required t :doc "lowercase letters, digits, - and _, at most 64"))

(nlk:define-contract field-definition
  (:doc "Create a custom field DEFINITION (no values). Refuses a key that already
exists. List the fields first.")
  ("key" :text :required t :doc "a lowercase_snake_case key, at most 64"))

(nlk:define-contract consent
  (:doc "Subscribe or unsubscribe ONE person. subscribe RE-SUBSCRIBES someone who
opted out, so it needs a written reason. Preview with dry_run first.")
  ("email" :text :required t :doc "the person's email address")
  ("action" :text :required t :options '("subscribe" "unsubscribe")
   :doc "subscribe or unsubscribe")
  ("reason" :text :slot why :doc "required for subscribe: where their consent came from")
  ("dry_run" :boolean :default t :doc "preview only, with a confirm_token")
  ("confirm_token" :text :doc "the token a dry run of these IDENTICAL parameters answered"))

(nlk:define-contract event-batch
  (:doc "Track 1-1000 custom events. Creates unknown subscribers and CAN TRIGGER
FLOWS that send real email, so it is gated. Purchases go through bento_purchase_track.")
  ("events" :array :required t
   :doc "one object an event: type, email, fields, details, date")
  ("dry_run" :boolean :default t :doc "preview only, with a confirm_token")
  ("confirm_token" :text :doc "the token a dry run of these IDENTICAL parameters answered"))

(nlk:define-contract purchase
  (:doc "Record ONE $purchase so revenue and lifetime value update. order_id is the
dedupe key: pass the real order id, never an invented one. Amounts are integer CENTS.
Triggers purchase Flows, so it is gated.")
  ("email" :text :required t :doc "the buyer's email address")
  ("order_id" :text :required t :doc "the stable order id, kept as details.unique.key")
  ("amount_cents" :integer :required t :doc "the order total in whole cents")
  ("currency" :text :required t :doc "a three-letter ISO-4217 code such as USD")
  ("items" :array :doc "one object a line: product_sku, product_name, quantity, price_cents")
  ("fields" :object :doc "flat custom field values to set on the buyer")
  ("date" :text :doc "when it happened, ISO-8601 UTC; default now")
  ("dry_run" :boolean :default t :doc "preview only, with a confirm_token")
  ("confirm_token" :text :doc "the token a dry run of these IDENTICAL parameters answered"))

(nlk:define-contract broadcast-list
  (:doc "List broadcasts by status. With no status Bento answers only SENT ones;
pass status=draft to find drafts. Read-only.")
  ("status" :text :options '("sent" "draft" "sending" "scheduled" "paused" "canceled")
   :doc "which broadcasts to list")
  ("page" :integer :doc "a page number, from 1"))

(nlk:define-contract broadcast
  (:doc "Create a DRAFT broadcast, or - with bento_broadcast_schedule - create AND
APPROVE one for send_at. An explicit audience is mandatory: inclusive_tags or
segment_id, never everyone.")
  ("name" :text :required t :doc "an internal broadcast name")
  ("subject" :text :required t :doc "the subject line")
  ("content" :text :required t :doc "the body (Liquid allowed)")
  ("type" :text :slot kind :options '("plain" "raw")
   :doc "plain for Bento's styling, raw for your whole HTML document")
  ("from_name" :text :required t :doc "the sender name")
  ("from_email" :text :required t :doc "a verified Bento Author address")
  ("inclusive_tags" :array :doc "send to subscribers having these tags; give this or segment_id")
  ("segment_id" :text :doc "send to this Bento segment; give this or inclusive_tags")
  ("exclusive_tags" :array :doc "leave out subscribers having any of these tags")
  ("batch_size_per_hour" :integer :doc "send pacing; required when scheduling")
  ("preview_text" :text :doc "the inbox preview snippet")
  ("send_at" :text :doc "UTC start time, at least the section's lead time ahead, at most 90 days out")
  ("acknowledge_unknown_audience" :boolean
   :doc "the real call to a tag audience needs this: Bento cannot report its size")
  ("dry_run" :boolean :default t :doc "defaults to true: preview and a confirm_token")
  ("confirm_token" :text :doc "the token a dry run of these IDENTICAL parameters answered"))

(nlk:define-contract email-send
  (:doc "Send transactional email through Bento (at most 60 a call). Real email to
real people: it previews first and needs its confirm_token and dry_run=false. 'from'
must be a verified Bento Author listed in allowed_senders. Accepted is not delivered.")
  ("from" :text :required t :slot sender :doc "a verified Bento Author address")
  ("emails" :array :required t
   :doc "one object a message: to, subject, html_body, personalizations")
  ("transactional" :boolean :doc "true sends even to unsubscribed people")
  ("reason" :text :slot why :doc "required when transactional is true")
  ("allow_duplicate" :boolean :doc "send even if the same message went out in the last 24 hours")
  ("dry_run" :boolean :default t :doc "defaults to true: preview and a confirm_token")
  ("confirm_token" :text :doc "the token a dry run of these IDENTICAL parameters answered"))

(nlk:define-contract stats-request
  (:doc "Read site, segment, report or ads statistics (cached; money values are
cents). Read-only.")
  ("kind" :text :required t :options '("site" "segment" "report" "ads")
   :doc "which statistics: site, segment, report or ads")
  ("segment_id" :text :doc "the segment, for kind=segment")
  ("report_id" :text :doc "the report, for kind=report")
  ("dimension" :text :doc "for kind=ads: the dimension to break the answer down by")
  ("revenue_mode" :text :doc "for kind=ads: how revenue is counted")
  ("value" :text :doc "for kind=ads: the dimension value to filter on")
  ("start_date" :text :doc "for kind=ads: the first day, YYYY-MM-DD")
  ("end_date" :text :doc "for kind=ads: the last day, YYYY-MM-DD"))

(nlk:define-contract no-request
  (:doc "List the site's whole set: tags or custom fields, cached five minutes.")
  ("refresh" :boolean :doc "read past the cache when it is true"))

;;; --- reading a raw object ------------------------------------------------------------

(defun text-of (object key)
  "KEY's member of OBJECT as a string, or NIL."
  (and (hash-table-p object) (nlk:json-value object :string key)))

(defun object-of (object key)
  "KEY's member of OBJECT as an object, or NIL."
  (and (hash-table-p object) (nlk:json-value object :object key)))

(defun array-of (object key)
  "KEY's member of OBJECT as a vector, or NIL."
  (and (hash-table-p object) (nlk:json-value object :array key)))

(defparameter +subscriber-attributes+
  '("email" "first_name" "last_name" "tags" "remove_tags" "created_at"
    "lifetime_value_currency" "lifetime_value_amount"
    "utm_source" "utm_medium" "utm_campaign" "utm_content" "utm_term")
  "The members a subscriber object carries itself; a fields key may not be one.")

;;; --- the bodies the original built ----------------------------------------------------

(defun subscriber-entry (item index)
  "ITEM, one subscriber of an import, as the object Bento takes."
  (unless (hash-table-p item)
    (fail "subscribers[~d] must be an object" index))
  (let* ((email (email-address (text-of item "email")
                                :what (format nil "subscribers[~d].email" index)))
         (entry (nlk:json-object "email" email)))
    (dolist (key '("first_name" "last_name" "utm_source" "utm_medium" "utm_campaign"
                   "utm_content" "utm_term"))
      (let ((value (nonblank (text-of item key))))
        (when value (setf (gethash key entry) value))))
    (dolist (key '("tags" "remove_tags"))
      (let ((names (tag-list! (array-of item key)
                              :what (format nil "subscribers[~d].~a" index key))))
        (when names (setf (gethash key entry) (join-comma names)))))
    (let ((created (nonblank (text-of item "created_at"))))
      (when created
        (setf (gethash "created_at" entry)
              (iso-of (iso-epoch created :what (format nil "subscribers[~d].created_at" index))))))
    (let ((currency (nonblank (text-of item "lifetime_value_currency")))
          (amount (and (hash-table-p item) (nlk:json-value item :integer "lifetime_value_amount"))))
      (when (or currency amount)
        (unless (and currency amount)
          (fail "subscribers[~d]: lifetime_value_currency and lifetime_value_amount go together" index))
        (setf (gethash "lifetime_value_currency" entry) (string-upcase currency)
              (gethash "lifetime_value_amount" entry) amount)))
    (let ((fields (object-of item "fields")))
      (when fields
        (maphash (lambda (key value)
                   (when (member key +subscriber-attributes+ :test #'string=)
                     (fail "subscribers[~d].fields: ~s collides with a subscriber attribute" index key))
                   (setf (gethash key entry) value))
                 fields)))
    entry))

(defun event-entry (item index)
  "ITEM, one tracked event, as the object Bento takes."
  (unless (hash-table-p item)
    (fail "events[~d] must be an object" index))
  (let* ((type (nonblank (text-of item "type")))
         (email (email-address (text-of item "email")
                                :what (format nil "events[~d].email" index))))
    (unless type (fail "events[~d].type is required" index))
    (when (string-equal type "$purchase")
      (fail "events[~d] is a purchase: use bento_purchase_track, which keeps the order id as its dedupe key" index))
    (let ((event (nlk:json-object "type" type "email" email)))
      (let ((fields (object-of item "fields")))
        (when fields (setf (gethash "fields" event) fields)))
      (let ((details (object-of item "details")))
        (when details (setf (gethash "details" event) details)))
      (let ((date (nonblank (text-of item "date"))))
        (when date
          (setf (gethash "date" event)
                (iso-of (iso-epoch date :what (format nil "events[~d].date" index))))))
      event)))

(defun purchase-item (item index)
  "ITEM, one line of a purchase, as the object Bento takes (price in cents)."
  (unless (hash-table-p item)
    (fail "items[~d] must be an object" index))
  (let ((entry (nlk:json-object)))
    (dolist (key '("product_sku" "product_name"))
      (let ((value (nonblank (text-of item key))))
        (when value (setf (gethash key entry) value))))
    (let ((quantity (nlk:json-value item :integer "quantity")))
      (when quantity (setf (gethash "quantity" entry) (max 1 quantity))))
    (let ((price (nlk:json-value item :integer "price_cents")))
      (when price (setf (gethash "price" entry) price)))
    entry))

(defun email-entry (item index sender transactional)
  "ITEM, one message of a send, as the object Bento takes. => (values ENTRY TO)."
  (unless (hash-table-p item)
    (fail "emails[~d] must be an object" index))
  (let* ((to (email-address (text-of item "to") :what (format nil "emails[~d].to" index)))
         (subject (nonblank (text-of item "subject")))
         (body (nonblank (text-of item "html_body"))))
    (unless subject (fail "emails[~d].subject is required" index))
    (unless body (fail "emails[~d].html_body is required" index))
    (let ((entry (nlk:json-object "to" to "from" sender "subject" subject
                                   "html_body" body
                                   "transactional" (if transactional t :false))))
      (let ((personalizations (object-of item "personalizations")))
        (when personalizations (setf (gethash "personalizations" entry) personalizations)))
      (values entry to))))

(defun broadcast-entry (request)
  "REQUEST as the broadcast object Bento takes, and the tag audience it names
(NIL for a segment)."
  (let* ((name (nonblank (broadcast-name request)))
         (subject (nonblank (broadcast-subject request)))
         (content (nonblank (broadcast-content request)))
         (from-name (nonblank (broadcast-from-name request)))
         (from-email (email-address (broadcast-from-email request) :what "from_email"))
         (tags (tag-list! (broadcast-inclusive-tags request) :what "inclusive_tags"))
         (segment (nonblank (broadcast-segment-id request)))
         (exclusive (tag-list! (broadcast-exclusive-tags request) :what "exclusive_tags"))
         (per-hour (broadcast-batch-size-per-hour request))
         (preview (nonblank (broadcast-preview-text request)))
         (kind (nonblank (broadcast-kind request))))
    (unless (and name subject content from-name)
      (fail "name, subject, content and from_name are all required"))
    (unless (or tags segment)
      (fail "an explicit audience is mandatory: inclusive_tags or segment_id, never everyone"))
    (when (and tags segment)
      (fail "name exactly one audience: inclusive_tags or segment_id, not both"))
    (when (and kind (not (member kind '("plain" "raw") :test #'string=)))
      (fail "type must be plain or raw: ~s" kind))
    (when (and per-hour (< per-hour 1))
      (fail "batch_size_per_hour must be at least 1"))
    (let ((entry (nlk:json-object "name" name "subject" subject "content" content
                                   "from" (nlk:json-object "name" from-name "email" from-email))))
      (when kind (setf (gethash "type" entry) kind))
      (if tags
          (setf (gethash "inclusive_tags" entry) (join-comma tags))
          (setf (gethash "segment_id" entry) segment))
      (when exclusive (setf (gethash "exclusive_tags" entry) (join-comma exclusive)))
      (when per-hour (setf (gethash "batch_size_per_hour" entry) per-hour))
      (when preview (setf (gethash "inbox_snippet" entry) preview))
      (values entry tags))))

;;; --- the reads ------------------------------------------------------------------------

(defun subscriber-get-tool (request)
  "bento_subscriber_get: one subscriber."
  (let ((email (nonblank (subscriber-lookup-email request)))
        (uuid (nonblank (subscriber-lookup-uuid request))))
    (unless (or email uuid)
      (fail "give an email address or a subscriber uuid"))
    (multiple-value-bind (data status)
        (request :get "/fetch/subscribers"
                 :params (list (cons "email" (and email (email-address email)))
                               (cons "uuid" uuid)))
      (answer-text :get "/fetch/subscribers" status data))))

(defun tag-list-tool (request)
  "bento_tag_list: every tag, cached."
  (multiple-value-bind (data age)
      (cached-get "/fetch/tags" :ttl (if (no-request-refresh request) 0 +tag-cache-ttl+))
    (answer-text :get "/fetch/tags" 200 data
                 :note (and age (format nil "cached ~d seconds ago" (round age))))))

(defun field-list-tool (request)
  "bento_field_list: every custom field, cached."
  (multiple-value-bind (data age)
      (cached-get "/fetch/fields" :ttl (if (no-request-refresh request) 0 +tag-cache-ttl+))
    (answer-text :get "/fetch/fields" 200 data
                 :note (and age (format nil "cached ~d seconds ago" (round age))))))

(defun broadcast-list-tool (request)
  "bento_broadcast_list: broadcasts by status."
  (let ((status (nonblank (broadcast-list-status request)))
        (page (broadcast-list-page request)))
    (when (and status (not (member status '("sent" "draft" "sending" "scheduled" "paused" "canceled")
                                       :test #'string=)))
      (fail "status must be sent, draft, sending, scheduled, paused or canceled: ~s" status))
    (multiple-value-bind (data code)
        (request :get "/fetch/broadcasts"
                 :params (list (cons "status" status) (cons "page" page)))
      (answer-text :get "/fetch/broadcasts" code data
                   :note (unless status
                           "no status given: Bento answers only SENT broadcasts; pass status=draft to find drafts")))))

(defun stats-tool (request)
  "bento_stats: site, segment, report or ads statistics."
  (let ((kind (nonblank (stats-request-kind request))))
    (multiple-value-bind (path params)
        (cond ((string= kind "site") (values "/stats/site" '()))
              ((string= kind "segment")
               (let ((segment (nonblank (stats-request-segment-id request))))
                 (unless segment (fail "kind=segment needs a segment_id"))
                 (values "/stats/segment" (list (cons "segment_id" segment)))))
              ((string= kind "report")
               (let ((report (nonblank (stats-request-report-id request))))
                 (unless report (fail "kind=report needs a report_id"))
                 (values "/stats/report" (list (cons "report_id" report)))))
              ((string= kind "ads")
               (values "/stats/ads"
                       (list (cons "dimension" (nonblank (stats-request-dimension request)))
                             (cons "revenue_mode" (nonblank (stats-request-revenue-mode request)))
                             (cons "value" (nonblank (stats-request-value request)))
                             (cons "start_date" (nonblank (stats-request-start-date request)))
                             (cons "end_date" (nonblank (stats-request-end-date request))))))
              (t (fail "kind must be site, segment, report or ads: ~s" kind)))
      (multiple-value-bind (data age) (cached-get path :params params :ttl +stats-cache-ttl+)
        (answer-text :get path 200 data
                     :note (format nil "~@[cached ~d seconds ago; ~]Bento allows 30 stats requests an hour; money values are cents"
                                   (and age (round age))))))))

;;; --- the writes ------------------------------------------------------------------------

(defun subscriber-upsert-tool (request)
  "bento_subscriber_upsert: 1-1000 subscribers imported, Tier 2 above the threshold."
  (let* ((items (subscriber-import-subscribers request))
         (items (cond ((vectorp items) items) ((listp items) (coerce items 'vector)) (t #())))
         (entries (loop for item across items for index from 0
                        collect (subscriber-entry item index)))
         (count (length entries)))
    (unless (<= 1 count 1000)
      (fail "subscribers must be 1 to 1000 objects, not ~d" count))
    (let* ((tier (if (> count (setting :bulk-threshold)) :t2 :t1))
           (built (make-built :path "/batch/subscribers" :tier tier :count count
                              :note (format nil "the import is queued and can take up to 5 minutes to appear; it does not trigger Flows~@[; ~d email(s) repeat~:p in the batch~]"
                                            (let ((emails (map 'list (lambda (entry) (gethash "email" entry)) entries)))
                                              (length (remove-duplicates emails :test #'string-equal))))
                              :body (nlk:json-object "subscribers" (coerce entries 'vector)))))
      (run-built built "bento_subscriber_upsert"
                 :dry-run (subscriber-import-dry-run request)
                 :confirm-token (subscriber-import-confirm-token request)))))

(defun subscriber-tag-tool (request)
  "bento_subscriber_tag: one tag added to or removed from one person."
  (let* ((email (email-address (subscriber-tag-email request)))
         (action (nonblank (subscriber-tag-action request)))
         (tag (tag-name! (subscriber-tag-tag request))))
    (unless (member action '("add" "remove") :test #'string=)
      (fail "action must be add or remove: ~s" action))
    (run-built (make-built :path "/fetch/commands" :tier :t1 :count 1
                            :note "async command: Bento answers a queued count, not the updated subscriber"
                            :body (nlk:json-object
                                   "command" (nlk:json-object "command" (format nil "~a_tag" action)
                                                               "email" email "query" tag)))
               "bento_subscriber_tag")))

(defun tag-create-tool (request)
  "bento_tag_create: one tag definition, refusing a name that is already there."
  (let* ((name (tag-definition! (tag-definition-name request)))
         (existing (ignore-errors (cached-get "/fetch/tags")))
         (names (labels ((walk (node)
                           (cond ((hash-table-p node)
                                  (loop for key being the hash-keys of node using (hash-value value)
                                        when (string= key "name") collect (princ-to-string value)
                                        when (or (hash-table-p value) (vectorp value)) append (walk value)))
                                 ((and (vectorp node) (not (stringp node)))
                                  (loop for item across node append (walk item)))
                                 (t '()))))
                  (ignore-errors (remove-duplicates (walk existing) :test #'string=)))))
    (if (member name names :test #'string-equal)
        (format nil "bento: tag ~s already exists; nothing was sent." name)
        (run-built (make-built :path "/fetch/tags" :tier :t1 :count 1
                                :note "creates the DEFINITION only; it is not applied to anyone"
                                :body (nlk:json-object "tag" (nlk:json-object "name" name)))
                   "bento_tag_create"))))

(defun field-create-tool (request)
  "bento_field_create: one custom field definition."
  (let ((key (field-key! (field-definition-key request))))
    (run-built (make-built :path "/fetch/fields" :tier :t1 :count 1
                            :note "creates the DEFINITION only; it carries no values"
                            :body (nlk:json-object "field" (nlk:json-object "key" key)))
               "bento_field_create")))

(defun consent-tool (request)
  "bento_subscriber_consent: subscribe or unsubscribe one person, Tier 2."
  (let* ((email (email-address (consent-email request)))
         (action (nonblank (consent-action request)))
         (reason (nonblank (consent-why request))))
    (unless (member action '("subscribe" "unsubscribe") :test #'string=)
      (fail "action must be subscribe or unsubscribe: ~s" action))
    (when (and (string= action "subscribe") (null reason))
      (fail "reason is required for subscribe: record where this person's consent came from"))
    (run-built (make-built :path "/fetch/commands" :tier :t2 :count 1
                            :note (if (string= action "subscribe")
                                      "subscribe RE-SUBSCRIBES a person who opted out; confirm they opted back in"
                                      "async command: Bento answers a queued count")
                            :body (nlk:json-object
                                   "command" (nlk:json-object "command" action "email" email)))
               "bento_subscriber_consent"
               :dry-run (consent-dry-run request)
               :confirm-token (consent-confirm-token request))))

(defun event-track-tool (request)
  "bento_event_track: 1-1000 events, Tier 2 because they can fire Flows."
  (let* ((items (event-batch-events request))
         (items (cond ((vectorp items) items) ((listp items) (coerce items 'vector)) (t #())))
         (entries (loop for item across items for index from 0
                        collect (event-entry item index))))
    (unless (<= 1 (length entries) 1000)
      (fail "events must be 1 to 1000 objects, not ~d" (length entries)))
    (run-built (make-built :path "/batch/events" :tier :t2 :count (length entries)
                            :note "events create unknown subscribers and CAN TRIGGER FLOWS that send real email"
                            :body (nlk:json-object "events" (coerce entries 'vector)))
               "bento_event_track"
               :dry-run (event-batch-dry-run request)
               :confirm-token (event-batch-confirm-token request))))

(defun purchase-track-tool (request)
  "bento_purchase_track: one $purchase, keyed by its order id."
  (let* ((email (email-address (purchase-email request)))
         (order (nonblank (purchase-order-id request)))
         (amount (purchase-amount-cents request))
         (currency (string-upcase (or (nonblank (purchase-currency request)) ""))))
    (unless order (fail "order_id is the dedupe key: pass the real order id, never an invented one"))
    (unless (and (integerp amount) (>= amount 0))
      (fail "amount_cents must be a whole number of cents: ~s" amount))
    (unless (= 3 (length currency))
      (fail "currency must be a three-letter ISO-4217 code such as USD: ~s" currency))
    (let* ((details (nlk:json-object
                     "unique" (nlk:json-object "key" order)
                     "value" (nlk:json-object "currency" currency "amount" amount)))
           (event (nlk:json-object "type" "$purchase" "email" email "details" details))
           (items (purchase-items request))
           (items (cond ((vectorp items) items) ((listp items) (coerce items 'vector)) (t #()))))
      (when (plusp (length items))
        (setf (gethash "cart" details)
              (nlk:json-object "items" (coerce (loop for item across items for index from 0
                                                      collect (purchase-item item index))
                                                'vector))))
      (let ((fields (purchase-fields request)))
        (when fields (setf (gethash "fields" event) fields)))
      (let ((date (nonblank (purchase-date request))))
        (when date (setf (gethash "date" event) (iso-of (iso-epoch date :what "date")))))
      (run-built (make-built :path "/batch/events" :tier :t2 :count 1
                              :note "order_id is the dedupe key (details.unique.key): reusing it cannot double-count revenue"
                              :body (nlk:json-object "events" (vector event)))
                 "bento_purchase_track"
                 :dry-run (purchase-dry-run request)
                 :confirm-token (purchase-confirm-token request)))))

(defun broadcast-create-tool (request)
  "bento_broadcast_create: a DRAFT only -- no approved, no send_at."
  (let ((entry (broadcast-entry request)))
    (run-built (make-built :path "/batch/broadcasts" :tier :t1 :count 1
                            :note "draft only: a person reviews and sends it from the Bento dashboard (the answer carries dashboard_url)"
                            :body (nlk:json-object "broadcasts" (vector entry)))
               "bento_broadcast_create")))

(defun broadcast-schedule-tool (request)
  "bento_broadcast_schedule: creates and APPROVES a broadcast for send_at, Tier 3."
  (let* ((entry (broadcast-entry request))
         (tags (nth-value 1 (broadcast-entry request)))
         (from (gethash "email" (gethash "from" entry)))
         (per-hour (gethash "batch_size_per_hour" entry))
         (send-at (iso-epoch (broadcast-send-at request)))
         (lead (setting :min-schedule-lead-minutes))
         (now (get-universal-time))
         (senders (setting :allowed-senders)))
    (unless senders
      (fail "refused: allowed_senders is empty. Name the verified Bento Author address(es) that may send in the section first."))
    (unless (member (string-downcase from) senders :test #'string-equal)
      (fail "refused: from_email ~s is not in allowed_senders" from))
    (unless per-hour
      (fail "batch_size_per_hour is required when scheduling a send"))
    (when (> per-hour (setting :max-batch-size-per-hour))
      (fail "batch_size_per_hour ~d exceeds the section's maximum of ~d"
            per-hour (setting :max-batch-size-per-hour)))
    (when (< send-at (+ now (* 60 lead)))
      (fail "send_at must be at least ~d minutes ahead; now is ~a" lead (iso-now)))
    (when (> send-at (+ now (* 90 86400)))
      (fail "send_at must be within 90 days; now is ~a" (iso-now)))
    (when (and tags (not (broadcast-acknowledge-unknown-audience request)))
      (fail "acknowledge_unknown_audience=true is required for a tag audience: Bento cannot report how many people it holds"))
    (setf (gethash "send_at" entry) (iso-of send-at)
          (gethash "approved" entry) t)
    (run-built (make-built :path "/batch/broadcasts" :tier :t3 :count 1
                            :note "this schedules a REAL send; the documented API has no cancel endpoint, so cancelling is dashboard-only"
                            :body (nlk:json-object "broadcasts" (vector entry)))
               "bento_broadcast_schedule"
               :dry-run (broadcast-dry-run request)
               :confirm-token (broadcast-confirm-token request))))

(defun email-send-tool (request)
  "bento_email_send: transactional email through Bento, Tier 3."
  (let* ((sender (email-address (email-send-sender request) :what "from"))
         (senders (setting :allowed-senders))
         (transactional (email-send-transactional request))
         (reason (nonblank (email-send-why request)))
         (items (email-send-emails request))
         (items (cond ((vectorp items) items) ((listp items) (coerce items 'vector)) (t #())))
         (count (length items)))
    (unless senders
      (fail "refused: allowed_senders is empty. Name the verified Bento Author address(es) that may send in the section first."))
    (unless (member (string-downcase sender) senders :test #'string-equal)
      (fail "refused: from ~s is not in allowed_senders" sender))
    (unless (<= 1 count 60)
      (fail "emails must be 1 to 60 messages, not ~d" count))
    (when (> count (setting :max-recipients-per-call))
      (fail "~d messages exceed the section's max_recipients_per_call of ~d"
            count (setting :max-recipients-per-call)))
    (when (and transactional (not (setting :allow-transactional-override)))
      (fail "transactional=true bypasses unsubscribe suppression: enable allow_transactional_override in the section first"))
    (when (and transactional (null reason))
      (fail "transactional=true needs a reason: say why this person must be mailed anyway"))
    (let ((entries (loop for item across items for index from 0
                         collect (email-entry item index sender transactional))))
      (run-built (make-built :path "/batch/emails" :tier :t3 :count count
                              :note "Bento answers only a queue-accept count; suppressed recipients are dropped silently, so accepted is not delivered"
                              :body (nlk:json-object "emails" (coerce entries 'vector)))
                 "bento_email_send"
                 :dry-run (email-send-dry-run request)
                 :confirm-token (email-send-confirm-token request)))))

;;; --- the operator's own doors ---------------------------------------------------------
;;; Each verb is the tool's call with Lisp arguments; a list of subscriber,
;;; event or email plists is accepted where a tool takes an array of objects.

(defun as-object (item)
  "ITEM as a JSON object: an object as it is, an alist or a plist as one."
  (cond ((hash-table-p item) item)
        ((and (listp item) (every (lambda (entry) (and (consp entry) (stringp (car entry)))) item))
         (apply #'nlk:make-json-object
                (loop for (key . value) in item append (list key value))))
        ((listp item)
         (apply #'nlk:make-json-object
                (loop for (key value) on item by #'cddr
                      append (list (string-downcase (symbol-name key)) value))))
        (t (fail "~s is not an object: give an object, an alist or a plist" item))))

(defun as-objects (items)
  "ITEMS as a vector of JSON objects."
  (coerce (loop for item in items collect (as-object item)) 'vector))

(define-verb subscriber (email-or-uuid &key uuid)
  "Look up ONE Bento subscriber by EMAIL-OR-UUID (or :UUID). => Bento's answer
as lines, with its own JSON under a line saying it is data."
  (subscriber-get-tool (make-subscriber-lookup
                        :email (or (nonblank email-or-uuid) (and uuid nil))
                        :uuid uuid)))

(define-verb tags ()
  "List every tag in the Bento site (cached five minutes)."
  (tag-list-tool nil))

(define-verb fields ()
  "List every custom field in the Bento site (cached five minutes)."
  (field-list-tool nil))

(define-verb broadcasts (&key status page)
  "List Bento broadcasts, by STATUS (sent, draft, sending, scheduled, paused or
canceled) and PAGE."
  (broadcast-list-tool (make-broadcast-list :status status :page page)))

(define-verb stats (&key kind segment-id report-id dimension revenue-mode value start-date end-date)
  "Read Bento statistics: KIND site, segment, report or ads, with the members
that kind needs."
  (stats-tool (make-stats-request :kind (or kind "site") :segment-id segment-id
                                   :report-id report-id :dimension dimension
                                   :revenue-mode revenue-mode :value value
                                   :start-date start-date :end-date end-date)))

(define-verb upsert (subscribers &key (dry-run t) confirm-token)
  "Upsert SUBSCRIBERS, a list of plists (:email \"a@b.c\" :tags (\"vip\") :fields
((\"plan\" . \"pro\"))), previewing unless DRY-RUN is NIL and CONFIRM-TOKEN is the
token that preview answered."
  (subscriber-upsert-tool (make-subscriber-import :subscribers (as-objects subscribers)
                                                  :dry-run dry-run
                                                  :confirm-token confirm-token)))

(define-verb tag (email action tag-name)
  "Add or remove TAG-NAME on the person EMAIL: ACTION \"add\" or \"remove\"."
  (subscriber-tag-tool (make-subscriber-tag :email email :action action :tag tag-name)))

(define-verb consent (email action &key reason (dry-run t) confirm-token)
  "Subscribe or unsubscribe EMAIL; a subscribe needs REASON."
  (consent-tool (make-consent :email email :action action :why reason
                               :dry-run dry-run :confirm-token confirm-token)))

(define-verb create-tag (name)
  "Create the tag definition NAME (lowercase letters, digits, - and _)."
  (tag-create-tool (make-tag-definition :name name)))

(define-verb create-field (key)
  "Create the custom field definition KEY (lowercase_snake_case)."
  (field-create-tool (make-field-definition :key key)))

(define-verb track (events &key (dry-run t) confirm-token)
  "Track EVENTS, a list of plists (:type \"$signed_up\" :email \"a@b.c\")."
  (event-track-tool (make-event-batch :events (as-objects events)
                                      :dry-run dry-run :confirm-token confirm-token)))

(define-verb purchase (email order-id amount-cents currency &key items fields date
                       (dry-run t) confirm-token)
  "Record one $purchase for EMAIL, ORDER-ID, AMOUNT-CENTS and CURRENCY, with
optional ITEMS and FIELDS."
  (purchase-track-tool (make-purchase :email email :order-id order-id
                                       :amount-cents amount-cents :currency currency
                                       :items (and items (as-objects items))
                                       :fields (and fields (as-object fields))
                                       :date date :dry-run dry-run
                                       :confirm-token confirm-token)))

(define-verb broadcast (name subject content from-name from-email
                        &key inclusive-tags segment-id exclusive-tags
                        type batch-size-per-hour preview-text)
  "Create a DRAFT broadcast (nothing is sent): NAME, SUBJECT, CONTENT, FROM-NAME,
FROM-EMAIL, and an audience -- INCLUSIVE-TAGS or SEGMENT-ID, never both."
  (broadcast-create-tool (make-broadcast
                          :name name :subject subject :content content
                          :from-name from-name :from-email from-email
                          :inclusive-tags (and inclusive-tags (coerce inclusive-tags 'vector))
                          :segment-id segment-id
                          :exclusive-tags (and exclusive-tags (coerce exclusive-tags 'vector))
                          :kind type :batch-size-per-hour batch-size-per-hour
                          :preview-text preview-text)))

(define-verb schedule (name subject content from-name from-email send-at
                       &key inclusive-tags segment-id exclusive-tags
                       batch-size-per-hour acknowledge-unknown-audience
                       type preview-text (dry-run t) confirm-token)
  "Create AND APPROVE a broadcast for SEND-AT (UTC, ISO-8601), a real send:
preview first, then repeat with :DRY-RUN NIL and the CONFIRM-TOKEN."
  (broadcast-schedule-tool
   (make-broadcast :name name :subject subject :content content
                    :from-name from-name :from-email from-email :send-at send-at
                    :inclusive-tags (and inclusive-tags (coerce inclusive-tags 'vector))
                    :segment-id segment-id
                    :exclusive-tags (and exclusive-tags (coerce exclusive-tags 'vector))
                    :batch-size-per-hour batch-size-per-hour
                    :acknowledge-unknown-audience acknowledge-unknown-audience
                    :kind type :preview-text preview-text
                    :dry-run dry-run :confirm-token confirm-token)))

(define-verb send-email (from emails &key transactional reason allow-duplicate
                          (dry-run t) confirm-token)
  "Send EMAILS, a list of plists (:to \"a@b.c\" :subject \"...\" :html_body
\"<p>...</p>\"), from the verified address FROM: preview first, then repeat with
:DRY-RUN NIL and the CONFIRM-TOKEN."
  (email-send-tool (make-email-send :sender from :emails (as-objects emails)
                                     :transactional transactional :why reason
                                     :allow-duplicate allow-duplicate
                                     :dry-run dry-run :confirm-token confirm-token)))
