;;;; client.lisp --- Parcel's external API, the local cache and the rate budget.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Parcel's external API is two reads and one write, strictly rate limited
;;;; (20 reads an hour, 20 adds a day, failures included) and served from
;;;; Parcel's own cache, so polling harder never freshens a list. Everything
;;;; here exists to spend as few of those requests as possible: a list is
;;;; reused for cache_seconds, a local count refuses the request past the
;;;; limit instead of letting Parcel reject it, and a read that fails answers
;;;; the last list marked stale.
;;;;
;;;; HTTP is the only seam that leaves this image, so it is one function --
;;;; HTTP here -- that a test replaces whole.

(in-package #:nodecode-parcel)

(defparameter +status+
  '((0 "delivered" "Delivered.")
    (1 "stalled" "No updates for a long time; Parcel expects none.")
    (2 "in_transit" "In transit.")
    (3 "ready_for_pickup" "Waiting for the recipient to pick it up.")
    (4 "out_for_delivery" "Out for delivery.")
    (5 "not_found" "The carrier doesn't know this tracking number yet.")
    (6 "delivery_attempt_failed" "A delivery attempt failed.")
    (7 "exception" "Delivery exception: something needs attention.")
    (8 "label_created" "The carrier has the shipping info but not the package yet."))
  "Parcel's status number as its name and the sentence a model reads.")

(defparameter +deliveries-window+ 3600
  "Parcel's read limit, in seconds.")

(defparameter +adds-window+ 86400
  "Parcel's add limit, in seconds.")

(defparameter +carriers-max-age+ (* 24 3600)
  "How long Parcel's carrier table is reused; it changes rarely.")

(defparameter +max-deliveries+ 100
  "The most deliveries one answer lists.")

(defparameter +max-events+ 25
  "The most events one delivery carries.")

;;; --- the running cell's memory ------------------------------------------------

(defvar *state* nil
  "The running cell's cache and request counts: (:cache HASH :calls HASH).
Nothing here is on disk: a restart begins with an empty cache and an empty
count, which costs at most one read.")

(defun state ()
  "The running cell's cache and counts, made on first use."
  (or *state*
      (setf *state* (list :cache (make-hash-table :test #'equal)
                           :calls (make-hash-table :test #'equal)))))

(defun cache () (getf (state) :cache))

(defun counts () (getf (state) :calls))

(defun now ()
  "The moment, in universal time seconds."
  (get-universal-time))

;;; --- reading what Parcel said ---------------------------------------------------

(defun shorten (value limit)
  "VALUE as one line of at most LIMIT characters."
  (let ((flat (map 'string (lambda (char)
                              (if (member char '(#\Newline #\Return #\Tab) :test #'char=)
                                  #\Space char))
                   (princ-to-string (or value "")))))
    (if (> (length flat) limit) (subseq flat 0 limit) flat)))

(defun clean-text (value limit)
  "VALUE as one line of at most LIMIT characters, or NIL when it is blank."
  (let ((text (shorten value limit)))
    (if (plusp (length text)) text nil)))

(defun text (value)
  "VALUE as text, an absent or null member reading as the empty string."
  (cond ((null value) "") ((eq value :null) "") (t (princ-to-string value))))

(defun status-of (code)
  "CODE, a Parcel status number, as (values NAME SENTENCE)."
  (let ((row (assoc code +status+)))
    (if row
        (values (second row) (third row))
        (values "unknown" "Unknown status."))))

;;; --- the one seam ---------------------------------------------------------------

(defun http (method url &key content headers timeout)
  "One HTTP exchange: (values BODY STATUS HEADERS). The cell's only way out."
  (nlk:http method url :headers headers :content content :timeout timeout))

(defun exchange (method url headers content)
  "HTTP under a refusal: a transport failure reads as network, in one sentence."
  (handler-case (http method url :headers headers :content content
                           :timeout (setting :request-timeout))
    (parcel-error (condition) (error condition))
    (error (condition)
      (fail-as "network" "couldn't reach Parcel (~a); check the network and try again later"
               (princ-to-string condition)))))

(defun check-status (status body)
  "Refuse a STATUS Parcel answered with, in words the operator can act on."
  (cond ((and (integerp status) (<= 200 status 299)) t)
        ((null status)
         (fail-as "network" "Parcel answered nothing; check the network and try again later"))
        ((= status 429)
         (fail-as "rate_limited" "Parcel's rate limit is used up; try again later"))
        ((member status '(401 403) :test #'eql)
         (fail-as "auth" "Parcel rejected the API key; make sure it is current and the account has Parcel Premium"))
        (t (let ((detail (clean-text body 200)))
             (fail-as "http" "Parcel answered HTTP ~a~@[: ~a~]" status detail)))))

(defun request (path &key (method :get) body (key t) (success t))
  "One request under PATH of the API base, the parsed JSON object Parcel answered.
With KEY the section's api-key rides it; with SUCCESS the answer must say
success, which the carrier table does not."
  (let ((headers (list (cons "accept" "application/json")))
        (content nil))
    (when key
      (push (cons "api-key" (setting :api-key)) headers))
    (when body
      (setf content (nlk:encode-json-object body))
      (push (cons "content-type" "application/json") headers))
    (multiple-value-bind (body-text status)
        (exchange method (concatenate 'string (setting :api-base) path) headers content)
      (check-status status body-text)
      (let ((parsed (ignore-errors (nlk:decode-json (or body-text "")))))
        (unless (hash-table-p parsed)
          (fail-as "bad_json" "Parcel answered something that isn't a JSON object"))
        (when (and success (not (nlk:json-value parsed :boolean "success")))
          (let ((message (or (nlk:json-value parsed :string "error_message") "unknown error")))
            (fail-as (if (search "limit" (string-downcase message)) "rate_limited" "api")
                     "Parcel said: ~a" message)))
        parsed))))

;;; --- the local rate budget ------------------------------------------------------

(defun spend (bucket window limit)
  "Count one request in BUCKET, refusing past LIMIT requests in WINDOW seconds."
  (let* ((at (now))
         (live (remove-if (lambda (stamp) (>= (- at stamp) window))
                           (gethash bucket (counts)))))
    (when (>= (length live) limit)
      (fail-as "rate_limited"
               "this machine already used Parcel's limit (~a per ~a); try again in about ~a min"
               limit (if (= window +deliveries-window+) "hour" "day")
               (max 1 (ceiling (- window (- at (reduce #'min live))) 60))))
    (setf (gethash bucket (counts)) (cons at live))))

(defun budget ()
  "The requests this process has left, as a plist.
Local only: other apps using the same key share Parcel's limit."
  (let ((at (now)))
    (flet ((left (bucket window limit)
             (max 0 (- limit (count-if (lambda (stamp) (< (- at stamp) window))
                                       (gethash bucket (counts)))))))
      (list :deliveries-left (left "deliveries" +deliveries-window+ (setting :reads-per-hour))
            :adds-left (left "add" +adds-window+ (setting :adds-per-day))))))

(defun retryable-p (condition)
  "Whether CONDITION leaves the last list worth answering with, marked stale."
  (and (typep condition 'parcel-failure)
       (member (parcel-failure-code condition) '("rate_limited" "network" "http")
               :test #'string=)))

;;; --- deliveries -----------------------------------------------------------------

(defun fetch-deliveries (filter)
  "FILTER's list from Parcel, spending one read. => (:fetched-at :deliveries)."
  (spend "deliveries" +deliveries-window+ (setting :reads-per-hour))
  (let* ((payload (request (format nil "/deliveries/?filter_mode=~a" filter)))
         (raw (nlk:json-value payload :array "deliveries")))
    (unless (or (null raw) (vectorp raw))
      (fail-as "bad_shape" "Parcel's delivery list was malformed"))
    (let ((entry (list :fetched-at (now)
                       :deliveries (remove-if-not #'hash-table-p (coerce (or raw #()) 'list)))))
      (setf (gethash filter (cache)) entry)
      entry)))

(defun deliveries (&key (filter "active") refresh)
  "FILTER's deliveries, as a plist (:deliveries :fetched-at :from-cache :stale).
A fresh cache answers without a request; a rate limit or an outage answers
the last list, marked stale, rather than hiding what is already known."
  (unless (member filter '("active" "recent") :test #'string=)
    (fail-as "invalid" "filter must be active or recent, not ~a" filter))
  (let ((cached (gethash filter (cache))))
    (cond ((and (not refresh) cached
                (< (- (now) (getf cached :fetched-at)) (setting :cache-seconds)))
           (append cached (list :from-cache t :stale nil)))
          (t (handler-case (append (fetch-deliveries filter) (list :from-cache nil :stale nil))
               (parcel-error (condition)
                 (if (and cached (retryable-p condition))
                     (append cached
                             (list :from-cache t :stale t
                                   :warning (princ-to-string condition)))
                     (error condition))))))))

(defun forget-cached-deliveries ()
  "Drop every cached list: an add makes them all older than what Parcel knows."
  (remhash "active" (cache))
  (remhash "recent" (cache)))

(defun tracked-numbers ()
  "Every tracking number this process has seen in a cached list, normalized."
  (let ((numbers '()))
    (maphash (lambda (filter entry)
               (declare (ignore filter))
               (dolist (delivery (getf entry :deliveries))
                 (let ((number (nlk:json-value delivery :string "tracking_number")))
                   (when number
                     (pushnew (normalize-tracking number) numbers :test #'string=)))))
             (cache))
    numbers))

;;; --- carriers -------------------------------------------------------------------

(defun carrier-code-p (code)
  "Whether CODE reads as one of Parcel's own carrier codes."
  (and (stringp code)
       (<= 2 (length code) 32)
       (every (lambda (char)
                (or (char<= #\a char #\z) (char<= #\0 char #\9) (char= char #\_)))
              code)))

(defun variations-of (info)
  "INFO's name variations, as at most ten short strings."
  (let ((found '()))
    (dolist (variation (coerce (or (nlk:json-value info :array "name_variations") #()) 'list))
      (when (and (stringp variation) (< (length found) 10))
        (push (shorten variation 80) found)))
    (nreverse found)))

(defun clean-carriers (table)
  "TABLE as the carrier table a lookup keeps: a well-formed code to its name,
whether it needs a postcode or email, and the other names it is known by."
  (let ((clean (make-hash-table :test #'equal)))
    (maphash (lambda (code info)
               (when (and (carrier-code-p code) (hash-table-p info))
                 (setf (gethash code clean)
                       (list :name (shorten (or (nlk:json-value info :string "name") code) 80)
                             :extra-required (and (nlk:json-value info :boolean "extra_required") t)
                             :variations (variations-of info)))))
             table)
    clean))

(defun carriers (&key refresh)
  "Parcel's carrier table, code to (:name :extra-required :variations), cached
for a day. It rides no key and costs no read."
  (let ((cached (gethash "carriers" (cache)))
        (at (now)))
    (cond ((and (not refresh) cached
                (< (- at (getf cached :fetched-at)) +carriers-max-age+))
           (getf cached :data))
          (t (handler-case
                 (let ((data (clean-carriers (request "/supported_carriers.json"
                                                      :key nil :success nil))))
                   (setf (gethash "carriers" (cache)) (list :fetched-at at :data data))
                   data)
               (parcel-error (condition)
                 (if cached
                     (getf cached :data)
                     (error condition))))))))

(defun carrier-name (table code)
  "CODE's name in TABLE, or the code itself."
  (or (getf (gethash code table) :name) code))

;;; --- adding a delivery ------------------------------------------------------------

(defun tracking-p (value)
  "Whether VALUE reads as a tracking number: 3 to 64 letters, digits, spaces,
dots, underscores, dashes or slashes, starting with a letter or digit."
  (and (stringp value)
       (<= 3 (length value) 64)
       (alphanumericp (char value 0))
       (every (lambda (char)
                (or (alphanumericp char)
                    (member char '(#\Space #\. #\_ #\/ #\-) :test #'char=)))
              value)))

(defun email-p (value)
  "Whether VALUE reads as an order email address."
  (let ((at (position #\@ value)))
    (and at (> at 0) (<= at 64)
         (not (find #\Space value))
         (let ((domain (subseq value (1+ at))))
           (and (< 0 (length domain) 191)
                (not (find #\@ domain)))))))

(defun language-p (value)
  "Whether VALUE reads as a two-letter lowercase language code."
  (and (= 2 (length value)) (every #'lower-case-p value)))

(defun trim (value)
  "VALUE as a string with the ends trimmed."
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or value "")))

(defun validate-add (&key tracking carrier description postcode email language carriers)
  "The add request's members, checked before anything is sent: an add Parcel
refuses still spends one of the day's twenty. => a plist of cleaned values."
  (let ((tracking (shorten (trim tracking) 64))
        (carrier (string-downcase (trim carrier)))
        (description (shorten (trim description) 120))
        (postcode (shorten (trim postcode) 20))
        (email (shorten (trim email) 254))
        (language (string-downcase (trim language))))
    (unless (tracking-p tracking)
      (fail-as "invalid" "tracking_number must be 3 to 64 letters, digits, spaces, dots, dashes or slashes"))
    (unless (carrier-code-p carrier)
      (fail-as "invalid" "carrier_code must be a Parcel carrier code such as ups, usps, fedex or amzlus; use parcel_carriers to look one up"))
    (when (zerop (length description))
      (fail-as "invalid" "description is required, for example new running shoes"))
    (when (and (plusp (length email)) (not (email-p email)))
      (fail-as "invalid" "email doesn't look like an email address"))
    (when (and (plusp (length language)) (not (language-p language)))
      (fail-as "invalid" "language must be a two-letter code like en"))
    (when (and carriers (not (string= carrier "pholder")))
      (let ((info (gethash carrier carriers)))
        (cond ((null info)
               (fail-as "unknown_carrier" "~s isn't a Parcel carrier code; use parcel_carriers to find the right one"
                        carrier))
              ((and (getf info :extra-required)
                    (zerop (+ (length postcode) (length email))))
               (fail-as "extra_required" "~a needs a postcode or order email to track; ask for it and pass postcode or email"
                        (getf info :name))))))
    (list :tracking tracking :carrier carrier :description description
          :postcode postcode :email email :language language)))

(defun add-delivery (&key tracking carrier description postcode email language push)
  "POST one delivery to Parcel, spending one of the day's adds. => T."
  (spend "add" +adds-window+ (setting :adds-per-day))
  (let ((body (nlk:json-object "tracking_number" tracking
                                "carrier_code" carrier
                                "description" description
                                "send_push_confirmation" (and push t)
                                :opt "postcode" postcode
                                :opt "email" email
                                :opt "language" language)))
    (request "/add-delivery/" :method :post :body body)
    (forget-cached-deliveries)
    t))

(defun normalize-tracking (value)
  "VALUE with the spaces and dashes out, upcased: the identity two spellings
of one number share."
  (string-upcase (remove-if (lambda (char) (member char '(#\Space #\-) :test #'char=))
                             (or value ""))))
