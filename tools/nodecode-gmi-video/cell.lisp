;;;; cell.lisp --- GMI Cloud video generation: the wire, the schema, the tools.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode rewrite of zkasuran's hermes-gmi-media gmi-video plugin (see
;;;; NOTICE). The original registers a video_gen provider with Hermes; a
;;;; Nodecode has no video lane, so the same two answers are a tool the model
;;;; calls, gmi_video, and the model list its picker offered, gmi_video_models.
;;;; Both read each model's live schema from GMI, so duration, resolution,
;;;; aspect ratio and audio use the values that model accepts, and anything it
;;;; lacks is dropped and reported -- as the original's map_payload does.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "gmi-video": {"api_key": "...", "model": "Minimax-Hailuo-02"}
;;;; A vetoed section ("enabled": false) installs nothing, and a section
;;;; with no key refuses at start, naming GMI_API_KEY.

(in-package #:nodecode-gmi-video)

;;; --- what the original's constants say ------------------------------------------

(defparameter +default-base-url+
  "https://console.gmicloud.ai/api/v1/ie/requestqueue/apikey"
  "GMI Cloud's request-queue API, as the original's DEFAULT_BASE_URL: GET
models, GET models/<id>, POST requests, GET requests/<id> under it.")

(defparameter +text-model+ "Minimax-Hailuo-02"
  "The text-to-video model the original prefers, its TEXT_MODELS.")

(defparameter +image-model+ "Minimax-Hailuo-2.3-Fast"
  "The image-to-video model the original prefers first, its IMAGE_MODELS.")

(defparameter +model-preferences+
  '("Minimax-Hailuo-02" "Minimax-Hailuo-2.3-Fast" "kling-v2-6")
  "The models the original names, in its order, before the cheapest usable
one the catalog offers.")

(defparameter +concepts+
  '((:prompt "prompt" "text")
    (:aspect-ratio "aspect_ratio" "aspectRatio")
    (:duration "duration" "durationSeconds" "duration_seconds")
    (:resolution "resolution")
    (:negative-prompt "negative_prompt" "negativePrompt")
    (:seed "seed")
    (:audio "generateAudio" "generate_audio" "sound")
    (:image-url "image" "first_frame_image" "first_frame" "image_url" "input_image"))
  "A concept, then the schema parameter names that carry it, as the
original's CANDIDATES. The first name the model declares is the one used.")

(defparameter +image-names+ '("image" "first_frame_image" "first_frame" "image_url" "input_image")
  "The schema names that take an image, as the original's IMAGE_NAMES.")

(defparameter +aspects+ '(("landscape" . "16:9") ("square" . "1:1") ("portrait" . "9:16"))
  "The named aspects the original's ASPECTS maps, and what they mean.")

(defparameter +internal-words+ '("capture" "probe" "ssrf" "test" "hunt" "bypass")
  "Model ids carrying one of these are GMI's own probes, as its INTERNAL.")

(defparameter +catalog-ttl+ 3600
  "Seconds a read of the model catalog stands, as the original's Catalog ttl.")

(defparameter +max-pixels+ 4194304
  "The largest image size a size parameter is asked for, as the original's cap.")

(defparameter +max-video-bytes+ 536870912
  "The most a result download may write: a video for a phone, not a disk's worth.")

(defparameter +terminal-failures+ '("failed" "error" "cancelled")
  "The statuses a job never leaves, unsuccessfully, as the original's _terminal.")

(defparameter +terminal-successes+ '("success" "completed")
  "The statuses that carry a result, as the original's _terminal.")

(defvar *catalog* nil
  "The GMI video catalog last read, (UNIVERSAL-TIME . RECORDS), or NIL: the
original's one-hour in-memory catalog, in the cell's own image.")

;;; --- the section, read back ----------------------------------------------------

(defun api-key ()
  "The key the section names, inline or by environment variable, else
GMI_API_KEY -- the original's get_secret_str order, without its harness."
  (or (let ((inline (setting :api-key)))
        (and inline (plusp (length inline)) inline))
      (let ((variable (setting :api-key-env)))
        (and variable (plusp (length variable)) (uiop:getenv variable)))
      (uiop:getenv "GMI_API_KEY")))

(defun base-url ()
  "The API root this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +default-base-url+)))

(defun output-directory ()
  "Where a downloaded video is written: the section's output_dir, else the
organism's cache, which is what the Hermes video cache was to the original."
  (let ((given (setting :output-dir)))
    (uiop:ensure-directory-pathname
     (cond ((pathnamep given) given)
           ((and (stringp given) (plusp (length (string-trim '(#\Space) given))))
            (nlk:expand-home given))
           (t (nlk:cache-path "gmi-video/"))))))

(defun check-config ()
  "Refuse a section that could never reach GMI, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "gmi-video: no key: set GMI_API_KEY in the environment, or \"api_key\" "
                                 "(or \"api_key_env\") in the gmi-video section; a key comes from "
                                 "console.gmicloud.ai"))))

;;; --- the wire -----------------------------------------------------------------

(defun now ()
  "Seconds on a monotonic clock, for a job's deadline."
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun scrub (text)
  "TEXT with the key cut out of it: an error is the model's to read, the
credential's not, as the original's scrub()."
  (let ((text (princ-to-string (or text "")))
        (key (api-key)))
    (if (and key (plusp (length key)))
        (with-output-to-string (out)
          (loop with start = 0
                for at = (search key text :start2 start)
                do (write-string (subseq text start (or at (length text))) out)
                while at
                do (write-string "[redacted]" out)
                   (setf start (+ at (length key)))))
        text)))

(defun url-for (path)
  "PATH under the configured root. The key never rides the url."
  (concatenate 'string (base-url) "/" path))

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded JSON object; anything else is an
empty one."
  (let ((value
          (cond ((hash-table-p answer) answer)
                ((and (vectorp answer) (not (stringp answer))) (nlk:json-object))
                ((stringp answer)
                 (if (plusp (length (nlk:trimmed answer)))
                     (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
                     (nlk:json-object)))
                (t (nlk:json-object)))))
    (if (hash-table-p value) value (nlk:json-object))))

(defun body-error-text (answer)
  "What a refused answer says, for the one line the model reads."
  (let ((body (decode-body answer)))
    (or (nlk:json-value body :string "error")
        (nlk:json-value body :string "error" "message")
        (nlk:json-value body :string "message")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer))))
        "No error detail")))

(defun one-request (method url key body limit)
  "One exchange with GMI => (values ANSWER STATUS), or (values CONDITION NIL)
when it never got an answer."
  (handler-case
      (multiple-value-bind (answer status)
          (nlk:http method url
                    :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                                   (cons "content-type" "application/json")
                                   (cons "accept" "application/json"))
                    :content body
                    :timeout limit
                    :connect-timeout (min 10 limit))
        (values answer status))
    ((or error sb-sys:deadline-timeout) (condition)
      (values condition nil))))

(defun refused (status answer &optional model)
  "The refusal a non-success answer is, as the original's _request maps it."
  (let ((detail (scrub (body-error-text answer))))
    (cond
      ((member status '(401 403))
       (fail "GMI rejected the credential. Check the active profile key and its permissions."))
      ((= status 429)
       (fail "GMI rate limit~@[ for ~a~]. Wait before trying again. ~a" model detail))
      ((and (>= status 400) (< status 500))
       (fail "GMI returned HTTP ~d: ~a. Check the model inputs and job status." status detail))
      (t (fail "GMI returned HTTP ~d: ~a. Check the connectivity and the job status." status detail)))))

(defun call (method path &key body deadline model retry)
  "METHOD PATH against the section's root with BODY as JSON text => the
decoded answer. A refusal below HTTP, or a refused status, signals
GMI-VIDEO-ERROR. RETRY asks each attempt twice more past a transport failure
or a 5xx, as polling does."
  (let ((key (api-key))
        (url (url-for path)))
    (unless key
      (fail "Set GMI_API_KEY in the environment, or \"api_key\" in the gmi-video section, before using GMI."))
    (loop with attempts = (if retry 3 1)
          for attempt from 1 to attempts
          for remaining = (if deadline (- deadline (now)) 30)
          do (when (and deadline (<= remaining 0))
               (fail "GMI job deadline expired. Check its status before submitting again."))
             (multiple-value-bind (answer status)
                 (one-request method url key body (max 1 (min 30 (if deadline remaining 30))))
               (cond
                 ((null status)
                  (if (< attempt attempts)
                      (sleep (min 1 (max 0 remaining)))
                      (fail "GMI network request failed (~a). Check connectivity and the job status before submitting again."
                            (nle:transport-failure-label answer url))))
                 ((and retry (>= status 500) (< attempt attempts))
                  (sleep (min 1 (max 0 remaining))))
                 ((not (< status 300)) (refused status answer model))
                 (t (return (decode-body answer))))))))

(defun list-model-ids ()
  "The public model ids GMI's catalog lists, as the original filters them."
  (let ((ids (nlk:json-array (call :get "models") "model_ids")))
    (loop for id across (or ids #())
          when (and (stringp id) (public-id-p id))
            collect id)))

(defun uuid-p (text)
  "TEXT spelled as a UUID, which the original's public_id() drops."
  (and (stringp text)
       (= (length text) 36)
       (loop for index from 0 below 36
             for character = (char text index)
             always (if (member index '(8 13 18 23))
                        (char= character #\-)
                        (not (null (digit-char-p character 16)))))))

(defun public-id-p (id)
  "ID as a model a person could pick: not a UUID, not one of GMI's probes."
  (let ((lowered (string-downcase id)))
    (and (stringp id)
         (plusp (length id))
         (not (uuid-p id))
         (notany (lambda (word) (search word lowered)) +internal-words+))))

(defun model-record (model-id)
  "MODEL-ID's live schema from GMI, as the original's get_model()."
  (let* ((data (call :get (format nil "models/~a" (quri:url-encode model-id :encoding :utf-8))))
         (record (let ((inner (nlk:json-value data :object "data"))) (or inner data))))
    (unless (and (hash-table-p record) (nlk:json-value record :text "model"))
      (fail "GMI returned no model schema for ~a. Choose another model." model-id))
    record))

;;; --- the catalog ---------------------------------------------------------------

(defun parameters (record)
  "RECORD's declared parameters, an alist of (NAME . SPEC)."
  (let ((specs '()))
    (loop for spec across (or (nlk:json-array record "parameters") #())
          for name = (and (hash-table-p spec) (nlk:json-value spec :text "name"))
          when name do (push (cons name spec) specs))
    (nreverse specs)))

(defun options (spec)
  "The values SPEC accepts: its options, else its enum, unwrapped."
  (let ((items (or (nlk:json-array spec "options") (nlk:json-array spec "enum") #())))
    (loop for item across items
          collect (if (hash-table-p item) (nlk:json-value item :any "value") item))))

(defun choose-param (specs names)
  "The first of NAMES SPECS declares, as the original's choose_param()."
  (loop for name in names when (assoc name specs :test #'equal) return name))

(defun accepts-media-p (record media)
  "Whether RECORD takes MEDIA -- an image as a declared parameter, as
accepts_media() has it."
  (let ((specs (parameters record)))
    (cond ((equal media "image") (and (choose-param specs +image-names+) t))
          (t (some (lambda (pair) (string-equal media (or (nlk:json-value (cdr pair) :string "type") "")))
                   specs)))))

(defun required-media (record)
  "The media types RECORD cannot be called without, as required_media()."
  (let ((required '()))
    (loop for modality across (or (nlk:json-array record "modalities" "input") #())
          for type = (nlk:json-value modality :string "type")
          when (and (nlk:json-value modality :boolean "required") type (not (equal type "text")))
            do (pushnew type required :test #'equal))
    (dolist (pair (parameters record))
      (when (nlk:json-value (cdr pair) :boolean "required")
        (let ((type (or (nlk:json-value (cdr pair) :string "type") "")))
          (cond ((or (search "image" type) (member (car pair) +image-names+ :test #'equal))
                 (pushnew "image" required :test #'equal))
                ((member type '("audio" "video") :test #'equal)
                 (pushnew type required :test #'equal))))))
    required))

(defun video-record-p (record)
  "A model that is live, staffing, visible, and answers with a video."
  (and (hash-table-p record)
       (let ((model (nlk:json-value record :string "model")))
         (and model (public-id-p model)))
       (equal (nlk:json-value record :string "status") "active")
       (let ((workers (nlk:json-value record :integer "qworkers"))) (and workers (plusp workers)))
       (not (equal (nlk:json-value record :string "model_metadata" "visibility") "hidden"))
       (loop for modality across (or (nlk:json-array record "modalities" "output") #())
             thereis (equal (nlk:json-value modality :string "type") "video"))))

(defun fits-p (record need-image)
  "Whether RECORD takes what this call has: an image, or nothing but text."
  (and (video-record-p record)
       (if need-image
           (and (accepts-media-p record "image")
                (subsetp (required-media record) '("image") :test #'equal))
           (null (required-media record)))))

(defun price-text (record)
  "RECORD's price as GMI states it, as the original's price_text()."
  (let ((details (nlk:json-value record :any "pricing_details")))
    (cond ((stringp details) details)
          (details (nlk:encode-json-object details))
          (t (let* ((info (nlk:json-value record :object "price_info"))
                    (price (and info (nlk:json-value info :any "price"))))
               (if price
                   (format nil "~a micro-dollars / ~a" price
                           (or (nlk:json-value info :string "unit") "unspecified unit"))
                   "Price unavailable"))))))

(defun price-number (record)
  "RECORD's price as a number for a cheapest-first pick, else infinity."
  (let ((price (nlk:json-value record :any "price_info" "price")))
    (if (realp price) (coerce price 'double-float) most-positive-double-float)))

(defun records (&key refresh)
  "GMI's video catalog, read once an hour: the original's Catalog, without
its threads -- one tool call reads it, so there is no picker to keep quick."
  (let ((cached (unless refresh *catalog*)))
    (if (and cached (< (- (get-universal-time) (car cached)) +catalog-ttl+))
        (cdr cached)
        (let ((read
                (loop for id in (list-model-ids)
                      for record = (handler-case (model-record id) (gmi-video-error () nil))
                      when record collect record)))
          (setf *catalog* (cons (get-universal-time) read))
          read))))

(defun pick-default (catalog need-image)
  "The model a call with no explicit one uses, as pick_default_model(): the
original's preferences first, then the cheapest usable model."
  (let ((candidates (remove-if-not (lambda (record) (fits-p record need-image)) catalog)))
    (or (loop for id in +model-preferences+
              for record = (find id candidates :key (lambda (one) (nlk:json-value one :string "model"))
                                          :test #'equal)
              when record return record)
        (first (sort (copy-list candidates) #'< :key #'price-number)))))

(defun suggestion (need-image)
  "A model that would take these inputs, named for a refusal, or NIL."
  (let ((record (handler-case (pick-default (records) need-image) (gmi-video-error () nil))))
    (and record (nlk:json-value record :string "model"))))

(defun select-model (explicit need-image)
  "The schema this call runs on, as the original's select_model(): the one
named, else the section's, else the preferences, else the cheapest that fits."
  (if explicit
      (let ((record (model-record explicit)))
        (unless (fits-p record need-image)
          (fail "Model ~a does not accept these inputs~@[; use ~a~]." explicit (suggestion need-image)))
        record)
      (let ((record (handler-case
                        (model-record (or (setting (if need-image :image-model :model))
                                          (if need-image +image-model+ +text-model+)))
                      (gmi-video-error () nil))))
        (unless (and record (fits-p record need-image))
          (setf record (pick-default (records) need-image)))
        (or record
            (fail "No usable GMI model found. Set \"model\" in the gmi-video section, or try again once GMI's catalog answers.")))))

;;; --- the schema mapping ---------------------------------------------------------

(defun number-of (value)
  "VALUE as a number: 12, \"12\", \"720p\", or a ratio such as \"16:9\"."
  (typecase value
    (real (coerce value 'double-float))
    (string
     (let ((text (string-downcase (nlk:trimmed value))))
       (cond
         ((zerop (length text)) nil)
         ((or (find #\: text) (find #\x text))
          (let* ((at (or (position #\: text) (position #\x text)))
                 (left (ignore-errors (read-from-string (subseq text 0 at) nil nil)))
                 (right (ignore-errors (read-from-string (subseq text (1+ at)) nil nil))))
            (and (realp left) (realp right) (plusp right) (/ left right))))
         ((and (> (length text) 1) (char= (char text (1- (length text))) #\p))
          (let ((value (ignore-errors (read-from-string (subseq text 0 (1- (length text))) nil nil))))
            (and (realp value) value)))
         (t (let ((value (ignore-errors (read-from-string text nil nil))))
              (and (realp value) value))))))
    (t nil)))

(defun nearest-option (requested choices)
  "The value of CHOICES closest to REQUESTED, as nearest_option()."
  (or (find (string-downcase (princ-to-string requested)) choices
            :key (lambda (choice) (string-downcase (princ-to-string choice))) :test #'equal)
      (let ((number (number-of requested))
            (numeric (loop for choice in choices
                           for value = (number-of choice)
                           when value collect (cons choice value))))
        (if (and number numeric)
            (first (first (sort (copy-list numeric) #'<
                                :key (lambda (pair) (abs (- (cdr pair) number))))))
            (fail "Invalid value ~a. Valid values: ~{~a~^, ~}."
                  requested (subseq choices 0 (min 10 (length choices))))))))

(defun coerce-value (value spec &key nearest)
  "VALUE as SPEC declares it, as the original's _coerce(): an option
matched, else nearest; a number clamped to its bounds."
  (let ((values (options spec)))
    (if values
        (let ((exact (find (string-downcase (princ-to-string value)) values
                           :key (lambda (choice) (string-downcase (princ-to-string choice)))
                           :test #'equal)))
          (or exact
              (if nearest
                  (nearest-option value values)
                  (fail "Invalid ~a value ~a. Valid values: ~{~a~^, ~}."
                        (or (nlk:json-value spec :string "name") "parameter")
                        value (subseq values 0 (min 10 (length values)))))))
        (let ((type (string-downcase (or (nlk:json-value spec :string "type") "")))
              (name (or (nlk:json-value spec :string "name") "parameter")))
          (cond
            ((member type '("integer" "float" "number") :test #'equal)
             (let ((number (or (number-of value) (fail "~a needs a number." name))))
               (let ((low (nlk:json-value spec :number "min_value"))
                     (high (nlk:json-value spec :number "max_value")))
                 (when low (setf number (max number (coerce low 'double-float))))
                 (when high (setf number (min number (coerce high 'double-float)))))
               (if (equal type "integer") (round number) number)))
            ((equal type "boolean")
             (cond ((eq value t) t)
                   ((null value) nil)
                   (t (fail "~a needs true or false." name))))
            (t value))))))

(defun public-image-url-p (url)
  "URL as GMI takes one: http or https with a hostname."
  (and (stringp url)
       (handler-case
           (let ((parsed (quri:uri url)))
             (and (member (quri:uri-scheme parsed) '("http" "https") :test #'equal)
                  (let ((host (quri:uri-host parsed))) (and host (plusp (length host))))))
         (error () nil))))

(defun image-value (value spec)
  "VALUE as the image parameter SPEC takes, as the original's _image_value()."
  (let* ((urls (if (listp value) value (list value)))
         (limit (max 1 (or (nlk:json-value spec :integer "max_value") 1))))
    (when (null urls)
      (fail "GMI takes image inputs as public http(s) URLs; upload the file first or pass a URL."))
    (dolist (url urls)
      (unless (public-image-url-p url)
        (fail "GMI takes image inputs as public http(s) URLs; upload the file first or pass a URL.")))
    (if (> limit 1)
        (coerce (subseq urls 0 (min limit (length urls))) 'vector)
        (first urls))))

(defun size-string-p (value)
  "VALUE spelled as a width x height size."
  (and (stringp value)
       (plusp (length value))
       (find #\x value)
       (every (lambda (character) (or (digit-char-p character) (char= character #\x))) value)))

(defun size-pixels (value)
  "The pixel count a WIDTHxHEIGHT size names."
  (reduce #'* (mapcar #'parse-integer (uiop:split-string value :separator "x"))))

(defun size-for (aspect spec)
  "The size parameter value closest to ASPECT, as the original's _size()."
  (let ((choices (options spec))
        (target (number-of (or (cdr (assoc aspect +aspects+ :test #'string-equal)) aspect))))
    (if choices
        (let* ((sizes (remove-if-not #'size-string-p choices))
               (safe (remove-if-not (lambda (value) (<= (size-pixels value) +max-pixels+)) sizes))
               (usable (if safe safe sizes)))
          (cond
            ((null usable) nil)
            ((null target) (fail "Use a named aspect or a width:height ratio."))
            (t (first (sort (copy-list usable) #'<
                            :key (lambda (value) (abs (- (or (number-of value) 0) target))))))))
        (when (equal (nlk:json-value spec :string "type") "string")
          (let ((ratio (or target 1)))
            (cond ((> ratio 1) "1536x1024") ((< ratio 1) "1024x1536") (t "1024x1024")))))))

(defun audio-on-off-p (spec)
  "Whether SPEC's values are the words on and off, which an audio flag maps to."
  (let ((values (options spec)))
    (and values
         (null (set-difference values '("on" "off") :test (lambda (a b) (string-equal (princ-to-string a) (princ-to-string b)))))
         (null (set-difference '("on" "off") values :test (lambda (a b) (string-equal (princ-to-string a) (princ-to-string b))))))))

(defun concept-name (concept)
  "CONCEPT as the word an answer names it by, for a dropped-option report."
  (string-downcase (symbol-name concept)))

(defun map-payload (record &key prompt image-url duration (aspect-ratio "16:9") (resolution "720p")
                               negative-prompt audio seed)
  "RECORD's payload for these inputs, as the original's map_payload():
=> (values PAYLOAD DROPPED), PAYLOAD a JSON object, DROPPED the concepts
this model has no parameter for."
  (let ((specs (parameters record))
        (payload (nlk:json-object))
        (dropped '()))
    (labels ((place (concept value &key nearest image)
               (when value
                 (let ((name (choose-param specs (cdr (assoc concept +concepts+)))))
                   (cond
                     ((and (null name) (eq concept :aspect-ratio) (assoc "size" specs :test #'equal))
                      (let ((size (size-for value (cdr (assoc "size" specs :test #'equal)))))
                        (if size
                            (setf (gethash "size" payload) size)
                            (push (concept-name concept) dropped))))
                     ((null name) (push (concept-name concept) dropped))
                     (t (let* ((spec (cdr (assoc name specs :test #'equal)))
                               (mapped (cond (image (image-value value spec))
                                             ((eq concept :aspect-ratio)
                                              (or (cdr (assoc value +aspects+ :test #'string-equal)) value))
                                             ((and (eq concept :audio) (audio-on-off-p spec))
                                              (if value "on" "off"))
                                             (t value))))
                          (setf (gethash name payload)
                                (coerce-value mapped spec :nearest nearest)))))))))
      (place :prompt prompt)
      (place :image-url image-url :image t)
      (place :duration duration :nearest t)
      (place :aspect-ratio aspect-ratio :nearest t)
      (place :resolution resolution :nearest t)
      (place :negative-prompt negative-prompt)
      (place :audio audio)
      (place :seed seed))
    ;; A required parameter with no input takes its default, as the original
    ;; fills it; one with neither refuses here, before a job is paid for.
    (dolist (pair specs)
      (when (and (nlk:json-value (cdr pair) :boolean "required")
                 (not (nth-value 1 (gethash (car pair) payload))))
        (let ((default (nlk:json-value (cdr pair) :any "default_value")))
          (when (null default)
            (fail "Required parameter ~a has no input or default. Supply it or choose another model." (car pair)))
          (setf (gethash (car pair) payload) (coerce-value default (cdr pair))))))
    (validate-payload record payload)
    (values payload (nreverse dropped))))

(defun validate-payload (record payload)
  "Refuse a PAYLOAD RECORD does not declare, as validate_payload()."
  (let ((specs (parameters record)))
    (maphash (lambda (name value)
               (let ((spec (cdr (assoc name specs :test #'equal))))
                 (unless spec
                   (fail "Unknown parameter ~a. Use the model's declared parameters." name))
                 (let ((values (options spec)))
                   (when (and values (not (member value values :test #'equal)))
                     (fail "Invalid ~a. Valid values: ~{~a~^, ~}." name
                           (subseq values 0 (min 10 (length values))))))))
             payload)))

;;; --- the job -------------------------------------------------------------------

(defun media-urls (result)
  "RESULT's media urls, video first, as the original's media_urls()."
  (let ((outcome (nlk:json-value result :object "outcome"))
        (urls '()))
    (dolist (key '("video_url" "media_urls" "image_url" "audio_url"))
      (let ((items (nlk:json-value outcome :any key)))
        (cond
          ((stringp items) (pushnew items urls :test #'equal))
          ((and (vectorp items) (not (stringp items)))
           (loop for item across items
                 for url = (if (stringp item) item (nlk:json-value item :string "url"))
                 when (and url (plusp (length url))) do (pushnew url urls :test #'equal))))))
    (or (nreverse urls)
        (let ((thumbnail (nlk:json-value outcome :string "thumbnail_image_url")))
          (and thumbnail (list thumbnail))))))

(defun status-of (data)
  "DATA's status, lowercased."
  (string-downcase (or (nlk:json-value data :string "status") "")))

(defun finished (data)
  "DATA when its job is done and carries a result; a failed job refuses, a
running one answers NIL, as the original's _terminal()."
  (let ((status (status-of data)))
    (cond
      ((member status +terminal-failures+ :test #'equal)
       (fail "GMI job ~a: ~a. Review the inputs before trying again."
             status (or (nlk:json-value data :string "error")
                        (nlk:json-value data :string "message")
                        "no detail")))
      ((member status +terminal-successes+ :test #'equal)
       (unless (media-urls data)
         (fail "GMI job returned no media URL. Check the request in the GMI console."))
       data)
      (t nil))))

(defun submit (model payload)
  "Start a job for MODEL with PAYLOAD => its answer, request_id included."
  (let* ((data (call :post "requests"
                     :body (nlk:encode-json-object (nlk:json-object "model" model "payload" payload))
                     :model model))
         (request-id (or (nlk:json-value data :string "request_id")
                         (nlk:json-value data :any "id"))))
    (unless request-id
      (fail "GMI returned no request id. Check the console before submitting again."))
    (setf (gethash "request_id" data) request-id)
    data))

(defun poll (request-id deadline)
  "REQUEST-ID's answer once it is done, checking every few seconds, as the
original's poll()."
  (let ((delay (max 1 (setting :poll-seconds))))
    (loop
      (let* ((remaining (- deadline (now)))
             (data (call :get (format nil "requests/~a" (quri:url-encode (princ-to-string request-id) :encoding :utf-8))
                         :deadline deadline :retry t)))
        (when (finished data)
          (setf (gethash "request_id" data) request-id)
          (return data))
        (when (<= remaining 0)
          (fail "GMI job ~a timed out. Check its status before submitting again." request-id))
        (sleep (min delay (max 0 remaining)))
        (setf delay (min 5 (* delay 1.5)))))))

(defun run (model payload timeout)
  "MODEL with PAYLOAD, until its job is done or TIMEOUT seconds pass, as the
original's run()."
  (let* ((start (now))
         (data (submit model payload))
         (request-id (gethash "request_id" data)))
    (or (finished data)
        (poll request-id (+ start timeout)))))

;;; --- the download ---------------------------------------------------------------

(defun public-download-url-p (url)
  "URL as a result may be fetched from: HTTPS, a public hostname, no
credentials in it, no bare address -- the original's check_url()."
  (and (stringp url)
       (handler-case
           (let ((parsed (quri:uri url)))
             (and (equal (quri:uri-scheme parsed) "https")
                  (let ((host (string-right-trim "." (string-downcase (or (quri:uri-host parsed) "")))))
                    (and (plusp (length host))
                         (not (equal host "localhost"))
                         (not (uiop:string-suffix-p host ".localhost"))
                         (null (quri:uri-userinfo parsed))
                         (not (every (lambda (character)
                                       (or (digit-char-p character) (char= character #\.)))
                                     host))))))
         (error () nil))))

(defun download (url request-id)
  "URL's video written under the output directory => its path; a url that
cannot be fetched refuses, and the caller falls back to the url."
  (unless (public-download-url-p url)
    (fail "GMI's result URL is not a public HTTPS address: ~a." url))
  (multiple-value-bind (body status headers truncated)
      (nlk:http :get url :binary t :max-bytes +max-video-bytes+ :timeout 300 :connect-timeout 10)
    (declare (ignore headers))
    (unless (and (integerp status) (< status 300))
      (fail "The video download failed with HTTP ~a. The video is still at its url." status))
    (when truncated
      (fail "The video download exceeded ~a bytes." +max-video-bytes+))
    (let* ((directory (output-directory))
           (name (format nil "~a-~a.mp4"
                         (if request-id (princ-to-string request-id) "gmi")
                         (get-universal-time)))
           (path (merge-pathnames name directory)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
        (write-sequence body out))
      (namestring path))))

;;; --- the two verbs and the two tools --------------------------------------------

(defun generate (prompt &key model image-url duration (aspect-ratio "16:9") (resolution "720p")
                            negative-prompt audio seed)
  "Make one video from PROMPT, or from IMAGE-URL and PROMPT, with a GMI
model => one line a person reads, the saved file and what it cost."
  (running-settings)
  (let ((prompt (nlk:trimmed (or prompt ""))))
    (when (zerop (length prompt))
      (fail "A nonempty text prompt is required."))
    (let* ((record (select-model model (and image-url t)))
           (model-id (nlk:json-value record :string "model"))
           (duration-name (choose-param (parameters record) (cdr (assoc :duration +concepts+)))))
      (multiple-value-bind (payload dropped)
          (map-payload record :prompt prompt :image-url image-url :duration duration
                               :aspect-ratio aspect-ratio :resolution resolution
                               :negative-prompt negative-prompt :audio audio :seed seed)
        (let* ((seconds (and duration-name (gethash duration-name payload)))
               (result (run model-id payload (setting :timeout-seconds)))
               (url (first (media-urls result)))
               (request-id (nlk:json-value result :any "request_id"))
               (saved (handler-case (download url request-id) (gmi-video-error () nil))))
          (format nil "gmi-video: ~a made a ~@[~a s ~]video with ~a. ~a~@[ Request ~a.~]~@[ Price: ~a.~]~@[ Dropped: ~{~a~^, ~}.~]"
                  (if image-url "image-to-video" "text-to-video")
                  seconds
                  model-id
                  (if saved
                      (format nil "Saved to ~a." saved)
                      (format nil "The video is at ~a." url))
                  request-id
                  (price-text record)
                  dropped))))))

(defun models ()
  "The video models GMI's catalog lists now => one line each, with price."
  (running-settings)
  (let ((usable (remove-if-not (lambda (record) (fits-p record nil)) (records))))
    (if (null usable)
        "gmi-video: GMI's catalog lists no usable video model right now."
        (with-output-to-string (out)
          (format out "gmi-video: GMI Cloud video models, as the catalog states them.~%")
          (dolist (record usable)
            (format out "  ~a  ~a~@[ -- ~a~]~%"
                    (nlk:json-value record :string "model")
                    (price-text record)
                    (let ((brief (nlk:json-value record :string "brief_description")))
                      (and brief (plusp (length brief))
                           (subseq brief 0 (min 80 (length brief)))))))
          (format out "Defaults: text-to-video ~a, image-to-video ~a."
                  (or (setting :model) +text-model+)
                  (or (setting :image-model) +image-model+))))))

(defun answering (body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  (handler-case (funcall body)
    ((or error sb-sys:deadline-timeout) (condition)
      (nle:failure (format nil "ERROR: gmi-video: ~a" condition)))))

(defun tool-generate (arguments)
  "The gmi_video tool: make a video from a prompt or a public image url."
  (answering
   (lambda ()
     (apply #'generate
            (or (nlk:json-value arguments :string "prompt") "")
            (append
             (loop for (name key) in '(("image_url" :image-url) ("model" :model)
                                        ("duration" :duration) ("aspect_ratio" :aspect-ratio)
                                        ("resolution" :resolution) ("negative_prompt" :negative-prompt)
                                        ("seed" :seed))
                   for value = (nlk:json-value arguments :any name)
                   when value append (list key value))
             (multiple-value-bind (audio present) (nlk:json-value arguments :boolean "audio")
               (if present (list :audio audio) '())))))))

(defun tool-models (arguments)
  "The gmi_video_models tool: GMI's video models, live, with prices."
  (declare (ignore arguments))
  (answering #'models))

(defun run-command (args session-id)
  "One /gmi-video invocation: the video models and their prices."
  (declare (ignore args session-id))
  (answering #'models))

;;; --- the tool schemas ----------------------------------------------------------

(defparameter *generate-schema*
  (list :description (concatenate 'string
                                  "Generate one short video with GMI Cloud from a text prompt (or from a public "
                                  "image URL plus a prompt). Returns the saved file's path, the model, the "
                                  "duration and the price GMI states. Every call spends GMI credits: most "
                                  "video models bill per second of output.")
   :parameters (nlk:json-object
                "type" "object"
                "properties" (nlk:json-object
                              "prompt" (nlk:json-object
                                        "type" "string"
                                        "description" "What the video shows, in words. The camera move, the subject and the setting all belong here.")
                              "image_url" (nlk:json-object
                                           "type" "string"
                                           "description" "A public http(s) URL of an image to animate. A local file is refused: upload it first, then pass its URL.")
                              "model" (nlk:json-object
                                       "type" "string"
                                       "description" "A GMI video model id from gmi_video_models. Defaults to the section's model for text-to-video, its image_model for image-to-video.")
                              "duration" (nlk:json-object
                                          "type" "integer"
                                          "description" "Seconds of video; clamped to the nearest value the chosen model accepts.")
                              "aspect_ratio" (nlk:json-object
                                              "type" "string"
                                              "description" "16:9, 9:16, 1:1, or the names landscape, portrait, square. Clamped to what the model accepts.")
                              "resolution" (nlk:json-object
                                            "type" "string"
                                            "description" "720p, 1080p and so on; clamped to what the model accepts.")
                              "negative_prompt" (nlk:json-object
                                                 "type" "string"
                                                 "description" "What to keep out of the video, when the model takes it.")
                              "seed" (nlk:json-object
                                      "type" "integer"
                                      "description" "A seed for a repeatable result, when the model takes one."))
                "required" (vector "prompt")))
  "The gmi_video tool, as the original's generate() arguments have them.")

(defparameter *models-schema*
  (list :description (concatenate 'string
                                  "List the GMI Cloud video models that are live right now, each with the price "
                                  "GMI states and whether it takes an image. Read the catalog for one hour at "
                                  "a time; call it before naming a model in gmi_video.")
   :parameters (nlk:json-object
                "type" "object"
                "properties" (nlk:json-object)
                "required" #()))
  "The gmi_video_models tool.")

;;; --- the manual ----------------------------------------------------------------

(defparameter +primer+
  "GMI Cloud video is available through the nodecode-gmi-video cell: one tool that makes a video,
one that lists the models, and the same two verbs through eval.
  gmi_video         prompt=, image_url=, model=, duration=, aspect_ratio=, resolution=,
      negative_prompt=, seed=. It reads the chosen model's live schema, maps what that model
      accepts, drops the rest and reports what it dropped, submits the job, polls it and saves
      the video under the GMI cache. It answers the file's path, the model, the duration and the
      price GMI states.
  gmi_video_models  the live catalog: id, price, and a short description each.
  (gmi-video:generate \"a blue boat\" :duration 5 :resolution \"720p\")   the same call, from eval.
  (gmi-video:models)                                                     the same list.
Image-to-video takes a PUBLIC http(s) URL: upload the file first and pass its URL, never a path.
Every generation spends GMI credits with no approval prompt, and most models bill per second:
ask for the shortest duration that tells the story, and say the price in the answer. A text job
and an image job default to different models. Use it when the operator wants a moving picture,
not for a still image or for editing a video. ERROR: gmi-video-error names the problem; a key
that is missing or refused is the operator's to set in the gmi-video section, not something to
retry."
  "What (help :gmi-video) answers while the cell runs.")

;;; --- the entry: the one declaration ---------------------------------------------

(nle:define-cell gmi-video
  (:section ("gmi-video")
    (:guide "paste a GMI Cloud API key from console.gmicloud.ai in api_key, or export GMI_API_KEY; model names come live from GMI's catalog; every generation spends GMI credits")
    ("api_key" :secret
     :doc "the GMI Cloud API key")
    ("api_key_env" :env :default "GMI_API_KEY"
     :doc "the environment variable holding the GMI Cloud API key (GMI_API_KEY is read either way)")
    ("base_url" :string :default "https://console.gmicloud.ai/api/v1/ie/requestqueue/apikey"
     :doc "the GMI request-queue API address")
    ("model" :string :default "Minimax-Hailuo-02"
     :doc "the text-to-video model a call that names none uses")
    ("image_model" :string :default "Minimax-Hailuo-2.3-Fast"
     :doc "the image-to-video model a call that names none uses")
    ("timeout_seconds" :integer :default 900 :min 30
     :doc "how long one job may take before it is given up")
    ("poll_seconds" :integer :default 2 :min 0
     :doc "seconds between two status checks")
    ("output_dir" :path
     :doc "where a finished video is saved; the organism's cache/gmi-video unless answered"))
  (:help :gmi-video "gmi-video: gmi_video makes a video from a prompt or a public image URL with GMI Cloud" +primer+)
  (:command "gmi-video" 'run-command
            :description "List GMI Cloud's video models and their prices")
  (:tool "gmi_video" *generate-schema* 'tool-generate)
  (:tool "gmi_video_models" *models-schema* 'tool-models)
  (:start (lambda () (check-config))))
