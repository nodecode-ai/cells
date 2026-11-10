;;;; cell.lisp --- GMI Cloud speech as three tools, a /gmi-speech command and a
;;;; manual topic.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The plugin this cell rewrites registered GMI Cloud's speech models as
;;;; Hermes' own TTS provider: text in, an audio file out, on the user's GMI
;;;; credits. A cell has no voice pipeline to join, so the same wire is here
;;;; three tools the model calls:
;;;;
;;;;   gmi_speech_say     GET schema -> POST /requests -> poll -> download
;;;;   gmi_speech_voices  the voice options the model's schema declares
;;;;   gmi_speech_models  the speech models GMI lists for this key
;;;;
;;;; Everything the plugin did to a request is kept: the parameter names are
;;;; discovered from the model's own schema (voice_id or voice, format or
;;;; audio_encoding, speed or speaking_rate), the format falls back to one the
;;;; model offers, the pace is clamped to the model's range, a required
;;;; parameter is filled from its declared default, and the download is
;;;; HTTPS-only, a public hostname, and capped at 50 MB. What the plugin's
;;;; picker said about prices or a catalog over every model is not repeated:
;;;; a call reads one schema, once an hour.

(in-package #:nodecode-gmi-speech)

;;; --- the key and the endpoint ----------------------------------------------------

(defun api-key ()
  "The GMI Cloud key a call is billed to: the section's, else the variable
api_key_env names. A refusal here costs nothing, so it is raised before any
request."
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (when (zerop (length key))
      (setf key (nlk:trimmed (or (funcall *getenv* (setting :api-key-env)) ""))))
    (when (zerop (length key))
      (fail "no GMI Cloud key: paste one into the gmi-speech section's api_key, or set the ~
             variable ~a (a key is made at https://console.gmicloud.ai). Nothing was ~
             sent and nothing was spent." (setting :api-key-env)))
    (unless (every (lambda (char) (<= 33 (char-code char) 126)) key)
      (fail "the GMI Cloud key holds characters an API key cannot have; enter it again. ~
             Nothing was sent and nothing was spent."))
    key))

(defun checked-api-url (text)
  "TEXT as the endpoint, without its trailing slash, or a CONFIG-REFUSAL. Plain
http is allowed only for a loopback host: anywhere else it would send the key
in clear."
  (let ((url (string-right-trim "/" (nlk:trimmed (or text "")))))
    (unless (plusp (length url))
      (nlk:config-error "gmi-speech.api_base_url is empty; it is ~a unless a stand-in is wanted"
                        +default-base-url+))
    (let* ((secure (uiop:string-prefix-p "https://" url))
           (plain (uiop:string-prefix-p "http://" url))
           (rest (and (or secure plain) (subseq url (if secure 8 7))))
           (host (and rest (subseq rest 0 (or (position-if (lambda (char) (find char "/:?#")) rest)
                                               (length rest))))))
      (unless (and rest host (plusp (length host)))
        (nlk:config-error "gmi-speech.api_base_url is ~s; it must be an https URL, such as ~a"
                          url +default-base-url+))
      (when (and plain (not (member (string-downcase host)
                                    '("localhost" "127.0.0.1" "::1" "[::1]") :test #'string=)))
        (nlk:config-error "gmi-speech.api_base_url is ~s; plain http would send the key in clear, so ~
                           it is allowed only for a localhost stand-in" url))
      url)))

(defun api-host ()
  "The host the endpoint answers on."
  (let* ((url (setting :api-base-url))
         (rest (subseq url (if (uiop:string-prefix-p "https://" url) 8 7))))
    (string-downcase (subseq rest 0 (or (position-if (lambda (char) (find char "/:?#")) rest)
                                        (length rest))))))

(defun parse-speed (text)
  "TEXT as a pace, or NIL when empty; a CONFIG-REFUSAL when it is not a number.
The plugin clamped a pace to the model's own range; so does this cell."
  (let ((text (nlk:trimmed (or text ""))))
    (if (zerop (length text))
        nil
        (let ((value (ignore-errors
                       (multiple-value-bind (number end) (read-from-string text nil nil)
                         (and (realp number) (= end (length text))
                              (coerce number 'double-float))))))
          (unless value
            (nlk:config-error "gmi-speech.speed is ~s; it must be a number such as 1.0, or empty" text))
          (when (or (<= value 0) (> value 10))
            (nlk:config-error "gmi-speech.speed is ~a; a pace is a positive number, and 10 is already ~
                               very fast" text))
          value))))

;;; --- reading JSON from the wire ------------------------------------------------

(defun payload-status (data)
  "DATA's status, downcased."
  (string-downcase (or (nlk:json-value data :string "status") "")))

(defun finished-status-p (status)
  (member status '("success" "completed") :test #'string=))

(defun failed-status-p (status)
  (member status '("failed" "error" "cancelled") :test #'string=))

(defun detail-of (data)
  "DATA's own words for what went wrong, one line."
  (one-line (or (nlk:json-value data :string "error")
                (nlk:json-value data :string "message")
                "no detail")))

;;; --- one exchange ------------------------------------------------------------------

(defun gmi-exchange (method path &key body (timeout 60))
  "One request to the queue, as (values DECODED HTTP-STATUS). Nothing here
fails on a status: the caller decides what a failure means. A stub in a test
stands at NLK:HTTP."
  (let* ((key (api-key))
         (url (format nil "~a/~a" (setting :api-base-url) path)))
    (multiple-value-bind (reply status) (nlk:http method url
                                                    :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                                                                   (cons "content-type" "application/json")
                                                                   (cons "accept" "application/json"))
                                                    :content (and body (nlk:encode-json-object body))
                                                    :timeout timeout)
      (let ((data (ignore-errors (nlk:decode-json (octets-text reply)))))
        (values (if (hash-table-p data) data (nlk:json-object))
                (or status 0)
                key)))))

(defun status-failure (status data what)
  "The refusal a non-2xx STATUS is, in GMI's own words and the fix it names."
  (let ((detail (detail-of data)))
    (cond ((member status '(401 403))
           (fail "GMI rejected the credential: ~a. Check the key in gmi-speech.api_key, or the ~
                  variable api_key_env names, and what it may call. Nothing was spent." detail))
          ((= status 429)
           (fail "GMI rate-limited ~a: ~a. Wait before trying again, or choose another model." what detail))
          ((<= 400 status 499)
           (fail "GMI returned HTTP ~d for ~a: ~a. Check the model's inputs." status what detail))
          (t (fail "GMI returned HTTP ~d for ~a: ~a. Try again, and check the queue in the GMI ~
                     console before submitting twice." status what detail)))))

(defun gmi-request (method path &key body (timeout 60))
  "One request, refused unless it answered 2xx."
  (multiple-value-bind (data status) (gmi-exchange method path :body body :timeout timeout)
    (unless (<= 200 status 299)
      (status-failure status data path))
    data))

;;; --- one model's schema ---------------------------------------------------------

(defun schema-of (data)
  "DATA as a model record: GMI wraps it in a data member."
  (or (nlk:json-value data :object "data") data))

(defun fetch-schema (model)
  "MODEL's record from the queue, or a refusal. A stub in a test stands at
NLK:HTTP through GMI-EXCHANGE."
  (let ((data (gmi-request :get (format nil "models/~a" (url-path-segment model)))))
    (let ((record (schema-of data)))
      (unless (nlk:json-value record :string "model")
        (fail "GMI answered no model schema for ~a; choose another speech model." model))
      record)))

(defun model-record (model)
  "MODEL's schema, read once an hour. The plugin's catalog kept the same hour."
  (let* ((key (cons (setting :api-base-url) model))
         (cached (gethash key *schemas*)))
    (if (and cached (< (- (funcall *now*) (car cached)) +schema-ttl+))
        (cdr cached)
        (let ((record (fetch-schema model)))
          (setf (gethash key *schemas*) (cons (funcall *now*) record))
          record))))

(defun probe-model (model)
  "MODEL's schema when GMI answers one, or NIL; a credential or rate refusal
goes up, because trying the next name cannot fix it."
  (multiple-value-bind (data status) (gmi-exchange :get (format nil "models/~a" (url-path-segment model)))
    (cond ((<= 200 status 299) (schema-of data))
          ((member status '(401 403 429)) (status-failure status data model))
          (t nil))))

;;; --- what a record declares ------------------------------------------------------

(defun parameters (record)
  "RECORD's declared parameters, an alist (name . spec) in GMI's own order."
  (let ((out '()))
    (loop for spec across (nlk:json-array record "parameters")
          do (let ((name (and (hash-table-p spec) (nlk:json-value spec :string "name"))))
               (when name (push (cons name spec) out))))
    (nreverse out)))

(defun parameter (specs name)
  (cdr (assoc name specs :test #'string=)))

(defun parameter-options (spec)
  "SPEC's fixed values, `options' or `enum', each as GMI wrote it."
  (loop for item across (or (nlk:json-value spec :array "options")
                             (nlk:json-value spec :array "enum")
                             #())
        collect (if (hash-table-p item) (nlk:json-value item :any "value") (nlk:json-value item :any))))

(defun parameter-required-p (spec)
  (and spec (nlk:json-value spec :boolean "required")))

(defun parameter-default (spec)
  (and spec (nlk:json-value spec :any "default_value")))

(defun choose-param (specs names)
  "The first of NAMES RECORD declares, or NIL."
  (find-if (lambda (name) (parameter specs name)) names))

(defun model-of (record)
  (or (nlk:json-value record :string "model") "the model"))

(defun schema-problem (record)
  "Why RECORD cannot speak, as one line, or NIL when it can. The plugin's own
`usable' and `speech_model' tests: an audio model that returns audio, is not a
music model, takes words, and wants no lyrics. A field the schema leaves out is
not held against it -- only one that says the model is off."
  (let* ((model (model-of record))
         (specs (parameters record))
         (status (nlk:json-value record :string "status"))
         (workers (nlk:json-value record :integer "qworkers"))
         (metadata (nlk:json-value record :object "model_metadata"))
         (outputs (loop for item across (nlk:json-array (nlk:json-value record :object "modalities") "output")
                        collect (nlk:json-value item :string "type"))))
    (cond ((and status (not (string-equal status "active")))
           (format nil "~a is ~a, not active" model status))
          ((and workers (<= workers 0)) (format nil "~a has no worker to run it" model))
          ((equal "hidden" (nlk:json-value metadata :string "visibility"))
           (format nil "~a is hidden from the picker" model))
          ((and outputs (not (member "audio" outputs :test #'string=)))
           (format nil "~a returns no audio" model))
          ((search "music" (string-downcase model))
           (format nil "~a is a music model" model))
          ((null (choose-param specs +prompt-params+))
           (format nil "~a takes no text to speak" model))
          ((parameter-required-p (parameter specs "lyrics"))
           (format nil "~a needs lyrics as well as text" model))
          (t nil))))

(defun usable-record (record)
  "RECORD, or a refusal naming what about it cannot speak."
  (let ((problem (schema-problem record)))
    (when problem
      (fail "~a does not fit a speech call: it ~a. Choose another model, or set ~
             gmi-speech.model to one that speaks." (model-of record) problem))
    record))

(defun resolve-model (explicit)
  "The record to speak with: the call's own model, else the section's, else the
first of the plugin's preferred speech models GMI answers a usable schema for."
  (let ((named (nlk:trimmed (or explicit ""))))
    (when (zerop (length named)) (setf named (nlk:trimmed (or (setting :model) ""))))
    (if (plusp (length named))
        (usable-record (model-record named))
        (let ((found (loop for model in +speech-models+
                           for record = (probe-model model)
                           when (and record (null (schema-problem record)))
                             return record)))
          (or found
              (fail "GMI answered no usable speech model for this key: it tried ~{~a~^, ~}. ~
                     Set gmi-speech.model to one the catalog lists, and see gmi_speech_models."
                    +speech-models+))))))

;;; --- the payload, from the schema -----------------------------------------------

(defun find-option (options value)
  "VALUE as the spelling OPTIONS use, or NIL."
  (let ((wanted (princ-to-string value)))
    (find-if (lambda (option) (string-equal wanted (princ-to-string option))) options)))

(defun coerce-number (name spec value)
  "VALUE as a number for NAME, clamped to SPEC's own range."
  (unless (realp value)
    (fail "~a needs a number, and ~s is not one." name value))
  (let* ((minimum (nlk:json-value spec :number "min_value"))
         (maximum (nlk:json-value spec :number "max_value"))
         (clamped (if minimum (max minimum value) value))
         (clamped (if maximum (min maximum clamped) clamped)))
    (if (string-equal "integer" (or (nlk:json-value spec :string "type") ""))
        (round clamped)
        (coerce clamped 'double-float))))

(defun coerce-voice (name spec value)
  "VALUE as the voice NAME declares, or a refusal naming the voices there are."
  (let ((options (parameter-options spec)))
    (or (and options (find-option options value))
        (and (null options) (princ-to-string value))
        (fail "~s is not a voice ~a declares. It offers: ~{~a~^, ~}~@[ and more~]. Ask ~
               gmi_speech_voices for the list."
              value name (subseq options 0 (min 10 (length options)))
              (> (length options) 10)))))

(defun coerce-format (name spec requested)
  "REQUESTED as the container NAME declares: its own spelling, the plugin's alias
for it, its mp3, its default, or the first container it offers."
  (let* ((options (parameter-options spec))
         (wanted (string-downcase (nlk:trimmed (or requested "")))))
    (when (zerop (length wanted)) (setf wanted "mp3"))
    (if (null options)
        wanted
        (or (find-option options wanted)
            (let ((alias (cdr (assoc wanted +format-aliases+ :test #'string=))))
              (and alias (find-option options alias)))
            (find-option options "mp3")
            (parameter-default spec)
            (first options)
            (fail "~a declares a format with no value this cell can ask for." name)))))

(defun coerce-speed (name spec value)
  "VALUE as the pace NAME declares: one of its options, else a clamped number."
  (let ((options (parameter-options spec)))
    (if (null options)
        (coerce-number name spec value)
        (or (find-option options value)
            (let ((numeric (loop for option in options
                                 for number = (ignore-errors
                                                (multiple-value-bind (parsed end)
                                                    (read-from-string (princ-to-string option) nil nil)
                                                  (and (realp parsed) (= end (length (princ-to-string option)))
                                                       parsed)))
                                 when number collect (cons option number))))
              (when numeric
                (let* ((wanted (coerce value 'double-float))
                       (closest (first (sort numeric (lambda (a b) (< (abs (- (cdr a) wanted))
                                                                     (abs (- (cdr b) wanted))))))))
                  (car closest))))
            (fail "~s is not a pace ~a declares. It offers: ~{~a~^, ~}."
                  value name (subseq options 0 (min 10 (length options))))))))

(defun build-payload (record text voice speed format)
  "RECORD's declared parameters and this call's inputs as GMI's PAYLOAD, and the
container it settled on, as (values PAYLOAD CONTAINER)."
  (let* ((specs (parameters record))
         (payload (make-hash-table :test #'equal))
         (dropped '()))
    (flet ((put (name value) (setf (gethash name payload) value)))
      (let ((name (choose-param specs +prompt-params+)))
        (unless name
          (fail "~a declares no parameter for the words to speak." (model-of record)))
        (put name text))
      (when (plusp (length (nlk:trimmed (or voice ""))))
        (let ((name (choose-param specs +voice-params+)))
          (if name
              (put name (coerce-voice name (parameter specs name) voice))
              (push :voice dropped))))
      (when speed
        (let ((name (choose-param specs +speed-params+)))
          (if name
              (put name (coerce-speed name (parameter specs name) speed))
              (push :speed dropped))))
      (let ((name (choose-param specs +format-params+)))
        (if name
            (put name (coerce-format name (parameter specs name) format))
            (push :format dropped)))
      (dolist (pair specs)
        (destructuring-bind (name . spec) pair
          (when (and (parameter-required-p spec)
                     (not (nth-value 1 (gethash name payload))))
            (let ((default (parameter-default spec)))
              (unless default
                (fail "~a requires ~a and declares no default for it, so this cell cannot fill ~
                       it in; choose another speech model." (model-of record) name))
              (put name default)))))
      (values payload
              (let ((name (choose-param specs +format-params+)))
                (and name (gethash name payload)))
              dropped))))

;;; --- submit, follow, download ---------------------------------------------------

(defun submit-job (record payload)
  "One POST for MODEL and PAYLOAD, as (values REQUEST-ID RESPONSE)."
  (let* ((data (gmi-request :post "requests"
                             :body (nlk:json-object "model" (model-of record)
                                                     "payload" payload)))
         (id (or (nlk:json-value data :string "request_id")
                 (nlk:json-value data :string "id"))))
    (unless (and id (plusp (length id)))
      (fail "GMI accepted the request but returned no request id; check the console before ~
             submitting the same words again."))
    (values id data)))

(defun media-urls (data)
  "The URLs DATA's outcome carries, audio first."
  (let ((outcome (nlk:json-value data :object "outcome"))
        (urls '()))
    (flet ((collect (value)
             (dolist (item (if (and (vectorp value) (not (stringp value)))
                              (coerce value 'list)
                              (list value)))
               (let ((url (if (hash-table-p item) (nlk:json-value item :string "url") item)))
                 (when (and (stringp url) (plusp (length url)))
                   (pushnew url urls :test #'string=))))))
      (dolist (key '("audio_url" "media_urls" "image_url" "video_url"))
        (collect (and outcome (gethash key outcome))))
      (when (null urls)
        (collect (nlk:json-value outcome :string "thumbnail_image_url"))))
    (nreverse urls)))

(defun job-outcome (data)
  "DATA as a finished job, NIL while it still runs, or a refusal when it failed."
  (let ((status (payload-status data)))
    (cond ((failed-status-p status)
           (fail "GMI's job ~a: ~a. Review the inputs before trying again." status (detail-of data)))
          ((finished-status-p status)
           (unless (media-urls data)
             (fail "GMI finished the job but returned no audio URL; check the request in the GMI console."))
           data)
          (t nil))))

(defun await-job (request-id deadline)
  "Follow REQUEST-ID until it finishes, or DEADLINE passes. The plugin polled the
same way: at once, then at a growing interval, never past five seconds."
  (let ((delay 1.0d0))
    (loop
      (let ((data (gmi-request :get (format nil "requests/~a" (url-path-segment request-id)))))
        (let ((done (job-outcome data)))
          (when done (return done))))
      (when (> (funcall *now*) deadline)
        (fail "GMI's job ~a was still running after ~a seconds. Check its status in the ~
               console before submitting the same words again."
              request-id (setting :poll-timeout-seconds)))
      (funcall *sleep* (max 1 (round delay)))
      (setf delay (min 5.0d0 (* delay 1.5d0))))))

(defun run-model (record payload)
  "Submit PAYLOAD to RECORD and answer the finished job."
  (let ((deadline (+ (funcall *now*) (setting :poll-timeout-seconds))))
    (multiple-value-bind (request-id data) (submit-job record payload)
      (or (job-outcome data) (await-job request-id deadline)))))

(defun download-host-ok-p (url)
  "Whether URL may be fetched for audio: https to a public hostname, no IP
literal, no loopback -- the plugin's own download check."
  (let* ((secure (uiop:string-prefix-p "https://" url))
         (rest (and secure (subseq url 8)))
         (authority (and rest (subseq rest 0 (or (position-if (lambda (char) (find char "/?#")) rest)
                                                  (length rest)))))
         (after-user (and authority (subseq authority (1+ (or (position #\@ authority :from-end t) -1)))))
         (host (and after-user (string-downcase (subseq after-user 0 (or (position #\: after-user)
                                                                        (length after-user)))))))
    (and secure host (plusp (length host))
         (not (search "localhost" host))
         (not (every (lambda (char) (or (digit-char-p char) (find char ".:[]"))) host)))))

(defun audio-url (data)
  "DATA's recording URL, or a refusal."
  (let ((url (first (media-urls data))))
    (unless url
      (fail "GMI finished the job but named no audio file; check the request in the GMI console."))
    (unless (download-host-ok-p url)
      (fail "GMI pointed at an audio URL this cell does not trust -- https to a public hostname ~
             is what it downloads from -- so nothing was fetched: ~a" (one-line url 120)))
    url))

(defun download-audio (url)
  "URL as an octet vector, refused when it is empty or larger than the cap.
NLK:HTTP follows up to five redirects on its own."
  (multiple-value-bind (body status headers truncated)
      (nlk:http :get url :binary t :max-bytes +max-audio-bytes+ :timeout 300)
    (declare (ignore headers))
    (when truncated
      (fail "GMI's recording is larger than the ~a this cell downloads in one call."
            (nlk:size-text +max-audio-bytes+)))
    (let ((octets (if (stringp body) (map '(vector (unsigned-byte 8)) #'char-code body) body)))
      (unless (= status 200)
        (fail "GMI's audio download answered HTTP ~d. Check the result URL." status))
      (when (zerop (length octets))
        (fail "GMI's audio download was empty. Check the result URL."))
      octets)))

(defun save-audio (octets text extension record voice container)
  "Write OCTETS under the section's folder, never over an existing file, and
answer the model where it is."
  (let* ((directory (setting :directory))
         (name (format nil "gmi_~a_~a_~a~a" (slug text) (datestamp) (fresh-id) extension))
         (path (merge-pathnames name directory)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :error
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
      (write-sequence octets out))
    (with-output-to-string (out)
      (format out "saved ~a~%" (namestring path))
      (format out "~a spoke it~@[ in ~a~], ~a, ~a~%"
              (model-of record) voice (or container "audio") (nlk:size-text (length octets)))
      (format out "Give the operator that path: any audio player opens it.~%")
      (format out "The call was billed to the GMI Cloud account the key belongs to.~%"))))

;;; --- speaking ------------------------------------------------------------------

(nlk:define-contract say-request
  (:doc "Say text out loud with a GMI Cloud speech model and save the recording on this machine: one call, one file. The answer names the saved path -- give that path to the operator. Billed to the operator's GMI credits. Pick a voice with gmi_speech_voices when the section names none.")
  ("text" :text :required t
          :doc "what to say, in the words to be spoken; numbers and names are read as written")
  ("voice" :text
           :doc "a voice the model declares, as gmi_speech_voices lists it; omit for the gmi-speech section's voice, or the model's own default")
  ("speed" :number :slot pace
           :doc "the pace, clamped to the model's own range; omit for the section's pace or the model's default")
  ("format" :text :slot container
            :doc "mp3, wav, flac or ogg; a model that cannot write it falls back to one it can, and the answer's file carries the container's own extension")
  ("model" :text
           :doc "a GMI speech model id, such as minimax-tts-speech-2.6-turbo; omit for the gmi-speech section's model"))

(defun say-now (request)
  "One utterance: the model's schema, its payload, the request queue and the
recording, saved."
  (let* ((text (nlk:trimmed (say-request-text request)))
         (record (resolve-model (say-request-model request)))
         (voice (or (nlk:trimmed (or (say-request-voice request) ""))
                    (nlk:trimmed (or (setting :voice) ""))))
         (speed (or (say-request-pace request) (setting :speed)))
         (format (or (nlk:trimmed (or (say-request-container request) ""))
                     (nlk:trimmed (or (setting :format) ""))
                     "mp3")))
    (multiple-value-bind (payload container) (build-payload record text voice speed format)
      (let ((extension (audio-extension container)))
        (unless extension
          (fail "~a declares the container ~s, which names no audio file this cell writes. ~
                 Ask for mp3, wav, flac or ogg, or choose another speech model."
                (model-of record) container))
        (let ((data (run-model record payload)))
          (save-audio (download-audio (audio-url data)) text extension record
                       (and (plusp (length voice)) voice) container))))))

(defun say (request)
  "The gmi_speech_say tool, one refusal away from the model's own words."
  (handler-case (say-now request)
    (gmi-speech-error (condition)
      (nle:failure (format nil "ERROR: ~a" (gmi-speech-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- the voices a model declares -----------------------------------------------

(nlk:define-contract voice-request
  (:doc "List the voices a GMI Cloud speech model declares, from its own live schema, so a voice can be named in gmi_speech_say or kept in the gmi-speech section. One schema read; nothing is generated and nothing is spent.")
  ("model" :text
           :doc "the GMI speech model whose voices to list; omit for the gmi-speech section's model")
  ("query" :text
           :doc "only voices whose id holds this text, case-insensitively; omit for all of them"))

(defun voice-rows (record)
  "RECORD's voice parameter and the voices it declares, as (values NAME VOICES
FROM-SCHEMA-P)."
  (let* ((specs (parameters record))
         (name (choose-param specs +voice-params+))
         (declared (and name (parameter-options (parameter specs name)))))
    (if declared
        (values name declared t)
        (values name (copy-list +fallback-voices+) nil))))

(defun voices-now (request)
  "The gmi_speech_voices answer: what the model declares and which one the section
speaks with."
  (let* ((record (usable-record (resolve-model (voice-request-model request))))
         (query (string-downcase (nlk:trimmed (or (voice-request-query request) "")))))
    (multiple-value-bind (name voices declared-p) (voice-rows record)
      (let* ((matching (if (zerop (length query)) voices
                           (remove-if-not (lambda (voice)
                                            (search query (string-downcase (princ-to-string voice))))
                                          voices)))
             (chosen (nlk:trimmed (or (setting :voice) ""))))
        (when (zerop (length matching))
          (fail "no voice of ~a matches ~s. It offers: ~{~a~^, ~}."
                (model-of record) query (subseq voices 0 (min 10 (length voices)))))
        (with-output-to-string (out)
          (format out "~d voice~:p of ~a~@[, matching ~s~]~%~%"
                  (length matching) (model-of record)
                  (and (plusp (length query)) query))
          (dolist (voice matching)
            (format out "~a~a~%" (if (and (plusp (length chosen))
                                             (string-equal chosen (princ-to-string voice)))
                                       "* " "  ")
                    voice))
          (format out "~%Pass one as voice to gmi_speech_say, or keep it in gmi-speech.voice for every call.~%")
          (if declared-p
              (format out "Read from the model's own schema (~a), so GMI is the authority on it.~%"
                      (or name "voice"))
              (format out "~a declares no voice option, so these are the voices the plugin offered: ~
                           GMI's own default is what speaks unless one of them is accepted.~%"
                      (model-of record)))
          (format out "Nothing was generated: one schema read, and nothing spent.~%"))))))

(defun voices (request)
  "The gmi_speech_voices tool, refusing the same way as speak."
  (handler-case (voices-now request)
    (gmi-speech-error (condition)
      (nle:failure (format nil "ERROR: ~a" (gmi-speech-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- the models the key may call ------------------------------------------------

(nlk:define-contract model-request
  (:doc "List the model ids GMI's request queue holds for this key, so a speech model can be named in gmi_speech_say or kept in the gmi-speech section. One request, nothing generated and nothing spent.")
  ("query" :text
           :doc "only ids holding this text, case-insensitively, such as tts or minimax; omit for all of them"))

(defun uuid-shaped-p (text)
  "Whether TEXT reads as a uuid, which the queue's own internal ids do."
  (and (= (length text) 36)
       (loop for index from 0 below 36
             for char = (char text index)
             always (if (member index '(8 13 18 23))
                        (char= char #\-)
                        (digit-char-p char 16)))))

(defun public-model-id-p (text)
  "Whether TEXT names a model a person can choose: not a uuid, and none of the
queue's own internal markers -- the plugin's public_id rule."
  (and (stringp text) (plusp (length text))
       (not (uuid-shaped-p text))
       (null (search "capture" (string-downcase text)))
       (null (search "probe" (string-downcase text)))
       (null (search "ssrf" (string-downcase text)))
       (null (search "test" (string-downcase text)))
       (null (search "hunt" (string-downcase text)))
       (null (search "bypass" (string-downcase text)))))

(defun models-now (request)
  "The gmi_speech_models answer: the ids the queue lists, filtered by the call's
own words."
  (let* ((data (gmi-request :get "models"))
         (ids (loop for id across (nlk:json-array data "model_ids")
                    when (public-model-id-p id) collect id))
         (query (string-downcase (nlk:trimmed (or (model-request-query request) ""))))
         (matching (if (zerop (length query)) ids
                       (remove-if-not (lambda (id) (search query (string-downcase id))) ids)))
         (chosen (nlk:trimmed (or (setting :model) ""))))
    (when (zerop (length ids))
      (fail "GMI listed no model for this key. Check the key's permissions in the GMI console."))
    (when (zerop (length matching))
      (fail "no model id GMI lists matches ~s; ~d are listed." query (length ids)))
    (with-output-to-string (out)
      (format out "~d model~:p~@[ matching ~s~] of the ~d this key may call~%~%"
              (length matching) (and (plusp (length query)) query) (length ids))
      (dolist (id (subseq matching 0 (min 60 (length matching))))
        (format out "~a~a~a~%" (if (string-equal chosen id) "* " "  ") id
                (if (member id +speech-models+ :test #'string=)
                    "   (a speech model this cell knows)" "")))
      (when (> (length matching) 60)
        (format out "... and ~d more; say query to narrow them~%" (- (length matching) 60)))
      (format out "~%The queue also lists image, video and music models. With none chosen, the cell ~
                   asks GMI for ~{~a~^, then ~} and speaks with the first it accepts."
              +speech-models+)
      (format out "~%Nothing was generated: one request, and nothing spent.~%"))))

(defun models (request)
  "The gmi_speech_models tool, refusing the same way as speak."
  (handler-case (models-now request)
    (gmi-speech-error (condition)
      (nle:failure (format nil "ERROR: ~a" (gmi-speech-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- /gmi-speech --------------------------------------------------------------

(defun key-state ()
  "Whether a key is ready, and where it comes from -- never the key itself."
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (cond ((plusp (length key)) "set in gmi-speech.api_key")
          ((plusp (length (nlk:trimmed (or (funcall *getenv* (setting :api-key-env)) ""))))
           (format nil "read from the variable ~a" (setting :api-key-env)))
          (t (format nil "not set: fill api_key in /settings, or export ~a"
                     (setting :api-key-env))))))

(defun status-text ()
  "What /gmi-speech says with no argument: the settings every call runs on."
  (with-output-to-string (out)
    (format out "GMI Cloud speech~%")
    (format out "  key: ~a~%" (key-state))
    (format out "  model: ~a~%" (let ((model (nlk:trimmed (or (setting :model) ""))))
                                    (if (plusp (length model)) model
                                        (format nil "none chosen: ~{~a~^, then ~}, whichever GMI accepts"
                                                +speech-models+))))
    (format out "  voice: ~a~@[, speed ~a~], format ~a~%"
            (let ((voice (nlk:trimmed (or (setting :voice) ""))))
              (if (plusp (length voice)) voice "the model's own default"))
            (setting :speed) (setting :format))
    (format out "  files: ~a~%" (namestring (setting :directory)))
    (format out "  endpoint: ~a, job ceiling ~a s~%"
            (setting :api-base-url) (setting :poll-timeout-seconds))
    (format out "  /gmi-speech voices [model] -- the voices; /gmi-speech models [words] -- the ids; ~
                 /gmi-speech say <text> -- speak it~%")))

(defun command-text (argument)
  "ARGUMENT, the words after /gmi-speech, as the text the shell shows."
  (let* ((argument (nlk:trimmed (or argument "")))
         (space (position #\Space argument))
         (word (string-downcase (if space (subseq argument 0 space) argument)))
         (rest (if space (nlk:trimmed (subseq argument (1+ space))) "")))
    (cond ((zerop (length argument)) (status-text))
          ((string= word "say")
           (if (zerop (length rest))
               "Say what to speak: /gmi-speech say Good morning from the lake."
               (say-now (make-say-request :text rest))))
          ((string= word "voices")
           (voices-now (if (zerop (length rest)) (make-voice-request)
                           (make-voice-request :model rest))))
          ((string= word "models")
           (models-now (if (zerop (length rest)) (make-model-request)
                           (make-model-request :query rest))))
          ((string= word "status") (status-text))
          (t (format nil "Unknown: /gmi-speech ~a. Try /gmi-speech, /gmi-speech voices [model], ~
                          /gmi-speech models [words], or /gmi-speech say <text>.~%"
                     argument)))))

(defun run-command (args session-id)
  "The /gmi-speech command: the settings, the voices, the model ids, or one utterance."
  (declare (ignore session-id))
  (handler-case (command-text args)
    (gmi-speech-error (condition) (gmi-speech-error-detail condition))
    (error (condition) (format nil "gmi-speech: ~a" condition))))

;;; --- the manual -----------------------------------------------------------------

(defparameter +primer+
  "GMI Cloud speech is available through the nodecode-gmi-speech cell: three tools
and the /gmi-speech command, on one GMI Cloud key billed to the operator's credits.
  gmi_speech_say(text, voice?, speed?, format?, model?)
      one utterance, saved as an audio file in this profile's voice folder; the
      answer names the path -- give it to the operator. At most a paragraph or two
      per call: the file is played, not read. The voice, the pace and the container
      are checked against the model's own schema; a container the model cannot
      write falls back to one it can, and the file carries that extension.
  gmi_speech_voices(model?, query?)
      the voices the model's live schema declares -- one schema read, nothing
      generated. Pick an id here before asking for a recording.
  gmi_speech_models(query?)
      the model ids the queue lists for this key; the speech ones are audio
      models, and with none chosen the cell asks for minimax-tts-speech-2.6-turbo,
      then inworld-tts-1.5-mini.
Every generation call spends the operator's GMI credits with no further prompt.
The section holds the defaults: model, voice, speed, format, directory,
api_base_url, poll_timeout_seconds, and the key (api_key, or the variable
api_key_env names)."
  "What (help :gmi-speech) answers while the cell runs.")

;;; --- the cell -----------------------------------------------------------------

(defun checked-settings (values table)
  "The section's values with the two shapes the declaration cannot say: an
endpoint that will not send the key in clear, and a pace read from a string."
  (declare (ignore table))
  (setf (getf values :api-base-url) (checked-api-url (getf values :api-base-url)))
  (setf (getf values :speed) (parse-speed (getf values :speed)))
  (let ((timeout (getf values :poll-timeout-seconds)))
    (when (or (> timeout +max-poll-timeout+) (< timeout +min-poll-timeout+))
      (nlk:config-error "gmi-speech.poll_timeout_seconds is ~a; it must be from ~a to ~a"
                        timeout +min-poll-timeout+ +max-poll-timeout+)))
  values)

(nle:define-cell gmi-speech
  (:section ("gmi-speech")
    (:guide "make a key at https://console.gmicloud.ai and paste it in api_key, or name the variable that holds it in api_key_env; ask gmi_speech_models for the ids, gmi_speech_voices for a voice, and keep the ones to use by default in model and voice; format is mp3 unless the model offers another; directory is where the recordings are kept; every generation spends GMI credits")
    ("api_key" :secret
               :doc "the GMI Cloud key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "GMI_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("model" :string
             :doc "the speech model every call uses unless it names one, such as minimax-tts-speech-2.6-turbo; empty, the first of GMI's speech models this key accepts")
    ("voice" :string
             :doc "the voice every call speaks in unless it names one, as gmi_speech_voices lists it; empty, the model's own default")
    ("speed" :string
             :doc "the pace every call asks for, written as a number; the model clamps it to its own range. Empty, the model's default")
    ("format" :string :default "mp3"
              :doc "the container to ask for: mp3, wav, flac or ogg. A model that cannot write it falls back to one it can, and the file carries that extension")
    ("directory" :directory :default "voice/"
                 :doc "where the recordings are kept, under the home")
    ("api_base_url" :string :default "https://console.gmicloud.ai/api/v1/ie/requestqueue/apikey"
                    :doc "the GMI request queue the plugin dialled; change it only for a stand-in")
    ("poll_timeout_seconds" :integer :default 300 :min 30
                             :doc "how long one job is followed before the call gives up, in seconds; 30 to 1800"))
  (:settings #'checked-settings)
  (:tool "gmi_speech_say" (nlk:contract 'say-request) 'say)
  (:tool "gmi_speech_voices" (nlk:contract 'voice-request) 'voices)
  (:tool "gmi_speech_models" (nlk:contract 'model-request) 'models)
  (:help :gmi-speech "gmi-speech: GMI Cloud speech - speak text into an audio file, list a model's voices and the ids" +primer+)
  (:command "gmi-speech" 'run-command
            :description "GMI Cloud speech: the settings, the voices, the models, or one utterance"
            :argument-hint "[status|voices [model]|models [words]|say <text>]"
            :session nil)
  (:start (lambda ()
            ;; A schema read outside this start is stale the moment the key or
            ;; the endpoint changes; and the folder the operator is pointed at
            ;; exists from the first start, before the first utterance.
            (clrhash *schemas*)
            (ensure-directories-exist (merge-pathnames "voice.keep" (setting :directory))))))
