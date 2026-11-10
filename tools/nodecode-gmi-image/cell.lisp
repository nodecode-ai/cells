;;;; cell.lisp --- gmi_image and /gmi-image, over GMI Cloud's media API.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One API base the operator names, in the `gmi_image' section of the
;;;; shared config:
;;;;   "gmi_image": {"key_env": "GMI_API_KEY", "model": "seedream-5.0-lite"}
;;;; Everything the cell does is GMI's request queue: GET <api>/models/<id> for
;;;; the schema of the model it will call, POST <api>/requests to submit, GET
;;;; <api>/requests/<id> until the job is finished, then the picture from the
;;;; URL it answers. Nothing hooks the provider path: GMI speaks HTTP, and
;;;; NLK:HTTP is the one seam.
;;;;
;;;; Rewritten from zkasuran's Hermes Agent plugin (see NOTICE): the one
;;;; requestqueue base URL, the bearer key and its scrubbing, the model schema
;;;; (parameters, their options and bounds, modalities, qworkers, pricing) and
;;;; the payload mapped onto it, the aspect-to-size fallback, image inputs as
;;;; public URLs only and at most three, the submit-then-poll job with its
;;;; backoff and deadline, the outcome's media URLs, and the picture written
;;;; under output_dir at once because GMI's link may expire.

(in-package #:nodecode-gmi-image)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Images are available through the nodecode-gmi-image cell: the gmi_image tool,
which a person can also call from eval in the gmi-image package, and the /gmi-image
command. Every call spends the operator's GMI credits.
  gmi_image  {prompt, aspect, model, images}
      One job on GMI Cloud's request queue: the model's schema is read first, the
      payload is mapped onto the parameters that model declares, and the picture is
      written under the cell's output_dir. The answer names the file, its size in
      bytes, the model, the aspect and the ratio asked for, the decoded pixel size,
      the request id and the model's price. Say where the file is; do not paste its
      bytes.
      aspect is landscape (16:9), square (1:1) or portrait (9:16). A model that
      declares an aspect parameter gets it; one that declares sizes gets the nearest
      size it offers, so the shape can differ from the request.
      model names a GMI model id. Left empty, the cell probes its own preferences -
      seedream-5.0-lite, gemini-2.5-flash-image, gpt-image-2-generate - and takes
      the first the key can reach that accepts these inputs. A model that needs a
      source picture is only chosen when the call names one.
      images are pictures to edit or to take a subject from: public http(s) URLs,
      at most three. GMI fetches them from its own side, so a local path is refused.
Write the prompt as an image model reads it - subject, setting, style, light, framing -
not as a question. A job that is still running is polled until the section's timeout;
a failed job names GMI's own reason, and a refused answer names the status and the
setting that usually fixes it. The key never appears in an answer. Type /gmi-image with
a prompt for the same thing from the operator's side: /gmi-image --portrait a red fox in
snow; /gmi-image --image https://example.com/fox.png the same fox, at night;
/gmi-image --model seedream-5.0-lite a heron. Typed alone it asks for the prompt."
  "What (help :gmi-image) answers while the cell runs.")

;;; --- what GMI's image models take ------------------------------------------------

(defparameter +default-api+
  "https://console.gmicloud.ai/api/v1/ie/requestqueue/apikey"
  "The one requestqueue base the original plugin dials.")

(defparameter +default-key-env+ "GMI_API_KEY"
  "The variable the key is read from unless the section names another.")

(defparameter +preferences+
  '("seedream-5.0-lite" "gemini-2.5-flash-image" "gpt-image-2-generate")
  "The models the cell probes, in order, when neither the call nor the section
names one: the original plugin's own preference list.")

(defparameter +aspects+
  '(("landscape" . "16:9") ("square" . "1:1") ("portrait" . "9:16"))
  "The aspect the call names to the ratio an aspect parameter takes.")

(defparameter +image-names+
  '("image" "first_frame_image" "first_frame" "image_url" "input_image")
  "The parameter names a model uses for a source picture, in the order the
original plugin probes them.")

(defparameter +max-references+ 3
  "The most source pictures one call sends: the original provider's cap.")

(defparameter +max-pixels+ 4194304
  "The most pixels a size the cell picks for a model may cost: 2048x2048,
the original plugin's ceiling for an auto-chosen size.")

(defparameter +catalog-ttl+ 3600
  "Seconds one model schema is kept before it is read again.")

(defparameter +error-chars+ 300
  "How much of GMI's own error text a refusal quotes back.")

(defparameter +user-agent+ "nodecode-gmi-image/0.1.0"
  "What the cell calls itself to GMI. Nothing of another client's is
impersonated, and no other product's session header rides along.")

(defparameter *schemas* (make-hash-table :test #'equal)
  "Model schemas read in this image, keyed by api base and model id, each
(TIME . RECORD): the original plugin's one-hour catalog cache, narrowed to the
models a call actually names.")

;;; --- answers, and the one condition ---------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (gmi-image-error (condition)
       (nle:failure (format nil "ERROR: ~a" (gmi-image-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: gmi-image: ~a"
                            (nlk:one-line (princ-to-string condition)))))))

(defun answer-detail (body)
  "GMI's own words for a failed call, clipped, or NIL."
  (let ((text (and (stringp body) (nlk:trimmed body))))
    (and text (plusp (length text)) (nlk:clip text +error-chars+))))

(defun replace-text (text part replacement)
  "TEXT with every occurrence of PART replaced."
  (if (or (null part) (zerop (length part)) (null (search part text)))
      text
      (with-output-to-string (out)
        (loop with start = 0
              for at = (search part text :start2 start)
              while at
              do (write-string text out :start start :end at)
                 (write-string replacement out)
                 (setf start (+ at (length part)))
              finally (write-string text out :start start)))))

(defun redact (text)
  "TEXT with the key, when one is configured, and any bearer token in it,
replaced: the original plugin's scrub, narrowed to what can leak here."
  (let* ((text (if (stringp text) text (princ-to-string text)))
         (key (setting :api-key))
         (text (if (and key (plusp (length key))) (replace-text text key "[key]") text)))
    (with-output-to-string (out)
      (loop with start = 0
            for mark = (search "Bearer " text :start2 start :test #'char-equal)
            while mark
            do (write-string text out :start start :end mark)
               (write-string "Bearer [redacted]" out)
               (setf start (or (position-if (lambda (c) (find c '(#\Space #\Tab #\Newline #\Return #\" #\')))
                                            text :start (min (length text) (+ mark 7)))
                               (length text)))
            finally (write-string text out :start start)))))

;;; --- reading the section --------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun url-parts (url)
  "(values SCHEME HOST USERINFO-P) of an absolute URL, both lowercased, or NILs."
  (let ((mark (and (stringp url) (search "://" url))))
    (when (and mark (plusp mark))
      (let* ((scheme (string-downcase (subseq url 0 mark)))
             (rest (subseq url (+ mark 3)))
             (end (or (position-if (lambda (character) (find character "/?#")) rest)
                      (length rest)))
             (authority (subseq rest 0 end))
             (at (position #\@ authority :from-end t))
             (hostport (if at (subseq authority (1+ at)) authority))
             (host (cond ((and (plusp (length hostport)) (char= #\[ (char hostport 0)))
                          (subseq hostport 1 (or (position #\] hostport) (length hostport))))
                         (t (subseq hostport 0 (or (position #\: hostport) (length hostport)))))))
        (values scheme (string-downcase host) (and at t))))))

(defun public-url-p (url)
  "True when URL is an absolute http(s) URL naming a host: what GMI will fetch."
  (and (stringp url)
       (multiple-value-bind (scheme host) (url-parts url)
         (and (member scheme '("http" "https") :test #'string=)
              host (plusp (length host))))))

(defun check-api (url)
  "URL, without its trailing slashes, or a refusal saying what is wrong with it."
  (let ((url (nlk:trimmed (or url ""))))
    (when (zerop (length url))
      (nlk:config-error "api is empty: set it to the GMI requestqueue base, e.g. ~a" +default-api+))
    (when (find-if (lambda (character) (member character '(#\Space #\Tab #\Newline #\Return))) url)
      (nlk:config-error "api ~s holds whitespace" url))
    (when (nth-value 2 (url-parts url))
      (nlk:config-error "api must not carry credentials in it: name the key in key_env"))
    (multiple-value-bind (scheme host) (url-parts url)
      (unless scheme
        (nlk:config-error "api ~s is not an absolute URL: give the scheme too, e.g. ~a" url +default-api+))
      (unless (member scheme '("http" "https") :test #'string=)
        (nlk:config-error "api scheme ~s is not http or https" scheme))
      (when (string= scheme "http")
        (nlk:config-error "api ~s is plain HTTP: the key would cross the public internet in clear text. Use https://~a" url host))
      (string-right-trim "/" url))))

(defun written-string (table key)
  "KEY in the section TABLE as the operator wrote it => (T . TEXT) when it
was answered at all, (T) when it was answered blank, NIL when it was absent."
  (when table
    (multiple-value-bind (value present) (nlk:section-value table key)
      (and present
           (cons t (and (stringp value) (nonblank value)))))))

(defun read-settings (values table)
  "The declared VALUES, plus the key the section names and the folder pictures
are written to: read once, here."
  (let* ((api (check-api (or (getf values :api) +default-api+)))
         ;; KEY_ENV carries a default, so the section as written is what tells
         ;; an answered variable from an absent one: "" is an answer that
         ;; names nothing, and a key_file stands alone.
         (written (written-string table "key_env"))
         (file (nonblank (getf values :key-file)))
         (env (cond ((and written (null (cdr written)))
                     (nlk:config-error "key_env is empty: name the environment variable holding the GMI key, or set key_file"))
                    ((cdr written))
                    (file nil)
                    (t +default-key-env+)))
         (plist (copy-list values)))
    (when (and env file)
      (nlk:config-error "set one of key_env and key_file, not both"))
    (setf (getf plist :api) api
          (getf plist :output-dir)
          (uiop:ensure-directory-pathname
           (nlk:expand-home (or (nonblank (getf values :output-dir)) "~/.nodecode/images")))
          (getf plist :api-key)
          (cond (env (nonblank (nle::credential-env env)))
                (file (or (nonblank (ignore-errors
                                     (uiop:read-file-string (nlk:expand-home file))))
                          (nlk:config-error "key_file ~a cannot be read, or is empty" file)))
                (t nil)))
    (let ((key (getf plist :api-key)))
      (when (and key (find-if (lambda (character)
                                (member character '(#\Space #\Tab #\Newline #\Return)))
                              key))
        (nlk:config-error "the key ~a names holds whitespace or line breaks" (or env file))))
    plist))

;;; --- one exchange with GMI --------------------------------------------------------

(defun api-url (path)
  (concatenate 'string (setting :api) "/" path))

(defun path-escape (text)
  "TEXT safe in one path segment: what quote(..., safe='') did."
  (with-output-to-string (out)
    (loop for character across (princ-to-string text)
          do (if (or (alphanumericp character)
                     (find character "-._~"))
                 (write-char character out)
                 (format out "%~2,'0X" (char-code character))))))

(defun headers (&key accept (content-type "application/json"))
  "The headers every GMI request carries: the key as a bearer token and
nowhere else."
  (append (list (cons "user-agent" +user-agent+)
                (cons "accept" (or accept "application/json")))
          (let ((key (setting :api-key)))
            (when key (list (cons "authorization" (format nil "Bearer ~a" key)))))
          (when content-type (list (cons "content-type" content-type)))))

(defun dial (method url &key content binary (accept "application/json"))
  "One exchange with GMI => (values BODY STATUS). A dial that fails is a
refusal, never an unhandled error; the key is never put in a URL or a log line."
  (handler-case
      (nlk:http method url
                :headers (headers :accept accept)
                :content content
                :binary binary
                :timeout (setting :timeout)
                :connect-timeout (min 15 (setting :timeout)))
    (error (condition)
      (fail "~(~a~) ~a did not answer: ~a" method url
            (nlk:one-line (princ-to-string condition))))))

(defun status-hint (status)
  "The parenthetical hint the model can act on, or NIL."
  (case status
    ((400 422) " (check the model id, the aspect and the image URLs: GMI refuses a payload its model does not declare)")
    ((401 403) " (check the key in the variable key_env names, and that it belongs to the GMI Cloud account you mean)")
    (404 " (check the model id: GMI lists the ids its key can reach in the console)")
    (429 " (GMI is rate limiting this model or the account is out of quota: retry in a moment, or choose another model)")
    (t nil)))

(defun error-message (body)
  "GMI's own words for a failed answer: its error or message member when the
body is the JSON object it answers with, else the text itself."
  (let* ((text (and (stringp body) body))
         (object (and text
                      (plusp (length (nlk:trimmed text)))
                      (ignore-errors (nlk:decode-json text))))
         (detail (or (and (hash-table-p object) (nlk:json-value object :string "error"))
                     (and (hash-table-p object) (nlk:json-value object :string "message")))))
    (or detail
        (and text (plusp (length (nlk:trimmed text)))
             (nlk:clip (nlk:one-line text) +error-chars+))
        "no error detail")))

(defun rate-limit-refusal (status body model)
  "A 429 said in the terms the original plugin used: a daily model limit is a
different problem from a burst."
  (declare (ignore status))
  (let* ((object (and (stringp body) (ignore-errors (nlk:decode-json body))))
         (limit (and (hash-table-p object) (nlk:json-value object :object "rate_limit")))
         (type (and limit (nlk:json-value limit :string "limit_type")))
         (value (and limit (or (nlk:json-value limit :any "limit")
                               (nlk:json-value limit :any "limit_value"))))
         (reset (and limit (nlk:json-value limit :integer "retry_after_ms"))))
    (if (and type (string= (string-upcase type) "RPD"))
        (fail "GMI daily model limit for ~a: ~a.~@[ ~a~] Choose another model."
              (or model "this model") (or value "unspecified")
              (and reset (format nil "Resets in about ~d h ~d min."
                                 (floor reset 3600000)
                                 (floor (mod reset 3600000) 60000))))
        (fail "GMI rate limit for ~a. Wait before trying again. ~a"
              (or model "this model") (redact (error-message body))))))

(defun http-refusal (status body &optional model)
  "A failed answer as a refusal: the status, GMI's words, and the hint that
usually fixes it."
  (cond ((eql status 429) (rate-limit-refusal status body model))
        ((member status '(401 403) :test #'=)
         (fail "GMI rejected the credential (HTTP ~a): check the key in the variable key_env names, and its permissions~a"
               status (or (status-hint status) "")))
        (t
         (fail "GMI answered HTTP ~a: ~a~@[~a~]"
               status (redact (error-message body)) (status-hint status)))))

(defun call-api (method path &key body (retry nil) (model nil))
  "One request to the requestqueue API => the JSON object it answers. A 5xx
that RETRY allows is tried three times; every other failed answer is a refusal
naming the status."
  (let ((url (api-url path))
        (left (if retry 3 1)))
    (loop
      (multiple-value-bind (reply status)
          (dial method url :content (and body (nlk:encode-json-object body)))
        (cond ((and (integerp status) (<= 200 status 299))
               (let ((data (and (stringp reply)
                                (plusp (length (nlk:trimmed reply)))
                                (ignore-errors (nlk:decode-json reply)))))
                 (return (if (hash-table-p data) data (nlk:json-object)))))
              ((and (> left 1) (or (null status) (and (integerp status) (>= status 500))))
               (decf left)
               (sleep (if (integerp status) 1 0.5)))
              (t (http-refusal status reply model)))))))

;;; --- the catalog, as much of it as one call needs --------------------------------

(defun record-model (record)
  (nlk:json-value record :string "model"))

(defun public-model-id-p (id)
  "True for a model id a person may name: not blank, not a UUID, not one of the
original plugin's internal probe names (capture, probe, ssrf, test, hunt, bypass)."
  (and (stringp id)
       (plusp (length id))
       (not (find-if (lambda (word) (search word id :test #'char-equal))
                     '("capture" "probe" "ssrf" "test" "hunt" "bypass")))
       (not (and (= 36 (length id))
                 (every (lambda (c i) (if (member i '(8 13 18 23)) (char= c #\-) (digit-char-p c 16)))
                        id '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35))))))

(defun parameter-specs (record)
  "RECORD's declared parameters as an alist of (NAME . SPEC), in their order."
  (let ((parameters (nlk:json-value record :array "parameters"))
        (specs '()))
    (loop for spec across parameters
          when (hash-table-p spec)
            do (let ((name (nlk:json-value spec :string "name")))
                 (when name (push (cons name spec) specs))))
    (nreverse specs)))

(defun param-spec (specs name)
  (cdr (assoc name specs :test #'equal)))

(defun choose-param (specs names)
  (find-if (lambda (name) (param-spec specs name)) names))

(defun spec-options (spec)
  "SPEC's fixed values, in its own order: `options', each an object with a
`value' or the value itself, else `enum'."
  (let ((raw (or (nlk:json-value spec :array "options")
                 (nlk:json-value spec :array "enum"))))
    (loop for item across (or raw #())
          for value = (if (hash-table-p item) (nlk:json-value item :any "value") item)
          unless (eq value :null) collect value)))

(defun output-type-p (record type)
  (let ((outputs (nlk:json-value record :array "modalities" "output")))
    (some (lambda (item) (and (hash-table-p item) (equal type (nlk:json-value item :string "type"))))
          (coerce outputs 'list))))

(defun required-media (record)
  "The media types RECORD cannot run without, as a list of strings: its input
modalities marked required, and every required parameter that is a picture."
  (let ((types '()))
    (loop for item across (nlk:json-value record :array "modalities" "input")
          when (and (hash-table-p item)
                    (eq t (nlk:json-value item :boolean "required"))
                    (not (equal "text" (nlk:json-value item :string "type"))))
            do (pushnew (nlk:json-value item :string "type") types :test #'equal))
    (dolist (spec (parameter-specs record))
      (when (eq t (nlk:json-value (cdr spec) :boolean "required"))
        (let ((type (nlk:json-value (cdr spec) :string "type")))
          (cond ((and type (search "image" type)) (pushnew "image" types :test #'equal))
                ((member type '("audio" "video") :test #'string=) (pushnew type types :test #'equal))
                ((member (car spec) +image-names+ :test #'equal) (pushnew "image" types :test #'equal))))))
    types))

(defun accepts-image-input-p (record)
  (and (choose-param (parameter-specs record) +image-names+) t))

(defun usable-p (record type)
  "True for a model a call may pick: a public id, active, with workers behind
it, not hidden, and able to answer TYPE."
  (and (public-model-id-p (record-model record))
       (equal "active" (nlk:json-value record :string "status"))
       (let ((workers (nlk:json-value record :integer "qworkers")))
         (and workers (plusp workers)))
       (not (equal "hidden" (nlk:json-value record :string "model_metadata" "visibility")))
       (output-type-p record type)))

(defun fits-p (record need-image)
  "True when RECORD draws an image from exactly these inputs: nothing required
it cannot be given, and, for an edit, a parameter that takes the picture."
  (let ((required (required-media record)))
    (and (usable-p record "image")
         (if need-image
             (and (accepts-image-input-p record)
                  (subsetp required '("image") :test #'equal))
             (null required)))))

(defun model-of (record)
  (or (record-model record) "this model"))

(defun model-record (id)
  "RECORD for model ID, from the one-hour cache or read now."
  (let ((key (concatenate 'string (setting :api) " " id)))
    (let ((cached (gethash key *schemas*)))
      (when (and cached (< (- (get-universal-time) (car cached)) +catalog-ttl+))
        (return-from model-record (cdr cached)))
      (let* ((data (call-api :get (format nil "models/~a" (path-escape id))))
             (record (or (nlk:json-value data :object "data") data)))
        (unless (and (hash-table-p record) (record-model record))
          (fail "GMI returned no schema for model ~a: choose another model, or check the id in the console" id))
        (setf (gethash key *schemas*) (cons (get-universal-time) record))
        record))))

(defun probe-model (need-image)
  "The first of the cell's preferred models that fits these inputs, or NIL.
A credential or rate-limit refusal goes up; a missing model is stepped over."
  (dolist (id +preferences+)
    (let ((record (handler-case (model-record id)
                    (gmi-image-error (condition)
                      (let ((detail (gmi-image-error-detail condition)))
                        (if (or (search "credential" detail) (search "rate limit" detail)
                                (search "daily model limit" detail))
                            (error condition)
                            nil))))))
      (when (and record (fits-p record need-image))
        (return record)))))

(defun select-model (explicit need-image setting-model)
  "The schema of the model this call runs on."
  (cond
    (explicit
     (let ((record (model-record explicit)))
       (unless (fits-p record need-image)
         (fail "GMI model ~a does not accept ~:[these inputs~;a source image~]. Use ~a."
               explicit need-image
               (let ((suggestion (probe-model need-image)))
                 (if suggestion (model-of suggestion) "another model from the catalog"))))
       record))
    (setting-model
     (let ((record (model-record setting-model)))
       (if (fits-p record need-image)
           record
           (or (probe-model need-image)
               (fail "GMI model ~a does not accept ~:[these inputs~;a source image~], and no preferred model does: set model in the gmi_image section"
                     setting-model need-image)))))
    (t
     (or (probe-model need-image)
         (fail "no usable GMI image model answered: check the key, and set model in the gmi_image section to one the catalog lists")))))

;;; --- the payload, mapped onto the model's own schema -------------------------------

(defun number-of (value)
  "\"16:9\", \"1024x1024\", \"1:1\", \"-3\" or \"4.5\" as a real, else NIL."
  (let* ((text (nlk:trimmed (princ-to-string value)))
         (mark (position-if (lambda (c) (member c '(#\: #\x #\X))) text)))
    (cond
      ((and mark (plusp mark))
       (let ((left (ignore-errors (number-of (subseq text 0 mark))))
             (right (ignore-errors (number-of (subseq text (1+ mark))))))
         (and left right (plusp right) (/ left right))))
      (t
       (let ((*read-eval* nil))
         (ignore-errors
          (multiple-value-bind (number end) (read-from-string text)
            (and (realp number) (= end (length text)) number))))))))

(defun image-type (octets)
  "\"png\", \"jpg\", \"webp\" or \"gif\" by the first bytes, or NIL."
  (let ((size (length octets)))
    (flet ((at (start end) (map 'string #'code-char (subseq octets start end))))
      (cond ((and (>= size 8)
                  (= (aref octets 0) #x89) (= (aref octets 1) 80)
                  (= (aref octets 2) 78) (= (aref octets 3) 71))
             "png")
            ((and (>= size 3) (= (aref octets 0) #xFF) (= (aref octets 1) #xD8) (= (aref octets 2) #xFF))
             "jpg")
            ((and (>= size 12) (string= "RIFF" (at 0 4)) (string= "WEBP" (at 8 12)))
             "webp")
            ((and (>= size 6) (string= "GIF8" (at 0 4)))
             "gif")))))

(defun size-of (text)
  "The (W . H) a \"WxH\" option names, or NIL."
  (let ((mark (and (stringp text) (position #\x text :test #'char-equal))))
    (when mark
      (let ((w (ignore-errors (parse-integer text :end mark)))
            (h (ignore-errors (parse-integer text :start (1+ mark)))))
        (and w h (plusp w) (plusp h) (cons w h))))))

(defun size-pixels (text)
  (let ((pair (size-of text)))
    (and pair (* (car pair) (cdr pair)))))

(defun size-ratio (text)
  (let ((pair (size-of text)))
    (and pair (/ (car pair) (cdr pair)))))

(defun aspect-target (aspect)
  "The width:height proportion an aspect names, or NIL."
  (number-of (or (cdr (assoc aspect +aspects+ :test #'string=)) aspect)))

(defun nearest-size (aspect spec)
  "The size SPEC offers that is closest to ASPECT, the larger one when two
are as close; or the original plugin's default sizes for a free-form size."
  (let* ((choices (remove-if-not (lambda (value) (and (stringp value) (size-of value)))
                                  (spec-options spec)))
         (safe (remove-if (lambda (value) (> (size-pixels value) +max-pixels+)) choices))
         (choices (or safe (and (= 1 (length (remove-duplicates choices :test #'equal))) choices)))
         (target (aspect-target aspect)))
    (cond
      ((null choices) nil)
      ((null target) (fail "use a named aspect, or a width:height ratio: ~s" aspect))
      (t (first (sort (copy-list choices)
                       (lambda (a b)
                         (let ((da (abs (- (size-ratio a) target)))
                               (db (abs (- (size-ratio b) target))))
                           (if (/= da db) (< da db) (> (size-pixels a) (size-pixels b)))))))))))

(defun nearest-choice (name value choices)
  "CHOICE closest to VALUE by its number, as the original plugin snapped
durations and resolutions."
  (let ((target (number-of value)))
    (let ((numeric (loop for choice in choices
                         for number = (number-of choice)
                         when number collect (cons choice number))))
      (unless (and target numeric)
        (fail "invalid ~a value ~s. Valid values: ~{~a~^, ~}" name value
              (subseq choices 0 (min 10 (length choices)))))
      (car (first (sort numeric
                         (lambda (a b)
                           (let ((da (abs (- (cdr a) target)))
                                 (db (abs (- (cdr b) target))))
                             (if (/= da db) (< da db) (< (cdr a) (cdr b)))))))))))

(defun coerce-value (name value spec &key nearest)
  "VALUE as SPEC declares it: one of its options (snapped when NEAREST), a
number inside its bounds, a boolean, or the value itself."
  (let ((choices (spec-options spec)))
    (when choices
      (let ((match (find value choices
                         :test (lambda (a b) (and (stringp a) (stringp b) (string-equal a b))))))
        (return-from coerce-value
          (cond (match match)
                (nearest (nearest-choice name value choices))
                (t (fail "invalid ~a value ~s. Valid values: ~{~a~^, ~}"
                         name value (subseq choices 0 (min 10 (length choices)))))))))
    (let ((type (nlk:json-value spec :string "type")))
      (cond
        ((member type '("integer" "float") :test #'string=)
         (let ((number (if (numberp value) value (number-of value))))
           (unless number (fail "~a needs a number, not ~s" name value))
           (let ((minimum (nlk:json-value spec :number "min_value"))
                 (maximum (nlk:json-value spec :number "max_value")))
             (when (and minimum (< number minimum)) (setf number minimum))
             (when (and maximum (> number maximum)) (setf number maximum))
             (if (string= type "integer") (round number) number))))
        ((and (string= type "boolean")
              (not (member value '(t nil) :test #'eq)))
         (fail "~a needs true or false, not ~s" name value))
        (t value)))))

(defun image-value (urls spec)
  "The `image' member one call rides as: a public URL, or the URLs the model's
parameter accepts, capped by its own max_value."
  (let ((urls (coerce urls 'list)))
    (unless urls
      (fail "GMI takes at least one image URL"))
    (dolist (url urls)
      (unless (public-url-p url)
        (fail "GMI takes image inputs as public http(s) URLs; upload the file first or pass a URL, not ~s" url)))
    (let ((limit (max 1 (or (nlk:json-value spec :integer "max_value") 1))))
      (let ((urls (subseq urls 0 (min limit (length urls)))))
        (if (> limit 1) (coerce urls 'vector) (first urls))))))

(defun map-payload (record prompt aspect images)
  "The payload RECORD declares for one call => (values PAYLOAD DROPPED): every
input it takes, its required parameters filled from their defaults, and the
inputs the model has no place for named in DROPPED."
  (let ((specs (parameter-specs record))
        (payload (make-hash-table :test #'equal))
        (dropped '()))
    (labels ((put (concept name value &key nearest)
               (if name
                   (setf (gethash name payload)
                         (coerce-value name value (param-spec specs name) :nearest nearest))
                   (push concept dropped))))
      (put "prompt" (choose-param specs '("prompt" "text")) prompt)
      (let ((name (choose-param specs '("aspect_ratio" "aspectRatio")))
            (size-spec (param-spec specs "size")))
        (cond
          (name (put "aspect" name (or (cdr (assoc aspect +aspects+ :test #'string=)) aspect)))
          (size-spec
           (let ((size (nearest-size aspect size-spec)))
             (if size
                 (setf (gethash "size" payload) (coerce-value "size" size size-spec))
                 (push "aspect" dropped))))
          (t (push "aspect" dropped))))
      (when images
        (put "images" (choose-param specs +image-names+) (image-value images (param-spec specs (choose-param specs +image-names+))))))
    (dolist (spec specs)
      (destructuring-bind (name . spec) spec
        (when (and (eq t (nlk:json-value spec :boolean "required"))
                   (not (nth-value 1 (gethash name payload))))
          (let ((default (nlk:json-value spec :any "default_value")))
            (when (or (null default)
                      (and (member name +image-names+ :test #'equal) (not default)))
              (fail "GMI model ~a needs ~a, and neither the call nor its schema supplies one: supply it or choose another model"
                    (model-of record) name))
            (setf (gethash name payload) (coerce-value name default spec))))))
    (maphash (lambda (name value)
               (let ((spec (param-spec specs name)))
                 (unless spec
                   (fail "GMI model ~a does not declare ~a: use the model's own parameters" (model-of record) name))
                 (let ((choices (spec-options spec)))
                   (when (and choices (not (member value choices :test #'equal)))
                     (fail "invalid ~a value ~s. Valid values: ~{~a~^, ~}"
                           name value (subseq choices 0 (min 10 (length choices))))))))
             payload)
    (values payload (nreverse dropped))))

;;; --- the job: submit, then poll ---------------------------------------------------

(defun media-urls (data)
  "The picture URLs DATA's outcome carries, in the original plugin's order:
image_url, media_urls, then the other media, then a thumbnail."
  (let ((outcome (nlk:json-value data :object "outcome"))
        (urls '()))
    (flet ((add (value)
             (dolist (item (cond ((stringp value) (list value))
                                 ((vectorp value) (coerce value 'list))
                                 (t (list value))))
               (let ((url (if (hash-table-p item) (nlk:json-value item :string "url") item)))
                 (when (and (stringp url) (plusp (length url)) (not (member url urls :test #'equal)))
                   (setf urls (append urls (list url))))))))
      (dolist (key '("image_url" "media_urls" "image_urls" "video_url" "audio_url"))
        (let ((value (nlk:json-value outcome :any key)))
          (when (and value (not (eq value :null))) (add value))))
      (when (null urls)
        (let ((thumbnail (nlk:json-value outcome :string "thumbnail_image_url")))
          (when thumbnail (add thumbnail)))))
    urls))

(defun terminal (data id)
  "DATA when the job has finished, NIL while it is still running; a failed one
is a refusal carrying GMI's own reason."
  (let ((status (string-downcase (or (nlk:json-value data :string "status") ""))))
    (cond
      ((member status '("failed" "error" "cancelled") :test #'string=)
       (fail "GMI job ~a ~a: ~a: review the inputs before trying again"
             (or id "?")
             status
             (or (nlk:json-value data :string "error")
                 (nlk:json-value data :string "message")
                 "no detail")))
      ((member status '("success" "completed") :test #'string=)
       (unless (media-urls data)
         (fail "GMI job ~a returned no image URL: check the request in the GMI console" (or id "?")))
       data)
      (t nil))))

(defun run-job (model payload)
  "One GMI job => the finished answer. Submitted, then polled with a growing
delay until the section's timeout."
  (let* ((submitted (call-api :post "requests"
                               :body (nlk:json-object "model" model "payload" payload)
                               :model model))
         (id (or (nlk:json-value submitted :string "request_id")
                 (nlk:json-value submitted :string "id"))))
    (unless id
      (fail "GMI returned no request id: check the console before submitting again"))
    (or (terminal submitted id)
        (let ((deadline (+ (get-universal-time) (setting :timeout)))
              (delay 1))
          (loop
            (let ((data (call-api :get (format nil "requests/~a" (path-escape id))
                                  :retry t :model model)))
              (let ((done (terminal data id)))
                (when done (return done))))
            (when (>= (get-universal-time) deadline)
              (fail "GMI job ~a timed out after ~d s: check its status in the console before submitting again"
                    id (setting :timeout)))
            (sleep (max 1 (min 5 (round delay))))
            (setf delay (min 5 (* delay 1.5))))))))

(defun price-text (record)
  (let ((details (nlk:json-value record :any "pricing_details")))
    (cond ((stringp details) details)
          ((hash-table-p details) (nlk:encode-json-object details))
          (t (let ((info (nlk:json-value record :object "price_info")))
               (if (and info (nlk:json-value info :any "price"))
                   (format nil "~a micro-dollars / ~a" (nlk:json-value info :any "price")
                           (or (nlk:json-value info :string "unit") "unspecified unit"))
                   "Price unavailable"))))))

;;; --- the picture, written at once ------------------------------------------------

(defun big-endian (octets at bytes)
  (loop with value = 0
        for index from 0 below bytes
        do (setf value (+ (* value 256) (aref octets (+ at index))))
        finally (return value)))

(defun png-size (octets)
  (when (and (>= (length octets) 24)
             (string= "IHDR" (map 'string #'code-char (subseq octets 12 16))))
    (cons (big-endian octets 16 4) (big-endian octets 20 4))))

(defun jpeg-size (octets)
  "The width and height in OCTETS' first SOF segment, or NIL."
  (let ((index 2)
        (size (length octets)))
    (loop while (< (+ index 9) size)
          do (if (/= (aref octets index) #xFF)
                 (incf index)
                 (let ((marker (aref octets (1+ index))))
                   (cond ((or (= marker #xD8) (= marker #xFF)) (incf index 2))
                         ((<= #xD0 marker #xD9) (incf index 2))
                         ((and (>= marker #xC0) (<= marker #xCF)
                               (not (member marker '(#xC4 #xC8 #xCC))))
                          (return (cons (big-endian octets (+ index 7) 2)
                                        (big-endian octets (+ index 5) 2))))
                         (t (incf index (+ 2 (big-endian octets (+ index 2) 2))))))))))

(defun image-pixels (octets)
  "OCTETS' \"WxH\" when its header says, else NIL."
  (let* ((type (image-type octets))
         (pair (cond ((equal type "png") (png-size octets))
                     ((equal type "jpg") (jpeg-size octets)))))
    (and pair (format nil "~dx~d" (car pair) (cdr pair)))))

(defun save-octets (octets)
  "(values PATH BYTES PIXELS): OCTETS written under the cell's output_dir,
never over another file there."
  (let ((type (image-type octets)))
    (unless type
      (fail "GMI answered bytes that are not a PNG, JPEG, WebP or GIF image"))
    (let ((directory (setting :output-dir)))
      (ensure-directories-exist (merge-pathnames "keep" directory))
      (let ((file (loop for attempt from 0
                         for candidate = (merge-pathnames
                                          (format nil "gmi-~d~@[-~d~].~a" (get-universal-time)
                                                  (and (plusp attempt) attempt) type)
                                          directory)
                         unless (probe-file candidate)
                           return candidate)))
        (alexandria:write-byte-vector-into-file octets file :if-exists :supersede)
        (values (namestring file) (length octets) (image-pixels octets))))))

(defun save-image (url)
  "(values PATH BYTES PIXELS) of the picture URL names, fetched now because
GMI's link may expire."
  (multiple-value-bind (body status) (dial :get url :binary t :accept "*/*")
    (unless (and (integerp status) (< status 400))
      (fail "the image URL answered HTTP ~a" status))
    (unless (typep body '(vector (unsigned-byte 8)))
      (fail "the image URL answered no bytes"))
    (save-octets body)))

;;; --- one picture -----------------------------------------------------------------

(defun render (prompt &key (aspect "landscape") model images)
  "One image, drawn or edited => the answer object: the file, its size, the
model, the aspect and the ratio asked for, the pixel size, the request id and
the model's price."
  (let* ((prompt (nlk:trimmed (or prompt "")))
         (sources (remove-if-not #'nonblank (coerce (or images '()) 'list)))
         (aspect (string-downcase (or (nonblank aspect) "landscape")))
         (named (or (nonblank model) (nonblank (setting :model)))))
    (when (zerop (length prompt))
      (fail "a prompt is required and cannot be empty"))
    (unless (cdr (assoc aspect +aspects+ :test #'string=))
      (fail "aspect must be one of ~{~a~^, ~}: ~s" (mapcar #'car +aspects+) aspect))
    (when (> (length sources) +max-references+)
      (fail "~d source images, over the ~a one call sends" (length sources) +max-references+))
    (unless (nonblank (setting :api-key))
      (fail "no GMI key: put one in the environment variable ~a names, or name a file in key_file"
            (or (nonblank (setting :key-env)) +default-key-env+)))
    (let* ((record (select-model (nonblank model) (and sources t) (and (null (nonblank model)) named)))
           (id (model-of record))
           (started (get-universal-time)))
      (multiple-value-bind (payload dropped) (map-payload record prompt aspect sources)
        (let* ((result (run-job id payload))
               (url (first (media-urls result))))
          (unless url
            (fail "GMI answered no image: ~a" "the outcome carried no media URL"))
          (multiple-value-bind (path bytes pixels) (save-image url)
            (let ((answer (nlk:json-object
                           "image" path
                           "bytes" bytes
                           "model" id
                           "aspect" aspect
                           "ratio" (or (cdr (assoc aspect +aspects+ :test #'string=)) aspect)
                           "mode" (if sources "edit" "generate")
                           "price" (price-text record))))
              (when pixels (setf (gethash "pixels" answer) pixels))
              (when (> (get-universal-time) started)
                (setf (gethash "seconds" answer) (- (get-universal-time) started)))
              (let ((request-id (or (nlk:json-value result :string "request_id") nil)))
                (when request-id (setf (gethash "request_id" answer) request-id)))
              (when dropped
                (setf (gethash "dropped" answer) (coerce dropped 'vector)))
              answer)))))))

;;; --- the tool --------------------------------------------------------------------

(defun gmi-image-tool (request)
  "The gmi_image tool: one picture, or an edit when source URLs are named."
  (answering
    (nlk:encode-json-object
     (render (image-request-prompt request)
             :aspect (image-request-aspect request)
             :model (image-request-model request)
             :images (coerce (image-request-images request) 'list)))))

;;; --- /gmi-image -------------------------------------------------------------

(defun command-request (text)
  "(values PROMPT OPTIONS): the leading --flags out of TEXT, then the prompt.
--image may repeat; every other flag takes one value."
  (let ((words (nlk:split-words text))
        (options '())
        (images '()))
    (loop while (and words (uiop:string-prefix-p "--" (first words)))
          do (let ((flag (string-downcase (subseq (pop words) 2))))
               (cond ((member flag '("landscape" "portrait" "square") :test #'string=)
                      (setf (getf options :aspect) flag))
                     ((null words) nil)
                     (t
                      (let ((value (pop words)))
                        (if (string= flag "image")
                            (push value images)
                            (setf (getf options (intern (string-upcase flag) :keyword)) value)))))))
    (values (format nil "~{~a~^ ~}" words)
            (list* :images (nreverse images) options))))

(defun gmi-image-command (args session-id)
  "One /gmi-image invocation => the answer's text, and typed alone the dialog
every command answers then: one row that asks for the prompt."
  (declare (ignore session-id))
  (let ((line (nlk:trimmed (or args ""))))
    (if (zerop (length line))
        (values "Describe the picture, then /gmi-image with it."
                (nle:list-dialog
                 :title "GMI Image"
                 :summary "GMI Cloud's image models, through your key"
                 :rows (list (nle:ask-row "Generate an image" "Prompt" "/gmi-image"
                                           :placeholder "a blue boat on a calm lake, wide shot"
                                           :verb "generate"))))
        (multiple-value-bind (prompt options) (command-request line)
          (if (zerop (length prompt))
              (values "Nothing to draw: give /gmi-image a prompt." nil)
              (handler-case
                  (let ((answer (render prompt
                                        :aspect (or (getf options :aspect) "landscape")
                                        :model (getf options :model)
                                        :images (getf options :images))))
                    (values (format nil "~a~@[ · ~a~]~@[ · ~a~]"
                                    (nlk:json-value answer :string "image")
                                    (nlk:json-value answer :string "model")
                                    (nlk:json-value answer :string "price"))
                            nil))
                (gmi-image-error (condition)
                  (values (format nil "ERROR: ~a" (gmi-image-error-detail condition)) nil))
                (error (condition)
                  (values (format nil "ERROR: gmi-image: ~a"
                                  (nlk:one-line (princ-to-string condition)))
                          nil))))))))

(defun generate-image (prompt &key (aspect "landscape") model images)
  "One image from eval, the same way the tool asks for one => the answer object."
  (render prompt :aspect aspect :model model :images images))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell gmi-image
  (:section ("gmi_image")
    (:guide "your GMI Cloud key, in the environment variable the section names; a model is picked for you; next step: ask for a picture")
    ("api" :string :default "https://console.gmicloud.ai/api/v1/ie/requestqueue/apikey"
     :doc "GMI's requestqueue base: leave it as it is unless GMI moves it")
    ("key_env" :string :default "GMI_API_KEY"
     :doc "the environment variable holding the GMI key - never the key itself")
    ("key_file" :path
     :doc "a file holding the key, which the file's own permissions guard; set instead of key_env")
    ("model" :string :default ""
     :doc "a GMI model id for every call, e.g. seedream-5.0-lite; empty picks the first preferred model the key reaches")
    ("output_dir" :path :default "~/.nodecode/images"
     :doc "where generated pictures are written")
    ("timeout" :integer :default 300 :min 1
     :doc "how long one GMI job may take, in seconds: a picture is queued, submitted and polled"))
  (:settings #'read-settings)
  (:start (lambda ()
            (clrhash *schemas*)
            nil))
  (:help :gmi-image
         "gmi-image: gmi_image draws or edits a picture with GMI Cloud's image models, spending GMI credits"
         *manual*)
  (:tool "gmi_image" (nlk:contract 'image-request) #'gmi-image-tool)
  (:command "gmi-image" 'gmi-image-command
            :description "Draw or edit an image with GMI Cloud"
            :argument-hint "PROMPT"))
