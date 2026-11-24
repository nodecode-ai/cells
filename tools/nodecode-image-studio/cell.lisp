;;;; cell.lisp --- draw with FAL or Vertex AI, keep every picture, list the gallery.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of phdigital01's imagegen-hermes (see NOTICE). The original is a
;;;; Hermes plugin: a desktop studio, an image_gen provider registration and a
;;;; `hermes imagegen` CLI over a Python engine that speaks to FAL's queue,
;;;; Magnific and Vertex AI. What survives here is the part that needs no
;;;; desktop: two model-facing tools and one command, over the two wires whose
;;;; contract is public and small - FAL's queue (submit, poll, fetch) and
;;;; Vertex AI's generateContent - with every picture written under one folder
;;;; and listed in one gallery.
;;;;
;;;; The section is the config; the keys are environment variables or files it
;;;; names, never values written in the folder. Every request is one NLK:HTTP
;;;; exchange, so a test stubs one name and nothing is drawn.

(in-package #:nodecode-image-studio)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Images are available through the nodecode-image-studio cell: the generate_image tool, the
image_gallery tool, and the /image-studio command.
  generate_image  {prompt, provider, model, aspect_ratio, resolution, count, references}
      One request to FAL (the queue: submit, poll, fetch) or to Vertex AI
      (generateContent), and every picture written under the cell's output_dir. The answer
      names each file and its size in bytes. provider is fal or vertex, and each needs its
      own key; blank uses the section's provider. aspect_ratio is width:height - 1:1, 4:5,
      3:2, 16:9, 9:16, 21:9 - and resolution (1K, 2K, 4K) is for Vertex AI's Gemini image
      models. references draws from pictures the user already has: https URLs on both, local
      paths too on Vertex AI. Say where each file is; do not paste its bytes.
  image_gallery   {limit}
      The pictures this cell drew, newest first: path, prompt, provider, model and when.
Type /image-studio with a prompt for the same call from the operator's side; typed alone it
asks for the prompt and lists what is in the gallery. Write the prompt as an image model
reads it - subject, setting, style, light, framing - not as a question. A keyless or failed
call says which provider, the HTTP status and the provider's own message, never the key."
  "What (help :image-studio) answers while the cell runs.")

;;; --- the two wires --------------------------------------------------------------

(defparameter +providers+ '("fal" "vertex")
  "The providers this cell speaks to, in the order the section names them.")

(defparameter +fal-root+ "https://queue.fal.run"
  "FAL's queue: one POST submits, the status URL answers, the response URL holds the result.")

(defparameter +vertex-root+ "https://aiplatform.googleapis.com/v1"
  "Vertex AI's generative API root; the model path is appended to it.")

(defparameter +fal-default-model+ "fal-ai/flux-2-pro"
  "The model a FAL call draws with when neither the call nor the section names one.")

(defparameter +vertex-default-model+ "gemini-nano-banana-2.1"
  "The model a Vertex AI call draws with when neither the call nor the section names one.")

(defparameter +max-count+ 4
  "The most pictures one call draws.")

(defparameter *poll-seconds* 2.5
  "How long between two looks at a FAL job; a test binds it to zero.")

(defparameter +poll-rounds+ 120
  "How many looks a FAL job gets before the call gives up.")

(defparameter +max-image-bytes+ (* 40 1024 1024)
  "The most one picture may be, in bytes, when it is read or fetched.")

(defparameter +error-chars+ 300
  "How much of the provider's own error text a refusal quotes back.")

(defparameter +user-agent+ "nodecode-image-studio/0.1.0"
  "What the cell calls itself to the provider. Nothing of another client's is impersonated.")

;;; --- the section, read back -----------------------------------------------------

(defparameter +blank+ '(#\Space #\Tab #\Newline #\Return)
  "What a wire string is trimmed of before it is read.")

(defun nonblank (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (string-trim +blank+ value)))
       (string-trim +blank+ value)))

(defun resolve-key (env file label)
  "The key the section names: the environment variable ENV, or the file FILE,
read once here. Neither is set => NIL, which a call refuses by name."
  (let ((env (nonblank env))
        (file (nonblank file)))
    (when (and env file)
      (nlk:config-error "set one of ~a and the file member, not both" label))
    (let ((key (cond (env (nonblank (nle::credential-env env)))
                     (file (or (nonblank (ignore-errors
                                          (uiop:read-file-string (nlk:expand-home file))))
                               (nlk:config-error "the file ~a names cannot be read, or is empty" label)))
                     (t nil))))
      (when (and key (find-if (lambda (character) (find character +blank+)) key))
        (nlk:config-error "the key ~a names holds whitespace or a line break" label))
      key)))

(defun read-settings (values table)
  "The declared VALUES, plus the two keys and the folder pictures are written
to: the environment and the filesystem are read once, here."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :fal-key) (resolve-key (getf values :fal-key-env)
                                              (getf values :fal-key-file) "fal_key_env")
          (getf plist :vertex-key) (resolve-key (getf values :vertex-key-env)
                                                 (getf values :vertex-key-file) "vertex_key_env")
          (getf plist :output-dir)
          (uiop:ensure-directory-pathname
           (nlk:expand-home (or (nonblank (getf values :output-dir)) "~/.nodecode/image-studio"))))
    plist))

(defun provider-key (provider)
  "The key PROVIDER is called with, or NIL."
  (cond ((string= provider "fal") (setting :fal-key))
        ((string= provider "vertex") (setting :vertex-key))
        (t nil)))

(defun provider-default-model (provider)
  "The model PROVIDER draws with when nothing names one."
  (if (string= provider "vertex") +vertex-default-model+ +fal-default-model+))

;;; --- one exchange ---------------------------------------------------------------

(defun exchange (method url &key content headers binary)
  "One exchange with the provider => (values BODY STATUS), BODY its text or
octets. A dial that fails is a refusal, never an unhandled error. A key rides
in HEADERS and nowhere else: never in a URL, never in a log line."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http method url
                    :headers (append (list (cons "user-agent" +user-agent+)
                                           (cons "accept" (if binary "*/*" "application/json")))
                                     headers)
                    :content content
                    :binary binary
                    :timeout (setting :timeout)
                    :connect-timeout (min 15 (setting :timeout)))
        (values body status))
    (error (condition)
      (fail "~(~a~) ~a did not answer: ~a" method url
            (nlk:one-line (princ-to-string condition))))))

(defun error-message (body)
  "The provider's own words for a failed answer, clipped, or NIL."
  (let* ((text (and (stringp body) body))
         (object (and text (plusp (length (nlk:trimmed text)))
                      (ignore-errors (nlk:decode-json text))))
         (inner (and (hash-table-p object) (nlk:json-value object :object "error")))
         (said (or (and (hash-table-p inner) (nlk:json-value inner :string "message"))
                   (and (hash-table-p object) (nlk:json-value object :string "message"))
                   (and (hash-table-p inner) (nlk:json-value inner :string "status"))
                   (and text (plusp (length (nlk:trimmed text)))
                        (nlk:clip (nlk:one-line text) +error-chars+)))))
    (and said (nlk:clip said +error-chars+))))

(defun status-hint (status)
  "The parenthetical hint the model can act on, or NIL."
  (case status
    ((401 403) " (check the key the section names, and what it is allowed to spend)")
    (404 " (check the model id is one this provider serves)")
    (422 " (check the settings this model takes, such as its aspect ratio or resolution)")
    (429 " (the provider is rate limiting: retry in a moment)")
    (t nil)))

(defun http-refusal (provider status body)
  "A failed answer as a refusal: who answered, the status, the provider's own
words and the hint that usually fixes it."
  (fail "~a answered HTTP ~a~@[: ~a~]~@[~a~]"
        provider status (error-message body) (status-hint status)))

;;; --- pictures on their way in and out --------------------------------------------

(defun remote-p (reference)
  "Whether REFERENCE is a URL rather than a local path."
  (let ((text (string-downcase (or reference ""))))
    (or (uiop:string-prefix-p "https://" text)
        (uiop:string-prefix-p "http://" text))))

(defun octets-prefixed-p (octets prefix)
  "Whether OCTETS open with PREFIX, a string read as bytes."
  (and (>= (length octets) (length prefix))
       (loop for index from 0 below (length prefix)
             always (= (aref octets index) (char-code (char prefix index))))))

(defun image-type (octets)
  "OCTETS' format from its first bytes - png, jpg or webp - or NIL."
  (cond ((octets-prefixed-p octets (format nil "~cPNG~c~c~c~c"
                                            (code-char 137) (code-char 13) (code-char 10)
                                            (code-char 26) (code-char 10)))
         "png")
        ((and (>= (length octets) 3)
              (= (aref octets 0) #xff) (= (aref octets 1) #xd8) (= (aref octets 2) #xff))
         "jpg")
        ((and (octets-prefixed-p octets "RIFF")
              (>= (length octets) 12)
              (octets-prefixed-p (subseq octets 8) "WEBP"))
         "webp")
        (t nil)))

(defun mime-of (type)
  (cond ((equal type "png") "image/png")
        ((equal type "jpg") "image/jpeg")
        ((equal type "webp") "image/webp")
        (t "application/octet-stream")))

(defun read-octets (path)
  "PATH's bytes, refused past the cap one picture may be."
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let* ((declared (file-length in))
           (size (if declared (min (1+ +max-image-bytes+) declared) (1+ +max-image-bytes+)))
           (octets (make-array size :element-type '(unsigned-byte 8))))
      (let ((read (read-sequence octets in)))
        (if (< read size) (subseq octets 0 read) octets)))))

(defun data-uri (text)
  "TEXT, a data: URI => (values MIME OCTETS)."
  (let ((comma (position #\, text)))
    (unless (and (uiop:string-prefix-p "data:" text) comma)
      (fail "the provider answered bytes it did not label as a data URI"))
    (let* ((meta (subseq text 5 comma))
           (semicolon (position #\; meta))
           (mime (if semicolon (subseq meta 0 semicolon) meta)))
      (values (if (plusp (length mime)) mime "image/png")
              (handler-case (cl-base64:base64-string-to-usb8-array (subseq text (1+ comma)))
                (error () (fail "the provider answered a data URI whose base64 does not decode")))))))

(defun fetch-octets (url what)
  "URL's bytes, refused unless it answers below 400 with at most the cap."
  (multiple-value-bind (body status) (exchange :get url :binary t)
    (unless (and (integerp status) (< status 400))
      (fail "~a ~a answered HTTP ~a" what url status))
    (unless (typep body '(vector (unsigned-byte 8)))
      (fail "~a ~a answered no bytes" what url))
    (when (> (length body) +max-image-bytes+)
      (fail "~a ~a answered ~:d bytes, over the ~:d one picture may be"
            what url (length body) +max-image-bytes+))
    body))

(defun local-octets (path what)
  "PATH's bytes, refused unless the file exists and holds an image."
  (let* ((pathname (nlk:expand-home path))
         (octets (and (probe-file pathname) (ignore-errors (read-octets pathname)))))
    (unless octets
      (fail "~a ~a does not exist, or cannot be read" what path))
    (when (> (length octets) +max-image-bytes+)
      (fail "~a ~a is over the ~:d bytes one picture may be" what path +max-image-bytes+))
    octets))

(defun answer-octets (source)
  "SOURCE, an answer's picture - an https URL, a data URI or raw bytes - as octets."
  (cond ((typep source '(vector (unsigned-byte 8))) source)
        ((and (stringp source) (uiop:string-prefix-p "data:" source))
         (nth-value 1 (data-uri source)))
        ((and (stringp source) (uiop:string-prefix-p "https://" source))
         (fetch-octets source "the picture"))
        ((stringp source)
         (fail "the provider answered the picture URL ~a, which is not https" source))
        (t (fail "the provider answered a picture of no readable shape"))))

(defun save-octets (octets)
  "OCTETS written under output_dir => (values PATH BYTES FORMAT)."
  (let ((directory (setting :output-dir)))
    (ensure-directories-exist (merge-pathnames "keep" directory))
    (let ((type (or (image-type octets) "png")))
      (loop for attempt from 0
            for file = (merge-pathnames
                        (format nil "image-~d~@[-~d~].~a" (get-universal-time)
                                (and (plusp attempt) attempt) type)
                        directory)
            unless (probe-file file)
              do (alexandria:write-byte-vector-into-file octets file :if-exists :supersede)
                 (return (values (namestring file) (length octets) type))))))

;;; --- FAL: one queue, then the result --------------------------------------------

(defun fal-prompt-settings (prompt aspect-ratio count references)
  "The JSON body one FAL model is asked for."
  (let ((payload (nlk:json-object "prompt" prompt "aspect_ratio" aspect-ratio
                                  "num_images" count)))
    (when references
      (setf (gethash "image_url" payload) (first references))
      (when (rest references)
        (setf (gethash "reference_image_urls" payload) (coerce (rest references) 'vector))))
    payload))

(defun fal-headers (key)
  (list (cons "authorization" (format nil "Key ~a" key))
        (cons "content-type" "application/json")))

(defun queue-url (url)
  "URL, refused unless it is FAL's own queue host: an answer never redirects the
key to another host."
  (unless (uiop:string-prefix-p "https://queue.fal.run/" url)
    (fail "FAL named a queue URL outside queue.fal.run: ~a" url))
  url)

(defun fal-submit (model payload key)
  "One POST to the queue => the queued job object, or the result itself."
  (multiple-value-bind (body status)
      (exchange :post (format nil "~a/~a" +fal-root+ model)
                :content (nlk:encode-json-object payload)
                :headers (fal-headers key))
    (unless (and (integerp status) (< status 400))
      (http-refusal "FAL" status body))
    (let ((object (ignore-errors (nlk:decode-json body))))
      (unless (hash-table-p object)
        (fail "FAL answered no JSON for ~a" model))
      object)))

(defun fal-result (object)
  "The finished job's result, or a refusal naming what FAL said."
  (let ((state (nlk:json-value object :string "status")))
    (cond ((or (null state) (string= state "COMPLETED")) object)
          ((member state '("FAILED" "ERROR") :test #'string=)
           (fail "FAL's job ~a~@[: ~a~]"
                 (string-downcase state)
                 (or (nlk:json-value object :string "error") (error-message nil))))
          (t (fail "FAL's job answered ~a, not a result" state)))))

(defun fal-application (model)
  "MODEL's queue application: its first two path segments."
  (format nil "~{~a~^/~}"
          (subseq (uiop:split-string model :separator "/") 0
                  (min 2 (length (uiop:split-string model :separator "/"))))))

(defun fal-finished (response key)
  "The result the response URL holds."
  (multiple-value-bind (body status)
      (exchange :get response :headers (list (cons "authorization" (format nil "Key ~a" key))))
    (unless (and (integerp status) (< status 400))
      (http-refusal "FAL" status body))
    (let ((result (ignore-errors (nlk:decode-json body))))
      (unless (hash-table-p result)
        (fail "FAL answered a result that is not JSON"))
      (fal-result result))))

(defun fal-wait (model queued key)
  "The result of the job QUEUED names: its status looked at until it completes."
  (let ((request-id (or (nlk:json-value queued :string "request_id")
                        (nlk:json-value queued :string "requestId"))))
    (if (null request-id)
        (fal-result queued)
        (let* ((response (queue-url (or (nlk:json-value queued :string "response_url")
                                        (format nil "~a/~a/requests/~a" +fal-root+
                                                (fal-application model) request-id))))
               (status-url (queue-url (or (nlk:json-value queued :string "status_url")
                                           (format nil "~a/status" response))))
               (state ""))
          (dotimes (look +poll-rounds+)
            (multiple-value-bind (body status)
                (exchange :get status-url :headers (list (cons "authorization"
                                                                (format nil "Key ~a" key))))
              (unless (and (integerp status) (< status 400))
                (http-refusal "FAL" status body))
              (let ((object (ignore-errors (nlk:decode-json body))))
                (unless (hash-table-p object)
                  (fail "FAL's status answer was not JSON"))
                (setf state (or (nlk:json-value object :string "status") ""))
                (cond ((string= state "COMPLETED")
                       (return-from fal-wait (fal-finished response key)))
                      ((member state '("FAILED" "ERROR") :test #'string=)
                       (fail "FAL's job ~a~@[: ~a~]" (string-downcase state)
                             (nlk:json-value object :string "error"))))))
            (sleep *poll-seconds*))
          (fail "FAL's job ~a answered ~s through ~a looks"
                request-id state +poll-rounds+)))))

(defun fal-sources (result)
  "The picture sources a FAL result holds, in its own order."
  (let ((sources '()))
    (labels ((take-one (value)
               (cond ((stringp value) (push value sources))
                     ((hash-table-p value)
                      (let ((url (nlk:json-value value :string "url")))
                        (when url (push url sources)))))))
      (let ((images (gethash "images" result)))
        (cond ((vectorp images) (loop for image across images do (take-one image)))
              (images (take-one images))))
      (when (null sources)
        (take-one (gethash "image" result))
        (take-one (gethash "url" result)))
      (nreverse sources))))

(defun fal-images (model prompt aspect-ratio count references key)
  "One generation on FAL => the pictures' sources."
  (let ((sources (fal-sources (fal-wait model (fal-submit model
                                                           (fal-prompt-settings prompt aspect-ratio count references)
                                                           key)
                                         key))))
    (unless sources
      (fail "FAL answered no picture for ~a" model))
    sources))

;;; --- Vertex AI: one generateContent ----------------------------------------------

(defparameter +vertex-ratios+ '("1:1" "3:2" "2:3" "3:4" "4:3" "4:5" "5:4" "9:16" "16:9" "21:9")
  "The aspect ratios Vertex AI's Gemini image models take.")

(defun vertex-endpoint (model)
  "The generateContent URL for MODEL, on the section's project when it names one."
  (let ((project (nonblank (setting :vertex-project))))
    (format nil "~a~@[/projects/~a/locations/global~]/publishers/google/models/~a:generateContent"
            +vertex-root+ project model)))

(defun vertex-part-for (reference)
  "REFERENCE - a URL or a local path - as an inlineData part."
  (let* ((octets (if (remote-p reference)
                     (fetch-octets reference "the reference image")
                     (local-octets reference "the reference image")))
         (type (or (image-type octets)
                   (fail "the reference image ~a is not a PNG, JPEG or WebP picture" reference))))
    (nlk:json-object "inlineData"
                      (nlk:json-object "mimeType" (mime-of type)
                                       "data" (cl-base64:usb8-array-to-base64-string octets)))))

(defun vertex-body (prompt aspect-ratio resolution references)
  "The generateContent body: the references, the prompt, and what the picture takes."
  (let ((parts (mapcar #'vertex-part-for references)))
    (setf parts (append parts (list (nlk:json-object "text" prompt))))
    (let ((image-config (nlk:json-object "imageSize" resolution)))
      (when (member aspect-ratio +vertex-ratios+ :test #'string=)
        (setf (gethash "aspectRatio" image-config) aspect-ratio))
      (nlk:json-object "contents"
                       (vector (nlk:json-object "role" "user"
                                                "parts" (coerce parts 'vector)))
                       "generationConfig"
                       (nlk:json-object "responseModalities" (vector "IMAGE")
                                        "imageConfig" image-config)))))

(defun vertex-sources (object)
  "The data URIs a generateContent answer holds, in its own order."
  (let ((sources '()))
    (dolist (candidate (coerce (or (nlk:json-value object :array "candidates") #()) 'list))
      (let ((content (nlk:json-value candidate :object "content")))
        (dolist (part (coerce (or (and content (nlk:json-value content :array "parts")) #()) 'list))
          (let ((inline (nlk:json-value part :object "inlineData")))
            (when inline
              (let ((data (nlk:json-value inline :string "data"))
                    (mime (or (nlk:json-value inline :string "mimeType") "image/png")))
                (when data
                  (push (format nil "data:~a;base64,~a" mime data) sources))))))))
    (nreverse sources)))

(defun vertex-images (model prompt aspect-ratio resolution references key)
  "One generation on Vertex AI => the pictures, as data URIs."
  (multiple-value-bind (body status)
      (exchange :post (vertex-endpoint model)
                :content (nlk:encode-json-object
                          (vertex-body prompt aspect-ratio resolution references))
                :headers (list (cons "x-goog-api-key" key)
                               (cons "content-type" "application/json")))
    (unless (and (integerp status) (< status 400))
      (http-refusal "Vertex AI" status body))
    (let* ((object (ignore-errors (nlk:decode-json body)))
           (sources (and (hash-table-p object) (vertex-sources object))))
      (unless sources
        (fail "Vertex AI answered no picture for ~a~@[ (finish reason ~a)~]"
              model (and (hash-table-p object)
                         (let ((first (and (plusp (length (or (nlk:json-value object :array "candidates") #())))
                                           (aref (nlk:json-value object :array "candidates") 0))))
                           (and first (nlk:json-value first :string "finishReason"))))))
      sources)))

;;; --- the gallery -----------------------------------------------------------------

(defun gallery-path ()
  "The gallery's index, in the folder the pictures are written to."
  (merge-pathnames "gallery.json" (setting :output-dir)))

(defun gallery-entries ()
  "The gallery's entries, newest first, each a JSON object."
  (let ((path (gallery-path)))
    (if (probe-file path)
        (let ((object (ignore-errors (nlk:decode-json (uiop:read-file-string path)))))
          (if (vectorp object) (coerce object 'list) '()))
        '())))

(defun record-gallery (entry)
  "ENTRY at the front of the gallery, which keeps the newest gallery_cap of them.
The pictures past the cap stay where they are: only the index is trimmed."
  (let* ((kept (cons entry (gallery-entries)))
         (limit (setting :gallery-cap))
         (kept (if (> (length kept) limit) (subseq kept 0 limit) kept)))
    (ensure-directories-exist (merge-pathnames "keep" (setting :output-dir)))
    (nlk:write-file-atomically (gallery-path)
                               (nlk:encode-json-object (coerce kept 'vector))
                               :mode #o600 :directory-mode #o700)
    entry))

(defun gallery (&optional (limit 10))
  "The newest LIMIT entries, as the answer object both the tool and the command read."
  (let* ((entries (gallery-entries))
         (kept (subseq entries 0 (min (max 1 limit) (length entries)))))
    (nlk:json-object "images" (coerce kept 'vector) "total" (length entries))))

(defun gallery-lines (&optional (limit 5))
  "The newest LIMIT pictures as lines a person reads, or a line saying there are none."
  (let ((entries (subseq (gallery-entries) 0 (min limit (length (gallery-entries))))))
    (if (null entries)
        "No pictures yet."
        (format nil "~d in the gallery; the newest:~{~%  ~a - ~a~}"
                (length (gallery-entries))
                (loop for entry in entries
                      collect (nlk:json-value entry :string "path")
                      collect (nlk:clip (or (nlk:json-value entry :string "prompt") "") 60))))))

;;; --- one generation, both wires ---------------------------------------------------

(defun generate-image (prompt &key provider model (aspect-ratio "1:1") (resolution "1K")
                                       (count 1) references)
  "One generation => the answer object: every file written, its size, the
provider and the model that drew it. A refusal names what to fix."
  (let* ((prompt (nlk:trimmed (or prompt "")))
         (provider (or (nonblank provider) (setting :provider)))
         (key (provider-key provider)))
    (when (zerop (length prompt))
      (fail "a prompt is required and cannot be empty"))
    (unless (member provider +providers+ :test #'string=)
      (fail "provider must be ~{~a~^ or ~}: ~s" +providers+ provider))
    (unless key
      (fail "~a needs a key: name the environment variable or file holding it in the ~a member of the image-studio section"
            provider (if (string= provider "fal") "fal_key_env" "vertex_key_env")))
    (let* ((model (or (nonblank model) (nonblank (setting :model))
                      (provider-default-model provider)))
           (count (min +max-count+ (max 1 (or count 1))))
           (references (remove-if-not #'nonblank references))
           (sources (if (string= provider "vertex")
                        (vertex-images model prompt aspect-ratio resolution references key)
                        (let ((urls (remove-if-not #'remote-p references)))
                          (when (< (length urls) (length references))
                            (fail "FAL draws from reference pictures by URL: name them as https URLs"))
                          (fal-images model prompt aspect-ratio count urls key))))
           (written '()))
      (dolist (source sources)
        (multiple-value-bind (path bytes type) (save-octets (answer-octets source))
          (let ((entry (nlk:json-object "path" path "prompt" prompt "provider" provider
                                         "model" model "aspect_ratio" aspect-ratio
                                         "bytes" bytes "format" type "at" (nlk:iso-now))))
            (record-gallery entry)
            (push entry written))))
      (nlk:json-object "images" (coerce (nreverse written) 'vector)
                       "provider" provider "model" model "prompt" prompt))))

;;; --- the tools ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     (image-studio-error (condition)
       (nle:failure (format nil "ERROR: image-studio: ~a" (image-studio-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: image-studio: ~a"
                            (nlk:one-line (princ-to-string condition)))))))

(defun generate-tool (request)
  "The generate_image tool: one call, every picture it drew."
  (answering
    (nlk:encode-json-object
     (generate-image (generation-request-prompt request)
                     :provider (generation-request-provider request)
                     :model (generation-request-model request)
                     :aspect-ratio (generation-request-aspect-ratio request)
                     :resolution (generation-request-resolution request)
                     :count (generation-request-image-count request)
                     :references (coerce (generation-request-references request) 'list)))))

(defun gallery-tool (request)
  "The image_gallery tool: what this cell drew, newest first."
  (answering
    (nlk:encode-json-object (gallery (gallery-request-limit request)))))

;;; --- /image-studio ----------------------------------------------------------------

(defun command-request (text)
  "(values PROMPT OPTIONS): the --flags out of TEXT wherever they stand, and
what is left as the prompt. --ref may repeat; --count takes a number; every
other flag takes one value."
  (let ((words (nlk:split-words text))
        (options '())
        (references '())
        (prompt '()))
    (loop while words
          do (let ((word (pop words)))
               (if (uiop:string-prefix-p "--" word)
                   (let ((flag (string-downcase (subseq word 2))))
                     (cond ((null words) nil)
                           ((string= flag "ref") (push (pop words) references))
                           (t (let ((value (pop words)))
                                (if (string= flag "count")
                                    (setf (getf options :count)
                                          (or (ignore-errors (parse-integer value)) 1))
                                    (setf (getf options (intern (string-upcase flag) :keyword))
                                          value))))))
                   (push word prompt))))
    (values (format nil "~{~a~^ ~}" (nreverse prompt))
            (list* :references (nreverse references) options))))

(defun image-studio-command (args session-id)
  "One /image-studio invocation => the answer's text, and typed alone the
dialog every command answers then: a row that asks for the prompt."
  (declare (ignore session-id))
  (let ((line (nlk:trimmed (or args ""))))
    (if (zerop (length line))
        (values (gallery-lines)
                (nle:list-dialog
                 :title "Image Studio"
                 :summary (format nil "~a, with your own key" (setting :provider))
                 :rows (list (nle:ask-row "Generate an image" "Prompt" "/image-studio"
                                           :placeholder "a red fox in snow, wide shot"
                                           :verb "generate"))))
        (multiple-value-bind (prompt options) (command-request line)
          (if (zerop (length prompt))
              (values "Nothing to draw: give /image-studio a prompt." nil)
              (handler-case
                  (let* ((answer (generate-image prompt
                                                 :provider (getf options :provider)
                                                 :model (getf options :model)
                                                 :aspect-ratio (or (getf options :aspect) "1:1")
                                                 :count (or (getf options :count) 1)
                                                 :references (getf options :references)))
                         (images (nlk:json-value answer :array "images")))
                    (values (format nil "~{~a~^~% ~}"
                                    (loop for image across images
                                          collect (nlk:json-value image :string "path")))
                            nil))
                (image-studio-error (condition)
                  (values (format nil "ERROR: ~a" (image-studio-error-detail condition)) nil))
                (error (condition)
                  (values (format nil "ERROR: image-studio: ~a"
                                  (nlk:one-line (princ-to-string condition)))
                          nil))))))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell image-studio
  (:section ("image-studio")
    (:guide "one key per provider, named by the environment variable or file that holds it; edge: run /image-studio with a prompt")
    ("provider" :choice :options '("fal" "vertex") :default "fal"
     :doc "the provider a call draws with when it names none; each has its own key")
    ("fal_key_env" :env :default "FAL_KEY"
     :doc "the environment variable holding the FAL key; set this or fal_key_file to draw with FAL")
    ("fal_key_file" :path
     :doc "a file holding the FAL key, which the file's own permissions guard; set instead of fal_key_env")
    ("vertex_key_env" :env :default "VERTEX_API_KEY"
     :doc "the environment variable holding the Google Cloud API key; set this or vertex_key_file to draw with Vertex AI")
    ("vertex_key_file" :path
     :doc "a file holding the Vertex AI key; set instead of vertex_key_env")
    ("vertex_project" :string :default ""
     :doc "the Google Cloud project id, when the key is bound to a service account; leave blank for an express-mode key")
    ("model" :string :default ""
     :doc "the model id both tools draw with, when a call names none; blank uses fal-ai/flux-2-pro on FAL and gemini-nano-banana-2.1 on Vertex AI")
    ("output_dir" :path :default "~/.nodecode/image-studio"
     :doc "the folder every picture is written to, with the gallery index beside them")
    ("gallery_cap" :integer :default 200 :min 1
     :doc "how many pictures the gallery lists; the ones past it stay on disk")
    ("timeout" :integer :default 300 :min 10
     :doc "how long one provider request may take, in seconds; a big render is slow"))
  (:settings #'read-settings)
  (:help :image-studio
         "image-studio: generate_image draws through FAL or Vertex AI, image_gallery lists what was drawn, /image-studio"
         *manual*)
  (:tool "generate_image" (nlk:contract 'generation-request) #'generate-tool)
  (:tool "image_gallery" (nlk:contract 'gallery-request) #'gallery-tool)
  (:command "image-studio" 'image-studio-command
            :description "Draw a picture through FAL or Vertex AI"
            :argument-hint "PROMPT"))
