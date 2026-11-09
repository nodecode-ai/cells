;;;; media.lisp --- the wire: one request, a job's poll, a download, four verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of EmpirioLabs-AI/hermes-empiriolabs/media (see NOTICE): its
;;;; three backends are four verbs of this cell, every one of them reachable
;;;; two ways — as the tool the model calls and as the Lisp function an eval
;;;; snippet calls.
;;;;
;;;; The wire is the original's. One POST to /v1/images/generations,
;;;; /v1/videos/generations or /v1/audio/speech; when the answer carries a
;;;; job id rather than an output, GET /v1/jobs/{id} is polled until it
;;;; completes or the wait runs out (a video's wait is the section's
;;;; video_timeout_seconds, a sync answer's is its own). The output is a
;;;; URL or inline base64; either way the file is fetched WITHOUT the key and
;;;; written under this home's media/empiriolabs/, and the answer names the
;;;; path (the signed URL when the copy could not be made).
;;;;
;;;; What is NOT the original's: the live catalog is no longer read to
;;;; negotiate each model's parameters. A call sends only what the caller
;;;; asked for — the prompt always, an aspect ratio, a duration, a resolution
;;;; or a source image when one was named — plus the release defaults the
;;;; section carries, so a model that does not declare a member is never sent
;;;; it. (models) still reads GET /v1/models for whoever wants the catalog.

(in-package #:nodecode-empiriolabs-media)

;;; --- the words and the bounds ------------------------------------------------

(defparameter +default-base-url+ "https://api.empiriolabs.ai/v1"
  "EmpirioLabs AI's API root, the original's DEFAULT_BASE_URL.")

(defparameter +api-keys-url+ "https://platform.empiriolabs.ai/dashboard/api-keys"
  "Where the operator makes a key; the original's setup link.")

(defparameter +user-agent+ "nodecode-empiriolabs-media/0.1.0"
  "What every request calls itself, as the original's does.")

(defparameter +runtimes+ '("processing" "queued" "pending" "running" "in_progress" "submitted" "starting")
  "The job statuses that mean keep waiting.")

(defparameter +finished+ '("completed" "succeeded")
  "The job statuses that mean the outputs are there.")

(defparameter +failed+ '("failed" "error" "cancelled" "canceled" "expired")
  "The job statuses that mean give up.")

(defparameter +aspect-ratios+ '(("square" . "1:1") ("landscape" . "16:9") ("portrait" . "9:16"))
  "The semantic ratio the caller names, and the one the original sends with no
catalog to ask: the first choice of its own table.")

(defparameter +max-media-bytes+ (* 200 1024 1024)
  "The largest generated file one download keeps.")

(defparameter +max-source-bytes+ (* 20 1024 1024)
  "The largest local source image the original inlines, 20 MB.")

(defparameter +detail-chars+ 300
  "How much of a failed answer's text a refusal repeats.")

(defparameter *poll-seconds* 3
  "Seconds between two job polls; a test binds 0 and never waits.")

(defparameter *serial* 0
  "One number per saved file, so two files made in one second never collide.")

(defparameter +folder+ "media/empiriolabs/"
  "Where generated files are written, under this home.")

;;; --- the section, read back --------------------------------------------------

(defun nonblank (value)
  "VALUE trimmed, when it is a non-blank string; else NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, plus the key every call authenticates with, read once
here: api_key written inline, else the variable api_key_env names, else
EMPIRIOLABS_API_KEY -- the order the original reads them in. NONE of the
three is required: a cell without a key starts, and its verbs say what to
do."
  (declare (ignore table))
  (let* ((env (nonblank (getf values :api-key-env)))
         (inline (nonblank (getf values :api-key)))
         (key (or inline
                  (and env (nonblank (uiop:getenv env)))
                  (nonblank (uiop:getenv "EMPIRIOLABS_API_KEY"))))
         (root (string-right-trim "/" (or (getf values :base-url) +default-base-url+)))
         (plist (copy-list values)))
    (when (and env (null key))
      (nlk:config-error "api_key_env names ~a, but nothing is set there: put an EmpirioLabs AI API key in it (~a), or clear api_key_env to set one later"
                         env +api-keys-url+))
    (unless (or (uiop:string-prefix-p "https://" root) (uiop:string-prefix-p "http://" root))
      (nlk:config-error "base_url ~s has no http:// or https:// scheme: set it to ~a, or to the proxy that answers for it"
                        root +default-base-url+))
    (setf (getf plist :key) key
          (getf plist :base-url) root)
    plist))

(defun base-url ()
  "The API root this cell calls."
  (setting :base-url))

(defun api-key ()
  "The key every API call carries, or NIL."
  (setting :key))

(defun need-key ()
  "The key, or a refusal that says where one comes from."
  (or (api-key)
      (fail "no EmpirioLabs AI API key: create one at ~a, then put it in EMPIRIOLABS_API_KEY or in this cell's api_key" +api-keys-url+)))

;;; --- one request -------------------------------------------------------------

(defun decode (body)
  "BODY, a JSON text, decoded; NIL when it is empty or unreadable."
  (when (and (stringp body) (plusp (length (nlk:trimmed body))))
    (or (ignore-errors (nlk:decode-json body)) nil)))

(defun error-message (body status)
  "The API's own words for a failed call, else an HTTP line, clipped."
  (let* ((json (decode body))
         (error (and (hash-table-p json) (nlk:json-value json :object "error")))
         (text (or (and error (nlk:json-value error :string "message"))
                   (and (hash-table-p json) (nlk:json-value json :string "message"))
                   (nonblank body)
                   (format nil "HTTP ~a" status))))
    (nlk:clip (nlk:trimmed text) +detail-chars+)))

(defun headers (key &key json)
  "The request headers: what the cell is, JSON in and out when JSON, and the
key when there is one."
  (let ((headers (list (cons "accept" "application/json")
                       (cons "user-agent" +user-agent+))))
    (when json (push (cons "content-type" "application/json") headers))
    (when key (push (cons "authorization" (format nil "Bearer ~a" key)) headers))
    headers))

(defun request-json (method url &key payload key (timeout 60))
  "METHOD URL with PAYLOAD as its JSON, KEY as its bearer, TIMEOUT seconds
bounding it => the decoded answer, or a refusal saying what the API said."
  (let ((content (and payload (nlk:encode-json-object payload))))
    (multiple-value-bind (body status)
        (handler-case (nlk:http method url :headers (headers key :json (and content t))
                                      :content content :timeout timeout)
          (error (e) (fail "could not reach the EmpirioLabs AI API at ~a: ~a" url e)))
      (cond ((>= status 400) (fail "EmpirioLabs AI answered HTTP ~a: ~a" status (error-message body status)))
            ((decode body))
            (t (fail "EmpirioLabs AI answered no JSON to ~a ~a" method url))))))

(defun job-url (id)
  "Where a job's state is read."
  (format nil "~a/jobs/~a" (base-url) id))

(defun job-status (job)
  (string-downcase (or (nlk:json-value job :string "status") "")))

(defun job-error (job status)
  "The job's own words for its failure."
  (or (let ((error (nlk:json-value job :object "error")))
        (and (hash-table-p error) (nlk:json-value error :string "message")))
      (format nil "the job ended with status ~a" status)))

(defun wait-for-job (id key seconds)
  "Poll GET /v1/jobs/ID until it succeeds => the job; refuse when it failed
or SECONDS ran out (it keeps running; the refusal names where to read it)."
  (let ((deadline (+ (get-universal-time) (max 0 seconds))))
    (loop
      for job = (request-json :get (job-url id) :key key :timeout (setting :timeout-seconds))
      for status = (job-status job)
      do (cond ((member status +finished+ :test #'equal) (return job))
               ((member status +failed+ :test #'equal)
                (fail "the EmpirioLabs AI job ~a failed: ~a" id (job-error job status)))
               ((not (member status +runtimes+ :test #'equal))
                (fail "the EmpirioLabs AI job ~a returned an unexpected status ~s" id status))
               ((>= (get-universal-time) deadline)
                (fail "the EmpirioLabs AI job ~a is still running after ~d seconds; it keeps running, and GET ~a tells you when it is done"
                      id seconds (job-url id))))
      do (sleep *poll-seconds*))))

;;; --- what an answer carries ---------------------------------------------------

(defun output-of (payload)
  "The first output in PAYLOAD's `data' or `result.data' => (values URL B64 ITEM)."
  (dolist (container (list payload (nlk:json-value payload :object "result")))
    (when (hash-table-p container)
      (loop for item across (or (nlk:json-value container :array "data") #())
            when (hash-table-p item)
              do (let ((url (nlk:json-value item :string "url"))
                       (b64 (nlk:json-value item :string "b64_json")))
                   (when (or url b64)
                     (return-from output-of (values url b64 item)))))))
  (values nil nil nil))

(defun resolve-output (payload key seconds)
  "PAYLOAD's output, following a job envelope when it is one =>
(values URL B64 ITEM JOB-ID)."
  (multiple-value-bind (url b64 item) (output-of payload)
    (let ((id (nonblank (nlk:json-value payload :string "job_id"))))
      (cond ((or url b64) (values url b64 item id))
            ((null id) (fail "EmpirioLabs AI answered no media"))
            (t (let ((job (wait-for-job id key seconds)))
                 (multiple-value-bind (url b64 item) (output-of job)
                   (unless (or url b64)
                     (fail "the EmpirioLabs AI job ~a completed without an output file" id))
                   (values url b64 item id))))))))

(defun download (url)
  "URL, fetched without the key: the original sends none to a signed URL.
=> the octets."
  (multiple-value-bind (body status)
      (handler-case (nlk:http :get url :headers (headers nil) :binary t
                                    :max-bytes +max-media-bytes+
                                    :timeout (setting :timeout-seconds))
        (error (e) (fail "could not download ~a: ~a" url e)))
    (cond ((>= status 400) (fail "downloading ~a answered HTTP ~a" url status))
          (body)
          (t (fail "downloading ~a answered no data" url)))))

;;; --- what a file is -----------------------------------------------------------

(defun ascii-at (octets text &optional (start 0))
  "Whether OCTETS carry TEXT from START."
  (and (>= (length octets) (+ start (length text)))
       (loop for i below (length text)
             always (= (aref octets (+ start i)) (char-code (char text i))))))

(defun image-kind (octets)
  "The extension an image's first octets name, or NIL."
  (cond ((and (>= (length octets) 8)
              (equalp (subseq octets 0 8) #(137 80 78 71 13 10 26 10))) "png")
        ((and (>= (length octets) 3) (= 255 (aref octets 0)) (= 216 (aref octets 1))) "jpg")
        ((or (ascii-at octets "GIF87a") (ascii-at octets "GIF89a")) "gif")
        ((and (ascii-at octets "RIFF") (ascii-at octets "WEBP" 8)) "webp")))

(defun audio-kind (octets)
  "The extension an audio file's first octets name; mp3 when unknown, which
is the one format the original asks for by default."
  (cond ((and (ascii-at octets "RIFF") (ascii-at octets "WAVE" 8)) "wav")
        ((ascii-at octets "OggS") "ogg")
        ((ascii-at octets "fLaC") "flac")
        ((ascii-at octets "ID3") "mp3")
        (t "mp3")))

(defun url-extension (url &optional (default "mp4"))
  "The extension URL's path ends in, when it is one of the media ones."
  (let* ((path (car (last (uiop:split-string url :separator "/"))))
         (dot (position #\. path :from-end t))
         (ext (and dot (string-downcase (subseq path (1+ dot))))))
    (if (member ext '("mp4" "webm" "mov" "png" "jpg" "jpeg" "webp" "gif" "mp3" "wav" "ogg" "flac" "m4a")
                :test #'equal)
        ext
        default)))

(defun without-data-prefix (text)
  "TEXT with a `data:...;base64,' prefix dropped."
  (let ((comma (search "," text)))
    (if (and comma (uiop:string-prefix-p "data:" text)) (subseq text (1+ comma)) text)))

(defun safe-prefix (text)
  "TEXT as a file name: letters, digits, . _ and - only, at most 60."
  (let ((name (substitute #\- #\/ (or text "media"))))
    (subseq (remove-if-not (lambda (c) (or (alphanumericp c) (find c "._-"))) name)
            0 (min 60 (length name)))))

(defun media-path (extension prefix)
  "A fresh path under this home's media folder."
  (nlk:home (format nil "~a~a-~a-~d.~a" +folder+ (safe-prefix prefix)
                    (get-universal-time) (incf *serial*) (or extension "png"))))

(defun save-bytes (octets extension prefix)
  "OCTETS written as a file under the media folder => its path."
  (let ((path (media-path extension prefix)))
    (nlk:write-file-atomically path octets)
    (namestring path)))

(defun save-media (url b64 extension-of prefix)
  "The media the answer carried, written under this home => (values PATH SAVED-P);
with nothing to save, URL and NIL."
  (let ((octets (cond (b64 (ignore-errors
                             (cl-base64:base64-string-to-usb8-array (without-data-prefix b64))))
                      (url (handler-case (download url)
                             (empiriolabs-media-error () nil))))))
    (if octets
        (values (save-bytes octets (funcall extension-of octets) prefix) t)
        (values url nil))))

;;; --- a source image, inlined --------------------------------------------------

(defun source-for-api (source)
  "SOURCE as the API takes it: an http(s) URL or a data:image URI as it is; a
local image file read once and inlined as a data URI (the one local file the
original reads)."
  (let ((value (nlk:trimmed (if (stringp source) source (princ-to-string source)))))
    (cond ((zerop (length value)) "")
          ((or (uiop:string-prefix-p "https://" (string-downcase value))
               (uiop:string-prefix-p "http://" (string-downcase value))
               (uiop:string-prefix-p "data:image/" (string-downcase value)))
           value)
          (t (let ((path (ignore-errors (probe-file value))))
               (unless path
                 (fail "~s is not an http(s) URL, a data:image URI or an existing local image file" value))
               (let ((size (with-open-file (in path :element-type '(unsigned-byte 8))
                             (file-length in))))
                 (when (> size +max-source-bytes+)
                   (fail "~a is larger than ~d MB" (file-namestring path)
                         (floor +max-source-bytes+ (* 1024 1024))))
                 (let ((octets (with-open-file (in path :element-type '(unsigned-byte 8))
                                 (let ((octets (make-array size :element-type '(unsigned-byte 8))))
                                   (read-sequence octets in)
                                   octets))))
                   (let ((kind (image-kind octets)))
                     (unless kind
                       (fail "~a is not a PNG, JPEG, WebP or GIF image" (file-namestring path)))
                     (format nil "data:image/~a;base64,~a" kind
                             (cl-base64:usb8-array-to-base64-string octets))))))))))

;;; --- the verbs ----------------------------------------------------------------

(define-verb image (prompt &key aspect source model)
  "Generate one image from PROMPT, or edit one, through EmpirioLabs AI =>
the file it saved. ASPECT is square, landscape or portrait; SOURCE is an
http(s) URL, a data:image URI or a local PNG, JPEG, WebP or GIF to edit or
reference. MODEL defaults to the section's image_model."
  (let ((key (need-key))
        (text (nlk:trimmed (if (stringp prompt) prompt (princ-to-string prompt)))))
    (when (zerop (length text)) (fail "an image needs a prompt"))
    (let* ((model (or (nonblank model) (setting :image-model)))
           (payload (nlk:json-object "model" model "prompt" text
                                      "num_images" 1 "sync" t
                                      "aspect_ratio"
                                      (or (cdr (assoc (string-downcase (or (nonblank aspect) "square"))
                                                       +aspect-ratios+ :test #'equal))
                                          "1:1")))
           (inline (and (nonblank source) (source-for-api source))))
      (when (and inline (plusp (length inline)))
        (setf (gethash "image" payload) (vector inline)))
      (let* ((answer (request-json :post (format nil "~a/images/generations" (base-url))
                                    :payload payload :key key :timeout (setting :timeout-seconds))))
        (multiple-value-bind (url b64 item id) (resolve-output answer key (setting :timeout-seconds))
          (declare (ignore item))
          (multiple-value-bind (saved made) (save-media url b64 #'image-kind (safe-prefix model))
            (format nil "image (~a~@[, job ~a~]): ~:[~a (the download failed, so this is the signed URL)~;saved at ~a~]~@[~%edited from ~a~]"
                    model id made saved
                    (and inline (if (uiop:string-prefix-p "data:" inline) "a local image" inline)))))))))

(define-verb video (prompt &key source aspect duration resolution model)
  "Generate one video from PROMPT, or animate an image, through EmpirioLabs
AI => the file it saved. SOURCE is an http(s) URL, a data:image URI or a
local image file. ASPECT, DURATION (seconds) and RESOLUTION are sent only
when named, so a model that does not offer them is never sent one. MODEL
defaults to the section's video_model."
  (let ((key (need-key))
        (text (nlk:trimmed (if (stringp prompt) prompt (princ-to-string prompt)))))
    (when (zerop (length text)) (fail "a video needs a prompt"))
    (let* ((model (or (nonblank model) (setting :video-model)))
           (payload (nlk:json-object "model" model "prompt" text))
           (inline (and (nonblank source) (source-for-api source))))
      (when (and inline (plusp (length inline)))
        (setf (gethash "image" payload) inline))
      (when (nonblank aspect) (setf (gethash "aspect_ratio" payload) aspect))
      (when (nonblank resolution) (setf (gethash "resolution" payload) resolution))
      (when (and (integerp duration) (plusp duration))
        (setf (gethash "duration" payload) duration))
      (let* ((answer (request-json :post (format nil "~a/videos/generations" (base-url))
                                    :payload payload :key key :timeout (setting :timeout-seconds))))
        (multiple-value-bind (url b64 item id) (resolve-output answer key (setting :video-timeout-seconds))
          (declare (ignore b64 item))
          (multiple-value-bind (saved made) (save-media url nil (lambda (octets) (declare (ignore octets))
                                                                      (url-extension url))
                                                         (safe-prefix model))
            (format nil "video (~a~@[, job ~a~]): ~:[~a (the download failed, so this is the signed URL)~;saved at ~a~@[ (~ds)~]~]"
                    model id made saved (and (integerp duration) (plusp duration) duration))))))))

(define-verb speech (text &key voice model)
  "Speak TEXT through EmpirioLabs AI => the audio file it saved. VOICE
defaults to the section's voice, and to the model's own when that is unset;
MODEL defaults to the section's tts_model."
  (let ((key (need-key))
        (words (nlk:trimmed (if (stringp text) text (princ-to-string text)))))
    (when (zerop (length words)) (fail "speech needs text"))
    (let* ((model (or (nonblank model) (setting :tts-model)))
           (voice (or (nonblank voice) (nonblank (setting :voice))))
           (payload (nlk:json-object "model" model "input" words)))
      (when voice (setf (gethash "voice" payload) voice))
      (let* ((answer (request-json :post (format nil "~a/audio/speech" (base-url))
                                    :payload payload :key key :timeout (setting :timeout-seconds))))
        (multiple-value-bind (url b64 item id) (resolve-output answer key (setting :timeout-seconds))
          (declare (ignore item id))
          (multiple-value-bind (saved made) (save-media url b64 #'audio-kind "speech")
            (if made
                (format nil "speech (~a~@[, voice ~a~]) saved at ~a" model voice saved)
                (format nil "speech (~a): ~a (the download failed, so this is the signed URL)" model saved))))))))

(define-verb models (&key kind)
  "The live EmpirioLabs AI model catalog => one line a model. KIND is
image, video or speech to keep only the models that serve that endpoint."
  (let* ((answer (request-json :get (format nil "~a/models" (base-url))
                                :key (need-key) :timeout (setting :timeout-seconds)))
         (rows (nlk:json-value answer :array "data"))
         (wanted (cdr (assoc (string-downcase (or (nonblank kind) ""))
                              '(("image" . "POST /v1/images/generations")
                                ("video" . "POST /v1/videos/generations")
                                ("speech" . "POST /v1/audio/speech"))
                              :test #'equal)))
         (lines '()))
    (loop for row across (or rows #())
          when (hash-table-p row)
            do (let ((id (nonblank (nlk:json-value row :string "id")))
                     (endpoints (nlk:json-value row :array "supported_endpoints")))
                 (when (and id
                            (or (null wanted)
                                (member wanted (coerce (or endpoints #()) 'list) :test #'equal)))
                   (push (format nil "~a~@[ (~a)~]~:[~; [featured]~]"
                                 id (nonblank (nlk:json-value row :string "category"))
                                 (nlk:json-value row :boolean "is_featured"))
                         lines))))
    (if lines
        (format nil "~d EmpirioLabs AI model~:p~@[ for ~a~]:~%~{~a~%~}"
                (length lines) (nonblank kind) (reverse lines))
        (fail "the EmpirioLabs AI catalog lists no model~@[ for ~a~]" (nonblank kind)))))
