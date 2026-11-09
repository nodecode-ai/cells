;;;; cell.lisp --- image, video, audio, task and lookup tools over Ace Data
;;;; Cloud's media API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One API root the operator names, in the `acedatacloud-media' section of
;;;; the shared config:
;;;;   "acedatacloud-media": {"api_key_env": "ACEDATACLOUD_API_KEY"}
;;;; Every service is one path on that root and one row in *SERVICES*:
;;;;   POST {api}/nano-banana/images   {"prompt": ..., "aspect_ratio": ...}
;;;;     -> {"task_id": "..."}                     (the jobs that render)
;;;;   POST {api}/nano-banana/tasks    {"action": "retrieve", "id": "..."}
;;;;     -> {"finished_at": 123,
;;;;         "response": {"success": true, "data": [{"image_url": "..."}]}}
;;;; A generate call submits; this cell then asks for the task until it is
;;;; terminal or the call's wait_seconds are gone, and answers the URLs or the
;;;; task id. Nothing hooks a provider lane: Ace Data Cloud speaks HTTP, and
;;;; NLK:HTTP is the one seam.
;;;;
;;;; Rewritten from AceDataCloud's Hermes Agent plugin (see NOTICE): the
;;;; services, their paths, defaults and the fields each takes, the submit and
;;;; bounded retrieve, and the statement that a call spends credits without an
;;;; approval prompt. The Hermes toolset registry, the per-service
;;;; enable/disable switch and the image_generate provider lane are not here:
;;;; a Nodecode cell declares its tools, its command and its section itself.

(in-package #:nodecode-acedatacloud-media)

;;; --- the manual -------------------------------------------------------------

(defparameter *manual*
  "Pictures, videos, music and speech come from the nodecode-acedatacloud-media cell: the
acedatacloud_image, acedatacloud_video, acedatacloud_audio, acedatacloud_task and
acedatacloud_lookup tools, which a person can also call from eval in the
NODECODE-ACEDATACLOUD-MEDIA package, and the /media command.
  acedatacloud_image  {prompt, service, model, size, resolution, count, image_urls, wait_seconds}
      Nano Banana, GPT Image, Seedream, FLUX, Qwen Image or Midjourney. Nano Banana edits the
      https reference images in image_urls when they are given.
  acedatacloud_video  {prompt, service, model, ratio, resolution, duration, image, wait_seconds}
      Seedance, HappyHorse, Veo, Kling, MiniMax, Wan or Grok Imagine; image is the first frame
      to animate, on Veo, Seedance and MiniMax.
  acedatacloud_audio  {prompt, service, model, title, style, lyric, custom, instrumental, duration, wait_seconds}
      Suno writes a song from the description, or from title, style and lyric with custom;
      Fish Audio says the text.
  acedatacloud_task   {service, task_id, wait_seconds}
      A render that outlived its call: the same task id, the same service name. It never
      submits a new job.
  acedatacloud_lookup {mode, query, image_url, number, page, country, language}
      web or news for Google results with links and snippets, face for landmark analysis.
  /media [--video|--audio|--lookup] [--service NAME] [--model ID] [--size S] [--wait N|--no-wait] PROMPT
      The same from the operator's side of the keyboard. Typed alone it asks for the prompt.
Every call is paid: it spends Ace Data Cloud credits on the operator's ACEDATACLOUD_API_KEY
straight away, with no approval prompt, and a costly option (a pro model, 4K, four pictures)
spends more. Prompts and reference URLs go to api.acedata.cloud. A call made with nothing to
spend is refused before the request, so it costs nothing. A call that ran out of its
wait_seconds answers the task id: hand it to acedatacloud_task, which is also free."

  "What (help :acedatacloud-media) answers while the cell runs.")

;;; --- what the API takes ------------------------------------------------------

(defparameter +default-api+ "https://api.acedata.cloud"
  "Ace Data Cloud's API root: the one host every service here answers on.")

(defparameter +user-agent+ "nodecode-acedatacloud-media/0.1.0"
  "What the cell calls itself to Ace Data Cloud. Nothing of another client is
impersonated, and no other client's headers ride along.")

(defparameter +error-chars+ 300
  "How much of Ace Data Cloud's own error text a refusal quotes back.")

(defparameter +max-wait-seconds+ 60
  "The longest a call waits for a render: the original plugin's retrieve bound,
so a tool call never sits on a video for minutes.")

(defparameter +media-keys+ '("image_url" "image_urls" "audio_url" "video_url"
                             "file_url" "output_url" "raw_image_url" "url")
  "The keys a finished answer names its media under, wherever they sit in it.")

(defparameter +result-keys+ '("id" "task_id" "state" "status" "success" "error"
                              "code" "message" "title" "duration" "model"
                              "finished_at" "image_url" "image_urls" "audio_url"
                              "video_url" "file_url" "url" "data" "organic" "news"
                              "items" "lyric" "image_width" "image_height")
  "The fields of an answer worth keeping: the original plugin's allowlist, so a
whole provider object never lands in the transcript.")

(defparameter +done-states+ '("complete" "completed" "succeed" "succeeded"
                              "success" "finished" "done")
  "The state words that mean a job has rendered.")

(defparameter +failed-states+ '("failed" "fail" "error" "cancelled" "canceled" "rejected")
  "The state words that mean a job will not render.")

;;; --- answers, and the one condition ------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (acedatacloud-media-error (condition)
       (nle:failure (format nil "ERROR: ~a" (acedatacloud-media-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: acedatacloud-media: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

;;; --- reading the section --------------------------------------------------------

(defun url-parts (url)
  "(values SCHEME HOST) of an absolute URL, both lowercased, or NILs."
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
                          (subseq hostport 0 (1+ (or (position #\] hostport)
                                                      (1- (length hostport))))))
                         (t (subseq hostport 0 (or (position #\: hostport)
                                                    (length hostport)))))))
        (values scheme (string-downcase host))))))

(defun local-host-p (host)
  "True for the hosts a plain-HTTP API root may live at: loopback, and a name
only a local network resolves."
  (let ((host (string-downcase (or host ""))))
    (or (member host '("localhost" "localhost.localdomain" "::1" "[::1]") :test #'string=)
        (uiop:string-prefix-p "127." host)
        (uiop:string-prefix-p "[::1]" host)
        (some (lambda (suffix) (uiop:string-suffix-p host suffix))
              '(".local" ".lan" ".internal" ".home.arpa")))))

(defun check-api (url)
  "URL, without its trailing slash, or a CONFIG-REFUSAL saying what is wrong."
  (let ((url (nlk:trimmed (or url ""))))
    (when (zerop (length url))
      (nlk:config-error "acedatacloud-media.api is empty: it is ~a" +default-api+))
    (multiple-value-bind (scheme host) (url-parts url)
      (unless scheme
        (nlk:config-error "acedatacloud-media.api is ~s; give the whole address, such as ~a"
                          url +default-api+))
      (unless (member scheme '("http" "https") :test #'string=)
        (nlk:config-error "acedatacloud-media.api scheme ~s is not http or https" scheme))
      (when (and (string= scheme "http") (not (local-host-p host)))
        (nlk:config-error "acedatacloud-media.api is ~s over plain http, so the key would cross the public internet in clear text: use https://" url))
      (string-right-trim "/" url))))

(defun read-settings (values table)
  "The declared VALUES, with the API root checked."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :api) (check-api (or (getf values :api) +default-api+)))
    plist))

;;; --- the key --------------------------------------------------------------------

(defun api-key ()
  "The Ace Data Cloud key: the section's own, else the variable api_key_env names."
  (or (nonblank (setting :api-key))
      (let ((name (nonblank (setting :api-key-env))))
        (and name (nonblank (nle::credential-env name))))))

(defun require-key ()
  "The key a call is billed to, refused before anything is sent when there is
none."
  (or (api-key)
      (fail "no Ace Data Cloud key: set the variable ~a, or paste a key into the acedatacloud-media section's api_key. Nothing was sent and nothing was billed."
            (or (nonblank (setting :api-key-env)) "ACEDATACLOUD_API_KEY"))))

(defun replace-text (text part replacement)
  "TEXT with every occurrence of PART replaced, so a key the service echoes
back is never shown."
  (if (or (zerop (length part)) (null (search part text)))
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
  "TEXT with the configured key replaced: a provider echoes a bad key back in
its message."
  (let ((key (api-key)))
    (if (and key (plusp (length key)))
        (replace-text text key "[key]")
        text)))

;;; --- the wire --------------------------------------------------------------------

(defun api-url (path)
  (concatenate 'string (setting :api) path))

(defun exchange (method url &key content headers)
  "One exchange with Ace Data Cloud => (values BODY STATUS). The key rides as
a bearer token and nowhere else: never in a URL, never in a log line."
  (let ((key (require-key)))
    (handler-case
        (nlk:http method url
                  :headers (append (list (cons "user-agent" +user-agent+)
                                         (cons "accept" "application/json")
                                         (cons "authorization" (format nil "Bearer ~a" key)))
                                   (when content
                                     (list (cons "content-type" "application/json")))
                                   headers)
                  :content content
                  :timeout (setting :request-timeout)
                  :connect-timeout (min 15 (setting :request-timeout)))
      (error (condition)
        (fail "~(~a~) ~a did not answer: ~a" method url
              (nlk:one-line (princ-to-string condition)))))))

(defun decode-body (body)
  "BODY, an HTTP body, as decoded JSON; a body that is not JSON is kept as the
text it was, so the model can read what Ace Data Cloud said."
  (cond ((hash-table-p body) body)
        ((stringp body)
         (let ((text (nlk:trimmed body)))
           (cond ((zerop (length text)) (nlk:json-object))
                 (t (handler-case (nlk:decode-json text)
                      (error () text))))))
        (t (nlk:json-object))))

(defun error-message (body)
  "Ace Data Cloud's own words for a failed answer, or what it answered."
  (let* ((object (decode-body body))
         (said (or (and (hash-table-p object)
                        (or (nlk:json-value object :string "message")
                            (nlk:json-value object :string "detail")))
                   (and (stringp body) (nonblank body)
                        (nlk:clip (nlk:one-line body) +error-chars+)))))
    (or said "no error message")))

(defun status-hint (status)
  "The parenthetical hint the model can act on, or NIL."
  (case status
    ((401 403) " (check the key: api_key in the acedatacloud-media section, or the variable api_key_env names)")
    (402 " (the Ace Data Cloud account is out of credits)")
    (404 " (check api in the acedatacloud-media section: https://api.acedata.cloud)")
    (429 " (rate limited: ask again in a moment)")
    (t nil)))

(defun http-refusal (status body)
  "A failed answer as a refusal: the status, Ace Data Cloud's words, and the
hint that usually fixes it."
  (fail "Ace Data Cloud answered HTTP ~a: ~a~@[~a~]"
        status (redact (error-message body)) (status-hint status)))

(defun ok-status-p (status)
  (and (integerp status) (< status 400)))

;;; --- reading an answer ----------------------------------------------------------

(defun nested-pairs (value keys)
  "Every (KEY . VALUE) of KEYS anywhere in VALUE, a decoded JSON value.
Arrays and nested objects are walked, so a media URL under response.data is
found where the service put it."
  (let ((found '()))
    (labels ((walk (node)
               (cond ((hash-table-p node)
                      (maphash (lambda (key item)
                                 (cond ((member key keys :test #'string=)
                                        (push (cons key item) found))
                                       ((or (hash-table-p item)
                                            (and (vectorp item) (not (stringp item))))
                                        (walk item))))
                               node))
                     ((and (vectorp node) (not (stringp node)))
                      (map nil #'walk node)))))
      (walk value))
    (nreverse found)))

(defun https-url-p (text)
  (and (stringp text) (>= (length text) 8) (string-equal "https://" text :end2 8)))

(defun media-urls (object)
  "Every https media URL the answer names, oldest first and once each."
  (let ((urls '()))
    (dolist (pair (nested-pairs object +media-keys+))
      (let ((value (cdr pair)))
        (cond ((https-url-p value) (pushnew value urls :test #'string=))
              ((and (vectorp value) (not (stringp value)))
               (dolist (item (coerce value 'list))
                 (when (https-url-p item) (pushnew item urls :test #'string=)))))))
    (nreverse urls)))

(defun state-words (object)
  "The state and status words the answer carries, lowercased."
  (loop for (nil . value) in (nested-pairs object '("state" "status"))
        when (stringp value) collect (string-downcase value)))

(defun failure-p (object)
  "True when the answer says the job failed: an error, or success false."
  (or (some (lambda (pair) (let ((value (cdr pair))) (and value (not (eq value :null)))))
            (nested-pairs object '("error")))
      (some (lambda (pair) (null (cdr pair))) (nested-pairs object '("success")))))

(defun unfinished-p (object)
  "True for a task the service has accepted but not finished: finished_at
present and still null."
  (and (hash-table-p object)
       (multiple-value-bind (value present) (gethash "finished_at" object)
         (and present (or (null value) (eq value :null))))))

(defun state-of (object &key synchronous)
  "What the answer says about the job: :succeeded, :failed or :pending.
SYNCHRONOUS is true for the services that answer their own result instead of
a task id."
  (let* ((words (state-words object))
         (failed (or (failure-p object)
                     (some (lambda (word) (member word +failed-states+ :test #'string=))
                           words))))
    (cond (failed :failed)
          ((unfinished-p object) :pending)
          ((and words (every (lambda (word) (member word +done-states+ :test #'string=))
                             words))
           :succeeded)
          (words :pending)
          (synchronous :succeeded)
          ((media-urls object) :succeeded)
          (t :pending))))

(defun state-name (state)
  (case state (:succeeded "succeeded") (:failed "failed") (t "pending")))

(defun unwrap (object)
  "The task's own result when the service wrapped it in a response member."
  (let ((response (and (hash-table-p object) (gethash "response" object))))
    (if (hash-table-p response) response (or object (nlk:json-object)))))

(defun public-data (value)
  "VALUE with only the fields worth keeping, the original plugin's allowlist."
  (cond ((hash-table-p value)
         (let ((out (nlk:json-object)))
           (maphash (lambda (key item)
                      (when (member key +result-keys+ :test #'string=)
                        (setf (gethash key out) (public-data item))))
                    value)
           out))
        ((and (vectorp value) (not (stringp value)))
         (map 'vector #'public-data value))
        (t value)))

(defun error-text (object)
  "The words the answer gives for its own failure, or NIL."
  (let ((pairs (nested-pairs object '("error" "message"))))
    (or (loop for (nil . value) in pairs
              when (and (stringp value) (nonblank value))
                return (nlk:clip (nlk:one-line value) +error-chars+))
        (loop for (nil . value) in pairs
              when (and value (not (eq value :null)) (not (stringp value)))
                return "the service reported a failure"))))

(defun task-id-of (object)
  "The task id an accepted job answers with, or NIL."
  (let ((id (and (hash-table-p object) (nlk:json-value object :string "task_id"))))
    (and (stringp id) (nonblank id))))

(defun media-answer (row object task-id)
  "The answer the model reads: the job's state, the service, the media URLs and
the fields worth keeping."
  (let* ((state (state-of object :synchronous (null task-id)))
         (answer (nlk:json-object "status" (state-name state)
                                   "service" (getf row :name))))
    (when task-id
      (setf (gethash "task_id" answer) task-id))
    (let ((urls (media-urls object)))
      (when urls
        (setf (gethash "urls" answer) (coerce urls 'vector))))
    (case state
      (:succeeded
       (let ((data (public-data (unwrap object))))
         (when (and (hash-table-p data) (plusp (hash-table-count data)))
           (setf (gethash "data" answer) data))))
      (:pending
       (setf (gethash "next_action" answer)
             (if task-id
                 (format nil "still running: call acedatacloud_task with service ~s and this task_id to finish it"
                         (getf row :name))
                 "still running: ask again in a moment")))
      (:failed
       (setf (gethash "error" answer)
             (or (error-text object) "the service reported a failure"))))
    answer))

;;; --- the services ---------------------------------------------------------------

(defun media-row (name kind path task &key fields defaults content-p model-header)
  "One service: its name, its kind, its two paths, and the fields a call may set."
  (list :name name :kind kind :path path :task task :fields fields
        :defaults defaults :content-p content-p :model-header model-header))

(defparameter *services*
  (list
   ;; --- pictures
   (media-row "nano-banana" :image "/nano-banana/images" "/nano-banana/tasks"
              :fields '(("prompt" . "prompt") ("model" . "model") ("size" . "aspect_ratio")
                        ("resolution" . "resolution") ("count" . "count")
                        ("image_urls" . "image_urls"))
              :defaults '(("model" . "nano-banana-2-lite") ("action" . "generate")
                          ("aspect_ratio" . "1:1") ("resolution" . "1K") ("count" . 1)))
   (media-row "gpt-image" :image "/openai/images/generations" "/openai/tasks"
              :fields '(("prompt" . "prompt") ("model" . "model") ("size" . "size")
                        ("count" . "n"))
              :defaults '(("model" . "gpt-image-2") ("size" . "1024x1024")
                          ("quality" . "auto") ("n" . 1) ("response_format" . "url")))
   (media-row "seedream" :image "/seedream/images" "/seedream/tasks"
              :fields '(("prompt" . "prompt") ("model" . "model") ("size" . "size"))
              :defaults '(("model" . "doubao-seedream-5-0-lite-260128")
                          ("size" . "2K") ("response_format" . "url")))
   (media-row "flux" :image "/flux/images" "/flux/tasks"
              :fields '(("prompt" . "prompt") ("model" . "model") ("size" . "size")
                        ("count" . "count"))
              :defaults '(("model" . "flux-dev") ("size" . "1024x1024")
                          ("count" . 1) ("action" . "generate")))
   (media-row "qwen-image" :image "/qwen-image/images" "/qwen-image/tasks"
              :fields '(("prompt" . "prompt") ("model" . "model") ("size" . "size")
                        ("count" . "n"))
              :defaults '(("model" . "qwen-image-3.0") ("n" . 1) ("size" . "1024*1024")))
   (media-row "midjourney" :image "/midjourney/imagine" "/midjourney/tasks"
              :fields '(("prompt" . "prompt") ("model" . "version"))
              :defaults '(("action" . "generate") ("version" . "8.2") ("mode" . "fast")))
   ;; --- videos
   (media-row "seedance" :video "/seedance/videos" "/seedance/tasks"
              :content-p t
              :fields '(("model" . "model") ("ratio" . "ratio")
                        ("resolution" . "resolution") ("duration" . "duration"))
              :defaults '(("model" . "doubao-seedance-2-0-mini-260615")
                          ("resolution" . "480p") ("ratio" . "16:9")
                          ("generate_audio" . nil) ("duration" . 4)))
   (media-row "happyhorse" :video "/happyhorse/videos" "/happyhorse/tasks"
              :fields '(("prompt" . "prompt") ("model" . "model") ("ratio" . "ratio")
                        ("resolution" . "resolution") ("duration" . "duration"))
              :defaults '(("model" . "happyhorse-1.0-t2v") ("action" . "generate")
                          ("resolution" . "720P") ("duration" . 3) ("ratio" . "16:9")))
   (media-row "veo" :video "/veo/videos" "/veo/tasks"
              :fields '(("prompt" . "prompt") ("model" . "model")
                        ("ratio" . "aspect_ratio") ("image" . "image_urls"))
              :defaults '(("model" . "veo31-fast") ("action" . "text2video")
                          ("aspect_ratio" . "16:9")))
   (media-row "kling" :video "/kling/videos" "/kling/tasks"
              :fields '(("prompt" . "prompt") ("model" . "model")
                        ("ratio" . "aspect_ratio") ("duration" . "duration"))
              :defaults '(("model" . "kling-v3-turbo") ("action" . "text2video")
                          ("mode" . "std") ("duration" . 5) ("aspect_ratio" . "16:9")))
   (media-row "minimax" :video "/minimax/videos" "/minimax/tasks"
              :content-p t
              :fields '(("model" . "model") ("ratio" . "ratio")
                        ("resolution" . "resolution") ("duration" . "duration"))
              :defaults '(("model" . "MiniMax-H3") ("resolution" . "768P")
                          ("duration" . 4) ("ratio" . "16:9")))
   (media-row "wan" :video "/wan/videos" "/wan/tasks"
              :fields '(("prompt" . "prompt") ("model" . "model")
                        ("resolution" . "resolution") ("duration" . "duration"))
              :defaults '(("model" . "wan2.6-t2v") ("action" . "text2video")
                          ("duration" . 5) ("resolution" . "720P")))
   (media-row "grok" :video "/grok/videos" "/grok/tasks"
              :fields '(("prompt" . "prompt") ("model" . "model")
                        ("ratio" . "aspect_ratio") ("resolution" . "resolution")
                        ("duration" . "duration"))
              :defaults '(("model" . "grok-imagine-video-1.5-fast:reverse")
                          ("resolution" . "480p") ("duration" . 6)))
   ;; --- music and speech
   (media-row "suno" :audio "/suno/audios" "/suno/tasks"
              :fields '(("prompt" . "prompt") ("model" . "model") ("title" . "title")
                        ("style" . "style") ("lyric" . "lyric")
                        ("duration" . "duration"))
              :defaults '(("model" . "chirp-v6") ("custom" . nil)
                          ("instrumental" . nil) ("action" . "generate")))
   (media-row "fish" :audio "/fish/tts" "/fish/tasks"
              :model-header t
              :fields '(("prompt" . "text") ("model" . "model"))
              :defaults '(("model" . "s2-pro") ("format" . "mp3")))
   ;; --- the two that answer their own result
   (media-row "google-search" :lookup "/serp/google" nil
              :defaults '(("type" . "search") ("number" . 3) ("page" . 1)))
   (media-row "face" :lookup "/face/analyze" nil))
  "Every Ace Data Cloud service this cell speaks to, in the original plugin's
order: the paths, the fields a call may set, and the defaults a call that
names none gets.")

(defun service-names (kind)
  (loop for row in *services*
        when (eq kind (getf row :kind)) collect (getf row :name)))

(defun default-service (kind)
  (or (first (service-names kind))
      (fail "no service of kind ~(~a~)" kind)))

(defun find-service (kind name)
  "The row of KIND that NAME names, else the kind's own default; a search
of :ANY matches every service, which is how a task is found by name."
  (let ((name (nonblank name)))
    (cond
      ((eq kind :any)
       (or (and name (find-if (lambda (row) (string-equal name (getf row :name)))
                              *services*))
           (fail "~a is not an Ace Data Cloud service here: ~{~a~^, ~}"
                 name (loop for row in *services* collect (getf row :name)))))
      (t
       (let ((name (or name (default-service kind))))
         (or (find name *services*
                   :test (lambda (wanted row)
                           (and (eq kind (getf row :kind))
                                (string-equal wanted (getf row :name)))))
             (fail "~a is not a ~(~a~) service here: ~{~a~^, ~}"
                   name kind (service-names kind))))))))

(defun defaults-body (row)
  "ROW's body with the service's own defaults in it."
  (let ((body (nlk:json-object)))
    (loop for (key . value) in (getf row :defaults)
          do (setf (gethash key body) value))
    body))

(defun put-field (body row name value)
  "BODY with ROW's key for NAME set to VALUE, when the row takes one and the
call gave one."
  (let ((key (cdr (assoc name (getf row :fields) :test #'string=))))
    (when (and key value)
      (setf (gethash key body) value)))
  body)

(defun call-wait (wait-seconds)
  "The seconds this call may wait: the call's own, else the section's, never
over the ceiling."
  (min +max-wait-seconds+
       (max 0 (or wait-seconds (setting :wait-seconds) 0))))

;;; --- a call's body, per kind -----------------------------------------------------

(defun image-body (row request)
  "The image request ROW takes: the prompt, and whichever of the call's
options the service has a field for."
  (let ((body (defaults-body row)))
    (put-field body row "prompt" (image-request-prompt request))
    (put-field body row "model" (image-request-model request))
    (put-field body row "size" (image-request-size request))
    (put-field body row "resolution" (image-request-resolution request))
    (put-field body row "count" (image-request-copies request))
    (let ((urls (coerce (or (image-request-image-urls request) '()) 'list)))
      (when (and urls (assoc "image_urls" (getf row :fields) :test #'string=))
        (setf (gethash "image_urls" body) (coerce urls 'vector))
        (when (string= (getf row :name) "nano-banana")
          (setf (gethash "action" body) "edit"))))
    body))

(defun content-body (prompt image)
  "The content array Seedance and MiniMax take: the prompt, and the first
frame when the call named one."
  (let ((parts (list (nlk:json-object "type" "text" "text" prompt))))
    (when (nonblank image)
      (setf parts (append parts
                           (list (nlk:json-object "type" "image_url"
                                                   "image_url" (nlk:json-object "url" (nlk:trimmed image))
                                                   "role" "first_frame")))))
    (coerce parts 'vector)))

(defun video-body (row request)
  "The video request ROW takes."
  (let ((body (defaults-body row))
        (prompt (video-request-prompt request))
        (image (nonblank (video-request-image request))))
    (if (getf row :content-p)
        (setf (gethash "content" body) (content-body prompt image))
        (setf (gethash "prompt" body) prompt))
    (put-field body row "model" (video-request-model request))
    (put-field body row "ratio" (video-request-frame-ratio request))
    (put-field body row "resolution" (video-request-resolution request))
    (put-field body row "duration" (video-request-duration request))
    (when image
      (cond ((string= (getf row :name) "veo")
             (setf (gethash "action" body) "image2video"
                   (gethash "image_urls" body) (vector image))
             (remhash "aspect_ratio" body))
            ((getf row :content-p) nil)
            (t (put-field body row "image" image))))
    body))

(defun audio-body (row request)
  "The audio request ROW takes: Suno's song, or the text Fish Audio says."
  (let ((body (defaults-body row))
        (name (getf row :name))
        (prompt (nonblank (audio-request-prompt request))))
    (put-field body row "prompt" prompt)
    (put-field body row "model" (audio-request-model request))
    (cond ((string= name "suno")
           (put-field body row "title" (audio-request-title request))
           (put-field body row "style" (audio-request-style request))
           (put-field body row "lyric" (audio-request-lyric request))
           (put-field body row "duration" (audio-request-duration request))
           (when (audio-request-custom request)
             (setf (gethash "custom" body) t))
           (when (audio-request-instrumental request)
             (setf (gethash "instrumental" body) t
                   (gethash "custom" body) t))
           (suno-checks body prompt))
          (t
           (unless prompt
             (fail "Fish Audio says a text: give prompt, the words to speak"))))
    body))

(defun suno-checks (body prompt)
  "Suno's own rules: a description, or the pieces of a custom song."
  (let ((custom (gethash "custom" body))
        (instrumental (gethash "instrumental" body)))
    (cond (custom
           (unless (nonblank (gethash "title" body))
             (fail "Suno custom mode wants a title: give title, and style, and a lyric or instrumental true"))
           (unless (nonblank (gethash "style" body))
             (fail "Suno custom mode wants a style: give style, such as \"synthwave, female vocal\""))
           (unless (or instrumental (nonblank (gethash "lyric" body)))
             (fail "Suno custom mode wants a lyric, or instrumental true for a song without words")))
          ((null prompt)
           (fail "give prompt, the song's description, or custom true with title, style and lyric"))))
  body)

(defun lookup-body (row mode request)
  "The lookup request ROW takes: a Google search's words, or one face image."
  (let ((body (defaults-body row)))
    (if (string= (getf row :name) "face")
        (let ((image (nonblank (lookup-request-image-url request))))
          (unless image
            (fail "face analysis wants image_url: an https URL of a picture with a face"))
          (setf (gethash "image_url" body) image))
        (let ((query (nonblank (lookup-request-query request))))
          (unless query
            (fail "a ~(~a~) search wants query: the words to search for" mode))
          (setf (gethash "query" body) query)
          (when (string= mode "news")
            (setf (gethash "type" body) "news"))
          (when (lookup-request-amount request)
            (setf (gethash "number" body) (lookup-request-amount request)))
          (when (lookup-request-page request)
            (setf (gethash "page" body) (lookup-request-page request)))
          (when (nonblank (lookup-request-country request))
            (setf (gethash "country" body) (lookup-request-country request)))
          (when (nonblank (lookup-request-language request))
            (setf (gethash "language" body) (lookup-request-language request)))))
    body))

;;; --- the three steps of a call ---------------------------------------------------

(defun submit (row body)
  "One POST to ROW's generate path => the decoded object it answers."
  (multiple-value-bind (text status)
      (exchange :post (api-url (getf row :path))
                :content (nlk:encode-json-object body)
                :headers (when (getf row :model-header)
                           (let ((model (gethash "model" body)))
                             (remhash "model" body)
                             (and model (list (cons "model" model))))))
    (unless (ok-status-p status)
      (http-refusal status text))
    (decode-body text)))

(defun retrieve (row task-id)
  "One POST of ROW's retrieve action => the decoded object it answers."
  (multiple-value-bind (text status)
      (exchange :post (api-url (getf row :task))
                :content (nlk:encode-json-object (nlk:json-object "action" "retrieve" "id" task-id)))
    (unless (ok-status-p status)
      (http-refusal status text))
    (decode-body text)))

(defun poll (row task-id wait-seconds)
  "TASK-ID's last answer within WAIT-SECONDS: the object that says it has
finished, or the one that says it has not."
  (let* ((interval (max 1 (or (setting :poll-seconds) 3)))
         (deadline (+ (get-internal-real-time)
                      (* (call-wait wait-seconds) internal-time-units-per-second))))
    (loop
      (let ((object (retrieve row task-id)))
        (when (or (member (state-of object) '(:succeeded :failed) :test #'eq)
                  (>= (get-internal-real-time) deadline))
          (return object)))
      (sleep (min interval
                  (max 1 (ceiling (- deadline (get-internal-real-time))
                                   internal-time-units-per-second)))))))

(defun call-and-wait (row body wait-seconds)
  "POST BODY to ROW, follow the task it answers within WAIT-SECONDS => the
answer object the model reads."
  (require-key)
  (let ((object (submit row body)))
    (if (getf row :task)
        (let ((task-id (task-id-of object)))
          (unless task-id
            (fail "~a accepted the job but answered no task_id: ~a"
                  (getf row :name)
                  (nlk:clip (nlk:one-line (princ-to-string (nlk:encode-json-object object)))
                            +error-chars+)))
          (media-answer row (poll row task-id wait-seconds) task-id))
        (media-answer row object nil))))

;;; --- what each tool runs ----------------------------------------------------------

(defun run-image (request)
  (let ((row (find-service :image (image-request-service request))))
    (call-and-wait row (image-body row request) (image-request-wait-seconds request))))

(defun run-video (request)
  (let ((row (find-service :video (video-request-service request))))
    (call-and-wait row (video-body row request) (video-request-wait-seconds request))))

(defun run-audio (request)
  (let ((row (find-service :audio (audio-request-service request))))
    (call-and-wait row (audio-body row request) (audio-request-wait-seconds request))))

(defun run-lookup (request)
  (let* ((mode (string-downcase (or (nonblank (lookup-request-mode request)) "web")))
         (row (find-service :lookup (if (string= mode "face") "face" "google-search"))))
    (call-and-wait row (lookup-body row mode request) (setting :wait-seconds))))

(defun run-task (request)
  "A task's state, whoever submitted it: the row is found by name, and the
retrieve action is the one every service answers."
  (let* ((row (find-service :any (task-request-service request)))
         (task-id (nonblank (task-request-task-id request))))
    (unless (getf row :task)
      (fail "~a submits no task: there is nothing to retrieve" (getf row :name)))
    (unless task-id
      (fail "a task_id is required"))
    (require-key)
    (media-answer row (poll row task-id (task-request-wait-seconds request)) task-id)))

;;; --- the tools -----------------------------------------------------------------

(define-verb acedatacloud-image (request)
  "The acedatacloud_image tool: one picture from an Ace Data Cloud image
service, or a Nano Banana edit of the reference images the call names."
  (answering
    (require-key)
    (nlk:encode-json-object (run-image request))))

(define-verb acedatacloud-video (request)
  "The acedatacloud_video tool: one video from an Ace Data Cloud video
service, text-to-video or from a first frame."
  (answering
    (require-key)
    (nlk:encode-json-object (run-video request))))

(define-verb acedatacloud-audio (request)
  "The acedatacloud_audio tool: a Suno song, or Fish Audio's speech."
  (answering
    (require-key)
    (nlk:encode-json-object (run-audio request))))

(define-verb acedatacloud-lookup (request)
  "The acedatacloud_lookup tool: a Google web or news search, or face
landmark analysis of one image."
  (answering
    (require-key)
    (nlk:encode-json-object (run-lookup request))))

(define-verb acedatacloud-task (request)
  "The acedatacloud_task tool: a task's state, and its URLs once it has
rendered."
  (answering
    (require-key)
    (nlk:encode-json-object (run-task request))))

;;; --- /media --------------------------------------------------------------------

(defun command-request (text)
  "(values KIND OPTIONS PROMPT) of TEXT: the leading --flags out of it, then
the prompt."
  (let ((words (nlk:split-words text))
        (kind :image)
        (options '()))
    (loop while (and words (uiop:string-prefix-p "--" (first words)))
          do (let ((flag (string-downcase (subseq (pop words) 2))))
               (cond ((string= flag "video") (setf kind :video))
                     ((string= flag "audio") (setf kind :audio))
                     ((string= flag "lookup") (setf kind :lookup))
                     ((string= flag "news")
                      (setf kind :lookup (getf options :mode) "news"))
                     ((string= flag "face")
                      (setf kind :lookup (getf options :mode) "face"))
                     ((string= flag "image") (setf kind :image))
                     ((string= flag "no-wait") (setf (getf options :wait) 0))
                     ((null words) nil)
                     (t (let ((value (pop words)))
                          (cond ((string= flag "service")
                                 (setf (getf options :service) value))
                                ((string= flag "model")
                                 (setf (getf options :model) value))
                                ((string= flag "size")
                                 (setf (getf options :size) value))
                                ((string= flag "duration")
                                 (setf (getf options :duration)
                                       (parse-integer value :junk-allowed t)))
                                ((string= flag "wait")
                                 (setf (getf options :wait)
                                       (parse-integer value :junk-allowed t)))
                                (t nil)))))))
    (values kind options (format nil "~{~a~^ ~}" words))))

(defun answer-line (answer)
  "The one line a command answers with: the URLs it made, or the task to ask
about."
  (let* ((status (gethash "status" answer))
         (service (gethash "service" answer))
         (urls (let ((urls (gethash "urls" answer)))
                 (if (vectorp urls) (coerce urls 'list) '())))
         (task-id (gethash "task_id" answer))
         (error (gethash "error" answer)))
    (format nil "~a · ~a~@[ · ~{~a~^ ~}~]~@[ · task ~a~]~@[ · ~a~]"
            status service (and urls urls) task-id error)))

(defun command-answer (kind options prompt)
  "The answer object KIND's service gives for the /media arguments."
  (let ((object (nlk:json-object "prompt" prompt
                                  "service" (getf options :service)
                                  "model" (getf options :model)
                                  "size" (getf options :size)
                                  "duration" (getf options :duration)
                                  "wait_seconds" (getf options :wait))))
    (case kind
      (:video (run-video (decode-video-request object)))
      (:audio (run-audio (decode-audio-request object)))
      (:lookup (run-lookup
                (decode-lookup-request
                 (nlk:json-object "mode" (getf options :mode) "query" prompt
                                   "image_url" (getf options :image)))))
      (t (run-image (decode-image-request object))))))

(defun media-command (args session-id)
  "One /media invocation => the answer's text, and typed alone the dialog every
command answers then: one row that asks for the prompt."
  (declare (ignore session-id))
  (let ((line (nlk:trimmed (or args ""))))
    (if (zerop (length line))
        (values "Describe what to make, then /media with it."
                (nle:list-dialog
                 :title "Media"
                 :summary "Ace Data Cloud, through the acedatacloud-media cell"
                 :rows (list (nle:ask-row "Make a picture" "Prompt" "/media"
                                           :placeholder "a lighthouse at dusk, wide shot"
                                           :verb "generate"))))
        (multiple-value-bind (kind options prompt) (command-request line)
          (handler-case
              (values (answer-line (command-answer kind options prompt)) nil)
            (acedatacloud-media-error (condition)
              (values (format nil "ERROR: ~a" (acedatacloud-media-error-detail condition))
                      nil))
            (error (condition)
              (values (format nil "ERROR: acedatacloud-media: ~a"
                              (nlk:one-line (princ-to-string condition)))
                      nil)))))))

;;; --- the entry: the one declaration -----------------------------------------------

(nle:define-cell acedatacloud-media
  (:section ("acedatacloud-media")
    (:guide "paste an Ace Data Cloud key into api_key, or name the variable holding it in api_key_env (ACEDATACLOUD_API_KEY unless you say otherwise); next step: ask for a picture, or /media a lighthouse at dusk")
    ("api" :string :default "https://api.acedata.cloud"
     :doc "the Ace Data Cloud API root")
    ("api_key" :secret
     :doc "the Ace Data Cloud key, pasted here instead of naming a variable")
    ("api_key_env" :env :default "ACEDATACLOUD_API_KEY"
     :doc "the environment variable holding the Ace Data Cloud key")
    ("wait_seconds" :integer :default 30 :min 0
     :doc "how long a generate call waits for its render before answering the task id, at most 60; 0 answers at once")
    ("poll_seconds" :integer :default 3 :min 1
     :doc "how long between two checks of a running task")
    ("request_timeout" :integer :default 90 :min 5
     :doc "one HTTP request's ceiling, in seconds"))
  (:settings #'read-settings)
  (:help :acedatacloud-media
         "acedatacloud-media: acedatacloud_image, _video, _audio, _task and _lookup make and fetch pictures, videos, songs, speech and search results on an Ace Data Cloud key"
         *manual*)
  (:tool "acedatacloud_image" (nlk:contract 'image-request) #'acedatacloud-image)
  (:tool "acedatacloud_video" (nlk:contract 'video-request) #'acedatacloud-video)
  (:tool "acedatacloud_audio" (nlk:contract 'audio-request) #'acedatacloud-audio)
  (:tool "acedatacloud_task" (nlk:contract 'task-request) #'acedatacloud-task)
  (:tool "acedatacloud_lookup" (nlk:contract 'lookup-request) #'acedatacloud-lookup)
  (:command "media" 'media-command
            :description "Make a picture, video, song or search with Ace Data Cloud"
            :argument-hint "PROMPT"))
