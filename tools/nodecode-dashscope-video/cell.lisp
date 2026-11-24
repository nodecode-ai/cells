;;;; cell.lisp --- DashScope (Qwen Cloud) video generation as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of rriggs/hermes-plugin-dashscope-video (see NOTICE).
;;;; The original is a Hermes video-generation backend: it put Alibaba Cloud
;;;; Model Studio's HappyHorse models behind Hermes's own video_generate tool.
;;;; This cell keeps that tool's work as one model-facing tool, video_generate,
;;;; and one verb called through eval, (dashscope-video:generate "prompt").
;;;;
;;;; What it does, as the original does it: pick the mode from the inputs
;;;; (prompt alone is text-to-video, an image is image-to-video, reference
;;;; images are reference-to-video), resolve the model id for that mode, POST
;;;; the task to DashScope's async endpoint with X-DashScope-Async, poll
;;;; GET /api/v1/tasks/<id> until it is terminal, then download the clip and
;;;; cache it under this home -- DashScope's own urls expire.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "dashscope-video": {"api_key": "sk-sp-...", "model_family": "happyhorse-1.1"}
;;;; A vetoed section ("enabled": false) installs nothing; a section with no
;;;; key refuses at start, naming the variable it read.

(defpackage #:nodecode-dashscope-video
  (:use #:cl)
  (:nicknames #:dashscope-video)
  (:export #:generate #:dashscope-video-error))

(in-package #:nodecode-dashscope-video)

(nlk:define-peripheral dashscope-video :not-running t)

;;; --- what the original's constants say ------------------------------------------

(defparameter +api-default+ "https://token-plan.ap-southeast-1.maas.aliyuncs.com"
  "DashScope's token-plan base, the original's default; a pay-as-you-go key
answers on https://dashscope-intl.aliyuncs.com instead.")

(defparameter +key-env-default+ "QWEN_API_KEY"
  "The variable the original read the key from, and this cell still does
unless key_env names another.")

(defparameter +family-default+ "happyhorse-1.1"
  "The model family the original defaults to; a mode suffix is appended to it.")

(defparameter +modes+ '(("t2v" . "-t2v") ("i2v" . "-i2v") ("r2v" . "-r2v"))
  "The three modes, and the suffix each appends to the family id.")

(defparameter +sizes+ '(("16:9" . "1280*720") ("9:16" . "720*1280") ("1:1" . "960*960")
                         ("4:3" . "1024*768") ("3:4" . "768*1024"))
  "The aspect ratios the family takes, and the pixel size DashScope wants for each.")

(defparameter +terminal-statuses+ '("SUCCEEDED" "FAILED" "CANCELED" "UNKNOWN")
  "The task statuses a poll never leaves, as the original has them.")

(defparameter +min-duration+ 3
  "The shortest clip the family makes, in seconds.")

(defparameter +max-duration+ 10
  "The longest clip the family makes, in seconds.")

(defparameter +max-inline-bytes+ (* 10 1024 1024)
  "How large a local image may be before it is inlined as a data uri, as the
original's _MAX_INLINE_BYTES.")

(defparameter +video-types+ '("mp4" "webm" "mov" "m4v")
  "The file types a downloaded clip may carry; anything else is saved as mp4.")

(defparameter +primer+
  "DashScope (Qwen Cloud) video generation is available through the nodecode-dashscope-video cell:
one tool and one Lisp function, both on the operator's DashScope key. A clip takes two to three
minutes, so call it once and let it finish.
  video_generate   prompt= what the clip shows; image_url= a url, a data uri or a local image path
      to animate (image-to-video); reference_image_urls= images whose subject the clip keeps
      (reference-to-video, the first is used); model= a full model id to override; duration= 3 to
      10 seconds; aspect_ratio= 16:9, 9:16, 1:1, 4:3 or 3:4. It answers the path of the saved
      clip, the model and mode it used, the task id and the size.
  (dashscope-video:generate \"prompt\" :image-url \"/path/to/frame.png\" :duration 5)
      the same call from eval: it answers a plist instead of JSON.
The mode follows the inputs: a prompt alone is text-to-video, an image makes it image-to-video, a
reference image makes it reference-to-video, and the model id is the family plus -t2v, -i2v or -r2v.
The clip is cached under this home; the url DashScope answered expires. ERROR: the answer names the
problem, and a refused key is the operator's to set in the dashscope-video section (/dashscope-video
says what it is running with)."
  "What (help :dashscope-video) answers while the cell runs.")

;;; --- the section, read back ----------------------------------------------------

(defparameter *clock* (lambda () (/ (get-internal-real-time) internal-time-units-per-second))
  "Seconds since some fixed origin: the deadline a poll gives up at is read
through it, so a test can move the clock instead of waiting.")

(defparameter *pause* #'sleep
  "How a poll waits between two looks at a task; a test binds it to nothing.")

(defun api-root ()
  "The DashScope base this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :api) ""))))
    (if (plusp (length given)) given +api-default+)))

(defun key-env-name ()
  "The environment variable the key is read from."
  (let ((named (setting :key-env)))
    (if (and named (plusp (length named))) named +key-env-default+)))

(defun api-key ()
  "The key: the section's api_key, else the variable key_env names."
  (or (let ((inline (setting :api-key)))
        (and inline (plusp (length inline)) inline))
      (let ((variable (key-env-name)))
        (uiop:getenv variable))))

(defun check-config ()
  "Refuse a section that could never reach DashScope, naming what to set."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (format nil
                            "dashscope-video: no key: export ~a, or put \"api_key\" in the ~
                             dashscope-video section; a key comes from Alibaba Cloud Model Studio ~
                             (sk-sp-* for the token plan, sk-ws-* pay-as-you-go)"
                            (key-env-name)))))

;;; --- the wire ------------------------------------------------------------------

(defun auth-headers ()
  "The headers every DashScope call carries: the key, and what it accepts."
  (list (cons "authorization" (format nil "Bearer ~a" (or (api-key) "")))
        (cons "accept" "application/json")))

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a failed answer says, for the one line the model reads: DashScope
answers code and message, a proxy answers anything."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :string "message")
        (nlk:json-value body :string "code")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun call-json (method url &key body extra-headers)
  "METHOD URL with the key and BODY as JSON text => the decoded answer. A
failure below HTTP, or a refused status, signals DASHSCOPE-VIDEO-ERROR."
  (let ((answer nil)
        (status nil)
        (timeout (setting :request-timeout)))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers (append (auth-headers) extra-headers)
                    :content body
                    :timeout timeout
                    :connect-timeout (min 10 timeout)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a ~a: ~a" (string-upcase (string method)) url
              (nle:transport-failure-label condition url))))
    (if (< status 300)
        (decode-body answer)
        (fail "~a ~a: HTTP ~d~@[: ~a~]" (string-upcase (string method)) url status
              (body-error-text answer)))))

;;; --- the inputs ----------------------------------------------------------------

(defun image-source (text)
  "TEXT as the API takes it: a url or data uri unchanged, a local path
inlined as a base64 data uri, as the original's _normalize_source."
  (let ((text (string-trim '(#\Space) text)))
    (if (or (uiop:string-prefix-p "http://" text)
            (uiop:string-prefix-p "https://" text)
            (uiop:string-prefix-p "data:" text))
        text
        (let* ((file (ignore-errors (probe-file (uiop:parse-native-namestring text))))
               (size (and file (with-open-file (in file :element-type '(unsigned-byte 8))
                                 (file-length in)))))
          (unless file
            (fail "image file not found: ~a (pass a url, a data uri, or a path that exists)" text))
          (when (> size +max-inline-bytes+)
            (fail "image too large to inline: ~d bytes, the cap is ~d; pass a url instead"
                  size +max-inline-bytes+))
          (let ((octets (with-open-file (in file :element-type '(unsigned-byte 8))
                          (let ((vector (make-array size :element-type '(unsigned-byte 8))))
                            (read-sequence vector in)
                            vector))))
            (format nil "data:~a;base64,~a" (mime-for text)
                    (cl-base64:usb8-array-to-base64-string octets)))))))

(defun mime-for (path)
  "PATH's image type, as the original guesses it, with png for anything else."
  (let ((type (string-downcase (or (pathname-type (pathname path)) ""))))
    (cond ((string= type "png") "image/png")
          ((member type '("jpg" "jpeg") :test #'string=) "image/jpeg")
          ((string= type "webp") "image/webp")
          ((string= type "gif") "image/gif")
          (t "image/png"))))

(defun mode-for (image reference-images)
  "The mode these inputs are: an image is i2v, reference images r2v, else t2v."
  (cond (image "i2v")
        ((plusp (length reference-images)) "r2v")
        (t "t2v")))

(defun model-override (mode)
  "The section's model for MODE -- model_t2v, model_i2v, model_r2v -- or NIL."
  (let ((given (setting (intern (concatenate 'string "MODEL-" (string-upcase mode)) :keyword))))
    (and given (plusp (length (string-trim '(#\Space) given))) (string-trim '(#\Space) given))))

(defun model-family ()
  "The family id a mode suffix is appended to."
  (let ((given (setting :model-family)))
    (if (and given (plusp (length (string-trim '(#\Space) given))))
        (string-trim '(#\Space) given)
        +family-default+)))

(defun resolved-model (mode explicit)
  "The model id MODE asks for: EXPLICIT, else the section's override for
the mode, else the family plus the mode's suffix -- the original's order."
  (or (and explicit (stringp explicit) (plusp (length (string-trim '(#\Space) explicit)))
           (string-trim '(#\Space) explicit))
      (model-override mode)
      (concatenate 'string (model-family) (cdr (assoc mode +modes+ :test #'string=)))))

(defun clamped-duration (duration)
  "DURATION in seconds, inside what the family makes; the section's default
when the call says nothing."
  (max +min-duration+ (min +max-duration+ (or duration (setting :duration) 5))))

(defun size-for (aspect-ratio)
  "ASPECT-RATIO's pixel size, 16:9 for an unknown one, as the original defaults."
  (or (cdr (assoc (or aspect-ratio "") +sizes+ :test #'string=)) "1280*720"))

;;; --- the task ------------------------------------------------------------------

(defun submit-task (payload)
  "PAYLOAD posted as an async task => its task id."
  (let ((answer (call-json :post (format nil "~a/api/v1/services/aigc/video-generation/video-synthesis"
                                         (api-root))
                            :body (nlk:encode-json-object payload)
                            :extra-headers (list (cons "content-type" "application/json")
                                                 (cons "x-dashscope-async" "enable")))))
    (when (nlk:json-value answer :string "code")
      (fail "DashScope refused the task: ~a: ~a"
            (nlk:json-value answer :string "code")
            (or (nlk:json-value answer :string "message") "")))
    (or (nlk:json-value answer :string "output" "task_id")
        (fail "DashScope answered no task_id"))))

(defun poll-task (task-id)
  "TASK-ID polled until it is terminal => (values STATUS ANSWER)."
  (let* ((url (format nil "~a/api/v1/tasks/~a" (api-root) task-id))
         (deadline (+ (funcall *clock*) (setting :poll-deadline)))
         (status "PENDING")
         (answer nil))
    (loop until (member status +terminal-statuses+ :test #'string=)
          do (when (>= (funcall *clock*) deadline)
               (fail "task ~a was still ~a after ~d seconds; it may yet finish, so ask again or raise poll_deadline"
                     task-id status (setting :poll-deadline)))
             (funcall *pause* (setting :poll-interval))
             (setf answer (call-json :get url)
                   status (or (nlk:json-value answer :string "output" "task_status") "UNKNOWN")))
    (values status answer)))

(defun video-url (answer)
  "The clip's url in ANSWER -- output.video_url, else the first results url,
else the first choices content item, as the original looks for it."
  (let ((output (nlk:json-value answer :object "output")))
    (or (nlk:json-value output :string "video_url")
        (loop for result across (or (nlk:json-value output :array "results") #())
              when (nlk:json-value result :string "url")
                do (return (nlk:json-value result :string "url")))
        (loop for choice across (or (nlk:json-value output :array "choices") #())
              do (loop for item across (or (nlk:json-value choice :array "message" "content") #())
                       when (or (nlk:json-value item :string "video")
                                (nlk:json-value item :string "url"))
                         do (return-from video-url
                              (or (nlk:json-value item :string "video")
                                  (nlk:json-value item :string "url"))))))))

;;; --- the clip on disk ----------------------------------------------------------

(defun video-extension (url)
  "URL's file type when it is a video's, else mp4."
  (let* ((uri (ignore-errors (quri:uri url)))
         (type (and uri (quri:uri-path uri)
                    (pathname-type (pathname (quri:uri-path uri))))))
    (if (and type (member (string-downcase type) +video-types+ :test #'string=))
        (string-downcase type)
        "mp4")))

(defun file-stem (text)
  "TEXT as a file name: letters and digits kept, everything else a dash."
  (string-downcase (map 'string (lambda (char) (if (alphanumericp char) char #\-))
                        (or text "video"))))

(defun save-video (url task-id)
  "URL downloaded and written under this home's cache => its namestring.
DashScope's urls expire, so the clip is kept, as the original kept it."
  (let ((answer nil)
        (status nil)
        (limit (* 1024 1024 (setting :max-video-mb)))
        (timeout (setting :request-timeout)))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :get url :binary t :max-bytes limit
                    :timeout timeout :connect-timeout (min 10 timeout)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "the clip at ~a could not be fetched: ~a" url
              (nle:transport-failure-label condition url))))
    (unless (< status 300)
      (fail "the clip at ~a answered HTTP ~d" url status))
    (let* ((octets (if (stringp answer) (sb-ext:string-to-octets answer) answer))
           (path (nlk:home (format nil "cache/videos/~a-~d.~a"
                                   (file-stem task-id) (get-universal-time) (video-extension url)))))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                :if-does-not-exist :create
                                :element-type '(unsigned-byte 8))
        (write-sequence octets out))
      (namestring path))))

;;; --- the one verb --------------------------------------------------------------

(defparameter *last* nil
  "The plist the last successful call answered, so /dashscope-video can name it.")

(defun generate (prompt &key model image-url reference-image-urls duration aspect-ratio)
  "PROMPT made into a short clip on DashScope => a plist (:path :url :model
:mode :task-id :duration :size); the clip itself is under this home."
  (running-settings)
  (let ((prompt (string-trim '(#\Space #\Newline #\Tab) (or prompt "")))
        (references (remove-if-not (lambda (text)
                                      (and (stringp text)
                                           (plusp (length (string-trim '(#\Space) text)))))
                                    (or reference-image-urls '()))))
    (when (zerop (length prompt))
      (fail "a video needs a prompt"))
    (let* ((mode (mode-for image-url references))
           (model-id (resolved-model mode model))
           (length (clamped-duration duration))
           (size (size-for aspect-ratio))
           (input (nlk:json-object "prompt" prompt))
           (answer nil)
           (status nil))
      (when (eq mode "i2v")
        (setf (gethash "image_url" input) (image-source image-url)))
      (when (eq mode "r2v")
        (setf (gethash "ref_image_url" input) (image-source (first references))))
      (let* ((task-id (submit-task (nlk:json-object
                                     "model" model-id
                                     "input" input
                                     "parameters" (nlk:json-object "duration" length "size" size))))
             (url nil))
        (multiple-value-setq (status answer) (poll-task task-id))
        (unless (string= status "SUCCEEDED")
          (fail "task ~a ended ~a~@[: ~a~]" task-id status
                (nlk:json-value answer :string "output" "message")))
        (setf url (video-url answer))
        (unless url
          (fail "task ~a succeeded but carried no video url" task-id))
        (setf *last* (list :path (save-video url task-id) :url url :model model-id
                           :mode mode :task-id task-id :duration length :size size))))))

;;; --- the tool ------------------------------------------------------------------

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
        (string-trim '(#\Space) value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun argument-optional-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or NIL."
  (let ((value (gethash name args)))
    (and (stringp value)
         (plusp (length (string-trim '(#\Space) value)))
         (string-trim '(#\Space) value))))

(defun argument-optional-integer (args name)
  "ARGS' NAME as an integer, or NIL when it is absent or not one."
  (let ((value (gethash name args)))
    (typecase value
      (integer value)
      (number (round value))
      (string (ignore-errors (parse-integer (string-trim '(#\Space) value) :junk-allowed t)))
      (t nil))))

(defun argument-list (args name)
  "ARGS' NAME as a list of the strings it carries."
  (let ((given (ignore-errors (nlk:json-array args name))))
    (loop for item across (or given #())
          when (and (stringp item) (plusp (length (string-trim '(#\Space) item))))
            collect (string-trim '(#\Space) item))))

(defun tool-generate (args)
  "The video_generate tool: the clip as a JSON object, or the repair line."
  (handler-case
      (let ((made (generate (argument-text args "prompt")
                            :model (argument-optional-text args "model")
                            :image-url (argument-optional-text args "image_url")
                            :reference-image-urls (argument-list args "reference_image_urls")
                            :duration (argument-optional-integer args "duration")
                            :aspect-ratio (argument-optional-text args "aspect_ratio"))))
        (nlk:encode-json-object
         (nlk:json-object "path" (getf made :path)
                           "url" (getf made :url)
                           "model" (getf made :model)
                           "mode" (getf made :mode)
                           "task_id" (getf made :task-id)
                           "duration" (getf made :duration)
                           "size" (getf made :size)
                           "note" "the clip is saved under this home; the url expires, the path does not")))
    ((or error sb-sys:deadline-timeout) (condition)
      (nle:failure (format nil "ERROR: dashscope-video: ~a" condition)))))

(defparameter *generate-schema*
  (list :description (concatenate 'string
                                  "Generate a short video on Alibaba Cloud DashScope (Qwen Cloud) with "
                                  "its HappyHorse video models. A prompt alone makes a text-to-video "
                                  "clip; adding image_url animates that image; adding "
                                  "reference_image_urls keeps the subject of the images you name. "
                                  "One clip takes two to three minutes and is billed to the "
                                  "operator's DashScope key, so call it once with a considered "
                                  "prompt. It answers the path of the saved clip, its url, the "
                                  "model and mode it used, and how long it is.")
   :parameters (nlk:json-object
                 "type" "object"
                 "properties" (nlk:json-object
                               "prompt" (nlk:json-object
                                         "type" "string"
                                         "description" (concatenate 'string
                                                                    "What the clip shows: the scene, the camera, "
                                                                    "the motion. The one field a call cannot do without."))
                               "image_url" (nlk:json-object
                                            "type" "string"
                                            "description" (concatenate 'string
                                                                       "An image to animate (image-to-video): an https url, "
                                                                       "a data uri, or a local image path, which is inlined."))
                               "reference_image_urls" (nlk:json-object
                                                       "type" "array"
                                                       "items" (nlk:json-object "type" "string")
                                                       "description" (concatenate 'string
                                                                                  "Images whose subject the clip keeps "
                                                                                  "(reference-to-video); the family takes one, "
                                                                                  "the first is used."))
                               "model" (nlk:json-object
                                        "type" "string"
                                        "description" (concatenate 'string
                                                                   "A full model id, overriding the section's family. "
                                                                   "Most calls leave it out."))
                               "duration" (nlk:json-object
                                           "type" "integer"
                                           "minimum" +min-duration+
                                           "maximum" +max-duration+
                                           "description" (format nil "Clip length in seconds, ~d to ~d."
                                                                 +min-duration+ +max-duration+))
                               "aspect_ratio" (nlk:json-object
                                               "type" "string"
                                               "enum" #("16:9" "9:16" "1:1" "4:3" "3:4")
                                               "description" "The clip's shape; 16:9 unless said."))
                 "required" #("prompt")))
  "The video_generate tool, as the original's model ids and parameters have it.")

;;; --- /dashscope-video ----------------------------------------------------------

(defun run-command (args session-id)
  "One /dashscope-video: what the cell runs with, and the last clip."
  (declare (ignore session-id args))
  (handler-case
      (with-output-to-string (out)
        (format out "dashscope-video: ~a~%  key: ~a~%  family: ~a~%"
                (api-root)
                (if (api-key)
                    (format nil "set (~a)"
                            (if (plusp (length (or (setting :api-key) "")))
                                "the section's api_key"
                                (key-env-name)))
                    (format nil "missing: export ~a or set api_key" (key-env-name)))
                (model-family))
        (dolist (mode +modes+)
          (format out "  ~a  ~a~%" (first mode) (resolved-model (first mode) nil)))
        (if *last*
            (format out "  last clip: ~a (~a, ~d s)~%" (getf *last* :path)
                    (getf *last* :model) (getf *last* :duration))
            (format out "  no clip yet: (dashscope-video:generate \"prompt\") makes one~%")))
    (error (condition) (format nil "dashscope-video: ~a" condition))))

;;; --- the entry: the one declaration ---------------------------------------------

(defun start ()
  "Refuse a section with no key, and forget the last clip on stop."
  (check-config)
  (setf *last* nil)
  (nle:on-stop (lambda () (setf *last* nil))))

(nle:define-cell dashscope-video
  (:section ("dashscope-video")
    (:guide "paste a DashScope key from Alibaba Cloud Model Studio into api_key (token plan keys start sk-sp-, pay-as-you-go sk-ws-), or name the environment variable holding it in key_env; next step: /dashscope-video shows the endpoint, the key it read and the model each mode asks for")
    ("api" :string
     :doc "the DashScope base url: https://token-plan.ap-southeast-1.maas.aliyuncs.com unless answered; pay-as-you-go keys answer on https://dashscope-intl.aliyuncs.com")
    ("api_key" :secret
     :doc "the DashScope key itself, when it is not in the environment")
    ("key_env" :env :default "QWEN_API_KEY"
     :doc "the environment variable holding the key")
    ("model_family" :string :default "happyhorse-1.1"
     :doc "the video family a mode suffix (-t2v, -i2v, -r2v) is appended to")
    ("model_t2v" :string
     :doc "the model id text-to-video asks for, when it is not the family's -t2v")
    ("model_i2v" :string
     :doc "the model id image-to-video asks for, when it is not the family's -i2v")
    ("model_r2v" :string
     :doc "the model id reference-to-video asks for, when it is not the family's -r2v")
    ("duration" :integer :default 5 :min 1
     :doc "the clip length in seconds a call asks for when it says nothing; 3 to 10 are taken")
    ("poll_interval" :integer :default 5 :min 1
     :doc "seconds between two looks at a running task")
    ("poll_deadline" :integer :default 600 :min 10
     :doc "seconds a task may run before the call gives up on it")
    ("request_timeout" :integer :default 30 :min 1
     :doc "one HTTP request's ceiling, connect included")
    ("max_video_mb" :integer :default 256 :min 1
     :doc "how large a downloaded clip may be"))
  (:help :dashscope-video
         "dashscope-video: video_generate makes a short clip on Alibaba Cloud's HappyHorse models"
         +primer+)
  (:command "dashscope-video" 'run-command
            :description "Show the DashScope endpoint, the key it read and the last clip")
  (:tool "video_generate" *generate-schema* 'tool-generate)
  (:start #'start))
