;;;; cell.lisp --- generate_video, video_status and /video, over MiniMax's
;;;; video generation API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One API root the operator names, in the `minimax-video' section of the
;;;; shared config:
;;;;   "minimax-video": {"api": "https://api.minimax.io",
;;;;                      "api_key_env": "MINIMAX_PAYG_KEY"}
;;;; Every video is a MiniMax asynchronous task:
;;;;   1. POST {api}/v1/video_generation           -> task_id
;;;;   2. GET  {api}/v1/query/video_generation     -> status, file_id
;;;;   3. GET  {api}/v1/files/retrieve             -> download_url
;;;;   4. GET  the download_url, written under output_dir.
;;;; Nothing hooks a provider lane: MiniMax speaks HTTP, and NLK:HTTP is the
;;;; one seam.
;;;;
;;;; Rewritten from rriggs's Hermes Agent plugin (see NOTICE): the three modes
;;;; (text, first-frame image, subject reference), the model and resolution
;;;; defaults, the 6-or-10-second clamp, camera control left in the prompt, the
;;;; local-image-to-data-URI path with its 10 MB cap, the poll and its
;;;; deadline, and the fields the answer carries. The Hermes video_gen
;;;; provider interface is not here: a Nodecode cell declares its tools, its
;;;; command and its section itself.

(in-package #:nodecode-minimax-video)

;;; --- the manual -------------------------------------------------------------

(defparameter *manual*
  "Videos come from the nodecode-minimax-video cell: the generate_video tool, which a person can
also call from eval in the NODECODE-MINIMAX-VIDEO package, the video_status tool, and the
/video command.
  generate_video  {prompt, model, image, reference_images, duration, resolution, wait_seconds}
      One MiniMax (Hailuo) video task: submit, poll, retrieve the file, download it under the
      cell's output_dir. The answer names the file, its size in bytes, the model, the mode
      (t2v, i2v or s2v), the duration and resolution, and the task id. A call that ran out of
      its wait_seconds answers the task id and its state instead: call video_status with that
      task_id to finish it.
  video_status  {task_id, wait_seconds}
      A task's state, and once it has succeeded the video itself, downloaded and named.
  /video [--model ID] [--duration 6|10] [--resolution 768P] [--image PATH] [--reference PATH]
         [--wait SECONDS|--no-wait] PROMPT
      The same from the operator's side of the keyboard. Typed alone it asks for the prompt.
Write the prompt as a shot: subject, action, setting, light, camera. Camera moves go in the
brackets MiniMax reads - [Push in], [Pan left], [Truck right], [Zoom out], [Static shot] -
and a cut reads \"[Pedestal up], then [Static shot]\". A duration is 6 or 10 seconds, the only
two MiniMax renders; anything else is taken to the nearer. A local image is sent inline as a
data URI, at most 10 MB, so a larger picture should be an https URL. A failed call says the
HTTP status, MiniMax's own message and the setting that fixes it: a wrong key, an api root for
another region, a rate limit or an empty account. Nothing is billed for a call refused before
the request."

  "What (help :minimax-video) answers while the cell runs.")

;;; --- what MiniMax's video API takes -----------------------------------------

(defparameter +default-api+ "https://api.minimax.io"
  "The MiniMax platform's API root for the global site; an account on another
region names its own in the section.")

(defparameter +default-model+ "MiniMax-Hailuo-2.3"
  "Text-to-video and image-to-video: the newest Hailuo model, the original
plugin's default.")

(defparameter +default-model-s2v+ "S2V-01"
  "Subject-reference-to-video, the original plugin's default.")

(defparameter +key-portal+
  "https://platform.minimax.io/user-center/basic-information/interface-key"
  "Where a MiniMax platform key is made.")

(defparameter +max-inline-bytes+ (* 10 1024 1024)
  "The most one local image may be before it is sent as a data URI: the
original plugin's cap, under MiniMax's own 20 MB.")

(defparameter +error-chars+ 300
  "How much of MiniMax's own error text a refusal quotes back.")

(defparameter +user-agent+ "nodecode-minimax-video/0.1.0"
  "What the cell calls itself to MiniMax. Nothing of another client is
impersonated, and no other client's headers ride along.")

;;; --- answers, and the one condition ------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (minimax-video-error (condition)
       (nle:failure (format nil "ERROR: ~a" (minimax-video-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: minimax-video: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun set-member (object key value)
  "OBJECT with KEY set to VALUE when VALUE is non-NIL; OBJECT either way."
  (when value (setf (gethash key object) value))
  object)

;;; --- reading the section --------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

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
      (nlk:config-error "minimax-video.api is empty: it is ~a unless your account is on another region"
                         +default-api+))
    (multiple-value-bind (scheme host) (url-parts url)
      (unless scheme
        (nlk:config-error "minimax-video.api is ~s; give the whole address, such as ~a"
                          url +default-api+))
      (unless (member scheme '("http" "https") :test #'string=)
        (nlk:config-error "minimax-video.api scheme ~s is not http or https" scheme))
      (when (and (string= scheme "http") (not (local-host-p host)))
        (nlk:config-error "minimax-video.api is ~s over plain http, so the key would cross the public internet in clear text: use https://" url))
      (string-right-trim "/" url))))

(defun read-settings (values table)
  "The declared VALUES, with the API root checked and the folder that videos are
written to resolved."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :api) (check-api (or (getf values :api) +default-api+))
          (getf plist :output-dir)
          (uiop:ensure-directory-pathname
           (nlk:expand-home (or (nonblank (getf values :output-dir)) "~/.nodecode/videos"))))
    plist))

;;; --- the key --------------------------------------------------------------------

(defun api-key ()
  "The MiniMax key: the section's own, else the variable api_key_env names."
  (or (nonblank (setting :api-key))
      (let ((name (nonblank (setting :api-key-env))))
        (and name (nonblank (nle::credential-env name))))))

(defun require-key ()
  "The key a call is billed to, refused before anything is sent when there is
none."
  (or (api-key)
      (fail "no MiniMax key: set the variable ~a, or paste a key into the minimax-video section's api_key (a key is made at ~a). Nothing was sent and nothing was billed."
            (or (nonblank (setting :api-key-env)) "MINIMAX_PAYG_KEY") +key-portal+)))

(defun replace-text (text part replacement)
  "TEXT with every occurrence of PART replaced, so a key MiniMax echoes back is
never shown."
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

;;; --- the call the request asks for ------------------------------------------------

(defun mode-of (image references)
  "The mode a call is: an image is a first frame, reference images are the
character, neither is a prompt alone. The original plugin resolves an image
first, and so does this."
  (cond (image "i2v")
        ((plusp (length references)) "s2v")
        (t "t2v")))

(defun resolve-model (mode named)
  "The model the call names, else the section's, else the mode's own default."
  (or (nonblank named)
      (if (string= mode "s2v")
          (or (nonblank (setting :model-s2v)) +default-model-s2v+)
          (or (nonblank (setting :model)) +default-model+))))

(defun clamp-duration (seconds)
  "MiniMax renders 6 or 10 seconds. The original plugin's rule: 10 or more is
10, anything else is 6."
  (if (and (integerp seconds) (>= seconds 10)) 10 6))

(defun normalize-resolution (text)
  (let ((res (string-upcase (nlk:trimmed text))))
    (if (uiop:string-suffix-p "P" res) res (concatenate 'string res "P"))))

(defun resolve-resolution (model named)
  "The resolution the call named, else the section's, else the model's own:
768P on Hailuo, 720P on the older models, as the original plugin defaulted."
  (let ((named (nonblank named))
        (configured (nonblank (setting :resolution))))
    (cond (named (normalize-resolution named))
          ((and configured (not (string-equal configured "auto")))
           (normalize-resolution configured))
          ((search "hailuo" (string-downcase (or model ""))) "768P")
          (t "720P"))))

;;; --- images the call names --------------------------------------------------------

(defun image-mime (path)
  "The media type of PATH by its extension, a PNG when the extension says
nothing: the original plugin's mimetypes guess and its fallback."
  (let ((type (string-downcase (or (pathname-type path) ""))))
    (cond ((string= type "png") "image/png")
          ((member type '("jpg" "jpeg") :test #'string=) "image/jpeg")
          ((string= type "webp") "image/webp")
          ((string= type "gif") "image/gif")
          (t "image/png"))))

(defun read-file-octets (path)
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun to-data-uri (path)
  "PATH's bytes as a data URI, refused when the file is missing or over the cap."
  (let ((truename (probe-file (nlk:expand-home path))))
    (unless truename
      (fail "the image ~a does not exist: give a local path, an https URL or a data URI" path))
    (let ((size (with-open-file (in truename :direction :input
                                     :element-type '(unsigned-byte 8))
                  (file-length in))))
      (when (> size +max-inline-bytes+)
        (fail "the image ~a is ~:d bytes, over the ~:d one local image may be sent inline: pass an https URL instead"
              path size +max-inline-bytes+))
      (format nil "data:~a;base64,~a"
              (image-mime truename)
              (cl-base64:usb8-array-to-base64-string (read-file-octets truename))))))

(defun normalize-source (text)
  "TEXT as the API takes it: an http(s) URL and a data URI pass through
unchanged, and a local path becomes a base64 data URI."
  (let ((text (nlk:trimmed (or text ""))))
    (cond ((uiop:string-prefix-p "https://" text) text)
          ((uiop:string-prefix-p "http://" text) text)
          ((uiop:string-prefix-p "data:" text) text)
          (t (to-data-uri text)))))

;;; --- the wire --------------------------------------------------------------------

(defun percent-encode (text)
  "TEXT as one URL query value: everything outside the unreserved set is
percent-encoded."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets (or text "") :external-format :utf-8)
          for char = (code-char octet)
          do (if (or (alphanumericp char) (find char "-._~"))
                 (write-char char out)
                 (format out "%~2,'0X" octet)))))

(defun api-url (path &optional query)
  (let ((url (concatenate 'string (setting :api) path)))
    (if query (format nil "~a?~a" url query) url)))

(defun exchange (method url &key content binary)
  "One exchange with MiniMax => (values BODY STATUS). The key rides as a bearer
token and nowhere else: never in a URL, never in a log line."
  (let ((key (require-key)))
    (handler-case
        (multiple-value-bind (body status)
            (nlk:http method url
                      :headers (append (list (cons "user-agent" +user-agent+)
                                             (cons "accept" (if binary "*/*" "application/json"))
                                             (cons "authorization" (format nil "Bearer ~a" key)))
                                       (when content
                                         (list (cons "content-type" "application/json"))))
                      :content content
                      :binary binary
                      :timeout (setting :request-timeout)
                      :connect-timeout (min 15 (setting :request-timeout)))
          (values body status))
      (error (condition)
        (fail "~(~a~) ~a did not answer: ~a" method url
              (nlk:one-line (princ-to-string condition)))))))

(defun decode-body (body)
  "BODY, an HTTP body, as decoded JSON; a body that is not JSON is kept as the
text it was, so the model can read what MiniMax said."
  (cond ((hash-table-p body) body)
        ((stringp body)
         (let ((text (nlk:trimmed body)))
           (cond ((zerop (length text)) (nlk:json-object))
                 (t (handler-case (nlk:decode-json text)
                      (error () text))))))
        (t (nlk:json-object))))

(defun api-error (object)
  "MiniMax's base_resp status_msg when its code is not 0, else NIL."
  (let ((base (nlk:json-value object :object "base_resp")))
    (when base
      (let ((code (nlk:json-value base :integer "status_code")))
        (when (and (integerp code) (not (zerop code)))
          (or (nlk:json-value base :string "status_msg") "unknown error"))))))

(defun error-message (body)
  "MiniMax's own words for a failed answer, or what it answered."
  (let* ((object (decode-body body))
         (said (or (api-error object)
                   (nlk:json-value object :string "message")
                   (and (stringp body) (nonblank body)
                        (nlk:clip (nlk:one-line body) +error-chars+)))))
    (or said "no error message")))

(defun status-hint (status)
  "The parenthetical hint the model can act on, or NIL."
  (case status
    ((401 403) " (check the key: api_key in the minimax-video section, or the variable api_key_env names)")
    (404 " (check api in the minimax-video section: https://api.minimax.io unless your account is on another region)")
    (429 " (the account is rate limited or out of credit: check the plan on the MiniMax platform)")
    (t nil)))

(defun http-refusal (status body)
  "A failed answer as a refusal: the status, MiniMax's words, and the hint that
usually fixes it."
  (fail "MiniMax answered HTTP ~a: ~a~@[~a~]"
        status (redact (error-message body)) (status-hint status)))

;;; --- the three steps of a render ---------------------------------------------------

(defun submit-task (prompt mode model duration resolution image references)
  "One POST to /v1/video_generation => the task id MiniMax answered with."
  (let ((payload (nlk:json-object "model" model "prompt" prompt
                                   "duration" duration "resolution" resolution
                                   "prompt_optimizer" (and (setting :prompt-optimizer) t))))
    (cond ((string= mode "i2v")
           (setf (gethash "first_frame_image" payload) (normalize-source image)))
          ((string= mode "s2v")
           (setf (gethash "subject_reference" payload)
                 (coerce (loop for source in references
                               collect (nlk:json-object
                                        "type" "character"
                                        "image" (vector (normalize-source source))))
                         'vector))))
    (multiple-value-bind (body status)
        (exchange :post (api-url "/v1/video_generation")
                  :content (nlk:encode-json-object payload))
      (unless (and (integerp status) (< status 400))
        (http-refusal status body))
      (let ((object (decode-body body)))
        (when (api-error object)
          (fail "MiniMax refused the job: ~a" (api-error object)))
        (or (nlk:json-value object :string "task_id")
            (fail "MiniMax accepted the job but answered no task_id"))))))

(defun query-task (task-id)
  "One GET of a task's state => the decoded object."
  (multiple-value-bind (body status)
      (exchange :get (api-url "/v1/query/video_generation"
                               (format nil "task_id=~a" (percent-encode task-id))))
    (unless (and (integerp status) (< status 400))
      (http-refusal status body))
    (let ((object (decode-body body)))
      (when (api-error object)
        (fail "MiniMax task ~a: ~a" task-id (api-error object)))
      object)))

(defun retrieve-file (file-id)
  "The download URL of a finished task's file."
  (multiple-value-bind (body status)
      (exchange :get (api-url "/v1/files/retrieve"
                               (format nil "file_id=~a" (percent-encode file-id))))
    (unless (and (integerp status) (< status 400))
      (http-refusal status body))
    (let ((object (decode-body body)))
      (when (api-error object)
        (fail "MiniMax file ~a: ~a" file-id (api-error object)))
      (or (nlk:json-value (nlk:json-value object :object "file") :string "download_url")
          (fail "MiniMax retrieved file ~a but answered no download_url" file-id)))))

(defun success-status-p (status)
  (string-equal (or status "") "Success"))

(defun failed-status-p (status)
  (member (string-downcase (or status "")) '("fail" "failed") :test #'string=))

;;; --- the file -----------------------------------------------------------------

(defun octet-string (octets start end)
  (map 'string #'code-char (subseq octets start end)))

(defun video-extension (octets)
  "\"mp4\" when the container says so, \"webm\" for Matroska, else \"mp4\"."
  (cond ((and (>= (length octets) 12) (string= "ftyp" (octet-string octets 4 8)))
         "mp4")
        ((and (>= (length octets) 4)
              (= (aref octets 0) #x1A) (= (aref octets 1) #x45)
              (= (aref octets 2) #xDF) (= (aref octets 3) #xA3))
         "webm")
        (t "mp4")))

(defun file-stem (model)
  "MODEL as a file name: lowercase, and anything outside letters, digits, a
dash and an underscore becomes a dash."
  (with-output-to-string (out)
    (loop for char across (string-downcase (or model "video"))
          do (write-char (if (or (alphanumericp char) (find char "-_")) char #\-) out))))

(defun save-video (octets model)
  "(values PATH BYTES): OCTETS written under the cell's output_dir, never over
another file there."
  (let ((directory (setting :output-dir))
        (type (video-extension octets)))
    (ensure-directories-exist (merge-pathnames "keep" directory))
    (let ((file (loop for attempt from 0
                      for candidate = (merge-pathnames
                                       (format nil "minimax-~a-~d~@[-~d~].~a"
                                               (file-stem model) (get-universal-time)
                                               (and (plusp attempt) attempt) type)
                                       directory)
                      unless (probe-file candidate)
                        return candidate)))
      (alexandria:write-byte-vector-into-file octets file :if-exists :supersede)
      (values (namestring file) (length octets)))))

(defun download-video (url model)
  "URL's bytes, written under output_dir => (values PATH BYTES)."
  (multiple-value-bind (body status) (exchange :get url :binary t)
    (unless (and (integerp status) (< status 400))
      (http-refusal status (and (stringp body) body)))
    (unless (typep body '(vector (unsigned-byte 8)))
      (fail "MiniMax's download answered no bytes"))
    (when (zerop (length body))
      (fail "MiniMax's download answered an empty file"))
    (save-video body model)))

;;; --- what a call answers -------------------------------------------------------------

(defun finished-answer (object model mode duration resolution)
  "The answer for a task that succeeded: the video retrieved, downloaded and
named."
  (let ((file-id (nlk:json-value object :string "file_id"))
        (task-id (nlk:json-value object :string "task_id")))
    (unless file-id
      (fail "MiniMax task ~@[~a ~]succeeded with no file_id" task-id))
    (multiple-value-bind (path bytes) (download-video (retrieve-file file-id) model)
      (let ((answer (nlk:json-object "status" "Success"
                                      "video" path "bytes" bytes "model" model)))
        (set-member answer "mode" mode)
        (set-member answer "duration" duration)
        (set-member answer "resolution" resolution)
        (set-member answer "task_id" task-id)
        (set-member answer "file_id" file-id)
        answer))))

(defun pending-answer (object model mode duration resolution)
  "The answer for a task that is still rendering, or that failed: its state and
the id to ask about again."
  (let ((status (or (nlk:json-value object :string "status") "Processing")))
    (let ((answer (nlk:json-object "status" status "model" model)))
      (set-member answer "mode" mode)
      (set-member answer "duration" duration)
      (set-member answer "resolution" resolution)
      (set-member answer "task_id" (nlk:json-value object :string "task_id"))
      (set-member answer "file_id" (nlk:json-value object :string "file_id"))
      (setf (gethash "note" answer)
            (if (failed-status-p status)
                "the render failed: ask again, a simpler or shorter prompt often helps"
                "the render is still running: call video_status with this task_id to finish it"))
      answer)))

(defun wait-for-task (task-id wait-seconds model mode duration resolution)
  "Poll TASK-ID until it is terminal or WAIT-SECONDS have passed => the answer
object: the video when the render succeeded, the task's state when it is still
running or failed."
  (let* ((interval (max 1 (or (setting :poll-seconds) 5)))
         (waited (max 0 (or wait-seconds 0)))
         (deadline (+ (get-internal-real-time)
                      (* waited internal-time-units-per-second))))
    (loop
      (let* ((object (query-task task-id))
             (status (nlk:json-value object :string "status")))
        (cond ((success-status-p status)
               (return (finished-answer object model mode duration resolution)))
              ((failed-status-p status)
               (return (pending-answer object model mode duration resolution)))
              ((>= (get-internal-real-time) deadline)
               (return (pending-answer object model mode duration resolution)))
              (t (sleep interval)))))))

(defun render (prompt &key model image references duration resolution wait-seconds)
  "One video, generated or animated, the way the tool and the command ask for
one => the answer object."
  (let* ((prompt (nlk:trimmed (or prompt "")))
         (image (nonblank image))
         (references (remove-if-not #'nonblank (coerce (or references '()) 'list)))
         (mode (mode-of image references))
         (model (resolve-model mode model))
         (length-seconds (clamp-duration duration))
         (size (resolve-resolution model resolution))
         (wait (if (integerp wait-seconds) wait-seconds
                   (or (setting :wait-seconds) 600))))
    (when (zerop (length prompt))
      (fail "a prompt is required and cannot be empty"))
    (require-key)
    (let ((task-id (submit-task prompt mode model length-seconds size image references)))
      (wait-for-task task-id wait model mode length-seconds size))))

;;; --- the tools -----------------------------------------------------------------

(define-verb generate-video (request)
  "The generate_video tool: one MiniMax video from a prompt, a first-frame
image, or a set of subject reference images."
  (answering
    (nlk:encode-json-object
     (render (video-request-prompt request)
             :model (video-request-model request)
             :image (video-request-image request)
             :references (video-request-reference-images request)
             :duration (video-request-duration request)
             :resolution (video-request-resolution request)
             :wait-seconds (video-request-wait-seconds request)))))

(define-verb video-status (request)
  "The video_status tool: a task's state, and its video once the render has
finished."
  (answering
    (let ((task-id (nonblank (video-task-task-id request))))
      (unless task-id
        (fail "a task_id is required"))
      (require-key)
      (nlk:encode-json-object
       (wait-for-task task-id
                      (video-task-wait-seconds request)
                      (or (nonblank (setting :model)) +default-model+)
                      nil nil nil)))))

;;; --- /video --------------------------------------------------------------------

(defun command-request (text)
  "(values PROMPT OPTIONS) of TEXT: the leading --flags out of it, then the
prompt. --image and --reference take one value each, --reference may repeat."
  (let ((words (nlk:split-words text))
        (options '())
        (references '()))
    (loop while (and words (uiop:string-prefix-p "--" (first words)))
          do (let ((flag (string-downcase (subseq (pop words) 2))))
               (cond ((string= flag "no-wait")
                      (setf (getf options :wait) 0))
                     ((null words) nil)
                     (t (let ((value (pop words)))
                          (cond ((string= flag "image")
                                 (setf (getf options :image) value))
                                ((string= flag "reference")
                                 (push value references))
                                ((string= flag "duration")
                                 (setf (getf options :duration)
                                       (parse-integer value :junk-allowed t)))
                                ((string= flag "resolution")
                                 (setf (getf options :resolution) value))
                                ((string= flag "model")
                                 (setf (getf options :model) value))
                                ((string= flag "wait")
                                 (setf (getf options :wait)
                                       (parse-integer value :junk-allowed t)))
                                (t nil)))))))
    (values (format nil "~{~a~^ ~}" words)
            (list* :references (nreverse references) options))))

(defun video-command (args session-id)
  "One /video invocation => the answer's text, and typed alone the dialog every
command answers then: one row that asks for the prompt."
  (declare (ignore session-id))
  (let ((line (nlk:trimmed (or args ""))))
    (if (zerop (length line))
        (values "Describe the video, then /video with it."
                (nle:list-dialog
                 :title "Video"
                 :summary "MiniMax Hailuo, through the minimax-video cell"
                 :rows (list (nle:ask-row "Generate a video" "Prompt" "/video"
                                           :placeholder "a red fox crossing snow, wide shot"
                                           :verb "generate"))))
        (multiple-value-bind (prompt options) (command-request line)
          (if (zerop (length prompt))
              (values "Nothing to render: give /video a prompt." nil)
              (handler-case
                  (let ((answer (render prompt
                                        :model (getf options :model)
                                        :image (getf options :image)
                                        :references (getf options :references)
                                        :duration (getf options :duration)
                                        :resolution (getf options :resolution)
                                        :wait-seconds (getf options :wait))))
                    (if (nlk:json-value answer :string "video")
                        (values (format nil "~a~@[ · ~a~]~@[ · ~ds~]~@[ · ~a~]"
                                        (nlk:json-value answer :string "video")
                                        (nlk:json-value answer :string "model")
                                        (nlk:json-value answer :integer "duration")
                                        (nlk:json-value answer :string "resolution"))
                                nil)
                        (values (format nil "~a · task ~a · ~a"
                                        (nlk:json-value answer :string "model")
                                        (nlk:json-value answer :string "task_id")
                                        (nlk:json-value answer :string "status"))
                                nil)))
                (minimax-video-error (condition)
                  (values (format nil "ERROR: ~a" (minimax-video-error-detail condition))
                          nil))
                (error (condition)
                  (values (format nil "ERROR: minimax-video: ~a"
                                  (nlk:one-line (princ-to-string condition)))
                          nil))))))))

;;; --- the entry: the one declaration -----------------------------------------------

(nle:define-cell minimax-video
  (:section ("minimax-video")
    (:guide "paste a MiniMax platform key into api_key, or name the variable holding it in api_key_env (MINIMAX_PAYG_KEY unless you say otherwise); next step: ask for a video, or /video a red fox in snow")
    ("api" :string :default "https://api.minimax.io"
     :doc "the MiniMax API root: https://api.minimax.io unless your account is on another region")
    ("api_key" :secret
     :doc "the MiniMax platform key, pasted here instead of naming a variable")
    ("api_key_env" :env :default "MINIMAX_PAYG_KEY"
     :doc "the environment variable holding the MiniMax platform key")
    ("model" :string :default "MiniMax-Hailuo-2.3"
     :doc "the model text-to-video and image-to-video use: MiniMax-Hailuo-2.3, MiniMax-Hailuo-02, T2V-01, I2V-01...")
    ("model_s2v" :string :default "S2V-01"
     :doc "the model subject-reference-to-video uses")
    ("resolution" :choice :options '("auto" "512P" "720P" "768P" "1080P") :default "auto"
     :doc "the video's resolution; auto picks 768P on the Hailuo models and 720P on the older ones")
    ("prompt_optimizer" :boolean :default t
     :doc "let MiniMax rewrite the prompt before rendering")
    ("output_dir" :path :default "~/.nodecode/videos"
     :doc "where downloaded videos are written")
    ("wait_seconds" :integer :default 600 :min 0
     :doc "how long a call waits for the render before answering the task id; 0 answers at once")
    ("poll_seconds" :integer :default 5 :min 1
     :doc "how long between two checks of a running render")
    ("request_timeout" :integer :default 120 :min 5
     :doc "one HTTP request's ceiling, in seconds"))
  (:settings #'read-settings)
  (:help :minimax-video
         "minimax-video: generate_video makes a video with MiniMax's Hailuo models; video_status finishes a render that is still running"
         *manual*)
  (:tool "generate_video" (nlk:contract 'video-request) #'generate-video)
  (:tool "video_status" (nlk:contract 'video-task) #'video-status)
  (:command "video" 'video-command
            :description "Generate a video with MiniMax's Hailuo models"
            :argument-hint "PROMPT"))
