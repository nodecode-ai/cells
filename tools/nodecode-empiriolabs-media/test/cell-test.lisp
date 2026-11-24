;;;; cell-test.lisp --- lifecycle, the three tools, /empiriolabs-media, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start with no key, a member under its floor or a root without a scheme
;;;; refused, an api_key_env naming an empty variable refused), the three
;;;; tools through the seam the model calls them by -- their URL, their
;;;; headers, their body and the answer they build -- a job polled to its
;;;; output, a local source image inlined as a data URI, the file written
;;;; where the answer says it is, and /empiriolabs-media with and without an
;;;; argument. Every request is a stubbed NLK:HTTP and every variable read is
;;;; stubbed UIOP:GETENV, so nothing reaches EmpirioLabs AI, the network or
;;;; the machine's own environment.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *media-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *media-answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS =>
(values BODY STATUS), BODY a JSON text or the octets a download answers.")

(defmacro with-media-wire (&body body)
  "BODY with NLK:HTTP stubbed and UIOP:GETENV answering nothing, so no test
dials EmpirioLabs AI and a key in the machine's environment cannot reach one."
  `(let ((*media-requests* '())
         (*media-answer-fn* (lambda (method url args)
                               (declare (ignore method url args))
                               (error "unexpected request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *media-requests*)
                                (funcall *media-answer-fn* method url args))
       (with-stubbed-fdefinition (uiop:getenv (name)
                                  (declare (ignore name))
                                  nil)
         ,@body))))

(defun media-asked ()
  "The stubbed requests, oldest first."
  (reverse *media-requests*))

(defmacro media-answers (&rest routes)
  "Answer stubbed requests by the first ROUTE whose fragment is in the URL;
a route is (FRAGMENT BODY &optional STATUS), BODY evaluated for its value."
  `(setf *media-answer-fn*
         (lambda (method url args)
           (declare (ignore method args))
           (let ((route (find-if (lambda (route) (search (first route) url))
                                  (list ,@(loop for (fragment body &optional status) in routes
                                                collect `(list ,fragment ,body ,status))))))
             (unless route (error "unexpected request to ~a" url))
             (values (second route) (or (third route) 200))))))

(defun media-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun media-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun media-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

(defun media-saved (text)
  "The path an answer names after `saved at', to the end of its line."
  (let* ((at (search "saved at " text))
         (rest (and at (subseq text (+ at 9)))))
    (and rest (nlk:trimmed (subseq rest 0 (position #\Newline rest))))))

(defun media-octets (&rest values)
  "VALUES as the octet vector cl-base64 and the wire take."
  (coerce values '(vector (unsigned-byte 8))))

(defun media-bytes (path)
  "PATH's octets."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "empiriolabs-media" "EMPIRIOLABS-MEDIA-CELL-"
  :start nodecode-empiriolabs-media:start-cell)

(define-cell-lifecycle-tests "empiriolabs-media"
  (:fixture with-media-wire)
  (:help :empiriolabs-media)
  (:command "empiriolabs-media")
  (:running
   (is (nle::find-tool "empiriolabs_image") "empiriolabs_image is registered")
   (is (nle::find-tool "empiriolabs_video") "empiriolabs_video is registered")
   (is (nle::find-tool "empiriolabs_speech") "empiriolabs_speech is registered")
   (is (equal "https://api.empiriolabs.ai/v1" (nodecode-empiriolabs-media::setting :base-url)) "the section's default root")
   (is (equal "flux-2-klein-4b" (nodecode-empiriolabs-media::setting :image-model)) "the section's default image model")
   (is (null (nodecode-empiriolabs-media::setting :key)) "no key: the cell still starts"))
  (:stopped
   (is (null (nle::find-tool "empiriolabs_image")) "stop unregisters the tools")
   (is (null (nle::find-tool "empiriolabs_speech")) "all of them"))
  (:refused ("timeout_seconds" 0) ("video_timeout_seconds" 0)
            ("base_url" "api.empiriolabs.ai/v1")
            ("api_key_env" "EMPIRIOLABS_UNSET_TEST_KEY"))
  (:idle nodecode-empiriolabs-media:empiriolabs-media-error
         (nodecode-empiriolabs-media::run-command "" nil)))

;;; --- the image ----------------------------------------------------------------

(deftest empiriolabs-media-cell-an-image-is-saved-where-the-answer-says ()
  (with-media-wire
    (with-cell-stop ((empiriolabs-media-start "api_key" "k-test"))
      (media-answers ("/images/generations" "{\"data\":[{\"url\":\"https://files.example/out.png\"}]}")
                      ("files.example" #(137 80 78 71 13 10 26 10 1 2 3)))
      (let* ((text (media-tool "empiriolabs_image"
                                (nlk:json-object "prompt" "a red fox in snow"
                                                 "aspect_ratio" "landscape")))
             (path (media-saved text)))
        (is (search "image (flux-2-klein-4b):" text) text)
        (is (and path (plusp (length path))) "the answer names the file it saved")
        (when (and path (probe-file path))
          (is (equalp #(137 80 78 71 13 10 26 10 1 2 3) (media-bytes path)) "the file holds the octets the download answered"))
        (let ((request (first (media-asked))))
          (is (eq :post (first request)))
          (is (equal "https://api.empiriolabs.ai/v1/images/generations" (second request)))
          (is (equal "Bearer k-test" (media-request-header request "authorization")))
          (let ((body (media-request-body request)))
            (is (equal "a red fox in snow" (nlk:json-value body :string "prompt")))
            (is (equal "flux-2-klein-4b" (nlk:json-value body :string "model")))
            (is (equal "16:9" (nlk:json-value body :string "aspect_ratio")) "landscape is the original's first ratio")))
        (let ((download (second (media-asked))))
          (is (equal "https://files.example/out.png" (second download)))
          (is (null (media-request-header download "authorization")) "the key never rides a download"))))))

(deftest empiriolabs-media-cell-an-edit-inlines-a-local-image ()
  (with-media-wire
    (with-cell-stop ((empiriolabs-media-start "api_key" "k-test"))
      (let ((source (merge-pathnames (format nil "media-source-~d.png" (get-universal-time))
                                     (uiop:temporary-directory)))
            (answer (format nil "{\"data\":[{\"b64_json\":\"~a\"}]}"
                            (cl-base64:usb8-array-to-base64-string
                             (media-octets 137 80 78 71 13 10 26 10 9 9 9)))))
        (with-open-file (out source :direction :output :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
          (write-sequence #(137 80 78 71 13 10 26 10 7 7 7) out))
        (unwind-protect
             (progn
               (media-answers ("/images/generations" answer))
               (let* ((text (media-tool "empiriolabs_image"
                                         (nlk:json-object "prompt" "make it night"
                                                           "image" (namestring source))))
                      (body (media-request-body (first (media-asked))))
                      (images (nlk:json-value body :array "image"))
                      (path (media-saved text)))
                 (is (= 1 (length images)) "one source image")
                 (is (uiop:string-prefix-p "data:image/png;base64," (aref images 0)) "a local file is inlined as a data URI")
                 (is (search "edited from a local image" text))
                 (is (and path (probe-file path)) "the base64 answer is written whole")
                 (when (and path (probe-file path))
                   (is (equalp #(137 80 78 71 13 10 26 10 9 9 9) (media-bytes path))))))
          (ignore-errors (delete-file source)))))))

;;; --- the video ----------------------------------------------------------------

(deftest empiriolabs-media-cell-a-video-job-is-polled-to-its-runtime ()
  (with-media-wire
    (with-cell-stop ((empiriolabs-media-start "api_key" "k-test" "video_timeout_seconds" 60))
      (let ((nodecode-empiriolabs-media::*poll-seconds* 0)
            (polls 0))
        (setf *media-answer-fn*
              (lambda (method url args)
                (declare (ignore method args))
                (cond ((search "/videos/generations" url)
                       (values "{\"job_id\":\"job-7\",\"status\":\"queued\"}" 200))
                      ((search "/jobs/job-7" url)
                       (if (= 1 (incf polls))
                           (values "{\"status\":\"running\"}" 200)
                           (values "{\"status\":\"completed\",\"data\":[{\"url\":\"https://files.example/clip.mp4\"}]}" 200)))
                      ((search "files.example" url) (values #(0 0 0 24 102 116 121 112) 200))
                      (t (error "unexpected request to ~a" url)))))
        (let* ((text (media-tool "empiriolabs_video"
                                  (nlk:json-object "prompt" "a kite over a beach" "duration" 4)))
               (path (media-saved text)))
          (is (search "video (hunyuan-video-1-5, job job-7)" text) text)
          (is (and path (search ".mp4" path)) "the video keeps its extension")
          (is (= 4 (length (media-asked))) "one POST, two polls, one download")
          (let ((poll (second (media-asked))))
            (is (eq :get (first poll)))
            (is (equal "https://api.empiriolabs.ai/v1/jobs/job-7" (second poll)))
            (is (equal "Bearer k-test" (media-request-header poll "authorization")) "a job poll carries the key"))
          (let ((body (media-request-body (first (media-asked)))))
            (is (= 4 (nlk:json-value body :integer "duration")))
            (is (equal "a kite over a beach" (nlk:json-value body :string "prompt")))))))))

;;; --- the speech ---------------------------------------------------------------

(deftest empiriolabs-media-cell-speech-writes-the-audio-it-was-given ()
  (with-media-wire
    (with-cell-stop ((empiriolabs-media-start "api_key" "k-test" "voice" "Sarah"))
      (let ((mp3 (media-octets 73 68 51 4 0 0 0 0 0 0)))
        (media-answers ("/audio/speech"
                         (format nil "{\"data\":[{\"b64_json\":\"~a\"}]}"
                                 (cl-base64:usb8-array-to-base64-string mp3))))
        (let* ((text (media-tool "empiriolabs_speech" (nlk:json-object "text" "hello there")))
               (path (media-saved text)))
          (is (search "speech (tts-2-flash, voice Sarah) saved at" text) text)
          (is (and path (search ".mp3" path)) "the audio is an mp3")
          (when (and path (probe-file path))
            (is (equalp mp3 (media-bytes path)) "and holds the audio the API answered"))
          (let ((body (media-request-body (first (media-asked)))))
            (is (equal "hello there" (nlk:json-value body :string "input")))
            (is (equal "tts-2-flash" (nlk:json-value body :string "model")))
            (is (equal "https://api.empiriolabs.ai/v1/audio/speech" (second (first (media-asked)))))
            (is (equal "Sarah" (nlk:json-value body :string "voice")) "the section's voice answers")))))))

;;; --- the key, and the catalog -------------------------------------------------

(deftest empiriolabs-media-cell-without-a-key-a-verb-says-where-one-comes-from ()
  (with-media-wire
    (with-cell-stop ((empiriolabs-media-start))
      (is-carrying (answer (refusal-text nodecode-empiriolabs-media:empiriolabs-media-error
                                          (nodecode-empiriolabs-media:image "a fox")))
                   ("EMPIRIOLABS_API_KEY" "the refusal names the variable")
                   ("platform.empiriolabs.ai" "and where to make a key"))
      (is (null (media-asked)) "no request was made"))
    (with-cell-stop ((empiriolabs-media-start "api_key" "k-test"))
      (is-carrying (answer (refusal-text nodecode-empiriolabs-media:empiriolabs-media-error
                                          (nodecode-empiriolabs-media:speech "  ")))
                   ("speech needs text" "an empty text refuses once the key is there"))
      (is (null (media-asked)) "and nothing was sent"))))

(deftest empiriolabs-media-cell-the-command-reads-the-catalog ()
  (with-media-wire
    (with-cell-stop ((empiriolabs-media-start "api_key" "k-test"))
      (media-answers ("/models" "{\"data\":[
         {\"id\":\"flux-2-klein-4b\",\"category\":\"image\",\"is_featured\":true,
          \"supported_endpoints\":[\"POST /v1/images/generations\"]},
         {\"id\":\"hunyuan-video-1-5\",\"category\":\"video\",
          \"supported_endpoints\":[\"POST /v1/videos/generations\"]}]}"))
      (let ((text (cell-entry "nodecode-empiriolabs-media" "empiriolabs-media" "models image")))
        (is (search "1 EmpirioLabs AI model for image:" text) text)
        (is (search "flux-2-klein-4b (image) [featured]" text) text)
        (is (null (search "hunyuan" text)) "a video model is not an image one"))
      (let ((status (cell-entry "nodecode-empiriolabs-media" "empiriolabs-media" "")))
        (is (search "EmpirioLabs AI media at https://api.empiriolabs.ai/v1" status) status)
        (is (search "key set" status) "the status line says a key is there")))))
