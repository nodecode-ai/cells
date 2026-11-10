;;;; cell-test.lisp --- lifecycle, the two tools, the verb, /gmi-video.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, integer fields under their floors refused, a start with no key
;;;; refused), each tool through the seam the model calls it by -- the url, the
;;;; Bearer header, the body, the answer -- the eval verb, and /gmi-video.
;;;; Every request is a stubbed NLK:HTTP and every job a stub's answer, so
;;;; nothing reaches GMI and no video costs anything.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS =>
(values BODY STATUS).")

(defmacro with-gmi-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials GMI."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected GMI request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defmacro with-gmi ((&rest routes) &body body)
  "BODY with the stubbed wire answering ROUTES, (METHOD FRAGMENT FORM): the
first whose METHOD and URL FRAGMENT match answers FORM's values; any other
request is an error."
  `(with-gmi-wire
     (let ((*answer-fn* (lambda (method url args)
                           (declare (ignore args))
                           (cond ,@(loop for (wanted fragment form) in routes
                                         collect `((and (eq method ,wanted) (search ,fragment url))
                                                   ,form))
                                 (t (error "unexpected GMI request: ~a ~a" method url))))))
       ,@body)))

(defmacro with-gmi-cell ((dir &rest config) &body body)
  "BODY with the cell started on CONFIG -- a key and a zero poll unless BODY
says otherwise -- and its videos written under a scratch DIR, removed after."
  `(let* ((,dir (merge-pathnames (format nil "gmi-video-test-~36r/"
                                          (random (expt 36 6) (make-random-state t)))
                                  (uiop:temporary-directory)))
          (nodecode-gmi-video::*catalog* nil))
     (ensure-directories-exist ,dir)
     (unwind-protect
          (with-cell-stop ((gmi-video-start "api_key" "k-test" "poll_seconds" 0
                                            "output_dir" (namestring ,dir) ,@config))
            ,@body)
       (ignore-errors (uiop:delete-directory-tree ,dir :validate t)))))

(defun gmi-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun gmi-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun gmi-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun gmi-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun gmi-answer (text)
  "TEXT as the one string a tool or command answers."
  (if (stringp text) text (princ-to-string text)))

;;; --- the fixtures GMI's catalog answers ------------------------------------------

(defparameter +text-record+
  "{\"model\":\"Minimax-Hailuo-02\",\"status\":\"active\",\"qworkers\":2,
    \"price_info\":{\"price\":40,\"unit\":\"second\"},
    \"brief_description\":\"Text to video, 6 to 10 seconds.\",
    \"modalities\":{\"input\":[{\"type\":\"text\",\"required\":true}],
                    \"output\":[{\"type\":\"video\"}]},
    \"parameters\":[
      {\"name\":\"prompt\",\"type\":\"string\",\"required\":true},
      {\"name\":\"duration\",\"type\":\"integer\",\"min_value\":3,\"max_value\":10,\"default_value\":5},
      {\"name\":\"aspect_ratio\",\"type\":\"string\",
       \"options\":[{\"value\":\"16:9\"},{\"value\":\"9:16\"},{\"value\":\"1:1\"}]},
      {\"name\":\"resolution\",\"type\":\"string\",
       \"options\":[{\"value\":\"720p\"},{\"value\":\"1080p\"}]},
      {\"name\":\"generateAudio\",\"type\":\"string\",
       \"options\":[{\"value\":\"on\"},{\"value\":\"off\"}]},
      {\"name\":\"seed\",\"type\":\"integer\"}]}"
  "A text-to-video model's live schema, as GET models/<id> answers one.")

(defparameter +image-record+
  "{\"model\":\"Minimax-Hailuo-2.3-Fast\",\"status\":\"active\",\"qworkers\":4,
    \"price_info\":{\"price\":55,\"unit\":\"second\"},
    \"modalities\":{\"input\":[{\"type\":\"text\",\"required\":true},
                               {\"type\":\"image\",\"required\":true}],
                    \"output\":[{\"type\":\"video\"}]},
    \"parameters\":[
      {\"name\":\"prompt\",\"type\":\"string\",\"required\":true},
      {\"name\":\"image\",\"type\":\"string\",\"required\":true},
      {\"name\":\"duration\",\"type\":\"string\",\"options\":[{\"value\":\"5\"},{\"value\":\"10\"}]},
      {\"name\":\"size\",\"type\":\"string\",
       \"options\":[{\"value\":\"1280x720\"},{\"value\":\"720x1280\"},{\"value\":\"1024x1024\"}]}]}"
  "An image-to-video model's live schema: a size instead of an aspect.")

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "gmi-video" "GMI-VIDEO-CELL-" :start nodecode-gmi-video:start-cell)

(define-cell-lifecycle-tests "gmi-video"
  (:fixture with-gmi-wire)
  (:config "api_key" "k-test")
  (:help :gmi-video)
  (:command "gmi-video")
  (:running (is (nle::find-tool "gmi_video") "gmi_video is registered")
            (is (nle::find-tool "gmi_video_models"))
            (is (= 900 (nodecode-gmi-video::setting :timeout-seconds)) "15 minutes to a job")
            (is (equal "https://console.gmicloud.ai/api/v1/ie/requestqueue/apikey"
                       (nodecode-gmi-video::base-url))))
  (:stopped (is (null (nle::find-tool "gmi_video")) "stop unregisters the tools"))
  (:refused ("timeout_seconds" 5) ("poll_seconds" -1))
  (:idle gmi-video:gmi-video-error (gmi-video:generate "a boat") (gmi-video:models)))

(deftest gmi-video-cell-a-key-is-required ()
  (with-gmi-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignorable name)) nil)
      (is-carrying (message (refusal-text nlk:config-refusal (gmi-video-start)))
                   ("gmi-video: no key" "GMI_API_KEY"))
      (is (null (nle::find-tool "gmi_video")) "a refused start installed nothing")
      (is (null (nle::find-registered-command "gmi-video")) "and registered no command"))))

;;; --- the tools ----------------------------------------------------------------

(deftest gmi-video-cell-a-text-prompt-makes-a-video ()
  (let ((polls 0))
    (with-gmi-cell (dir)
      (with-gmi ((:get "models/Minimax-Hailuo-02" (values +text-record+ 200))
                 (:post "requests" (values "{\"request_id\":\"req-1\"}" 200))
                 (:get "requests/req-1"
                       (progn (incf polls)
                              (values (if (= polls 1)
                                          "{\"status\":\"pending\"}"
                                          "{\"status\":\"success\",\"outcome\":{\"video_url\":\"https://cdn.gmi.example/v/req-1.mp4\"}}")
                                      200)))
                 (:get "cdn.gmi.example/v/req-1.mp4"
                       (values (make-array 4 :element-type '(unsigned-byte 8)
                                             :initial-contents '(77 80 52 33))
                               200)))
        (let ((answer (gmi-answer (gmi-tool "gmi_video"
                                             (nlk:json-object "prompt" "a blue boat at dusk"
                                                              "duration" 12
                                                              "resolution" "480p"
                                                              "audio" t)))))
          (is-carrying answer
            ("gmi-video: text-to-video made a 10 s video with Minimax-Hailuo-02")
            ("Saved to") ("Request req-1") ("Price: 40 micro-dollars / second"))
          (is (= 2 polls) "a pending job is polled until it is done")
          (let ((post (find :post (gmi-asked) :key #'first)))
            (is (equal "Bearer k-test" (gmi-request-header post "authorization")) "the key rides the header")
            (let ((body (gmi-request-body post)))
              (is (equal "Minimax-Hailuo-02" (nlk:json-value body :string "model")))
              (is (equal "a blue boat at dusk" (nlk:json-value body :string "payload" "prompt")))
              (is (= 10 (nlk:json-value body :integer "payload" "duration")) "12 clamped to the model's 10")
              (is (equal "720p" (nlk:json-value body :string "payload" "resolution")) "480p reads as the nearest the model takes")
              (is (equal "on" (nlk:json-value body :string "payload" "generateAudio")) "true is the model's on")))
          (let ((files (uiop:directory-files dir)))
            (is (= 1 (length files)) "the video landed in the output directory")
            (with-open-file (in (first files) :element-type '(unsigned-byte 8))
              (is (equal "MP4!" (map 'string #'code-char
                                      (let ((octets (make-array 4 :element-type '(unsigned-byte 8))))
                                        (read-sequence octets in)
                                        octets)))))))))))

(deftest gmi-video-cell-an-image-url-uses-the-image-model-and-a-size ()
  (with-gmi-cell (dir)
    (with-gmi ((:get "models/Minimax-Hailuo-2.3-Fast" (values +image-record+ 200))
               (:post "requests" (values "{\"request_id\":\"req-2\"}" 200))
               (:get "requests/req-2"
                     (values "{\"status\":\"completed\",\"outcome\":{\"media_urls\":[{\"url\":\"https://cdn.gmi.example/v/req-2.mp4\"}]}}"
                             200))
               (:get "cdn.gmi.example/v/req-2.mp4"
                     (values (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3)) 200)))
      (let ((answer (gmi-answer (gmi-tool "gmi_video"
                                           (nlk:json-object "prompt" "the boat moves"
                                                            "image_url" "https://images.example/boat.png"
                                                            "aspect_ratio" "portrait"
                                                            "resolution" "1080p")))))
        (is-carrying answer
          ("gmi-video: image-to-video") ("Minimax-Hailuo-2.3-Fast")
          ("Dropped: resolution" "the option this model lacks is reported"))
        (let* ((post (find :post (gmi-asked) :key #'first))
               (payload (nlk:json-value (gmi-request-body post) :object "payload")))
          (is (equal "https://images.example/boat.png" (nlk:json-value payload :string "image")))
          (is (equal "720x1280" (nlk:json-value payload :string "size")) "portrait picks the portrait size")
          (is (not (nth-value 1 (gethash "resolution" payload))) "no resolution parameter was sent"))))))

(deftest gmi-video-cell-the-models-tool-lists-what-is-usable ()
  (with-gmi-cell (dir)
    (with-gmi ((:get "models/Minimax-Hailuo-02" (values +text-record+ 200))
               (:get "models" (values "{\"model_ids\":[\"Minimax-Hailuo-02\",\"11111111-1111-1111-1111-111111111111\",\"capture-probe\"]}" 200)))
      (let ((answer (gmi-answer (gmi-tool "gmi_video_models" (nlk:json-object)))))
        (is-carrying answer
          ("Minimax-Hailuo-02") ("40 micro-dollars / second") ("Text to video"))
        (let ((fetches (count "models/" (gmi-asked) :test #'search :key #'second)))
          (is (= 1 fetches) "a UUID and a probe model are never fetched"))))))

(deftest gmi-video-cell-a-failed-job-is-the-model-repair ()
  (with-gmi-cell (dir)
    (with-gmi ((:get "models/Minimax-Hailuo-02" (values +text-record+ 200))
               (:post "requests" (values "{\"request_id\":\"req-3\"}" 200))
               (:get "requests/req-3" (values "{\"status\":\"failed\",\"error\":\"prompt rejected\"}" 200)))
      (is-carrying (message (gmi-tool "gmi_video" (nlk:json-object "prompt" "a boat")))
        ("ERROR: gmi-video:") ("GMI job failed: prompt rejected")))))

;;; --- the eval verb and the command -----------------------------------------------

(deftest gmi-video-cell-the-verb-answers-the-same-call ()
  (with-gmi-cell (dir)
    (with-gmi ((:get "models/Minimax-Hailuo-02" (values +text-record+ 200))
               (:post "requests" (values "{\"request_id\":\"req-4\"}" 200))
               (:get "requests/req-4"
                     (values "{\"status\":\"success\",\"outcome\":{\"video_url\":\"https://cdn.gmi.example/v/req-4.mp4\"}}"
                             200))
               (:get "cdn.gmi.example/v/req-4.mp4"
                     (values (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(9 9)) 200)))
      (is-carrying (message (gmi-video:generate "a boat" :duration 5))
        ("gmi-video: text-to-video made a 5 s video with Minimax-Hailuo-02")
        ("Saved to")))))

(deftest gmi-video-cell-the-command-lists-the-models ()
  (with-gmi-cell (dir)
    (with-gmi ((:get "models/Minimax-Hailuo-02" (values +text-record+ 200))
               (:get "models" (values "{\"model_ids\":[\"Minimax-Hailuo-02\"]}" 200)))
      (is-carrying (message (cell-entry "nodecode-gmi-video" "gmi-video" ""))
        ("GMI Cloud video models") ("Minimax-Hailuo-02")
        ("Defaults: text-to-video Minimax-Hailuo-02, image-to-video Minimax-Hailuo-2.3-Fast")))))
