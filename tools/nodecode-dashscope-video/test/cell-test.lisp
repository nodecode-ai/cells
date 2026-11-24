;;;; cell-test.lisp --- lifecycle, video_generate, the verb, /dashscope-video.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, integer fields under their floors refused, a start with no key
;;;; refused), the tool through the seam the model calls it by -- its url, its
;;;; Bearer header, its async header, its body and its answer -- the mode each
;;;; shape of input picks, the verb, and /dashscope-video. Every request is a
;;;; stubbed NLK:HTTP and every wait a stubbed clock and pause, so nothing
;;;; reaches DashScope and no test sleeps.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values BODY STATUS).")

(defmacro with-dsv-wire (&body body)
  "BODY with NLK:HTTP stubbed -- a request is logged in *REQUESTS* and
answered by *ANSWER-FN* -- and the cell's clock and pause stubbed, so a poll
neither sleeps nor counts real seconds."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected dashscope request")))
         (nodecode-dashscope-video::*pause* (lambda (seconds) (declare (ignore seconds)) nil))
         (nodecode-dashscope-video::*clock* (let ((seconds 0.0))
                                              (lambda () (incf seconds 1.0)))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defmacro with-dsv-answers ((&rest routes) &body body)
  "BODY with the cell started on a test key and the stubbed wire answering
ROUTES, (FRAGMENT FORM): the first whose FRAGMENT the url holds answers
FORM's (values BODY STATUS); a url no route names is an error."
  `(with-dsv-wire
     (let ((*answer-fn* (lambda (method url args)
                           (declare (ignore method args))
                           (cond ,@(loop for (fragment form) in routes
                                         collect `((search ,fragment url) ,form))
                                 (t (error "unexpected request to ~a" url))))))
       (with-cell-stop ((dashscope-video-start "api_key" "sk-sp-test"))
         ,@body))))

(defun dsv-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun dsv-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun dsv-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun dsv-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun dsv-answer-json (text)
  "A tool TEXT answer, decoded."
  (nlk:decode-json text))

(defparameter +dsv-submit+ (format nil "{\"output\":{\"task_id\":\"task-1\"}}")
  "What a submit answers.")

(defun dsv-finished (url)
  "What a poll answers for a task that produced URL."
  (format nil "{\"output\":{\"task_status\":\"SUCCEEDED\",\"video_url\":\"~a\"}}" url))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "dashscope-video" "DASHSCOPE-VIDEO-CELL-" :start nodecode-dashscope-video:start-cell)

(define-cell-lifecycle-tests "dashscope-video"
  (:fixture with-dsv-wire)
  (:config "api_key" "sk-sp-test")
  (:help :dashscope-video)
  (:command "dashscope-video")
  (:running (is (nle::find-tool "video_generate") "video_generate is registered")
            (is (equal "https://token-plan.ap-southeast-1.maas.aliyuncs.com"
                       (nodecode-dashscope-video::api-root)) "the original's token-plan base")
            (is (equal "sk-sp-test" (nodecode-dashscope-video::api-key)))
            (is (equal "happyhorse-1.1-t2v" (nodecode-dashscope-video::resolved-model "t2v" nil)) "the family plus the mode's suffix")
            (is (equal "1280*720" (nodecode-dashscope-video::size-for "16:9"))))
  (:stopped (is (null (nle::find-tool "video_generate")) "stop unregisters the tool"))
  (:refused ("duration" 0) ("poll_interval" 0) ("poll_deadline" 5) ("max_video_mb" 0))
  (:idle dashscope-video:dashscope-video-error (dashscope-video:generate "x")))

(deftest dashscope-video-cell-a-key-is-required ()
  (with-dsv-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignore name)) nil)
      (is-carrying (said (refusal-text nlk:config-refusal (dashscope-video-start)))
        ("dashscope-video: no key") ("QWEN_API_KEY") ("api_key"))
      (is (null (nle::find-tool "video_generate")) "a refused start installed nothing"))))

(deftest dashscope-video-cell-a-key-in-the-environment-is-enough ()
  (with-dsv-wire
    (with-stubbed-fdefinition (uiop:getenv (name)
                                (if (equal name "QWEN_API_KEY") "sk-sp-env" nil))
      (with-cell-stop ((dashscope-video-start))
        (is (equal "sk-sp-env" (nodecode-dashscope-video::api-key)) "the variable key_env names")
        (is (nle::find-tool "video_generate"))))))

;;; --- video_generate ------------------------------------------------------------

(deftest dashscope-video-cell-text-to-video-submits-polls-and-saves ()
  (with-dsv-answers (("/video-synthesis" (values +dsv-submit+ 200))
                     ("/api/v1/tasks/task-1" (values (dsv-finished "https://oss.example.com/clips/a.mp4") 200))
                     ("https://oss.example.com/clips/a.mp4" (values (octets 0 1 2 3) 200)))
    (let ((made (dsv-answer-json (dsv-tool "video_generate"
                                            (nlk:json-object "prompt" "a lighthouse in fog")))))
      (is (equal "t2v" (nlk:json-value made :string "mode")))
      (is (equal "happyhorse-1.1-t2v" (nlk:json-value made :string "model")))
      (is (= 5 (nlk:json-value made :integer "duration")) "the section's default length")
      (is (equal "1280*720" (nlk:json-value made :string "size")))
      (let ((path (nlk:json-value made :string "path")))
        (is (probe-file path) "the clip is on disk")
        (is (equal '(0 1 2 3)
                   (with-open-file (in path :element-type '(unsigned-byte 8))
                     (loop for byte = (read-byte in nil nil) while byte collect byte))) "the bytes the wire answered")))
    (let ((asks (dsv-asked)))
      (is (= 3 (length asks)) "submit, poll, download")
      (destructuring-bind (method url . args) (first asks)
        (declare (ignore args))
        (is (eq :post method))
        (is (search "/api/v1/services/aigc/video-generation/video-synthesis" url))
        (is (equal "enable" (dsv-request-header (first asks) "x-dashscope-async")) "the async header the video models need")
        (is (equal "Bearer sk-sp-test" (dsv-request-header (first asks) "authorization"))))
      (let ((body (dsv-request-body (first asks))))
        (is (equal "a lighthouse in fog" (nlk:json-value body :string "input" "prompt")))
        (is (= 5 (nlk:json-value body :integer "parameters" "duration")))
        (is (equal "1280*720" (nlk:json-value body :string "parameters" "size"))))
      (is (equal '(:get :get) (map 'list #'first (rest asks))))
      (is (equal "Bearer sk-sp-test" (dsv-request-header (second asks) "authorization")) "the poll carries the key too")
      (is (eq t (getf (cddr (third asks) ) :binary)) "the clip is fetched as octets"))))

(deftest dashscope-video-cell-image-to-video-inlines-a-local-frame ()
  (let ((frame (nlk:home "frames/frame.png")))
    (ensure-directories-exist frame)
    (with-open-file (out frame :direction :output :if-exists :supersede
                               :if-does-not-exist :create :element-type '(unsigned-byte 8))
      (write-sequence (octets 137 80 78 71) out))
    (with-dsv-answers (("/video-synthesis" (values +dsv-submit+ 200))
                       ("/api/v1/tasks/task-1" (values (dsv-finished "https://oss.example.com/b.mp4") 200))
                       ("https://oss.example.com/b.mp4" (values (octets 9 9) 200)))
      (let ((made (dsv-answer-json (dsv-tool "video_generate"
                                              (nlk:json-object "prompt" "the frame moves"
                                                               "image_url" (namestring frame))))))
        (is (equal "i2v" (nlk:json-value made :string "mode")))
        (is (equal "happyhorse-1.1-i2v" (nlk:json-value made :string "model"))))
      (let ((body (dsv-request-body (first (dsv-asked)))))
        (is (equal "data:image/png;base64,iVBORw=="
                   (nlk:json-value body :string "input" "image_url")) "a local path rides as a png data uri")))))

(deftest dashscope-video-cell-reference-images-route-to-r2v ()
  (with-dsv-answers (("/video-synthesis" (values +dsv-submit+ 200))
                     ("/api/v1/tasks/task-1" (values (dsv-finished "https://oss.example.com/c.mp4") 200))
                     ("https://oss.example.com/c.mp4" (values (octets 7) 200)))
    (let ((made (dsv-answer-json
                 (dsv-tool "video_generate"
                           (nlk:json-object "prompt" "she walks on"
                                            "reference_image_urls"
                                            (vector "https://example.com/ref.png" "https://example.com/two.png"))))))
      (is (equal "r2v" (nlk:json-value made :string "mode")))
      (is (equal "happyhorse-1.1-r2v" (nlk:json-value made :string "model"))))
    (let ((body (dsv-request-body (first (dsv-asked)))))
      (is (equal "https://example.com/ref.png" (nlk:json-value body :string "input" "ref_image_url")) "the first reference, and only it")
      (is (null (nlk:json-value body :string "input" "image_url")) "i2v's field is not also sent"))))

(deftest dashscope-video-cell-an-explicit-model-and-a-long-clip-are-honoured ()
  (with-dsv-answers (("/video-synthesis" (values +dsv-submit+ 200))
                     ("/api/v1/tasks/task-1" (values (dsv-finished "https://oss.example.com/d.mp4") 200))
                     ("https://oss.example.com/d.mp4" (values (octets 7) 200)))
    (dsv-tool "video_generate" (nlk:json-object "prompt" "a kite"
                                                 "model" "happyhorse-1.1-t2v-cinema"
                                                 "aspect_ratio" "9:16"
                                                 "duration" 99))
    (let ((body (dsv-request-body (first (dsv-asked)))))
      (is (equal "happyhorse-1.1-t2v-cinema" (nlk:json-value body :string "model")) "an explicit id wins over the family")
      (is (= 10 (nlk:json-value body :integer "parameters" "duration")) "99 is clamped to 10")
      (is (equal "720*1280" (nlk:json-value body :string "parameters" "size")) "9:16 in pixels"))))

(deftest dashscope-video-cell-a-failed-task-names-its-status ()
  (with-dsv-answers (("/video-synthesis" (values +dsv-submit+ 200))
                     ("/api/v1/tasks/task-1"
                      (values "{\"output\":{\"task_status\":\"FAILED\",\"message\":\"content moderated\"}}" 200)))
    (is-carrying (said (refusal-text dashscope-video:dashscope-video-error
                                     (dashscope-video:generate "a lighthouse")))
      ("task task-1 ended FAILED") ("content moderated"))))

(deftest dashscope-video-cell-a-refused-submit-is-the-repair-line ()
  (with-dsv-answers (("/video-synthesis"
                      (values "{\"code\":\"InvalidApiKey\",\"message\":\"Invalid API-key provided.\"}" 401)))
    (is-carrying (said (refusal-text dashscope-video:dashscope-video-error
                                     (dashscope-video:generate "a lighthouse")))
      ("HTTP 401") ("Invalid API-key provided."))))

(deftest dashscope-video-cell-a-task-that-never-finishes-gives-up ()
  (with-dsv-answers (("/video-synthesis" (values +dsv-submit+ 200)))
    (let ((nodecode-dashscope-video::*clock* (let ((calls 0))
                                               (lambda () (incf calls 1000.0)))))
      (is-carrying (said (refusal-text dashscope-video:dashscope-video-error
                                       (dashscope-video:generate "a lighthouse")))
        ("still PENDING") ("poll_deadline"))
      (is (= 1 (length (dsv-asked))) "no poll was spent before the deadline"))))

;;; --- the verb, and /dashscope-video ---------------------------------------------

(deftest dashscope-video-cell-the-verb-answers-a-plist ()
  (with-dsv-answers (("/video-synthesis" (values +dsv-submit+ 200))
                     ("/api/v1/tasks/task-1" (values (dsv-finished "https://oss.example.com/e.mp4") 200))
                     ("https://oss.example.com/e.mp4" (values (octets 5) 200)))
    (let ((made (dashscope-video:generate "a kite" :duration 4 :aspect-ratio "1:1")))
      (is (equal "t2v" (getf made :mode)))
      (is (= 4 (getf made :duration)))
      (is (equal "960*960" (getf made :size)))
      (is (equal "task-1" (getf made :task-id)))
      (is (probe-file (getf made :path)) "the answer names the saved clip"))))

(deftest dashscope-video-cell-the-command-reports-what-it-runs-with ()
  (with-dsv-answers ()
    (is-carrying (report (cell-entry "nodecode-dashscope-video" "dashscope-video" ""))
      ("https://token-plan.ap-southeast-1.maas.aliyuncs.com")
      ("key: set") ("the section's api_key") ("happyhorse-1.1")
      ("t2v  happyhorse-1.1-t2v") ("r2v  happyhorse-1.1-r2v")
      ("no clip yet"))))
