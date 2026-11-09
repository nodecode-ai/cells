;;;; cell-test.lisp --- the minimax-video cell against a stubbed MiniMax.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every MiniMax request is a stubbed NLK:HTTP and every key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches MiniMax, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it -
;;;; through the real dispatch, contract and all - so the schema the provider
;;;; is shown and the body that runs are one shape. The videos are written
;;;; under a fresh folder in the scratch home and removed with it.

(in-package #:nodecode.test)

(define-test-slice "minimax-video" "MINIMAX-VIDEO-CELL-")

;;; --- fixtures --------------------------------------------------------------------

(defun minimax-video-config (&rest pairs)
  "A whole shared config holding the `minimax-video' section PAIRS make."
  (nlk:json-object "minimax-video" (apply #'nlk:make-json-object pairs)))

(defun minimax-video-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-minimax-video:start-cell (apply #'minimax-video-config pairs)))

(defparameter +png+
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  "A 1x1 PNG in base64: the first frame an image-to-video call sends.")

(defun png-octets ()
  (cl-base64:base64-string-to-usb8-array +png+))

(defun mp4-octets ()
  "The first bytes of an MP4 file: a box header and its ftyp."
  (concatenate '(vector (unsigned-byte 8))
               #(0 0 0 32)
               (map '(vector (unsigned-byte 8)) #'char-code "ftypmp42")
               #(0 0 0 0)))

(defmacro with-video-directory ((directory) &body body)
  "BODY with DIRECTORY a fresh empty folder, removed afterwards."
  `(let ((,directory (merge-pathnames
                      (format nil "minimax-video-test-~d-~d/" (get-universal-time) (random 1000000))
                      (uiop:temporary-directory))))
     (ensure-directories-exist (merge-pathnames "keep" ,directory))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,directory :validate t)))))

(defmacro with-key ((&optional (key "mm-secret") (name "MM_KEY")) &body body)
  "BODY with the credential seam answering KEY for the variable NAME."
  `(with-stubbed-fdefinition (nle::credential-env (variable)
                              (and (equal variable ,name) ,key))
     ,@body))

(defmacro with-minimax ((directory &rest pairs) &body body)
  "A started cell whose section is PAIRS, its videos under DIRECTORY, one
second between checks and its key in MM_KEY."
  `(with-cell-stop ((minimax-video-start "api" "https://api.minimax.test"
                                          "output_dir" (namestring ,directory)
                                          "poll_seconds" 1
                                          "api_key_env" "MM_KEY"
                                          ,@pairs))
     ,@body))

(defmacro with-minimax-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS) or (values OCTETS STATUS), as NLK:HTTP does. ASKED collects
(METHOD URL OPTIONS), oldest first, once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url options) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected MiniMax request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun video-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun video-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (video-answer id tool arguments)))

(defun payload-of (request)
  "The JSON payload REQUEST, a stub's (METHOD URL OPTIONS), sent."
  (nlk:decode-json (getf (third request) :content)))

(defun header-of (request name)
  (cdr (assoc name (getf (third request) :headers) :test #'equal)))

(defparameter +submitted+
  "{\"task_id\":\"t-1\",\"base_resp\":{\"status_code\":0,\"status_msg\":\"\"}}"
  "What MiniMax answers a submitted job.")

(defparameter +succeeded+
  "{\"task_id\":\"t-1\",\"status\":\"Success\",\"file_id\":\"f-1\"}"
  "What MiniMax answers for a finished job.")

(defparameter +processing+
  "{\"task_id\":\"t-1\",\"status\":\"Processing\"}"
  "What MiniMax answers for a job still rendering.")

(defparameter +retrieved+
  "{\"file\":{\"download_url\":\"https://cdn.minimax.test/t-1.mp4\"},\"base_resp\":{\"status_code\":0}}"
  "What MiniMax answers a file retrieve.")

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "minimax-video"
  (:config "api" "https://api.minimax.test")
  (:help :minimax-video)
  (:command "video")
  (:refused ("api" "ftp://api.minimax.test")
             ("request_timeout" 1)
             ("poll_seconds" 0))
  (:idle nodecode-minimax-video:minimax-video-error
         (nodecode-minimax-video:generate-video nil)
         (nodecode-minimax-video:video-status nil)))

;;; --- generate_video -----------------------------------------------------------------

(deftest minimax-video-cell-generates-a-text-to-video ()
  (with-video-directory (directory)
    (with-minimax (directory)
      (with-key ()
        (with-minimax-wire (asked
                            ("/v1/query/video_generation" (values +succeeded+ 200))
                            ("/v1/files/retrieve" (values +retrieved+ 200))
                            ("/t-1.mp4" (values (mp4-octets) 200))
                            ("/v1/video_generation" (values +submitted+ 200)))
          (let ((object (video-object
                         "c1" "generate_video"
                         "{\"prompt\":\"a red fox crossing snow, wide shot\",\"duration\":10}")))
            (is (equal "Success" (nlk:json-value object :string "status")))
            (is (probe-file (nlk:json-value object :string "video")) "the file is written")
            (is (plusp (or (nlk:json-value object :integer "bytes") 0)) "with its size")
            (is (equal "t2v" (nlk:json-value object :string "mode")))
            (is (equal "t-1" (nlk:json-value object :string "task_id")))
            (is (equal "768P" (nlk:json-value object :string "resolution")) "the model's default")
            (is (equal "MiniMax-Hailuo-2.3" (nlk:json-value object :string "model"))))
          (let* ((submit (first asked))
                 (payload (payload-of submit)))
            (is (eq :post (first submit)))
            (is (equal "https://api.minimax.test/v1/video_generation" (second submit)))
            (is (equal "Bearer mm-secret" (header-of submit "authorization")) "a bearer token")
            (is (equal "a red fox crossing snow, wide shot" (nlk:json-value payload :string "prompt")))
            (is (equal "MiniMax-Hailuo-2.3" (nlk:json-value payload :string "model")))
            (is (equal 10 (nlk:json-value payload :integer "duration")))
            (is (equal "768P" (nlk:json-value payload :string "resolution")))
            (is (eq t (nlk:json-value payload :boolean "prompt_optimizer")))
            (is (null (gethash "first_frame_image" payload)) "text-to-video sends no image")))))))

(deftest minimax-video-cell-clamps-a-duration-to-six-or-ten ()
  (with-video-directory (directory)
    (with-minimax (directory "wait_seconds" 0)
      (with-key ()
        (with-minimax-wire (asked
                            ("/v1/query/video_generation" (values +processing+ 200))
                            ("/v1/video_generation" (values +submitted+ 200)))
          (video-object "c2" "generate_video" "{\"prompt\":\"a slow river\",\"duration\":7}")
          (is (equal 6 (nlk:json-value (payload-of (first asked)) :integer "duration")) "seven to six"))))))

(deftest minimax-video-cell-sends-a-local-image-as-a-data-uri ()
  (with-video-directory (directory)
    (let ((source (merge-pathnames "fox.png" directory)))
      (alexandria:write-byte-vector-into-file (png-octets) source :if-exists :supersede)
      (with-minimax (directory "wait_seconds" 0)
        (with-key ()
          (with-minimax-wire (asked
                              ("/v1/query/video_generation" (values +processing+ 200))
                              ("/v1/video_generation" (values +submitted+ 200)))
            (let ((object (video-object
                           "c3" "generate_video"
                           (format nil "{\"prompt\":\"make it move\",\"image\":\"~a\"}"
                                   (namestring source)))))
              (is (equal "i2v" (nlk:json-value object :string "mode"))))
            (let ((first-frame (nlk:json-value (payload-of (first asked))
                                               :string "first_frame_image")))
              (is (uiop:string-prefix-p "data:image/png;base64," first-frame) "as a data URI")
              (is (search +png+ first-frame) "with the file's own bytes"))))))))

(deftest minimax-video-cell-uses-the-subject-model ()
  (with-video-directory (directory)
    (with-minimax (directory "wait_seconds" 0)
      (with-key ()
        (with-minimax-wire (asked
                            ("/v1/query/video_generation" (values +processing+ 200))
                            ("/v1/video_generation" (values +submitted+ 200)))
          (let ((object (video-object
                         "c4" "generate_video"
                         "{\"prompt\":\"the hero walks forward\",\"reference_images\":[\"https://cdn.test/hero.png\"]}")))
            (is (equal "s2v" (nlk:json-value object :string "mode"))))
          (let* ((payload (payload-of (first asked)))
                 (refs (nlk:json-value payload :array "subject_reference"))
                 (first (and refs (plusp (length refs)) (aref refs 0)))
                 (images (and first (nlk:json-value first :array "image"))))
            (is (equal "S2V-01" (nlk:json-value payload :string "model")) "model_s2v's model")
            (is (equal "720P" (nlk:json-value payload :string "resolution")) "the older default")
            (is (equal "character" (nlk:json-value first :string "type")))
            (is (equal "https://cdn.test/hero.png" (and images (aref images 0))))))))))

(deftest minimax-video-cell-answers-a-task-that-is-still-running ()
  (with-video-directory (directory)
    (with-minimax (directory "wait_seconds" 0)
      (with-key ()
        (with-minimax-wire (asked
                            ("/v1/query/video_generation" (values +processing+ 200))
                            ("/v1/video_generation" (values +submitted+ 200)))
          (let ((object (video-object "c5" "generate_video" "{\"prompt\":\"a slow river\"}")))
            (is (equal "Processing" (nlk:json-value object :string "status")))
            (is (equal "t-1" (nlk:json-value object :string "task_id")))
            (is (null (nlk:json-value object :string "video")) "no file yet")
            (is (search "video_status" (nlk:json-value object :string "note")) "it names video_status"))
          (is (= 2 (length asked)) "the submit and one check, and no wait"))))))

(deftest minimax-video-cell-finishes-a-task-from-its-id ()
  (with-video-directory (directory)
    (with-minimax (directory)
      (with-key ()
        (with-minimax-wire (asked
                            ("/v1/query/video_generation" (values +succeeded+ 200))
                            ("/v1/files/retrieve" (values +retrieved+ 200))
                            ("/t-1.mp4" (values (mp4-octets) 200)))
          (let ((object (video-object "c6" "video_status" "{\"task_id\":\"t-1\",\"wait_seconds\":5}")))
            (is (equal "Success" (nlk:json-value object :string "status")))
            (is (probe-file (nlk:json-value object :string "video")) "it downloads the render")
            (is (equal "t-1" (nlk:json-value object :string "task_id"))))
          (is (search "task_id=t-1" (second (first asked))) "the query names the task")
          (is (eq :get (first (first asked)))))))))

;;; --- refusals --------------------------------------------------------------------------

(deftest minimax-video-cell-refuses-without-a-key ()
  (with-video-directory (directory)
    (with-minimax (directory)
      (with-stubbed-fdefinition (nle::credential-env (variable) nil)
        (with-minimax-wire (asked)
          (let ((text (video-answer "c7" "generate_video" "{\"prompt\":\"a fox\"}")))
            (is (search "MM_KEY" text) "the variable the section names")
            (is (search "Nothing was sent" text) "and that nothing was billed")
            (is (null asked) "nothing reached MiniMax")))))))

(deftest minimax-video-cell-explains-a-refused-call ()
  (with-video-directory (directory)
    (with-minimax (directory)
      (with-key ()
        (with-minimax-wire (asked
                            ("/v1/video_generation"
                             (values "{\"base_resp\":{\"status_code\":1004,\"status_msg\":\"invalid api key mm-secret\"}}"
                                     401)))
          (let ((text (video-answer "c8" "generate_video" "{\"prompt\":\"a fox\"}")))
            (is (search "HTTP 401" text) "the status comes back")
            (is (search "invalid api key" text) "with MiniMax's own words")
            (is (search "api_key" text) "and the setting that fixes it")
            (is (null (search "mm-secret" text)) "the key is never echoed")
            (is (search "[key]" text))))))))

(deftest minimax-video-cell-refuses-a-missing-image-file ()
  (with-video-directory (directory)
    (with-minimax (directory)
      (with-key ()
        (with-minimax-wire (asked)
          (let ((text (video-answer
                       "c9" "generate_video"
                       "{\"prompt\":\"make it move\",\"image\":\"/nonexistent/fox.png\"}")))
            (is (search "does not exist" text))
            (is (null asked) "nothing reached MiniMax")))))))

;;; --- /video -----------------------------------------------------------------------------

(deftest minimax-video-cell-answers-the-video-command ()
  (with-video-directory (directory)
    (with-minimax (directory)
      (multiple-value-bind (text dialog) (nodecode-minimax-video:video-command "" "s1")
        (is (search "Describe the video" text) "typed alone it says what to do")
        (is (consp (getf dialog :rows)) "and asks for the prompt in a dialog"))
      (with-key ()
        (with-minimax-wire (asked
                            ("/v1/query/video_generation" (values +succeeded+ 200))
                            ("/v1/files/retrieve" (values +retrieved+ 200))
                            ("/t-1.mp4" (values (mp4-octets) 200))
                            ("/v1/video_generation" (values +submitted+ 200)))
          (multiple-value-bind (text dialog)
              (nodecode-minimax-video:video-command "--duration 6 a red fox in snow" "s1")
            (is (null dialog) "a prompt answers text only")
            (is (search ".mp4" text) "the answer names the file it wrote")
            (is (search "MiniMax-Hailuo-2.3" text) "and the model"))
          (let ((payload (payload-of (first asked))))
            (is (equal "a red fox in snow" (nlk:json-value payload :string "prompt")) "the flags are not the prompt")
            (is (equal 6 (nlk:json-value payload :integer "duration")) "a flag picks the length")))))))
