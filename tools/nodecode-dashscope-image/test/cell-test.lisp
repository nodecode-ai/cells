;;;; cell-test.lisp --- the dashscope-image cell against a stubbed DashScope.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every DashScope request is a stubbed NLK:HTTP and every key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches a provider, the environment or the
;;;; operator's files. The tool is called the way a model's call reaches it -
;;;; through the real dispatch, contract and all - so the schema the provider
;;;; is shown and the body that runs are one shape. The pictures are written
;;;; under a fresh folder in the scratch home and removed with it.

(in-package #:nodecode.test)

(define-test-slice "dashscope-image" "DASHSCOPE-IMAGE-CELL-")

(defun dashscope-image-config (&rest pairs)
  "A whole shared config holding the `dashscope' section PAIRS make."
  (nlk:json-object "dashscope" (apply #'nlk:make-json-object pairs)))

(defun dashscope-image-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-dashscope-image:start-cell (apply #'dashscope-image-config pairs)))

(defparameter +png+
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  "A 1x1 PNG in base64: what DashScope links, and what a source image is.")

(defparameter +key+ "sk-secret-123"
  "The key the stubbed environment holds.")

(defun png-octets ()
  (cl-base64:base64-string-to-usb8-array +png+))

(defun image-answer (&optional (url "https://oss.test/out.png"))
  "The body DashScope answers for one generated picture."
  (format nil "{\"output\":{\"choices\":[{\"message\":{\"content\":[{\"image\":\"~a\"}]}}],\"finish_reason\":\"stop\"},\"usage\":{\"image_count\":1}}"
          url))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-dashscope-image-directory ((directory) &body body)
  "BODY with DIRECTORY a fresh empty folder, removed afterwards."
  `(let ((,directory (merge-pathnames
                      (format nil "dashscope-test-~d-~d/" (get-universal-time) (random 1000000))
                      (uiop:temporary-directory))))
     (ensure-directories-exist (merge-pathnames "keep" ,directory))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,directory :validate t)))))

(defmacro with-dashscope ((asked &rest routes) &body body)
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
                                         (t (error "unexpected request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defmacro with-key ((value) &body body)
  "BODY with the credential the cell reads being VALUE."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                               (progn name ,value))
     ,@body))

(defun dashscope-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun dashscope-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded; a refusal is raised with its text."
  (let ((text (nlk:trimmed (dashscope-answer id tool arguments))))
    (unless (uiop:string-prefix-p "{" text)
      (error "the tool refused: ~a" text))
    (nlk:decode-json text)))

(defun sent (asked)
  "The one (METHOD URL OPTIONS) ASKED holds, in its three parts."
  (first asked))

(defun content-items (payload)
  "The image and text members of the payload's one user message, as a list."
  (let* ((input (nlk:json-value payload :object "input"))
         (messages (and input (nlk:json-array input "messages")))
         (message (and messages (aref messages 0)))
         (content (and message (nlk:json-array message "content"))))
    (coerce content 'list)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "dashscope-image"
  (:config "api_key_env" "QWEN_KEY" "api" "https://dashscope.test")
  (:help :dashscope-image)
  (:command "dashscope")
  (:refused ("api_key_env" "")
             ("api" "ftp://dashscope.test")
             ("timeout" 0)))

(deftest dashscope-image-cell-without-its-key-variable-refuses ()
  ;; The one setting no verb can work without: the name of the variable.
  (let ((condition (signals-error nlk:config-refusal (dashscope-image-start "api" "https://dashscope.test"))))
    (is (search "api_key_env" (princ-to-string condition)) "the refusal names the setting")
    (is (null (nle::find-tool "dashscope_image")) "and installs nothing")))

(deftest dashscope-image-cell-keeps-a-key-off-plain-http-to-a-public-host ()
  (let ((condition (signals-error nlk:config-refusal
                                  (dashscope-image-start "api_key_env" "QWEN_KEY"
                                                         "api" "http://dashscope.public.test"))))
    (is (search "clear text" (princ-to-string condition)) "a public plain-HTTP base is refused, because the key would ride in the open")))

;;; --- dashscope_image ---------------------------------------------------------------

(deftest dashscope-image-cell-generates-an-image ()
  (with-dashscope-image-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((dashscope-image-start "api_key_env" "QWEN_KEY"
                                              "api" "https://dashscope.test"
                                              "output_dir" (namestring directory)))
        (is (nle::find-tool "dashscope_image") "the tool is registered")
        (with-dashscope (asked ("/multimodal-generation/generation"
                                 (values (image-answer) 200))
                                ("/out.png" (values (png-octets) 200)))
          (let* ((object (dashscope-object
                          "dss-1" "dashscope_image"
                          "{\"prompt\":\"a red fox\",\"aspect\":\"landscape\"}"))
                 (path (nlk:json-value object :string "image")))
            (is (and path (probe-file path)) "the picture was written under output_dir")
            (is (plusp (or (nlk:json-value object :integer "bytes") 0)) "with its size")
            (is (equal "1x1" (nlk:json-value object :string "pixels")) "and the pixel size its header carries")
            (is (equal "generate" (nlk:json-value object :string "mode")))
            (is (equal "1280*720" (nlk:json-value object :string "size")) "the aspect's W*H size"))
          (destructuring-bind (method url options) (sent asked)
            (is (eq :post method))
            (let ((route "https://dashscope.test/api/v1/services/aigc/multimodal-generation/generation")
                  (bearer (cdr (assoc "authorization" (getf options :headers) :test #'equal))))
              (is (equal route url) "the native multimodal-generation route")
              (is (equal bearer (format nil "Bearer ~a" +key+)) "the key rides as a bearer token"))
            (let* ((payload (nlk:decode-json (getf options :content)))
                   (items (content-items payload)))
              (is (equal "wan2.7-image" (nlk:json-value payload :string "model")) "the section's model")
              (is (equal "1280*720"
                         (nlk:json-value (nlk:json-value payload :object "parameters") :string "size")))
              (is (= 1 (length items)) "no source image, so one member")
              (is (equal "a red fox" (nlk:json-value (first items) :string "text")))))
          (is (eq t (getf (third (second asked)) :binary)) "the picture is fetched as bytes"))
        (with-dashscope (asked ("/multimodal-generation/generation" (values (image-answer) 200))
                              ("/out.png" (values (png-octets) 200)))
        (dashscope-object "dss-2" "dashscope_image" "{\"prompt\":\"a heron\",\"aspect\":\"portrait\"}")
        (let* ((payload (nlk:decode-json (getf (third (sent asked)) :content)))
               (parameters (nlk:json-value payload :object "parameters")))
          (is (equal "720*1280" (nlk:json-value parameters :string "size")) "portrait is 720*1280")))))))

(deftest dashscope-image-cell-inlines-a-source-image ()
  (with-dashscope-image-directory (directory)
    (let ((source (merge-pathnames "fox.png" directory)))
      (alexandria:write-byte-vector-into-file (png-octets) source :if-exists :supersede)
      (with-key (+key+)
        (with-cell-stop ((dashscope-image-start "api_key_env" "QWEN_KEY"
                                                "api" "https://dashscope.test"
                                                "output_dir" (namestring directory)))
          (with-dashscope (asked ("/multimodal-generation/generation" (values (image-answer) 200))
                                  ("/out.png" (values (png-octets) 200)))
            (let ((object (dashscope-object
                           "dss-3" "dashscope_image"
                           (format nil "{\"prompt\":\"make it dusk\",\"images\":[\"~a\"]}"
                                   (namestring source)))))
              (is (equal "edit" (nlk:json-value object :string "mode")) "a source image makes it an edit")
              (let ((model (nlk:json-value object :string "model")))
                (is (equal "wan2.7-image-pro" model) "and the answer names the pro model")))
            (let* ((payload (nlk:decode-json (getf (third (sent asked)) :content)))
                   (items (content-items payload))
                   (image (nlk:json-value (first items) :string "image")))
              (let ((model (nlk:json-value payload :string "model"))
                    (inline (and image (uiop:string-prefix-p "data:image/png;base64," image)))
                    (text (nlk:json-value (second items) :string "text")))
                (is (equal "wan2.7-image-pro" model) "the pro model is what was sent")
                (is inline "a local file travels base64-inlined, with its own type")
                (is (equal "make it dusk" text) "the text member comes last")))))))))

(deftest dashscope-image-cell-refuses-a-source-that-is-not-an-image ()
  (with-dashscope-image-directory (directory)
    (let ((source (merge-pathnames "notes.txt" directory)))
      (with-open-file (out source :direction :output :if-exists :supersede)
        (write-string "not a picture" out))
      (with-key (+key+)
        (with-cell-stop ((dashscope-image-start "api_key_env" "QWEN_KEY"
                                                "api" "https://dashscope.test"
                                                "output_dir" (namestring directory)))
          (with-dashscope (asked)
            (let ((text (dashscope-answer
                         "dss-4" "dashscope_image"
                         (format nil "{\"prompt\":\"edit\",\"images\":[\"~a\"]}"
                                 (namestring source)))))
              (is (search "not a PNG" text) "only image bytes are sent")
              (is (null asked) "and nothing reached DashScope"))))))))

(deftest dashscope-image-cell-says-which-setting-holds-the-key ()
  (with-dashscope-image-directory (directory)
    (with-key (nil)
      (with-cell-stop ((dashscope-image-start "api_key_env" "QWEN_KEY"
                                              "api" "https://dashscope.test"
                                              "output_dir" (namestring directory)))
        (with-dashscope (asked)
          (let ((text (dashscope-answer "dss-5" "dashscope_image" "{\"prompt\":\"a fox\"}")))
            (is (search "QWEN_KEY" text) "the variable by name")
            (is (search "api_key_file" text) "and the file setting beside it")
            (is (null asked) "nothing was dialled")))))))

(deftest dashscope-image-cell-explains-a-refused-call ()
  (with-dashscope-image-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((dashscope-image-start "api_key_env" "QWEN_KEY"
                                              "api" "https://dashscope.test"
                                              "output_dir" (namestring directory)))
        (with-dashscope (asked ("/multimodal-generation/generation"
                                 (values (format nil "{\"code\":\"InvalidApiKey\",\"message\":\"bad key ~a\"}" +key+)
                                         401)))
          (let ((text (dashscope-answer "dss-6" "dashscope_image" "{\"prompt\":\"a fox\"}")))
            (is (search "HTTP 401" text) "the status comes back")
            (is (search "InvalidApiKey" text) "with DasScope's own code")
            (is (search "api_key_env" text) "and the setting that fixes it")
            (is (null (search +key+ text)) "the key is never echoed")
            (is (search "[key]" text))))))))

(deftest dashscope-image-cell-reports-a-code-in-a-200 ()
  ;; DashScope answers some refusals inside a healthy status: a code and a message.
  (with-dashscope-image-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((dashscope-image-start "api_key_env" "QWEN_KEY"
                                              "api" "https://dashscope.test"
                                              "output_dir" (namestring directory)))
        (with-dashscope (asked ("/multimodal-generation/generation"
                                 (values "{\"code\":\"InvalidParameter\",\"message\":\"size unsupported\"}" 200)))
          (let ((text (dashscope-answer "dss-7" "dashscope_image" "{\"prompt\":\"a fox\"}")))
            (is (search "InvalidParameter" text) "the code is named")
            (is (search "size unsupported" text) "with the message")))))))

;;; --- /dashscope --------------------------------------------------------------------

(deftest dashscope-image-cell-answers-the-dashscope-command ()
  (with-dashscope-image-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((dashscope-image-start "api_key_env" "QWEN_KEY"
                                              "api" "https://dashscope.test"
                                              "output_dir" (namestring directory)))
        (multiple-value-bind (text dialog) (nodecode-dashscope-image:dashscope-command "" "s1")
          (is (search "Describe the picture" text) "typed alone it says what to do")
          (is (consp (getf dialog :rows)) "and asks for the prompt in a dialog"))
        (with-dashscope (asked ("/multimodal-generation/generation" (values (image-answer) 200))
                                ("/out.png" (values (png-octets) 200)))
          (multiple-value-bind (text dialog)
              (nodecode-dashscope-image:dashscope-command "--landscape a red fox" "s1")
            (is (null dialog) "a prompt answers text only")
            (is (search ".png" text) "the answer names the file it wrote")
            (is (search "wan2.7-image" text) "and the model"))
          (let* ((payload (nlk:decode-json (getf (third (sent asked)) :content)))
                 (items (content-items payload)))
            (let ((text (nlk:json-value (first items) :string "text")))
              (is (equal "a red fox" text) "the flags are not part of the prompt"))))))))
