;;;; cell-test.lisp --- the gmi-image cell against a stubbed GMI.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every GMI request is a stubbed NLK:HTTP and every key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches a provider, the environment or the
;;;; operator's files. The tool is called the way a model's call reaches it -
;;;; through the real dispatch, contract and all - so the schema the provider
;;;; is shown and the body that runs are one shape. The pictures are written
;;;; under a fresh folder in the scratch home and removed with it.

(in-package #:nodecode.test)

(define-test-slice "gmi-image" "GMI-IMAGE-CELL-")

(defun gmi-image-config (&rest pairs)
  "A whole shared config holding the `gmi_image' section PAIRS make."
  (nlk:json-object "gmi_image" (apply #'nlk:make-json-object pairs)))

(defun gmi-image-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-gmi-image:start-cell (apply #'gmi-image-config pairs)))

(defparameter +png+
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  "A 1x1 PNG in base64: what a GMI result links.")

(defparameter +key+ "gmi-secret-123"
  "The key the stubbed environment holds.")

(defun png-octets ()
  (cl-base64:base64-string-to-usb8-array +png+))

;;; --- GMI's own shapes ------------------------------------------------------------

(defun gmi-parameter (name type &key required options default (minimum nil) (maximum nil))
  "One parameter of a model schema, as GMI's catalog declares it."
  (apply #'nlk:make-json-object
         (append (list "name" name "type" type)
                 (when required (list "required" t))
                 (when options (list "options" (coerce options 'vector)))
                 (when default (list "default_value" default))
                 (when minimum (list "min_value" minimum))
                 (when maximum (list "max_value" maximum)))))

(defun gmi-model-record (&key (id "seedream-5.0-lite") (parameters '())
                              (status "active") (workers 1) (hidden nil))
  "A model schema as GET <api>/models/<id> answers it."
  (nlk:encode-json-object
   (apply #'nlk:make-json-object
          (append (list "model" id
                        "status" status
                        "qworkers" workers
                        "display_name" id
                        "brief_description" "a test model"
                        "modalities" (nlk:make-json-object
                                      "output" (vector (nlk:make-json-object "type" "image"))
                                      "input" (vector (nlk:make-json-object "type" "text")))
                        "pricing_details" "$0.03 per image"
                        "parameters" (coerce parameters 'vector))
                  (when hidden (list "model_metadata" (nlk:make-json-object "visibility" "hidden")))))))

(defun gmi-submitted (&optional (id "req-1"))
  (format nil "{\"request_id\":\"~a\",\"status\":\"queued\"}" id))

(defun gmi-finished (&optional (url "https://cdn.test/out.png"))
  (format nil "{\"request_id\":\"req-1\",\"status\":\"success\",\"outcome\":{\"image_url\":\"~a\"}}" url))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-gmi-directory ((directory) &body body)
  "BODY with DIRECTORY a fresh empty folder, removed afterwards."
  `(let ((,directory (merge-pathnames
                      (format nil "gmi-test-~d-~d/" (get-universal-time) (random 1000000))
                      (uiop:temporary-directory))))
     (ensure-directories-exist (merge-pathnames "keep" ,directory))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,directory :validate t)))))

(defmacro with-gmi ((asked &rest routes) &body body)
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

(defun gmi-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun gmi-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded; a refusal is raised with its text."
  (let ((text (nlk:trimmed (gmi-answer id tool arguments))))
    (unless (uiop:string-prefix-p "{" text)
      (error "the tool refused: ~a" text))
    (nlk:decode-json text)))

(defun sent (asked)
  "The one (METHOD URL OPTIONS) ASKED holds, in its three parts."
  (first asked))

(defun sent-body (asked)
  "The JSON object the first POST ASKED carried, decoded."
  (nlk:decode-json (getf (third (find :post asked :key #'first)) :content)))

(defun sent-payload (asked)
  "The `payload' member of the first POST ASKED carried, decoded."
  (nlk:json-value (sent-body asked) :object "payload"))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "gmi-image"
  (:config "key_env" "GMI_KEY" "api" "https://gmi.test/api")
  (:help :gmi-image)
  (:command "gmi-image")
  (:refused ("key_env" "")
             ("api" "ftp://gmi.test")
             ("timeout" 0)))

(deftest gmi-image-cell-without-a-key-variable-says-which-setting-names-one ()
  ;; The one setting no call can work without: the name of the variable, or a file.
  (let ((condition (signals-error nlk:config-refusal (gmi-image-start "key_env" ""))))
    (is (search "key_env" (princ-to-string condition)) "the refusal names the setting")
    (is (null (nle::find-tool "gmi_image")) "and installs nothing")))

(deftest gmi-image-cell-keeps-a-key-off-plain-http ()
  (let ((condition (signals-error nlk:config-refusal
                                  (gmi-image-start "key_env" "GMI_KEY"
                                                   "api" "http://gmi.public.test"))))
    (is (search "clear text" (princ-to-string condition)) "the key would ride in the open")))

;;; --- gmi_image -----------------------------------------------------------------

(deftest gmi-image-cell-draws-a-picture ()
  (with-gmi-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((gmi-image-start "key_env" "GMI_KEY"
                                        "api" "https://gmi.test/api"
                                        "timeout" 5
                                        "output_dir" (namestring directory)))
        (is (nle::find-tool "gmi_image") "the tool is registered")
        (with-gmi (asked ("/models/seedream-5.0-lite"
                          (values (gmi-model-record
                                   :parameters (list (gmi-parameter "prompt" "string" :required t)
                                                     (gmi-parameter "aspect_ratio" "string"
                                                                     :options '("16:9" "1:1" "9:16"))))
                                  200))
                         ("/requests/req-1" (values (gmi-finished) 200))
                         ("/api/requests" (values (gmi-submitted) 200))
                         ("https://cdn.test/out.png" (values (png-octets) 200)))
          (let* ((object (gmi-object "g-1" "gmi_image"
                                      "{\"prompt\":\"a blue boat\",\"aspect\":\"landscape\"}"))
                 (path (nlk:json-value object :string "image")))
            (is (and path (probe-file path)) "the picture was written under output_dir")
            (is (plusp (or (nlk:json-value object :integer "bytes") 0)) "with its size")
            (is (equal "1x1" (nlk:json-value object :string "pixels")) "the pixel size in the header")
            (is (equal "generate" (nlk:json-value object :string "mode")))
            (is (equal "seedream-5.0-lite" (nlk:json-value object :string "model")))
            (is (equal "16:9" (nlk:json-value object :string "ratio")) "the aspect's ratio")
            (is (equal "req-1" (nlk:json-value object :string "request_id")) "the job's id")
            (is (search "0.03" (nlk:json-value object :string "price")) "the model's price"))
          (let* ((submit (find-if (lambda (entry)
                                     (and (eq :post (first entry))
                                          (search "/requests" (second entry))
                                          (not (search "/requests/" (second entry)))))
                                   asked))
                 (url (second submit))
                 (bearer (cdr (assoc "authorization" (getf (third submit) :headers) :test #'equal))))
            (is (equal "https://gmi.test/api/requests" url) "the requestqueue route")
            (is (equal bearer (format nil "Bearer ~a" +key+)) "the key rides as a bearer token"))
          (let ((payload (sent-payload asked)))
            (is (equal "a blue boat" (nlk:json-value payload :string "prompt")))
            (is (equal "16:9" (nlk:json-value payload :string "aspect_ratio")) "the named aspect became the ratio the parameter declares")
            (is (null (nlk:json-value payload :any "size")) "and no size was invented"))
          (is (eq t (getf (third (car (last asked))) :binary)) "the picture is fetched as bytes"))))))

(deftest gmi-image-cell-gives-a-fixed-size-model-the-nearest-size ()
  (with-gmi-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((gmi-image-start "key_env" "GMI_KEY"
                                        "api" "https://gmi.test/api"
                                        "timeout" 5
                                        "output_dir" (namestring directory)))
        (with-gmi (asked ("/models/seedream-5.0-lite"
                          (values (gmi-model-record
                                   :parameters (list (gmi-parameter "prompt" "string" :required t)
                                                     (gmi-parameter "size" "string"
                                                                     :options '("1024x1024" "1536x1024"
                                                                                "1024x1536" "2048x2048"))
                                                     (gmi-parameter "seed" "integer" :required t
                                                                     :default 42)))
                                  200))
                         ("/requests/req-1" (values (gmi-finished) 200))
                         ("/api/requests" (values (gmi-submitted) 200))
                         ("https://cdn.test/out.png" (values (png-octets) 200)))
          (gmi-object "g-2" "gmi_image" "{\"prompt\":\"a heron\",\"aspect\":\"landscape\"}")
          (let ((payload (sent-payload asked)))
            (is (equal "1536x1024" (nlk:json-value payload :string "size")) "the landscape size nearest the ratio")
            (is (= 42 (nlk:json-value payload :integer "seed")) "a required parameter with a default is sent, as the plugin did")))))))

(deftest gmi-image-cell-edits-a-public-image-url ()
  (with-gmi-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((gmi-image-start "key_env" "GMI_KEY"
                                        "api" "https://gmi.test/api"
                                        "timeout" 5
                                        "output_dir" (namestring directory)))
        (with-gmi (asked ("/models/seedream-5.0-lite"
                          (values (gmi-model-record
                                   :parameters (list (gmi-parameter "prompt" "string" :required t)
                                                     (gmi-parameter "image" "string" :required t
                                                                     :maximum 3)))
                                  200))
                         ("/requests/req-1" (values (gmi-finished) 200))
                         ("/api/requests" (values (gmi-submitted) 200))
                         ("https://cdn.test/out.png" (values (png-octets) 200)))
          (let ((object (gmi-object "g-3" "gmi_image"
                                     "{\"prompt\":\"the same boat, at sunset\",\"images\":[\"https://example.com/boat.png\"]}")))
            (is (equal "edit" (nlk:json-value object :string "mode")) "a source URL makes it an edit"))
          (let* ((sent-images (nlk:json-value (sent-payload asked) :array "image"))
                     (first-image (and sent-images (aref sent-images 0))))
              (is (equal "https://example.com/boat.png" first-image) "the URL rides as the model's own image parameter")))
        (with-gmi (asked)
          (let ((text (gmi-answer "g-4" "gmi_image"
                                   "{\"prompt\":\"the same boat\",\"images\":[\"boat.png\"]}")))
            (is (search "public http(s) URLs" text) "a local path is refused")
            (is (null asked) "and nothing was dialled")))))))

(deftest gmi-image-cell-says-which-setting-holds-the-key ()
  (with-gmi-directory (directory)
    (with-key (nil)
      (with-cell-stop ((gmi-image-start "key_env" "GMI_KEY"
                                        "api" "https://gmi.test/api"
                                        "timeout" 5
                                        "output_dir" (namestring directory)))
        (with-gmi (asked)
          (let ((text (gmi-answer "g-5" "gmi_image" "{\"prompt\":\"a boat\"}")))
            (is (search "GMI_KEY" text) "the variable by name")
            (is (search "key_file" text) "and the file setting beside it")
            (is (null asked) "nothing was dialled")))))))

(deftest gmi-image-cell-explains-a-refused-credential ()
  (with-gmi-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((gmi-image-start "key_env" "GMI_KEY"
                                        "api" "https://gmi.test/api"
                                        "timeout" 5
                                        "output_dir" (namestring directory)))
        (with-gmi (asked ("/models/seedream-5.0-lite"
                          (values (format nil "{\"error\":\"invalid api key ~a\"}" +key+) 401)))
          (let ((text (gmi-answer "g-6" "gmi_image" "{\"prompt\":\"a boat\"}")))
            (is (search "HTTP 401" text) "the status comes back")
            (is (search "credential" text) "with what it means")
            (is (search "key_env" text) "and the setting that fixes it")
            (is (null (search +key+ text)) "the key is never echoed")))))))

(deftest gmi-image-cell-reports-a-failed-job ()
  (with-gmi-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((gmi-image-start "key_env" "GMI_KEY"
                                        "api" "https://gmi.test/api"
                                        "timeout" 5
                                        "output_dir" (namestring directory)))
        (with-gmi (asked ("/models/seedream-5.0-lite"
                          (values (gmi-model-record
                                   :parameters (list (gmi-parameter "prompt" "string" :required t)))
                                  200))
                         ("/requests/req-1"
                          (values "{\"request_id\":\"req-1\",\"status\":\"failed\",\"error\":\"content policy\"}" 200))
                         ("/api/requests" (values (gmi-submitted) 200)))
          (let ((text (gmi-answer "g-7" "gmi_image" "{\"prompt\":\"a boat\"}")))
            (is (search "failed" text) "the job's state is named")
            (is (search "content policy" text) "with GMI's own reason")))))))

(deftest gmi-image-cell-names-a-daily-model-limit ()
  (with-gmi-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((gmi-image-start "key_env" "GMI_KEY"
                                        "api" "https://gmi.test/api"
                                        "timeout" 5
                                        "output_dir" (namestring directory)))
        (with-gmi (asked ("/models/seedream-5.0-lite"
                          (values "{\"rate_limit\":{\"limit_type\":\"RPD\",\"limit\":100,\"retry_after_ms\":7200000}}"
                                  429)))
          (let ((text (gmi-answer "g-8" "gmi_image" "{\"prompt\":\"a boat\"}")))
            (is (search "daily model limit" text) "a daily limit reads as one")
            (is (search "100" text) "with the limit")
            (is (search "Choose another model" text) "and what to do about it")))))))

;;; --- /gmi-image ----------------------------------------------------------------

(deftest gmi-image-cell-answers-the-command ()
  (with-gmi-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((gmi-image-start "key_env" "GMI_KEY"
                                        "api" "https://gmi.test/api"
                                        "timeout" 5
                                        "output_dir" (namestring directory)))
        (multiple-value-bind (text dialog) (nodecode-gmi-image:gmi-image-command "" "s1")
          (is (search "Describe the picture" text) "typed alone it says what to do")
          (is (consp (getf dialog :rows)) "and asks for the prompt in a dialog"))
        (with-gmi (asked ("/models/seedream-5.0-lite"
                          (values (gmi-model-record
                                   :parameters (list (gmi-parameter "prompt" "string" :required t)
                                                     (gmi-parameter "aspect_ratio" "string"
                                                                     :options '("16:9" "1:1" "9:16"))))
                                  200))
                         ("/requests/req-1" (values (gmi-finished) 200))
                         ("/api/requests" (values (gmi-submitted) 200))
                         ("https://cdn.test/out.png" (values (png-octets) 200)))
          (multiple-value-bind (text dialog)
              (nodecode-gmi-image:gmi-image-command "--portrait a red fox" "s1")
            (is (null dialog) "a prompt answers text only")
            (is (search ".png" text) "the answer names the file it wrote")
            (is (search "seedream-5.0-lite" text) "and the model"))
          (let* ((payload (sent-payload asked))
                 (prompt (nlk:json-value payload :string "prompt")))
            (is (equal "a red fox" prompt) "the flags are not part of the prompt")
            (is (equal "9:16" (nlk:json-value payload :string "aspect_ratio")))))))))
