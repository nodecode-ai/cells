;;;; cell-test.lisp --- the minimax-image cell against a stubbed MiniMax.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every MiniMax request is a stubbed NLK:HTTP and every key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches a provider, the environment or the
;;;; operator's files. The tool is called the way a model's call reaches it -
;;;; through the real dispatch, contract and all - so the schema the provider
;;;; is shown and the body that runs are one shape. The pictures are written
;;;; under a fresh folder in the scratch home and removed with it.

(in-package #:nodecode.test)

(define-test-slice "minimax-image" "MINIMAX-IMAGE-CELL-")

(defun minimax-image-config (&rest pairs)
  "A whole shared config holding the `minimax_image' section PAIRS make."
  (nlk:json-object "minimax_image" (apply #'nlk:make-json-object pairs)))

(defun minimax-image-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-minimax-image:start-cell (apply #'minimax-image-config pairs)))

(defparameter +png+
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  "A 1x1 PNG in base64: what MiniMax links, and what a source image is.")

(defparameter +key+ "mm-secret-123"
  "The key the stubbed environment holds.")

(defun png-octets ()
  (cl-base64:base64-string-to-usb8-array +png+))

(defun image-answer (&optional (url "https://cdn.test/out.png"))
  "The body MiniMax answers for one generated picture."
  (format nil "{\"data\":{\"image_urls\":[\"~a\"]},\"base_resp\":{\"status_code\":0,\"status_msg\":\"success\"},\"id\":\"gen-1\"}"
          url))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-minimax-image-directory ((directory) &body body)
  "BODY with DIRECTORY a fresh empty folder, removed afterwards."
  `(let ((,directory (merge-pathnames
                      (format nil "minimax-test-~d-~d/" (get-universal-time) (random 1000000))
                      (uiop:temporary-directory))))
     (ensure-directories-exist (merge-pathnames "keep" ,directory))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,directory :validate t)))))

(defmacro with-minimax ((asked &rest routes) &body body)
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

(defun minimax-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun minimax-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded; a refusal is raised with its text."
  (let ((text (nlk:trimmed (minimax-answer id tool arguments))))
    (unless (uiop:string-prefix-p "{" text)
      (error "the tool refused: ~a" text))
    (nlk:decode-json text)))

(defun sent (asked)
  "The one (METHOD URL OPTIONS) ASKED holds, in its three parts."
  (first asked))

(defun sent-payload (asked)
  "The JSON body of the first POST ASKED holds, decoded."
  (nlk:decode-json (getf (third (sent asked)) :content)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "minimax-image"
  (:config "key_env" "MINIMAX_KEY" "api" "https://minimax.test")
  (:help :minimax-image)
  (:command "minimax-image")
  (:refused ("key_env" "")
             ("api" "ftp://minimax.test")
             ("timeout" 0)))

(deftest minimax-image-cell-without-a-key-variable-says-which-setting-names-one ()
  ;; The one setting no call can work without: the name of the variable, or a file.
  (let ((condition (signals-error nlk:config-refusal (minimax-image-start "key_env" ""))))
    (is (search "key_env" (princ-to-string condition)) "the refusal names the setting")
    (is (null (nle::find-tool "minimax_image")) "and installs nothing")))

(deftest minimax-image-cell-keeps-a-key-off-plain-http-to-a-public-host ()
  (let ((condition (signals-error nlk:config-refusal
                                  (minimax-image-start "key_env" "MINIMAX_KEY"
                                                       "api" "http://minimax.public.test"))))
    (is (search "clear text" (princ-to-string condition)) "the key would ride in the open")))

;;; --- minimax_image -----------------------------------------------------------------

(deftest minimax-image-cell-generates-an-image ()
  (with-minimax-image-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((minimax-image-start "key_env" "MINIMAX_KEY"
                                            "api" "https://minimax.test"
                                            "output_dir" (namestring directory)))
        (is (nle::find-tool "minimax_image") "the tool is registered")
        (with-minimax (asked ("/v1/image_generation" (values (image-answer) 200))
                              ("/out.png" (values (png-octets) 200)))
          (let* ((object (minimax-object
                           "mm-1" "minimax_image"
                           "{\"prompt\":\"a red fox\",\"aspect\":\"landscape\"}"))
                 (path (nlk:json-value object :string "image")))
            (is (and path (probe-file path)) "the picture was written under output_dir")
            (is (plusp (or (nlk:json-value object :integer "bytes") 0)) "with its size")
            (is (equal "1x1" (nlk:json-value object :string "pixels")) "the pixel size in the header")
            (is (equal "generate" (nlk:json-value object :string "mode")))
            (is (equal "image-01" (nlk:json-value object :string "model")))
            (is (equal "16:9" (nlk:json-value object :string "ratio")) "the aspect's W:H ratio"))
          (destructuring-bind (method url options) (sent asked)
            (is (eq :post method))
            (let ((route "https://minimax.test/v1/image_generation")
                  (bearer (cdr (assoc "authorization" (getf options :headers) :test #'equal))))
              (is (equal route url) "the image_generation route")
              (is (equal bearer (format nil "Bearer ~a" +key+)) "the key rides as a bearer token"))
            (let ((payload (sent-payload asked)))
              (is (equal "a red fox" (nlk:json-value payload :string "prompt")))
              (is (equal "16:9" (nlk:json-value payload :string "aspect_ratio")))
              (is (equal "url" (nlk:json-value payload :string "response_format")) "a link, downloaded at once")
              (is (= 1 (nlk:json-value payload :integer "n")))
              (is (null (nlk:json-value payload :any "subject_reference")) "no subject reference")))
          (is (eq t (getf (third (second asked)) :binary)) "the picture is fetched as bytes"))
        (with-minimax (asked ("/v1/image_generation" (values (image-answer) 200))
                              ("/out.png" (values (png-octets) 200)))
          (minimax-object "mm-2" "minimax_image" "{\"prompt\":\"a heron\",\"aspect\":\"portrait\"}")
          (is (equal "9:16" (nlk:json-value (sent-payload asked) :string "aspect_ratio")) "portrait"))))))

(deftest minimax-image-cell-inlines-a-subject-image ()
  (with-minimax-image-directory (directory)
    (let ((source (merge-pathnames "fox.png" directory)))
      (alexandria:write-byte-vector-into-file (png-octets) source :if-exists :supersede)
      (with-key (+key+)
        (with-cell-stop ((minimax-image-start "key_env" "MINIMAX_KEY"
                                              "api" "https://minimax.test"
                                              "output_dir" (namestring directory)))
          (with-minimax (asked ("/v1/image_generation" (values (image-answer) 200))
                                ("/out.png" (values (png-octets) 200)))
            (let ((object (minimax-object
                           "mm-3" "minimax_image"
                           (format nil "{\"prompt\":\"the same fox, at night\",\"images\":[\"~a\"]}"
                                   (namestring source)))))
              (is (equal "subject" (nlk:json-value object :string "mode")) "a source image makes it a subject")
              (is (equal "image-01-live" (nlk:json-value object :string "model")) "the live model"))
            (let* ((payload (sent-payload asked))
                   (references (nlk:json-value payload :array "subject_reference"))
                   (entry (and references (aref references 0)))
                   (file (and entry (nlk:json-value entry :string "image_file"))))
              (is (equal "image-01-live" (nlk:json-value payload :string "model")) "the live model was sent")
              (is (equal "character" (and entry (nlk:json-value entry :string "type"))))
              (is (and file (uiop:string-prefix-p "data:image/png;base64," file)) "the file travels inline"))))))))

(deftest minimax-image-cell-refuses-a-source-that-is-not-an-image ()
  (with-minimax-image-directory (directory)
    (let ((source (merge-pathnames "notes.txt" directory)))
      (with-open-file (out source :direction :output :if-exists :supersede)
        (write-string "not a picture" out))
      (with-key (+key+)
        (with-cell-stop ((minimax-image-start "key_env" "MINIMAX_KEY"
                                              "api" "https://minimax.test"
                                              "output_dir" (namestring directory)))
          (with-minimax (asked)
            (let ((text (minimax-answer
                         "mm-4" "minimax_image"
                         (format nil "{\"prompt\":\"restyle\",\"images\":[\"~a\"]}"
                                 (namestring source)))))
              (is (search "not a PNG" text) "only image bytes are sent")
              (is (null asked) "and nothing reached MiniMax"))))))))

(deftest minimax-image-cell-says-which-setting-holds-the-key ()
  (with-minimax-image-directory (directory)
    (with-key (nil)
      (with-cell-stop ((minimax-image-start "key_env" "MINIMAX_KEY"
                                            "api" "https://minimax.test"
                                            "output_dir" (namestring directory)))
        (with-minimax (asked)
          (let ((text (minimax-answer "mm-5" "minimax_image" "{\"prompt\":\"a fox\"}")))
            (is (search "MINIMAX_KEY" text) "the variable by name")
            (is (search "key_file" text) "and the file setting beside it")
            (is (null asked) "nothing was dialled")))))))

(deftest minimax-image-cell-explains-a-refused-call ()
  (with-minimax-image-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((minimax-image-start "key_env" "MINIMAX_KEY"
                                            "api" "https://minimax.test"
                                            "output_dir" (namestring directory)))
        (with-minimax (asked ("/v1/image_generation"
                              (values (format nil "{\"base_resp\":{\"status_code\":1004,\"status_msg\":\"invalid api key ~a\"}}"
                                              +key+)
                                      401)))
          (let ((text (minimax-answer "mm-6" "minimax_image" "{\"prompt\":\"a fox\"}")))
            (is (search "HTTP 401" text) "the status comes back")
            (is (search "invalid api key" text) "with MiniMax's own message")
            (is (search "key_env" text) "and the setting that fixes it")
            (is (null (search +key+ text)) "the key is never echoed")
            (is (search "[key]" text))))))))

(deftest minimax-image-cell-reports-a-code-in-a-200 ()
  ;; MiniMax answers some refusals inside a healthy status: a base_resp code.
  (with-minimax-image-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((minimax-image-start "key_env" "MINIMAX_KEY"
                                            "api" "https://minimax.test"
                                            "output_dir" (namestring directory)))
        (with-minimax (asked ("/v1/image_generation"
                              (values "{\"base_resp\":{\"status_code\":2013,\"status_msg\":\"invalid params, prompt over 1500\"}}" 200)))
          (let ((text (minimax-answer "mm-7" "minimax_image" "{\"prompt\":\"a fox\"}")))
            (is (search "MiniMax refused the call" text) "the refusal is named")
            (is (search "prompt over 1500" text) "with its message")))))))

;;; --- /minimax-image ----------------------------------------------------------------

(deftest minimax-image-cell-answers-the-command ()
  (with-minimax-image-directory (directory)
    (with-key (+key+)
      (with-cell-stop ((minimax-image-start "key_env" "MINIMAX_KEY"
                                            "api" "https://minimax.test"
                                            "output_dir" (namestring directory)))
        (multiple-value-bind (text dialog) (nodecode-minimax-image:minimax-image-command "" "s1")
          (is (search "Describe the picture" text) "typed alone it says what to do")
          (is (consp (getf dialog :rows)) "and asks for the prompt in a dialog"))
        (with-minimax (asked ("/v1/image_generation" (values (image-answer) 200))
                              ("/out.png" (values (png-octets) 200)))
          (multiple-value-bind (text dialog)
              (nodecode-minimax-image:minimax-image-command "--landscape a red fox" "s1")
            (is (null dialog) "a prompt answers text only")
            (is (search ".png" text) "the answer names the file it wrote")
            (is (search "image-01" text) "and the model"))
          (let* ((payload (sent-payload asked))
                 (prompt (nlk:json-value payload :string "prompt")))
            (is (equal "a red fox" prompt) "the flags are not part of the prompt")
            (is (equal "16:9" (nlk:json-value payload :string "aspect_ratio")))))))))
