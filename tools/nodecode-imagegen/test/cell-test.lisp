;;;; cell-test.lisp --- the imagegen cell against a stubbed gateway.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every gateway request is a stubbed NLK:HTTP and every key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches a gateway, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it -
;;;; through the real dispatch, contract and all - so the schema the provider
;;;; is shown and the body that runs are one shape. The pictures are written
;;;; under a fresh folder in the scratch home and removed with it.

(in-package #:nodecode.test)

(define-test-slice "imagegen" "IMAGEGEN-CELL-")

(defun imagegen-config (&rest pairs)
  "A whole shared config holding the `imagegen' section PAIRS make."
  (nlk:json-object "imagegen" (apply #'nlk:make-json-object pairs)))

(defun imagegen-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-imagegen:start-cell (apply #'imagegen-config pairs)))

(defparameter +png+
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  "A 1x1 PNG in base64: what the gateway answers, and what a source image is.")

(defun png-octets ()
  (cl-base64:base64-string-to-usb8-array +png+))

(defun b64-answer ()
  "The body a gateway answers for one generated picture."
  (format nil "{\"data\":[{\"b64_json\":\"~a\"}]}" +png+))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-imagegen-directory ((directory) &body body)
  "BODY with DIRECTORY a fresh empty folder, removed afterwards."
  `(let ((,directory (merge-pathnames
                      (format nil "imagegen-test-~d-~d/" (get-universal-time) (random 1000000))
                      (uiop:temporary-directory))))
     (ensure-directories-exist (merge-pathnames "keep" ,directory))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,directory :validate t)))))

(defmacro with-gateway ((asked &rest routes) &body body)
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
                                         (t (error "unexpected gateway request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun imagegen-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun imagegen-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (imagegen-answer id tool arguments)))

(defun sent (asked)
  "The one (METHOD URL OPTIONS) ASKED holds, in its three parts."
  (first asked))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "imagegen"
  (:config "base_url" "https://gateway.test/v1")
  (:help :imagegen)
  (:command "image")
  (:refused ("base_url" "ftp://gateway.test/v1")
             ("base_url" "")
             ("model" 7)))

(deftest imagegen-cell-without-a-base-url-refuses ()
  ;; The one setting no verb can work without.
  (let ((condition (signals-error nlk:config-refusal (imagegen-start))))
    (is (search "base_url" (princ-to-string condition)) "the refusal names the setting")
    (is (null (nle::find-tool "generate_image")) "and installs nothing")))

(deftest imagegen-cell-keeps-a-key-off-plain-http-to-a-public-host ()
  (let ((condition (signals-error nlk:config-refusal
                                  (imagegen-start "base_url" "http://gateway.public.test/v1"))))
    (is (search "clear text" (princ-to-string condition)) "a public plain-HTTP gateway is refused, because the key would ride in the open"))
  (with-cell-stop ((imagegen-start "base_url" "http://127.0.0.1:8317/v1"))
    (is (functionp stop) "a loopback gateway may be plain HTTP")))

;;; --- generate_image ------------------------------------------------------------------

(deftest imagegen-cell-generates-an-image ()
  (with-imagegen-directory (directory)
    (with-cell-stop ((imagegen-start "base_url" "https://gateway.test/v1"
                                     "output_dir" (namestring directory)))
      (is (nle::find-tool "generate_image") "the tool is registered")
      (with-gateway (asked ("/images/generations"
                            (values (format nil "{\"data\":[{\"b64_json\":\"~a\",\"revised_prompt\":\"a red fox\"}]}"
                                            +png+)
                                    200)))
        (let* ((object (imagegen-object
                        "img-1" "generate_image"
                        "{\"prompt\":\"a red fox\",\"aspect\":\"landscape\"}"))
               (path (nlk:json-value object :string "image")))
          (is (and path (probe-file path)) "the picture was written under output_dir")
          (is (plusp (or (nlk:json-value object :integer "bytes") 0)) "with its size")
          (is (equal "1x1" (nlk:json-value object :string "pixels")) "and the pixel size its header carries")
          (is (equal "generate" (nlk:json-value object :string "mode")))
          (is (equal "a red fox" (nlk:json-value object :string "revised_prompt"))))
        (destructuring-bind (method url options) (sent asked)
          (is (eq :post method))
          (is (equal "https://gateway.test/v1/images/generations" url))
          (let ((payload (nlk:decode-json (getf options :content))))
            (is (equal "gpt-image-2" (nlk:json-value payload :string "model")) "the section's model")
            (is (equal "medium" (nlk:json-value payload :string "quality")) "a quality always rides a sized model")
            (is (equal "1536x1024" (nlk:json-value payload :string "size")) "and the aspect's size")))))))

(deftest imagegen-cell-reads-a-quality-out-of-a-model-name ()
  (with-imagegen-directory (directory)
    (with-cell-stop ((imagegen-start "base_url" "https://gateway.test/v1"
                                     "output_dir" (namestring directory)))
      (with-gateway (asked ("/images/generations" (values (b64-answer) 200)))
        (let ((object (imagegen-object
                       "img-2" "generate_image"
                       "{\"prompt\":\"a heron\",\"aspect\":\"portrait\",\"model\":\"gpt-image-2-high\"}")))
          (is (equal "gpt-image-2" (nlk:json-value object :string "api_model")) "gpt-image-2-high is gpt-image-2 at high")
          (is (equal "high" (nlk:json-value object :string "requested_quality"))))
        (let ((payload (nlk:decode-json (getf (third (sent asked)) :content))))
          (is (equal "gpt-image-2" (nlk:json-value payload :string "model")))
          (is (equal "high" (nlk:json-value payload :string "quality")))
          (is (equal "1024x1536" (nlk:json-value payload :string "size"))))))))

(deftest imagegen-cell-sends-no-enums-to-a-gateway-model ()
  ;; A model the gateway defines rejects size and quality enums it does not
  ;; know, so the cell sends neither.
  (with-imagegen-directory (directory)
    (with-cell-stop ((imagegen-start "base_url" "https://gateway.test/v1"
                                     "output_dir" (namestring directory)))
      (with-gateway (asked ("/images/generations" (values (b64-answer) 200)))
        (imagegen-object "img-3" "generate_image"
                          "{\"prompt\":\"a heron\",\"model\":\"flux-schnell\"}")
        (let ((payload (nlk:decode-json (getf (third (sent asked)) :content))))
          (is (equal "flux-schnell" (nlk:json-value payload :string "model")) "the id is sent verbatim")
          (is (null (gethash "size" payload)) "with no size")
          (is (null (gethash "quality" payload)) "and no quality"))))))

(deftest imagegen-cell-saves-an-image-the-gateway-links ()
  (with-imagegen-directory (directory)
    (with-cell-stop ((imagegen-start "base_url" "https://gateway.test/v1"
                                     "output_dir" (namestring directory)))
      (with-gateway (asked ("/images/generations"
                            (values "{\"data\":[{\"url\":\"https://cdn.test/fox.png\"}]}" 200))
                            ("/fox.png" (values (png-octets) 200)))
        (let ((object (imagegen-object "img-4" "generate_image" "{\"prompt\":\"a fox\"}")))
          (is (probe-file (nlk:json-value object :string "image")) "a url answer is fetched and saved, not handed on")
          (is (equal "1x1" (nlk:json-value object :string "pixels"))))
        (is (eq t (getf (third (second asked)) :binary)) "fetched as bytes, not as text")
        (is (equal "https://cdn.test/fox.png" (second (second asked))))))))

(deftest imagegen-cell-edits-with-source-images ()
  (with-imagegen-directory (directory)
    (let ((source (merge-pathnames "fox.png" directory)))
      (alexandria:write-byte-vector-into-file (png-octets) source :if-exists :supersede)
      (with-cell-stop ((imagegen-start "base_url" "https://gateway.test/v1"
                                       "output_dir" (namestring directory)))
        (with-gateway (asked ("/images/edits" (values (b64-answer) 200)))
          (let ((object (imagegen-object
                         "img-5" "generate_image"
                         (format nil "{\"prompt\":\"make it dusk\",\"images\":[\"~a\"]}"
                                 (namestring source)))))
            (is (equal "edit" (nlk:json-value object :string "mode")) "a source image makes it an edit"))
          (destructuring-bind (method url options) (sent asked)
            (is (eq :post method))
            (is (equal "https://gateway.test/v1/images/edits" url))
            (let ((content (getf options :content)))
              (is (equal "make it dusk" (cdr (assoc "prompt" content :test #'equal))))
              (is (pathnamep (cdr (assoc "image" content :test #'equal))) "the source rides as a file part")
              (is (null (getf options :content-type)) "and the body is multipart, not JSON"))))))))

(deftest imagegen-cell-refuses-a-source-that-is-not-an-image ()
  (with-imagegen-directory (directory)
    (let ((source (merge-pathnames "notes.txt" directory)))
      (with-open-file (out source :direction :output :if-exists :supersede)
        (write-string "not a picture" out))
      (with-cell-stop ((imagegen-start "base_url" "https://gateway.test/v1"
                                       "output_dir" (namestring directory)))
        (with-gateway (asked)
          (let ((text (imagegen-answer
                       "img-6" "generate_image"
                       (format nil "{\"prompt\":\"edit\",\"images\":[\"~a\"]}"
                               (namestring source)))))
            (is (search "not a PNG" text) "only image bytes are sent")
            (is (null asked) "and nothing reached the gateway")))))))

(deftest imagegen-cell-explains-a-refused-call ()
  (with-imagegen-directory (directory)
    (with-stubbed-fdefinition (nle::credential-env (name)
                                (and (equal name "IMG_KEY") "sk-secret-123"))
      (with-cell-stop ((imagegen-start "base_url" "https://gateway.test/v1"
                                       "output_dir" (namestring directory)
                                       "api_key_env" "IMG_KEY"))
        (with-gateway (asked ("/images/generations"
                              (values "{\"error\":{\"message\":\"bad key sk-secret-123\"}}" 401)))
          (let ((text (imagegen-answer "img-7" "generate_image" "{\"prompt\":\"a fox\"}")))
            (is (search "HTTP 401" text) "the status comes back")
            (is (search "bad key" text) "with the gateway's own words")
            (is (search "api_key_env" text) "and the setting that fixes it")
            (is (null (search "sk-secret-123" text)) "the key is never echoed")
            (is (search "[key]" text)))
          (is (equal "Bearer sk-secret-123"
                     (cdr (assoc "authorization" (getf (third (sent asked)) :headers)
                                  :test #'equal))) "the key rides as a bearer token"))))))

;;; --- /image --------------------------------------------------------------------------

(deftest imagegen-cell-answers-the-image-command ()
  (with-imagegen-directory (directory)
    (with-cell-stop ((imagegen-start "base_url" "https://gateway.test/v1"
                                     "output_dir" (namestring directory)))
      (multiple-value-bind (text dialog) (nodecode-imagegen:image-command "" "s1")
        (is (search "Describe the picture" text) "typed alone it says what to do")
        (is (consp (getf dialog :rows)) "and asks for the prompt in a dialog"))
      (with-gateway (asked ("/images/generations" (values (b64-answer) 200)))
        (multiple-value-bind (text dialog)
            (nodecode-imagegen:image-command "--landscape --quality high a red fox" "s1")
          (is (null dialog) "a prompt answers text only")
          (is (search ".png" text) "the answer names the file it wrote")
          (is (search "gpt-image-2" text) "and the model"))
        (let ((payload (nlk:decode-json (getf (third (sent asked)) :content))))
          (is (equal "a red fox" (nlk:json-value payload :string "prompt")) "the flags are not part of the prompt")
          (is (equal "high" (nlk:json-value payload :string "quality")) "a flag overrides the setting")
          (is (equal "1536x1024" (nlk:json-value payload :string "size")) "and picks the aspect"))))))

(deftest imagegen-cell-refuses-while-not-running ()
  (is (signals-error nodecode-imagegen:imagegen-error
                      (nodecode-imagegen:generate-image "a fox"))))
