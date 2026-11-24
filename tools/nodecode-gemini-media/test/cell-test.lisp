;;;; cell-test.lisp --- the gemini-media cell against a stubbed Gemini API.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every API request is a stubbed NLK:HTTP and every key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches Google, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it -
;;;; through the real dispatch, contract and all - so the schema the provider
;;;; is shown and the body that runs are one shape. The media is written under
;;;; a fresh folder in the scratch home and removed with it.

(in-package #:nodecode.test)

(define-test-slice "gemini-media" "GEMINI-MEDIA-CELL-")

(defun gemini-media-config (&rest pairs) "A whole shared config holding the `gemini-media' section PAIRS make."
  (nlk:json-object "gemini-media" (apply #'nlk:make-json-object pairs)))

(defun gemini-media-start (&rest pairs) "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-gemini-media:start-cell (apply #'gemini-media-config pairs)))

(defparameter +png+
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  "A 1x1 PNG in base64: what the API answers, and what a reference picture is.")

(defun png-octets ()
  (cl-base64:base64-string-to-usb8-array +png+))

(defun put-be (octets at bytes value)
  (loop for index from 0 below bytes
        do (setf (aref octets (+ at index))
                 (ldb (byte 8 (* 8 (- bytes 1 index))) value))))

(defun test-mp4 (timescale duration) "A 40-byte stand-in for an MP4: the bytes up to and including its mvhd atom."
  (let ((octets (make-array 40 :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace octets (map '(vector (unsigned-byte 8)) #'char-code "mvhd") :start1 4)
    (put-be octets 20 4 timescale)
    (put-be octets 24 4 duration)
    octets))

(defun image-answer (id)
  (format nil "{\"id\":\"~a\",\"output_image\":{\"data\":\"~a\",\"mime_type\":\"image/png\"}}"
          id +png+))

(defun video-answer (id timescale duration)
  (format nil "{\"id\":\"~a\",\"output_video\":{\"data\":\"~a\",\"mime_type\":\"video/mp4\"}}"
          id (cl-base64:usb8-array-to-base64-string (test-mp4 timescale duration))))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-media-directory ((directory) &body body) "BODY with DIRECTORY a fresh empty folder, removed afterwards."
  `(let ((,directory (merge-pathnames
                      (format nil "gemini-media-test-~d-~d/" (get-universal-time) (random 1000000))
                      (uiop:temporary-directory))))
     (ensure-directories-exist (merge-pathnames "keep" ,directory))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,directory :validate t)))))

(defmacro with-api ((asked &rest routes) &body body) "BODY with NLK:HTTP answering by URL and the key read from the section. ROUTES are
(FRAGMENT FORM), tried in order; FORM runs with METHOD, URL and OPTIONS bound and answers
(values TEXT STATUS), as NLK:HTTP does. ASKED collects (METHOD URL OPTIONS), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nle::credential-env (name)
                                   (declare (ignore name))
                                   nil)
         (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                     (push (list method url options) ,log)
                                     (cond ,@(loop for (fragment form) in routes
                                                   collect `((search ,fragment url) ,form))
                                           (t (error "unexpected API request to ~a" url))))
           (symbol-macrolet ((,asked (reverse ,log)))
             ,@body))))))

(defun gemini-media-answer (id tool arguments) "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun gemini-media-object (id tool arguments)
  (nlk:decode-json (gemini-media-answer id tool arguments)))

(defun sent (asked)
  (first asked))

(defun sent-payload (asked)
  (nlk:decode-json (getf (third (sent asked)) :content)))

(defun sent-header (asked name)
  (cdr (assoc name (getf (third (sent asked)) :headers) :test #'string=)))

;;; --- the lifecycle, from the one declaration ---------------------------------------

(define-cell-lifecycle-tests "gemini-media"
  (:config "api_url" "https://gemini.test/v1beta" "api_key" "test-key")
  (:help :gemini-media)
  (:command "gemini-media")
  (:refused ("api_url" "ftp://gemini.test/v1beta")
            ("api_url" "")
            ("image_timeout" 5)))

(deftest gemini-media-cell-without-an-api-url-refuses ()
  (let ((condition (signals-error nlk:config-refusal (gemini-media-start))))
    (is (search "api_url" (princ-to-string condition)) "the refusal names the setting")
    (is (null (nle::find-tool "gemini_image")) "and installs nothing")))

(deftest gemini-media-cell-takes-a-bare-host-for-the-api-root ()
  (with-cell-stop ((gemini-media-start "api_url" "https://gemini.test" "api_key" "test-key"))
    (is (functionp stop) "a host without a path is the API root")
    (is (equal "https://gemini.test/v1beta"
               (nodecode-gemini-media::setting :api-url)) "and gains the /v1beta the API answers on")))

(deftest gemini-media-cell-keeps-a-key-off-plain-http-to-a-public-host ()
  (let ((condition (signals-error nlk:config-refusal
                                  (gemini-media-start "api_url" "http://gemini.public.test/v1beta"
                                                      "api_key" "test-key"))))
    (is (search "clear text" (princ-to-string condition)) "a public plain-HTTP root is refused, because the key would ride in the open")))

;;; --- gemini_image ------------------------------------------------------------------

(deftest gemini-media-cell-generates-an-image ()
  (with-media-directory (directory)
    (with-cell-stop ((gemini-media-start "api_url" "https://gemini.test/v1beta"
                                         "api_key" "test-key"
                                         "output_dir" (namestring directory)))
      (is (nle::find-tool "gemini_image") "the tool is registered")
      (with-api (asked ("/interactions" (values (image-answer "int-1") 200)))
        (let* ((object (gemini-media-object
                        "i-1" "gemini_image"
                        "{\"prompt\":\"a red fox\",\"aspect\":\"landscape\",\"image_size\":\"4K\"}"))
               (path (nlk:json-value object :string "image")))
          (is (and path (probe-file path)) "the picture was written under output_dir")
          (is (plusp (or (nlk:json-value object :integer "bytes") 0)) "with its size")
          (is (equal "1x1" (nlk:json-value object :string "pixels")) "and the pixel size its header carries")
          (is (equal "generate" (nlk:json-value object :string "mode")))
          (is (equal "int-1" (nlk:json-value object :string "interaction_id")) "and the interaction the API named"))
        (destructuring-bind (method url options) (sent asked)
          (declare (ignore options))
          (is (eq :post method))
          (is (equal "https://gemini.test/v1beta/interactions" url))
          (is (equal "test-key" (sent-header asked "x-goog-api-key")) "the key rides as x-goog-api-key and nowhere else")
          (let* ((payload (sent-payload asked))
                 (format (nlk:json-value payload :object "response_format")))
            (is (equal "gemini-nano-banana-2.1" (nlk:json-value payload :string "model")) "the section's image model")
            (is (equal "16:9" (nlk:json-value format :string "aspect_ratio")) "the aspect's ratio")
            (is (equal "4K" (nlk:json-value format :string "image_size")))
            (is (equal "a red fox" (nlk:json-value payload :string "input")) "a text prompt alone is the input")))))))

(deftest gemini-media-cell-sends-a-reference-picture-inline ()
  (with-media-directory (directory)
    (let ((picture (merge-pathnames "fox.png" directory)))
      (alexandria:write-byte-vector-into-file (png-octets) picture)
      (with-cell-stop ((gemini-media-start "api_url" "https://gemini.test/v1beta"
                                           "api_key" "test-key"
                                           "output_dir" (namestring (merge-pathnames "out/" directory))))
        (with-api (asked ("/interactions" (values (image-answer "int-2") 200)))
          (let ((object (gemini-media-object
                         "i-2" "gemini_image"
                         (format nil "{\"prompt\":\"the same fox at dusk\",\"images\":[\"~a\"]}"
                                 (namestring picture)))))
            (is (equal "edit" (nlk:json-value object :string "mode")) "a reference makes it an edit")
            (is (= 1 (nlk:json-value object :integer "references_used"))))
          (let* ((payload (sent-payload asked))
                 (input (nlk:json-value payload :array "input")))
            (is (= 2 (length input)) "the picture rides as a part before the prompt")
            (is (equal "image" (nlk:json-value (aref input 0) :string "type")))
            (is (equal "image/png" (nlk:json-value (aref input 0) :string "mime_type")))
            (is (equal "the same fox at dusk" (nlk:json-value (aref input 1) :string "text")))))))))

(deftest gemini-media-cell-refuses-when-the-key-is-absent ()
  (with-media-directory (directory)
    (with-cell-stop ((gemini-media-start "api_url" "https://gemini.test/v1beta"
                                         "output_dir" (namestring directory)))
      (let ((answer (gemini-media-answer "i-3" "gemini_image" "{\"prompt\":\"a heron\"}")))
        (is (search "no Gemini API key" answer) "the answer names the missing key")
        (is (search "GEMINI_API_KEY" answer) "and the variable that holds it")))))

;;; --- gemini_video ------------------------------------------------------------------

(deftest gemini-media-cell-renders-a-video ()
  (with-media-directory (directory)
    (with-cell-stop ((gemini-media-start "api_url" "https://gemini.test/v1beta"
                                         "api_key" "test-key"
                                         "output_dir" (namestring directory)))
      (with-api (asked ("/interactions" (values (video-answer "int-4" 1000 8250) 200)))
        (let ((object (gemini-media-object
                       "v-1" "gemini_video"
                       "{\"prompt\":\"a fox crossing a snowy field\",\"duration\":8}")))
          (is (probe-file (nlk:json-value object :string "video")) "the clip was written")
          (is (equal 8 (nlk:json-value object :integer "duration")) "its length is read out of the MP4's mvhd atom")
          (is (equal "text_to_video" (nlk:json-value object :string "task"))))
        (let* ((payload (sent-payload asked))
               (format (nlk:json-value payload :object "response_format")))
          (is (equal "video" (nlk:json-value format :string "type")))
          (is (equal "8s" (nlk:json-value format :string "duration")) "the API takes the length as a string")
          (is (equal "720p" (nlk:json-value format :string "resolution")))
          (is (null (gethash "generation_config" payload)) "a text prompt sends no task: the original measured that value hanging"))))))

(deftest gemini-media-cell-makes-a-picture-the-first-frame ()
  (with-media-directory (directory)
    (let ((picture (merge-pathnames "fox.png" directory)))
      (alexandria:write-byte-vector-into-file (png-octets) picture)
      (with-cell-stop ((gemini-media-start "api_url" "https://gemini.test/v1beta"
                                           "api_key" "test-key"
                                           "output_dir" (namestring (merge-pathnames "out/" directory))))
        (with-api (asked ("/interactions" (values (video-answer "int-5" 1000 3000) 200)))
          (let ((object (gemini-media-object
                         "v-2" "gemini_video"
                         (format nil "{\"prompt\":\"she turns and looks at us\",\"image\":\"~a\",\"duration\":3}"
                                 (namestring picture)))))
            (is (equal "image_to_video" (nlk:json-value object :string "task")) "one picture given is the first frame"))
          (let* ((payload (sent-payload asked))
                 (config (nlk:json-value payload :object "generation_config"))
                 (video (nlk:json-value config :object "video_config")))
            (is (equal "image_to_video" (nlk:json-value video :string "task")) "and the task rides in generation_config.video_config")))))))

;;; --- gemini_models -----------------------------------------------------------------

(deftest gemini-media-cell-lists-the-models-the-key-offers ()
  (with-cell-stop ((gemini-media-start "api_url" "https://gemini.test/v1beta"
                                       "api_key" "test-key"))
    (with-api (asked ("/models"
                      (values (concatenate
                               'string
                               "{\"models\":["
                               "{\"name\":\"models/gemini-3-pro-image\",\"displayName\":\"Nano Banana Pro\","
                               "\"supportedGenerationMethods\":[\"generateContent\"]},"
                               "{\"name\":\"models/gemini-omni-1.1-flash\",\"supportedActions\":[\"predict\"]},"
                               "{\"name\":\"models/text-embedding-004\",\"supportedGenerationMethods\":[\"embedContent\"]}]}")
                              200)))
      (let* ((object (gemini-media-object "m-1" "gemini_models" "{\"kind\":\"all\"}"))
             (images (nlk:json-array object "image"))
             (videos (nlk:json-array object "video")))
        (is (equal "live" (nlk:json-value object :string "source")) "the key's own list is read")
        (is (= 1 (length images)) "one image model, the embedding model left out")
        (is (equal "gemini-3-pro-image" (nlk:json-value (aref images 0) :string "id")))
        (is (= 1 (length videos)) "and one video model")
        (is (equal "gemini-omni-1.1-flash" (nlk:json-value (aref videos 0) :string "id")))))))

;;; --- /gemini-media ------------------------------------------------------------------

^(deftest gemini-media-cell-command-asks-when-typed-alone ()
  (with-cell-stop ((gemini-media-start "api_url" "https://gemini.test/v1beta"
                                       "api_key" "test-key"))
    (multiple-value-bind (text dialog) (nodecode-gemini-media:media-command "" nil)
      (is (search "Describe the picture" text) "typed alone it asks for the prompt")
      (is (and dialog (getf dialog :rows)) "and offers a row for each kind"))))

^(deftest gemini-media-cell-command-renders-an-image ()
  (with-media-directory (directory)
    (with-cell-stop ((gemini-media-start "api_url" "https://gemini.test/v1beta"
                                         "api_key" "test-key"
                                         "output_dir" (namestring directory)))
      (with-api (asked ("/interactions" (values (image-answer "int-6") 200)))
        (multiple-value-bind (text dialog)
            (nodecode-gemini-media:media-command "--landscape a heron at dusk" nil)
          (declare (ignore dialog))
          (is (search "gemini-image-" text) "the answer names the file it wrote"))
        (let* ((payload (sent-payload asked))
               (format (nlk:json-value payload :object "response_format")))
          (is (equal "16:9" (nlk:json-value format :string "aspect_ratio"))))))))
