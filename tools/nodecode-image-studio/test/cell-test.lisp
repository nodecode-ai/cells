;;;; cell-test.lisp --- the image-studio cell against a stubbed provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every provider request is a stubbed NLK:HTTP and every key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches FAL, Vertex AI, the environment or
;;;; the network. Each tool is called the way a model's call reaches it -
;;;; through the real dispatch, contract and all - so the schema the provider
;;;; is shown and the body that runs are one shape. The pictures are written
;;;; under a fresh folder and removed with it.

(in-package #:nodecode.test)

;;; --- the slice ------------------------------------------------------------------

(define-test-slice "image-studio" "IMAGE-STUDIO-CELL-")

(defun image-studio-config (&rest pairs)
  "A whole shared config holding the `image-studio' section PAIRS make."
  (nlk:json-object "image-studio" (apply #'nlk:make-json-object pairs)))

(defun image-studio-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-image-studio:start-cell (apply #'image-studio-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defparameter +png+
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  "A 1x1 PNG in base64: what a provider answers, and what a reference image is.")

(defun png-octets ()
  (cl-base64:base64-string-to-usb8-array +png+))

(defmacro with-image-studio-directory ((directory) &body body)
  "BODY with DIRECTORY a fresh empty folder, removed afterwards."
  `(let ((,directory (merge-pathnames
                      (format nil "image-studio-test-~d-~d/" (get-universal-time) (random 1000000))
                      (uiop:temporary-directory))))
     (ensure-directories-exist (merge-pathnames "keep" ,directory))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,directory :validate t)))))

(defmacro with-provider-keys ((&key (fal "k-fal") (vertex "k-vertex")) &body body)
  "BODY with NLE::CREDENTIAL-ENV answering the two keys, and nothing else: no
test reads the environment it runs in."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (cond ((equal name "FAL_KEY") ,fal)
                                    ((equal name "VERTEX_API_KEY") ,vertex)
                                    (t nil)))
     ,@body))

(defmacro with-studio ((asked &rest routes) &body body)
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
                                         (t (error "unexpected provider request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun image-studio-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun image-studio-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded, or the refusal it answered with."
  (let ((text (image-studio-answer id tool arguments)))
    (handler-case (nlk:decode-json text)
      (error () (error "the tool answered ~s" text)))))

(defun studio-sent (asked method)
  "The one request of METHOD, and what it carried."
  (find method asked :key #'first))

;;; --- what the providers answer ---------------------------------------------------

(defun fal-queued-answer ()
  "The queue's answer to a submission: one job, and where to look for it."
  "{\"request_id\":\"r-1\",
    \"status_url\":\"https://queue.fal.run/fal-ai/fox/requests/r-1/status\",
    \"response_url\":\"https://queue.fal.run/fal-ai/fox/requests/r-1\"}")

(defun vertex-answer ()
  "The answer generateContent gives for one picture."
  (format nil "{\"candidates\":[{\"content\":{\"parts\":[{\"inlineData\":{\"mimeType\":\"image/png\",\"data\":\"~a\"}}]}}]}"
          +png+))

(defmacro with-fal-picture ((&key queued) &body body)
  "BODY over a stubbed FAL queue that answers one picture at cdn.fal.test."
  `(with-studio (asked
                  ("cdn.fal.test" (values (png-octets) 200))
                  ("/status" (values "{\"status\":\"COMPLETED\"}" 200))
                  ("/requests/r-1" (values "{\"images\":[{\"url\":\"https://cdn.fal.test/fox.png\"}]}" 200))
                  ("queue.fal.run" (values (or ,queued (fal-queued-answer)) 200)))
     ,@body))

;;; --- the lifecycle, from the one declaration -------------------------------------

(define-cell-lifecycle-tests "image-studio"
  (:config "fal_key_env" "FAL_KEY")
  (:help :image-studio)
  (:command "image-studio")
  (:running (is (nle::find-tool "generate_image") "generate_image is registered")
            (is (nle::find-tool "image_gallery") "image_gallery is registered")
            (is (equal "fal" (nodecode-image-studio::setting :provider)) "fal is the provider a call names none with")
            (is (= 200 (nodecode-image-studio::setting :gallery-cap)) "the gallery keeps the newest 200"))
  (:refused ("provider" "midjourney") ("gallery_cap" 0) ("timeout" 5))
  (:idle nodecode-image-studio:image-studio-error
         (nodecode-image-studio:image-studio-command "" nil)))

(deftest image-studio-cell-a-keyless-provider-refuses-by-name ()
  (with-image-studio-directory (directory)
    (with-provider-keys (:fal nil :vertex nil)
      (with-cell-stop ((image-studio-start "output_dir" (namestring directory)))
        (with-studio (asked)
          (let ((text (image-studio-answer "is-1" "generate_image" "{\"prompt\":\"a red fox\"}")))
            (is (search "ERROR: image-studio" text) "the answer is a refusal the model can repair from")
            (is (search "fal_key_env" text) "naming the member that holds the key")
            (is (null asked) "and nothing was sent")))))))

;;; --- FAL ---------------------------------------------------------------------------

(deftest image-studio-cell-draws-on-fal ()
  (with-image-studio-directory (directory)
    (with-provider-keys ()
      (with-cell-stop ((image-studio-start "output_dir" (namestring directory)))
        (with-fal-picture ()
          (let* ((nodecode-image-studio::*poll-seconds* 0)
                 (object (image-studio-object "is-2" "generate_image"
                                               "{\"prompt\":\"a red fox in snow\",\"aspect_ratio\":\"16:9\"}")))
            (let ((image (aref (nlk:json-value object :array "images") 0)))
              (is (probe-file (nlk:json-value image :string "path")) "the picture was written under output_dir")
              (is (plusp (nlk:json-value image :integer "bytes")) "with its size in bytes")
              (is (equal "png" (nlk:json-value image :string "format")) "and the format its first bytes carry"))
            (is (equal "fal" (nlk:json-value object :string "provider")))
            (let ((post (studio-sent asked :post)))
              (is (equal "https://queue.fal.run/fal-ai/flux-2-pro" (second post)) "the default FAL model was asked for")
              (is (equal "Key k-fal" (cdr (assoc "authorization" (getf (third post) :headers) :test #'equal))) "the key rides in the header and nowhere else")
              (let ((payload (nlk:decode-json (getf (third post) :content))))
                (is (equal "a red fox in snow" (nlk:json-value payload :string "prompt")))
                (is (equal "16:9" (nlk:json-value payload :string "aspect_ratio")))
                (is (= 1 (nlk:json-value payload :integer "num_images")))))
            (is (probe-file (merge-pathnames "gallery.json" directory)) "and the gallery index holds it")))))))

(deftest image-studio-cell-reports-a-provider-failure ()
  (with-image-studio-directory (directory)
    (with-provider-keys ()
      (with-cell-stop ((image-studio-start "output_dir" (namestring directory)))
        (with-studio (asked ("queue.fal.run" (values "{\"message\":\"Insufficient credit\"}" 429)))
          (let ((text (image-studio-answer "is-3" "generate_image" "{\"prompt\":\"a heron\"}")))
            (is (search "HTTP 429" text) "the status the provider answered")
            (is (search "Insufficient credit" text) "the provider's own words")
            (is (search "rate limiting" text) "and the hint that usually fixes it")))))))

;;; --- Vertex AI ---------------------------------------------------------------------

(deftest image-studio-cell-draws-on-vertex-from-a-reference ()
  (with-image-studio-directory (directory)
    (let ((source (merge-pathnames "ref.png" directory)))
      (alexandria:write-byte-vector-into-file (png-octets) source :if-exists :supersede)
      (with-provider-keys ()
        (with-cell-stop ((image-studio-start "output_dir" (namestring directory)))
          (with-studio (asked ("generateContent" (values (vertex-answer) 200)))
            (let ((object (image-studio-object
                           "is-4" "generate_image"
                           (format nil "{\"prompt\":\"the same fox at dusk\",\"provider\":\"vertex\",\"resolution\":\"2K\",\"references\":[\"~a\"]}"
                                   (namestring source)))))
              (let ((image (aref (nlk:json-value object :array "images") 0)))
                (is (probe-file (nlk:json-value image :string "path")) "the inline bytes were written to a file"))
              (is (equal "vertex" (nlk:json-value object :string "provider")))
              (is (equal "gemini-nano-banana-2.1" (nlk:json-value object :string "model")) "the Vertex AI default"))
            (let* ((post (studio-sent asked :post))
                   (payload (nlk:decode-json (getf (third post) :content))))
              (is (search "gemini-nano-banana-2.1:generateContent" (second post)))
              (is (equal "k-vertex" (cdr (assoc "x-goog-api-key" (getf (third post) :headers) :test #'equal))) "the Google key rides in its own header")
              (let* ((parts (nlk:json-value (aref (nlk:json-value payload :array "contents") 0) :array "parts"))
                     (config (nlk:json-value (nlk:json-value payload :object "generationConfig") :object "imageConfig")))
                (is (= 2 (length parts)) "the reference and the prompt")
                (is (nlk:json-value (aref parts 0) :object "inlineData") "the reference's bytes ride inline")
                (is (equal "2K" (nlk:json-value config :string "imageSize")))
                (is (equal "1:1" (nlk:json-value config :string "aspectRatio")))))))))))

;;; --- the gallery -------------------------------------------------------------------

(deftest image-studio-cell-lists-the-gallery-newest-first ()
  (with-image-studio-directory (directory)
    (with-provider-keys ()
      (with-cell-stop ((image-studio-start "output_dir" (namestring directory)))
        (with-studio (asked)
          (nodecode-image-studio::record-gallery
           (nlk:json-object "path" "older.png" "prompt" "older" "at" "2026-01-01T00:00:00Z"))
          (nodecode-image-studio::record-gallery
           (nlk:json-object "path" "newer.png" "prompt" "newer" "at" "2026-01-02T00:00:00Z"))
          (let* ((object (image-studio-object "is-5" "image_gallery" "{\"limit\":1}"))
                 (images (nlk:json-value object :array "images")))
            (is (= 2 (nlk:json-value object :integer "total")) "the whole gallery is counted")
            (is (= 1 (length images)) "and limit bounds what is listed")
            (is (equal "newer.png" (nlk:json-value (aref images 0) :string "path")) "newest first")
            (is (null asked) "nothing was sent to a provider")))))))

;;; --- /image-studio ------------------------------------------------------------------

(deftest image-studio-cell-answers-the-command ()
  (with-image-studio-directory (directory)
    (with-provider-keys ()
      (with-cell-stop ((image-studio-start "output_dir" (namestring directory)))
        (multiple-value-bind (text panel) (nodecode-image-studio:image-studio-command "" nil)
          (is (search "No pictures yet" text) "typed alone it says what the gallery holds")
          (let ((dialog (nle::dialog-json panel)))
            (is (equal '("/image-studio")
                       (remove nil (map 'list (lambda (row) (nle::json-value row :text "ask" "line"))
                                        (nle::json-array dialog "rows")))) "and asks for the prompt")))
        (with-fal-picture ()
          (let ((nodecode-image-studio::*poll-seconds* 0))
            (multiple-value-bind (text panel) (nodecode-image-studio:image-studio-command
                                                "a red fox --aspect 4:5" nil)
              (declare (ignore panel))
              (is (probe-file text) "a prompt writes one picture and answers its path"))
            (let ((payload (nlk:decode-json (getf (third (studio-sent asked :post)) :content))))
              (is (equal "4:5" (nlk:json-value payload :string "aspect_ratio")) "the flag sets the shape"))))))))
