;;;; cell-test.lisp --- lifecycle, the routing rule, both paths, /vision.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and its one rule: a healthy
;;;; start and a stop that takes everything back, a section the declaration
;;;; refuses, a section that names no captioner while it needs one, ROUTE-FOR
;;;; for each pair of mode and model, an image attached inline with no request
;;;; sent, the captioner path with its stub, a URL fetched and then attached,
;;;; a file that is not an image, a session with no turn to attach to, and
;;;; /vision with and without an argument. Every request is a stubbed NLK:HTTP,
;;;; so nothing reaches the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defvar *attached* '()
  "Every image the cell handed to the model, (MEDIA-TYPE LENGTH) newest first.")

(defmacro with-vision-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials a captioner; and with ATTACH-TO-TURN
replaced by a recorder, so no test needs a live turn."
  `(let ((*requests* '())
         (*attached* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected image request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       (let ((nodecode-vision-inline::*attach*
               (lambda (octets media-type)
                 (push (list media-type (length octets)) *attached*)
                 "c1")))
         ,@body))))

(defmacro with-seeing-model ((verdict) &body body)
  "BODY with models.dev answering VERDICT for this session's model."
  `(with-stubbed-fdefinition (nodecode.evolved::model-reads-images-p
                                 (model &optional provider)
                                 (declare (ignore model provider))
                                 ,verdict)
     ,@body))

(defmacro without-a-turn (&body body)
  "BODY with the inline path declining, as it does when no turn is running."
  `(let ((nodecode-vision-inline::*attach* (lambda (octets media-type)
                                             (declare (ignore octets media-type))
                                             nil)))
     ,@body))

(defun vision-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun vision-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun vision-request-body (request)
  (nlk:decode-json (getf (cddr request) :content)))

(defun vision-request-header (request name)
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun vision-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun vision-bytes (text)
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defparameter +png-signature+
  (let ((magic (make-array 8 :element-type '(unsigned-byte 8)
                              :initial-contents '(#x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A))))
    magic)
  "The eight bytes that make bytes a PNG and nothing else.")

(defun vision-png ()
  (coerce (concatenate 'vector +png-signature+ (vision-bytes "hello")) '(vector (unsigned-byte 8))))

(defun vision-scratch-directory ()
  (let ((directory (merge-pathnames (format nil "nodecode-vision-inline-test-~d/"
                                            (random 1000000))
                                    (uiop:temporary-directory))))
    (ensure-directories-exist directory)
    directory))

(defun vision-scratch-image (&optional (name "shot.png") (bytes (vision-png)))
  "BYTES written at NAME in a scratch directory => the pathname."
  (let ((path (merge-pathnames name (vision-scratch-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
      (write-sequence bytes out))
    path))

(defun vision-message-content (body)
  "BODY's first message content, the array of parts."
  (let* ((messages (nlk:json-value body :array "messages"))
         (message (and messages (aref messages 0))))
    (nlk:json-value message :array "content")))

(defmacro is-carrying-form (form &body needles)
  "Assert that FORM's answer carries every needle; a list of needles is one group."
  `(let ((answer ,form))
     ,@(loop for needle in (loop for given in needles
                                 if (consp given) append given else collect given)
             collect `(is (search ,needle answer)
                          (format nil "~s carries ~s" answer ,needle)))))

;;; --- lifecycle ------------------------------------------------------------------

(define-test-slice "vision-inline" "VISION-INLINE-CELL-"
  :start nodecode-vision-inline:start-cell)

(define-cell-lifecycle-tests "vision-inline"
  (:fixture with-vision-wire)
  (:config "api_base" "https://cap.test/v1" "model" "cap-1" "api_key" "k-test")
  (:help :vision-inline)
  (:command "vision")
  (:running (is (nle::find-tool "see_image") "see_image is registered")
            (is (equal "auto" (nodecode-vision-inline::setting :image-input-mode)))
            (is (equal "https://cap.test/v1" (nodecode-vision-inline::caption-base)))
            (is (equal "cap-1" (nodecode-vision-inline::caption-model)))
            (is (= 5 (nodecode-vision-inline::setting :max-file-mb)) "the default ceiling")
            (is (equal "Bearer k-test"
                       (cdr (assoc "authorization" (nodecode-vision-inline::auth-headers)
                                   :test #'equal)))))
  (:stopped (is (null (nle::find-tool "see_image")) "stop unregisters the tool"))
  (:refused ("image_input_mode" "sometimes") ("max_tokens" 10) ("image_input_mode" "text")))

(deftest vision-inline-cell-a-captioner-is-required-when-the-mode-says-text ()
  (with-vision-wire
    (is-carrying-form (refusal-text nlk:config-refusal
                                    (vision-inline-start "image_input_mode" "text"))
                      ("api_base" "captioner"))
    (is-carrying-form (refusal-text nlk:config-refusal
                                    (vision-inline-start "image_input_mode" "text"
                                                         "api_base" "https://cap.test/v1"))
                      ("model" "captioning model"))
    (is (null (nle::find-tool "see_image")) "a refused start installed nothing")
    (is-carrying-form (refusal-text nlk:config-refusal
                                    (vision-inline-start "api_base" "http://cap.test/v1"
                                                         "model" "cap-1"))
                      ("not HTTPS" "allow_insecure_http"))))

(deftest vision-inline-cell-nothing-is-required-while-the-model-sees ()
  (with-vision-wire
    (with-seeing-model (t)
      (with-cell-stop ((vision-inline-start))
        (is (equal "" (nodecode-vision-inline::caption-base)) "no captioner")
        (is (eq :inline (nodecode-vision-inline::route)) "and the model still reads the image")))))

;;; --- the one rule ----------------------------------------------------------------

(defun vision-route (mode sees)
  "ROUTE-FOR, under a short name for the test's lines."
  (nodecode-vision-inline:route-for mode sees))

(deftest vision-inline-cell-the-route-is-mode-and-model-together ()
  (is (eq :inline (vision-route "auto" t)))
  (is (eq :inline (vision-route "auto" :unknown)) "an unknown model is tried first")
  (is (eq :caption (vision-route "auto" nil)))
  (is (eq :caption (vision-route "text" t)) "text always asks the captioner"))

(deftest vision-inline-cell-base64-encodes-the-rfc-vectors ()
  (is (equal "" (nodecode-vision-inline:base64-encode
                  (make-array 0 :element-type '(unsigned-byte 8)))))
  (is (equal "TWFu" (nodecode-vision-inline:base64-encode (vision-bytes "Man"))))
  (is (equal "TWE=" (nodecode-vision-inline:base64-encode (vision-bytes "Ma"))))
  (is (equal "TQ==" (nodecode-vision-inline:base64-encode (vision-bytes "M")))))

;;; --- the inline path ---------------------------------------------------------------

(deftest vision-inline-cell-an-image-goes-to-a-model-that-sees ()
  (with-vision-wire
    (with-seeing-model (t)
      (with-cell-stop ((vision-inline-start "api_base" "https://cap.test/v1" "model" "cap-1"))
        (let* ((path (vision-scratch-image))
               (answer (vision-tool "see_image"
                                    (nlk:json-object "image" (namestring path)
                                                     "question" "what colour is it?"))))
          (is-carrying-form answer ("shot.png" "image/png" "attached to this turn")
                            ("next round") ("kept in the conversation"))
          (is (equal '(("image/png" 13)) *attached*) "the bytes went to the turn")
          (is (null (vision-asked)) "and nothing was sent anywhere"))))))

(deftest vision-inline-cell-a-url-is-fetched-and-then-attached ()
  (with-vision-wire
    (with-seeing-model (:unknown)
      (with-cell-stop ((vision-inline-start))
        (setf *answer-fn* (lambda (method url args)
                            (declare (ignore args))
                            (is (eq :get method))
                            (is (equal "https://img.test/a.png" url))
                            (values (vision-png) 200)))
        (let ((answer (vision-tool "see_image"
                                    (nlk:json-object "image" "https://img.test/a.png"))))
          (is-carrying-form answer ("https://img.test/a.png" "image/png") ("attached"))
          (is (equal '(("image/png" 13)) *attached*))
          (is (= 1 (length (vision-asked)))))))))

(deftest vision-inline-cell-a-file-that-is-not-an-image-is-a-repair ()
  (with-vision-wire
    (with-seeing-model (t)
      (with-cell-stop ((vision-inline-start "api_base" "https://cap.test/v1" "model" "cap-1"))
        (let ((path (vision-scratch-image "notes.txt" (vision-bytes "plain text"))))
          (is-carrying-form (vision-tool "see_image" (nlk:json-object "image" (namestring path)))
                            ("ERROR: vision-inline:" "not a PNG, JPEG, GIF or WebP"))
          (is (null *attached*) "nothing was attached")
          (is (null (vision-asked)) "and nothing was sent"))
        (is-carrying-form (vision-tool "see_image"
                                        (nlk:json-object "image" "/nonexistent/a.png"))
                          ("no file at"))))))

;;; --- the captioner path ---------------------------------------------------------------

(deftest vision-inline-cell-a-text-only-model-asks-the-captioner ()
  (with-vision-wire
    (with-seeing-model (nil)
      (with-cell-stop ((vision-inline-start "api_base" "https://cap.test/v1" "model" "cap-1"
                                            "api_key" "k-test" "max_tokens" 256))
        (vision-answer "{\"choices\":[{\"message\":{\"content\":\"a red fox in snow\"}}]}")
        (let* ((path (vision-scratch-image))
               (answer (vision-tool "see_image"
                                    (nlk:json-object "image" (namestring path)
                                                     "question" "what is in it?"))))
          (is-carrying-form answer ("read by the captioner cap-1") ("a red fox in snow"))
          (is (null *attached*) "no image was attached")
          (let ((request (first (vision-asked))))
            (is (= 1 (length (vision-asked))) "one request for one image")
            (is (eq :post (first request)))
            (is (equal "https://cap.test/v1/chat/completions" (second request)))
            (is (equal "Bearer k-test" (vision-request-header request "authorization")))
            (let* ((body (vision-request-body request))
                   (content (vision-message-content body)))
              (is (equal "cap-1" (nlk:json-value body :string "model")))
              (is (= 256 (nlk:json-value body :integer "max_tokens")))
              (is (= 2 (length content)) "the image and the question")
              (is (equal "image_url" (nlk:json-value (aref content 0) :string "type")))
              (is (search "data:image/png;base64,"
                          (nlk:json-value (aref content 0) :string "image_url" "url")))
              (is (equal "what is in it?" (nlk:json-value (aref content 1) :string "text"))))))))))

(deftest vision-inline-cell-text-mode-captions-even-when-the-model-sees ()
  (with-vision-wire
    (with-seeing-model (t)
      (with-cell-stop ((vision-inline-start "image_input_mode" "text"
                                            "api_base" "https://cap.test/v1" "model" "cap-1"))
        (vision-answer "{\"choices\":[{\"message\":{\"content\":\"a small chart\"}}]}")
        (let ((answer (vision-tool "see_image"
                                    (nlk:json-object "image"
                                                     (namestring (vision-scratch-image))))))
          (is-carrying-form answer ("a small chart") ("captioner"))
          (is (null *attached*))
          (let ((asked (nlk:json-value
                        (aref (vision-message-content
                               (vision-request-body (first (vision-asked))))
                              1)
                        :string "text")))
            (is (equal nodecode-vision-inline::+default-question+ asked) "the built-in question stands in")))))))

(deftest vision-inline-cell-a-session-with-no-turn-captions ()
  (with-vision-wire
    (with-seeing-model (t)
      (with-cell-stop ((vision-inline-start "api_base" "https://cap.test/v1" "model" "cap-1"))
        (without-a-turn
          (vision-answer "{\"choices\":[{\"message\":{\"content\":\"a fox\"}}]}")
          (let ((answer (vision-tool "see_image"
                                      (nlk:json-object "image"
                                                       (namestring (vision-scratch-image))))))
            (is-carrying-form answer ("No turn is running") ("a fox"))))))))

(deftest vision-inline-cell-a-captioner-that-refuses-is-a-repair ()
  (with-vision-wire
    (with-seeing-model (nil)
      (with-cell-stop ((vision-inline-start "api_base" "https://cap.test/v1" "model" "cap-1"
                                            "api_key" "k"))
        (vision-answer "{\"error\":{\"message\":\"Invalid token\"}}" 401)
        (is-carrying-form (vision-tool "see_image"
                                        (nlk:json-object "image"
                                                         (namestring (vision-scratch-image))))
                          ("ERROR: vision-inline:" "HTTP 401" "Invalid token"))))))

(deftest vision-inline-cell-no-captioner-is-a-repair-that-names-the-member ()
  (with-vision-wire
    (with-seeing-model (nil)
      (with-cell-stop ((vision-inline-start))
        (is-carrying-form (vision-tool "see_image"
                                        (nlk:json-object "image"
                                                         (namestring (vision-scratch-image))))
                          ("ERROR: vision-inline:" "\"api_base\"" "api_base\" and \"model\""))))))

;;; --- /vision -------------------------------------------------------------------------

(deftest vision-inline-cell-the-command-shows-the-route-or-sees-one-image ()
  (with-vision-wire
    (with-seeing-model (t)
      (with-cell-stop ((vision-inline-start "api_base" "https://cap.test/v1" "model" "cap-1"))
        (let ((answer (cell-entry "nodecode-vision-inline" "vision" "")))
          (is (search "image_input_mode auto" answer))
          (is (search "this session's model takes images" answer))
          (is (search "the session model inline" answer))
          (is (search "https://cap.test/v1, model cap-1" answer))
          (is (null (vision-asked)) "a status read asks nobody anything"))
        (let ((answer (cell-entry "nodecode-vision-inline" "vision"
                                  (namestring (vision-scratch-image)))))
          (is (search "attached to this turn" answer))
          (is (equal '(("image/png" 13)) *attached*)))
        (is (search "no file at"
                    (cell-entry "nodecode-vision-inline" "vision" "/nonexistent/a.png")))))))
