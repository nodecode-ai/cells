;;;; cell-test.lisp --- lifecycle, each tool, /fashio, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a member under its floor or a host without a scheme refused, a key
;;;; variable with nothing in it refused), each tool through the seam the model
;;;; calls it by -- its URL, its headers, its body and the answer it builds --
;;;; the confirmation every paid call needs, an image read by signature, and
;;;; /fashio with and without an argument. Every request is a stubbed NLK:HTTP
;;;; and the key a stubbed NLE::CREDENTIAL-ENV, so nothing reaches Fashio, the
;;;; network or the machine's environment.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *fashio-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *fashio-answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-fashio-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *FASHIO-REQUESTS* and
answered by *FASHIO-ANSWER-FN*, so no test dials Fashio. NLE::CREDENTIAL-ENV
answers the Fashio key for FASHIO_API_KEY and nothing else, so the machine's
own environment cannot reach a test that did not ask for it."
  `(let ((*fashio-requests* '())
         (*fashio-answer-fn* (lambda (method url args)
                                (declare (ignore method args))
                                (error "unexpected request to ~a" url))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *fashio-requests*)
                                (funcall *fashio-answer-fn* method url args))
       (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and (equal name "FASHIO_API_KEY") "fio_live_test"))
         ,@body))))

(defun fashio-answer (text &optional (status 200))
  "The stubbed wire answers TEXT, STATUS from here on."
  (setf *fashio-answer-fn* (lambda (method url args)
                              (declare (ignore method url args))
                              (values text status))))

(defun fashio-asked ()
  "The stubbed requests, oldest first."
  (reverse *fashio-requests*))

(defun fashio-last-request ()
  "The newest stubbed request."
  (first *fashio-requests*))

(defun fashio-request-url (request)
  "REQUEST's URL."
  (second request))

(defun fashio-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun fashio-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun fashio-ok (&rest pairs)
  "A Fashio success answer carrying PAIRS, as the wire sends it."
  (let ((body (nlk:json-object "success" t)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key body) value))
    (nlk:encode-json-object body)))

(defun fashio-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

(defmacro with-png-file ((var) &body body)
  "VAR bound to a small PNG's path -- its eight signature bytes, nothing else
-- written beside the run's temp files and deleted however BODY leaves."
  `(let ((,var (merge-pathnames (format nil "fashio-test-~a.png" (get-universal-time))
                                 (uiop:temporary-directory))))
     (with-open-file (out ,var :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8))
       (write-sequence (coerce (list #x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A #x00 #x00 #x00 #x00)
                               '(vector (unsigned-byte 8)))
                       out))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file ,var)))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "fashio" "FASHIO-CELL-"
  :start nodecode-fashio:start-cell)

(define-cell-lifecycle-tests "fashio"
  (:fixture with-fashio-wire)
  (:help :fashio)
  (:command "fashio")
  (:running (is (nle::find-tool "fashio_account") "fashio_account is registered")
            (is (nle::find-tool "fashio_catalog_job") "fashio_catalog_job is registered")
            (is (equal "https://api.fashiolabs.com" (nodecode-fashio::setting :base-url)) "Fashio's own host by default")
            (is (equal "fio_live_test" (nodecode-fashio::setting :key)) "the key comes from the variable the section names"))
  (:stopped (is (null (nle::find-tool "fashio_account")) "stop unregisters the tools")
            (is (null (nle::find-tool "fashio_generate")) "every one of them"))
  (:refused ("timeout_seconds" 0) ("list_limit" 0) ("base_url" "api.fashiolabs.com"))
  (:idle nodecode-fashio:fashio-error
         (nodecode-fashio:generate nil nil :confirm t)))

;;; --- the account ----------------------------------------------------------------

(deftest fashio-cell-account-reads-the-balance (with-fashio-wire)
  (fashio-answer (fashio-ok "diamondBalance" 214
                             "plan" (nlk:json-object "status" "active" "renewsAt" "2026-09-27T00:00:00.000Z")))
  (with-cell-stop ((fashio-start))
    (is-carrying (answer (fashio-tool "fashio_account" (nlk:json-object)))
                 ("214 diamonds" "the balance is said")
                 ("plan active" "with the plan's status")
                 ("renews 2026-09-27" "and when it renews")))
  (let ((request (first (fashio-asked))))
    (is (equal "https://api.fashiolabs.com/v1/account" (fashio-request-url request)) "the account endpoint")
    (is (equal "Bearer fio_live_test" (fashio-request-header request "authorization")) "the key rides as a bearer")))

;;; --- the catalogues ---------------------------------------------------------------

(deftest fashio-cell-catalog-lists-tools-with-their-price (with-fashio-wire)
  (fashio-answer (fashio-ok "models" (vector (nlk:json-object "id" "virtual_model" "inputShape" "model_garment"
                                                               "fallbackPriceDiamonds" 4)
                                              (nlk:json-object "id" "remove_background"
                                                               "fallbackPriceDiamonds" 1))))
  (with-cell-stop ((fashio-start))
    (is-carrying (answer (fashio-tool "fashio_catalog" (nlk:json-object "kind" "tools")))
                 ("virtual_model" "the tool id")
                 ("model_garment" "what it needs")
                 ("4 diamonds" "its fallback price")
                 ("remove_background" "and the free-ish one")))
  (let ((url (fashio-request-url (first (fashio-asked)))))
    (is (equal "https://api.fashiolabs.com/v1/models" url) "the tools endpoint")))

(deftest fashio-cell-catalog-carries-gender-and-limit (with-fashio-wire)
  (fashio-answer (fashio-ok "models" (vector (nlk:json-object "id" "6a2f811f" "name" "Ava" "gender" "female"))))
  (with-cell-stop ((fashio-start))
    (is-carrying (answer (fashio-tool "fashio_catalog" (nlk:json-object "kind" "fashion_models"
                                                                        "gender" "female" "limit" 3)))
                 ("Ava" "the model's name")
                 ("6a2f811f" "and the id a generation takes")))
  (let ((url (fashio-request-url (first (fashio-asked)))))
    (is (equal "https://api.fashiolabs.com/v1/fashion-models?gender=female&limit=3" url) "the gender and the limit ride the query")))

(deftest fashio-cell-a-catalogue-kind-outside-the-list-refuses (with-fashio-wire)
  (with-cell-stop ((fashio-start))
    (is-carrying (answer (fashio-tool "fashio_catalog" (nlk:json-object "kind" "shoes")))
                 ("ERROR:" "a call the declaration does not allow is refused before it is sent")
                 ("shoes" "naming what was asked for"))
    (let ((refusal (refusal-text nodecode-fashio:fashio-error (nodecode-fashio:catalog "shoes"))))
      (is (search "is not one of" refusal) "the verb refuses it too")))
  (is (null (fashio-asked)) "and nothing was sent"))

;;; --- a generation -------------------------------------------------------------------

(deftest fashio-cell-a-paid-generation-waits-for-confirmation (with-fashio-wire)
  (with-cell-stop ((fashio-start))
    (is-carrying (answer (fashio-tool "fashio_generate"
                                       (nlk:json-object "tool_id" "virtual_model"
                                                        "images" (nlk:json-object "garment_photo" "https://example.com/g.jpg"))))
                 ("was not confirmed" "the call is refused")
                 ("no test mode" "saying why")
                 ("confirm: true" "and what to send instead")))
  (is (null (fashio-asked)) "nothing was charged"))

(deftest fashio-cell-a-confirmed-generation-starts-a-job (with-fashio-wire)
  (fashio-answer (fashio-ok "id" "AbCdEf123" "status" "processing"))
  (with-cell-stop ((fashio-start))
    (is-carrying (answer (fashio-tool "fashio_generate"
                                       (nlk:json-object "tool_id" "virtual_model"
                                                        "images" (nlk:json-object "garment_photo" "https://example.com/g.jpg"
                                                                                  "fashion_model" "6a2f811f")
                                                        "pose" "contrapposto_elegance"
                                                        "ratio" "4:5"
                                                        "confirm" t)))
                 ("AbCdEf123" "the job id")
                 ("fashio_generation" "and how to poll it")))
  (let* ((request (first (fashio-asked)))
         (body (fashio-request-body request)))
    (is (equal "https://api.fashiolabs.com/v1/generations" (fashio-request-url request)) "the generations endpoint")
    (is (equal "virtual_model" (nlk:json-value body :string "toolId")) "the tool id goes as toolId")
    (is (equal "contrapposto_elegance" (nlk:json-value body :string "pose")) "the pose")
    (is (equal "4:5" (nlk:json-value body :string "ratio")) "the ratio")
    (let ((images (nlk:json-value body :object "images")))
      (is (equal "https://example.com/g.jpg" (nlk:json-value images :string "garment_photo")) "the image fields go under images"))))

(deftest fashio-cell-a-generation-without-a-tool-or-preset-refuses (with-fashio-wire)
  (with-cell-stop ((fashio-start))
    (let ((call (nlk:json-object "images" (nlk:json-object "garment_photo" "https://example.com/g.jpg") "confirm" t)))
      (is (search "needs a tool_id" (fashio-tool "fashio_generate" call)) "a generation names what to run"))))

(deftest fashio-cell-a-generation-is-polled-by-id (with-fashio-wire)
  (fashio-answer (fashio-ok "id" "AbCdEf123" "status" "completed"
                             "resultUrls" (vector "https://cdn.example.com/out-1.png" "https://cdn.example.com/out-2.png")))
  (with-cell-stop ((fashio-start))
    (is-carrying (answer (fashio-tool "fashio_generation" (nlk:json-object "id" "AbCdEf123")))
                 ("completed" "the status")
                 ("https://cdn.example.com/out-1.png" "the first result")
                 ("https://cdn.example.com/out-2.png" "and the second")))
  (let ((url (fashio-request-url (first (fashio-asked)))))
    (is (equal "https://api.fashiolabs.com/v1/generations/AbCdEf123" url) "the job's own path")))

(deftest fashio-cell-a-failed-status-and-an-http-refusal-are-text (with-fashio-wire)
  (fashio-answer (fashio-ok "id" "Job9" "status" "failed" "resultUrls" #()
                             "errorMessage" "the model could not be applied"))
  (with-cell-stop ((fashio-start))
    (is-carrying (answer (fashio-tool "fashio_generation" (nlk:json-object "id" "Job9")))
                 ("failed" "a failed job")
                 ("the model could not be applied" "with what Fashio said")))
  (fashio-answer "{\"success\":false,\"error\":\"insufficient diamond balance\"}" 402)
  (with-cell-stop ((fashio-start))
    (is-carrying (answer (fashio-tool "fashio_generation" (nlk:json-object "id" "Job9")))
                 ("ERROR:" "an HTTP refusal the model reads")
                 ("402" "naming the status")
                 ("insufficient diamond balance" "and Fashio's own message"))))

;;; --- uploads --------------------------------------------------------------------

(deftest fashio-cell-upload-reads-the-image-by-signature (with-fashio-wire)
  (fashio-answer (fashio-ok "id" "u1" "url" "https://cdn.example.com/garment.png"))
  (with-png-file (png)
    (with-cell-stop ((fashio-start))
      (is-carrying (answer (fashio-tool "fashio_upload" (nlk:json-object "path" (namestring png))))
                   ("https://cdn.example.com/garment.png" "the url the upload answered")
                   ("image/png" "the type read from the bytes"))))
  (let ((request (first (fashio-asked))))
    (is (equal "https://api.fashiolabs.com/v1/uploads" (fashio-request-url request)) "the uploads endpoint")
    (is (equal "image/png" (fashio-request-header request "content-type")) "the signature's type rides the header")
    (is (typep (getf (cddr request) :content) '(vector (unsigned-byte 8))) "the file's own bytes are the body")))

(deftest fashio-cell-upload-refuses-what-is-not-an-image (with-fashio-wire)
  (with-cell-stop ((fashio-start))
    (let ((answer (fashio-tool "fashio_upload" (nlk:json-object "path" "/nonexistent/nothing.png"))))
      (is (search "there is no file at" answer) "a missing file is said, not signalled")))
  (is (null (fashio-asked)) "and nothing was sent"))

;;; --- presets and a curated photoshoot -----------------------------------------------

(deftest fashio-cell-presets-are-listed-saved-and-deleted (with-fashio-wire)
  (with-cell-stop ((fashio-start))
    (fashio-answer (fashio-ok "templateSets" (vector (nlk:json-object "id" "tset_9f2a" "label" "Studio white"))))
    (is-carrying (answer (fashio-tool "fashio_templates" (nlk:json-object "action" "list")))
                 ("tset_9f2a" "a saved preset's id")
                 ("Studio white" "and its label"))
    (fashio-answer (fashio-ok "id" "tset_new"))
    (is-carrying (answer (fashio-tool "fashio_templates"
                                       (nlk:json-object "action" "save" "label" "Studio white"
                                                        "source_media_id" "8b1e")))
                 ("tset_new" "a save answers the new id"))
    (let ((sent (fashio-request-body (fashio-last-request))))
      (is (equal "8b1e" (nlk:json-value sent :string "sourceMediaId")) "the source generation goes as sourceMediaId"))
    (fashio-answer (fashio-ok))
    (let ((answer (fashio-tool "fashio_templates" (nlk:json-object "action" "delete" "id" "tset_new"))))
      (is (search "was not confirmed" answer) "a delete waits for confirmation"))
    (is-carrying (answer (fashio-tool "fashio_templates" (nlk:json-object "action" "delete" "id" "tset_new"
                                                                          "confirm" t)))
                 ("Deleted" "a confirmed delete")
                 ("tset_new" "naming the preset"))
    (is (equal :delete (first (fashio-last-request))) "the delete method is sent")))

(deftest fashio-cell-a-catalog-job-sends-the-shots-and-says-the-cost (with-fashio-wire)
  (fashio-answer (fashio-ok "mediaItemId" "9c3f" "status" "pack_processing" "packTotal" 6 "diamondCost" 18))
  (with-cell-stop ((fashio-start))
    (is-carrying (answer (fashio-tool "fashio_catalog_job"
                                       (nlk:json-object "ready_template_set_id" "set_editorial_04"
                                                        "image" "https://example.com/dress.jpg"
                                                        "shot_ids" (vector "shot_1" "shot_2")
                                                        "confirm" t)))
                 ("9c3f" "the job id")
                 ("6 shots" "how many shots")
                 ("18 diamonds" "what was charged")))
  (let* ((request (first (fashio-asked)))
         (body (fashio-request-body request)))
    (is (equal "https://api.fashiolabs.com/v1/catalog-jobs" (fashio-request-url request)) "the catalog-jobs endpoint")
    (is (equal "set_editorial_04" (nlk:json-value body :string "readyTemplateSetId")) "the ready set")
    (let ((images (nlk:json-value body :object "images")))
      (is (equal "https://example.com/dress.jpg" (nlk:json-value images :string "garment_photo")) "the product photo is the garment"))
    (is (equal 2 (length (nlk:json-array body "shotIds"))) "only the shots asked for")))

;;; --- the section --------------------------------------------------------------

(deftest fashio-cell-a-key-variable-with-nothing-in-it-refuses (with-fashio-wire)
  (with-stubbed-fdefinition (nle::credential-env (name)
                             (declare (ignore name))
                             nil)
    (is-carrying (text (refusal-text nlk:config-refusal
                                     (fashio-start "api_key_env" "FASHIO_TEST_KEY")))
                 ("FASHIO_TEST_KEY" "the refusal names the variable")
                 ("fashiolabs.com" "and where a key comes from")))
  (is (null (nle::find-tool "fashio_account")) "a refused start installed nothing"))

(deftest fashio-cell-a-key-written-inline-and-a-base-url-move-every-call (with-fashio-wire)
  (fashio-answer (fashio-ok "diamondBalance" 5))
  (with-cell-stop ((fashio-start "api_key" "fio_live_inline" "base_url" "http://proxy.example.com:8080/"
                                  "list_limit" 2))
    (is (equal "fio_live_inline" (nodecode-fashio::setting :key)) "the section's own key")
    (fashio-tool "fashio_account" (nlk:json-object)))
  (let ((request (first (fashio-asked))))
    (is (equal "http://proxy.example.com:8080/v1/account" (fashio-request-url request)) "a base url replaces the host")
    (is (equal "Bearer fio_live_inline" (fashio-request-header request "authorization")) "and the inline key rides it")))

;;; --- /fashio ---------------------------------------------------------------------

(deftest fashio-cell-the-command-explains-itself-and-polls (with-fashio-wire)
  (fashio-answer (fashio-ok "id" "AbCdEf123" "status" "completed"
                             "resultUrls" (vector "https://cdn.example.com/out.png")))
  (with-cell-stop ((fashio-start))
    (is-carrying (status (cell-entry "nodecode-fashio" "fashio" ""))
                 ("https://api.fashiolabs.com" "no argument: where it points")
                 ("fashio_generate" "and the tools it offers"))
    (is-carrying (answer (cell-entry "nodecode-fashio" "fashio" "AbCdEf123"))
                 ("completed" "an argument: that job's status")
                 ("https://cdn.example.com/out.png" "with its result")))
  (let ((url (fashio-request-url (first (fashio-asked)))))
    (is (equal "https://api.fashiolabs.com/v1/generations/AbCdEf123" url) "the operator's words are the job id")))
