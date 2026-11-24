;;;; cell-test.lisp --- lifecycle, the four tools, /cognee.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a value under its floor refused, a remote server with no key
;;;; refused), the four tools through the seam the model calls them by --
;;;; their URL, their body and their answer -- and /cognee with and without
;;;; a query. Every request is a stubbed NLK:HTTP, so nothing reaches
;;;; Cognee or the network.

(in-package #:nodecode.test)

;;; --- fixtures ----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-cognee-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Cognee."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected cognee request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun cognee-asked () (reverse *requests*))
(defun cognee-request-body (request) (getf (cddr request) :content))
(defun cognee-request-json (request) (nlk:decode-json (cognee-request-body request)))
(defun cognee-request-header (request name)
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))
(defun cognee-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))
(defun cognee-tool (name arguments)
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ---------------------------------------------------------------

(define-test-slice "cognee" "COGNEE-CELL-" :start nodecode-cognee:start-cell)

(define-cell-lifecycle-tests "cognee"
  (:fixture with-cognee-wire)
  (:config "base_url" "https://cognee.test" "api_key" "k-test")
  (:help :cognee)
  (:command "cognee")
  (:running (is (nle::find-tool "cognee_recall") "cognee_recall is registered")
            (is (nle::find-tool "cognee_remember"))
            (is (nle::find-tool "cognee_forget"))
            (is (nle::find-tool "cognee_switch_dataset"))
            (is (equal "https://cognee.test" (nodecode-cognee::root)))
            (is (equal "nodecode" nodecode-cognee::*dataset*) "the default dataset")
            (is (= 10 (nodecode-cognee::setting :top-k))))
  (:stopped (is (null (nle::find-tool "cognee_recall")) "stop unregisters the tools")
            (is (null nodecode-cognee::*dataset*) "and forgets the conversation's dataset"))
  (:refused ("top_k" 0) ("recall_timeout_seconds" 0) ("write_timeout_seconds" 0))
  (:idle nodecode-cognee:cognee-error (nodecode-cognee::run-command "" nil)))

(deftest cognee-cell-a-remote-server-needs-a-key ()
  (with-cognee-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal
                                         (cognee-start "base_url" "https://cognee.test")))
                   ("a remote server needs a key" "COGNEE_API_KEY"))
      (is (null (nle::find-tool "cognee_recall")) "a refused start installed nothing"))
    ;; a server on this machine asks for none
    (with-cell-stop ((cognee-start "base_url" "http://127.0.0.1:8011"))
      (is (nle::find-tool "cognee_recall") "a local server starts without a key"))))

;;; --- recall -------------------------------------------------------------------

(deftest cognee-cell-recall-answers-the-nearest-passages ()
  (with-cognee-wire
    (with-cell-stop ((cognee-start "base_url" "https://cognee.test" "api_key" "k-test"
                                    "dataset" "work" "search_type" "chunks"))
      (cognee-answer "[{\"text\":\"the release branch is cut on Fridays\"},{\"answer\":\"tea, never coffee\"},{\"text\":\"  \"}]")
      (let ((answer (cognee-tool "cognee_recall" (nlk:json-object "query" "when is the release" "top_k" 5))))
        (is (search "2 Cognee memories for \"when is the release\"" answer))
        (is (search "the release branch is cut on Fridays" answer))
        (is (search "tea, never coffee" answer) "the answer slot is a memory too"))
      (let* ((request (first (cognee-asked)))
             (body (cognee-request-json request)))
        (is (= 1 (length (cognee-asked))) "one request")
        (is (eq :post (first request)))
        (is (equal "https://cognee.test/api/v1/recall" (second request)))
        (is (equal "k-test" (cognee-request-header request "x-api-key")))
        (is (equal "when is the release" (nlk:json-value body :string "query")))
        (is (= 5 (nlk:json-value body :integer "top_k")) "the call's top_k")
        (is (equal "work" (aref (nlk:json-value body :array "datasets") 0)) "the section's dataset")
        (is (equal "graph" (aref (nlk:json-value body :array "scope") 0)) "graph scope, as the original")
        (is (equal "CHUNKS" (nlk:json-value body :string "search_type")) "the section's search_type")))))

(deftest cognee-cell-a-failed-answer-is-a-repair-not-a-crash ()
  (with-cognee-wire
    (with-cell-stop ((cognee-start "base_url" "https://cognee.test" "api_key" "k-test"))
      (cognee-answer "{\"detail\":\"bad api key\"}" 401)
      (is-carrying (answer (cognee-tool "cognee_recall" (nlk:json-object "query" "x")))
                   ("ERROR: cognee:") ("HTTP 401") ("bad api key"))
      (cognee-answer "[]")
      (is (equal "No relevant Cognee memory for \"x\"."
                 (cognee-tool "cognee_recall" (nlk:json-object "query" "x")))))))

;;; --- remember ------------------------------------------------------------------

(deftest cognee-cell-remember-stores-one-document ()
  (with-cognee-wire
    (with-cell-stop ((cognee-start "base_url" "https://cognee.test" "api_key" "k-test"
                                  "dataset" "work"))
      (cognee-answer "{\"status\":\"running\"}")
      (is (search "Content stored in Cognee (work)"
                  (cognee-tool "cognee_remember"
                               (nlk:json-object "content" "the release branch is cut on Fridays"))))
      (let* ((request (first (cognee-asked)))
             (body (cognee-request-body request)))
        (is (eq :post (first request)))
        (is (equal "https://cognee.test/api/v1/remember" (second request)))
        (is (search "multipart/form-data; boundary=" (cognee-request-header request "content-type")))
        (is (search "name=\"datasetName\"" body) "the dataset travels as a form field")
        (is (search "work" body))
        (is (search "name=\"run_in_background\"" body) "the pipeline runs server-side")
        (is (search "name=\"data\"; filename=\"memory-" body) "the document is the data part")
        (is (search "the release branch is cut on Fridays" body))))))

;;; --- forget ----------------------------------------------------------------------

(deftest cognee-cell-forget-finds-then-deletes-what-was-confirmed ()
  (with-cognee-wire
    (with-cell-stop ((cognee-start "base_url" "https://cognee.test" "api_key" "k-test"
                                    "dataset" "work"))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore method args))
              (cond ((search "/datasets/d-1/data/doc-1/raw" url)
                     (values "\"we talked about tennis on Tuesday\"" 200))
                    ((search "/datasets/d-1/data" url)
                     (values "[{\"id\":\"doc-1\",\"name\":\"memory-1.txt\"},{\"id\":\"doc-2\",\"name\":\"memory-2.txt\"}]" 200))
                    ((search "/datasets" url)
                     (values "[{\"id\":\"d-1\",\"name\":\"work\"}]" 200))
                    ((equal url "https://cognee.test/api/v1/datasets")
                     (values "[{\"id\":\"d-1\",\"name\":\"work\"}]" 200))
                    (t (values "{\"deleted\":1}" 200)))))
      (let ((found (cognee-tool "cognee_forget"
                                 (nlk:json-object "action" "find" "terms" "tennis"))))
        (is (search "data_id doc-1" found) "the candidate is named")
        (is (search "we talked about tennis" found) "with a preview of its text")
        (is (not (search "doc-2" found)) "the document without the term is not a candidate"))
      (setf *requests* '())
      (is-carrying (answer (cognee-tool "cognee_forget"
                                         (nlk:json-object "action" "forget" "data_ids" (vector "doc-1"))))
                   ("confirm=true"))
      (is (null (cognee-asked)) "an unconfirmed delete writes nothing")
      (is (search "Deleted 1 document from \"work\""
                  (cognee-tool "cognee_forget"
                               (nlk:json-object "action" "forget" "confirm" t
                                                "data_ids" (vector "doc-1")))))
      (let ((body (cognee-request-json (car (last (cognee-asked))))))
        (is (equal "d-1" (nlk:json-value body :string "datasetId")))
        (is (equal "doc-1" (nlk:json-value body :string "dataId")))))))

;;; --- datasets and /cognee ---------------------------------------------------------

(deftest cognee-cell-datasets-switch-and-the-command-answers ()
  (with-cognee-wire
    (with-cell-stop ((cognee-start "base_url" "https://cognee.test" "api_key" "k-test"
                                    "dataset" "work"))
      (cognee-answer "[{\"id\":\"d-1\",\"name\":\"work\"},{\"id\":\"d-2\",\"name\":\"notes\"}]")
      (is (search "work" (cognee-tool "cognee_switch_dataset" (nlk:json-object "action" "list"))))
      (is (search "*current*" (cognee-tool "cognee_switch_dataset" (nlk:json-object "action" "list"))))
      (is (search "now uses the \"notes\""
                  (cognee-tool "cognee_switch_dataset"
                               (nlk:json-object "action" "switch" "dataset" "notes"))))
      (is (equal "notes" nodecode-cognee::*dataset*) "the switch moved the conversation")
      (is (search "Back to the configured dataset, \"work\""
                  (cognee-tool "cognee_switch_dataset" (nlk:json-object "action" "reset"))))
      (let ((status (cell-entry "nodecode-cognee" "cognee" "")))
        (is (search "Cognee memory: https://cognee.test, dataset \"work\"" status))
        (is (search "key set" status)))
      (setf *requests* '())
      (cognee-answer "[{\"text\":\"the release branch is cut on Fridays\"}]")
      (is (search "the release branch" (cell-entry "nodecode-cognee" "cognee" "release")))
      (is (equal "release"
                 (nlk:json-value (cognee-request-json (first (cognee-asked))) :string "query"))))))
