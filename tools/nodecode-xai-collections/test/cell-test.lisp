;;;; cell-test.lisp --- lifecycle, the tool, and /collections, all offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, an integer under its floor refused, a start that could reach no
;;;; team refused), the one tool through the seam the model calls it by --
;;;; its URL, its headers, its body and its answer -- and /collections with
;;;; and without a query. Every request is a stubbed NLK:HTTP, so nothing
;;;; reaches xAI.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-xai-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials xAI."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected xAI request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defparameter +collection-a+ "collection_aaaaaaaa-0000-4000-8000-000000000001"
  "One well-shaped collection id, for the fixtures.")

(defparameter +collection-b+ "collection_bbbbbbbb-0000-4000-8000-000000000002"
  "A second well-shaped collection id.")

(defun xai-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun xai-request-body (request)
  "REQUEST's JSON body, decoded, or NIL when it carried none."
  (let ((content (getf (cddr request) :content)))
    (and (stringp content) (nlk:decode-json content))))

(defun xai-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun xai-answer (text &optional (status 200))
  "Answer every stubbed request with TEXT and STATUS."
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun xai-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defparameter +leases+ "{\"matches\":[{\"file_id\":\"f1\",\"chunk_content\":\"The contract renews in March.\",\"score\":0.49,\"page_number\":3,\"fields\":{\"title\":\"leases.pdf\"}},{\"file_id\":\"f2\",\"chunk_content\":\"Option to renew for two years.\",\"score\":0.41}]}"
  "One search answer, two passages: one titled, one without a title.")

(defparameter +one-collection+ "{\"collections\":[{\"collection_id\":\"collection_aaaaaaaa-0000-4000-8000-000000000001\",\"collection_name\":\"Leases\",\"documents_count\":26}]}"
  "One listing answer.")

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "xai-collections" "XAI-COLLECTIONS-CELL-"
  :start nodecode-xai-collections:start-cell)

(define-cell-lifecycle-tests "xai-collections"
  (:fixture with-xai-wire)
  (:config "api_key" "k-test")
  (:help :xai-collections)
  (:command "collections")
  (:running (is (nle::find-tool "xai_collections_search") "the tool is registered")
            (is (equal "k-test" (nodecode-xai-collections::api-key)) "the section's key answers first")
            (is (= 5 (nodecode-xai-collections::setting :default-limit)) "the default cap")
            (is (= 30 (nodecode-xai-collections::setting :timeout-seconds))))
  (:stopped (is (null (nle::find-tool "xai_collections_search")) "stop unregisters the tool"))
  (:refused ("default_limit" 0) ("timeout_seconds" 0))
  (:idle nodecode-xai-collections:xai-collections-error
         (nodecode-xai-collections::run-command "" nil)))

(deftest xai-collections-cell-a-team-needs-a-key ()
  (with-xai-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (text (refusal-text nlk:config-refusal (xai-collections-start)))
                   ("an xAI API key is required")
                   ("XAI_API_KEY" "the refusal names the variable the operator sets"))
      (is (null (nle::find-tool "xai_collections_search")) "a refused start installed nothing"))))

;;; --- the tool ------------------------------------------------------------------

(deftest xai-collections-cell-a-search-answers-the-passages ()
  (with-xai-wire
    (with-cell-stop ((xai-collections-start "api_key" "k-test"
                                            "default_collection_ids" (vector +collection-a+)))
      (xai-answer +leases+)
      (let ((answer (nlk:decode-json
                     (xai-tool "xai_collections_search"
                               (nlk:json-object "query" "when does the lease renew")))))
        (is (equal "when does the lease renew" (nlk:json-value answer :string "query")))
        (is (equal +collection-a+ (aref (nlk:json-array answer "collection_ids") 0)) "the section's default collection, the call naming none")
        (let ((matches (nlk:json-array answer "matches")))
          (is (= 2 (length matches)) "both passages")
          (is (equal "leases.pdf" (nlk:json-value (aref matches 0) :string "title")))
          (is (equal "The contract renews in March." (nlk:json-value (aref matches 0) :string "text")))
          (is (= 3 (nlk:json-value (aref matches 0) :integer "page")))
          (is (equal "f2" (nlk:json-value (aref matches 1) :string "title")) "a passage without a title reads as its file id")))
      (let* ((request (first (xai-asked)))
             (body (xai-request-body request)))
        (is (= 1 (length (xai-asked))) "one request")
        (is (eq :post (first request)))
        (is (equal "https://api.x.ai/v1/documents/search" (second request)))
        (is (equal "Bearer k-test" (xai-request-header request "authorization")) "the key rides as a bearer token")
        (is (equal "when does the lease renew" (nlk:json-value body :string "query")))
        (is (equal +collection-a+
                   (aref (nlk:json-value body :array "source" "collection_ids") 0)))
        (is (= 5 (nlk:json-value body :integer "limit")) "the section's default cap")
        (is (null (nlk:json-value body :any "filter")) "no filter asked, none sent")))))

(deftest xai-collections-cell-a-call-can-name-its-collections ()
  (with-xai-wire
    (with-cell-stop ((xai-collections-start "api_key" "k-test"))
      (xai-answer +leases+)
      (xai-tool "xai_collections_search"
                (nlk:json-object "query" "renewal" "collection_ids" (vector +collection-a+ +collection-b+)
                                 "limit" 2 "filter" "year > 2024"))
      (let ((body (xai-request-body (first (xai-asked)))))
        (is (= 2 (length (nlk:json-value body :array "source" "collection_ids"))))
        (is (= 2 (nlk:json-value body :integer "limit")) "the call's own cap")
        (is (equal "year > 2024" (nlk:json-value body :string "filter")))))))

(deftest xai-collections-cell-lists-the-collections ()
  (with-xai-wire
    (with-cell-stop ((xai-collections-start "api_key" "k-test"))
      (xai-answer +one-collection+)
      (let* ((answer (nlk:decode-json
                       (xai-tool "xai_collections_search" (nlk:json-object "list_collections" t))))
             (collections (nlk:json-array answer "collections")))
        (is (= 1 (length collections)))
        (is (equal "Leases" (nlk:json-value (aref collections 0) :string "name")))
        (is (= 26 (nlk:json-value (aref collections 0) :integer "documents"))))
      (let ((request (first (xai-asked))))
        (is (eq :get (first request)))
        (is (equal "https://api.x.ai/v1/collections" (second request)))))))

(deftest xai-collections-cell-a-call-without-ids-reads-every-collection ()
  (with-xai-wire
    (with-cell-stop ((xai-collections-start "api_key" "k-test"))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore url args))
              (if (eq :get method)
                  (values (format nil "{\"collections\":[{\"collection_id\":\"~a\"},{\"collection_id\":\"~a\"}]}"
                                  +collection-a+ +collection-b+)
                          200)
                  (values "{\"matches\":[]}" 200))))
      (let ((answer (nlk:decode-json
                     (xai-tool "xai_collections_search" (nlk:json-object "query" "renewal")))))
        (is (= 2 (length (nlk:json-array answer "collection_ids"))) "the listing is read when nothing else names a collection")
        (is (= 0 (length (nlk:json-array answer "matches"))) "no passage, an empty array"))
      (let ((requests (xai-asked)))
        (is (= 2 (length requests)) "a listing, then the search")
        (is (eq :get (first (first requests))))
        (is (= 2 (length (nlk:json-value (xai-request-body (second requests))
                                          :array "source" "collection_ids"))))))))

(deftest xai-collections-cell-a-wrong-answer-is-a-repair-not-a-crash ()
  (with-xai-wire
    (with-cell-stop ((xai-collections-start "api_key" "k-test"))
      (xai-answer "{\"error\":\"Invalid API key\"}" 401)
      (is-carrying (text (xai-tool "xai_collections_search" (nlk:json-object "query" "renewal")))
                   ("ERROR: xai collections:" "the refusal is the model's repair")
                   ("HTTP 401")
                   ("Invalid API key" "the vendor's word for it"))
      (setf *requests* '())
      (is-carrying (text (xai-tool "xai_collections_search"
                                    (nlk:json-object "query" "renewal"
                                                     "collection_ids" (vector "nope"))))
                   ("ERROR: xai collections:" "not an xAI collection id")
                   ("collection_<uuid>" "the shape is named"))
      (is (null (xai-asked)) "a badly shaped id never reaches the wire")
      (xai-answer "{\"matches\":[]}" 200)
      (let ((answer (nlk:decode-json
                     (xai-tool "xai_collections_search"
                               (nlk:json-object "query" "renewal"
                                                "collection_ids" (vector +collection-a+))))))
        (is (equal "renewal" (nlk:json-value answer :string "query")) "an empty answer is still an answer")
        (is (= 0 (length (nlk:json-array answer "matches"))))))))

;;; --- /collections ---------------------------------------------------------------

(deftest xai-collections-cell-the-command-lists-and-searches ()
  (with-xai-wire
    (with-cell-stop ((xai-collections-start "api_key" "k-test"
                                            "default_collection_ids" (vector +collection-a+)))
      (xai-answer +one-collection+)
      (let ((text (cell-entry "nodecode-xai-collections" "collections" "")))
        (is (search "1 collection on xAI:" text))
        (is (search (format nil "[~a] Leases (26 documents)" +collection-a+) text))
        (is (null (xai-request-body (first (xai-asked)))) "a listing sends no body"))
      (setf *requests* '())
      (xai-answer +leases+)
      (let ((text (cell-entry "nodecode-xai-collections" "collections" "renewal")))
        (is (search "2 passages for \"renewal\":" text))
        (is (search "leases.pdf (score 0.49) p.3" text))
        (is (search "Option to renew for two years." text)))
      (is (equal "renewal" (nlk:json-value (xai-request-body (first (xai-asked))) :string "query")) "the argument is the query"))))
