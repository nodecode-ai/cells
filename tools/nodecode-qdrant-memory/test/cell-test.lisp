;;;; cell-test.lisp --- the qdrant-memory cell against a stubbed store.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Qdrant endpoint and the embedding endpoint are one stubbed NLK:HTTP,
;;;; and the API-key environment a stubbed NLE::CREDENTIAL-ENV: nothing reaches
;;;; a store, the environment or the operator's files. Each tool is called the
;;;; way a model's call reaches it -- through the real dispatch, contract and
;;;; all -- so the schema the provider is shown and the body that runs are one
;;;; shape.

(in-package #:nodecode.test)

(define-test-slice "qdrant-memory" "QDRANT-MEMORY-CELL-")

(defun qdrant-memory-config (&rest pairs)
  "A whole shared config holding the `qdrant_memory' section PAIRS make, with
the store and the embedding endpoint defaulted to the stubs'."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "url" section "http://q.test:6333")
    (alexandria:ensure-gethash "embedding_url" section "http://embed.test/api/embed")
    (alexandria:ensure-gethash "timeout" section 20)
    (nlk:json-object "qdrant_memory" section)))

(defun qdrant-memory-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-qdrant-memory:start-cell (apply #'qdrant-memory-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-no-api-key (&body body)
  "BODY with no QDRANT_API_KEY in the environment."
  `(with-stubbed-fdefinition (nle::credential-env (name) nil)
     ,@body))

(defparameter +done+ "{\"result\":{\"operation_id\":1,\"status\":\"completed\"}}"
  "What a write answers.")

(defparameter +collection+ (concatenate 'string
  "{\"result\":{\"status\":\"green\",\"points_count\":2,\"config\":"
  "{\"params\":{\"vectors\":{\"size\":3,\"distance\":\"Cosine\"}}}}}")
  "What GET /collections/<name> answers for a collection of 3-dimension vectors.")

(defparameter +missing+ "{\"status\":{\"error\":\"Not found: Collection does not exist\"}}"
  "What GET /collections/<name> answers when there is no such collection.")

(defparameter +id+ "11111111-1111-4111-8111-111111111111"
  "The id the stubs' one stored memory has.")

(defparameter +point+ (concatenate 'string
  "{\"result\":{\"id\":\"" +id+ "\",\"payload\":"
  "{\"text\":\"Prefers green tea\",\"canonical\":\"prefers green tea\","
  "\"namespace\":\"default\",\"source\":\"manual\","
  "\"created_at\":\"2026-01-01T00:00:00Z\"}}}")
  "What GET /collections/<name>/points/<id> answers for the stored memory.")

(defparameter +hit+ (concatenate 'string
  "{\"id\":\"" +id+ "\",\"score\":0.87,\"payload\":"
  "{\"text\":\"Prefers green tea\",\"namespace\":\"default\","
  "\"created_at\":\"2026-01-01T00:00:00Z\",\"tags\":[\"preferences\"]}}")
  "One search hit.")

;;; --- the stubbed store -----------------------------------------------------------

(defvar *stored-collection* t "Whether the collection exists in the stub.")
(defvar *stored-point* nil "The stored point's payload as JSON text, or NIL.")
(defvar *hits* "[]" "The search answer's result array, as JSON text.")
(defparameter +scroll-point+ (concatenate 'string
  "{\"id\":\"" +id+ "\",\"payload\":{\"text\":\"Prefers green tea\","
  "\"canonical\":\"prefers green tea\",\"namespace\":\"default\"}}")
  "One point as a scroll answer carries it: the id and the payload, with no
result wrapper around them.")

(defvar *duplicate* nil "The point a scroll answers, or NIL.")
(defvar *requests* '() "Every (METHOD URL CONTENT), newest first.")

(defun stub-answer (method url options)
  "The stub's answer to METHOD URL with OPTIONS, as (values TEXT STATUS)."
  (push (list method url (getf options :content)) *requests*)
  (cond
    ((search "embed.test" url)
     (values "{\"embeddings\":[[0.1,0.2,0.3]]}" 200))
    ((search "/points/search" url)
     (values (format nil "{\"result\":~a}" *hits*) 200))
    ((search "/points/scroll" url)
     (values (format nil "{\"result\":{\"points\":[~a]}}"
                     (cond ((null *duplicate*) "")
                           ((stringp *duplicate*) *duplicate*)
                           (t +scroll-point+)))
             200))
    ((search "/points/payload" url) (values +done+ 200))
    ((search "/points/delete" url) (values +done+ 200))
    ((search "/points?wait=true" url) (values +done+ 200))
    ((search "/points/" url)
     (if *stored-point* (values +point+ 200) (values +missing+ 404)))
    ((search "/collections/" url)
     (cond ((eq method :put) (values +done+ 200))
           (*stored-collection* (values +collection+ 200))
           (t (values +missing+ 404))))
    (t (error "unexpected request to ~a" url))))

(defmacro with-qdrant (&body body)
  "BODY with NLK:HTTP answering the stub, and the request log empty."
  `(let ((*requests* '())
         (*stored-collection* t)
         (*stored-point* t)
         (*hits* (format nil "[~a]" +hit+)))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                 (stub-answer method url options))
       (symbol-macrolet ((asked (reverse *requests*)))
         ,@body))))

(defun asked-p (fragment)
  "Whether any request of the log names FRAGMENT in its url."
  (some (lambda (entry) (search fragment (second entry))) *requests*))

(defun asked-content (fragment)
  "The content of the first request naming FRAGMENT, decoded."
  (let ((entry (find-if (lambda (entry) (search fragment (second entry))) (reverse *requests*))))
    (and entry (third entry) (nlk:decode-json (third entry)))))

(defun qm-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun qm-object (id tool arguments)
  (nlk:decode-json (qm-answer id tool arguments)))

(defun first-point (content)
  "The first point of a write's own body, or NIL."
  (let ((points (nlk:json-array content "points")))
    (when (plusp (length points)) (aref points 0))))

(defun first-must (content)
  "The first filter clause of a search's own body, or NIL."
  (let ((must (nlk:json-array (nlk:json-value content :object "filter") "must")))
    (when (plusp (length must)) (aref must 0))))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "qdrant-memory"
  (:config "embedding_url" "http://embed.test/api/embed")
  (:help :qdrant-memory)
  (:command "qdrant-memory")
  (:fixture with-no-api-key)
  (:refused ("embedding_url" "") ("url" "not-a-url")))

(deftest qdrant-memory-cell-without-an-embedding-endpoint-refuses ()
  ;; The one setting no memory can be written without: a vector has to come
  ;; from somewhere, and the refusal names the member.
  (let ((condition (signals-error nlk:config-refusal
                                  (qdrant-memory-start "embedding_url" ""))))
    (is (search "embedding_url" (princ-to-string condition)) "the refusal names the member")
    (is (null (nle::find-tool "qdrant_memory_search")) "and installs nothing")))

;;; --- qdrant_memory_search ----------------------------------------------------------

(deftest qdrant-memory-cell-searches-by-meaning (with-no-api-key)
  (with-cell-stop ((qdrant-memory-start))
    (with-qdrant
      (let ((object (qm-object "qm-1" "qdrant_memory_search" "{\"query\":\"what do I drink\"}")))
        (is (= 1 (nlk:json-value object :integer "count")))
        (is (equal "Prefers green tea"
                   (nlk:json-value (aref (nlk:json-array object "memories") 0) :string "text")))
        (is (equal +id+ (nlk:json-value (aref (nlk:json-array object "memories") 0) :string "id")) "the id update and delete take comes back")
        (is (asked-p "/collections/nodecode_memory/points/search") "the store is searched")
        (is (not (asked-p "/collections/nodecode_memory?wait")) "a search writes nothing")
        (let ((content (asked-content "/points/search")))
          (is (equal "default" (nlk:json-value (first-must content) :string "match" "value")) "the search is scoped to the namespace"))))))

(deftest qdrant-memory-cell-says-when-it-holds-nothing (with-no-api-key)
  (with-cell-stop ((qdrant-memory-start))
    (with-qdrant
      (setf *hits* "[]")
      (let ((object (qm-object "qm-2" "qdrant_memory_search" "{\"query\":\"a thing never said\"}")))
        (is (= 0 (nlk:json-value object :integer "count")) "an empty answer is a zero count, not a failure")))))

(deftest qdrant-memory-cell-refuses-a-search-with-no-query (with-no-api-key)
  (with-cell-stop ((qdrant-memory-start))
    (with-qdrant
      (let ((answer (qm-answer "qm-3" "qdrant_memory_search" "{}")))
        (is (search "preflight" answer) "the contract refuses it before the body")
        (is (null asked))))))

;;; --- qdrant_memory_add ----------------------------------------------------------------

(deftest qdrant-memory-cell-adds-a-fact (with-no-api-key)
  (with-cell-stop ((qdrant-memory-start))
    (with-qdrant
      (setf *stored-collection* nil)
      (let ((object (qm-object "qm-4" "qdrant_memory_add"
                                "{\"text\":\"Prefers green tea\",\"tags\":[\"preferences\"]}")))
        (is (eq t (nlk:json-value object :boolean "stored")) "the fact is stored")
        (is (stringp (nlk:json-value object :string "id")) "with the id update and delete take")
        (is (eq :put (first (find-if (lambda (entry) (search "points?wait=true" (second entry)))
                                     *requests*))) "the memory is written with an upsert")
        (let ((content (asked-content "/points?wait=true")))
          (is (equal "Prefers green tea"
                     (nlk:json-value (first-point content) :string "payload" "text")) "the fact rides the payload as written")
          (is (equal "prefers green tea"
                     (nlk:json-value (first-point content) :string "payload" "canonical")) "and its canonical form is stored for the duplicate check"))
        (is (some (lambda (entry)
                     (and (eq :put (first entry))
                          (search "/collections/nodecode_memory" (second entry))
                          (not (search "/points" (second entry)))))
                   *requests*) "a missing collection is created first")))))

(deftest qdrant-memory-cell-does-not-store-the-same-fact-twice (with-no-api-key)
  (with-cell-stop ((qdrant-memory-start))
    (with-qdrant
      (setf *duplicate* t)
      (let ((object (qm-object "qm-5" "qdrant_memory_add" "{\"text\":\"Prefers green tea\"}")))
        (is (eq t (nlk:json-value object :boolean "duplicate")) "the stored copy is answered")
        (is (eq nil (nlk:json-value object :boolean "stored")) "and nothing is written")
        (is (equal +id+ (nlk:json-value object :string "id")))
        (is (not (asked-p "points?wait=true")) "no second copy")))))

;;; --- qdrant_memory_update --------------------------------------------------------------

(deftest qdrant-memory-cell-updates-a-fact-in-place (with-no-api-key)
  (with-cell-stop ((qdrant-memory-start))
    (with-qdrant
      (let ((object (qm-object "qm-6" "qdrant_memory_update"
                                (format nil "{\"id\":\"~a\",\"text\":\"Prefers black tea\"}" +id+))))
        (is (eq t (nlk:json-value object :boolean "updated")))
        (is (eq t (nlk:json-value object :boolean "changed")) "the text changed")
        (is (equal "Prefers black tea" (nlk:json-value object :string "text")))
        (let ((content (asked-content "points?wait=true")))
          (is (equal +id+ (nlk:json-value (first-point content) :string "id")) "the same point is rewritten, not a second one")
          (is (equal "prefers black tea"
                     (nlk:json-value (first-point content) :string "payload" "canonical"))))))))

(deftest qdrant-memory-cell-refuses-to-update-a-missing-id (with-no-api-key)
  (with-cell-stop ((qdrant-memory-start))
    (with-qdrant
      (setf *stored-point* nil)
      (let ((answer (qm-answer "qm-7" "qdrant_memory_update"
                                (format nil "{\"id\":\"~a\",\"text\":\"x\"}" +id+))))
        (is (search "no memory with id" answer) "a store that does not hold it says so")
        (is (not (asked-p "points?wait=true")) "and nothing is written")))))

;;; --- qdrant_memory_delete ----------------------------------------------------------------

(deftest qdrant-memory-cell-deletes-a-fact (with-no-api-key)
  (with-cell-stop ((qdrant-memory-start))
    (with-qdrant
      (let ((object (qm-object "qm-8" "qdrant_memory_delete" (format nil "{\"id\":\"~a\"}" +id+))))
        (is (eq t (nlk:json-value object :boolean "deleted")))
        (is (equal "Prefers green tea" (nlk:json-value object :string "text")) "the answer says what was forgotten")
        (let ((body (asked-content "/points/delete")))
          (is (equal +id+ (aref (nlk:json-array body "points") 0)) "the id asked for is the id deleted"))))))

(deftest qdrant-memory-cell-refuses-to-delete-twice (with-no-api-key)
  (with-cell-stop ((qdrant-memory-start))
    (with-qdrant
      (setf *stored-point* nil)
      (let ((answer (qm-answer "qm-9" "qdrant_memory_delete" (format nil "{\"id\":\"~a\"}" +id+))))
        (is (search "already be deleted" answer))
        (is (not (asked-p "/points/delete")))))))

;;; --- recall, and the capture ---------------------------------------------------------------

(deftest qdrant-memory-cell-recalls-for-the-newest-ask (with-no-api-key)
  (with-cell-stop ((qdrant-memory-start))
    (with-qdrant
      (cell-publish "nodecode-qdrant-memory"
                    (cell-frame-op "turn.input_committed"
                                   :payload (nlk:json-object "message" "what do I drink")))
      (let ((section (cell-section "s1" "qdrant-memory")))
        (is (stringp section) "the request carries a qdrant-memory block")
        (is (search "Prefers green tea" section) "with the memory the store holds")
        (is (search +id+ section) "and its id")
        (let ((content (asked-content "embed.test")))
          (is (equal "what do I drink" (nlk:json-value (aref (nlk:json-array content "input") 0) :string)) "the newest ask is what is embedded")))
      (let ((before (length *requests*)))
        (cell-section "s1" "qdrant-memory")
        (is (= before (length *requests*)) "a second request in the same turn searches once")))))

(deftest qdrant-memory-cell-captures-a-finished-turn (with-no-api-key)
  (let ((nodecode-qdrant-memory::*use-worker* nil))
    (with-cell-stop ((qdrant-memory-start))
      (with-qdrant
        (cell-publish "nodecode-qdrant-memory"
                      (cell-frame-op "turn.input_committed"
                                     :payload (nlk:json-object "message" "I live in Oslo")))
        (cell-publish "nodecode-qdrant-memory" (cell-frame-op "turn.completed"))
        (nodecode-qdrant-memory::capture-step)
        (let ((content (asked-content "points?wait=true")))
          (is (equal "I live in Oslo" (nlk:json-value (first-point content) :string "payload" "text")) "the turn's ask is filed")
          (is (equal "conversation"
                     (nlk:json-value (first-point content) :string "payload" "source")) "stamped as captured, not as an explicit add"))))))

;;; --- the command -------------------------------------------------------------------------

(deftest qdrant-memory-cell-command-shows-and-searches (with-no-api-key)
  (with-cell-stop ((qdrant-memory-start))
    (with-qdrant
      (let ((shown (cell-entry "nodecode-qdrant-memory" "qdrant-memory" "")))
        (is (search "nodecode_memory" shown) "no argument shows the store")
        (is (search "2 memories" shown) "and how many memories it holds"))
      (let ((found (cell-entry "nodecode-qdrant-memory" "qdrant-memory" "tea")))
        (is (search "Prefers green tea" found) "an argument searches the store")))))
