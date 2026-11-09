;;;; cell-test.lisp --- lifecycle, the six tools, /memoria, the round's recall.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Nothing here dials: the cell's one network seam, *REQUEST*, answers
;;;; canned objects and records what it was asked, so a test proves the
;;;; binding the request carried as well as the answer the model read.

(in-package #:nodecode.test)

(define-test-slice "memoria" "MEMORIA-CELL-"
  :start nodecode-memoria:start-cell
  :defaults (("api_key" "sk-memoria-test")))

;;; --- the stubbed service -------------------------------------------------------

(defvar *sp-answers* '()
  "((METHOD PATH-SUFFIX PARSED STATUS) ...): what the stubbed service answers.")

(defvar *sp-requests* '()
  "(METHOD URL BODY) newest first: every exchange the cell asked for.")

(defun sp-answer (method path parsed &optional (status 200))
  "Make the stubbed service answer PARSED with STATUS for METHOD on PATH."
  (push (list method path parsed status) *sp-answers*))

(defun sp-request (method url body)
  "The stubbed seam: (METHOD URL BODY) => (VALUES PARSED STATUS)."
  (push (list method url body) *sp-requests*)
  (let* ((rows (remove-if-not (lambda (row) (and (eq (first row) method)
                                                 (search (second row) url)))
                               *sp-answers*))
         ;; the longest matching path wins: /v1/memories is not /v1/memories/retrieve
         (row (first (sort rows #'> :key (lambda (row) (length (second row)))))))
    (if row (values (third row) (fourth row)) (values nil 599))))

(defun sp-memory (id &key (content "a memory") (type "semantic")
                       (subject (nodecode-memoria::subject)))
  "One memory record the service would answer."
  (nlk:make-json-object "memory_id" id "memory_type" type "content" content
                         "subject_id" subject))

(defun sp-page (&rest rows)
  "Those memory records as the array a retrieve answer is."
  (coerce rows 'vector))

(defun sp-body (&optional (n 0))
  "The JSON object the Nth-newest exchange sent, or NIL."
  (let ((body (third (nth n *sp-requests*))))
    (and body (nlk:decode-json body))))

(defun sp-answer-text (id name arguments)
  "What the model read back from tool NAME called with ARGUMENTS."
  (princ-to-string
   (nle::message-content (execute-wire-call id name arguments))))

(defmacro with-memoria-service (&body body)
  "BODY with the cell's network seam stubbed and answerless."
  `(with-saved-globals (nodecode-memoria::*request* *sp-answers* *sp-requests*)
     (setf *sp-answers* '()
           *sp-requests* '()
           nodecode-memoria::*request* #'sp-request)
     ,@body))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "memoria"
  (:config "api_key" "sk-memoria-test")
  (:hooks :round)
  (:help :memoria)
  (:command "memoria")
  (:running (is (nle::find-tool "memoria_search") "the tools are registered")
            (is (nle::find-tool "memoria_store"))
            (is (nle::find-tool "memoria_update"))
            (is (nle::find-tool "memoria_forget"))
            (is (nle::find-tool "memoria_profile"))
            (is (nle::find-tool "memoria_feedback"))
            (is (equal "https://api.thememoria.ai" (nodecode-memoria::setting :api-url)) "the free Cloud origin is the default")
            (is (= 5 (nodecode-memoria::setting :top-k)))
            (is (uiop:string-prefix-p "nodecode:" (nodecode-memoria::subject)) "the subject is this home's hash, and no path"))
  (:stopped (is (null (nle::find-tool "memoria_search")) "stop unregisters the tools")
            (is (null nodecode-memoria::*subject*) "and forgets the subject"))
  (:refused ("api_url" "ftp://example.test")
            ("api_url" "https://example.test/v1")
            ("api_url" "http://example.test")))

;;; --- the section, and the command ----------------------------------------------

(deftest memoria-cell-the-command-shows-the-state-and-asks-the-service ()
  (with-memoria-service
    (with-cell-stop ((memoria-start))
      (let ((state (nodecode-memoria::run-command "" "s1")))
        (is (search "api.thememoria.ai" state) "the state names the service")
        (is (search "branch main" state))
        (is (search "API key set" state) "and whether a key is there")
        (is (null *sp-requests*) "showing the state dials nothing"))
      (sp-answer :post "/v1/memories/retrieve" (sp-page (sp-memory "mem-1" :content "drinks tea")))
      (is-carrying (text (nodecode-memoria::run-command "search tea" "s1"))
        ("mem-1" "the command lists the memory id")
        ("drinks tea" "and what it says"))
      (is (equal "tea" (nlk:json-value (sp-body) :string "query")) "the words typed are the query")
      (is (equal "main" (nlk:json-value (sp-body) :string "branch")) "scoped to the branch")
      (is (equal "s1" (nlk:json-value (sp-body) :string "session_id")) "and to the session that asked"))))

(deftest memoria-cell-a-url-that-is-no-origin-is-refused-in-words ()
  (let ((detail (handler-case (progn (memoria-start "api_url" "not a url") "")
                  (nlk:config-refusal (condition) (nlk:config-refusal-detail condition)))))
    (is (search "memoria.api_url" detail) "the refusal names the member to fix")))

;;; --- the tools ------------------------------------------------------------------

(deftest memoria-cell-a-search-answers-every-hit-under-the-scope ()
  (with-session-store ("s1")
    (with-memoria-service
      (with-cell-stop ((memoria-start))
        (sp-answer :post "/v1/memories/retrieve"
                   (sp-page (sp-memory "mem-1" :content "likes tea" :type "profile")))
        (let ((answer (sp-answer-text "c1" "memoria_search"
                                      "{\"query\":\"tea\",\"top_k\":3}")))
          (is (search "mem-1" answer) "the id the model needs is there")
          (is (search "likes tea" answer))
          (is (search "profile" answer))
          (is (not (search "success\":false" answer))))
        (let ((body (sp-body)))
          (is (= 3 (nlk:json-value body :integer "top_k")) "the call's own top_k")
          (is (equal (nodecode-memoria::subject) (nlk:json-value body :string "subject_id")) "the subject is the profile's, never a model's argument")
          (is (null (nlk:json-value body :string "extra_metadata")))
          (is (search "\"query\":\"tea\"" (third (first *sp-requests*))) "the query itself is sent"))))))

(deftest memoria-cell-a-memory-of-another-subject-is-refused ()
  (with-session-store ("s1")
    (with-memoria-service
      (with-cell-stop ((memoria-start))
        (sp-answer :post "/v1/memories/retrieve"
                   (sp-page (sp-memory "mem-9" :subject "nodecode:elsewhere")))
        (is (search "another subject" (sp-answer-text "c2" "memoria_search" "{\"query\":\"tea\"}")) "a backend answering another subject is refused, not believed")))))

(deftest memoria-cell-a-store-answers-a-receipt-and-never-the-content ()
  (with-session-store ("s1")
    (with-memoria-service
      (with-cell-stop ((memoria-start))
        (sp-answer :post "/v1/memories"
                   (nlk:make-json-object "memory_id" "mem-new"
                                          "subject_id" (nodecode-memoria::subject)
                                          "memory_type" "profile"
                                          "content" "the whole record, echoed"))
        (let ((answer (sp-answer-text "c3" "memoria_store"
                                      "{\"content\":\"I take my coffee black\",\"memory_type\":\"profile\"}")))
          (is (search "mem-new" answer) "the receipt carries the new id")
          (is (not (search "the whole record, echoed" answer)) "and not the content back")
          (is (search "profile" answer)))
        (is (equal "I take my coffee black" (nlk:json-value (sp-body) :string "content")) "the fact is what was sent")
        (is (equal "explicit" (nlk:json-value (nlk:json-value (sp-body) :object "extra_metadata") :string "write_origin")) "the write says where it came from")))))

(deftest memoria-cell-a-forget-reads-the-record-before-it-deletes-it ()
  (with-session-store ("s1")
    (with-memoria-service
      (with-cell-stop ((memoria-start))
        (sp-answer :get "/v1/memories/mem-1" (sp-memory "mem-1"))
        (sp-answer :delete "/v1/memories/mem-1" nil 204)
        (let ((answer (sp-answer-text "c4" "memoria_forget" "{\"memory_id\":\"mem-1\"}")))
          (is (search "mem-1" answer)))
        (is (equal '(:delete :get) (mapcar #'first (subseq *sp-requests* 0 2))) "the record is read first, so a wrong id is a refusal and not a deletion"))
      ;; a record of another subject is never deleted
      (setf *sp-requests* '())
      (with-cell-stop ((memoria-start))
        (sp-answer :get "/v1/memories/mem-2" (sp-memory "mem-2" :subject "nodecode:elsewhere"))
        (is (search "another subject"
                    (sp-answer-text "c5" "memoria_forget" "{\"memory_id\":\"mem-2\"}")))
        (is (notany (lambda (row) (eq (first row) :delete)) *sp-requests*) "and nothing was deleted")))))

(deftest memoria-cell-without-a-key-every-verb-refuses-naming-the-setting ()
  (with-session-store ("s1")
    (with-memoria-service
      (with-cell-stop ((nodecode-memoria:start-cell (memoria-config "api_key" "")))
        (is (search "memoria.api_key" (sp-answer-text "c6" "memoria_search" "{\"query\":\"tea\"}")) "the refusal names the member to fill in")
        (is (null *sp-requests*) "and nothing was dialled")))))

;;; --- recall, in the turn ---------------------------------------------------------

(defun sp-recall (op)
  "Run the cell's :ROUND hook on OP, as the turns run it."
  (funcall (cell-hook "nodecode-memoria" :round)
           op (lambda (operation) (declare (ignore operation)) nil)))

(deftest memoria-cell-recall-rides-the-turns-first-round-once ()
  (with-memoria-service
    (with-cell-stop ((memoria-start))
      (let ((seen '()))
        (sp-answer :post "/v1/memories/retrieve"
                   (sp-page (sp-memory "mem-1" :content "drinks tea")))
        (with-stubbed-fdefinition (nodecode-memoria::turn-words (id)
                                    "what do I drink")
          (with-stubbed-fdefinition (nlk:record-turn-context-prompt (turn &rest args)
                                      (push (list turn (getf args :text)) seen))
            (sp-recall (list :session-id "s1" :turn "t1" :round 1))
            (is (= 1 (length seen)) "the first round carries one block")
            (is (search "<memoria_context>" (second (first seen))))
            (is (search "mem-1" (second (first seen))))
            (is (search "drinks tea" (second (first seen))))
            (sp-recall (list :session-id "s1" :turn "t1" :round 2))
            (is (= 1 (length seen)) "a later round of the same turn asks once, not again")
            (sp-recall (list :session-id "s1" :turn "t2" :round 1))
            (is (= 2 (length seen)) "the next turn asks again")))))))

(deftest memoria-cell-recall-is-off-when-the-section-says-so ()
  (with-memoria-service
    (with-cell-stop ((memoria-start "auto_recall" nil))
      (let ((seen '()))
        (sp-answer :post "/v1/memories/retrieve"
                   (sp-page (sp-memory "mem-1" :content "drinks tea")))
        (with-stubbed-fdefinition (nodecode-memoria::turn-words (id)
                                    "what do I drink")
          (with-stubbed-fdefinition (nlk:record-turn-context-prompt (turn &rest args)
                                      (push (list turn (getf args :text)) seen))
            (sp-recall (list :session-id "s1" :turn "t1" :round 1))
            (is (null seen) "nothing rides the turn")
            (is (null *sp-requests*) "and nothing is asked")))))))
