;;;; tools.lisp --- the four verbs: what a call fetches, and what the model reads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Each verb answers one string, which is both what a tool result carries and
;;;; what an eval snippet prints, so the model and the operator read the same
;;;; words. Search is the workhorse: it returns passages verbatim, which keeps
;;;; the agent's own reasoning auditable, and it answers the two questions a
;;;; model cannot tell apart - where is this discussed, and where is the
;;;; document called X - in one call.
;;;;
;;;; ask delegates the whole question to WeKnora, which runs another model
;;;; server-side: it is slow and answers with a conclusion, so it is for broad
;;;; synthesis questions only. read_document exists because retrieval returns
;;;; fragments: a hit's knowledge_id pages through its neighbours.

(in-package #:nodecode-weknora)

;;; --- the words --------------------------------------------------------------

(defun tool-name (suffix)
  "The name this deployment registers its SUFFIX tool under."
  (format nil "~a_~a" (setting :tool-prefix) suffix))

(defun scope-note ()
  "One sentence on what a search spans, for the tool description."
  (let ((ids (coerce (setting :knowledge-base-ids) 'list)))
    (if ids
        (format nil " Searches knowledge base(s) ~{~a~^, ~} unless you name others." ids)
        " Searches every knowledge base this credential can see unless you narrow it with knowledge_base_ids.")))

(defun search-description ()
  (format nil "Search WeKnora knowledge bases and return the matching passages verbatim ~
(hybrid vector + keyword retrieval, no model summarization). Use this to ground an answer in ~
the organization's own documents, and also to locate a document by name - a query that reads ~
like a title additionally returns the documents it names.~a Every hit carries a knowledge_id ~
you can pass to ~a for the full document."
          (scope-note) (tool-name "read_document")))

(defun list-description ()
  (format nil "List the WeKnora knowledge bases this deployment can retrieve from, with their ids. ~
~a already spans them all, so reach for this only to report what is available or to narrow a ~
later search to one of them."
          (tool-name "search")))

(defun read-description ()
  (format nil "Read a WeKnora document's stored passages in order, reassembled into text. ~
Use it after ~a when one passage is not enough context; page through long documents."
          (tool-name "search")))

(defun ask-description ()
  (format nil "Ask WeKnora a question and get its own composed answer with citations (~a pipeline ~
runs server-side: query rewriting, retrieval, reranking and summarization). Reserve it for ~
broad or synthesis questions whose answer spans many documents, where retrieving passages ~
yourself would take several rounds. For anything you can answer from specific passages, prefer ~
~a: it is far faster and leaves you the evidence to reason over rather than another model's ~
conclusion."
          (if (setting :agent-id) "agent" "RAG") (tool-name "search")))

;;; --- little projections ------------------------------------------------------

(defun clip-text (text max-chars)
  "TEXT within MAX-CHARS, carriage returns read as line breaks.
=> (values TEXT TRUNCATED-P), so a clip is reported instead of hidden."
  (let ((text (nlk:trimmed
               (with-output-to-string (out)
                 (loop for char across (or text "")
                       unless (char= char #\Return) do (write-char char out))))))
    (if (<= (length text) max-chars)
        (values text nil)
        (values (concatenate 'string
                             (string-right-trim '(#\Space #\Tab #\Newline)
                                                (subseq text 0 max-chars))
                             "...")
                t))))

(defun format-score (score)
  "SCORE to three places, tolerant of a backend that omits it."
  (format nil "~,3f" (coerce (or score 0) 'single-float)))

(defun describe-scope (ids)
  "A retrieval scope named without spending the context on every id: a
deployment can hold dozens, and the ids stay in the verb's own arguments."
  (cond ((null ids) "(deployment default)")
        ((<= (length ids) 3) (format nil "~{~a~^, ~}" ids))
        (t (format nil "~d knowledge bases" (length ids)))))

(defun document-label (record)
  "A hit's document by its title, then its source filename, then its id."
  (or (nlk:json-value record :string "knowledge_title")
      (nlk:json-value record :string "knowledge_filename")
      (nlk:json-value record :string "knowledge_id")
      "(untitled)"))

(defun chunk-index (record)
  (or (nlk:json-value record :integer "chunk_index") -1))

;;; --- scope -------------------------------------------------------------------

(defun resolve-scope (requested knowledge-ids)
  "The knowledge bases a call searches: the ones it names, else the configured
scope, else - when it scoped by document instead - none, else every one the
credential can see. An unscoped call reaches WeKnora as a refusal on the
retrieval route and as an answer grounded in nothing on the RAG route, so the
last fallback is what makes an unconfigured deployment work at all."
  (let ((configured (coerce (setting :knowledge-base-ids) 'list)))
    (cond ((plusp (length requested)) requested)
          ((plusp (length configured)) configured)
          ((plusp (length knowledge-ids)) '())
          (t (visible-ids)))))

(defun no-scope-refusal (tool verb)
  (fail "~a: this WeKnora credential can see no knowledge base, so there is nothing to ~a. ~
Check the deployment's API key scope." tool verb))

;;; --- list ---------------------------------------------------------------------

(defun knowledge-bases ()
  "The deployment's knowledge bases, one line each. => the verbs' answer."
  (let ((bases (loop for record across (list-knowledge-bases)
                      collect (list :id (or (nlk:json-value record :string "id") "")
                                    :name (or (nlk:json-value record :string "name") "")
                                    :description (or (nlk:json-value record :string "description") "")))))
    (if (null bases)
        "No knowledge base is available to this WeKnora credential."
        (with-output-to-string (out)
          (format out "~d knowledge base~:p:" (length bases))
          (dolist (base bases)
            (format out "~%- ~a (id: ~a)~@[ - ~a~]"
                    (getf base :name) (getf base :id)
                    (let ((description (getf base :description)))
                      (and (plusp (length description)) description))))))))

;;; --- search -------------------------------------------------------------------

(defun project-hit (record rank)
  "One hit as the fields the model and a follow-up call need."
  (multiple-value-bind (content truncated)
      (clip-text (nlk:json-value record :string "content") (setting :max-chunk-chars))
    (list :rank rank
          :knowledge-id (or (nlk:json-value record :string "knowledge_id") "")
          :document (document-label record)
          :chunk-index (chunk-index record)
          :score (nlk:json-value record :number "score")
          :content content
          :truncated truncated)))

(defun project-documents (documents passages knowledge-base-ids knowledge-ids)
  "The by-name matches worth showing: inside the scope the caller asked for and
not already a passage hit. WeKnora's by-name search spans the whole tenant, so
the scope filter is what keeps a narrowed call from reporting documents outside
it."
  (let ((seen (mapcar (lambda (hit) (getf hit :knowledge-id)) passages))
        (out '()))
    (dolist (record (coerce (or documents '()) 'list) (nreverse out))
      (let* ((id (nlk:json-value record :string "id"))
             (base (nlk:json-value record :string "knowledge_base_id")))
        (when (and id
                   (not (member id seen :test #'equal))
                   (if knowledge-ids
                       (member id knowledge-ids :test #'equal)
                       (member base knowledge-base-ids :test #'equal)))
          (push id seen)
          (push (list :knowledge-id id
                      :title (or (nlk:json-value record :string "title")
                                 (nlk:json-value record :string "file_name")
                                 id)
                      :knowledge-base (or (nlk:json-value record :string "knowledge_base_name") base))
                out))))))

(defun render-search (query scope passages documents)
  "The search answer: the passages first, then the documents a title-like query
named, which passage retrieval alone would miss."
  (let ((named (if (null documents)
                   ""
                   (format nil "~2%Documents named ~s (read them with ~a):~%{~a~^~%}"
                           query (tool-name "read_document")
                           (loop for document in documents
                                 collect (format nil "- ~a | knowledge_id: ~a | in ~a"
                                                 (getf document :title)
                                                 (getf document :knowledge-id)
                                                 (getf document :knowledge-base)))))))
    (cond
      ((null passages)
       (if (null documents)
           (format nil "Nothing in WeKnora matched ~s (searched: ~a). Try a differently ~
worded query, or widen the knowledge base scope."
                   query (describe-scope scope))
           (format nil "No passage matched ~s, but its name matches document(s).~a" query named)))
      (t (format nil "~d passage~:p for ~s (searched: ~a):~2%~{~a~^~2%~}~a"
                 (length passages) query (describe-scope scope)
                 (loop for hit in passages
                       collect (format nil "[~d] ~a | score ~a | chunk ~d | knowledge_id: ~a~%~a~@[~%(passage truncated)~]"
                                       (getf hit :rank) (getf hit :document)
                                       (format-score (getf hit :score))
                                       (getf hit :chunk-index) (getf hit :knowledge-id)
                                       (getf hit :content) (getf hit :truncated)))
                 named)))))

(defun search (query &key knowledge-base-ids knowledge-ids max-results)
  "Search the deployment and answer the matching passages verbatim, plus the
documents a title-like QUERY names. Every hit carries the knowledge_id
READ-DOCUMENT pages through."
  (let* ((knowledge-ids (coerce (or knowledge-ids '()) 'list))
         (requested (coerce (or knowledge-base-ids '()) 'list))
         (scope (resolve-scope requested knowledge-ids))
         (limit (let ((asked (and (integerp max-results) (plusp max-results) max-results)))
                  (min (or asked (setting :max-results)) (setting :max-results)))))
    (when (and (null scope) (null knowledge-ids))
      (no-scope-refusal (tool-name "search") "search"))
    (let* ((hits (coerce (search-passages query scope knowledge-ids) 'list))
           (records (subseq hits 0 (min limit (length hits))))
           (passages (loop for record in records
                           for rank from 1
                           collect (project-hit record rank)))
           ;; By-name matching is an enrichment, and older WeKnora builds may
           ;; not route it: losing it must not cost the model its passages.
           (documents (project-documents (ignore-errors (find-documents query limit))
                                          passages scope knowledge-ids)))
      (render-search query scope passages documents))))

;;; --- read a document ------------------------------------------------------------

(defun chunk-text (record)
  (or (nlk:json-value record :string "content") ""))

(defun read-document (knowledge-id &key page page-size)
  "One document's passages, reassembled in storage order. Page 1 leads with the
title and the summary WeKnora generated, so a long document can be judged
without paging through it."
  (let* ((page (if (and (integerp page) (plusp page)) (min page 10000) 1))
         (page-size (if (and (integerp page-size) (plusp page-size)) (min page-size 100) 20)))
    (multiple-value-bind (items total answer-page answer-page-size)
        (chunks knowledge-id page page-size)
      (let* ((ordered (sort (coerce items 'list) #'< :key #'chunk-index))
             ;; Only page 1 is worth the extra call and the summary's tokens:
             ;; by page 2 the reader has already seen what this document is.
             (metadata (if (= 1 page)
                           (handler-case (document knowledge-id) (error () nil))
                           nil))
             (joined (format nil "~{~a~^~2%~}"
                             (loop for chunk in ordered collect (chunk-text chunk))))
             (budget (* (setting :max-chunk-chars) (max 1 (min (length ordered) 10)))))
        (multiple-value-bind (content truncated) (clip-text joined budget)
          (multiple-value-bind (summary summary-cut)
              (clip-text (or (nlk:json-value metadata :string "description") "")
                         (setting :max-chunk-chars))
            (declare (ignore summary-cut))
            (let ((title (or (nlk:json-value metadata :string "title")
                             (nlk:json-value metadata :string "file_name")
                             "")))
              (if (zerop (length ordered))
                  (format nil "Document ~a has no passage on page ~d (~d passage~:p in total)."
                          (if (plusp (length title)) (format nil "~a (~a)" title knowledge-id) knowledge-id)
                          answer-page total)
                  (format nil "Document ~a, passages ~d of ~d (page ~d):~@[~2%Summary: ~a~]~2%~a~@[~%(content truncated)~]~@[~2%(more passages available: request page ~d)~]"
                          (if (plusp (length title))
                              (format nil "~a (~a)" title knowledge-id)
                              knowledge-id)
                          (length ordered) total answer-page
                          (and (plusp (length summary)) summary)
                          content truncated
                          (and (< (* answer-page answer-page-size) total) (1+ answer-page)))))))))))

;;; --- ask --------------------------------------------------------------------------

(defun reference-lines (references limit)
  "The citations of an answer, one line each, newest-last, at most LIMIT."
  (loop for reference in (subseq (coerce (or references '()) 'list)
                                 0 (min limit (length (coerce (or references '()) 'list))))
        for index from 1
        collect (format nil "[~d] ~a | chunk ~d | knowledge_id: ~a"
                        index (document-label reference) (chunk-index reference)
                        (or (nlk:json-value reference :string "knowledge_id") ""))))

(defun ask (query &key knowledge-base-ids agent-id session-id web-search)
  "WeKnora's own composed answer to QUERY, with its citations and the session
id a follow-up continues in. AGENT-ID (or the configured one) selects the
server-side ReAct pipeline; without it the RAG pipeline answers."
  (let* ((requested (coerce (or knowledge-base-ids '()) 'list))
         (agent (or agent-id (setting :agent-id)))
         ;; A custom agent resolves its own scope server-side from its knowledge
         ;; base selection mode, and ids sent from here would override that as an
         ;; explicit mention. The RAG pipeline has no such default: it retrieves
         ;; only what the request names and answers from nothing when it names
         ;; nothing.
         (scope (if agent
                    (or requested (coerce (setting :knowledge-base-ids) 'list))
                    (resolve-scope requested '()))))
    (when (and (null agent) (null scope))
      (no-scope-refusal (tool-name "ask") "answer from"))
    (let* ((session (or session-id
                         (create-session (format nil "nodecode: ~a" (clip-text query 60)))))
           (answer (answer query scope agent session (and web-search t)))
           (citations (reference-lines (getf answer :references) (setting :max-results)))
           (tools (getf answer :tool-calls))
           (text (getf answer :answer)))
      (format nil "~a~@[~2%Citations:~%{~a~^~%}~]~@[~2%WeKnora tools used: ~{~a~^, ~}~]~2%WeKnora session: ~a (pass session_id to ask a follow-up in context)"
              (if (zerop (length text))
                  "WeKnora returned an empty answer. Retry with a more specific question, or retrieve passages instead."
                  text)
              citations tools
              (or (getf answer :session-id) session)))))
