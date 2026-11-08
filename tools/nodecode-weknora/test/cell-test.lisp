;;;; cell-test.lisp --- lifecycle, the four tools, /weknora, and every refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not WeKnora: the config
;;;; surface (an address that is not an address refuses the start, a section
;;;; with no address still starts and its first call names the setting), each
;;;; of the four tools through the real dispatch seam against a stubbed wire,
;;;; that an unscoped call resolves the visible knowledge bases once and not
;;;; per call, the handle-mode fallback, and that a refusal carries the status
;;;; and WeKnora's reason and never the key.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "weknora"
  (:fixture with-wire)
  (:config "base_url" "https://kb.test")
  (:help :weknora)
  (:command "weknora")
  (:running
   (is (equal "https://kb.test/api/v1" (nodecode-weknora::setting :base-url)) "the address is the API root")
   (dolist (name '("weknora_list_knowledge_bases" "weknora_search"
                   "weknora_read_document" "weknora_ask"))
     (is (nle::find-tool name) (format nil "the ~a tool is registered" name))))
  (:stopped
   (dolist (name '("weknora_list_knowledge_bases" "weknora_search"
                   "weknora_read_document" "weknora_ask"))
     (is (null (nle::find-tool name)) (format nil "stop unregisters ~a" name))))
  (:refused
   ("base_url" "not a url at all")
   ("max_results" 0)
   ("tool_prefix" "Bad")))

(deftest weknora-cell-the-section-decides-which-tools-exist (with-wire)
  (with-cell-stop ((weknora-start "base_url" +base-url+ "tool_prefix" "kb"
                                   "tools" (nlk:json-object "ask" nil "read_document" nil)))
    (is (nle::find-tool "kb_search"))
    (is (nle::find-tool "kb_list_knowledge_bases"))
    (is (null (nle::find-tool "kb_ask")) "tools.ask false leaves it out")
    (is (null (nle::find-tool "kb_read_document"))))
  (is (signals-error nlk:config-refusal
        (weknora-start "base_url" +base-url+
                        "tools" (nlk:json-object "search" nil "ask" nil
                                                  "read_document" nil
                                                  "list_knowledge_bases" nil))) "every tool off"))

;;; --- search --------------------------------------------------------------------

(deftest weknora-cell-search-answers-passages-and-the-documents-a-title-names (with-weknora ("base_url" +base-url+)
    (let ((text (call-tool "weknora_search" "{\"query\":\"receipt\"}")))
      (is (search "2 passages for \"receipt\"" text))
      (is (search "(searched: kb-1, kb-2)" text) "an unscoped call spans what the credential sees")
      (is (search "doc-1" text) "every hit carries the id read_document needs")
      (is (search "score 0.81" text))
      (is (search "chunk 2" text))
      (is (search "A receipt is filed within 30 days." text) "the passage is verbatim")
      (is (search "Documents named \"receipt\"" text) "the by-name match is reported")
      (is (search "Receipt procedure | knowledge_id: doc-9" text)))
    (is (= 1 (length (calls-to "/knowledge-bases"))) "the visible set is resolved once")
    (call-tool "weknora_search" "{\"query\":\"receipt\"}")
    (is (= 1 (length (calls-to "/knowledge-bases"))) "and shared by the next call")))

(deftest weknora-cell-search-takes-a-narrower-scope-and-a-lower-limit (with-weknora ("base_url" +base-url+
                                                                                       "max_results" 1
                                                                                       "knowledge_base_ids" (vector "kb-2"))
    (let ((text (call-tool "weknora_search" "{\"query\":\"receipt\"}")))
      (is (search "1 passage for \"receipt\"" text) "the configured maximum bounds one search")
      (is (search "(searched: kb-2)" text) "the configured scope is what it searches")
      (is (null (calls-to "/knowledge-bases")) "a configured scope asks the deployment for no list"))
    (let ((body (third (first (calls-to "/knowledge-search")))))
      (is (search "kb-2" body) "the scope reaches WeKnora in the body"))))

;;; --- read a document --------------------------------------------------------------

(deftest weknora-cell-read-document-leads-with-the-title-and-the-summary (with-weknora ("base_url" +base-url+)
    (let ((text (call-tool "weknora_read_document" "{\"knowledge_id\":\"doc-1\",\"page_size\":2}")))
      (is (search "Document Receipts (doc-1), passages 2 of 5 (page 1)" text))
      (is (search "Summary: How expense claims work." text) "page 1 says what the document is")
      (is (< (search "The first paragraph." text) (search "The second paragraph." text)) "storage order, not wire order")
      (is (search "more passages available: request page 2" text)))
    (let ((text (call-tool "weknora_read_document" "{\"knowledge_id\":\"doc-1\",\"page\":3}")))
      (is (null (search "Summary:" text)) "a later page pays for no metadata"))))

;;; --- ask -------------------------------------------------------------------------

(deftest weknora-cell-ask-streams-an-answer-with-its-citations (with-weknora ("base_url" +base-url+)
    (let ((text (call-tool "weknora_ask" "{\"query\":\"when is a receipt due?\"}")))
      (is (search "A receipt is filed within 30 days." text) "the streamed answer, assembled whole")
      (is (search "Citations:" text))
      (is (search "[1] Receipts | chunk 2 | knowledge_id: doc-1" text))
      (is (search "WeKnora session: sess-1" text) "the session id a follow-up continues in"))
    (is-present (created (first (calls-to "/sessions")))
      "a session is created for a question that names none"
      (is (equal :post (first created)))
      (is (search "when is a receipt due?" (third created)) "titled with the question"))))

(deftest weknora-cell-ask-refuses-a-stream-that-ended-early (with-weknora ("base_url" +base-url+)
    (push (cons "/knowledge-chat/sess-1"
                (lambda () (values (make-string-input-stream +unfinished-stream+) 200)))
          *overrides*)
    (let ((text (call-tool "weknora_ask" "{\"query\":\"when is a receipt due?\"}")))
      (is (search "ended before WeKnora completed the answer" text) "a cut answer refuses")
      (is (search "character(s) had streamed" text)))))

;;; --- the deployment says no ----------------------------------------------------------

(deftest weknora-cell-a-missing-address-refuses-naming-the-setting (with-weknora ()
    (is (nle::find-tool "weknora_search") "the tools install whatever the section says")
    (let ((text (call-tool "weknora_search" "{\"query\":\"receipt\"}")))
      (is (search "no WeKnora deployment is set" text))
      (is (search "base_url" text) "the refusal names the member to write"))
    (is (null *calls*) "and it dialled nothing")))

(deftest weknora-cell-a-refusal-carries-the-status-and-weknora-s-reason (with-weknora ("base_url" +base-url+
                                                                                        "api_key" "secret-key")
    (script "/knowledge-search"
            "{\"success\":false,\"error\":{\"message\":\"invalid api key\"}}" 401)
    (let ((text (call-tool "weknora_search" "{\"query\":\"receipt\"}")))
      (is (search "HTTP 401" text) "the status")
      (is (search "invalid api key" text) "and WeKnora's own sentence")
      (is (null (search "secret-key" text)) "never the key"))))

(deftest weknora-cell-a-restricted-key-retries-once-in-handle-mode (with-weknora ("base_url" +base-url+)
    (let ((asked 0))
      (push (cons "/knowledge-search"
                  (lambda ()
                    (if (zerop asked)
                        (progn (incf asked)
                               (values "{\"error\":{\"message\":\"resource_urls=public is not allowed\"}}" 403))
                        (values +passages+ 200))))
            *overrides*))
    (let ((text (call-tool "weknora_search" "{\"query\":\"receipt\"}")))
      (is (search "A receipt is filed within 30 days." text) "the retry answered")
      (is nodecode-weknora::*resource-blocked* "and the client stayed in handle mode"))
    (let ((calls (reverse (calls-to "/knowledge-search"))))
      (is (= 2 (length calls)) "one retry, not a loop")
      (is (search "resource_urls=public" (fourth (first calls))) "the first asked for public URLs")
      (is (null (search "resource_urls" (fourth (second calls)))) "the retry asked for handles"))
    (call-tool "weknora_search" "{\"query\":\"receipt\"}")
    (is (= 3 (length (calls-to "/knowledge-search"))) "and no later call pays it again")))

(deftest weknora-cell-a-call-without-its-required-member-never-reaches-the-wire (with-weknora ("base_url" +base-url+)
    (let ((text (call-tool "weknora_search" "{}")))
      (is (search "query" text) "the refusal names the missing member")
      (is (null *calls*) "and nothing was dialled"))))

;;; --- the command and the list tool ---------------------------------------------------

(deftest weknora-cell-the-command-says-what-this-deployment-is (with-weknora ("base_url" +base-url+)
    (let ((status (nodecode-weknora::run-command "" "s1"))
          (bases (nodecode-weknora::run-command "kbs" "s1"))
          (bogus (nodecode-weknora::run-command "bogus" "s1")))
      (is (search "weknora: https://kb.test/api/v1" status))
      (is (search "every knowledge base the credential can see" status))
      (is (search "weknora_search" status) "it names the tools this start registered")
      (is (search "2 knowledge bases:" bases))
      (is (search "- Handboek (id: kb-1) - how we work" bases))
      (is (search "- Inkoop (id: kb-2)" bases) "an undescribed base stops at its id")
      (is (search "not one of its choices" bogus)))
    (is (search "Inkoop (id: kb-2)" (call-tool "weknora_list_knowledge_bases" "{}")) "the tool answers as the command does")))
