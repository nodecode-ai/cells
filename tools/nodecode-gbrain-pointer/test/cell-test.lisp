;;;; cell-test.lisp --- lifecycle, the tool, the round, /gbrain-pointer, the parsing.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a stubbed GBrain: that a
;;;; start installs the tool, /gbrain-pointer, the manual and the round hook and
;;;; a stop takes every one of them back; that a section with no credentials is
;;;; refused at the start and never dials; that the tool asks the configured
;;;; endpoint with the section's token and limit and names what GBrain said when
;;;; it refuses; that the client pair mints a token before it searches; that a
;;;; turn hands over what the last search found, once, into its own request; and
;;;; that a flagged or duplicate row never becomes a pointer.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "gbrain-pointer"
  (:fixture with-no-gbrain-env)
  (:config)
  (:hooks :round)
  (:help :gbrain-pointer)
  (:command "gbrain-pointer")
  (:running (is (nle::find-tool "gbrain_pointer") "the search tool is registered")
            (is (eq 'nodecode-gbrain-pointer::search-tool
                    (third (nle::find-tool "gbrain_pointer"))) "the tool is this cell's own body")
            (is (equal 3 (nodecode-gbrain-pointer::setting :limit)))
            (is (eq t (nodecode-gbrain-pointer::setting :auto))))
  (:stopped (is (null (nle::find-tool "gbrain_pointer")) "stop unregisters the tool"))
  (:refused ("limit" 0)
            ("mcp_url" "ftp://brain.example/mcp")
            ("mcp_url" "http://brain.example/mcp")
            ("access_token" "")))

;;; --- the endpoint, before anything is dialled -----------------------------------------

(deftest gbrain-pointer-cell-a-plain-http-host-is-allowed-when-the-section-says-so ()
  (with-no-gbrain-env
    (is (null (nodecode-gbrain-pointer::url-trouble "http://127.0.0.1:3131/mcp" nil)) "loopback is fine over http")
    (is (search "not loopback"
                (nodecode-gbrain-pointer::url-trouble "http://brain.example/mcp" nil)) "another host is refused over plain http")
    (is (null (nodecode-gbrain-pointer::url-trouble "http://brain.example/mcp" t)) "unless allow_http says the path is already encrypted")
    (is (search "credentials"
                (nodecode-gbrain-pointer::url-trouble "http://user:pw@127.0.0.1:3131/mcp" nil)) "userinfo in the URL is refused")))

(deftest gbrain-pointer-cell-the-token-url-keeps-the-path-prefix ()
  (with-no-gbrain-env
    (is (equal "http://127.0.0.1:3131/token"
               (nodecode-gbrain-pointer::token-url "http://127.0.0.1:3131/mcp")) "the origin's /token")
    (is (equal "https://brain.example/gbrain/token"
               (nodecode-gbrain-pointer::token-url "https://brain.example/gbrain/mcp/")) "a path in front of /mcp is kept")))

;;; --- the tool through the real dispatch seam --------------------------------------------

(deftest gbrain-pointer-cell-the-tool-asks-the-configured-brain-with-the-section-s-token ()
  (with-gbrain ()
    (gbrain-answering "/mcp"
                       (answer-with (search-answer-body
                                     (list (list :slug "projects/alpha"
                                                 :title "Alpha project notes"
                                                 :source "docs")))))
    (let ((result (execute-wire-call "gp-1" "gbrain_pointer" "{\"query\":\"alpha\"}")))
      (is (search "Retrieved reference pointers" (nle::message-content result)))
      (is (search "- [docs:projects/alpha] Alpha project notes" (nle::message-content result))))
    (let* ((request (first (gbrain-requests)))
           (body (gbrain-body request))
           (params (gethash "params" body))
           (arguments (gethash "arguments" params)))
      (is (eq :post (first request)))
      (is (equal "http://127.0.0.1:3131/mcp" (second request)) "the section's endpoint")
      (is (equal "Bearer test-token" (gbrain-header request "authorization")) "the section's token")
      (is (equal "2025-03-26" (gbrain-header request "mcp-protocol-version")))
      (is (equal "tools/call" (gethash "method" body)))
      (is (equal "search" (gethash "name" params)) "GBrain's own search tool")
      (is (equal "alpha" (gethash "query" arguments)))
      (is (= 3 (gethash "limit" arguments)) "the section's limit")
      (is (= 1 (gethash "snippet_chars" arguments)) "the smallest snippet cap")
      (is (equal "lean" (gethash "fields" arguments))))
    ;; a call that leaves out the contract's required member is refused before the body
    (let ((refused (execute-wire-call "gp-2" "gbrain_pointer" "{}")))
      (is (search "query" (nle::message-content refused)) "the refusal names the member"))
    (is (= 1 (length (gbrain-requests))) "one call each, nothing else")))

(deftest gbrain-pointer-cell-a-source-filter-and-a-limit-travel-with-the-search ()
  (with-gbrain ("source" "docs" "limit" 5)
    (gbrain-answering "/mcp"
                       (answer-with (search-answer-body
                                     (list (list :slug "a" :title "A" :source "docs")))))
    (is (search "- [docs:a] A" (gbrain-pointer:recall "anything at all")))
    (let* ((arguments (gethash "arguments" (gethash "params" (gbrain-body (first (gbrain-requests)))))))
      (is (equal "docs" (gethash "source_id" arguments)) "the section's source filter")
      (is (= 5 (gethash "limit" arguments))))))

(deftest gbrain-pointer-cell-a-refusal-names-what-gbrain-said ()
  (with-gbrain ()
    (gbrain-answering "/mcp" (answer-with "{\"detail\":\"brain unavailable\"}" 503))
    (let ((answer (gbrain-pointer:recall "anything at all")))
      (is (search "ERROR: GBRAIN-POINTER-ERROR" answer) "a failure the model reads")
      (is (search "HTTP 503" answer) "with the status")
      (is (search "brain unavailable" answer) "and GBrain's own sentence"))
    (is (search "HTTP 503" nodecode-gbrain-pointer::*last-error*) "and /gbrain-pointer keeps it")))

(deftest gbrain-pointer-cell-the-client-pair-mints-a-token-before-it-searches ()
  (with-gbrain ("access_token" "" "client_id" "nc-reader" "client_secret" "s3cret")
    (gbrain-answering "/token" (answer-with "{\"access_token\":\"minted\",\"token_type\":\"bearer\"}"))
    (gbrain-answering "/mcp"
                       (answer-with (search-answer-body
                                     (list (list :slug "a" :title "A" :source "docs")))))
    (is (search "- [docs:a] A" (gbrain-pointer:recall "anything at all")))
    (let* ((requests (gbrain-requests))
           (mint (first requests))
           (search (second requests)))
      (is (equal "http://127.0.0.1:3131/token" (second mint)) "the origin's token endpoint")
      (is (search "grant_type=client_credentials" (fourth mint)))
      (is (search "client_secret=s3cret" (fourth mint)))
      (is (equal "Bearer minted" (gbrain-header search "authorization")) "the minted token, not the client pair"))))

;;; --- the round: what the last search found, into this request --------------------------

(defun prompt-text (turn)
  "The last <system-reminder> text recorded on TURN, or NIL."
  (nlk:json-value (nlk:events :turn-id (if (stringp turn) turn (nlk:durable-turn-turn-id turn))
                               :kind nlk::+kind-turn-context-prompt+
                               :order :newest :as :payload)
                  :string "text"))

(deftest gbrain-pointer-cell-a-turn-searches-for-the-next-and-hands-over-what-it-found ()
  (with-session-store ("gp-round")
    (with-gbrain ()
      (gbrain-answering "/mcp"
                         (answer-with (search-answer-body
                                       (list (list :slug "projects/alpha"
                                                   :title "Alpha project notes"
                                                   :source "docs")))))
      (with-durable-turn (turn "gp-round" :input "what do we know about the alpha project" :current t)
        (nodecode-gbrain-pointer::on-round
         (list :session-id "gp-round" :turn nle::*current-durable-turn* :round 0))
        (is (null (prompt-text turn)) "the first turn of a conversation has nothing to hand over yet")
        (is (await (:timeout 5)
                   (gethash "gp-round" nodecode-gbrain-pointer::*pointers*)) "and its own words are searched in the background"))
      (with-durable-turn (turn "gp-round" :input "and what did we decide about beta" :current t)
        (nodecode-gbrain-pointer::on-round
         (list :session-id "gp-round" :turn nle::*current-durable-turn* :round 0))
        (let ((text (prompt-text turn)))
          (is (search "Retrieved reference pointers" text) "the block says what it is")
          (is (search "- [docs:projects/alpha] Alpha project notes" text) "and carries what the last search found"))
        ;; a later round of the same turn re-injects nothing: the block is handed out once
        (nodecode-gbrain-pointer::on-round
         (list :session-id "gp-round" :turn nle::*current-durable-turn* :round 1))
        (is (null (nodecode-gbrain-pointer::take-pointers "gp-round")) "nothing is left waiting"))
      (let ((queries (loop for request in (gbrain-requests)
                           collect (gethash "query"
                                             (gethash "arguments"
                                                      (gethash "params" (gbrain-body request)))))))
        (is (member "what do we know about the alpha project" queries :test #'equal) "the turn's own words went to the brain")
        (is (member "and what did we decide about beta" queries :test #'equal))))))

(deftest gbrain-pointer-cell-a-thin-turn-and-a-switched-off-auto-never-search ()
  (with-session-store ("gp-quiet")
    (with-gbrain ("auto" nil)
      (with-durable-turn (turn "gp-quiet" :input "something worth searching for" :current t)
        (nodecode-gbrain-pointer::on-round
         (list :session-id "gp-quiet" :turn nle::*current-durable-turn* :round 0))
        (is (null (prompt-text turn)))
        (is (null (gbrain-requests)) "auto off dials nothing")))
    (with-gbrain ()
      (with-durable-turn (turn "gp-quiet" :input "thanks" :current t)
        (nodecode-gbrain-pointer::on-round
         (list :session-id "gp-quiet" :turn nle::*current-durable-turn* :round 0))
        (is (null (gethash "gp-quiet" nodecode-gbrain-pointer::*pending*)) "an acknowledgement is not worth a search")
        (is (null (gbrain-requests)) "and nothing was dialled")))))

;;; --- /gbrain-pointer ------------------------------------------------------------------

(deftest gbrain-pointer-cell-slash-shows-the-endpoint-and-searches-by-hand ()
  (with-gbrain ()
    (let ((line (cell-entry "nodecode-gbrain-pointer" "gbrain-pointer" "" "s1")))
      (is (search "gbrain-pointer: bearer token at http://127.0.0.1:3131/mcp" line) "the endpoint and how it asks")
      (is (search "limit 3" line)))
    (is (null (gbrain-requests)) "a status line dials nothing")
    (gbrain-answering "/mcp"
                       (answer-with (search-answer-body
                                     (list (list :slug "concepts/beta" :title "Beta" :source "wiki")))))
    (is (search "- [wiki:concepts/beta] Beta"
                (cell-entry "nodecode-gbrain-pointer" "gbrain-pointer" "beta" "s1")))
    (is (= 1 (length (gbrain-requests))) "one search for one question")))

;;; --- the parsing, without a wire ---------------------------------------------------------

(deftest gbrain-pointer-cell-a-flagged-or-duplicate-row-never-becomes-a-pointer ()
  (let* ((rows (list (list :slug "a" :title "First" :source "docs")
                      (list :slug "a" :title "First again" :source "docs")
                      (list :slug "bad" :title "Flagged" :source "docs" :injection t)
                      (list :slug "b" :title (format nil "Second~%line") :source "docs")
                      (list :slug "c" :title "Third" :source "docs")))
         (raw (search-answer-body rows)))
    (is (equal '("- [docs:a] First" "- [docs:b] Second line")
               (nodecode-gbrain-pointer::pointer-lines raw 2)) "the limit counts clean rows, and a title is one line")
    (is (equal '("- [docs:a] First" "- [docs:b] Second line" "- [docs:c] Third")
               (nodecode-gbrain-pointer::pointer-lines (sse-body raw) 5)) "a flagged row and its duplicate are dropped; SSE parses the same")
    (is (null (nodecode-gbrain-pointer::pointer-lines "not json" 3)))
    (is (null (nodecode-gbrain-pointer::pointer-lines
               (nlk:encode-json-object (nlk:json-object "result" (nlk:json-object "isError" t)))
               3)) "an error envelope carries no pointers")))
