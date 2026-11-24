;;;; cell-test.lisp --- lifecycle, the runtime's tools, the command, the refusals.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring, not Kiwi: the config
;;;; surface, every tool registered and taken back, each call's frame over the
;;;; stubbed stdio seam, the text a model reads back, the error a refusing or
;;;; silent runtime answers, and the pin the cell starts.

(in-package #:nodecode.test)

;;; --- fixtures ------------------------------------------------------------------

(defun tools-frame ()
  "A tools/list result naming two tools, as the runtime describes them."
  (frame-result
   (nlk:json-object
    "tools" (vector (nlk:json-object "name" "kiwi_search"
                                       "description" "Find suppliers on the Kiwi Network")
                     (nlk:json-object "name" "kiwi_handoff"
                                       "description" "Produce the trade handoff")))))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "kiwi"
  (:help :kiwi)
  (:command "kiwi")
  (:running
   (is (nle::find-tool "kiwi_search") "kiwi_search is registered")
   (is (nle::find-tool "kiwi_request_quotes") "kiwi_request_quotes is registered")
   (is (nle::find-tool "kiwi_accept_agreement") "kiwi_accept_agreement is registered")
   (is (nle::find-tool "kiwi_tools") "kiwi_tools is registered")
   (is (equal "@harrylabsj/kiwi@0.12.24" (nodecode-kiwi::setting :package)) "the pinned runtime the section carries"))
  (:stopped
   (is (null (nle::find-tool "kiwi_search")) "stop unregisters the tools")
   (is (null (nle::find-tool "kiwi_tools")) "every one of them"))
  (:refused
   ("command" "")
   ("package" "")
   ("timeout_seconds" 1)))

(deftest kiwi-cell-a-blank-launcher-says-what-to-set ()
  ;; The two settings a call cannot work without refuse at start, in words that
  ;; name the key, and install nothing.
  (let ((command (signals-error nlk:config-refusal (kiwi-start "command" "")))
        (package (signals-error nlk:config-refusal (kiwi-start "package" " "))))
    (is (search "command" (princ-to-string command)) "the refusal names the key")
    (is (search "package" (princ-to-string package)) "and the package one"))
  (is (null (nle::find-tool "kiwi_search")) "and nothing was installed"))

(deftest kiwi-cell-starts-the-pinned-runtime ()
  (with-kiwi ()
    (is (equal '("npx" "-y" "@harrylabsj/kiwi@0.12.24" "mcp" "serve")
               (nodecode-kiwi::runtime-argv)) "the child is npx, the pinned package, and the runtime's own serve verbs")
    (is (null (nodecode-kiwi::runtime-env)) "with no store named, the runtime's default stands")))

;;; --- a tool call, end to end over the stubbed seam -------------------------------

(deftest kiwi-cell-search-asks-the-runtime-and-answers-its-text ()
  (with-kiwi ()
    (with-runtime ((frame-result (content "2 merchants: a bottle supplier")))
      (let ((text (kiwi-call-tool "kiwi_search"
                                   "{\"query\":\"insulated bottle\",\"max_results\":2}"))
            (frame (last-frame)))
        (is (search "2 merchants" text) "the runtime's text reaches the model")
        (is (equal "tools/call" (getf frame :method)) "the call is an MCP tools/call")
        (is (equal "kiwi_search" (asked frame "name")) "naming the runtime's own tool")
        (is (equal "insulated bottle" (asked frame "arguments" "query")) "with the words the model sent")
        (is (equal "2" (asked frame "arguments" "max_results")) "and every argument it sent")))))

(deftest kiwi-cell-a-refused-frame-is-an-error-line ()
  (with-kiwi ()
    (with-runtime ((frame-error "unknown tool kiwi_follow" :code -32602))
      (let ((text (kiwi-call-tool "kiwi_search" "{\"query\":\"bottle\"}")))
        (is (search "ERROR: KIWI" text) "the model reads an error, not a throw")
        (is (search "unknown tool kiwi_follow" text) "in the runtime's own words")
        (is (search "-32602" text) "with the JSON-RPC code")))))

(deftest kiwi-cell-a-tool-that-reports-a-failure-is-an-error-line ()
  (with-kiwi ()
    (with-runtime ((frame-result (content "approval_required: show the terms first"
                                           :error-p t)))
      (let ((text (kiwi-call-tool "kiwi_accept_agreement" "{\"task_id\":\"t-1\"}")))
        (is (search "ERROR: kiwi_accept_agreement" text) "the tool's own name leads")
        (is (search "approval_required" text) "and the runtime's sentence follows")))))

(deftest kiwi-cell-a-runtime-that-will-not-start-says-so ()
  (with-kiwi ()
    (with-runtime ((error "cannot start the Kiwi buyer runtime"))
      (let ((text (kiwi-call-tool "kiwi_get_task" "{\"task_id\":\"t-1\"}")))
        (is (search "ERROR: KIWI" text) "a refused child does not throw at the model")
        (is (search "cannot start" text) "it says what happened")))))

;;; --- what the runtime answers it can do -------------------------------------------

(deftest kiwi-cell-tools-lists-what-the-runtime-answers ()
  (with-kiwi ()
    (with-runtime ((tools-frame))
      (let ((text (kiwi-call-tool "kiwi_tools" "{}"))
            (frame (last-frame)))
        (is (search "kiwi_search" text) "the runtime's tool names are listed")
        (is (search "Find suppliers" text) "with what each one does")
        (is (search "2 tools" text) "and how many the runtime answered")
        (is (equal "tools/list" (getf frame :method)) "asked over MCP's own method")))))

(deftest kiwi-cell-a-runtime-that-will-not-answer-among-the-tools-says-so ()
  (with-kiwi ()
    (with-runtime ((frame-error "no store"))
      (let ((text (kiwi-call-tool "kiwi_tools" "{}")))
        (is (search "ERROR: KIWI" text) "kiwi_tools answers an error member")
        (is (search "no store" text) "in the runtime's words")))))

;;; --- the command, and the schema a tool is registered with -------------------------

(deftest kiwi-cell-the-command-explains-itself ()
  (with-kiwi ()
    (is (search "Usage: /kiwi" (cell-entry "nodecode-kiwi" "kiwi" "help" "kiwi-cell-1")) "a bare /kiwi explains itself")
    (is (search "Unknown /kiwi subcommand"
                (cell-entry "nodecode-kiwi" "kiwi" "nonsense" "kiwi-cell-1")) "an unknown word says so")
    (with-runtime ((tools-frame))
      (let ((text (cell-entry "nodecode-kiwi" "kiwi" "tools" "kiwi-cell-1")))
        (is (search "kiwi_handoff" text) "/kiwi tools lists the runtime's tools"))
      (is (equal "tools/list" (getf (last-frame) :method)) "and asks the runtime for them"))))

(deftest kiwi-cell-a-schema-declares-the-arguments-that-matter ()
  (let* ((schema (nodecode-kiwi::schema-of "kiwi_get_task"))
         (required (nlk:json-value (getf schema :parameters) :array "required")))
    (is (equal '("task_id") (coerce required 'list)) "task_id is the one a task read needs"))
  (let* ((schema (nodecode-kiwi::schema-of "kiwi_search"))
         (properties (nlk:json-value (getf schema :parameters) :object "properties")))
    (is (nth-value 1 (gethash "query" properties)) "a search declares its query")
    (is (nth-value 1 (gethash "max_results" properties)) "and how many to list")))
