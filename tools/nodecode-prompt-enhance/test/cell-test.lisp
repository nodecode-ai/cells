;;;; cell-test.lisp --- the prompt-enhance cell: lifecycle, the library, the call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here opens a socket: the cell's one network seam (NLK:HTTP) is
;;;; stubbed with a scripted endpoint that records every request it took, so
;;;; "the draft left exactly once, to this URL, with this body" is a fact a
;;;; test reads. The library is a temp file, so no test reads or writes the
;;;; operator's own. The tools are called through the real dispatch seam
;;;; (EXECUTE-WIRE-CALL), so the schema the provider is shown and the body
;;;; that runs are the same declaration.

(in-package #:nodecode.test)

(define-test-slice "prompt-enhance" "PROMPT-ENHANCE-CELL-" :start nodecode-prompt-enhance:start-cell)

;;; --- the fixtures ------------------------------------------------------------------

(defvar *requests* '()
  "Every request the fake endpoint took, newest first: (METHOD URL ARGS).")

(defvar *calls* 0
  "A counter for wire call ids.")

(defun rewrite-answer (&optional (text "Tightened: fix the parser, then the test.")
                              (status 200))
  "What the stubbed endpoint answers, as NLK:HTTP does: a chat-completions
body carrying TEXT as its one choice, and STATUS."
  (values (nlk:encode-json-object
           (nlk:make-json-object
            "choices" (vector (nlk:make-json-object
                               "message" (nlk:make-json-object
                                          "role" "assistant" "content" text)))))
          status nil nil))

(defun tool-text (name &rest arguments)
  "The text the tool called NAME answers, called as the provider's call is."
  (princ-to-string
   (nle::message-content
    (execute-wire-call (format nil "prompt-enhance-call-~d" (incf *calls*)) name
                        (if arguments
                            (nlk:encode-json-object (apply #'nlk:make-json-object arguments))
                            "{}")))))

(defmacro with-prompt-enhance ((&rest pairs) &body body)
  "The cell started on the carrier's config, plus PAIRS, over a library file of
its own and a stubbed key, and stopped after."
  `(with-temp-file (nodecode-prompt-enhance::*library-path*)
     (setf *requests* '())
     (with-stubbed-fdefinition (nle::credential-env (name)
                                (and (string= name "OPENAI_API_KEY") "test-key"))
       (with-cell-stop ((prompt-enhance-start "model" "test-model"
                                             "base_url" "https://model.test/v1" ,@pairs))
         ,@body))))

(defmacro with-prompt-enhance-cells (&body body)
  "The fixture form every test in this slice runs inside: the cell started
over a library file of its own, the key stubbed, and stopped after."
  `(with-prompt-enhance () ,@body))

(defmacro with-endpoint (answer &body body)
  "The cell over an endpoint answering ANSWER -- a form that answers as
NLK:HTTP does -- and recording every request."
  `(with-stubbed-fdefinition (nlk:http (method url &rest args)
                              (push (list method url args) *requests*)
                              ,answer)
     ,@body))

;;; --- lifecycle -------------------------------------------------------------------------

(define-cell-lifecycle-tests "prompt-enhance"
  (:fixture with-prompt-enhance-cells)
  (:config "model" "test-model")
  (:help :prompt-enhance)
  (:command "prompt")
  (:running
   (is (equal "test-model" (nodecode-prompt-enhance::setting :model)) "model is read")
   (loop for name in '("prompt_enhance" "prompt_compose" "prompt_save"
                       "prompt_list" "prompt_show" "prompt_delete")
         do (is (nle::find-tool name) name)))
  (:stopped
   (is (null (nle::find-tool "prompt_enhance")) "stop takes every tool out of the catalog"))
  (:refused ("max_tokens" 0)))

;;; --- the library ------------------------------------------------------------------------

(deftest prompt-enhance-cell-the-library-seeds-the-originals-five (with-prompt-enhance ()
  (let ((listing (tool-text "prompt_list")))
    (dolist (name '("clarify" "tighten" "grammar" "expand" "summarize"))
      (is (search name listing) (format nil "~a is seeded" name)))
    (is (search "[enhancers]" listing) "in the original's folder")
    (is (search "5 saved prompts" listing) "and nothing else is there"))
  (let ((text (tool-text "prompt_show" "name" "tighten")))
    (is (search "Cut fluff. Keep the ask and the constraints already in the draft." text) "an enhancer is the original's instruction, verbatim"))
  (is (null (probe-file (nodecode-prompt-enhance::library-file))) "reading the seeds wrote no file")))

(deftest prompt-enhance-cell-save-list-show-and-delete-round-trip (with-prompt-enhance ()
  (let ((saved (tool-text "prompt_save"
                           "name" "Review a diff"
                           "body" "Read the diff line by line. Name every risk."
                           "folder" "review")))
    (is (search "saved \"Review a diff\" in [review]" saved) "the save said what it kept")
    (is (probe-file (nodecode-prompt-enhance::library-file)) "and wrote the library file"))
  (let ((listing (tool-text "prompt_list")))
    (is (search "Review a diff  [review]  Read the diff line by line." listing) "the listing carries it")
    (is (search "6 saved prompts" listing) "beside the five enhancers"))
  (is (search "every risk" (tool-text "prompt_list" "query" "RISK")) "a query reads the body, any case")
  (is (search "nothing matches" (tool-text "prompt_list" "query" "no-such-prompt")))
  (is (search "Name every risk" (tool-text "prompt_show" "name" "review a diff")) "a name is found however it is cased")
  (is (search "saved \"Verify\" in [review]" (tool-text "prompt_save"
                                                          "name" "Verify"
                                                          "body" "Verify the parser before the plan."
                                                          "folder" "review")) "a second prompt saves")
  (is (search "replaced \"Verify\"" (tool-text "prompt_save"
                                                         "name" "Verify"
                                                         "body" "Verify the parser again: first the plan."
                                                         "folder" "review")) "the same name in the same folder replaces")
  (is (search "first the plan" (tool-text "prompt_show" "name" "Verify")) "with the new body")
  (is (search "removed 1 prompt called \"Verify\"" (tool-text "prompt_delete" "name" "Verify")))
  (is (search "no saved prompt is called \"Verify\"" (tool-text "prompt_show" "name" "Verify")))
  (is (search "ERROR" (tool-text "prompt_delete" "name" "Verify")) "deleting it twice refuses")))

(deftest prompt-enhance-cell-compose-puts-the-draft-under-the-prompt (with-prompt-enhance ()
  (let ((text (tool-text "prompt_compose" "name" "clarify" "draft" "fix it")))
    (is (equal "Make the ask unambiguous. Keep the intent. Do not add requirements.

Context:
fix it" text) "the payload is the prompt, a blank line, Context:, the draft"))
  (let ((text (tool-text "prompt_compose" "name" "clarify")))
    (is (equal "Make the ask unambiguous. Keep the intent. Do not add requirements." text) "no draft, the prompt alone"))
  (is (null *requests*) "and nothing was sent")))

;;; --- the one model call -----------------------------------------------------------------

(deftest prompt-enhance-cell-enhance-posts-the-draft-and-answers-the-rewrite (with-prompt-enhance ()
  (with-endpoint (rewrite-answer "```text
Tightened: fix the parser, then the test.
```")
    (let ((text (tool-text "prompt_enhance"
                            "draft" "fix the parser and the test"
                            "enhancer" "tighten")))
      (is (equal "Tightened: fix the parser, then the test." text)
          (format nil "the answer is the rewritten prompt, its fence taken off: ~s" text)))
    (is (= 1 (length *requests*)) "one call")
    (let* ((request (first *requests*))
           (url (second request))
           (args (third request))
           (content (getf args :content)))
      (is (eq :post (first request)) "a POST")
      (is (equal "https://model.test/v1/chat/completions" url) "to the section's endpoint")
      (is (equal "Bearer test-key" (cdr (assoc "authorization" (getf args :headers) :test #'equal))) "with the key the variable named")
      (is (search "\"model\":\"test-model\"" content) "the section's model")
      (is (search "Cut fluff" content) "the enhancer's instruction")
      (is (search "Return only the rewritten prompt" content) "and the original's closing line")
      (is (search "fix the parser and the test" content) "the draft itself")))))

(deftest prompt-enhance-cell-a-refused-call-says-what-came-back (with-prompt-enhance ()
  (with-endpoint (rewrite-answer "{\"error\":{\"message\":\"model test-model does not exist\"}}" 404)
    (let ((text (tool-text "prompt_enhance" "draft" "fix it")))
      (is (search "ERROR" text))
      (is (search "HTTP 404" text) "the status")
      (is (search "model test-model does not exist" text) "and what the endpoint said")))))

(deftest prompt-enhance-cell-without-a-key-names-the-variable (with-prompt-enhance ()
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-endpoint (rewrite-answer)
      (let ((text (tool-text "prompt_enhance" "draft" "fix it")))
        (is (search "ERROR" text))
        (is (search "OPENAI_API_KEY" text) "the variable to set is named")
        (is (search "no key for https://model.test/v1" text) "beside the endpoint it is for")
        (is (null *requests*) "and nothing was sent"))))))

(deftest prompt-enhance-cell-an-unknown-enhancer-lists-the-ones-there-are (with-prompt-enhance ()
  (with-endpoint (rewrite-answer)
    (let ((text (tool-text "prompt_enhance" "draft" "fix it" "enhancer" "polish")))
      (is (search "no enhancer is called \"polish\"" text))
      (is (search "clarify, tighten, grammar, expand, summarize" text) "the seeded ones are named")
      (is (null *requests*) "and nothing was sent")))))
