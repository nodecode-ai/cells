;;;; verbs.lisp --- the five verbs, what each sends, and what it answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin served Hermes five things under one key: System One judgments,
;;;; structured extraction, reranking, embeddings and server-tool web search.
;;;; Each is one POST to an OpenRouter route and one answer to read, so each
;;;; is one DEFINE-VERB here: the model reaches it through eval, the four with
;;;; a wire shape are also tools (cell.lisp), and both go through the same
;;;; body. A verb answers values a Lisp caller reads; the tool form of it
;;;; answers the text the model reads.

(in-package #:nodecode-openrouter-everything)

(defun require-text (value label)
  "VALUE as a non-empty string, or the refusal naming LABEL."
  (or (nonempty value)
      (fail "openrouter-everything: ~a is required" label)))

(defun usage-plist (answer)
  "The spend ANSWER reports, as a plist, or NIL."
  (usage-of answer))

;;; --- decide -----------------------------------------------------------------

(defun decision-questions (options questions instructions)
  "The questions a decision carries: QUESTIONS as given, else one choice
question built from OPTIONS, a map of option name to its criteria."
  (cond
    ((and (hash-table-p questions) (plusp (hash-table-count questions))) questions)
    ((hash-table-p options)
     (nlk:json-object
      "decision"
      (nlk:json-object
       "type" "choice"
       "instructions" (or (nonempty instructions) "Select the best option for the given state.")
       "criteria" options)))
    (t (fail "openrouter-everything: decide needs \"options\" (a map of option name to criteria) or ~
\"questions\" (OpenRouter's own schema)"))))

(define-verb decide (state &key options questions instructions model)
  "Answer one decision about STATE with OpenRouter's Decisions API: calibrated
probabilities over candidate choices or numeric criteria in one pass.
STATE is the context to judge. Give either OPTIONS -- a map of option name to
its description or criteria -- or QUESTIONS, OpenRouter's own multi-question
schema (question name to (:type \"choice\"|\"score\"|\"bool\" :instructions TEXT
:criteria ...)), a hash table or a plist. MODEL overrides the configured
decision model. => (values ANSWERS MODEL USAGE): ANSWERS the API's own answer
object (nlk:json-value reads it), MODEL the model that answered, USAGE
(:tokens N :cost USD-or-NIL)."
  (let* ((state (require-text state "state"))
         (model (or (nonempty model) (configured-model :decision)))
         (answer (exchange :post (alpha-url "/decisions")
                           :content (nlk:encode-json-object
                                     (nlk:json-object
                                      "model" model
                                      "state" state
                                      "questions" (decision-questions (object-of options "options")
                                                                      (object-of questions "questions")
                                                                      instructions))))))
    (values (or (nlk:json-value answer :object "answers") (nlk:json-object))
            (or (nonempty (nlk:json-value answer :string "model")) model)
            (usage-plist answer))))

;;; --- extract-fields ---------------------------------------------------------

(define-verb extract-fields (state fields &key model)
  "Extract typed fields from STATE with OpenRouter's Extractions API: each
field's value, span and confidence, in one pass. FIELDS maps a field name to
its spec -- (:type \"text\"|\"number\"|\"boolean\"|\"category\"|\"list\"
[:items] [:decimals] [:unit] [:choices]) -- a hash table or a plist. MODEL
overrides the configured extraction model. => (values ANSWERS MODEL USAGE),
ANSWERS the API's own answer object."
  (let* ((state (require-text state "state"))
         (fields (object-of fields "fields"))
         (model (or (nonempty model) (configured-model :extraction)))
         (answer (exchange :post (alpha-url "/extractions")
                           :content (nlk:encode-json-object
                                     (nlk:json-object "model" model "state" state "fields" fields)))))
    (values (or (nlk:json-value answer :object "answers") (nlk:json-object))
            (or (nonempty (nlk:json-value answer :string "model")) model)
            (usage-plist answer))))

;;; --- rerank -----------------------------------------------------------------

(define-verb rerank (query documents &key top-n model)
  "Rank DOCUMENTS -- a list of strings -- by how well each answers QUERY, with
OpenRouter's Rerank API. TOP-N keeps only that many. MODEL overrides the
configured rerank model. => (values RESULTS MODEL USAGE): RESULTS a list of
the API's own result objects, best first (:index, :relevance-score, and the
document as the service returns it)."
  (let* ((query (require-text query "query"))
         (documents (coerce (string-list documents "documents") 'vector))
         (model (or (nonempty model) (configured-model :rerank)))
         (answer (exchange :post (api-url "/rerank")
                           :content (nlk:encode-json-object
                                     (apply #'nlk:make-json-object
                                            (append (list "model" model
                                                          "query" query
                                                          "documents" documents)
                                                    (when (and (integerp top-n) (plusp top-n))
                                                      (list "top_n" top-n))))))))
    (values (array-value answer "results")
            (or (nonempty (nlk:json-value answer :string "model")) model)
            (usage-plist answer))))

;;; --- embed ------------------------------------------------------------------

(defun cosine (a b)
  "The cosine similarity of two vectors of numbers, 0 when either is zero."
  (let ((dot 0.0d0) (na 0.0d0) (nb 0.0d0))
    (loop for x in a for y in b
          do (incf dot (* (float x 1.0d0) (float y 1.0d0)))
             (incf na (* (float x 1.0d0) (float x 1.0d0)))
             (incf nb (* (float y 1.0d0) (float y 1.0d0))))
    (if (and (plusp na) (plusp nb))
        (/ dot (sqrt (* na nb)))
        0.0d0)))

(defun similarity-matrix (vectors)
  "VECTORS as the matrix of every pair's cosine similarity, rows in order."
  (mapcar (lambda (a) (mapcar (lambda (b) (cosine a b)) vectors)) vectors))

(define-verb embed (input &key model (include-vectors nil))
  "Embed TEXT strings with OpenRouter's Embeddings API. INPUT is one string or
a list of them; they go in one request, so comparing them costs one call.
MODEL overrides the configured embeddings model; INCLUDE-VECTORS keeps the
full float vectors in the answer, which is otherwise a preview. => (values
PLIST MODEL USAGE): PLIST (:count N :dimensions D :similarity-matrix M
:preview P [:vectors V]) -- M is every pair's cosine similarity, so a caller
can see which two texts are near without the arithmetic."
  (let* ((texts (string-list input "input"))
         (model (or (nonempty model) (configured-model :embeddings)))
         (answer (exchange :post (api-url "/embeddings")
                           :content (nlk:encode-json-object
                                     (nlk:json-object "model" model
                                                      "input" (coerce texts 'vector)))))
         (vectors (loop for entry in (array-value answer "data")
                        collect (array-value entry "embedding"))))
    (when (null vectors)
      (fail "openrouter-everything: the embeddings API answered no vector"))
    (let ((dimensions (length (first vectors))))
      (values (list :count (length vectors)
                    :dimensions dimensions
                    :similarity-matrix (when (> (length vectors) 1) (similarity-matrix vectors))
                    :preview (mapcar (lambda (vector) (subseq vector 0 (min 5 (length vector)))) vectors)
                    :vectors (and include-vectors vectors))
              (or (nonempty (nlk:json-value answer :string "model")) model)
              (usage-plist answer)))))

;;; --- search -----------------------------------------------------------------

(defun search-hits (message)
  "MESSAGE -- a chat completion's message carrying url_citation annotations
or a top-level citation list -- as a list of plists (:title :url :description)."
  (let ((hits '())
        (seen (make-hash-table :test #'equal)))
    (flet ((add (url title description)
             (when (and (nonempty url) (not (gethash url seen)))
               (setf (gethash url seen) t)
               (push (list :title (or (nonempty title) url)
                           :url url
                           :description (or description ""))
                     hits))))
      (dolist (annotation (array-value message "annotations"))
        (let ((citation (if (equal "url_citation" (nlk:json-value annotation :string "type"))
                            (nlk:json-value annotation :object "url_citation")
                            annotation)))
          (when (hash-table-p citation)
            (add (nlk:json-value citation :string "url")
                 (nlk:json-value citation :string "title")
                 (nlk:json-value citation :string "content")))))
      (dolist (citation (array-value message "citations"))
        (add (and (stringp citation) citation) nil nil)))
    (nreverse hits)))

(define-verb search (query &key (limit 5) model)
  "Search the web through OpenRouter's web_search server tool, on the key this
cell already holds. QUERY is what to look for; LIMIT caps the hits (1-25).
MODEL overrides the configured web-search model, which drives the tool.
=> (values HITS ANSWER MODEL): HITS a list of plists (:title :url
:description), ANSWER the model's synthesized sentence or two, MODEL the model
that answered."
  (let* ((query (require-text query "query"))
         (model (or (nonempty model) (configured-model :web-search)))
         (answer (exchange :post (api-url "/chat/completions")
                           :content (nlk:encode-json-object
                                     (nlk:json-object
                                      "model" model
                                      "messages" (vector (nlk:json-object
                                                          "role" "system"
                                                          "content" (concatenate
                                                                     'string
                                                                     "Use the web_search tool to search the web and answer"
                                                                     " with a clear, factual synthesis with citations."))
                                                         (nlk:json-object
                                                          "role" "user"
                                                          "content" query))
                                      "tools" (vector (nlk:json-object
                                                       "type" "openrouter:web_search"
                                                       "parameters" (nlk:json-object
                                                                     "engine" "auto"
                                                                     "max_results"
                                                                     (max 1 (min (or limit 5) 25)))))))))
         (choices (array-value answer "choices"))
         (message (and choices (nlk:json-value (first choices) :object "message"))))
    (values (search-hits message)
            (or (nonempty (nlk:json-value message :string "content")) "")
            (or (nonempty (nlk:json-value answer :string "model")) model))))
