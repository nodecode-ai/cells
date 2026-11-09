;;;; cell.lisp --- Insight: a definition of a term, with the sources it was given.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes Desktop panel: select text, get a definition in
;;;; a popup, streaming. The popup is Hermes's own UI; what it asks the
;;;; backend for is one stateless completion -- no memory, no skills, no tools
;;;; -- carrying the user's format template and, when a search is on, up to
;;;; two sources cited by URL. That is what a cell keeps: the same completion
;;;; against the OpenAI-compatible endpoint the operator names, reached as a
;;;; tool the model calls and as /insight, plus the same optional SearXNG
;;;; search for the sources.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "insight": {"base_url": "https://api.example.com/v1",
;;;;               "model": "gpt-4.1-mini",
;;;;               "api_key_env": "OPENAI_API_KEY",
;;;;               "searxng_url": "http://localhost:8888"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from BrokeSkill's Hermes Agent plugin (see NOTICE): the stateless
;;;; `hermes insight --term ... --template ... --search ...` completion, its
;;;; heldprompt and writing rules, the two-source search and citation rule,
;;;; and the section of the provider it talks to (base URL, key, model).

(in-package #:nodecode-insight)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Insight is available through the nodecode-insight cell: a definition of a term, written
by the provider you configured, with the sources it was given.
  define_term  {term, template}
      Defines TERM in markdown, in one or two hundred words, in the cell's format template
      or in the TEMPLATE you pass. The answer is JSON: term, definition, model, and sources
      (title, url, snippet) - up to two of them, from the SearXNG instance the operator
      configured, when its search setting is on. Cite those URLs when you quote it; when
      there are none, the definition stands on the model's own knowledge and says so.
      Use it for a definition the user asked for, then answer in your own words, or quote
      it where the citations matter.
The operator can type /insight <term> for the same definition. Neither surface remembers
anything: every definition is one completion, and nothing from the conversation is sent
beyond the term, the template and the two source titles and URLs."
  "What (help :insight) answers while the cell runs.")

;;; --- what the plugin's renderer asked for ---------------------------------------

(defparameter +default-template+
  "Define \"{term}\" for a knowledgeable general audience.

You have web-search context below. Use ONLY the URLs supplied there as
citations. NEVER invent URLs - if the search returned nothing usable,
omit Sources entirely and say \"no definitive online source found\".

Format (markdown):

**{term}** - one-sentence definition.
Then, only when useful: 2-3 short \"Examples\" bullets and one \"Also known as\" line.

Use fenced code blocks with a language tag for code (```js, ```python).
Cite sources in-place: when a search returned a real result, write
[Source Name](url) next to the claim it supports - use the exact URL
from the search context. Do NOT paraphrase or hand-craft URLs. Do not embed
images: none are supplied.

Writing rules (no AI slop):
- Prefer concrete nouns over abstractions.
- No filler (\"In today's world...\", \"It's important to note...\", \"Let's dive in...\").
- No em-dash overuse. No \"delve\", \"leverage\", \"robust\", \"seamless\", \"cutting-edge\".
- No breathless affirmations or sycophantic tone. State the thing, then stop.
- Short sentences. If a sentence crosses two clauses, split it.
- Plain English. If a 6-letter word replaces a 14-letter one, use it.

Keep the whole reply under ~180 words. No preamble, no closing remarks."
  "The renderer's DEFAULT_TEMPLATE, which is what a definition is written in
when the section names none. The only system instruction the provider sees:
the plugin never appended one of its own to a user's template.")

(defparameter +source-limit+ 2
  "How many search results travel with a definition. The plugin's own
citation rule says cite exactly two, so it hands the provider exactly two.")

(defparameter +snippet-chars+ 300
  "How much of a result's snippet is kept for the caller (the plugin kept the
same 300 characters for its source cards); only the title and URL are sent on.")

(defparameter +note-chars+ 300
  "How much of the provider's or the instance's own error body a refusal quotes back.")

;;; --- reading the section --------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, plus the key the section names: read once, here."
  (declare (ignore table))
  (let* ((base (string-right-trim "/" (or (getf values :base-url) "")))
         (model (nonblank (getf values :model)))
         (env (nonblank (getf values :api-key-env)))
         (file (nonblank (getf values :api-key-file)))
         (plist (copy-list values)))
    (when (and env file)
      (nlk:config-error "set one of api_key_env and api_key_file, not both"))
    (unless (or (uiop:string-prefix-p "http://" base) (uiop:string-prefix-p "https://" base))
      (nlk:config-error "base_url ~s is not an http:// or https:// URL: set it to the OpenAI-compatible base URL of your provider, e.g. \"https://api.example.com/v1\"" base))
    (unless model
      (nlk:config-error "no model: set model to the id the provider serves, e.g. \"gpt-4.1-mini\""))
    (setf (getf plist :base-url) base
          (getf plist :model) model
          (getf plist :api-key)
          (cond (env (or (nonblank (nle::credential-env env))
                         (nlk:config-error "api_key_env names ~a, which is unset or empty" env)))
                (file (or (nonblank (ignore-errors
                                      (uiop:read-file-string (nlk:expand-home file))))
                          (nlk:config-error "api_key_file ~a cannot be read, or is empty" file)))
                (t nil)))
    plist))

;;; --- answers, and the one condition ---------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (insight-error (condition)
       (nle:failure (format nil "ERROR: ~a" (insight-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: insight: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun answer-detail (body)
  "The peer's own words for a failed exchange, clipped, or NIL."
  (let* ((text (if (stringp body) body (ignore-errors (nlk:body-text body))))
         (data (ignore-errors (nlk:decode-json text)))
         (error (and (hash-table-p data) (nlk:json-value data :object "error")))
         (message (and error (nlk:json-value error :string "message")))
         (words (nlk:trimmed (or message text ""))))
    (and (plusp (length words)) (nlk:clip words +note-chars+))))

;;; --- the sources -----------------------------------------------------------------

(defun search-sources (term)
  "Up to +source-limit+ rows from the configured SearXNG instance, as plists
(:title :url :content). No instance configured, no rows and no request --
which is the plugin's own rule: nothing is fetched without a searxng URL."
  (let ((base (string-right-trim "/" (or (setting :searxng-url) ""))))
    (if (zerop (length base))
        '()
        (handler-case
            (multiple-value-bind (body status)
                (nlk:http :get (format nil "~a/search?q=~a&format=json"
                                        base (quri:url-encode term :encoding :utf-8))
                          :headers '(("accept" . "application/json"))
                          :timeout 10 :connect-timeout 10)
              (if (and (integerp status) (< status 400))
                  (let* ((text (if (stringp body) body (nlk:body-text body)))
                         (data (ignore-errors (nlk:decode-json text)))
                         (rows '()))
                    (loop for row across (or (and (hash-table-p data)
                                                   (nlk:json-array data "results"))
                                             #())
                          for url = (nlk:json-value row :string "url")
                          when (and url (plusp (length url)))
                            do (push (list :title (or (nlk:json-value row :string "title") "")
                                           :url url
                                           :content (nlk:clip (or (nlk:json-value row :string "content") "")
                                                              +snippet-chars+))
                                     rows))
                    (let ((rows (nreverse rows)))
                      (subseq rows 0 (min (length rows) +source-limit+))))
                  '()))
          ;; A search that fails is not a definition that fails: the
          ;; definition goes on with no sources and says so.
          (error () '())))))

(defun cited-lines (sources)
  "The search context the provider is handed: a title and a URL each, never
a snippet -- the plugin sent the same two fields."
  (loop for source in sources
        for index from 1
        collect (format nil "[~d] ~a - ~a" index
                        (getf source :title) (getf source :url))))

(defun user-prompt (term sources)
  "TERM, and the sources when there are any, as the plugin built it."
  (let ((context (cited-lines sources)))
    (if context
        (format nil "~a~%~%Search results (cite EXACTLY TWO of them inline like [1], [2] - never more, never invented sources):~%~{~a~%~}~%Only these URLs may be cited."
                term context)
        term)))

(defun template-for (template term)
  "TEMPLATE with its {term} replaced by TERM: the plugin left the placeholder
standing, since the term also travels in the user message."
  (let ((text (or (nonblank template) +default-template+)))
    (if (search "{term}" text)
        (ppcre:regex-replace-all "\\{term\\}" text
                                  (lambda (target &rest positions)
                                    (declare (ignore target positions))
                                    term))
        text)))

;;; --- the provider ---------------------------------------------------------------

(defun authorization ()
  "The key the section names, as a header, when there is one: a local server
may take none."
  (let ((key (setting :api-key)))
    (and (stringp key) (plusp (length key))
         (list (cons "authorization" (format nil "Bearer ~a" key))))))

(defun completion (system user)
  "SYSTEM and USER, sent to the configured provider; its whole answer, or a
refusal carrying the provider's own words. One request, no streaming: a cell
answers with text, and the panel's streaming was the renderer's."
  (let* ((url (concatenate 'string (setting :base-url) "/chat/completions"))
         (payload (nlk:json-object
                   "model" (setting :model)
                   "max_tokens" 1200
                   "temperature" 0.3
                   "messages" (vector (nlk:json-object "role" "system" "content" system)
                                      (nlk:json-object "role" "user" "content" user)))))
    (multiple-value-bind (body status)
        (handler-case
            (nlk:http :post url
                      :headers (cons (cons "content-type" "application/json")
                                     (authorization))
                      :content (nlk:encode-json-object payload)
                      :timeout (setting :timeout)
                      :connect-timeout (min 20 (setting :timeout)))
          (error (condition)
            (fail "~a did not answer: ~a" url
                  (nlk:one-line (princ-to-string condition)))))
      (unless (and (integerp status) (< status 400))
        (fail "~a answered HTTP ~a~@[: ~a~]" url status (answer-detail body)))
      (let* ((text (if (stringp body) body (nlk:body-text body)))
             (data (ignore-errors (nlk:decode-json text)))
             (choices (and (hash-table-p data) (nlk:json-array data "choices")))
             (choice (and choices (plusp (length choices)) (aref choices 0)))
             (message (and choice (nlk:json-value choice :object "message")))
             (answer (and message (nlk:json-value message :string "content"))))
        (or (nonblank answer)
            (fail "~a answered no message" (setting :model)))))))

;;; --- the one verb ---------------------------------------------------------------

(defun define (term &key template (search (setting :search)))
  "TERM's definition from the configured provider, in TEMPLATE or the
cell's own, with the sources SEARCH found. => (values TEXT SOURCES MODEL).
The term, the template and the two sources are the whole of what leaves the
machine besides the key."
  (let* ((term (nlk:trimmed (or term "")))
         (sources (if search (search-sources term) '())))
    (when (zerop (length term))
      (fail "no term to define"))
    (values (completion (template-for template term) (user-prompt term sources))
            sources
            (setting :model))))

(defun define-term (call)
  "The define_term tool: a definition as JSON, sources included."
  (answering
    (let ((term (nlk:trimmed (or (term-definition-term call) "")))
          (template (term-definition-template call)))
      (when (zerop (length term))
        (return-from define-term (refuse "no term: pass the word or phrase to define")))
      (multiple-value-bind (text sources model)
          (define term :template template)
        (nlk:encode-json-object
         (nlk:json-object
          "term" term
          "model" model
          "definition" text
          "sources" (coerce (loop for source in sources
                                   collect (nlk:json-object
                                            "title" (getf source :title)
                                            "url" (getf source :url)
                                            "snippet" (getf source :content)))
                            'vector)))))))

(defun run-slash (args session-id)
  "/insight typed alone: how to use it. /insight <term>: the definition, its
sources as URLs after it, and the model that wrote it."
  (declare (ignore session-id))
  (handler-case
      (let ((term (nlk:trimmed (or args ""))))
        (if (zerop (length term))
            (format nil "insight: /insight <term> writes a definition with the sources it was given; the model can call define_term the same way. Set base_url, model and api_key_env in the insight section.~%A term looks like: /insight idempotent")
            (multiple-value-bind (text sources model)
                (define term)
              (format nil "~a~@[~%~%Sources:~%~{  ~a~%~}~]~@[~%(~a)~]"
                      text
                      (and sources (loop for source in sources
                                         collect (getf source :url)))
                      model))))
    (insight-error (condition)
      (format nil "ERROR: ~a" (insight-error-detail condition)))
    (error (condition)
      (format nil "ERROR: insight: ~a" (nlk:one-line (princ-to-string condition))))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell insight
  (:section ("insight")
    (:guide "point the cell at any OpenAI-compatible endpoint: base_url, model, and the name of the variable holding the key; searxng_url is what gives a definition its sources; next step: ask for a definition, or type /insight <term>")
    ("base_url" :string
     :doc "the provider's OpenAI-compatible base URL, scheme included, e.g. https://api.example.com/v1; the cell POSTs <base_url>/chat/completions")
    ("model" :string
     :doc "the model id the provider serves, e.g. gpt-4.1-mini")
    ("api_key_env" :env
     :doc "the environment variable holding the provider key (OPENAI_API_KEY); optional, since a local server may take none")
    ("api_key_file" :path
     :doc "a file holding the provider key, which the file's own permissions guard; set one of api_key_env and api_key_file, not both")
    ("template" :string
     :doc "the format template: the whole system instruction, with {term} replaced by the term; empty, the cell's built-in one")
    ("searxng_url" :string
     :doc "a SearXNG instance with format=json enabled, for up to two sources; empty, a definition carries no sources and nothing is searched")
    ("search" :boolean :default t
     :doc "whether a definition looks for sources first; it needs searxng_url to find any")
    ("timeout" :integer :default 120 :min 1
     :doc "how long the provider may take to answer, in seconds"))
  (:settings #'read-settings)
  (:help :insight
         "insight: the define_term tool and /insight <term> write a cited definition from your configured provider"
         *manual*)
  (:command "insight" 'run-slash
            :catches 'insight-error
            :description "Define a term with the sources it was given")
  (:tool "define_term" (nlk:contract 'term-definition) #'define-term))
