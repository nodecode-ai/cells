;;;; cell.lisp --- the manual, the section, the six tools, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model finds the tools in its own catalog, listed with the schema
;;;; each declaration below projects; (help :prompt-enhance) carries the
;;;; rules for using them, and every request carries the one line the :HELP
;;;; clause gives.
;;;;
;;;; A rewrite is one POST <base_url>/chat/completions: the enhancer's body
;;;; as the system instruction with the original's closing line, the draft as
;;;; the user message. Nothing else leaves the process, and the answer is the
;;;; rewritten prompt, returned to the caller to put where it likes -- the
;;;; composer is the room's business, not this cell's.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "prompt-enhance": {"model": "gpt-5", "base_url": "https://api.openai.com/v1",
;;;;                      "api_key_env": "OPENAI_API_KEY"}
;;;; A vetoed section ("enabled": false) registers no tool at all.

(in-package #:nodecode-prompt-enhance)

;;; --- the manual ------------------------------------------------------------------

(defparameter +primer+
  "Prompt Enhance rewrites a draft into the prompt you meant, outside the chat, and keeps
the prompts you reuse. The tools are prompt_enhance, prompt_compose, prompt_save,
prompt_list, prompt_show and prompt_delete.

  prompt_enhance draft [enhancer]   rewrite the draft and answer only the rewritten
      prompt. enhancer names a saved one -- clarify, tighten (the default), grammar,
      expand, summarize, or any enhancer you saved in the enhancers folder. The call
      goes to the chat-completions endpoint the section names, so the draft leaves the
      process once, to that endpoint, and nowhere else.
  prompt_compose name [draft]       a saved prompt with the draft under it, as
      \"Context:\", ready to send as one message. No model call.
  prompt_save name body [folder]    save a prompt, replacing one of the same name in
      that folder; the draft you just wrote is the usual body, and the first line of
      it is the usual name.
  prompt_list [query] [folder]      the library, a line a prompt: its name, its folder,
      the first of its body.
  prompt_show name                  one saved prompt, whole.
  prompt_delete name                remove it from the library.

The library is one JSON file under the home: enhancers are instructions, so an enhancer
is sent verbatim with the draft and its words are the behaviour; prompts are text. The
original's composer buttons, its glyphs, its drag reorder and its one-call session model
override have no seat in a headless tool -- reordering is not needed when a listing is
searched, and the section's model is the rewrite's model already. An empty answer, a
missing key or an endpoint that refused is ERROR: PROMPT-ENHANCE-ERROR with the reason:
fix that, do not repeat the call."
  "What (help :prompt-enhance) answers while the cell runs.")

;;; --- what a tool call carries ---------------------------------------------------------

(nlk:define-contract enhance-arguments
  (:doc "A draft to rewrite, and the enhancer whose instruction rewrites it.")
  ("draft" :text :required t
   :doc "the text to rewrite, as it stands: a composer draft, a question, an instruction")
  ("enhancer" :text :default "tighten"
   :doc "the saved enhancer to apply: clarify, tighten, grammar, expand, summarize, or one saved in the enhancers folder"))

(nlk:define-contract compose-arguments
  (:doc "A saved prompt, and the draft it carries as context.")
  ("name" :text :required t :doc "the saved prompt's name")
  ("draft" :text :default ""
   :doc "the current draft, appended under the prompt as context; empty sends the prompt alone"))

(nlk:define-contract save-arguments
  (:doc "A prompt to keep.")
  ("name" :text :required t :doc "what to call it, one line, at most 80 characters")
  ("body" :text :required t :doc "the prompt itself, whole")
  ("folder" :text :default "" :doc "the folder to keep it in; empty is `prompts'"))

(nlk:define-contract list-arguments
  (:doc "How to narrow the library listing; both empty lists everything.")
  ("query" :text :default "" :doc "text a prompt's name or body must carry, case-insensitively")
  ("folder" :text :default "" :doc "only this folder"))

(nlk:define-contract named-arguments
  (:doc "One saved prompt, by name.")
  ("name" :text :required t :doc "the saved prompt's name"))

;;; --- where the rewrite goes -------------------------------------------------------------

(defparameter +route+ "/chat/completions"
  "The route under base_url: the OpenAI-compatible chat wire every provider serves.")

(defparameter +default-base+ "https://api.openai.com/v1"
  "Where a rewrite goes when the section names no base_url.")

(defparameter +closing+
  "Return only the rewritten prompt. No preamble and no code fence."
  "The original's closing instruction, verbatim: what makes one model call
answer a draft rather than a remark about it.")

(defun base-url ()
  "The section's base_url, else the public one; no trailing slash."
  (string-right-trim "/" (or (nonblank (setting :base-url)) +default-base+)))

(defun api-key-env ()
  "The environment variable the key is read from."
  (or (nonblank (setting :api-key-env)) "OPENAI_API_KEY"))

(defun api-key ()
  "The key a rewrite sends, or NIL."
  (nle::credential-env (api-key-env)))

(defun require-key ()
  "The key a rewrite sends; a refusal naming the variable when none is set.
A base of the operator's own that wants no key still needs the variable set,
to anything, which is the one rule this cell keeps."
  (or (api-key)
      (fail "no key for ~a: set the environment variable ~a and restart the cell (any value, for an endpoint of your own that takes none); the library tools need no key"
            (base-url) (api-key-env))))

(defun headers (key)
  "What a rewrite carries: the bearer KEY and a JSON body."
  `(("authorization" . ,(format nil "Bearer ~a" key))
    ("accept" . "application/json")
    ("content-type" . "application/json")))

(defun messages (instructions draft)
  "The two messages a rewrite sends: the enhancer's instruction, the draft."
  (vector (nlk:make-json-object "role" "system" "content" instructions)
          (nlk:make-json-object "role" "user" "content" draft)))

(defun request-body (instructions draft)
  "The JSON text one rewrite posts."
  (nlk:encode-json-object
   (nlk:make-json-object "model" (setting :model)
                          "messages" (messages instructions draft)
                          "temperature" 0.2
                          "max_tokens" (setting :max-tokens))))

(defun unfence (text)
  "TEXT with a code fence around it taken off: models add one however the
instruction reads, and the fence is not part of the prompt."
  (let ((text (nlk:trimmed (or text ""))))
    (when (and (>= (length text) 3) (string= "```" (subseq text 0 3)))
      (let ((break (position #\Newline text)))
        (setf text (nlk:trimmed (if break (subseq text (1+ break)) "")))))
    (when (and (>= (length text) 3) (string= "```" (subseq text (- (length text) 3))))
      (setf text (nlk:trimmed (subseq text 0 (- (length text) 3)))))
    text))

(defun error-message (decoded fallback)
  "What a refused response says, when it says anything."
  (or (nlk:json-value decoded :text "error" "message")
      (nlk:json-value decoded :text "message")
      fallback))

(defun answer-text (body)
  "The rewritten prompt a chat-completions BODY carries, or a refusal."
  (let* ((decoded (handler-case (nlk:decode-json body)
                     (error () nil)))
         (choices (and decoded (nlk:json-value decoded :array "choices")))
         (first (and choices (plusp (length choices)) (aref choices 0)))
         (text (and first (nlk:json-value first :string "message" "content")))
         (text (unfence text)))
    (cond ((plusp (length text)) text)
          (t (fail "the endpoint answered no prompt: ~a"
                   (error-message decoded (preview (or body "") 300)))))))

(defun rewrite (instructions draft)
  "One chat-completions call: INSTRUCTIONS as the system message, DRAFT as
the user's, and the rewritten prompt back. Every status is a value here, so
a refusal can say what the endpoint sent."
  (let* ((key (require-key))
         (url (format nil "~a~a" (base-url) +route+)))
    (multiple-value-bind (body status)
        (nlk:http :post url
                  :headers (headers key)
                  :content (request-body instructions draft)
                  :timeout (setting :timeout-seconds))
      (unless (<= 200 status 299)
        (fail "~a answered HTTP ~a: ~a" url status
              (or (ignore-errors (error-message (nlk:decode-json body) body))
                  (preview (or body "") 300))))
      (answer-text body))))

;;; --- the tools ---------------------------------------------------------------------

(defun instructions-for (prompt)
  "The system message an enhancer PROMPT becomes: its body, then the
original's closing line."
  (format nil "~a~%~%~a" (nlk:trimmed (getf prompt :body)) +closing+))

(defun enhancer (name)
  "The saved prompt called NAME, as one who rewrites the draft, or a refusal
naming the ones there are."
  (let ((prompt (find-prompt name)))
    (unless prompt
      (fail "no enhancer is called ~s; they are ~{~a~^, ~} (prompt_list shows every saved prompt)"
            name
            (loop for saved in (read-library)
                  when (string-equal (getf saved :folder) +enhancer-folder+)
                    collect (getf saved :name))))
    (when (zerop (length (nlk:trimmed (getf prompt :body))))
      (fail "the enhancer ~s has no instruction in it" name))
    prompt))

(defun run-enhance (arguments)
  "prompt_enhance: rewrite the draft with the enhancer and answer only the
rewritten prompt."
  (let* ((draft (nlk:trimmed (enhance-arguments-draft arguments)))
         (prompt (enhancer (enhance-arguments-enhancer arguments))))
    (when (zerop (length draft))
      (fail "there is no draft to rewrite: prompt_enhance takes the text itself"))
    (when (> (length draft) +draft-limit+)
      (fail "the draft is ~d characters, past the ~d this cell sends; cut it first"
            (length draft) +draft-limit+))
    (rewrite (instructions-for prompt) draft)))

(defun compose-text (prompt draft)
  "A saved PROMPT with DRAFT under it as context, or the prompt alone: the
original's payload, its blank line and \"Context:\" heading unchanged."
  (let ((body (nlk:trimmed (getf prompt :body)))
        (draft (nlk:trimmed (or draft ""))))
    (if (plusp (length draft))
        (format nil "~a~%~%Context:~%~a" body draft)
        body)))

(defun run-compose (arguments)
  "prompt_compose: the saved prompt with the draft as context, ready to send."
  (let ((prompt (find-prompt (compose-arguments-name arguments))))
    (unless prompt
      (fail "no saved prompt is called ~s; prompt_list shows the ones that are"
            (compose-arguments-name arguments)))
    (compose-text prompt (compose-arguments-draft arguments))))

(defun run-save (arguments)
  "prompt_save: keep a prompt, replacing that folder's one of the same name."
  (multiple-value-bind (saved replaced)
      (save-prompt (save-arguments-name arguments)
                    (save-arguments-body arguments)
                    (save-arguments-folder arguments))
    (format nil "~a ~s in [~a] (~d character~:p), in ~a"
            (if replaced "replaced" "saved")
            (getf saved :name) (getf saved :folder)
            (length (getf saved :body))
            (namestring (library-file)))))

(defun header-line (count query folder)
  "The first line of a listing: how many, and how they were narrowed."
  (format nil "~{~a~^ ~}:" (remove nil
                                    (list (format nil "~d saved prompt~:p" count)
                                          (if (plusp (length folder)) (format nil "in ~a" folder) nil)
                                          (if (plusp (length query)) (format nil "matching ~s" query) nil)))))

(defun matching (prompts query folder)
  "PROMPTS narrowed by QUERY (name or body, case-insensitively) and FOLDER."
  (let ((query (string-downcase (nlk:trimmed (or query ""))))
        (folder (nlk:trimmed (or folder ""))))
    (remove-if-not (lambda (prompt)
                     (and (or (zerop (length folder))
                              (string-equal (getf prompt :folder) folder))
                          (or (zerop (length query))
                              (search query (string-downcase
                                             (format nil "~a ~a" (getf prompt :name)
                                                     (getf prompt :body)))))))
                   prompts)))

(defun listing (query folder)
  "The library as a line a prompt, or a line saying nothing matched."
  (let* ((query (nlk:trimmed (or query "")))
         (folder (nlk:trimmed (or folder "")))
         (rows (matching (read-library) query folder)))
    (if (null rows)
        (format nil "~a~%nothing matches; the library is ~a"
                (header-line 0 query folder) (namestring (library-file)))
        (format nil "~a~%~{~a~^~%~}"
                (header-line (length rows) query folder)
                (mapcar #'prompt-line rows)))))

(defun run-list (arguments)
  "prompt_list: the library, narrowed when the call says how."
  (listing (list-arguments-query arguments) (list-arguments-folder arguments)))

(defun run-show (arguments)
  "prompt_show: one saved prompt, whole."
  (let ((name (named-arguments-name arguments)))
    (let ((prompt (find-prompt name)))
      (unless prompt
        (fail "no saved prompt is called ~s; prompt_list shows the ones that are" name))
      (format nil "~a  [~a]~%~a" (getf prompt :name) (getf prompt :folder)
              (getf prompt :body)))))

(defun run-delete (arguments)
  "prompt_delete: take a prompt out of the library."
  (let ((name (named-arguments-name arguments)))
    (format nil "removed ~d prompt~:p called ~s from ~a"
            (delete-prompt name) name (namestring (library-file)))))

(defun run-prompt-command (&optional args session-id)
  "The /prompt dialog's answer: the library, or one prompt named as the argument."
  (declare (ignore session-id))
  (let* ((args (nlk:trimmed (or args "")))
         (prompt (and (plusp (length args)) (find-prompt args))))
    (cond (prompt (format nil "~a  [~a]~%~a" (getf prompt :name) (getf prompt :folder)
                           (getf prompt :body)))
          ((plusp (length args)) (listing args ""))
          (t (listing "" "")))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell prompt-enhance
  (:section ("prompt-enhance")
    (:guide "put the chat-completions endpoint that rewrites a draft in base_url, the model it asks for in model, and the name of the environment variable holding its key in api_key_env; - then ask for prompt_enhance; the prompt library itself needs none of it and lives under the home; - /cells shows whether the folder started")
    ("base_url" :string :default +default-base+
     :doc "the chat-completions endpoint's root, e.g. https://api.openai.com/v1 or http://127.0.0.1:11434/v1")
    ("model" :string :required t
     :doc "the model that rewrites a draft, as the endpoint names it; prompt_enhance refuses until it is set")
    ("api_key_env" :string :default "OPENAI_API_KEY"
     :doc "the environment variable the endpoint's key is read from; an endpoint of your own still needs it set, to anything")
    ("max_tokens" :integer :default 4096 :min 256
     :doc "the most the rewrite may answer, in tokens")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "how long one rewrite may take"))
  (:tool "prompt_enhance" (nlk:contract 'enhance-arguments) 'run-enhance)
  (:tool "prompt_compose" (nlk:contract 'compose-arguments) 'run-compose)
  (:tool "prompt_save" (nlk:contract 'save-arguments) 'run-save)
  (:tool "prompt_list" (nlk:contract 'list-arguments) 'run-list)
  (:tool "prompt_show" (nlk:contract 'named-arguments) 'run-show)
  (:tool "prompt_delete" (nlk:contract 'named-arguments) 'run-delete)
  (:help :prompt-enhance
    "prompt-enhance: prompt_enhance rewrites a draft with a saved enhancer; prompt_save and prompt_list keep the library"
    +primer+)
  (:command "prompt" 'run-prompt-command
   :description "The prompt library: list the saved prompts, or show one"
   :session nil))
