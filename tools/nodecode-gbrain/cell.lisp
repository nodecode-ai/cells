;;;; cell.lisp --- the primer, the section, the tool, /gbrain, the round hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two surfaces over one client. Gbrain_recall is the model asking the
;;;; brain itself; /gbrain is the operator asking. The :ROUND hook is the
;;;; one the original plugin is: before a turn's first model request, the
;;;; brain's pages for the conversation so far are recorded as context, so
;;;; the model reads them without having decided to look.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "gbrain": {"url": "http://127.0.0.1:3131/mcp",
;;;;              "token_env": "GBRAIN_TOKEN",
;;;;              "max_pages": 5, "ambient": true}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-gbrain)

(defparameter *primer*
  "A GBrain server holds this operator's own pages - people, projects, notes - and answers two
questions: which named entities a conversation mentions, and which pages a subject touches. The
nodecode-gbrain cell puts them in front of the model.
  (gbrain:recall \"what to look for\" &key window max-pages min-confidence)   one STRING: the
      top pages, each a title, its slug, a strength and a one-line preview, strongest first.
      Entities the text names come first, then the pages a hybrid topical query answers.
  The gbrain_recall tool is the same call, and a turn's first request also carries the pages
      for the conversation so far (gbrain.ambient; set it to false to ask alone).
Use it when the answer may already be in the operator's own notes - a person, a project, a
decision made earlier - and say which page you read. A page is a pointer, not the record: the
preview is cut short, so read the page itself before relying on a detail.
ERROR: GBRAIN-ERROR names the problem; with no token in gbrain.token, gbrain.token_env or
gbrain.token_file, tell the operator which one to set. An unreachable brain is not a failed turn."
  "What (help :gbrain) answers while the cell runs.")

;;; --- the conversation the brain is asked about -------------------------------------

(defun message-role (message)
  "MESSAGE's role, lowercased: user, assistant, or anything else."
  (let ((role (ignore-errors (nlk::message-event-role message))))
    (cond ((and (stringp role) (plusp (length role))) (string-downcase role))
          ((ignore-errors (nle::user-role-message-p message)) "user")
          (t "assistant"))))

(defun message-text (message)
  "MESSAGE's text, as one line, or NIL."
  (let ((content (or (ignore-errors (nle:message-content message))
                     (ignore-errors (nlk::message-event-content message)))))
    (cond ((stringp content) content)
          (t (let ((parts (loop for part in (elements content)
                                 for text = (or (and (stringp part) part)
                                                (valued (field part "text")))
                                 when text collect text)))
               (and parts (format nil "~{~a~^ ~}" parts)))))))

(defun window-for (session-id)
  "The last turns of SESSION, oldest first, as user:/assistant: lines, or NIL
when there is nothing to ask about. Never signals: a closed store, an unknown
session and a history that does not read all answer NIL."
  (when (and (stringp session-id) (nlk:store-open-p))
    (let* ((turns (ignore-errors (nle::history-turns-for-session (nle:intern-session session-id))))
           (lines (loop for unit in (last turns (setting :history-turns))
                        append (loop for message in (ignore-errors (nle::history-turn-messages unit))
                                     for role = (message-role message)
                                     for body = (valued (message-text message))
                                     when body
                                       collect (format nil "~a: ~a" role
                                                       (clipped (strip-injected body) 500))))))
      (and lines (format nil "~{~a~%~}" lines)))))

;;; --- the verb ----------------------------------------------------------------------

(define-verb recall (text &key window)
  "Ask the brain for TEXT: (gbrain:recall \"what\") answers the top pages as
one string. Answers an empty-brain sentence rather than NIL, and signals
GBRAIN-ERROR when the brain refuses or does not answer."
  (let* ((asked (or (valued text) (fail "say what to look for: (gbrain:recall \"what\")")))
         (pages (recall-pages asked :window window)))
    (or (format-pages pages)
        (format nil "GBrain has no page for ~s." asked))))

;;; --- the tool and the command --------------------------------------------------

(nlk:define-contract gbrain-recall-arguments
  (:doc "Ask this machine's GBrain brain - the operator's own pages on people, projects and older decisions - for the pages a name or a subject touches. Answers the pages strongest first: title, slug, strength, a one-line preview. Named entities in the text are matched first, then a topical query fills in. Reach for it before answering from memory about a person, a project or what was decided before; then read the page it names, since a preview is cut short.")
  ("query" :text :required t
   :doc "what to look for: a name, a project, or the question in the operator's own words")
  ("window" :string
   :doc "a short multi-turn window (user: and assistant: lines) when an entity named earlier in the conversation is the one that matters"))

(defun recall-tool (args)
  "The gbrain_recall tool: one lookup, its pages as the answer."
  (let ((text (gbrain-recall-arguments-query args)))
    (recall (or (valued text) (gbrain-recall-arguments-window args)))))

(defun run-command (args session-id)
  "One /gbrain: what the brain has for ARGS, or how to use it."
  (declare (ignore session-id))
  (let ((ask (valued args)))
    (if ask
        (recall ask)
        "Usage: /gbrain WHAT - a name, a project, or a question; the model asks with gbrain_recall.")))

;;; --- the ambient half --------------------------------------------------------------

(defun ambient (op next)
  "At a turn's first round: the brain's pages for the conversation so far,
recorded as context in the request about to be built. Silent when the section
turns it off, the turn is a bare acknowledgement, or the brain does not
answer -- a missing brain never costs a turn."
  (ignore-errors
    (when (and (eql 1 (getf op :round)) (setting :ambient) (getf op :turn))
      (let ((window (window-for (getf op :session-id))))
        (when (and window (not (trivial-p window)))
          (let ((block (format-pages (recall-pages window :window window))))
            (when block
              (nlk:record-turn-context-prompt (getf op :turn) :text block)))))))
  (funcall next op))

;;; --- the one declaration -----------------------------------------------------------

(nle:define-cell gbrain
  (:section ("gbrain")
    (:guide "point url at the GBrain server's HTTP MCP endpoint (gbrain serve --http); give a token inline, in token_env, or in the token file gbrain mints under its state directory; every other member has a working default")
    ("url" :string :default "http://127.0.0.1:3131/mcp"
     :doc "the GBrain server's HTTP MCP endpoint")
    ("token" :secret
     :doc "an inline bearer token (a token the GBrain server minted)")
    ("token_env" :env
     :doc "the environment variable holding the bearer token")
    ("token_file" :string :default ""
     :doc "a file holding the bearer token, as gbrain auth writes one")
    ("max_pages" :integer :default 5 :min 1
     :doc "how many pages one lookup answers with")
    ("min_confidence" :choice :options '("0.4" "0.5" "0.6" "0.7" "0.8") :default "0.6"
     :doc "the weakest named-entity hit kept")
    ("history_turns" :integer :default 6 :min 1
     :doc "how many past turns the entity window carries")
    ("synopsis_chars" :integer :default 400 :min 40
     :doc "characters of preview per page")
    ("max_context_bytes" :integer :default 3200 :min 200
     :doc "the byte ceiling on one block")
    ("ambient" :boolean :default t
     :doc "put the brain's pages in front of every turn, not only when the model asks")
    ("timeout_seconds" :integer :default 8 :min 1
     :doc "one request's ceiling"))
  (:tool "gbrain_recall" (nlk:contract 'gbrain-recall-arguments) 'recall-tool)
  (:command "gbrain" 'run-command
            :description "Ask this machine's GBrain brain"
            :argument-hint "WHAT")
  (:hook :round #'ambient)
  (:help :gbrain "gbrain:recall asks this machine's GBrain brain for the pages a name or subject touches" *primer*))
