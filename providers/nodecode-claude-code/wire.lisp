;;;; wire.lisp --- a Messages body, as the CLI is handed it and as it comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure transforms, no I/O. The round starts as the body Nodecode's own
;;;; Anthropic lane built (ANTHROPIC-REQUEST-BODY): system, messages, tools,
;;;; max_tokens, thinking. The CLI is handed that body in three pieces — the
;;;; system text as its --system-prompt-file, the messages as stream-json
;;;; frames it replays, and everything else as CLAUDE_CODE_EXTRA_BODY, which it
;;;; lays over the request it writes — and the request it writes is sent with
;;;; the round's own messages in place of the CLI's (REQUEST-MESSAGES).
;;;;
;;;; What the CLI does to a replayed history is why these look the way they do:
;;;;
;;;;   - an assistant frame whose message names no model, or another model,
;;;;     loses its thinking blocks; one naming the model the CLI runs keeps
;;;;     them, signature and all
;;;;   - extra-body keys replace the CLI's own: tools, max_tokens, thinking,
;;;;     output_config, context_management
;;;;   - the CLI adds its per-request context to the newest turn (the date,
;;;;     the account's email): inside the last tool_result's text on some
;;;;     models, as a trailing role:system message on others, and the next
;;;;     round replays that turn without it
;;;;
;;;; That last edit is why the messages are the round's own. A thinking block
;;;; is signed against every message before it, so a turn sent with the date
;;;; and replayed without it would fail the API's check for every block after
;;;; it: the API would reject them with anthropic-thinking-prefix-mismatch
;;;; (kind=blocks_removed, item=text) and drop them unbilled, so the model
;;;; would see none of its earlier reasoning. The round's own messages only
;;;; grow (ANTHROPIC-MESSAGES keeps its thinking behind the prefix it was
;;;; signed with), so the CLI writes the request's headers, system blocks and
;;;; tools, and Nodecode's history is the messages.

(in-package #:nodecode-claude-code)

(defparameter +tool-prefix+ "mcp__nodecode__"
  "The name a tool rides under inside the CLI's request: the prefix the CLI
gives a tool an MCP server named nodecode lists, so a request carries the
tool names a Claude Code session with that server would.")

(defparameter +long-context-floor+ 200000
  "The window past which a model is asked for on its 1M route: the CLI behind
a custom ANTHROPIC_BASE_URL serves a model's 200K window unless the name
carries [1m].")

(defun block-type (block)
  "BLOCK's type, or NIL for anything that is not a content block."
  (nlk:json-value block :string "type"))

(defun plain-block (block &rest pairs)
  "A copy of the content BLOCK without its cache marker, PAIRS set over it:
the CLI places its own markers, and a replayed one would spend one of the
four a request may carry."
  (let ((copy (apply #'nlk:copy-json-object block pairs)))
    (remhash "cache_control" copy)
    copy))

(defun content-blocks (content)
  "A message's CONTENT as a vector of content blocks: a string is one text
block."
  (if (stringp content)
      (vector (nlk:json-object "type" "text" "text" content))
      (coerce (or content #()) 'vector)))

(defun replay-content (message)
  "MESSAGE's content blocks as the CLI replays them: markers off, and every
tool call under the name the request advertises it by."
  (map 'vector (lambda (block)
                 (if (equal (block-type block) "tool_use")
                     (plain-block block "name" (concatenate 'string +tool-prefix+
                                                            (nlk:json-value block :string "name")))
                     (plain-block block)))
       (content-blocks (gethash "content" message))))

(defun replay-frames (body model)
  "The stream-json frames that replay BODY's messages to the CLI, in order:
every user frame but the last marked shouldQuery false, every assistant
frame naming MODEL."
  ;; The last frame is the one the CLI answers, so the history must end on a
  ;; user turn: the CLI has no assistant prefill.
  (let ((messages (coerce (or (nlk:json-value body :array "messages") #()) 'list)))
    (unless (equal "user" (nlk:json-value (car (last messages)) :string "role"))
      (error 'nle::provider-config-error
             :status 400
             :detail "the claude-code lane cannot continue an assistant reply: the round must end on a user turn"))
    (loop for (message . more) on messages
          for role = (nlk:json-value message :string "role")
          collect (nlk:json-object
                   "type" role
                   "message" (nlk:json-object
                              "role" role
                              :when (equal role "assistant") "model" model
                              "content" (replay-content message))
                   :when (and more (equal role "user")) "shouldQuery" nil))))

(defun system-text (body)
  "BODY's system prompt as one text, its blocks joined by a blank line."
  (let ((system (gethash "system" body)))
    (if (stringp system)
        system
        (format nil "~{~a~^~%~%~}"
                (loop for block across (or system #())
                      for text = (nlk:json-value block :string "text")
                      when text collect text)))))

(defun extra-body (body)
  "The members of BODY the CLI's request takes over its own: the tools under
their request names, the output ceiling, thinking, the effort and the tool
choice."
  ;; Thinking absent is thinking off: the CLI's own default is a budget near
  ;; max_tokens. With it off, the CLI's clear-thinking context edit is
  ;; refused by the API, so the edits go too.
  (let ((extra (nlk:json-object
                "tools" (map 'vector (lambda (tool)
                                       (plain-block tool "name" (concatenate 'string +tool-prefix+
                                                                             (nlk:json-value tool :string "name"))))
                             (or (nlk:json-value body :array "tools") #()))
                "max_tokens" (gethash "max_tokens" body)
                "thinking" (or (nlk:json-value body :object "thinking")
                               (nlk:json-object "type" "disabled"))
                :opt "output_config" (nlk:json-value body :object "output_config")
                :opt "tool_choice" (nlk:json-value body :object "tool_choice"))))
    (when (equal "disabled" (nlk:json-value extra :string "thinking" "type"))
      (setf (gethash "context_management" extra) (nlk:json-object "edits" #())))
    extra))

(defparameter +cli-efforts+ '("low" "medium" "high" "xhigh" "max")
  "The levels the CLI's --effort takes.")

(defun effort (body)
  "The effort BODY asks for when the CLI's --effort can say it, or NIL: without
the flag the CLI states its own default level to the model instead."
  (let ((effort (nlk:json-value body :string "output_config" "effort")))
    (and (member effort +cli-efforts+ :test #'equal) effort)))

(defun model-argument (model window)
  "What the CLI's --model is for MODEL: its 1M route when the WINDOW
Nodecode sized the round to is past +LONG-CONTEXT-FLOOR+."
  (if (and (integerp window) (> window +long-context-floor+) (not (find #\[ model)))
      (concatenate 'string model "[1m]")
      model))

;;; --- the request the CLI wrote ------------------------------------------------

(defun request-messages (body)
  "BODY's messages as the request sends them: every tool call under the name
the request advertises it by, every other block, Nodecode's cache markers
among them, as Nodecode built it."
  (map 'vector (lambda (message)
                 (nlk:copy-json-object
                  message
                  "content" (map 'vector (lambda (block)
                                           (if (equal (block-type block) "tool_use")
                                               (nlk:copy-json-object block "name" (concatenate 'string +tool-prefix+
                                                                                               (nlk:json-value block :string "name")))
                                               block))
                                 (content-blocks (gethash "content" message)))))
       (or (nlk:json-value body :array "messages") #())))

(defun unprefixed-fold (fold)
  "FOLD, handed every frame with the tool call a content_block_start opens
back under Nodecode's own name."
  ;; The one place a request name turns back into a tool name: the fold opens
  ;; the call from this frame, so every part the stream emits after it, the
  ;; assembled message included, carries the name the turn dispatches.
  (lambda (frame finish record)
    (let ((block (and (equal "content_block_start" (nlk:json-value frame :string "type"))
                      (nlk:json-value frame :object "content_block"))))
      (when (equal "tool_use" (block-type block))
        (let ((name (nlk:json-value block :string "name")))
          (when (and name (uiop:string-prefix-p +tool-prefix+ name))
            (setf (gethash "name" block) (subseq name (length +tool-prefix+)))))))
    (funcall fold frame finish record)))
