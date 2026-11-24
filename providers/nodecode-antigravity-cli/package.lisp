;;;; package.lisp --- the NODECODE-ANTIGRAVITY-CLI package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-antigravity-cli
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:call-agy-streaming))

(in-package #:nodecode-antigravity-cli)

(nlk:define-peripheral antigravity-cli :not-running t)

(defparameter +provider+ "antigravity-cli"
  "The provider id and the lane's name: what /models lists models under and
what a turn's frozen config names.")

(defparameter +default-models+
  (vector "gemini-3.8-flash" "gemini-3.1-pro" "gpt-oss-120b"
          "claude-sonnet-4-6" "claude-opus-4-6-thinking")
  "The roster the Antigravity CLI serves on a typical account; `agy models'
shows what an account actually has, so the section may name others.")

(defparameter +preamble+
  (format nil "~{~a~^~%~}"
          '("You are operating in HEADLESS INFERENCE MODE: your native tools"
            "(run_command, write_to_file, view_file, edit_file, ...) are disabled"
            "and will be silently rejected, and all tool execution and filesystem"
            "work is performed by Nodecode on the host side."
            "If you need to call a tool, emit ONLY <tool_call>{...}</tool_call> blocks"
            "in your text output, each a JSON object with 'id', 'type': 'function'"
            "and 'function': {'name': '...', 'arguments': '<JSON encoded string>'}."
            "After emitting tool calls, STOP generating; never simulate a tool result."))
  "What every prompt tells the CLI's model, so tools come back as text blocks
the cell parses instead of native steps the CLI would run itself. Rewritten
from the original's _PROMPT_PREAMBLE (prompt.py).")
