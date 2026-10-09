;;;; live-smoke.lisp --- manual live smoke for the channel cells.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; NOT auto-run: this talks to real Discord/Telegram bot accounts. Run it
;;;; form by form from a REPL inside the repo's src/ directory, or follow
;;;; the operator flow at the bottom against the released binary.
;;;;
;;;; Prerequisites:
;;;;   - a test bot token exported, e.g. NODECODE_TELEGRAM_TOKEN /
;;;;     NODECODE_DISCORD_TOKEN
;;;;   - your own chat/channel id allowlisted (fail-closed: an empty
;;;;     allowlist refuses to start)
;;;;   - ~/.nodecode/config.jsonc carrying:
;;;;       "cells": ["nodecode-channel-kit"],
;;;;       "channels": {
;;;;         "telegram": {"enabled": true,
;;;;                      "token_env": "NODECODE_TELEGRAM_TOKEN",
;;;;                      "allowed_chats": ["<your chat id>"],
;;;;                      "require_mention": false},
;;;;         "discord":  {"enabled": true,
;;;;                      "bot_token_env": "NODECODE_DISCORD_TOKEN",
;;;;                      "allowed_channels": ["<your channel id>"],
;;;;                      "require_mention": false}}

(in-package #:cl-user)

;; 1. Load the cell stack (from src/ with the registry pushed; under a
;;    bare quicklisp REPL run (ql:register-local-projects) first).
;; (require :asdf)
;; (push (truename ".") asdf:*central-registry*)
;; (asdf:load-system :nodecode-channel-telegram)

;; 2. Boot the organism gateway; the generic cell hook reads `cells`
;;    from the shared config and starts the channel host.
;; (asdf:load-system :nodecode)
;; (nodecode.evolved:start-gateway) ; binds a kernel-assigned port and returns it

;; 3. Message the bot from your allowlisted chat. Expect, on both: a typing
;;    indicator, a status line edited in place when the turn calls a tool or
;;    thinks long, then the answer as a reply to your message, chunked when
;;    long, with the status line deleted behind it.

;; 4. Inspect lane status — the same surface /channels renders in the TUI:
;; (nodecode-channel-kit:channels-status-report)

;; 5. Continuity: the ask ran in a lane forked off the chat's room session
;;    telegram-<chat_id> / discord-<channel_id> and its exchange was written
;;    back. Resume the room in a TUI and the transcript is there:
;;      ./nodecode resume telegram-<chat_id>
;;    Kill the gateway, restart `./nodecode serve`, message again:
;;    the adapter reconnects from config and the conversation continues in
;;    the same session.

;; 6. Unwind without killing the image:
;; (nodecode.evolved:stop-gateway)

;;; Operator flow against the released binary (no REPL):
;;;   just release
;;;   ./nodecode serve
;;;   ... message the bot; /channels in an attached TUI shows lane status.
