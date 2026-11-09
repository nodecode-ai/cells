;;;; provider.lisp --- what Sabi is: its address, its one alias, its headers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from vizuh/sabi's Hermes plugin (see NOTICE): plugin.yaml and
;;;; __init__.py. Sabi is a proxy on the machine that speaks the OpenAI Chat
;;;; Completions wire and makes every routing decision itself - model, effort
;;;; and provider - per request. A client therefore sends one alias
;;;; (`sabi-code') and its own opaque attribution, and nothing else: Hermes
;;;; did exactly that from a middleware, this cell does it from the walk.
;;;;
;;;; Deliberately as narrow as the original. The headers ride only a round
;;;; whose provider is `sabi'; the client id is this client's own name, never
;;;; another product's; a session id that is not a short opaque string is
;;;; left off rather than rewritten; nothing is derived from the prompt.

(in-package #:nodecode-sabi)

(defparameter +base+ "http://127.0.0.1:8787/v1"
  "Sabi's Chat Completions base: a loopback address the proxy listens on.")

(defparameter +alias+ "sabi-code"
  "The one model id a Sabi round asks for. Sabi names the upstream model
itself, so the picker lists this alias and the wire carries it as it is.")

(defparameter +client+ "nodecode"
  "The value of X-Sabi-Client: who is asking, for the proxy's own attribution.")

(defparameter +owned-headers+ '("X-Sabi-Client" "X-Sabi-Session" "X-Sabi-Turn")
  "The headers this cell owns: a stale copy of one is dropped before it is set.")

(defparameter +opaque-chars+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._:-"
  "The characters an opaque id is made of: the original's own set.")

(defun opaque-id (value)
  "VALUE as an attribution id, or NIL when it is missing or not opaque: 1 to
128 characters of +OPAQUE-CHARS-."
  (and (stringp value)
       (<= 1 (length value) 128)
       (every (lambda (character) (find character +opaque-chars+)) value)
       value))

(defun session-attribution ()
  "The live session's id when it is an opaque one, else NIL. A session that
is not there yet, or an id no proxy should read, simply leaves the header off."
  (let ((session (and (boundp 'nle::*session*) (symbol-value 'nle::*session*))))
    (and session (opaque-id (ignore-errors (nle::session-id session))))))

(defun attribution-headers ()
  "The attribution a Sabi round carries: this client's name, and the session
id when there is an opaque one."
  (let ((session (opaque-id (session-attribution))))
    (append (list (cons "X-Sabi-Client" +client+))
            (when session (list (cons "X-Sabi-Session" session))))))

(defun model-row ()
  "The one catalog entry this provider offers: Sabi's alias, with the context
window the section states (Sabi spends it per upstream model, so the section's
number is the floor a turn is sized against)."
  (nle::make-catalog-model "Sabi Code" (setting :context-tokens) 8192
                            #("text") #("text") '() nil nil t nil))
