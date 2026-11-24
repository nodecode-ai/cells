;;;; package.lisp --- the package, and the family every refusal is in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a launcher and a skill over the Agent Vault app's own MCP
;;;; server: two tools, search_sessions and get_session, spoken to over
;;;; newline-delimited JSON-RPC on the server's stdin and stdout. This cell is
;;;; the client half of that in Lisp, with the same two tools against the same
;;;; server (see NOTICE). NLK:DEFINE-PERIPHERAL makes VAULT-ERROR, FAIL,
;;;; *VAULT*, RUNNING-SETTINGS and DEFINE-VERB; the three children below
;;;; classify how a connection fails, and each prints as the text the model
;;;; reads.

(defpackage #:nodecode-agentvault
  (:use #:cl)
  (:shadow #:search)
  (:export #:start-cell #:stop-cell
           #:search #:session #:status #:tools
           #:agentvault-error #:vault-offline #:vault-timeout #:vault-refused))

(in-package #:nodecode-agentvault)

(nlk:define-peripheral agentvault :not-running t)

(nlk:define-error vault-offline (agentvault-error) ()
  (:report "~a" detail)
  (:documentation "The app is not where vault.app_path says, or its server
closed the stream: the archive is not reachable from here."))

(nlk:define-error vault-timeout (agentvault-error) ()
  (:report "~a" detail)
  (:documentation "The server did not answer within vault.timeout_seconds. The
connection is kept; the read that ran out is the one that is lost."))

(nlk:define-error vault-refused (agentvault-error) ()
  (:report "~a" detail)
  (:documentation "The server answers with an error of its own: a revoked
binding, an index that is not ready, a query too broad. DETAIL is its text."))
