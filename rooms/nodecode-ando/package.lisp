;;;; package.lisp --- the Ando cell's one package, and its one value.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system: the folder loader finds
;;;; START-CELL as the exported symbol of package NODECODE-ANDO, and every
;;;; contribution is filed under "nodecode-ando", the folder's name. The
;;;; nickname is what the model types: (ando:reply "...").
;;;;
;;;; ANDO-CONNECTION is declared here, before the files that read it, so
;;;; the wire and the credential file share one shape and neither has to
;;;; know how the other fills it.

(defpackage #:nodecode-ando
  (:use #:cl)
  (:nicknames #:ando)
  (:export #:start-cell #:stop-cell #:reply #:ando-error #:ando-error-detail))

(in-package #:nodecode-ando)

(defstruct (ando-connection (:constructor make-connection) (:copier nil))
  "One redeemed invitation: the key, the workspace and agent it is for, the
MCP URL it names, and the session that URL answered."
  ;; PATH is the credential file it was read from or written to; the rest are
  ;; the invitation's own fields, validated on the way in.
  api-key workspace-id membership-id installer-id mcp-url origin display-name receiver-id path
  (session-id nil)
  (initialized-p nil)
  (next-id 0))
