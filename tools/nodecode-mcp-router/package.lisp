;;;; package.lisp --- the NODECODE-MCP-ROUTER package: the selector's verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed MCP-ROUTER so an eval form reads
;;;; (mcp-router:select-tools ...). Every refusal the model sees is
;;;; MCP-ROUTER-ERROR, declared by NLK:DEFINE-PERIPHERAL.

(defpackage #:nodecode-mcp-router
  (:use #:cl)
  (:nicknames #:mcp-router)
  (:export
   ;; the selector, callable from eval as well as through the tool
   #:select-tools
   ;; a candidate, as a catalog declares it
   #:tool-profile #:make-tool-profile
   #:tool-profile-id #:tool-profile-name #:tool-profile-description
   #:tool-profile-server #:tool-profile-tags #:tool-profile-tokens
   #:tool-profile-permissions #:tool-profile-enabled #:tool-profile-fallback-id
   #:tool-profile-max-calls #:tool-profile-scope
   ;; the lexical half, for a caller that wants the words
   #:tokenize
   ;; the one condition
   #:mcp-router-error))

(in-package #:nodecode-mcp-router)

(nlk:define-peripheral mcp-router :not-running t)
