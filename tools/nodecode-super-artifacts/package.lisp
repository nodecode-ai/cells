;;;; package.lisp --- the SUPER-ARTIFACTS package, its condition and its constants.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of Super-Artifacts/plugins (see NOTICE). The original
;;;; is a portable Agent Plugins package with no code: an MCP server entry for
;;;; https://superartifacts.app/mcp and one workflow skill. Here the package
;;;; name is the folder loader's, and NLK:DEFINE-PERIPHERAL gives it the
;;;; condition SUPER-ARTIFACTS-ERROR, FAIL, the settings variable
;;;; *SUPER-ARTIFACTS*, RUNNING-SETTINGS, and DEFINE-VERB, whose functions
;;;; refuse while the cell is not running.

(defpackage #:nodecode-super-artifacts
  (:use #:cl)
  (:export #:super-artifacts-error #:start-cell))

(in-package #:nodecode-super-artifacts)

(nlk:define-peripheral super-artifacts :not-running t)

(defparameter +endpoint+ "https://superartifacts.app/mcp"
  "The hosted MCP server the original's mcp.json names, and the default of
the section's base_url.")

(defparameter +origin+ "https://superartifacts.app"
  "The product's own origin: the OAuth endpoints a sign-in uses sit on it.")

(defparameter +key-variable+ "SUPER_ARTIFACTS_KEY"
  "The variable the server's own refusal names, and the default of the
section's key_env: a key from the dashboard's API keys page.")

(defparameter +scope+ "artifacts:write"
  "The scope the server advertises on its 401, and the one a sign-in asks for.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this cell speaks: the one the server answers with a
streamable HTTP transport and an Mcp-Session-Id.")

