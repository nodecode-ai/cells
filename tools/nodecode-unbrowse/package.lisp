;;;; package.lisp --- the UNBROWSE package: the verbs, their condition, the settings.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed UNBROWSE so an eval form reads
;;;; (unbrowse:scrape "https://example.com"). MAP and INSPECT are the hosted
;;;; tools' own names and shadow their CL namesakes; nothing here uses those.
;;;;
;;;; NLK:DEFINE-PERIPHERAL declares UNBROWSE-ERROR, FAIL, *UNBROWSE*,
;;;; RUNNING-SETTINGS and DEFINE-VERB, which the files after this one read.
;;;;
;;;; Ported from unbrowse-ai/unbrowse's Hermes plugin, plugins/hermes (see
;;;; NOTICE): client.py became mcp.lisp, tools.py's one handler became the
;;;; CALL-TOOL every verb answers through, and the shipped tools.json schemas
;;;; became keyword arguments on the verbs of verbs.lisp.

(defpackage #:nodecode-unbrowse
  (:use #:cl)
  (:nicknames #:unbrowse)
  (:shadow #:map #:inspect)
  (:export #:scrape #:discover #:run #:resume #:cancel #:learn #:forget
           #:map #:inspect #:sites #:credits #:usage #:index #:index-status
           #:browse-open #:browse-snapshot #:browse-act #:browse-finish #:browse-close
           #:credentials-list #:credentials-request #:credentials-status
           #:canvas-read #:canvas-put
           #:call-tool #:tools #:endpoint
           #:unbrowse-error))

(in-package #:nodecode-unbrowse)

(nlk:define-peripheral unbrowse :not-running t)

(defparameter +endpoint+ "https://unbrowse.ai/mcp"
  "The hosted MCP endpoint, what the section defaults to. The plugin's only outbound host.")

(defparameter +protocol-version+ "2025-11-25"
  "Sent as Mcp-Protocol-Version: the hosted server refuses 2025-06-18 on its streaming path.")

(defparameter +client-version+ "0.1.0"
  "What the user-agent names, as the plugin's VERSION does.")

(defparameter +key-page+ "https://unbrowse.ai/app"
  "Where a user creates the API key every call authenticates with.")
