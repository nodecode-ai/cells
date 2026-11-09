;;;; package.lisp --- the AGENTPURSE package: twelve verbs, the gate, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed PURSE so an eval form reads (purse:get-budget).
;;;; Every verb answers a STRING the model reads and every refusal is
;;;; AGENTPURSE-ERROR, rendered by the eval snippet as
;;;; "ERROR: AGENTPURSE-ERROR: <detail>", exactly as the Hermes plugin
;;;; answered a JSON {"ok": false, "error": ...}.
;;;;
;;;; The settings this cell runs on derive from the one `agentpurse' section
;;;; declared in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :base-url).

(defpackage #:nodecode-agentpurse
  (:use #:cl)
  (:nicknames #:purse)
  (:export
   ;; the twelve tools of the plugin's MCP server, as verbs, every one a string
   #:search-merchants #:search-products #:get-product #:check-price
   #:find-alternatives #:get-quote #:request-purchase #:watch-price
   #:list-watches #:cancel-watch #:get-budget #:list-receipts
   ;; the gate's own: what waits for a person, and the person's answer
   #:pending #:approve #:deny
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:agentpurse-error))

(in-package #:nodecode-agentpurse)

(nlk:define-peripheral agentpurse :not-running t :lock "agentpurse")
