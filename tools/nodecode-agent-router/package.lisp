;;;; package.lisp --- the AGENT-ROUTER package: the same decision the tool makes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed AGENT-ROUTER, so an eval form reads
;;;; (agent-router:route "fix the CORS bug"): the same decision the agent_route
;;;; tool makes, over the same registry, for the operator who would rather type
;;;; one.
;;;;
;;;; Every verb answers a STRING: ROUTE-JSON the decision as the JSON the tool
;;;; answers, ROUTE and AGENTS-TEXT as the lines /agents prints. A refusal is
;;;; AGENT-ROUTER-ERROR, which the eval snippet and a tool result both render
;;;; as "ERROR: ...".
;;;;
;;;; The settings these run on derive from the one `agent-router' section
;;;; declaration in cell.lisp, read with (SETTING :MIN-CONFIDENCE).

(defpackage #:nodecode-agent-router
  (:use #:cl)
  (:nicknames #:agent-router)
  (:export
   ;; the verbs, every one a string
   #:route #:route-json #:agents-text
   ;; the example registry as data, so a layer can reword it
   #:*example-registry* #:*primer*
   ;; the one condition
   #:agent-router-error))

(in-package #:nodecode-agent-router)

(nlk:define-peripheral agent-router :not-running t)
