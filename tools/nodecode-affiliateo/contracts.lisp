;;;; contracts.lisp --- the two verbs' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it.

(in-package #:nodecode-affiliateo)

(nlk:define-contract tool-call
  (:doc "Call one tool of the Affiliateo MCP server, on the operator's own Affiliateo account. Call affiliateo_tools first when you do not know the name, and affiliateo_tools with tool when you do not know its arguments. The answer is only ever what the server returned: never write one yourself. Writes that reach a person or money (create_invoice, resend_invoice, reschedule_booking, cancel_booking, invite_to_app, reply_to_support_ticket, pay_affiliates, approve_submission, approve_app_request, and every delete_*) act at once, so confirm them with the operator first.")
  ("tool" :string :required t
   :doc "the server's tool name, exactly as tools/list spells it, e.g. list_businesses")
  ("arguments" :object
   :doc "that tool's arguments as one object, e.g. {\"slug\": \"acme\"}; empty when the tool takes none"))

(nlk:define-contract tool-list
  (:doc "List the Affiliateo MCP server's tools, one line each: the name and what it does. Read it before guessing a tool name in affiliateo_call. With tool, answer that one tool's arguments as a JSON Schema instead of the list.")
  ("tool" :string
   :doc "one tool's name, to read its description and argument schema in full")
  ("filter" :string
   :doc "only the tools whose name or description contains this text, case-insensitive; empty lists them all")
  ("refresh" :boolean :default nil
   :doc "read the list from the server again, rather than the copy this image kept"))
