;;;; contracts.lisp --- the tools' arguments.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for each tool's shape: the JSON Schema the provider is
;;;; shown is projected from this, the value the body runs on is materialized
;;;; from it, and a call that does not satisfy it is refused before the body
;;;; sees it.

(in-package #:nodecode-pethost)

(nlk:define-contract tool-call
  (:doc "Call one tool of the Pethost MCP server. Call pethost_tools first when you do not know the names. The work on a Pethost machine is done by these tools: GetMachine, GetProject, CreateProject, CreateDatabase, LinkDatabase, DeployProject, ListCommits, GetOperation, RunProjectAction, SetProjectAlerts, RunMachineAction, QueryHttpTraffic, QueryContainerLogs, RunServiceCommand, ReadPath, CreateTransfer. An answer is only ever what the call returned: never write one yourself.")
  ("tool" :string :required t
   :doc "the server's tool name, exactly as tools/list spells it, e.g. GetMachine")
  ("arguments" :object
   :doc "that tool's arguments as one object, e.g. {\"project_id\": \"recipes\"}; empty when the tool takes none"))

(nlk:define-contract tool-list
  (:doc "List the Pethost MCP server's tools, one line each: the name and what it does. Read it when you do not know which tool does what, before guessing a name in pethost_call.")
  ("filter" :string
   :doc "only the tools whose name or description contains this text, case-insensitive; empty lists them all"))
